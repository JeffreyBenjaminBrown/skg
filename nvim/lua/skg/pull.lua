-- PURPOSE: Client-owned Git pull as a durable maintenance origin.
-- Local worktree paths never participate in shared identity: editor and server
-- independently group configured source names by Git root and compare a
-- portable digest before the first process starts.

local client = require('skg.client')
local config = require('skg.config')
local maintenance = require('skg.maintenance')
local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

M.defer = function (callback) vim.schedule(callback) end
M.confirm = function (prompt, choices, default)
  return vim.fn.confirm(prompt, choices, default)
end
M.is_directory = function (path) return vim.fn.isdirectory(path) == 1 end
M.realpath = function (path) return vim.uv.fs_realpath(path) end
M.jobstart = function (argv, options) return vim.fn.jobstart(argv, options) end

local function sorted_copy (values)
  local result = vim.deepcopy(values or {})
  table.sort(result)
  return result
end

local function equal_lists (left, right)
  if #left ~= #right then return false end
  for index, value in ipairs(left) do
    if value ~= right[index] then return false end end
  return true
end

local function canonical_directory (path, label)
  if type(path) ~= 'string' or path == '' or not M.is_directory(path) then
    error(label .. ' is not a local directory: ' .. tostring(path)) end
  local canonical = M.realpath(path)
  if type(canonical) ~= 'string' or canonical == '' then
    error('Cannot resolve ' .. label .. ': ' .. path) end
  canonical = vim.fs.normalize(canonical)
  if #canonical > 1 then canonical = canonical:gsub('[/\\]+$', '') end
  return canonical
end

local function absolute_path_p (path)
  return path:sub(1, 1) == '/' or path:match('^%a:[/\\]') ~= nil
end

local function described_source_path (config_file, configured_path)
  local normalized = vim.fs.normalize(configured_path)
  if absolute_path_p(normalized) then return normalized end
  return vim.fs.normalize(
    vim.fs.dirname(vim.fs.normalize(config_file)) .. '/' .. normalized)
end

---Return the Git worktree root containing PATH, or nil and a diagnostic.
function M.discover_git_root (path)
  local result = vim.system(
    { 'git', '-C', path, 'rev-parse', '--show-toplevel' },
    { text = true }):wait()
  if result.code ~= 0 then
    local detail = vim.trim(result.stderr or '')
    if detail == '' then detail = 'git exited ' .. tostring(result.code) end
    return nil, detail
  end
  local root = vim.trim(result.stdout or '')
  if root == '' or root:find('\n', 1, true) then
    return nil, 'git returned an invalid worktree root' end
  return root
end

---Portable identity for one repository's sorted configured source names.
---@param sources string[]
---@return string
function M.repository_key (sources)
  return vim.fn.sha256(table.concat(sorted_copy(sources), '\0'))
end

local function checked_names (entries, label, field)
  local names, seen = {}, {}
  for _, entry in ipairs(entries or {}) do
    local name = entry[field or 'name']
    if type(name) ~= 'string' or name == '' then
      error(label .. ' contains an invalid source name') end
    if seen[name] then error(label .. ' repeats source ' .. name) end
    seen[name] = true
    table.insert(names, name)
  end
  table.sort(names)
  return names
end

---Build the deterministic local pull plan for every configured source.
---@return table[] repositories with key, root, and sorted sources
function M.local_repositories ()
  if not config.source_inventory then
    error('Skg has no verified server source inventory') end
  local config_file = config.config_file()
  if not config_file then error('Skg has no local skgconfig.toml') end
  local local_sources = config.source_paths_from_toml(config_file)
  local server_names = checked_names(
    config.source_inventory, 'Server source inventory')
  local local_names = checked_names(local_sources, 'Local source inventory')
  if not equal_lists(server_names, local_names) then
    error('Local and server source inventories differ: '
      .. table.concat(local_names, ', ') .. ' versus '
      .. table.concat(server_names, ', '))
  end

  local server_by_name = {}
  for _, source in ipairs(config.source_inventory) do
    server_by_name[source.name] = source end
  local by_root = {}
  for _, source in ipairs(local_sources) do
    local server_source = server_by_name[source.name]
    local configured_path = server_source and server_source.configured_path
    if type(configured_path) ~= 'string' or configured_path == '' then
      error('Server source ' .. source.name .. ' has no raw path descriptor') end
    local local_identity = canonical_directory(
      source.path, 'Configured source ' .. source.name)
    local described = described_source_path(config_file, configured_path)
    local described_identity = canonical_directory(
      described, 'Server-described source ' .. source.name)
    if local_identity ~= described_identity then
      error('Local source ' .. source.name
        .. " does not match the server's raw path " .. configured_path) end
    local root, git_error = M.discover_git_root(source.path)
    if not root then
      error('Configured source ' .. source.name
        .. ' is not in a Git worktree: ' .. tostring(git_error)) end
    root = canonical_directory(root, 'Git root for source ' .. source.name)
    by_root[root] = by_root[root] or {}
    table.insert(by_root[root], source.name)
  end

  local roots = vim.tbl_keys(by_root)
  table.sort(roots)
  local repositories, keys = {}, {}
  for _, root in ipairs(roots) do
    local sources = sorted_copy(by_root[root])
    local key = M.repository_key(sources)
    if keys[key] then
      error('Two local Git roots produced repository key ' .. key) end
    keys[key] = true
    table.insert(repositories, { key = key, root = root, sources = sources })
  end
  return repositories
end

---Drop client-local roots and sort the shared repository mapping by key.
function M.logical_repositories (repositories)
  local result = {}
  for _, repository in ipairs(repositories or {}) do
    table.insert(result, {
      key = repository.key,
      sources = sorted_copy(repository.sources),
    })
  end
  table.sort(result, function (left, right) return left.key < right.key end)
  return result
end

---Structured bootstrap fields containing no client-local absolute roots.
function M.request_fields (repositories)
  local records = {}
  for _, repository in ipairs(M.logical_repositories(repositories)) do
    table.insert(records, {
      sexpr.pair(sexpr.symbol('repository-key'), repository.key),
      { sexpr.symbol('sources'), repository.sources },
    })
  end
  return { { sexpr.symbol('pull-repositories'), records } }
end

function M.new_context (repositories)
  return {
    repositories = repositories,
    server_repositories = nil,
    remaining = nil,
    current_job = nil,
    advance_pending = false,
    started = false,
    details = {},
    failures = {},
    external_result = nil,
    diagnostic_buffers = {},
    diagnostic_window = nil,
    diagnostic_sequence = 0,
  }
end

---Return or recreate process-local state for the active pull incident.
function M.context ()
  local incident = state.maintenance_client_incident
  if not incident or not incident.offer or incident.offer.origin ~= 'pull' then
    error('No client-owned pull incident is active') end
  if not incident.origin_context then
    incident.origin_context = M.new_context(nil) end
  local context = incident.origin_context
  context.details = context.details or {}
  context.failures = context.failures or {}
  context.diagnostic_buffers = context.diagnostic_buffers or {}
  context.diagnostic_sequence = context.diagnostic_sequence or 0
  return context
end

local function dirty_buffers ()
  local result = {}
  for _, buf in ipairs(registry.buffers()) do
    if registry.dirty(buf) then table.insert(result, buf) end end
  return result
end

M.dirty_buffers = dirty_buffers

local function buffer_description (buf)
  local name = vim.api.nvim_buf_get_name(buf)
  if name == '' then name = '[No Name ' .. tostring(buf) .. ']' end
  local record = registry.record(buf)
  return string.format('%s [%s]', name, record and record.kind or 'unregistered')
end

---Pull every configured repository through one maintenance incident.
---@return boolean started
function M.pull_all ()
  if state.maintenance_client_incident then
    error('Maintenance is already active') end
  if state.active_source_set_name ~= 'all' then
    error("Pull requires source-set 'all'; switch from '"
      .. tostring(state.active_source_set_name) .. "' before pulling") end
  local repositories = M.local_repositories()
  local dirty = M.dirty_buffers()
  local dirty_raw = {}
  for _, buf in ipairs(dirty) do
    local record = registry.record(buf)
    if record and record.kind == 'raw-skg-file' then
      table.insert(dirty_raw, buf) end
  end
  if #dirty_raw > 0 then
    local names = {}
    for _, buf in ipairs(dirty_raw) do
      table.insert(names, vim.api.nvim_buf_get_name(buf)) end
    error('Pull refuses modified raw .skg buffers: '
      .. table.concat(names, ', '))
  end
  if #repositories == 0 then
    error('No configured Git repositories can be pulled') end
  if #dirty > 0 then
    local descriptions = {}
    for _, buf in ipairs(dirty) do
      table.insert(descriptions, buffer_description(buf)) end
    local answer = M.confirm(
      'Pull will archive these dirty Skg buffers: '
        .. table.concat(descriptions, ', ')
        .. '. Impacted buffers will become detached recovery buffers; '
        .. 'orthogonal buffers will remain editable afterward. A partially '
        .. 'failed pull will still reconcile the final disk. Continue?',
      '&Continue\n&Cancel', 2)
    if answer ~= 1 then return false end
  end
  local context = M.new_context(repositories)
  maintenance.begin('pull', nil, nil, nil, M.terminal, context,
    M.request_fields(repositories))
  return true
end

local function request (name, atom_fields, list_fields)
  local form = { sexpr.pair(sexpr.symbol('request'), name) }
  for _, entry in ipairs(atom_fields or {}) do
    table.insert(form, sexpr.pair(sexpr.symbol(entry[1]), entry[2])) end
  for _, entry in ipairs(list_fields or {}) do
    local field = { sexpr.symbol(entry[1]) }
    for _, value in ipairs(entry[2] or {}) do
      table.insert(field, value) end
    table.insert(form, field)
  end
  return sexpr.to_string(form) .. '\n'
end

local function response_epoch (response)
  local value = payload.field(response, 'maintenance-epoch')
  if type(value) == 'number' and value >= 0
     and value == math.floor(value) then return value end
  local text = payload.field_text(response, 'maintenance-epoch')
  if text and text:match('^%d+$') then return tonumber(text) end
  error('Pull response has no valid maintenance epoch')
end

local function require_response_incident (response)
  local incident = assert(state.maintenance_client_incident,
    'Pull response arrived without client state')
  if payload.field_text(response, 'incident-id') ~= incident.incident_id
     or response_epoch(response) ~= incident.epoch then
    error('Pull response names another incident or epoch') end
  return incident
end

local function request_failure (phase, label)
  return function (reason)
    local incident = require('skg.state').maintenance_client_incident
    if incident then incident.phase = phase end
    vim.notify(label .. ': ' .. tostring(reason), vim.log.levels.WARN)
  end
end

function M.request_authorization ()
  local incident = assert(state.maintenance_client_incident,
    'No pull incident is ready for authorization')
  incident.phase = 'origin-operation-start-pending'
  state.register_response_handler(
    'maintenance-status', M.handle_authorization, true)
  state.set_request_failure_handler(request_failure(
    'origin-operation-start-pending', 'Pull authorization was not delivered'))
  client.submit_request(request('run maintenance origin', {
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

local function schedule_protected (label, callback)
  M.defer(function ()
    local ok, error_text = pcall(callback)
    if not ok then
      vim.notify(label .. ': ' .. tostring(error_text), vim.log.levels.ERROR)
    end
  end)
end

function M.handle_authorization (_payload_text, response)
  local incident = require_response_incident(response)
  local context = M.context()
  if payload.field_text(response, 'status') ~= 'external-mutation-authorized'
     or payload.field_text(response, 'phase')
        ~= 'running-external-mutation' then
    error('Server did not authorize the client-owned pull') end
  incident.phase = 'running-external-mutation'
  if payload.field_text(response, 'replayed') == 'true' then
    M.resume_running(
      'authorization reply was replayed without a live owned Git child')
  else
    context.started = true
    context.remaining = vim.deepcopy(context.repositories)
    M.start_next()
  end
end

local function repository_label (repository)
  return string.format('repository %s [%s]', repository.key:sub(1, 12),
    table.concat(repository.sources or {}, ', '))
end

function M.git_argv (repository)
  return { 'git', '-C', repository.root, 'pull' }
end

local function diagnostic_name (incident_id, sequence, key)
  return string.format('skg://pull/%s/%03d-%s',
    incident_id, sequence, key:sub(1, 12))
end

---Start one interactive terminal job and preserve its buffer permanently.
function M.start_terminal (repository, context, on_exit)
  local incident = assert(state.maintenance_client_incident,
    'Cannot open pull diagnostics without an incident')
  context.diagnostic_sequence = context.diagnostic_sequence + 1
  local name = diagnostic_name(
    incident.incident_id, context.diagnostic_sequence, repository.key)
  while vim.fn.bufnr(name) >= 0 do
    context.diagnostic_sequence = context.diagnostic_sequence + 1
    name = diagnostic_name(
      incident.incident_id, context.diagnostic_sequence, repository.key)
  end
  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_buf_set_name(buf, name)
  vim.api.nvim_set_option_value('bufhidden', 'hide', { buf = buf })
  vim.api.nvim_set_option_value('swapfile', false, { buf = buf })
  table.insert(context.diagnostic_buffers, {
    buffer = buf, name = name, key = repository.key, root = repository.root,
  })

  local win = context.diagnostic_window
  if win and vim.api.nvim_win_is_valid(win) then
    vim.api.nvim_win_set_buf(win, buf)
  else
    vim.cmd('botright split')
    win = vim.api.nvim_get_current_win()
    context.diagnostic_window = win
    vim.api.nvim_win_set_buf(win, buf)
  end

  local job
  vim.api.nvim_buf_call(buf, function ()
    job = M.jobstart(M.git_argv(repository), {
      term = true,
      on_exit = function (job_id, exit_code, event)
        vim.schedule(function () on_exit(job_id, exit_code, event) end)
      end,
    })
  end)
  vim.b[buf].skg_pull_diagnostic_name = name
  return job, buf
end

---Poll JOB_ID without waiting. Returns 'running', an exit code, or 'lost'.
function M.job_status (job_id)
  local ok, statuses = pcall(vim.fn.jobwait, { job_id }, 0)
  if not ok or type(statuses) ~= 'table' then return 'lost' end
  local status = statuses[1]
  if status == -1 then return 'running' end
  if type(status) == 'number' and status >= 0 then return status end
  return 'lost'
end

function M.schedule_next ()
  local context = M.context()
  if context.advance_pending then return end
  context.advance_pending = true
  local incident_id = state.maintenance_client_incident.incident_id
  M.defer(function ()
    local ok, error_text = pcall(function ()
      require('skg.pull').run_scheduled_next(incident_id) end)
    if not ok then
      vim.notify('Pull process chain could not advance: '
        .. tostring(error_text), vim.log.levels.ERROR) end
  end)
end

function M.run_scheduled_next (incident_id)
  local incident = state.maintenance_client_incident
  if not incident or incident.incident_id ~= incident_id
     or not incident.offer or incident.offer.origin ~= 'pull' then return end
  local context = M.context()
  context.advance_pending = false
  M.start_next()
end

function M.start_next ()
  local incident = assert(state.maintenance_client_incident,
    'No pull incident can start a Git job')
  local context = M.context()
  local repository = context.remaining and table.remove(context.remaining, 1)
  if not repository then
    M.finish_origin(#context.failures > 0 and 'failed' or 'completed',
      vim.deepcopy(context.details))
    return
  end
  local incident_id, repository_key = incident.incident_id, repository.key
  local callback = function (job_id, exit_code, event)
    local ok, error_text = pcall(function ()
      require('skg.pull').job_exited(
        incident_id, repository_key, job_id, exit_code, event) end)
    if not ok then
      vim.notify('Pull job completion failed: ' .. tostring(error_text),
        vim.log.levels.ERROR) end
  end
  local ok, job_or_error, diagnostic_buffer = pcall(
    M.start_terminal, repository, context, callback)
  if not ok or type(job_or_error) ~= 'number' or job_or_error <= 0 then
    local reason = ok and ('jobstart returned ' .. tostring(job_or_error))
      or tostring(job_or_error)
    local detail = repository_label(repository)
      .. ' could not start: ' .. reason
    table.insert(context.details, detail)
    table.insert(context.failures, detail)
    if diagnostic_buffer and vim.api.nvim_buf_is_valid(diagnostic_buffer) then
      vim.api.nvim_buf_set_lines(
        diagnostic_buffer, 0, -1, false, { detail })
      vim.bo[diagnostic_buffer].modified = false
    end
    M.schedule_next()
    return
  end
  context.current_job = {
    id = job_or_error,
    key = repository.key,
    repository = repository,
    diagnostic_buffer = diagnostic_buffer,
    diagnostic_name = diagnostic_buffer
      and vim.b[diagnostic_buffer].skg_pull_diagnostic_name or nil,
  }
end

function M.job_exited (incident_id, repository_key, job_id, exit_code, event)
  local incident = state.maintenance_client_incident
  if not incident or incident.incident_id ~= incident_id then return end
  local context = M.context()
  local current = context.current_job
  if not current or current.id ~= job_id or current.key ~= repository_key then
    return end
  context.current_job = nil
  if current.diagnostic_buffer
     and vim.api.nvim_buf_is_valid(current.diagnostic_buffer) then
    local name = current.diagnostic_name
      or vim.b[current.diagnostic_buffer].skg_pull_diagnostic_name
    if name then vim.api.nvim_buf_set_name(current.diagnostic_buffer, name) end
  end
  local event_label = type(event) == 'string' and vim.trim(event) or 'exit'
  if event_label == '' then event_label = 'exit' end
  local detail = string.format('%s: %s %s',
    repository_label(current.repository), event_label, tostring(exit_code))
  table.insert(context.details, detail)
  if event_label ~= 'exit' or exit_code ~= 0 then
    table.insert(context.failures, detail) end
  M.schedule_next()
end

function M.finish_origin (outcome, details)
  local incident = assert(state.maintenance_client_incident,
    'No pull incident can report completion')
  local context = M.context()
  local record = { outcome = outcome, details = vim.deepcopy(details or {}) }
  context.external_result = record
  context.advance_pending = false
  incident.phase = 'origin-completion-pending'
  M.send_finish(record)
end

function M.send_finish (record)
  local incident = assert(state.maintenance_client_incident,
    'No pull incident can send completion')
  state.register_response_handler(
    'maintenance-status', M.handle_finish, true)
  state.set_request_failure_handler(request_failure(
    'origin-completion-pending', 'Pull completion was not delivered'))
  client.submit_request(request('finish maintenance origin', {
    { 'maintenance-epoch', incident.epoch },
    { 'external-outcome', record.outcome },
  }, {
    { 'external-details', record.details },
  }), nil, incident.incident_id)
end

function M.handle_finish (_payload_text, response)
  local incident = require_response_incident(response)
  if payload.field_text(response, 'status') ~= 'origin-operation-finished'
     or payload.field_text(response, 'phase') ~= 'final-observation' then
    error('Server did not begin exact post-pull observation') end
  incident.phase = 'waiting-for-origin-observation'
  vim.notify('Skg is observing exact disk after pull')
end

local function atom_list (value, label)
  if not sexpr.is_list(value) then error(label .. ' is not a list') end
  local result = {}
  for _, item in ipairs(value) do
    local ok, text = pcall(sexpr.atom_text, item)
    if not ok or text == '' then error(label .. ' contains a non-atom') end
    table.insert(result, text)
  end
  return result
end

function M.response_repositories (response)
  local records = payload.field(response, 'pull-repositories')
  if not sexpr.is_list(records) then
    error('Server returned a malformed pull repository mapping') end
  local repositories, keys = {}, {}
  for _, record in ipairs(records) do
    if not sexpr.is_list(record) then
      error('Server returned a malformed pull repository record') end
    local key = payload.field_text(record, 'repository-key')
    local sources = atom_list(
      payload.field(record, 'sources'), 'Pull repository sources')
    local source_count = #sources
    sources = sorted_copy(sources)
    local deduplicated = {}
    for _, source in ipairs(sources) do deduplicated[source] = true end
    if type(key) ~= 'string' or not key:match('^[0-9a-f]+$')
       or #key ~= 64 or source_count == 0
       or vim.tbl_count(deduplicated) ~= source_count
       or key ~= M.repository_key(sources) then
      error('Server returned an invalid pull repository mapping') end
    if keys[key] then error('Server repeated pull repository key ' .. key) end
    keys[key] = true
    table.insert(repositories, { key = key, sources = sources })
  end
  table.sort(repositories,
    function (left, right) return left.key < right.key end)
  return repositories
end

function M.require_matching_repositories (context)
  if not context.server_repositories then
    error('Server has not confirmed the pull repository mapping') end
  if not vim.deep_equal(context.server_repositories,
      M.logical_repositories(context.repositories)) then
    error('Local Git roots do not match the server repository mapping') end
end

function M.install_server_repositories (response)
  local value = payload.field(response, 'pull-repositories')
  if value == nil then return end
  local context = M.context()
  local repositories = M.response_repositories(response)
  if #repositories == 0 then
    error('Server returned an empty pull repository mapping') end
  if context.server_repositories
     and not vim.deep_equal(context.server_repositories, repositories) then
    error('Server changed the pull repository mapping') end
  context.server_repositories = repositories
  if context.repositories then M.require_matching_repositories(context) end
end

function M.install_server_result (response)
  if payload.field(response, 'external-outcome') == nil then return end
  local outcome = payload.field_text(response, 'external-outcome')
  if outcome ~= 'completed' and outcome ~= 'failed'
     and outcome ~= 'indeterminate' then
    error('Server returned an invalid pull outcome: ' .. tostring(outcome)) end
  local details_value = payload.field(response, 'external-details')
  local details = details_value == nil and {} or atom_list(
    details_value, 'Pull external details')
  local context = M.context()
  local record = { outcome = outcome, details = details }
  if context.external_result
     and not vim.deep_equal(context.external_result, record) then
    error('Server changed the journaled pull outcome') end
  context.external_result = record
end

function M.resume_archive_ready ()
  local context = M.context()
  local repositories = M.local_repositories()
  if #repositories == 0 then
    error('No configured Git repositories can be pulled') end
  if context.repositories and not vim.deep_equal(
      M.logical_repositories(context.repositories),
      M.logical_repositories(repositories)) then
    error('Local pull repository topology changed before authorization') end
  context.repositories = repositories
  M.require_matching_repositories(context)
  M.request_authorization()
end

function M.resume_running (lost_child_reason)
  local context = M.context()
  local current = context.current_job
  if current then
    local status = M.job_status(current.id)
    if status == 'running' then return true end
    if type(status) == 'number' and status >= 0 then
      M.job_exited(state.maintenance_client_incident.incident_id,
        current.key, current.id, status, 'exit')
      return true
    end
    context.current_job = nil
  end
  if context.external_result then
    schedule_protected('Pull result could not be resent', function ()
      require('skg.pull').send_finish(context.external_result) end)
  elseif context.advance_pending then
    return true
  else
    schedule_protected('Indeterminate pull result could not be reported',
      function ()
        require('skg.pull').finish_origin(
          'indeterminate', { lost_child_reason })
      end)
  end
  return true
end

function M.origin_operation_handler (_incident, phase, response)
  M.context()
  M.install_server_repositories(response)
  M.install_server_result(response)
  if phase == 'archive-ready' then
    schedule_protected('Pull remains archive-ready', function ()
      require('skg.pull').resume_archive_ready() end)
  elseif phase == 'running-external-mutation' then
    M.resume_running(
      'server awaited pull completion but no owned Git child survived')
  elseif phase == 'final-observation' then
    local record = M.context().external_result
    if not record then
      error('Final pull observation has no journaled external result') end
    schedule_protected('Pull result could not be replayed', function ()
      require('skg.pull').send_finish(record) end)
  else
    return false
  end
  return true
end

function M.terminal (_response)
  local incident = state.maintenance_client_incident
  local context = incident and incident.origin_context
  local names = {}
  for _, diagnostic in ipairs(context and context.diagnostic_buffers or {}) do
    if diagnostic.buffer and vim.api.nvim_buf_is_valid(diagnostic.buffer) then
      table.insert(names, diagnostic.name) end
  end
  if #names == 0 then
    vim.notify('Skg pull maintenance completed')
  else
    vim.notify('Skg pull maintenance completed; diagnostics remain in '
      .. table.concat(names, ', '))
  end
end

maintenance.register_origin_operation_handler('pull', M.origin_operation_handler)

return M
