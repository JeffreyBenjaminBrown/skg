-- Detached maintenance-recovery UI.  Recovery buffers are independent
-- scratch copies: they carry no view URI, registry record, save authority, or
-- close-view callback, and no operation in this module writes into an archive.

local archive = require('skg.recovery_archive')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

local recovery_contexts = {}
local list_rows = {}
local recovery_sequence = 0

local function fail (message)
  error('skg recovery UI failed: ' .. message, 0)
end

local function required_field (value, key, context)
  if not sexpr.is_list(value) then fail(context .. ' is not a field list') end
  local result = payload.field(value, key)
  if result == nil then fail(context .. ' lacks ' .. key) end
  return result
end

local function required_text (value, key, context)
  local result = required_field(value, key, context)
  if sexpr.is_nil(result) then return 'nil' end
  if sexpr.is_list(result) or sexpr.is_pair(result) then
    fail(context .. ' has non-atomic ' .. key) end
  return sexpr.atom_text(result)
end

local function required_integer (value, key, context)
  local result = required_field(value, key, context)
  if type(result) ~= 'number' or result < 0 or result ~= math.floor(result) then
    fail(context .. ' has invalid ' .. key) end
  return result
end

local function required_list (value, key, context)
  local result = required_field(value, key, context)
  if not sexpr.is_list(result) then
    fail(context .. ' has malformed ' .. key .. ' list') end
  return result
end

local function parse_exact (bytes, context)
  local ok, value, position = pcall(sexpr.read, bytes)
  if not ok then fail('invalid ' .. context .. ': ' .. tostring(value)) end
  if not bytes:sub(position):match('^%s*$') then
    fail(context .. ' has trailing data') end
  return value
end

local function install_raw_text (buf, text)
  local endofline = text:sub(-1) == '\n'
  local body = endofline and text:sub(1, -2) or text
  local lines = vim.split(body, '\n', { plain = true, trimempty = false })
  if #lines == 0 then lines = { '' } end
  vim.bo[buf].undolevels = -1
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].endofline = endofline
  vim.bo[buf].undolevels = vim.o.undolevels
  vim.bo[buf].modified = false
end

local function raw_text (buf)
  local text = table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
  if vim.bo[buf].endofline then text = text .. '\n' end
  return text
end

local function interrupted_records (summary)
  if summary.status ~= 'finalized' then
    fail('only finalized incidents have interrupted dispositions') end
  local keys, expected, result = {}, 0, {}
  for _, settlement in ipairs(required_list(
    summary.final, 'buffer-dispositions', 'final manifest')) do
    if required_text(settlement, 'disposition', 'buffer disposition')
       == 'interrupted' then
      local key = required_text(settlement, 'buffer-key', 'buffer disposition')
      if not keys[key] then keys[key], expected = true, expected + 1 end
    end
  end
  for _, record in ipairs(required_list(
    summary.initial, 'buffers', 'initial manifest')) do
    if keys[required_text(record, 'buffer-key', 'initial buffer')] then
      table.insert(result, record) end
  end
  if #result ~= expected then
    fail('an interrupted disposition has no archived buffer') end
  return result
end

local function picker_label (summary)
  return string.format('%s  %-24s  %s',
    summary.started_at_utc or summary.name,
    summary.origin or 'invalid', summary.status)
end

M.incident_picker = function (summaries, prompt, callback)
  vim.ui.select(summaries, {
    prompt = prompt,
    format_item = picker_label,
  }, callback)
end

M.buffer_picker = function (records, prompt, callback)
  vim.ui.select(records, {
    prompt = prompt,
    format_item = function (record)
      return string.format('%s  [%s]  %s',
        required_text(record, 'buffer-key', 'initial buffer'),
        required_text(record, 'kind', 'initial buffer'),
        required_text(record, 'name', 'initial buffer'))
    end,
  }, callback)
end

M.root_picker = function (roots, prompt, callback)
  vim.ui.select(roots, { prompt = prompt }, callback)
end

M.confirm = function (prompt)
  return vim.fn.confirm(prompt, '&Yes\n&No', 2) == 1
end

local function current_summary ()
  local buf = vim.api.nvim_get_current_buf()
  local context = recovery_contexts[buf]
  if context then return archive.inspect(context.summary.path) end
  local rows = list_rows[buf]
  if rows then
    local row = vim.api.nvim_win_get_cursor(0)[1]
    local summary = rows[row]
    if summary then return archive.inspect(summary.path) end
  end
end

local function with_incident (incident, callback)
  if type(incident) == 'table' and incident.path then
    return callback(archive.inspect(incident.path)) end
  if type(incident) == 'string' then
    return callback(archive.inspect(incident)) end
  local current = current_summary()
  if current then return callback(current) end
  local summaries = archive.list()
  if #summaries == 0 then fail('no retained maintenance incidents') end
  M.incident_picker(summaries, 'Maintenance incident', function (selected)
    if selected then callback(archive.inspect(selected.path)) end
  end)
end

local function find_buffer_record (records, key)
  if not key then return nil end
  for _, record in ipairs(records) do
    if required_text(record, 'buffer-key', 'initial buffer') == tostring(key) then
      return record end
  end
end

local function with_target (incident, buffer_key, callback)
  return with_incident(incident, function (summary)
    local records = interrupted_records(summary)
    local current = recovery_contexts[vim.api.nvim_get_current_buf()]
    local key = buffer_key or (current and current.buffer_key)
    local record = find_buffer_record(records, key)
    if record then return callback(summary, record) end
    if buffer_key ~= nil then
      fail('incident has no interrupted buffer with key '
        .. tostring(buffer_key)) end
    if #records == 0 then fail('this incident has no interrupted buffers') end
    M.buffer_picker(records, 'Interrupted buffer', function (selected)
      if selected then callback(summary, selected) end
    end)
  end)
end

local function verify_buffer_artifacts (summary, record)
  local key = required_text(record, 'buffer-key', 'initial buffer')
  if not key:match('^[A-Za-z0-9][A-Za-z0-9._-]*$') then
    fail('unsafe archived buffer key') end
  local prefix = 'buffer-snapshots/' .. key .. '/'
  local artifacts, records, sidecar_error = {}, {}, nil
  for _, artifact in ipairs(required_list(
    record, 'artifacts', 'initial buffer')) do
    local relative = required_text(artifact, 'path', 'buffer artifact')
    if relative:sub(1, #prefix) ~= prefix then
      fail('buffer artifact belongs outside its snapshot') end
    local basename = vim.fs.basename(relative)
    if basename == 'undo.nvim' or basename == 'undo.emacs.gz' then
      local ok, result = pcall(archive.verify_recorded_artifact,
        summary.path, artifact, 'native undo artifact')
      if ok then artifacts[relative], records[relative] = result, artifact
      else sidecar_error = tostring(result) end
    else
      artifacts[relative] = archive.verify_recorded_artifact(
        summary.path, artifact, 'buffer artifact')
      records[relative] = artifact
    end
  end
  for _, filename in ipairs({
    'README.org', 'metadata.sexp', 'last-fetched.org',
    'unsaved-changes.org', 'diff.txt',
  }) do
    if artifacts[prefix .. filename] == nil then
      fail('snapshot lacks verified ' .. filename) end
  end
  return {
    key = key, prefix = prefix, artifacts = artifacts,
    records = records, sidecar_error = sidecar_error,
  }
end

local function verified_metadata (record, verified)
  local metadata = parse_exact(
    verified.artifacts[verified.prefix .. 'metadata.sexp'], 'buffer metadata')
  if required_integer(metadata, 'archive-format-version', 'buffer metadata')
       ~= archive.archive_format_version
     or required_text(metadata, 'buffer-key', 'buffer metadata')
       ~= required_text(record, 'buffer-key', 'initial buffer')
     or required_text(metadata, 'buffer-id', 'buffer metadata')
       ~= required_text(record, 'buffer-id', 'initial buffer') then
    fail('buffer metadata belongs to another snapshot') end
  return metadata
end

local function artifact_links (summary, record, verified)
  local links = {}
  for relative, artifact in pairs(verified.records) do
    table.insert(links, {
      label = relative, record = artifact, context = 'buffer artifact',
    })
  end
  for _, group in ipairs({
    { key = 'root-artifacts', context = 'incident artifact' },
    { key = 'node-artifacts', context = 'node evidence' },
  }) do
    for _, artifact in ipairs(required_list(
      summary.final, group.key, 'final manifest')) do
      table.insert(links, {
        label = required_text(artifact, 'path', group.context),
        record = artifact, context = group.context,
      })
    end
  end
  table.sort(links, function (left, right) return left.label < right.label end)
  return links
end

local function reset_text_only_history (buf, text)
  install_raw_text(buf, text)
  vim.bo[buf].modified = false
end

local function restore_native_undo (buf, summary, record, verified, text)
  local undo = required_list(record, 'undo', 'initial buffer')
  local status = required_text(undo, 'status', 'buffer undo')
  if status ~= 'archived' then return 'text-only-no-native-history' end
  if summary.client_kind ~= 'neovim' then
    vim.notify('Archive came from another editor; using exact text fallback',
               vim.log.levels.WARN)
    return 'text-only-other-client'
  end
  if verified.sidecar_error then
    vim.notify('Native undo is corrupt; using exact text fallback: '
      .. verified.sidecar_error, vim.log.levels.WARN)
    return 'text-only-corrupt-sidecar'
  end
  local relative = verified.prefix .. 'undo.nvim'
  if not verified.artifacts[relative] then
    vim.notify('Native undo sidecar is missing; using exact text fallback',
               vim.log.levels.WARN)
    return 'text-only-missing-sidecar'
  end
  local pseudo = summary.path .. '/' .. verified.prefix
    .. 'unsaved-changes.org'
  local sidecar = summary.path .. '/' .. relative
  local ok, result = pcall(require('skg.undo_sidecar').restore,
    buf, pseudo, sidecar, required_text(undo, 'version', 'buffer undo'))
  if not ok then
    reset_text_only_history(buf, text)
    vim.notify('Native undo could not be restored; using exact text fallback: '
      .. tostring(result), vim.log.levels.WARN)
    return 'text-only-native-error'
  end
  return 'native-restored'
end

local function configure_recovery_buffer (buf)
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].bufhidden = 'hide'
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modifiable = true
  vim.b[buf].skg_recovery = true
  vim.api.nvim_create_autocmd('BufWriteCmd', {
    buffer = buf,
    callback = function ()
      fail('detached recovery cannot be written; copy into a live view or ordinary buffer')
    end,
  })
  vim.api.nvim_create_autocmd({ 'BufDelete', 'BufWipeout' }, {
    buffer = buf,
    once = true,
    callback = function () recovery_contexts[buf] = nil end,
  })
  vim.keymap.set('n', '<localleader>ro', function () M.open_artifact() end,
    { buffer = buf, desc = 'Open a verified recovery artifact' })
  vim.keymap.set('n', '<localleader>rf',
    function () M.open_fresh_view_for_interrupted() end,
    { buffer = buf, desc = 'Open a fresh live view beside recovery' })
end

local function restore_layout (buf, metadata)
  local windows = required_list(metadata, 'windows', 'buffer metadata')
  local window = vim.fn.bufwinid(buf)
  if window == -1 then return end
  local record = windows[1]
  if record then
    local ok_row, row = pcall(required_integer,
      record, 'cursor-row', 'buffer window')
    local ok_column, column = pcall(required_integer,
      record, 'cursor-column', 'buffer window')
    if ok_row and ok_column then
      row = math.max(1, math.min(row, vim.api.nvim_buf_line_count(buf)))
      local maximum = #vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
      pcall(vim.api.nvim_win_set_cursor, window,
        { row, math.min(column, maximum) })
    end
    local folds = payload.field(record, 'closed-folds')
    if sexpr.is_list(folds) then
      vim.api.nvim_win_call(window, function ()
        vim.wo.foldmethod = 'manual'
        vim.cmd('silent! normal! zE')
        for _, fold in ipairs(folds) do
          if sexpr.is_list(fold) and type(fold[1]) == 'number'
             and type(fold[2]) == 'number' and fold[1] < fold[2]
             and fold[2] <= vim.api.nvim_buf_line_count(buf) then
            vim.cmd(string.format('silent! %d,%dfold', fold[1], fold[2])) end
        end
      end)
    end
  end
end

local function recovery_name (summary, record)
  recovery_sequence = recovery_sequence + 1
  return string.format('skg://recovery/%s/%s/%d',
    summary.incident_id:sub(1, 8),
    required_text(record, 'buffer-key', 'initial buffer'), recovery_sequence)
end

---Open one independent checksum-verified copy of an interrupted buffer.
function M.open_interrupted_view (incident, buffer_key)
  return with_target(incident, buffer_key, function (summary, record)
    local verified = verify_buffer_artifacts(summary, record)
    local metadata = verified_metadata(record, verified)
    local text = verified.artifacts[verified.prefix .. 'unsaved-changes.org']
    local buf = vim.api.nvim_create_buf(true, true)
    local ok, result = xpcall(function ()
      vim.api.nvim_buf_set_name(buf, recovery_name(summary, record))
      configure_recovery_buffer(buf)
      install_raw_text(buf, text)
      local context = {
        summary = summary,
        buffer_record = record,
        buffer_key = verified.key,
        metadata = metadata,
        artifact_links = artifact_links(summary, record, verified),
      }
      recovery_contexts[buf] = context
      context.native_undo_status = restore_native_undo(
        buf, summary, record, verified, text)
      vim.b[buf].skg_recovery_native_undo_status = context.native_undo_status
      vim.bo[buf].modified = false
      vim.api.nvim_set_current_buf(buf)
      restore_layout(buf, metadata)
      return buf
    end, debug.traceback)
    if not ok then
      recovery_contexts[buf] = nil
      if vim.api.nvim_buf_is_valid(buf) then
        vim.bo[buf].modified = false
        vim.api.nvim_buf_delete(buf, { force = true }) end
      error(result, 0)
    end
    return result
  end)
end

local function summary_line (summary)
  if summary.status == 'invalid' then
    return string.format('| %s |  | invalid |  |  |  |  |  |  | no |',
      summary.name) end
  return string.format('| %s | %s | %s | %d | %d | %d | %s | %s/%s | %d (%s) | %s |',
    summary.started_at_utc, summary.origin, summary.status,
    summary.changed_nodes, summary.interrupted_buffers,
    summary.released_buffers, summary.client_kind, summary.g0,
    summary.g1 or '—', summary.bytes, summary.iec,
    summary.native_undo_compatible and 'yes' or 'no')
end

---Show all retained incidents in an offline-capable scratch table.
function M.list_maintenance_incidents ()
  local summaries = archive.list()
  local name = 'skg://maintenance-incidents'
  local buf = vim.fn.bufnr(name)
  if buf < 0 then
    buf = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(buf, name)
  end
  vim.bo[buf].modifiable = true
  vim.bo[buf].buftype = 'nofile'
  vim.bo[buf].bufhidden = 'hide'
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = 'org'
  local lines = {
    '* Retained Skg maintenance incidents', '',
    '| Time (UTC) | Origin | Status | Nodes | Interrupted | Released | Client | G0/G1 | Bytes | Undo |',
    '|-',
  }
  local rows = {}
  for _, summary in ipairs(summaries) do
    table.insert(lines, summary_line(summary))
    rows[#lines] = summary
  end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.bo[buf].modified = false
  list_rows[buf] = rows
  if not vim.b[buf].skg_recovery_list_keys then
    vim.b[buf].skg_recovery_list_keys = true
    vim.keymap.set('n', '<CR>', function () M.open_interrupted_view() end,
      { buffer = buf, desc = 'Open an interrupted recovery view' })
    vim.keymap.set('n', 'f',
      function () M.open_fresh_view_for_interrupted() end,
      { buffer = buf, desc = 'Open a fresh live view' })
    vim.keymap.set('n', 'd',
      function () M.delete_maintenance_incident() end,
      { buffer = buf, desc = 'Delete a terminal incident' })
    vim.keymap.set('n', 'r', function () M.list_maintenance_incidents() end,
      { buffer = buf, desc = 'Refresh retained incidents' })
  end
  vim.api.nvim_set_current_buf(buf)
  return buf
end

---Open a selected artifact read-only after re-verifying it.
function M.open_artifact (label)
  local context = recovery_contexts[vim.api.nvim_get_current_buf()]
  if not context then fail('current buffer is not detached recovery') end
  local function open (entry)
    if not entry then return end
    archive.verify_recorded_artifact(
      context.summary.path, entry.record, entry.context)
    local path = context.summary.path .. '/' .. entry.label
    vim.cmd('view ' .. vim.fn.fnameescape(path))
    vim.bo.readonly = true
    vim.bo.modifiable = false
  end
  if label then
    for _, entry in ipairs(context.artifact_links) do
      if entry.label == label then return open(entry) end end
    fail('unknown recovery artifact ' .. tostring(label))
  end
  vim.ui.select(context.artifact_links, {
    prompt = 'Recovery artifact',
    format_item = function (entry) return entry.label end,
  }, open)
end

local function recipe_value (recipe, key)
  if not sexpr.is_list(recipe) then return nil end
  for _, entry in ipairs(recipe) do
    if sexpr.is_pair(entry) and sexpr.atom_text(entry.car) == key then
      return entry.cdr end
    if sexpr.is_list(entry) and #entry >= 2
       and not sexpr.is_list(entry[1]) and not sexpr.is_pair(entry[1])
       and sexpr.atom_text(entry[1]) == key then
      return entry[2] end
  end
end

local function truthy (value)
  return value == true or (value ~= nil and sexpr.atom_text(value) == 'true')
end

local function root_ids (record, recipe)
  local result, seen = {}, {}
  local function add (value)
    if value ~= nil and not sexpr.is_list(value) and not sexpr.is_pair(value) then
      local text = sexpr.atom_text(value)
      if text ~= '' and not seen[text] then
        seen[text] = true
        table.insert(result, text) end
    end
  end
  add(recipe_value(recipe, 'root-id'))
  for _, value in ipairs(required_list(record, 'root-ids', 'initial buffer')) do
    add(value) end
  return result
end

---Open a current live view/search from an interrupted buffer's recipe.
function M.open_fresh_view_for_interrupted (incident, buffer_key)
  return with_target(incident, buffer_key, function (summary, record)
    local kind = required_text(record, 'kind', 'initial buffer')
    local recipe = parse_exact(
      required_text(record, 'recipe', 'initial buffer'), 'archived view recipe')
    if kind == 'content-view' then
      local roots = root_ids(record, recipe)
      if #roots == 0 then fail('archive records no content-view root') end
      local function open (root)
        if root then
          require('skg.content_view')
            .request_single_root_content_view_from_id(root) end
      end
      if #roots == 1 then return open(roots[1]) end
      return M.root_picker(roots, 'Fresh live root', open)
    elseif kind == 'search-view' then
      local terms = recipe_value(recipe, 'terms')
      if terms == nil then fail('archive records no search terms') end
      terms = sexpr.atom_text(terms)
      if not M.confirm(string.format(
        'Rerun archived search %q against current data?', terms)) then
        fail('fresh search cancelled') end
      return require('skg.search').request_text_search(
        terms, truthy(recipe_value(recipe, 'regex')),
        truthy(recipe_value(recipe, 'body')),
        truthy(recipe_value(recipe, 'operators')),
        recipe_value(recipe, 'ugly-choice'))
    end
    fail('fresh recovery is unsupported for archived kind ' .. kind)
  end)
end

local function active_incident_id ()
  local incident = state.maintenance_client_incident
  if incident and incident.incident_id then return incident.incident_id end
  local summary = state.maintenance_state
  return summary and (summary.incident_id or summary.active_incident_id) or nil
end

---Delete one terminal immediate-child incident after explicit confirmation.
function M.delete_maintenance_incident (incident, confirmed)
  return with_incident(incident, function (summary)
    if summary.status ~= 'finalized'
       or required_text(summary.final, 'terminal-status', 'final manifest')
         ~= 'completed' then
      fail('only terminal finalized maintenance incidents can be deleted') end
    local root = archive.resolve_archive_root()
    local path = vim.fs.normalize(vim.fn.fnamemodify(summary.path, ':p'))
    local stat = vim.uv.fs_lstat(path)
    if vim.fs.dirname(path) ~= root or not stat or stat.type ~= 'directory' then
      fail('incident is not a safe immediate child of the archive root') end
    if summary.incident_id == active_incident_id() then
      fail('the active maintenance incident still needs this archive') end
    if not confirmed and not M.confirm(string.format(
      'Delete %s (%d bytes, %s)? This is ordinary deletion, not secure erase.',
      summary.name, summary.bytes, summary.iec)) then
      fail('incident deletion cancelled') end
    if vim.fn.delete(path, 'rf') ~= 0 or vim.uv.fs_lstat(path) then
      fail('ordinary incident deletion failed') end
    vim.notify('Deleted maintenance incident ' .. summary.name
      .. '; ordinary deletion may remain recoverable from backups or storage')
    return true
  end)
end

-- Test-only visibility into the otherwise deliberately private detached state.
function M._context (buf) return recovery_contexts[buf] end

return M
