-- Private, portable recovery-archive publication for the Neovim client.
--
-- Nothing in this module mutates Git, source files, or server state.  It
-- publishes a complete initial incident under the configured private archive
-- root and returns the exact manifest checksum which the server must ACK
-- before any risky maintenance step may begin.

local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

M.archive_format_version = 1
M.native_undo_save = function (...)
  return require('skg.undo_sidecar').save(...) end
M.fs_write = function (...) return vim.uv.fs_write(...) end

local function fail (message)
  error('skg recovery archive failed: ' .. message, 0)
end

local function field (name, value)
  return { sexpr.symbol(name), value }
end

local function canonical_quote (value)
  value = value:gsub('\\', '\\\\'):gsub('"', '\\"')
  value = value:gsub('\n', '\\n'):gsub('\t', '\\t')
  return '"' .. value .. '"'
end

---Render the deliberately small portable manifest grammar.
---Only proper lists, symbols, strings, and integers are admitted.
function M.canonical_sexpr (value)
  if sexpr.is_symbol(value) then return value.name end
  if type(value) == 'string' then return canonical_quote(value) end
  if type(value) == 'number' and value == math.floor(value) then
    return string.format('%d', value) end
  if sexpr.is_list(value) then
    local count, maximum = 0, 0
    for key in pairs(value) do
      if type(key) ~= 'number' or key < 1 or key ~= math.floor(key) then
        fail('manifest list contains a non-array key: ' .. tostring(key)) end
      count = count + 1
      maximum = math.max(maximum, key)
    end
    if count ~= maximum then fail('manifest list contains an array hole') end
    local rendered = {}
    for _, item in ipairs(value) do
      table.insert(rendered, M.canonical_sexpr(item)) end
    return '(' .. table.concat(rendered, ' ') .. ')' end
  fail('manifest contains an unsupported value: ' .. vim.inspect(value))
end

local function absolute (path)
  return vim.fs.normalize(vim.fn.fnamemodify(path, ':p'))
end

local function beneath (child, parent)
  return child == parent or child:sub(1, #parent + 1) == parent .. '/'
end

local function mode_bits (stat)
  return stat.mode % 512
end

local function require_directory (path, mode, description)
  local stat = vim.uv.fs_lstat(path)
  if not stat or stat.type ~= 'directory' then
    fail(description .. ' is not a directory: ' .. path) end
  if mode and mode_bits(stat) ~= mode then
    fail(string.format('%s has mode %04o, expected %04o: %s',
      description, mode_bits(stat), mode, path)) end
  return stat
end

local function reject_parent_components (path)
  for component in path:gmatch('[^/\\]+') do
    if component == '..' then
      fail("maintenance_archive_folder may not contain '..'") end
  end
end

local function mkdir_private (path, allow_existing)
  local stat = vim.uv.fs_lstat(path)
  if stat then
    if not allow_existing then fail('archive directory already exists: ' .. path) end
    if stat.type ~= 'directory' then fail('archive path is not a directory: ' .. path) end
    if mode_bits(stat) ~= 448 then
      local ok, error_text = vim.uv.fs_chmod(path, 448)
      if not ok then fail('cannot make archive directory private: ' .. error_text) end
      require_directory(path, 448, 'archive directory')
    end
    return
  end
  local ok, error_text = vim.uv.fs_mkdir(path, 448)
  if not ok then fail('cannot create archive directory: ' .. error_text) end
  require_directory(path, 448, 'new archive directory')
end

local function create_root_without_following_missing_components (raw_path)
  local target = absolute(raw_path)
  local missing = {}
  local cursor = target
  while not vim.uv.fs_lstat(cursor) do
    table.insert(missing, 1, vim.fs.basename(cursor))
    local parent = vim.fs.dirname(cursor)
    if parent == cursor then fail('no existing parent for archive root') end
    cursor = parent
  end
  local real_parent = vim.uv.fs_realpath(cursor)
  if not real_parent then fail('cannot resolve archive-root parent: ' .. cursor) end
  local built = vim.fs.normalize(real_parent)
  for _, component in ipairs(missing) do
    built = built .. '/' .. component
    mkdir_private(built, false)
  end
  if #missing == 0 then
    local stat = vim.uv.fs_lstat(target)
    if not stat or stat.type ~= 'directory' then
      fail('archive root is not a directory: ' .. target) end
    local resolved = vim.uv.fs_realpath(target)
    if not resolved then fail('cannot resolve archive root: ' .. target) end
    built = vim.fs.normalize(resolved)
    mkdir_private(built, true)
  end
  return built
end

local function local_source_identities ()
  local config = require('skg.config')
  local config_file = config.config_file()
  if not config_file then fail('no local skgconfig.toml is active') end
  local identities = {}
  for _, source in ipairs(config.source_paths_from_toml(config_file)) do
    local identity = vim.uv.fs_realpath(source.path) or absolute(source.path)
    table.insert(identities, {
      name = source.name, path = vim.fs.normalize(identity),
    })
  end
  return identities, config_file
end

---Resolve and validate the client-visible archive root.
---@param configured string|nil raw config spelling; defaults to handshake
---@return string canonical client-side root
function M.resolve_archive_root (configured)
  configured = configured or state.maintenance_archive_folder
  if not configured or configured == '' then
    fail('the server supplied no maintenance archive folder') end
  reject_parent_components(configured)
  local sources, config_file = local_source_identities()
  local proposed = configured
  if not proposed:match('^/') then
    proposed = vim.fs.dirname(config_file) .. '/' .. proposed end
  proposed = absolute(proposed)
  for _, source in ipairs(sources) do
    if beneath(proposed, source.path) or beneath(source.path, proposed) then
      fail(string.format("archive root and source '%s' overlap", source.name)) end
  end
  local root = create_root_without_following_missing_components(proposed)
  for _, source in ipairs(sources) do
    if beneath(root, source.path) or beneath(source.path, root) then
      fail(string.format("resolved archive root and source '%s' overlap", source.name)) end
  end
  require_directory(root, 448, 'archive root')
  return root
end

local function assert_confined_parent (path, incident_root)
  local target = absolute(path)
  local root = absolute(incident_root)
  if target:sub(1, #root + 1) ~= root .. '/' then
    fail('artifact path escapes incident: ' .. target) end
  local parent = vim.fs.dirname(target)
  local relative = parent:sub(#root + 2)
  local cursor = root
  require_directory(cursor, 448, 'incident directory')
  for component in relative:gmatch('[^/]+') do
    cursor = cursor .. '/' .. component
    require_directory(cursor, 448, 'incident path component')
  end
  return target
end

local function close_descriptor (descriptor, description)
  local ok, error_text = vim.uv.fs_close(descriptor)
  if not ok then fail('cannot close ' .. description .. ': ' .. error_text) end
end

local function write_private_file (path, bytes, incident_root)
  path = assert_confined_parent(path, incident_root)
  if vim.uv.fs_lstat(path) then fail('artifact already exists: ' .. path) end
  local descriptor, open_error = vim.uv.fs_open(path, 'wx', 384)
  if not descriptor then fail('cannot create artifact: ' .. open_error) end
  local offset = 0
  local ok, write_error = xpcall(function ()
    while offset < #bytes do
      local written, error_text = M.fs_write(
        descriptor, bytes:sub(offset + 1), offset)
      if not written then fail('artifact write failed: ' .. error_text) end
      if written <= 0 then fail('artifact write made no progress') end
      offset = offset + written
    end
    if offset ~= #bytes then fail('artifact short write') end
    local synced, sync_error = vim.uv.fs_fsync(descriptor)
    if not synced then fail('cannot sync artifact: ' .. sync_error) end
  end, debug.traceback)
  local close_ok, close_error = pcall(close_descriptor, descriptor, path)
  if not ok then error(write_error, 0) end
  if not close_ok then error(close_error, 0) end
  local stat = vim.uv.fs_lstat(path)
  if not stat or stat.type ~= 'file' or mode_bits(stat) ~= 384 then
    fail('new artifact is not a private regular file: ' .. path) end
  if stat.size ~= #bytes then fail('artifact length changed after close: ' .. path) end
end

local function read_regular_file (path)
  local stat = vim.uv.fs_lstat(path)
  if not stat or stat.type ~= 'file' then
    fail('artifact is not a regular file: ' .. path) end
  local handle, open_error = io.open(path, 'rb')
  if not handle then fail('cannot reopen artifact: ' .. open_error) end
  local bytes = handle:read('*a')
  local ok, close_error = handle:close()
  if not ok then fail('cannot close reopened artifact: ' .. close_error) end
  if #bytes ~= stat.size then fail('artifact changed while being read: ' .. path) end
  return bytes, stat
end

local function artifact_record (path, incident_root)
  local bytes = read_regular_file(path)
  return {
    field('path', absolute(path):sub(#absolute(incident_root) + 2)),
    field('bytes', #bytes),
    field('sha256', vim.fn.sha256(bytes)),
  }
end

local function sync_directory (path)
  local descriptor, open_error = vim.uv.fs_open(path, 'r', 0)
  if not descriptor then fail('cannot open directory for sync: ' .. open_error) end
  local ok, sync_error = vim.uv.fs_fsync(descriptor)
  local close_ok, close_error = vim.uv.fs_close(descriptor)
  if not ok then fail('cannot sync directory: ' .. sync_error) end
  if not close_ok then fail('cannot close synced directory: ' .. close_error) end
end

local function strict_uuid (value)
  return type(value) == 'string' and value:match(
    '^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]%-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]%-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]%-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]%-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$') ~= nil
end

local function validate_archive_name (name, incident_id)
  local captured = type(name) == 'string' and name:match(
    '^%d%d%d%d%d%d%d%dT%d%d%d%d%d%d%.%d%d%d%d%d%dZ_([0-9a-f%-]+)$')
  if captured ~= incident_id then
    fail('invalid server-owned archive directory name: ' .. tostring(name)) end
end

local function new_nonce ()
  local bytes = vim.uv.random(16)
  if not bytes then fail('cannot obtain randomness for archive staging') end
  return vim.fn.sha256(bytes .. tostring(vim.uv.hrtime())):sub(1, 24)
end

local function safe_primary_root (roots)
  if #roots ~= 1 then return nil end
  local root = tostring(roots[1])
  if #root > 80 or root == '.' or root == '..'
     or not root:match('^[A-Za-z0-9][A-Za-z0-9._-]*$') then
    return nil end
  return root
end

local function portable_recipe_value (value)
  local value_type = type(value)
  if value == nil then return 'none' end
  if value_type == 'string' or value_type == 'number' then return value end
  if value_type == 'boolean' then return value and 'true' or 'false' end
  if sexpr.is_symbol(value) then return { sexpr.symbol('symbol'), value.name } end
  if sexpr.is_pair(value) then
    return { sexpr.symbol('pair'), portable_recipe_value(value.car),
             portable_recipe_value(value.cdr) } end
  if value_type ~= 'table' then
    fail('recipe contains unsupported ' .. value_type) end
  local count, maximum, map = 0, 0, false
  for key in pairs(value) do
    if type(key) ~= 'number' or key < 1 or key ~= math.floor(key) then
      map = true
    else
      count = count + 1
      maximum = math.max(maximum, key)
    end
  end
  if not map and count == maximum then
    local result = {}
    for _, item in ipairs(value) do
      table.insert(result, portable_recipe_value(item)) end
    return result
  end
  local keys = {}
  for key in pairs(value) do
    table.insert(keys, { text = tostring(key), original = key }) end
  table.sort(keys, function (left, right) return left.text < right.text end)
  local result = {}
  for _, key in ipairs(keys) do
    table.insert(result, {
      key.text, portable_recipe_value(value[key.original]),
    }) end
  return result
end

local function recipe_text (recipe)
  local ok, result = pcall(
    M.canonical_sexpr, portable_recipe_value(recipe or {}))
  if not ok then fail('buffer recipe is not portable: ' .. tostring(result)) end
  return result
end

local function allocate_buffer_keys (buffers)
  local descriptors = {}
  for _, buf in ipairs(buffers) do
    local record = registry.record(buf)
    if not record then fail('attempted to archive an unregistered buffer') end
    table.insert(descriptors, { buf = buf, record = record })
  end
  table.sort(descriptors, function (left, right)
    return left.record.id < right.record.id end)
  local used = {}
  for index, descriptor in ipairs(descriptors) do
    local record = descriptor.record
    local identity = table.concat({
      record.id, record.kind or '', record.view_uri or '',
      table.concat(record.root_ids or {}, '\0'), recipe_text(record.recipe),
    }, '\0')
    local prefix = safe_primary_root(record.root_ids or {})
                   or ('view-' .. tostring(index))
    local base = prefix .. '_' .. vim.fn.sha256(identity):sub(1, 12)
    local key, suffix = base, 1
    while used[key] do
      suffix = suffix + 1
      key = base .. '-' .. tostring(suffix) end
    used[key] = true
    descriptor.key = key
  end
  return descriptors
end

local function window_state (buf)
  local windows = {}
  local ordinal = 0
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_buf(win) == buf then
      ordinal = ordinal + 1
      table.insert(windows, vim.api.nvim_win_call(win, function ()
        local view = vim.fn.winsaveview()
        local folds = {}
        local line = 1
        while line <= vim.api.nvim_buf_line_count(buf) do
          local first = vim.fn.foldclosed(line)
          if first == line then
            local last = vim.fn.foldclosedend(line)
            table.insert(folds, { first, last })
            line = last + 1
          else line = line + 1 end
        end
        return {
          field('ordinal', ordinal),
          field('cursor-row', view.lnum),
          field('cursor-column', view.col),
          field('topline', view.topline),
          field('closed-folds', folds),
        }
      end))
    end
  end
  return windows
end

local function unified_diff (old_path, new_path)
  local result = vim.system({
    'diff', '-U', '3', '--label', 'last-fetched.org',
    '--label', 'unsaved-changes.org', old_path, new_path,
  }, { text = false }):wait()
  if result.code ~= 0 and result.code ~= 1 then
    fail('diff failed: ' .. (result.stderr or ('exit ' .. result.code))) end
  return (result.stdout or ''):gsub('\r\n', '\n')
end

local function undo_fields (result)
  local fields = {
    field('status', tostring(result.status)),
    field('kind', tostring(result.kind)),
    field('version', tostring(result.version or 'unknown')),
  }
  for _, key in ipairs({ 'header_identity', 'validation', 'bytes', 'sha256',
                         'reason' }) do
    if result[key] ~= nil then
      table.insert(fields, field(key:gsub('_', '-'), result[key])) end end
  return fields
end

local function root_ids_value (roots)
  local result = {}
  for _, root in ipairs(roots or {}) do table.insert(result, tostring(root)) end
  return result
end

local function buffer_metadata (descriptor, undo, windows)
  local record = descriptor.record
  return {
    field('archive-format-version', M.archive_format_version),
    field('buffer-key', descriptor.key),
    field('buffer-id', record.id),
    field('kind', record.kind),
    field('lifecycle', record.lifecycle or 'unknown'),
    field('name', vim.api.nvim_buf_get_name(descriptor.buf)),
    field('view-uri', record.view_uri or 'none'),
    field('root-ids', root_ids_value(record.root_ids)),
    field('recipe', recipe_text(record.recipe)),
    field('source-set', record.source_set or 'all'),
    field('graph-generation', record.graph_generation or 0),
    field('presentation-generation', record.presentation_generation or 0),
    field('server-revision', record.server_revision or 0),
    field('application-token', record.application_token or 0),
    field('dirty', registry.dirty(descriptor.buf) and 'true' or 'nil'),
    field('logical-dirty', record.logical_dirty and 'true' or 'nil'),
    field('point', windows[1] or {}),
    field('windows', windows),
    field('undo', undo_fields(undo)),
    field('initial-disposition', 'pending-classification'),
  }
end

local function buffer_readme (descriptor, undo)
  local record = descriptor.record
  return table.concat({
    '* Skg recovery snapshot',
    '',
    'This directory preserves a buffer exactly as it existed when maintenance began.',
    '',
    '- Buffer key: =' .. descriptor.key .. '=',
    '- Buffer kind: =' .. tostring(record.kind) .. '=',
    '- Original editor name: =' .. vim.api.nvim_buf_get_name(descriptor.buf) .. '=',
    '- Native undo status: =' .. tostring(undo.status) .. '=',
    '',
    'Open =unsaved-changes.org= for the authored text.  =last-fetched.org=',
    'is its exact server-rendered base and =diff.txt= is the portable fallback.',
    '',
  }, '\n')
end

local function snapshot_one_buffer (descriptor, staging, waiver_reason)
  local buf, record = descriptor.buf, descriptor.record
  if record.maintenance_epoch == nil then
    fail('buffer ' .. record.id .. ' is not maintenance-locked') end
  if not registry.dirty(buf) then
    fail('buffer ' .. record.id .. ' is no longer dirty') end
  local directory = staging .. '/buffer-snapshots/' .. descriptor.key
  mkdir_private(directory, false)
  local last_path = directory .. '/last-fetched.org'
  local current_path = directory .. '/unsaved-changes.org'
  local diff_path = directory .. '/diff.txt'
  local metadata_path = directory .. '/metadata.sexp'
  local readme_path = directory .. '/README.org'
  local last_fetched = record.last_fetched
  if type(last_fetched) ~= 'string' then
    fail('buffer ' .. record.id .. ' has no exact last-fetched text') end
  local current = registry.raw_text(buf)
  write_private_file(last_path, last_fetched, staging)
  write_private_file(current_path, current, staging)
  write_private_file(diff_path, unified_diff(last_path, current_path), staging)

  local undo
  if waiver_reason then
    undo = {
      status = 'undo-unavailable-approved', kind = 'nvim-wundo',
      version = string.format('%d.%d.%d',
        vim.version().major, vim.version().minor, vim.version().patch),
      reason = waiver_reason,
    }
  else
    local ok, result = pcall(M.native_undo_save,
      buf, current_path, directory .. '/undo.nvim', staging)
    if not ok then
      error({
        kind = 'native-undo-failure',
        buffer_id = record.id,
        buffer_key = descriptor.key,
        reason = tostring(result),
      }, 0) end
    undo = result
  end

  local windows = window_state(buf)
  local metadata = buffer_metadata(descriptor, undo, windows)
  write_private_file(metadata_path,
    M.canonical_sexpr(metadata) .. '\n', staging)
  write_private_file(readme_path, buffer_readme(descriptor, undo), staging)

  if registry.raw_text(buf) ~= current then
    fail('buffer changed while its archive was being written: ' .. record.id) end
  local artifacts = {}
  for _, path in ipairs({ readme_path, metadata_path, last_path,
                          current_path, diff_path }) do
    table.insert(artifacts, artifact_record(path, staging)) end
  if undo.status == 'archived' then
    table.insert(artifacts,
      artifact_record(directory .. '/undo.nvim', staging)) end
  return {
    field('buffer-key', descriptor.key),
    field('buffer-id', record.id),
    field('kind', record.kind),
    field('name', vim.api.nvim_buf_get_name(buf)),
    field('view-uri', record.view_uri or 'none'),
    field('root-ids', root_ids_value(record.root_ids)),
    field('recipe', recipe_text(record.recipe)),
    field('graph-generation', record.graph_generation or 0),
    field('presentation-generation', record.presentation_generation or 0),
    field('server-revision', record.server_revision or 0),
    field('application-token', record.application_token or 0),
    field('undo', undo_fields(undo)),
    field('artifacts', artifacts),
    field('initial-disposition', 'pending-classification'),
  }
end

local function incident_readme (offer, descriptors)
  return table.concat({
    '* Skg maintenance recovery incident',
    '',
    'Incident =' .. offer.incident_id .. '= began for =' .. offer.origin .. '=.',
    'Its initial archive protects ' .. tostring(#descriptors) .. ' dirty buffer(s).',
    '',
    'No source file, Git state, or selected Skg store is changed by this archive.',
    'The server may cross the maintenance point of no return only after it ACKs',
    'the exact checksum in =ARCHIVE-READY=.',
    '',
  }, '\n')
end

local function manifest_value (offer, root, buffer_records, root_artifacts)
  local client_version = vim.version()
  return {
    field('archive-format-version', M.archive_format_version),
    field('manifest-kind', 'initial'),
    field('incident-id', offer.incident_id),
    field('maintenance-epoch', offer.epoch),
    field('origin', offer.origin),
    field('started-at-utc', offer.started_at_utc),
    field('archive-directory-name', offer.archive_name),
    field('client-kind', 'neovim'),
    field('client-version', string.format('%d.%d.%d',
      client_version.major, client_version.minor, client_version.patch)),
    field('client-session-id', state.client_session_id),
    field('client-archive-identity', root),
    field('server-archive-identity',
      state.maintenance_archive_identity or 'unavailable'),
    field('source-set', offer.source_set or state.active_source_set_name or 'all'),
    field('g0-graph-generation', offer.graph_generation or 0),
    field('g0-manifest-revision', offer.manifest_revision or 0),
    field('directory-sync', 'libuv-fsync'),
    field('artifacts', root_artifacts),
    field('buffers', buffer_records),
    field('initial-status', 'prepared-for-publication'),
  }
end

local function each_directory_postorder (root, callback)
  local scanner, scan_error = vim.uv.fs_scandir(root)
  if not scanner then fail('cannot scan archive directory: ' .. scan_error) end
  while true do
    local name, kind = vim.uv.fs_scandir_next(scanner)
    if not name then break end
    local path = root .. '/' .. name
    local stat = vim.uv.fs_lstat(path)
    if not stat or stat.type ~= kind then
      fail('archive entry changed during traversal: ' .. path) end
    if kind == 'link' then fail('archive contains a symlink: ' .. path) end
    if kind == 'directory' then each_directory_postorder(path, callback)
    elseif kind ~= 'file' then fail('archive contains a non-regular entry: ' .. path) end
  end
  callback(root)
end

local function marker_value (offer, manifest_sha256)
  return {
    field('archive-format-version', M.archive_format_version),
    field('incident-id', offer.incident_id),
    field('manifest-sha256', manifest_sha256),
  }
end

local function valid_final_name (name)
  local incident = name:match(
    '^%d%d%d%d%d%d%d%dT%d%d%d%d%d%d%.%d%d%d%d%d%dZ_(.+)$')
  return incident ~= nil and strict_uuid(incident)
end

local function tree_bytes (root)
  local total = 0
  local function walk (directory)
    local scanner, scan_error = vim.uv.fs_scandir(directory)
    if not scanner then fail('cannot size archive: ' .. scan_error) end
    while true do
      local name, kind = vim.uv.fs_scandir_next(scanner)
      if not name then break end
      local path = directory .. '/' .. name
      local stat = vim.uv.fs_lstat(path)
      if not stat or stat.type ~= kind then
        fail('archive entry changed during size walk: ' .. path) end
      if kind == 'link' then fail('archive size walk found symlink: ' .. path) end
      if kind == 'directory' then walk(path)
      elseif kind == 'file' then total = total + stat.size
      else fail('archive size walk found special entry: ' .. path) end
    end
  end
  walk(root)
  return total
end

local function iec_size (bytes)
  local units = { 'B', 'KiB', 'MiB', 'GiB', 'TiB' }
  local value, unit = bytes, 1
  while value >= 1024 and unit < #units do
    value = value / 1024
    unit = unit + 1
  end
  if unit == 1 then return tostring(bytes) .. ' B' end
  return string.format('%.1f %s', value, units[unit])
end

function M.size_report (root, incident_path)
  local incident_bytes = tree_bytes(incident_path)
  local retained_bytes, retained_count = 0, 0
  local scanner, scan_error = vim.uv.fs_scandir(root)
  if not scanner then fail('cannot list retained archives: ' .. scan_error) end
  while true do
    local name, kind = vim.uv.fs_scandir_next(scanner)
    if not name then break end
    if valid_final_name(name) then
      if kind ~= 'directory' then
        fail('strict incident name is not a directory: ' .. name) end
      retained_count = retained_count + 1
      retained_bytes = retained_bytes + tree_bytes(root .. '/' .. name)
    end
  end
  return {
    incident_bytes = incident_bytes,
    incident_iec = iec_size(incident_bytes),
    retained_bytes = retained_bytes,
    retained_iec = iec_size(retained_bytes),
    retained_count = retained_count,
  }
end

---Publish the initial recovery incident for already maintenance-locked BUFFERS.
---OFFER carries server-owned incident/epoch/time/name identities.  OPTIONS may
---supply a deterministic client_nonce and undo_waivers keyed by buffer ID.
function M.publish_initial (offer, buffers, options)
  options = options or {}
  if not strict_uuid(offer.incident_id) then
    fail('invalid server-owned incident UUID') end
  if type(offer.epoch) ~= 'number' or offer.epoch < 1
     or offer.epoch ~= math.floor(offer.epoch) then
    fail('invalid maintenance epoch') end
  validate_archive_name(offer.archive_name, offer.incident_id)
  if type(offer.origin) ~= 'string' or offer.origin == ''
     or type(offer.started_at_utc) ~= 'string' then
    fail('maintenance offer lacks origin or start time') end

  local root = M.resolve_archive_root(options.archive_root)
  local staging_root = root .. '/.staging'
  mkdir_private(staging_root, true)
  local nonce = options.client_nonce or new_nonce()
  if not nonce:match('^[0-9a-f]+$') or #nonce < 16 or #nonce > 64 then
    fail('invalid client staging nonce') end
  local staging_name = offer.incident_id .. '.' .. nonce .. '.partial'
  local staging = staging_root .. '/' .. staging_name
  local final = root .. '/' .. offer.archive_name
  if vim.uv.fs_lstat(final) then fail('final incident path already exists') end
  mkdir_private(staging, false)
  mkdir_private(staging .. '/buffer-snapshots', false)
  mkdir_private(staging .. '/interrupted-buffers', false)

  buffers = buffers or registry.buffers()
  local dirty = {}
  for _, buf in ipairs(buffers) do
    if registry.dirty(buf) then table.insert(dirty, buf) end end
  local descriptors = allocate_buffer_keys(dirty)
  local buffer_records = {}
  local waivers = options.undo_waivers or {}
  for _, descriptor in ipairs(descriptors) do
    local record = descriptor.record
    if record.maintenance_epoch ~= offer.epoch then
      fail(string.format('buffer %s is not locked for epoch %d',
        record.id, offer.epoch)) end
    table.insert(buffer_records, snapshot_one_buffer(
      descriptor, staging, waivers[descriptor.key] or waivers[record.id]))
  end

  local interrupted_readme = staging .. '/interrupted-buffers/README.org'
  local incident_path = staging .. '/incident.org'
  write_private_file(interrupted_readme,
    '* Interrupted buffers\n\nNo buffer disposition has been selected yet.\n',
    staging)
  write_private_file(incident_path,
    incident_readme(offer, descriptors), staging)
  local root_artifacts = {
    artifact_record(incident_path, staging),
    artifact_record(interrupted_readme, staging),
  }
  local manifest_text = M.canonical_sexpr(
    manifest_value(offer, root, buffer_records, root_artifacts)) .. '\n'
  local manifest_path = staging .. '/manifest.initial.sexp'
  write_private_file(manifest_path, manifest_text, staging)
  local reread_manifest = read_regular_file(manifest_path)
  if reread_manifest ~= manifest_text then
    fail('initial manifest changed after durable write') end
  local manifest_sha256 = vim.fn.sha256(reread_manifest)

  each_directory_postorder(staging, sync_directory)
  local renamed, rename_error = vim.uv.fs_rename(staging, final)
  if not renamed then fail('cannot publish incident directory: ' .. rename_error) end
  sync_directory(staging_root)
  sync_directory(root)

  local marker_text = M.canonical_sexpr(
    marker_value(offer, manifest_sha256)) .. '\n'
  local marker_temp = final .. '/.ARCHIVE-READY.' .. nonce .. '.tmp'
  write_private_file(marker_temp, marker_text, final)
  local marker_final = final .. '/ARCHIVE-READY'
  local marker_renamed, marker_error = vim.uv.fs_rename(
    marker_temp, marker_final)
  if not marker_renamed then fail('cannot publish ARCHIVE-READY: ' .. marker_error) end
  sync_directory(final)
  sync_directory(root)
  if read_regular_file(final .. '/manifest.initial.sexp') ~= manifest_text
     or read_regular_file(marker_final) ~= marker_text then
    fail('published archive failed final checksum reread') end

  local sizes = M.size_report(root, final)
  vim.notify(string.format(
    'Recovery archive ready: %d bytes (%s); %d retained incident(s), %d bytes (%s) total',
    sizes.incident_bytes, sizes.incident_iec, sizes.retained_count,
    sizes.retained_bytes, sizes.retained_iec))
  return {
    path = final,
    archive_name = offer.archive_name,
    manifest_sha256 = manifest_sha256,
    sizes = sizes,
  }
end

return M
