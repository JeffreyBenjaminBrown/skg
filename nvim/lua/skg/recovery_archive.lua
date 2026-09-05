-- Private, portable recovery-archive publication for the Neovim client.
--
-- Nothing in this module mutates Git, source files, or server state.  It
-- publishes a complete initial incident under the configured private archive
-- root and returns the exact manifest checksum which the server must ACK
-- before any risky maintenance step may begin.

local registry = require('skg.buffer_registry')
local payload = require('skg.payload')
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

local function recipe_text (recipe)
  local ok, result = pcall(registry.recipe_text, recipe)
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
    field('disposable', record.disposable and 'true' or 'nil'),
    field('continuation-id', record.continuation_id or 'none'),
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
    field('maintenance-epoch', record.maintenance_epoch),
    field('presentation-stale', record.presentation_stale and 'true' or 'nil'),
    field('search-stale', record.search_stale and 'true' or 'nil'),
    field('herald-bearing', vim.b[descriptor.buf].skg_herald_bearing == true
      and 'true' or 'nil'),
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
    field('lifecycle', record.lifecycle or 'unknown'),
    field('disposable', record.disposable and 'true' or 'nil'),
    field('continuation-id', record.continuation_id or 'none'),
    field('name', vim.api.nvim_buf_get_name(buf)),
    field('view-uri', record.view_uri or 'none'),
    field('root-ids', root_ids_value(record.root_ids)),
    field('recipe', recipe_text(record.recipe)),
    field('source-set', record.source_set or 'all'),
    field('graph-generation', record.graph_generation or 0),
    field('presentation-generation', record.presentation_generation or 0),
    field('server-revision', record.server_revision or 0),
    field('application-token', record.application_token or 0),
    field('dirty', registry.dirty(buf) and 'true' or 'nil'),
    field('logical-dirty', record.logical_dirty and 'true' or 'nil'),
    field('maintenance-epoch', record.maintenance_epoch),
    field('presentation-stale', record.presentation_stale and 'true' or 'nil'),
    field('search-stale', record.search_stale and 'true' or 'nil'),
    field('herald-bearing', vim.b[buf].skg_herald_bearing == true
      and 'true' or 'nil'),
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
      offer.archive_identity or state.maintenance_archive_identity
        or 'unavailable'),
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

local evidence_categories = {
  'new-nodes', 'deleted-nodes', 'modified-nodes', 'invalid-paths',
}

local evidence_category_set = {}
for _, category in ipairs(evidence_categories) do
  evidence_category_set[category] = true end

local function required_field (value, key, context)
  if not sexpr.is_list(value) then
    fail(context .. ' is not a field list') end
  local result = payload.field(value, key)
  if result == nil then fail(context .. ' lacks ' .. key) end
  return result
end

local function required_text (value, key, context)
  local result = required_field(value, key, context)
  -- The shared wire reader represents both the bare atom `nil' and () as
  -- its singleton empty list.  Boolean false fields emitted by the server
  -- therefore arrive as this value even though they are scalar authority.
  if sexpr.is_nil(result) then return 'nil' end
  if sexpr.is_list(result) or sexpr.is_pair(result) then
    fail(context .. ' has non-atomic ' .. key) end
  return sexpr.atom_text(result)
end

local function required_integer (value, key, context)
  local result = required_field(value, key, context)
  if type(result) ~= 'number' or result < 0 or result ~= math.floor(result)
     or result > 9007199254740991 then
    fail(context .. ' has invalid ' .. key) end
  return result
end

local function required_list (value, key, context)
  local result = required_field(value, key, context)
  if not sexpr.is_list(result) then
    fail(context .. ' has malformed ' .. key .. ' list') end
  return result
end

local function sha256_valid (value)
  return type(value) == 'string' and #value == 64
    and value:match('^[0-9a-f]+$') ~= nil
end

local function read_exact_sexpr (path)
  local bytes = read_regular_file(path)
  local ok, value, position = pcall(sexpr.read, bytes)
  if not ok then fail('invalid machine record ' .. path .. ': ' .. value) end
  if not bytes:sub(position):match('^%s*$') then
    fail('machine record has trailing data: ' .. path) end
  return value
end

local function safe_evidence_components (relative)
  if type(relative) ~= 'string' or relative:sub(1, 1) == '/'
     or relative:find('\\', 1, true) then
    fail('unsafe evidence path: ' .. tostring(relative)) end
  local components = {}
  for component in (relative .. '/'):gmatch('(.-)/') do
    table.insert(components, component) end
  if #components < 3 or table.concat(components, '/') ~= relative
     or not evidence_category_set[components[1]] then
    fail('unsafe evidence path: ' .. relative) end
  local prefix, digits, digest = components[2]:match(
    '^(node%-)(%d+)%-(%x+)$')
  if not prefix then
    prefix, digits, digest = components[2]:match(
      '^(path%-)(%d+)%-(%x+)$') end
  if not prefix or #digits ~= 8 or #digest ~= 12
     or not digest:match('^[0-9a-f]+$') then
    fail('unsafe evidence path: ' .. relative) end
  for _, component in ipairs(components) do
    if component == '' or component == '.' or component == '..'
       or not component:match('^[A-Za-z0-9._-]+$') then
      fail('unsafe evidence path: ' .. relative) end
  end
  return components
end

local function parse_evidence_bundle (descriptor, opaque_bytes)
  if type(opaque_bytes) ~= 'string' then
    fail('artifact bundle did not remain opaque bytes') end
  local context = 'maintenance evidence descriptor'
  local version = required_integer(
    descriptor, 'artifact-bundle-format-version', context)
  local count = required_integer(descriptor, 'artifact-count', context)
  local declared_bytes = required_integer(
    descriptor, 'artifact-bytes', context)
  local payload_sha = required_text(
    descriptor, 'artifact-bytes-sha256', context)
  local transfer_sha = required_text(
    descriptor, 'transfer-manifest-sha256', context)
  local wire_records = required_list(descriptor, 'artifacts', context)
  if version ~= 1 then
    fail('unsupported evidence bundle version: ' .. tostring(version)) end
  if not sha256_valid(payload_sha) or not sha256_valid(transfer_sha) then
    fail('evidence bundle has an invalid checksum') end
  if #wire_records ~= count or #opaque_bytes ~= declared_bytes
     or vim.fn.sha256(opaque_bytes) ~= payload_sha then
    fail('opaque evidence inventory/checksum does not match') end
  local seen_keys, seen_paths, records = {}, {}, {}
  local expected_offset = 0
  for index, wire_record in ipairs(wire_records) do
    local record_context = 'evidence artifact ' .. tostring(index - 1)
    local key = required_text(wire_record, 'artifact-key', record_context)
    local relative = required_text(
      wire_record, 'relative-path', record_context)
    local purpose = required_text(wire_record, 'purpose', record_context)
    local offset = required_integer(wire_record, 'byte-offset', record_context)
    local length = required_integer(wire_record, 'byte-length', record_context)
    local sha = required_text(wire_record, 'sha256', record_context)
    if key ~= string.format('artifact-%08d', index - 1) then
      fail('artifact key/order changed at ' .. tostring(index - 1)) end
    safe_evidence_components(relative)
    if purpose == '' or not sha256_valid(sha) or offset ~= expected_offset
       or length > #opaque_bytes - offset
       or seen_keys[key] or seen_paths[relative] then
      fail('invalid or overlapping ' .. record_context) end
    local bytes = opaque_bytes:sub(offset + 1, offset + length)
    if vim.fn.sha256(bytes) ~= sha then
      fail(record_context .. ' checksum mismatch') end
    seen_keys[key], seen_paths[relative] = true, true
    table.insert(records, {
      key = key, relative_path = relative, purpose = purpose,
      byte_offset = offset, byte_length = length, sha256 = sha, bytes = bytes,
    })
    expected_offset = offset + length
  end
  if expected_offset ~= #opaque_bytes then
    fail('artifact records do not consume the opaque body') end
  return {
    records = records,
    transfer_manifest_sha256 = transfer_sha,
    artifact_bytes_sha256 = payload_sha,
  }
end

local function ensure_private_relative_directory (root, relative)
  local cursor = root
  for component in relative:gmatch('[^/]+') do
    cursor = cursor .. '/' .. component
    local stat = vim.uv.fs_lstat(cursor)
    if stat then require_directory(cursor, 448, 'evidence directory')
    else mkdir_private(cursor, false) end
  end
  return cursor
end

local function expected_evidence_directories (records, category)
  local expected = { [category] = true }
  for _, record in ipairs(records) do
    local accumulated = nil
    local parent = vim.fs.dirname(record.relative_path)
    for component in parent:gmatch('[^/]+') do
      accumulated = accumulated and (accumulated .. '/' .. component)
        or component
      expected[accumulated] = true
    end
  end
  return expected
end

local function verify_evidence_category (incident_root, category, records)
  local expected_files = {}
  for _, record in ipairs(records) do
    expected_files[record.relative_path] = record end
  local expected_directories = expected_evidence_directories(records, category)
  local seen_files = 0
  local function walk (directory)
    require_directory(directory, 448, 'evidence directory')
    local relative_directory = directory:sub(#incident_root + 2)
    if not expected_directories[relative_directory] then
      fail('undeclared evidence directory: ' .. relative_directory) end
    local scanner, scan_error = vim.uv.fs_scandir(directory)
    if not scanner then fail('cannot scan evidence directory: ' .. scan_error) end
    while true do
      local name, kind = vim.uv.fs_scandir_next(scanner)
      if not name then break end
      local path = directory .. '/' .. name
      local stat = vim.uv.fs_lstat(path)
      if not stat or stat.type ~= kind then
        fail('evidence entry changed during traversal: ' .. path) end
      if kind == 'link' then fail('evidence entry is a symlink: ' .. path)
      elseif kind == 'directory' then walk(path)
      elseif kind == 'file' then
        if mode_bits(stat) ~= 384 or (stat.nlink and stat.nlink ~= 1) then
          fail('evidence entry is not a private regular file: ' .. path) end
        local relative = path:sub(#incident_root + 2)
        local record = expected_files[relative]
        local bytes = record and read_regular_file(path) or nil
        if not record or #bytes ~= record.byte_length
           or vim.fn.sha256(bytes) ~= record.sha256 then
          fail('undeclared or changed evidence artifact: ' .. relative) end
        seen_files = seen_files + 1
      else fail('special evidence entry is forbidden: ' .. path) end
    end
  end
  walk(incident_root .. '/' .. category)
  if seen_files ~= #records then
    fail('evidence category ' .. category .. ' is incomplete') end
end

local function atom_list (value, key, context)
  local result = {}
  for _, item in ipairs(required_list(value, key, context)) do
    if sexpr.is_list(item) or sexpr.is_pair(item) then
      fail(context .. ' has a non-atomic ' .. key .. ' member') end
    table.insert(result, sexpr.atom_text(item))
  end
  return result
end

local function normalize_settlement_application (record, required_ack)
  local application = payload.field(record, 'application')
  if required_ack == 'application-ack' then
    if application == nil then
      fail('application settlement has no exact rendered identity') end
    local context = 'view application'
    return field('application', {
      field('content-sha256', required_text(
        application, 'content-sha256', context)),
      field('resulting-graph-generation', required_integer(
        application, 'resulting-graph-generation', context)),
      field('resulting-presentation-generation', required_integer(
        application, 'resulting-presentation-generation', context)),
      field('resulting-server-revision', required_integer(
        application, 'resulting-server-revision', context)),
      field('resulting-application-token', required_integer(
        application, 'resulting-application-token', context)),
    })
  elseif application ~= nil then
    fail('non-application settlement unexpectedly carries rendered identity')
  end
end

local function normalize_settlements (settlements, initial_buffers)
  if not sexpr.is_list(settlements) then
    fail('view settlements are not a proper list') end
  local valid_dispositions = {
    interrupted = true, ['released-unimpacted'] = true, refreshed = true,
    ['retained-clean'] = true, ['closed-disposable'] = true,
    ['detached-derived'] = true, ['maintenance-aborted'] = true, failed = true,
  }
  local valid_acks = {
    ['retirement-ack'] = true, ['release-ack'] = true,
    ['application-ack'] = true, ['close-ack'] = true,
  }
  local seen, normalized = {}, {}
  for _, record in ipairs(settlements) do
    local context = 'view settlement'
    local buffer_id = required_text(record, 'buffer-id', context)
    local buffer_key = required_text(record, 'buffer-key', context)
    local disposition = required_text(record, 'planned-disposition', context)
    local required_ack = required_text(record, 'required-ack', context)
    if seen[buffer_id] then
      fail('duplicate settlement for buffer ' .. buffer_id) end
    if not valid_dispositions[disposition] then
      fail('unknown buffer disposition: ' .. disposition) end
    if not valid_acks[required_ack] then
      fail('unknown settlement acknowledgement: ' .. required_ack) end
    seen[buffer_id] = buffer_key
    local normalized_record = {
      field('buffer-id', buffer_id),
      field('buffer-key', buffer_key),
      field('kind', required_text(record, 'kind', context)),
      field('view-uri', required_text(record, 'view-uri', context)),
      field('dirty', required_text(record, 'dirty', context)),
      field('impacted', required_text(record, 'impacted', context)),
      field('parse-uncertain', required_text(
        record, 'parse-uncertain', context)),
      field('observed-ids', atom_list(record, 'observed-ids', context)),
      field('resolved-primary-ids', atom_list(
        record, 'resolved-primary-ids', context)),
      field('base-graph-generation', required_integer(
        record, 'base-graph-generation', context)),
      field('base-presentation-generation', required_integer(
        record, 'base-presentation-generation', context)),
      field('base-server-revision', required_integer(
        record, 'base-server-revision', context)),
      field('base-application-token', required_integer(
        record, 'base-application-token', context)),
      field('disposition', disposition),
      field('required-ack', required_ack),
      field('acknowledged', 'true'),
    }
    local application = normalize_settlement_application(record, required_ack)
    if application then table.insert(normalized_record, application) end
    table.insert(normalized, normalized_record)
  end
  for _, buffer in ipairs(initial_buffers) do
    local buffer_id = required_text(buffer, 'buffer-id', 'initial buffer')
    local buffer_key = required_text(buffer, 'buffer-key', 'initial buffer')
    if seen[buffer_id] ~= buffer_key then
      fail('dirty buffer ' .. buffer_id .. ' has no exact final settlement') end
  end
  return normalized
end

local function final_artifact_record (record)
  return {
    field('artifact-key', record.key),
    field('path', record.relative_path),
    field('purpose', record.purpose),
    field('byte-offset', record.byte_offset),
    field('bytes', record.byte_length),
    field('sha256', record.sha256),
  }
end

local function final_manifest_value (
    initial, initial_sha, descriptor, evidence, settlements, root_artifacts)
  local context = 'maintenance evidence descriptor'
  local node_artifacts = {}
  for _, record in ipairs(evidence.records) do
    table.insert(node_artifacts, final_artifact_record(record)) end
  return {
    field('archive-format-version', M.archive_format_version),
    field('manifest-kind', 'final'),
    field('incident-id', required_text(descriptor, 'incident-id', context)),
    field('maintenance-epoch', required_integer(
      descriptor, 'maintenance-epoch', context)),
    field('initial-manifest-sha256', initial_sha),
    field('origin', required_text(initial, 'origin', 'initial manifest')),
    field('started-at-utc', required_text(
      initial, 'started-at-utc', 'initial manifest')),
    field('client-kind', required_text(
      initial, 'client-kind', 'initial manifest')),
    field('client-version', required_text(
      initial, 'client-version', 'initial manifest')),
    field('candidate-id', required_text(descriptor, 'candidate-id', context)),
    field('g0-graph-generation', required_integer(
      descriptor, 'g0-graph-generation', context)),
    field('g0-manifest-revision', required_integer(
      descriptor, 'g0-manifest-revision', context)),
    field('g1-graph-generation', required_integer(
      descriptor, 'g1-graph-generation', context)),
    field('g1-manifest-revision', required_integer(
      descriptor, 'g1-manifest-revision', context)),
    field('tantivy-generation', required_integer(
      descriptor, 'tantivy-generation', context)),
    field('server-evidence-sha256', required_text(
      descriptor, 'server-evidence-sha256', context)),
    field('transfer-manifest-sha256', evidence.transfer_manifest_sha256),
    field('artifact-bytes-sha256', evidence.artifact_bytes_sha256),
    field('node-artifacts', node_artifacts),
    field('root-artifacts', root_artifacts),
    field('buffers', required_list(initial, 'buffers', 'initial manifest')),
    field('buffer-dispositions', settlements),
    field('directory-sync', 'libuv-fsync'),
    field('terminal-status', 'completed'),
  }
end

local function final_incident_report (descriptor, evidence, settlements)
  local lines = {
    '* Skg maintenance recovery incident (finalized)', '',
    '- Incident :: =' .. required_text(
      descriptor, 'incident-id', 'evidence descriptor') .. '=',
    '- Candidate :: =' .. required_text(
      descriptor, 'candidate-id', 'evidence descriptor') .. '=',
    '- Selected graph :: =' .. tostring(required_integer(
      descriptor, 'g1-graph-generation', 'evidence descriptor')) .. '=',
    '- Node evidence artifacts :: ' .. tostring(#evidence.records), '',
    '** Buffer dispositions', '',
  }
  if #settlements == 0 then
    table.insert(lines, 'No buffers were registered.')
  else
    for _, record in ipairs(settlements) do
      table.insert(lines, string.format('- =%s= :: %s',
        required_text(record, 'buffer-id', 'final settlement'),
        required_text(record, 'disposition', 'final settlement'))) end
  end
  table.insert(lines, '')
  return table.concat(lines, '\n')
end

local function interrupted_index (settlements)
  local lines = { '* Interrupted buffers', '' }
  local count = 0
  for _, record in ipairs(settlements) do
    if required_text(record, 'disposition', 'final settlement')
       == 'interrupted' then
      local key = required_text(record, 'buffer-key', 'final settlement')
      if not key:match('^[A-Za-z0-9][A-Za-z0-9._-]*$') then
        fail('unsafe interrupted buffer key: ' .. key) end
      table.insert(lines, string.format(
        '- [[file:../buffer-snapshots/%s/unsaved-changes.org][%s]]',
        key, key))
      count = count + 1
    end
  end
  if count == 0 then table.insert(lines, 'No dirty buffer was interrupted.') end
  table.insert(lines, '')
  return table.concat(lines, '\n')
end

local function replace_private_file (path, bytes, incident_root, token)
  local temporary = vim.fs.dirname(path) .. '/.' .. vim.fs.basename(path)
    .. '.' .. token .. '.tmp'
  write_private_file(temporary, bytes, incident_root)
  local ok, error_text = vim.uv.fs_rename(temporary, path)
  if not ok then fail('cannot atomically replace archive file: ' .. error_text) end
  if read_regular_file(path) ~= bytes then
    fail('atomic replacement failed exact reread: ' .. path) end
end

local function finalized_marker (
    incident_id, manifest_sha, transfer_sha)
  return {
    field('archive-format-version', M.archive_format_version),
    field('incident-id', incident_id),
    field('manifest-sha256', manifest_sha),
    field('transfer-manifest-sha256', transfer_sha),
  }
end

local function bytes_artifact_record (relative, bytes)
  return {
    field('path', relative),
    field('bytes', #bytes),
    field('sha256', vim.fn.sha256(bytes)),
  }
end

---Append exact server evidence and final dispositions to INITIAL_RESULT.
---DESCRIPTOR is the parsed UTF-8 bundle descriptor; OPAQUE_BYTES is its exact
---binary tail; SETTLEMENTS have already been applied and acknowledged.
function M.finalize (initial_result, descriptor, opaque_bytes, settlements)
  local incident_root = initial_result and initial_result.path
  local initial_sha = initial_result and initial_result.manifest_sha256
  if type(incident_root) ~= 'string' or not sha256_valid(initial_sha) then
    fail('initial archive result is incomplete') end
  require_directory(incident_root, 448, 'incident directory')
  local initial_path = incident_root .. '/manifest.initial.sexp'
  local ready_path = incident_root .. '/ARCHIVE-READY'
  local initial_bytes = read_regular_file(initial_path)
  local initial = read_exact_sexpr(initial_path)
  local ready = read_exact_sexpr(ready_path)
  local incident_id = required_text(
    descriptor, 'incident-id', 'evidence descriptor')
  local epoch = required_integer(
    descriptor, 'maintenance-epoch', 'evidence descriptor')
  local evidence = parse_evidence_bundle(descriptor, opaque_bytes)
  local normalized_settlements = normalize_settlements(
    settlements, required_list(initial, 'buffers', 'initial manifest'))
  local transfer_sha = evidence.transfer_manifest_sha256
  local token = transfer_sha:sub(1, 20)
  local attempt_nonce = new_nonce()
  local attempt_token = token .. '-' .. attempt_nonce
  local staging = incident_root .. '/.finalizing.' .. token .. '.'
    .. attempt_nonce .. '.partial'
  local report_bytes = final_incident_report(
    descriptor, evidence, normalized_settlements)
  local index_bytes = interrupted_index(normalized_settlements)
  local root_artifacts = {
    bytes_artifact_record('incident.org', report_bytes),
    bytes_artifact_record('interrupted-buffers/README.org', index_bytes),
  }
  local final_value = final_manifest_value(
    initial, initial_sha, descriptor, evidence,
    normalized_settlements, root_artifacts)
  local final_bytes = M.canonical_sexpr(final_value) .. '\n'
  local final_sha = vim.fn.sha256(final_bytes)
  local marker_bytes = M.canonical_sexpr(finalized_marker(
    incident_id, final_sha, transfer_sha)) .. '\n'
  local final_manifest_path = incident_root .. '/manifest.final.sexp'
  local finalized_path = incident_root .. '/FINALIZED'

  if vim.fn.sha256(initial_bytes) ~= initial_sha
     or required_text(initial, 'manifest-kind', 'initial manifest') ~= 'initial'
     or not strict_uuid(incident_id)
     or not strict_uuid(required_text(
       descriptor, 'candidate-id', 'evidence descriptor'))
     or required_text(initial, 'incident-id', 'initial manifest') ~= incident_id
     or required_integer(initial, 'maintenance-epoch', 'initial manifest')
        ~= epoch
     or required_integer(initial, 'g0-graph-generation', 'initial manifest')
        ~= required_integer(
          descriptor, 'g0-graph-generation', 'evidence descriptor')
     or required_integer(initial, 'g0-manifest-revision', 'initial manifest')
        ~= required_integer(
          descriptor, 'g0-manifest-revision', 'evidence descriptor')
     or required_text(ready, 'incident-id', 'ARCHIVE-READY') ~= incident_id
     or required_text(ready, 'manifest-sha256', 'ARCHIVE-READY') ~= initial_sha
  then
    fail('evidence does not belong to this exact ready archive') end
  for _, key in ipairs({
    'server-evidence-sha256', 'transfer-manifest-sha256',
    'artifact-bytes-sha256',
  }) do
    if not sha256_valid(required_text(descriptor, key, 'evidence descriptor')) then
      fail('evidence descriptor has an invalid ' .. key) end
  end

  mkdir_private(staging, false)
  for _, category in ipairs(evidence_categories) do
    local records = {}
    for _, record in ipairs(evidence.records) do
      if safe_evidence_components(record.relative_path)[1] == category then
        table.insert(records, record) end
    end
    local destination = incident_root .. '/' .. category
    if #records > 0 then
      if vim.uv.fs_lstat(destination) then
        verify_evidence_category(incident_root, category, records)
      else
        mkdir_private(staging .. '/' .. category, false)
        for _, record in ipairs(records) do
          ensure_private_relative_directory(
            staging, vim.fs.dirname(record.relative_path))
          write_private_file(staging .. '/' .. record.relative_path,
            record.bytes, staging)
        end
        verify_evidence_category(staging, category, records)
        each_directory_postorder(staging .. '/' .. category, sync_directory)
        local renamed, rename_error = vim.uv.fs_rename(
          staging .. '/' .. category, destination)
        if not renamed then
          fail('cannot install evidence category: ' .. rename_error) end
        sync_directory(incident_root)
        verify_evidence_category(incident_root, category, records)
      end
    elseif vim.uv.fs_lstat(destination) then
      fail('archive contains undeclared evidence category: ' .. category)
    end
  end
  local empty, scan_error = vim.uv.fs_scandir(staging)
  if not empty then fail('cannot inspect finalization staging: ' .. scan_error) end
  if not vim.uv.fs_scandir_next(empty) then
    local removed, remove_error = vim.uv.fs_rmdir(staging)
    if not removed then fail('cannot remove empty staging: ' .. remove_error) end
  end

  if vim.uv.fs_lstat(finalized_path) then
    if read_regular_file(final_manifest_path) ~= final_bytes
       or read_regular_file(finalized_path) ~= marker_bytes
       or read_regular_file(incident_root .. '/incident.org') ~= report_bytes
       or read_regular_file(incident_root .. '/interrupted-buffers/README.org')
          ~= index_bytes then
      fail('existing FINALIZED archive differs from this exact replay') end
  else
    replace_private_file(incident_root .. '/incident.org', report_bytes,
      incident_root, attempt_token)
    replace_private_file(incident_root .. '/interrupted-buffers/README.org',
      index_bytes, incident_root, attempt_token)
    if vim.uv.fs_lstat(final_manifest_path) then
      if read_regular_file(final_manifest_path) ~= final_bytes then
        fail('existing final manifest differs from exact replay') end
    else
      replace_private_file(final_manifest_path, final_bytes,
        incident_root, attempt_token)
    end
    sync_directory(incident_root)
    local marker_temp = incident_root .. '/.FINALIZED.' .. attempt_token .. '.tmp'
    write_private_file(marker_temp, marker_bytes, incident_root)
    local renamed, rename_error = vim.uv.fs_rename(marker_temp, finalized_path)
    if not renamed then fail('cannot publish FINALIZED: ' .. rename_error) end
    sync_directory(incident_root)
    sync_directory(vim.fs.dirname(incident_root))
  end
  if read_regular_file(final_manifest_path) ~= final_bytes
     or read_regular_file(finalized_path) ~= marker_bytes then
    fail('final archive failed durable checksum reread') end
  return {
    path = incident_root,
    manifest_sha256 = final_sha,
    transfer_manifest_sha256 = transfer_sha,
    artifact_bytes_sha256 = evidence.artifact_bytes_sha256,
    sizes = M.size_report(vim.fs.dirname(incident_root), incident_root),
  }
end

-- ---- read-only retained-incident inspection ----------------------

---Read one recorded artifact only after its path, privacy, length, and
---checksum have been verified.  This is also the recovery UI's read boundary.
function M.verify_recorded_artifact (incident_root, record, context)
  context = context or 'archive artifact'
  local relative = required_text(record, 'path', context)
  local declared_bytes = required_integer(record, 'bytes', context)
  local declared_sha = required_text(record, 'sha256', context)
  if relative:sub(1, 1) == '/' or relative:find('\\', 1, true)
     or not sha256_valid(declared_sha) then
    fail(context .. ' has an unsafe artifact record') end
  local components = {}
  for component in (relative .. '/'):gmatch('(.-)/') do
    table.insert(components, component) end
  for _, component in ipairs(components) do
    if component == '' or component == '.' or component == '..' then
      fail(context .. ' has an unsafe artifact record') end
  end
  if table.concat(components, '/') ~= relative then
    fail(context .. ' has an unsafe artifact record') end
  local path = assert_confined_parent(incident_root .. '/' .. relative,
    incident_root)
  local bytes, stat = read_regular_file(path)
  if mode_bits(stat) ~= 384 or (stat.nlink and stat.nlink ~= 1)
     or #bytes ~= declared_bytes or vim.fn.sha256(bytes) ~= declared_sha then
    fail(context .. ' is not the exact private recorded artifact') end
  return bytes
end

local function verify_archive_marker (
    incident_root, marker_name, manifest_name, expected_kind)
  local manifest_path = incident_root .. '/' .. manifest_name
  local marker_path = incident_root .. '/' .. marker_name
  local manifest_bytes = read_regular_file(manifest_path)
  local manifest = read_exact_sexpr(manifest_path)
  local marker = read_exact_sexpr(marker_path)
  local context = marker_name .. ' marker'
  local sha = vim.fn.sha256(manifest_bytes)
  if required_integer(manifest, 'archive-format-version', manifest_name)
       ~= M.archive_format_version
     or required_text(manifest, 'manifest-kind', manifest_name) ~= expected_kind
     or required_integer(marker, 'archive-format-version', context)
       ~= M.archive_format_version
     or required_text(marker, 'incident-id', context)
       ~= required_text(manifest, 'incident-id', manifest_name)
     or required_text(marker, 'manifest-sha256', context) ~= sha then
    fail(marker_name .. ' does not bind the exact ' .. manifest_name) end
  return { value = manifest, bytes = manifest_bytes, sha256 = sha }
end

local function changed_node_count (final)
  local seen, count = {}, 0
  for _, record in ipairs(required_list(
    final, 'node-artifacts', 'final manifest')) do
    local relative = required_text(record, 'path', 'final node artifact')
    local components = safe_evidence_components(relative)
    local key = components[1] .. '/' .. components[2]
    if not seen[key] then seen[key], count = true, count + 1 end
  end
  return count
end

local function current_nvim_version ()
  local version = vim.version()
  return string.format('%d.%d.%d',
    version.major, version.minor, version.patch)
end

local function native_undo_compatible (initial)
  local client_kind = required_text(initial, 'client-kind', 'initial manifest')
  for _, buffer in ipairs(required_list(
    initial, 'buffers', 'initial manifest')) do
    local undo = required_list(buffer, 'undo', 'initial buffer')
    if required_text(undo, 'status', 'buffer undo') == 'archived'
       and (client_kind ~= 'neovim'
         or required_text(undo, 'kind', 'buffer undo') ~= 'nvim-wundo'
         or required_text(undo, 'version', 'buffer undo')
           ~= current_nvim_version()) then
      return false end
  end
  return true
end

---Verify and summarize one published maintenance incident without a server.
function M.inspect (incident_root)
  incident_root = absolute(incident_root)
  require_directory(incident_root, 448, 'incident directory')
  local name = vim.fs.basename(incident_root)
  if not valid_final_name(name) then
    fail('invalid retained incident directory name: ' .. name) end
  local ready = verify_archive_marker(
    incident_root, 'ARCHIVE-READY', 'manifest.initial.sexp', 'initial')
  local initial = ready.value
  local incident_id = required_text(initial, 'incident-id', 'initial manifest')
  if name ~= required_text(
      initial, 'archive-directory-name', 'initial manifest')
     or name:sub(-#incident_id) ~= incident_id then
    fail('incident directory and initial manifest differ') end
  local has_final_marker = vim.uv.fs_lstat(
    incident_root .. '/FINALIZED') ~= nil
  local has_final_manifest = vim.uv.fs_lstat(
    incident_root .. '/manifest.final.sexp') ~= nil
  if has_final_marker ~= has_final_manifest then
    fail('incident has an incomplete final marker pair') end

  local final, status
  if has_final_marker then
    local verified = verify_archive_marker(
      incident_root, 'FINALIZED', 'manifest.final.sexp', 'final')
    final, status = verified.value, 'finalized'
    if required_text(final, 'incident-id', 'final manifest') ~= incident_id
       or required_text(final, 'initial-manifest-sha256', 'final manifest')
         ~= ready.sha256 then
      fail('final manifest belongs to another archive') end
  else status = 'archive-ready' end

  local dispositions = final and required_list(
    final, 'buffer-dispositions', 'final manifest') or {}
  local interrupted, released = 0, 0
  for _, record in ipairs(dispositions) do
    local disposition = required_text(record, 'disposition', 'buffer disposition')
    if disposition == 'interrupted' then interrupted = interrupted + 1
    elseif disposition == 'released-unimpacted' then released = released + 1 end
  end
  local bytes = tree_bytes(incident_root)
  return {
    name = name, path = incident_root, incident_id = incident_id,
    status = status,
    origin = required_text(initial, 'origin', 'initial manifest'),
    started_at_utc = required_text(
      initial, 'started-at-utc', 'initial manifest'),
    client_kind = required_text(initial, 'client-kind', 'initial manifest'),
    client_version = required_text(
      initial, 'client-version', 'initial manifest'),
    g0 = required_integer(initial, 'g0-graph-generation', 'initial manifest'),
    g1 = final and required_integer(
      final, 'g1-graph-generation', 'final manifest') or nil,
    changed_nodes = final and changed_node_count(final) or 0,
    dirty_buffers = #required_list(initial, 'buffers', 'initial manifest'),
    interrupted_buffers = interrupted, released_buffers = released,
    native_undo_compatible = native_undo_compatible(initial),
    bytes = bytes, iec = iec_size(bytes), initial = initial, final = final,
  }
end

---List every strict-named retained incident, preserving corrupt entries as
---invalid summaries so one bad archive cannot hide its healthy siblings.
function M.list (archive_root)
  local root = M.resolve_archive_root(archive_root)
  local scanner, scan_error = vim.uv.fs_scandir(root)
  if not scanner then fail('cannot list retained archives: ' .. scan_error) end
  local summaries = {}
  while true do
    local name = vim.uv.fs_scandir_next(scanner)
    if not name then break end
    if valid_final_name(name) then
      local path = root .. '/' .. name
      local ok, result = pcall(M.inspect, path)
      if ok then table.insert(summaries, result)
      else table.insert(summaries, {
        name = name, path = path, status = 'invalid', error = tostring(result),
      }) end
    end
  end
  table.sort(summaries, function (left, right)
    return left.name > right.name end)
  return summaries
end

return M
