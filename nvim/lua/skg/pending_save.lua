-- Durable client identity and exact replay material for ordinary saves.

local payload = require('skg.payload')
local recovery = require('skg.recovery_archive')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = { format_version = 1 }
local valid_states = {
  prepared = true, uncertain = true, terminal = true,
  refused = true, acknowledged = true,
}

local function fail (message) error('Skg pending save failed: ' .. message) end

function M.new_operation_id ()
  return require('skg.buffer_registry').new_local_id()
end

function M.fingerprint (request_intent, content)
  return vim.fn.sha256(request_intent .. string.char(0) .. content)
end

local function strict_uuid (value)
  local hex = '[0-9a-f]'
  local pattern = '^' .. hex:rep(8) .. '%-' .. hex:rep(4)
    .. '%-' .. hex:rep(4) .. '%-' .. hex:rep(4)
    .. '%-' .. hex:rep(12) .. '$'
  return type(value) == 'string' and value:match(pattern) ~= nil
end

local function mode_bits (stat) return stat.mode % 512 end

local function require_private_directory (path)
  local stat = vim.uv.fs_lstat(path)
  if not stat then
    local ok, err = vim.uv.fs_mkdir(path, 448)
    if not ok then fail('cannot create private directory: ' .. tostring(err)) end
    stat = vim.uv.fs_lstat(path)
  end
  if not stat or stat.type ~= 'directory' then
    fail('pending-save path is not a directory: ' .. path) end
  if mode_bits(stat) ~= 448 then
    local ok, err = vim.uv.fs_chmod(path, 448)
    if not ok then fail('cannot make directory private: ' .. tostring(err)) end
  end
end

local function root ()
  local archive = recovery.resolve_archive_root()
  local path = archive .. '/pending-saves'
  require_private_directory(path)
  return path
end

local function path_for (directory, operation_id)
  if not strict_uuid(operation_id) then
    fail('invalid save operation id: ' .. tostring(operation_id)) end
  return directory .. '/operation-' .. operation_id .. '.sexp'
end

local function field (name, value)
  return { sexpr.symbol(name), value }
end

local function sync_directory (path)
  local descriptor, open_error = vim.uv.fs_open(path, 'r', 0)
  if not descriptor then fail('cannot open directory for sync: ' .. open_error) end
  local ok, sync_error = vim.uv.fs_fsync(descriptor)
  local close_ok, close_error = vim.uv.fs_close(descriptor)
  if not ok then fail('cannot sync directory: ' .. sync_error) end
  if not close_ok then fail('cannot close directory: ' .. close_error) end
end

local function read_file (path)
  local stat = vim.uv.fs_lstat(path)
  if not stat or stat.type ~= 'file' or mode_bits(stat) ~= 384 then
    fail('record is not a private regular file: ' .. path) end
  local handle, open_error = io.open(path, 'rb')
  if not handle then fail('cannot read record: ' .. open_error) end
  local bytes = handle:read('*a')
  local ok, close_error = handle:close()
  if not ok then fail('cannot close record: ' .. tostring(close_error)) end
  if #bytes ~= stat.size then fail('record changed while reading: ' .. path) end
  return bytes
end

local function validate (record, expected_id)
  if not sexpr.is_list(record)
     or tonumber(payload.field_text(record, 'format-version')) ~= M.format_version
  then fail('unsupported pending-save record') end
  local operation_id = payload.field_text(record, 'operation-id')
  local fingerprint = payload.field_text(record, 'request-base-fingerprint')
  local record_state = payload.field_text(record, 'state')
  if not strict_uuid(operation_id) or (expected_id and operation_id ~= expected_id) then
    fail('pending-save operation identity changed') end
  if type(fingerprint) ~= 'string' or not fingerprint:match('^[0-9a-f]+$')
     or #fingerprint ~= 64 then fail('invalid pending-save fingerprint') end
  if not valid_states[record_state] then
    fail('invalid pending-save state: ' .. tostring(record_state)) end
  if record_state == 'prepared' or record_state == 'uncertain'
     or record_state == 'terminal' then
    if payload.field_text(record, 'request') == nil
       or payload.field_text(record, 'content') == nil then
      fail('unresolved pending save lacks exact bytes') end
  end
  return record
end

local function read_path (path, expected_id)
  local ok, record = pcall(sexpr.read, read_file(path))
  if not ok then fail('cannot parse ' .. path .. ': ' .. tostring(record)) end
  return validate(record, expected_id)
end

local function write_all (descriptor, bytes)
  local offset = 0
  while offset < #bytes do
    local written, err = vim.uv.fs_write(descriptor, bytes:sub(offset + 1), offset)
    if not written or written <= 0 then fail('record write failed: ' .. tostring(err)) end
    offset = offset + written
  end
  local ok, err = vim.uv.fs_fsync(descriptor)
  if not ok then fail('cannot sync record: ' .. tostring(err)) end
end

local function write_record (record)
  validate(record)
  local directory = root()
  local operation_id = payload.field_text(record, 'operation-id')
  local path = path_for(directory, operation_id)
  local temporary = path .. '.' .. vim.fn.sha256(
    operation_id .. tostring(vim.uv.hrtime())):sub(1, 20) .. '.tmp'
  local descriptor, open_error = vim.uv.fs_open(temporary, 'wx', 384)
  if not descriptor then fail('cannot create record: ' .. tostring(open_error)) end
  local ok, write_error = pcall(write_all, descriptor,
                                recovery.canonical_sexpr(record) .. '\n')
  local close_ok, close_error = vim.uv.fs_close(descriptor)
  if not ok then pcall(vim.uv.fs_unlink, temporary); error(write_error, 0) end
  if not close_ok then fail('cannot close record: ' .. tostring(close_error)) end
  local renamed, rename_error = vim.uv.fs_rename(temporary, path)
  if not renamed then pcall(vim.uv.fs_unlink, temporary)
    fail('cannot atomically replace record: ' .. tostring(rename_error)) end
  sync_directory(directory)
  if read_file(path) ~= recovery.canonical_sexpr(record) .. '\n' then
    fail('record failed exact reread: ' .. path) end
  return path
end

function M.prepare (values)
  local directory = root()
  local path = path_for(directory, values.operation_id)
  if vim.uv.fs_lstat(path) then
    local existing = read_path(path, values.operation_id)
    if payload.field_text(existing, 'request-base-fingerprint')
         ~= values.request_base_fingerprint
       or payload.field_text(existing, 'request') ~= values.request
       or payload.field_text(existing, 'content') ~= values.content then
      fail('operation id already binds a different save: ' .. values.operation_id) end
    return existing
  end
  local record = {
    field('format-version', M.format_version),
    field('operation-id', values.operation_id),
    field('request-base-fingerprint', values.request_base_fingerprint),
    field('client', 'nvim'),
    field('client-session-id', state.client_session_id),
    field('buffer-id', values.buffer_id or ''),
    field('state', sexpr.symbol('prepared')),
    field('request', values.request),
    field('content', values.content),
  }
  write_record(record)
  return record
end

function M.records ()
  local directory = root()
  local names = vim.fn.readdir(directory, function (name)
    return name:match('^operation%-[0-9a-f%-]+%.sexp$') and 1 or 0 end)
  table.sort(names)
  local records = {}
  for _, name in ipairs(names) do
    local path = directory .. '/' .. name
    local stat = vim.uv.fs_lstat(path)
    if not stat or stat.type ~= 'file' then
      fail('pending-save record is not a regular file: ' .. path) end
    table.insert(records, read_path(path))
  end
  return records
end

function M.unresolved_records ()
  local result = {}
  for _, record in ipairs(M.records()) do
    local record_state = payload.field_text(record, 'state')
    if record_state == 'prepared' or record_state == 'uncertain'
       or record_state == 'terminal' then table.insert(result, record) end
  end
  return result
end

function M.assert_none_unresolved ()
  local record = M.unresolved_records()[1]
  if record then error('Save ' .. payload.field_text(record, 'operation-id')
    .. ' is unresolved; inspect it with :SkgPendingSaveStatus') end
end

local function replace_field (record, name, value)
  for _, entry in ipairs(record) do
    if sexpr.is_list(entry) and payload.field_text({ entry }, name) ~= nil then
      entry[2] = value
      return record end
  end
  table.insert(record, field(name, value))
  return record
end

local function transition (record, record_state, response, server_state)
  local operation_id = payload.field_text(record, 'operation-id')
  local current = read_path(path_for(root(), operation_id), operation_id)
  if payload.field_text(current, 'request-base-fingerprint')
       ~= payload.field_text(record, 'request-base-fingerprint') then
    fail('pending-save fingerprint changed') end
  replace_field(current, 'state', sexpr.symbol(record_state))
  if response then replace_field(current, 'terminal-response', response) end
  if server_state then
    replace_field(current, 'server-state', sexpr.symbol(server_state)) end
  write_record(current)
  return current
end

function M.mark_uncertain (record)
  local operation_id = payload.field_text(record, 'operation-id')
  local current = read_path(path_for(root(), operation_id), operation_id)
  if payload.field_text(current, 'state') == 'prepared' then
    return transition(record, 'uncertain') end
  return current
end

function M.mark_terminal (record, response_text, refused)
  return transition(record, refused and 'refused' or 'terminal', response_text)
end

function M.mark_acknowledged (record)
  local operation_id = payload.field_text(record, 'operation-id')
  local current = read_path(path_for(root(), operation_id), operation_id)
  if payload.field_text(current, 'request-base-fingerprint')
       ~= payload.field_text(record, 'request-base-fingerprint') then
    fail('pending-save fingerprint changed') end
  if payload.field_text(current, 'state') ~= 'terminal' then
    fail('cannot acknowledge a save without a terminal result') end
  local compact = {
    field('format-version', M.format_version),
    field('operation-id', operation_id),
    field('request-base-fingerprint',
          payload.field_text(record, 'request-base-fingerprint')),
    field('client', payload.field_text(current, 'client') or 'nvim'),
    field('client-session-id',
          payload.field_text(current, 'client-session-id')
          or state.client_session_id),
    field('buffer-id', payload.field_text(current, 'buffer-id') or ''),
    field('state', sexpr.symbol('acknowledged')),
  }
  write_record(compact)
  return compact
end

function M.apply_status (record, response)
  M.verify_response(record, response)
  local server_state = payload.field_text(response, 'state')
  local terminal_response = payload.field_text(response, 'terminal-response')
  if server_state == 'committed' then
    if terminal_response == nil then
      fail('committed save status lacks its exact terminal response') end
    local updated = transition(
      record, 'terminal', terminal_response, 'committed')
    local ok, terminal = pcall(sexpr.read, terminal_response)
    if not ok then fail('cannot parse committed terminal response') end
    if payload.field_text(terminal, 'requires-fresh-view') == 'true' then
      replace_field(updated, 'fresh-view-required', sexpr.symbol('true'))
      write_record(updated) end
    return updated
  elseif server_state == 'refused' then
    if terminal_response == nil then
      fail('refused save status lacks its exact terminal response') end
    return transition(record, 'refused', terminal_response, 'refused')
  elseif server_state == 'unknown' or server_state == 'prepared'
      or server_state == 'authorized' or server_state == 'applied'
      or server_state == 'blocked' then
    return transition(record, 'uncertain', nil, server_state)
  end
  fail('invalid save-operation status: ' .. tostring(server_state))
end

function M.retry_material (record)
  local server_state = payload.field_text(record, 'server-state')
  if server_state ~= 'unknown' and server_state ~= 'prepared' then
    fail('save must have explicit unknown/prepared status before retry') end
  return payload.field_text(record, 'request'),
         payload.field_text(record, 'content')
end

function M.verify_response (record, response)
  if payload.field_text(response, 'operation-id')
       ~= payload.field_text(record, 'operation-id')
     or payload.field_text(response, 'request-base-fingerprint')
       ~= payload.field_text(record, 'request-base-fingerprint') then
    fail('save response identity mismatch') end
  return true
end

M.field_text = function (record, name) return payload.field_text(record, name) end
M.transition = transition

return M
