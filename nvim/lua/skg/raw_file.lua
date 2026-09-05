-- Exact before-state guard for ordinary buffers visiting configured .skg files.

local client = require('skg.client')
local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local state = require('skg.state')

local M = {}

local function current_disk_state (path)
  if type(path) ~= 'string' or path == '' then
    return { kind = 'unsafe', reason = 'buffer has no file name' } end
  local stat, stat_error = vim.uv.fs_lstat(path)
  if not stat then
    if stat_error and stat_error:find('ENOENT', 1, true) then
      return { kind = 'absent' } end
    return { kind = 'unsafe', reason = stat_error or 'could not inspect path' }
  end
  if stat.type ~= 'file' then
    return {
      kind = 'unsafe', reason = 'path is filesystem type ' .. tostring(stat.type),
    }
  end
  local descriptor, open_error = vim.uv.fs_open(path, 'r', 438)
  if not descriptor then
    return { kind = 'unsafe', reason = open_error or 'could not open path' } end
  local chunks, offset, read_error = {}, 0, nil
  while true do
    local chunk, error_text = vim.uv.fs_read(descriptor, 65536, offset)
    if not chunk then read_error = error_text or 'could not read path'; break end
    if chunk == '' then break end
    table.insert(chunks, chunk)
    offset = offset + #chunk
  end
  local _, close_error = vim.uv.fs_close(descriptor)
  if read_error then
    return { kind = 'unsafe', reason = read_error } end
  if close_error then
    return { kind = 'unsafe', reason = close_error } end
  return { kind = 'regular', digest = vim.fn.sha256(table.concat(chunks)) }
end

local function recorded_disk_state (buf)
  local kind = vim.b[buf].skg_raw_disk_kind
  if not kind then return nil end
  return {
    kind = kind,
    digest = vim.b[buf].skg_raw_disk_digest,
    reason = vim.b[buf].skg_raw_disk_reason,
  }
end

local function safe_state (value)
  return value and (value.kind == 'regular' or value.kind == 'absent')
end

local function same_state (left, right)
  return safe_state(left) and safe_state(right)
    and left.kind == right.kind and left.digest == right.digest
end

local function describe_state (value)
  if not value then return 'no recorded state' end
  if value.kind == 'regular' then return 'SHA-256 ' .. value.digest end
  if value.kind == 'absent' then return 'absence' end
  return 'unsafe state (' .. tostring(value.reason or 'unknown') .. ')'
end

function M.record_disk_state (buf)
  if not vim.api.nvim_buf_is_valid(buf) then return nil end
  local snapshot = current_disk_state(vim.api.nvim_buf_get_name(buf))
  vim.b[buf].skg_raw_disk_kind = snapshot.kind
  vim.b[buf].skg_raw_disk_digest = snapshot.digest
  vim.b[buf].skg_raw_disk_reason = snapshot.reason
  vim.b[buf].skg_raw_externally_stale = false
  if registry.record(buf) then
    local text = registry.raw_text(buf)
    vim.b[buf].skg_last_fetched = text
    vim.b[buf].skg_last_fetched_sha256 = vim.fn.sha256(text)
  end
  return snapshot
end

function M.enroll (buf, refreshed_from_disk)
  local record = registry.register_raw_file_if_configured(buf)
  -- BufReadPost can precede the verified server source inventory.  Capture
  -- that actual read even before registration so a later enrollment cannot
  -- bless an intervening external rewrite.
  if refreshed_from_disk or (record and not recorded_disk_state(buf)) then
    M.record_disk_state(buf) end
  return record
end

function M.enroll_open_files ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do M.enroll(buf, false) end
end

function M.queue_observation ()
  state.register_response_handler('reload-paths', function (_, response)
    if payload.field_text(response, 'observation-queued') ~= 'true' then
      vim.notify(payload.field_text(response, 'content')
        or 'Skg could not queue raw-file observation', vim.log.levels.WARN)
    end
  end, true)
  client.submit_request(
    '((request . "reload paths") (full-sweep . "true"))\n')
end

function M.refresh_staleness (buf)
  local record = registry.record(buf)
  if not record or record.kind ~= 'raw-skg-file' then return false end
  local stale = not same_state(
    recorded_disk_state(buf),
    current_disk_state(vim.api.nvim_buf_get_name(buf)))
  vim.b[buf].skg_raw_externally_stale = stale
  return stale
end

local function queue_observation_safely ()
  local ok, reason = pcall(M.queue_observation)
  if not ok then
    vim.notify('Raw .skg change is safe, but observation could not be queued: '
      .. tostring(reason), vim.log.levels.WARN)
  end
end

function M.guard_before_save (buf)
  local record = M.enroll(buf, false)
  if not record then return end
  if record.maintenance_epoch then
    error('Raw .skg save refused: maintenance epoch '
      .. tostring(record.maintenance_epoch) .. ' is active') end
  local expected = recorded_disk_state(buf)
  local actual = current_disk_state(vim.api.nvim_buf_get_name(buf))
  if not same_state(expected, actual) then
    vim.b[buf].skg_raw_externally_stale = true
    queue_observation_safely()
    error('Raw .skg save refused: disk changed since this buffer was read '
      .. '(expected ' .. describe_state(expected) .. ', found '
      .. describe_state(actual) .. '); revert or reconcile explicitly')
  end
  local dirty_views = {}
  for _, candidate in ipairs(registry.buffers()) do
    local candidate_record = registry.record(candidate)
    if candidate_record.lifecycle == 'live-view'
       and registry.dirty(candidate) then
      table.insert(dirty_views, vim.api.nvim_buf_get_name(candidate)) end
  end
  if #dirty_views > 0 then
    error('Raw .skg save refused: save or close these Skg views first: '
      .. table.concat(dirty_views, ', '))
  end
end

function M.after_save (buf)
  if not registry.register_raw_file_if_configured(buf) then return end
  M.record_disk_state(buf)
  queue_observation_safely()
end

return M
