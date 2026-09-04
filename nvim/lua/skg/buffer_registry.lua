-- Explicit lifecycle records for every Skg-owned Neovim buffer.

local M = {}

local function uuid ()
  local bytes = { vim.uv.random(16):byte(1, 16) }
  bytes[7] = bytes[7] % 16 + 64
  bytes[9] = bytes[9] % 64 + 128
  local hex = {}
  for _, byte in ipairs(bytes) do
    table.insert(hex, string.format('%02x', byte)) end
  local value = table.concat(hex)
  return string.format('%s-%s-%s-%s-%s',
    value:sub(1, 8), value:sub(9, 12), value:sub(13, 16),
    value:sub(17, 20), value:sub(21, 32))
end

function M.raw_text (buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local text = table.concat(lines, '\n')
  if vim.bo[buf].endofline then text = text .. '\n' end
  return text
end

local function digest (text)
  return vim.fn.sha256(text)
end

local function conservative_ids (text)
  local result, seen = {}, {}
  for _, key in ipairs({ 'id', 'pid', 'extra_ids', 'extraIds' }) do
    for id in text:gmatch('%(' .. key .. '%s+([^()%s]+)') do
      if not seen[id] then
        seen[id] = true
        table.insert(result, id) end
    end
  end
  return result
end

function M.register (buf, kind, options)
  options = options or {}
  local state = require('skg.state')
  local current = M.raw_text(buf)
  local last_fetched = options.last_fetched or current
  vim.b[buf].skg_buffer_id = vim.b[buf].skg_buffer_id or uuid()
  vim.b[buf].skg_buffer_kind = assert(kind, 'explicit Skg buffer kind required')
  vim.b[buf].skg_lifecycle = options.lifecycle or 'live-view'
  vim.b[buf].skg_disposable = options.disposable == true
  vim.b[buf].skg_continuation_id = options.continuation_id
  vim.b[buf].skg_recipe = options.recipe or {}
  vim.b[buf].skg_root_ids = options.root_ids or conservative_ids(last_fetched)
  vim.b[buf].skg_record_source_set = state.active_source_set_name
  local store_state = require('skg.config').store_state or {}
  vim.b[buf].skg_graph_generation =
    options.graph_generation or store_state.graph_generation or 0
  vim.b[buf].skg_presentation_generation =
    options.presentation_generation or 0
  vim.b[buf].skg_server_revision = options.server_revision or 0
  vim.b[buf].skg_application_token = options.application_token or
    ((vim.b[buf].skg_application_token or 0) + 1)
  vim.b[buf].skg_last_fetched = last_fetched
  vim.b[buf].skg_last_fetched_sha256 = digest(last_fetched)
  vim.b[buf].skg_logical_dirty = false
  vim.b[buf].skg_presentation_stale = false
  vim.b[buf].skg_search_stale = false
  vim.b[buf].skg_herald_bearing = last_fetched:find('(heralds', 1, true) ~= nil
  return M.record(buf)
end

function M.record (buf)
  if not vim.api.nvim_buf_is_valid(buf) or not vim.b[buf].skg_buffer_id then
    return nil end
  return {
    id = vim.b[buf].skg_buffer_id,
    kind = vim.b[buf].skg_buffer_kind,
    lifecycle = vim.b[buf].skg_lifecycle,
    disposable = vim.b[buf].skg_disposable == true,
    continuation_id = vim.b[buf].skg_continuation_id,
    view_uri = vim.b[buf].skg_view_uri,
    recipe = vim.b[buf].skg_recipe,
    root_ids = vim.b[buf].skg_root_ids,
    source_set = vim.b[buf].skg_record_source_set,
    graph_generation = vim.b[buf].skg_graph_generation,
    presentation_generation = vim.b[buf].skg_presentation_generation,
    server_revision = vim.b[buf].skg_server_revision,
    application_token = vim.b[buf].skg_application_token,
    last_fetched = vim.b[buf].skg_last_fetched,
    last_fetched_sha256 = vim.b[buf].skg_last_fetched_sha256,
    logical_dirty = vim.b[buf].skg_logical_dirty == true,
    maintenance_epoch = vim.b[buf].skg_maintenance_epoch,
    presentation_stale = vim.b[buf].skg_presentation_stale == true,
    search_stale = vim.b[buf].skg_search_stale == true,
  }
end

function M.buffers ()
  local result = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if M.record(buf) then table.insert(result, buf) end end
  return result
end

function M.dirty (buf)
  return vim.bo[buf].modified or vim.b[buf].skg_logical_dirty == true
end

function M.apply_server_text (buf, text, expected)
  local record = assert(M.record(buf), 'Skg buffer is not registered')
  if record.view_uri ~= expected.view_uri then
    error('Skg view URI changed before application') end
  if record.application_token ~= expected.application_token then
    error('Skg application token changed before application') end
  if M.dirty(buf) then error('Skg refuses to replace a dirty buffer') end
  vim.bo[buf].modifiable = true
  require('skg.buffer').disarm_first_change_warning(buf)
  local endofline = text:sub(-1) == '\n'
  local body = endofline and text:sub(1, -2) or text
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(body, '\n'))
  vim.bo[buf].endofline = endofline
  vim.bo[buf].modified = false
  vim.b[buf].skg_application_token = record.application_token + 1
  vim.b[buf].skg_last_fetched = text
  vim.b[buf].skg_last_fetched_sha256 = digest(text)
  vim.b[buf].skg_graph_generation = expected.graph_generation
  vim.b[buf].skg_presentation_generation = expected.presentation_generation
  vim.b[buf].skg_server_revision = expected.server_revision
  vim.b[buf].skg_presentation_stale = false
  require('skg.buffer').arm_first_change_warning(buf)
  return vim.b[buf].skg_application_token
end

function M.lock_for_maintenance (buf, epoch)
  if not M.record(buf) then return end
  vim.b[buf].skg_maintenance_epoch = epoch
  vim.bo[buf].modifiable = false
end

function M.unlock_after_maintenance (buf, epoch)
  if not M.record(buf) or vim.b[buf].skg_maintenance_epoch ~= epoch then
    return end
  vim.b[buf].skg_maintenance_epoch = nil
  if not vim.b[buf].skg_save_locked then vim.bo[buf].modifiable = true end
end

function M.census ()
  local result = {}
  for _, buf in ipairs(M.buffers()) do
    local record = M.record(buf)
    local current = M.raw_text(buf)
    local undo_tree = vim.api.nvim_buf_call(buf, vim.fn.undotree)
    table.insert(result, {
      buffer_id = record.id,
      kind = record.kind,
      view_uri = record.view_uri or 'nil',
      graph_generation = record.graph_generation or 0,
      server_revision = record.server_revision or 0,
      application_token = record.application_token or 0,
      dirty = M.dirty(buf),
      undo_required = M.dirty(buf) and #(undo_tree.entries or {}) > 0,
      last_fetched_sha256 = record.last_fetched_sha256,
      current_sha256 = digest(current),
    })
  end
  return result
end

local function atom_pair (sexpr, key, value)
  return sexpr.pair(sexpr.symbol(key), tostring(value))
end

function M.census_payload ()
  local sexpr = require('skg.sexpr.parse')
  local records = {}
  for _, descriptor in ipairs(M.census()) do
    table.insert(records, {
      atom_pair(sexpr, 'buffer-id', descriptor.buffer_id),
      atom_pair(sexpr, 'kind', descriptor.kind),
      atom_pair(sexpr, 'view-uri', descriptor.view_uri),
      atom_pair(sexpr, 'graph-generation', descriptor.graph_generation),
      atom_pair(sexpr, 'server-revision', descriptor.server_revision),
      atom_pair(sexpr, 'application-token', descriptor.application_token),
      atom_pair(sexpr, 'dirty', descriptor.dirty and 'true' or 'nil'),
      atom_pair(sexpr, 'undo-required',
                descriptor.undo_required and 'true' or 'nil'),
      atom_pair(sexpr, 'last-fetched-sha256',
                descriptor.last_fetched_sha256),
      atom_pair(sexpr, 'current-sha256', descriptor.current_sha256),
    })
  end
  return sexpr.to_string(records)
end

function M.find_by_id (buffer_id)
  for _, buf in ipairs(M.buffers()) do
    if vim.b[buf].skg_buffer_id == buffer_id then return buf end end
  return nil
end

function M.census_texts_payload (buffer_ids)
  local sexpr = require('skg.sexpr.parse')
  local records = {}
  for _, buffer_id in ipairs(buffer_ids) do
    local buf = M.find_by_id(buffer_id)
    if not buf then
      error('Server requested census text for dead buffer ' .. buffer_id) end
    table.insert(records, {
      atom_pair(sexpr, 'buffer-id', buffer_id),
      atom_pair(sexpr, 'last-fetched', vim.b[buf].skg_last_fetched or ''),
      atom_pair(sexpr, 'current', M.raw_text(buf)),
    })
  end
  return sexpr.to_string(records)
end

function M.mark_census_buffers_stale (buffer_ids)
  for _, buffer_id in ipairs(buffer_ids) do
    local buf = M.find_by_id(buffer_id)
    if buf then
      vim.b[buf].skg_presentation_stale = true
      vim.b[buf].skg_view_uri = nil
    end
  end
  if #buffer_ids > 0 then
    vim.notify(string.format(
      '%d Skg buffer(s) could not be reattached; text is preserved without live save authority',
      #buffer_ids), vim.log.levels.WARN)
  end
end

return M
