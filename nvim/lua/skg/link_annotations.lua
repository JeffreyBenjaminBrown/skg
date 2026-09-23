-- Display-only source suffixes and confirmed broken-link styling in views.

local client = require('skg.client')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

M.namespace = vim.api.nvim_create_namespace('skg-link-annotations')
M.cache = {}
M.requests = {}
M.epoch = 0
M.next_request = 0

vim.api.nvim_set_hl(0, 'SkgBrokenLink',
  { fg = '#ff69b4', underline = true, default = true })

local function valid (buf)
  return vim.api.nvim_buf_is_valid(buf)
         and vim.b[buf].skg_link_annotations_enabled == true
end

---All literal [[id:ID][LABEL]] links in BUF, with byte columns.
function M.collect (buf)
  local positions = {}
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local from = 1
    while true do
      local start, finish, id, label =
        line:find('%[%[id:([^%]\n]+)%]%[([^%]\n]*)%]%]', from)
      if not start then break end
      local label_start = start + 6 + #id
      table.insert(positions, {
        row = row - 1, label_start = label_start,
        label_end = label_start + #label, link_end = finish,
        id = id })
      from = finish + 1
    end
  end
  return positions
end

function M.paint (buf, positions)
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
  local suffix_enabled = vim.b[buf].skg_link_source_suffix == true
  for _, position in ipairs(positions) do
    local status = M.cache[position.id]
    local kind = status and status[1] or 'pending'
    if kind == 'missing' and position.label_end > position.label_start then
      vim.api.nvim_buf_set_extmark(buf, M.namespace,
        position.row, position.label_start, {
          end_row = position.row, end_col = position.label_end,
          hl_group = 'SkgBrokenLink', priority = 150 })
    end
    if suffix_enabled then
      local label = ({ missing = '⌂:missing', inactive = '⌂:inactive',
                       error = '⌂:unavailable', pending = '⌂:…' })[kind]
                    or ('⌂:' .. status[3])
      local group = kind == 'resolved' and 'SkgHeraldGreen' or 'Comment'
      vim.api.nvim_buf_set_extmark(buf, M.namespace,
        position.row, position.link_end, {
          virt_text = { { ' [' .. label .. ']', group } },
          virt_text_pos = 'inline', right_gravity = false })
    end
  end
end

function M.refresh (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  if not valid(buf) then return end
  local generation = (vim.b[buf].skg_link_annotations_generation or 0) + 1
  vim.b[buf].skg_link_annotations_generation = generation
  local tick = vim.api.nvim_buf_get_changedtick(buf)
  local positions = M.collect(buf)
  M.paint(buf, positions)
  local seen, ids = {}, {}
  for _, position in ipairs(positions) do
    if M.cache[position.id] == nil and not seen[position.id] then
      seen[position.id] = true
      table.insert(ids, position.id) end
  end
  if #ids > 0 then M.request(buf, generation, tick, ids) end
end

function M.request (buf, generation, tick, ids)
  if not state.tcp or state.tcp:is_closing() then
    for _, id in ipairs(ids) do M.cache[id] = { 'error' } end
    M.paint(buf, M.collect(buf))
    return end
  M.next_request = M.next_request + 1
  local request_id = 'links-' .. M.next_request
  local ids_form = { sexpr.symbol('ids') }
  for _, id in ipairs(ids) do table.insert(ids_form, id) end
  local request = sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'link statuses'),
    sexpr.pair(sexpr.symbol('request-id'), request_id),
    ids_form }) .. '\n'
  state.register_response_handler('link-statuses',
    function (_payload_text, response) M.handle_response(response) end,
    false)
  M.requests[request_id] = { buf = buf, generation = generation,
    tick = tick, epoch = M.epoch, ids = ids }
  state.lp_pending_count = state.lp_pending_count + 1
  local ok = pcall(client.send_string, request)
  if not ok then
    M.requests[request_id] = nil
    state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
    for _, id in ipairs(ids) do M.cache[id] = { 'error' } end
    if valid(buf) then M.paint(buf, M.collect(buf)) end
  end
end

function M.handle_response (response)
  local request_id = payload.field_text(response, 'request-id')
  local entry = request_id and M.requests[request_id] or nil
  if not entry then return end
  M.requests[request_id] = nil
  state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
  local buf = entry.buf
  if entry.epoch ~= M.epoch or not valid(buf)
     or entry.generation ~= vim.b[buf].skg_link_annotations_generation
     or entry.tick ~= vim.api.nvim_buf_get_changedtick(buf) then
    return end
  local requested = {}
  for _, id in ipairs(entry.ids) do requested[id] = true end
  local results = payload.field(response, 'results')
  if sexpr.is_list(results) then
    for _, row in ipairs(results) do
      if sexpr.is_list(row) then
        local id = sexpr.atom_text(row[1])
        if requested[id] then
          local kind = sexpr.atom_text(row[2])
          M.cache[id] = kind == 'resolved'
            and { kind, sexpr.atom_text(row[3]), sexpr.atom_text(row[4]) }
            or { kind }
        end
      end
    end
  end
  M.paint(buf, M.collect(buf))
end

function M.invalidate_all ()
  M.epoch = M.epoch + 1
  M.cache = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if valid(buf) then M.refresh(buf) end end
end

function M.connection_reset ()
  M.requests = {}
  M.invalidate_all()
end

function M.connection_changed ()
  M.invalidate_all()
end

function M.enable (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  vim.b[buf].skg_link_annotations_enabled = true
  local group = vim.api.nvim_create_augroup(
    'skg-link-annotations-' .. buf, { clear = true })
  vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI' }, {
    group = group, buffer = buf,
    callback = function ()
      local serial = (vim.b[buf].skg_link_annotations_edit_serial or 0) + 1
      vim.b[buf].skg_link_annotations_edit_serial = serial
      vim.defer_fn(function ()
        if valid(buf)
           and vim.b[buf].skg_link_annotations_edit_serial == serial then
          M.refresh(buf) end
      end, 150)
    end })
  M.refresh(buf)
end

function M.disable (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(buf) then return end
  vim.b[buf].skg_link_annotations_enabled = false
  vim.b[buf].skg_link_annotations_generation =
    (vim.b[buf].skg_link_annotations_generation or 0) + 1
  pcall(vim.api.nvim_del_augroup_by_name, 'skg-link-annotations-' .. buf)
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
end

function M.toggle_source_overlay (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  if not valid(buf) then error('This command needs an Skg view buffer') end
  vim.b[buf].skg_link_source_suffix =
    not (vim.b[buf].skg_link_source_suffix == true)
  M.refresh(buf)
end

table.insert(state.connection_reset_hooks, M.connection_reset)

return M
