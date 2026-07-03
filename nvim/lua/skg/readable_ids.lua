-- PURPOSE: Annotate skg UUIDs in a buffer with their titles: each
-- UUID's tail is concealed to an ellipsis (the text is unchanged, so
-- copy-paste still yields the full id) and the node's title, fetched
-- from the server, is rendered after it as virtual text. Works on
-- .skg file buffers and git-status/diff buffers alike. The Lua port
-- of elisp/skg-readable-ids.el, with extmarks in place of the
-- display/after-string overlays.

local client = require('skg.client')
local id_search = require('skg.id_search')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

M.namespace = vim.api.nvim_create_namespace('skg-readable-ids')

vim.api.nvim_set_hl(0, 'SkgTitle',
                    { fg = '#6495ed', default = true }) -- cornflower

---FIFO queue of pending titles-by-ids requests, each {generation,
---buf}. The server responds on the one connection in request order,
---so a response matches the oldest pending entry.
---@type table[]
M.pending_title_requests = {}

---@param buf integer
---@return boolean
function M.enabled_p (buf)
  return vim.b[buf].skg_readable_ids == true
end

---@param buf integer
function M.toggle (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  if M.enabled_p(buf) then M.disable(buf) else M.enable(buf) end
end

---@param buf integer
function M.enable (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  vim.b[buf].skg_readable_ids = true
  require('skg.heralds').conceal_windows_showing(buf)
  M.annotate_buffer(buf)
end

---@param buf integer
function M.disable (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  vim.b[buf].skg_readable_ids = false
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
end

---All UUID occurrences in BUF: {row (0-based), start_col, end_col
---(0-based, exclusive), id} tuples.
---@param buf integer
---@return table[]
function M.collect_ids (buf)
  local positions = {}
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for row, line in ipairs(lines) do
    local from = 1
    while true do
      local start, finish = line:find(id_search.uuid_v4_pattern, from)
      if not start then break end
      table.insert(positions, {
        row = row - 1, start_col = start - 1, end_col = finish,
        id = line:sub(start, finish) })
      from = finish + 1
    end
  end
  return positions
end

---Clear annotations, scan for ids, shorten their display, and request
---titles from the server.
---@param buf integer
function M.annotate_buffer (buf)
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
  local positions = M.collect_ids(buf)
  if #positions == 0 then return end
  vim.b[buf].skg_readable_ids_positions = positions
  for _, position in ipairs(positions) do
    -- Conceal the tail (bytes 9 onward) to a single ellipsis.
    if position.end_col - position.start_col > 8 then
      vim.api.nvim_buf_set_extmark(
        buf, M.namespace, position.row, position.start_col + 8, {
          end_col = position.end_col,
          conceal = '…',
          hl_group = 'SkgTitle' })
    end
  end
  local generation = (vim.b[buf].skg_readable_ids_generation or 0) + 1
  vim.b[buf].skg_readable_ids_generation = generation
  local unique = {}
  local ids = {}
  for _, position in ipairs(positions) do
    if not unique[position.id] then
      unique[position.id] = true
      table.insert(ids, position.id)
    end
  end
  M.request_titles(ids, generation, buf)
end

---Send a titles-by-ids request; GENERATION and BUF ride the FIFO for
---the response handler.
---@param ids string[]
---@param generation integer
---@param buf integer
function M.request_titles (ids, generation, buf)
  local ok, err = pcall(function ()
    M.ensure_title_response_handler()
    state.lp_reset()
    local ids_form = { sexpr.symbol('ids') }
    for _, id in ipairs(ids) do table.insert(ids_form, id) end
    local request = sexpr.to_string({
      sexpr.pair(sexpr.symbol('request'), 'titles by ids'),
      ids_form }) .. '\n'
    table.insert(M.pending_title_requests, { generation, buf })
    state.lp_pending_count = state.lp_pending_count + 1
    client.send_string(request)
  end)
  if not ok then
    vim.notify('skg readable ids: server not connected: '
               .. tostring(err))
  end
end

---Install the shared titles-by-ids response handler (non-one-shot;
---the FIFO does the per-request accounting).
function M.ensure_title_response_handler ()
  state.register_response_handler('titles-by-ids',
    function (_payload_text, response)
      local entry = table.remove(M.pending_title_requests, 1)
      if not entry then
        vim.notify('skg readable ids: titles-by-ids response without'
                   .. ' pending request')
        return end
      state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
      M.handle_response(response, entry[1], entry[2])
    end, false)
end

---Annotate BUF from RESPONSE, unless GENERATION is stale.
---@param response any
---@param generation integer
---@param buf integer
function M.handle_response (response, generation, buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if generation ~= vim.b[buf].skg_readable_ids_generation then return end
  local title_map = {}
  local content = payload.field(response, 'content')
  if content ~= nil and sexpr.is_list(content) then
    for _, pair in ipairs(content) do
      if sexpr.is_pair(pair) then
        title_map[sexpr.atom_text(pair.car)] =
          sexpr.atom_text(pair.cdr)
      end
    end
  end
  for _, position in ipairs(
      vim.b[buf].skg_readable_ids_positions or {}) do
    local title = title_map[position.id]
    if title then
      pcall(vim.api.nvim_buf_set_extmark,
            buf, M.namespace, position.row, position.end_col, {
              virt_text = { { ' ' .. M.format_title(title),
                              'SkgTitle' } },
              virt_text_pos = 'inline' })
    end
  end
end

---The display form of TITLE: '[[id:X][LABEL]]' renders as '[[LABEL]]',
---so link-bearing titles read as links without exposing the id.
---@param title string
---@return string
function M.format_title (title)
  return (title:gsub('%[%[id:[^%]]+%]%[([^%]]+)%]%]', '[[%1]]'))
end

return M
