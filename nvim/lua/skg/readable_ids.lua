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

---The namespace of the title annotations alone, so that placing them
---again never doubles one and leaves the concealed tails alone.
M.title_namespace = vim.api.nvim_create_namespace('skg-readable-ids-titles')

vim.api.nvim_set_hl(0, 'SkgTitle',
                    { fg = '#6495ed', default = true }) -- cornflower

---FIFO queue of pending titles-by-ids requests, each {generation,
---buf}. The server responds on the one connection in request order,
---so a response matches the oldest pending entry.
---@type table[]
M.pending_title_requests = {}

---Session-global map from node ID to its title, or to false for an
---ID the server did not resolve (unknown to it, or restricted under the
---skgrepo restriction). Kept across neogit refreshes, so that a
---re-annotation asks for nothing it has seen; one whose IDs are all
---cached sends no request at all. It goes stale when a node is
---retitled, and after a skgrepo widening; M.refresh empties it.
---@type table<string, string|false>
M.title_cache = {}

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

---Empty the ID-to-title cache and annotate BUF again. For after
---anything that changes what titles the server would answer with: a
---node retitled, or a skgrepo widening (IDs restricted under the old
---skgrepo restriction were cached as missing).
---@param buf integer
function M.refresh (buf)
  buf = buf ~= 0 and buf or vim.api.nvim_get_current_buf()
  M.title_cache = {}
  M.annotate_buffer(buf)
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
  vim.api.nvim_buf_clear_namespace(buf, M.title_namespace, 0, -1)
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

---Clear annotations, scan for ids, shorten their display, annotate
---from the title cache, and request only the uncached titles from the
---server. When every ID is cached, no request is sent.
---@param buf integer
function M.annotate_buffer (buf)
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
  vim.api.nvim_buf_clear_namespace(buf, M.title_namespace, 0, -1)
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
  M.place_title_annotations(buf)
  local unique = {}
  local uncached_gnids = {}
  for _, position in ipairs(positions) do
    if not unique[position.id] and M.title_cache[position.id] == nil then
      unique[position.id] = true
      table.insert(uncached_gnids, position.id)
    end
  end
  if #uncached_gnids == 0 then return end
  local generation = (vim.b[buf].skg_readable_ids_generation or 0) + 1
  vim.b[buf].skg_readable_ids_generation = generation
  M.request_titles(uncached_gnids, generation, buf)
end

---(Re)place the title annotations over BUF's scanned ID positions,
---from M.title_cache.
---@param buf integer
function M.place_title_annotations (buf)
  vim.api.nvim_buf_clear_namespace(buf, M.title_namespace, 0, -1)
  for _, position in ipairs(
      vim.b[buf].skg_readable_ids_positions or {}) do
    local title = M.title_cache[position.id]
    if type(title) == 'string' then
      pcall(vim.api.nvim_buf_set_extmark,
            buf, M.title_namespace, position.row, position.end_col, {
              virt_text = { { ' ' .. M.format_title(title),
                              'SkgTitle' } },
              virt_text_pos = 'inline' })
    end
  end
end

---Send a titles-by-ids request; GENERATION and BUF ride the FIFO for
---the response handler.
---@param gnids string[]
---@param generation integer
---@param buf integer
function M.request_titles (gnids, generation, buf, approved_pids)
  local ok, err = pcall(function ()
    M.ensure_title_response_handler()
    state.lp_reset()
    local ids_form = { sexpr.symbol('ids') }
    for _, gnid in ipairs(gnids) do table.insert(ids_form, gnid) end
    local request_form = {
      sexpr.pair(sexpr.symbol('request'), 'titles by ids'),
      ids_form }
    if approved_pids and #approved_pids > 0 then
      local approval = { sexpr.symbol('approved-overPrivateText-pids') }
      for _, pid in ipairs(approved_pids) do
        table.insert(approval, pid) end
      table.insert(request_form, approval) end
    local request = sexpr.to_string(request_form) .. '\n'
    table.insert(M.pending_title_requests, { generation, buf, gnids })
    state.lp_pending_count = state.lp_pending_count + 1
    client.send_string(request)
  end)
  if not ok then
    vim.notify('skg-readable-ids: server not connected: '
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
        vim.notify('skg-readable-ids: titles-by-ids response without'
                   .. ' pending request')
        return end
      state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
      M.handle_response(response, entry[1], entry[2], entry[3])
    end, false)
  state.register_response_handler('overPrivateText-telescope-confirmation',
    function (_payload_text, response)
      local entry = table.remove(M.pending_title_requests, 1)
      if not entry then
        vim.notify('skg-readable-ids: privacy challenge without pending request')
        return end
      state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
      local prompt = payload.field_text(response, 'prompt') or
        'This title lookup includes text selected below home. Include it?'
      local pids = payload.string_list(payload.field(response, 'pids'))
      if vim.fn.confirm(prompt, '&Include\n&Decline', 2) == 1 then
        M.request_titles(entry[3], entry[1], entry[2], pids) end
    end, false)
end

---Merge RESPONSE's titles into M.title_cache, whatever the buffer's
---state (they are true regardless), caching each of REQUESTED_GNIDS
---the server did not answer for as missing; then annotate BUF from
---the cache, unless GENERATION is stale.
---@param response any
---@param generation integer
---@param buf integer
---@param requested_gnids string[]|nil
function M.handle_response (response, generation, buf, requested_gnids)
  for _, warning in ipairs(
      payload.string_list(payload.field(response, 'warnings'))) do
    vim.notify(warning, vim.log.levels.WARN) end
  local content = payload.field(response, 'content')
  if content ~= nil and sexpr.is_list(content) then
    for _, pair in ipairs(content) do
      if sexpr.is_pair(pair) then
        M.title_cache[sexpr.atom_text(pair.car)] =
          sexpr.atom_text(pair.cdr)
      end
    end
  end
  for _, gnid in ipairs(requested_gnids or {}) do
    if M.title_cache[gnid] == nil then M.title_cache[gnid] = false end
  end
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if generation ~= vim.b[buf].skg_readable_ids_generation then return end
  M.place_title_annotations(buf)
end

---The display form of TITLE: '[[id:X][LABEL]]' renders as '[[LABEL]]',
---so link-bearing titles read as links without exposing the id.
---@param title string
---@return string
function M.format_title (title)
  return (title:gsub('%[%[id:[^%]]+%]%[([^%]]+)%]%]', '[[%1]]'))
end

return M
