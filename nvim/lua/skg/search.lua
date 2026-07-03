-- PURPOSE: Text search: the request, the immediate results buffer,
-- and the three-message enrichment flow (search-results ->
-- request-snapshot -> search-enrichment). The Lua port of
-- elisp/skg-request-text-search.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local heralds = require('skg.heralds')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Functions run inside a freshly populated search buffer (with it
---current). Used by skg.search_make_link to upgrade the buffer to
---link-creation mode.
---@type fun()[]
M.search_buffer_setup_hooks = {}

---Text search with the conservative defaults: no regex, titles only,
---no Tantivy operator syntax.
---@param search_terms string|nil prompted for when absent
function M.search (search_terms)
  search_terms = search_terms or vim.fn.input('Search terms: ')
  if search_terms == nil or search_terms == '' then return end
  M.request_text_search(search_terms, false, false, false)
end

---Text search choosing axes by typing option characters: 'r' for
---per-token regex, 'b' to include body text, 't' for Tantivy phrase
---and operator syntax, 'h' to open the docs (abandoning the search).
---Unrecognized characters are ignored; an empty answer uses the
---conservative defaults.
---@param search_terms string|nil
---@param options string|nil
function M.search_interactive (search_terms, options)
  search_terms = search_terms or vim.fn.input('Search terms: ')
  if search_terms == nil or search_terms == '' then return end
  options = options or vim.fn.input(
    'Type a character for each search option:'
    .. ' (r)egex, (b)ody, (t)antivy syntax, (h)elp: ')
  if options:find('h', 1, true) then
    M.search_interactive_help()
    return end
  M.request_text_search(search_terms,
                        options:find('r', 1, true) ~= nil,
                        options:find('b', 1, true) ~= nil,
                        options:find('t', 1, true) ~= nil)
end

---Open docs/COMMANDS-nvim.org (falling back to docs/COMMANDS.org),
---with the cursor on the search-interactive documentation.
function M.search_interactive_help ()
  local this_file = debug.getinfo(1, 'S').source:sub(2)
  local repo_root = vim.fn.fnamemodify(this_file, ':h:h:h:h')
  local doc = repo_root .. '/docs/COMMANDS-nvim.org'
  if vim.fn.filereadable(doc) == 0 then
    doc = repo_root .. '/docs/COMMANDS.org' end
  if vim.fn.filereadable(doc) == 0 then
    error('Help file not found: ' .. doc) end
  vim.cmd('edit ' .. vim.fn.fnameescape(doc))
  vim.cmd('silent! %foldopen!')
  local found = vim.fn.search('^\\*\\+.*search.interactive', 'w')
  if found == 0 then
    vim.notify("Couldn't find the search-interactive headline;"
               .. ' showing the whole file.')
  end
end

---@param b boolean
---@return string the wire-format 'true'/'false'
function M.bool_to_string (b)
  return b and 'true' or 'false'
end

---Request a text search; REGEX, BODY, OPERATORS are booleans.
---@param search_terms string
---@param regex boolean
---@param body boolean
---@param operators boolean
function M.request_text_search (search_terms, regex, body, operators)
  local request = sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'text search'),
    sexpr.pair(sexpr.symbol('terms'), search_terms),
    sexpr.pair(sexpr.symbol('regex'), M.bool_to_string(regex)),
    sexpr.pair(sexpr.symbol('body'), M.bool_to_string(body)),
    sexpr.pair(sexpr.symbol('operators'),
               M.bool_to_string(operators)) }) .. '\n'
  state.register_response_handler('search-results',
    function (_payload_text, response)
      M.display_search_phase1(response, search_terms)
    end, true)
  state.register_response_handler('search-enrichment',
    function (_payload_text, response)
      M.display_search_enrichment(response)
    end, true)
  state.register_response_handler('request-snapshot',
    function (_payload_text, response)
      -- The server asks for a snapshot of the search buffer so it
      -- can integrate ancestry without losing user edits.
      M.handle_snapshot_request(response)
    end, false) -- persistent, not one-shot
  state.lp_reset()
  client.send_string(request)
end

---Display the immediate results: a search view buffer registered
---under the uri 'search:TERMS'.
---@param response any
---@param search_terms string
function M.display_search_phase1 (response, search_terms)
  local content = payload.field_text(response, 'content')
  if not content then return end
  local buf = buffer.open_org_buffer_from_text(
    content and (vim.trim(content) .. '\n') or '',
    buffer.search_buffer_name(search_terms),
    'search:' .. search_terms)
  vim.bo[buf].modified = false
  for _, hook in ipairs(M.search_buffer_setup_hooks) do
    pcall(hook)
  end
end

---Replace the search buffer with the enriched results and exit
---read-only.
---@param response any
function M.display_search_enrichment (response)
  local terms = payload.field_text(response, 'terms')
  local content = payload.field_text(response, 'content')
  if not terms or not content then return end
  local buf = buffer.find_buffer_by_uri('search:' .. terms)
  if not buf then return end
  vim.bo[buf].modifiable = true
  buffer.disarm_first_change_warning(buf)
  local cursor_saved = nil
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_buf(win) == buf then
      cursor_saved = { win, vim.api.nvim_win_get_cursor(win) }
      break end
  end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
    vim.split(vim.trim(content) .. '\n', '\n'))
  if cursor_saved then
    local line_count = vim.api.nvim_buf_line_count(buf)
    local cursor = cursor_saved[2]
    pcall(vim.api.nvim_win_set_cursor, cursor_saved[1],
          { math.min(cursor[1], line_count), cursor[2] })
  end
  vim.bo[buf].modified = false
  buffer.arm_first_change_warning(buf)
  heralds.enable(buf)
  vim.notify('Search results enriched.')
end

---Freeze the search buffer read-only and send its text back for
---enrichment.
---@param response any
function M.handle_snapshot_request (response)
  local terms = payload.field_text(response, 'content')
  local buf = terms and buffer.find_buffer_by_uri('search:' .. terms)
  if not buf then return end
  vim.bo[buf].modifiable = false
  vim.notify('Enriching search results...')
  local contents = table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'snapshot response'),
    sexpr.pair(sexpr.symbol('terms'), terms) }) .. '\n')
  client.send_string(string.format('Content-Length: %d\r\n\r\n',
                                   #contents))
  client.send_string(contents)
end

return M
