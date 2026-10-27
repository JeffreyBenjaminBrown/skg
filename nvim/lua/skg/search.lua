-- PURPOSE: Text search: the request, and the search buffer, which
-- one 'search-results' message carries, each hit with what the
-- enrichment draws under it (docs/api-and-formats.org, "Text
-- search"). The Lua port of elisp/skg-request-text-search.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local heralds = require('skg.heralds')
local messages = require('skg.messages')
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
function M.request_text_search (search_terms, regex, body, operators,
                                overPrivateText_choice)
  local ok, err = pcall(
    M.send_text_search_request, search_terms, regex, body, operators,
    overPrivateText_choice)
  if not ok then
    M.cancel_locally_failed_search()
    error(err) end
end

---Register handlers and send one text-search request.
function M.send_text_search_request (search_terms, regex, body, operators,
                                     overPrivateText_choice)
  local request_form = {
    sexpr.pair(sexpr.symbol('request'), 'text search'),
    sexpr.pair(sexpr.symbol('terms'), search_terms),
    sexpr.pair(sexpr.symbol('regex'), M.bool_to_string(regex)),
    sexpr.pair(sexpr.symbol('body'), M.bool_to_string(body)),
    sexpr.pair(sexpr.symbol('operators'),
               M.bool_to_string(operators)) }
  if overPrivateText_choice then
    table.insert(request_form,
      sexpr.pair(sexpr.symbol('overPrivateText-telescopes'), overPrivateText_choice)) end
  local request = sexpr.to_string(request_form) .. '\n'
  state.register_response_handler('search-results',
    function (_payload_text, response)
      state.response_handler_map['overPrivateText-telescope-confirmation'] = nil
      M.display_search_results(response, search_terms)
    end, true)
  state.register_response_handler('overPrivateText-telescope-confirmation',
    function (_payload_text, response)
      state.response_handler_map['overPrivateText-telescope-confirmation'] = nil
      M.forget_search_results_handler()
      local prompt = payload.field_text(response, 'prompt') or
        'Include overPrivateText telescopes in this search?'
      local choice = vim.fn.confirm(
        prompt, '&Include\n&Exclude', 2) == 1 and 'include' or 'exclude'
      M.request_text_search(
        search_terms, regex, body, operators, choice)
    end, false)
  state.lp_reset()
  client.send_string(request)
end

---Drop the one-shot 'search-results' handler, if it is still waiting,
---and the response it was counted as.
function M.forget_search_results_handler ()
  if state.response_handler_map['search-results'] then
    state.response_handler_map['search-results'] = nil
    state.lp_pending_count = math.max(0, state.lp_pending_count - 1) end
end

---Unwind the search handlers after a local send failure.
function M.cancel_locally_failed_search ()
  M.forget_search_results_handler()
  state.response_handler_map['overPrivateText-telescope-confirmation'] = nil
end

---Display the search buffer: a view buffer registered under the
---view_id 'search:TERMS'.
---@param response any
---@param search_terms string
function M.display_search_results (response, search_terms)
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
  M.display_warnings(response, 'Search completed with warnings')
end

function M.display_warnings (response, headline)
  local warnings = payload.string_list(payload.field(response, 'warnings'))
  if #warnings > 0 then
    messages.big_nonfatal_message(
      'skg://messages/search', headline,
      messages.errors_and_warnings_to_org_string({}, warnings))
  end
end

return M
