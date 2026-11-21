-- PURPOSE: Write a read receipt for the item at the cursor, in a view
-- of someone's mailbox to you: the server writes a note, how you
-- handled the item, as the first child of your fork of it (forking it
-- first if you hold none), and hides the item from your fork of the
-- mailbox, in one write, then redraws the views that show them. When
-- the server needs a choice (the mailbox, where the item is in
-- several; the skgrepo of a new fork, where nothing safe suggests
-- one), it refuses, naming the choices; this asks, and sends again.
-- The Lua port of elisp/skg-request-receipt.el.
--
-- DATA USED/ASSUMED: See docs/api-and-formats.org, "Receipt", and
-- docs/forks.org, "Read receipts".

local metadata = require('skg.metadata')
local payload = require('skg.payload')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')

local M = {}

---The receipt's text when you type none, and its title.
M.default_text = 'Read.'

---The number of the headline line at or above LINE_NUMBER, or nil.
---@param line_number integer
---@return integer|nil
local function headline_line_at_or_above (line_number)
  for candidate = line_number, 1, -1 do
    if metadata.line_text(candidate):match('^%*+%s') then
      return candidate end
  end
  return nil
end

---The texts of the list under KEY in RESPONSE, or {}.
---@param response any
---@param key string
---@return string[]
local function strings_of (response, key)
  local value = payload.field(response, key)
  local texts = {}
  if sexpr.is_list(value) then
    for _, element in ipairs(value) do
      table.insert(texts, sexpr.atom_text(element)) end end
  return texts
end

---Ask the server for a receipt for ITEM saying TEXT, hiding it from
---MAILBOX (inferred when nil), forking it into FORK_INTO (the
---server's choice when nil). The views with unsaved edits are not
---redrawn.
---@param item string
---@param text string
---@param mailbox string|nil
---@param fork_into string|nil
function M.send (item, text, mailbox, fork_into)
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'receipt'),
    sexpr.pair(sexpr.symbol('item'), item),
    sexpr.pair(sexpr.symbol('text'), text) }
  if mailbox then
    table.insert(request, sexpr.pair(sexpr.symbol('mailbox'), mailbox)) end
  if fork_into then
    table.insert(request, sexpr.pair(sexpr.symbol('fork-into'), fork_into)) end
  local excluded = { sexpr.symbol('exclude-view-ids') }
  for _, view_id in ipairs(rerender.dirty_view_buffers()) do
    table.insert(excluded, view_id) end
  table.insert(request, excluded)
  rerender.send_streaming_request('receipt', 'receipt', request,
    function (_payload_text, response)
      M.handler(item, text, mailbox, fork_into, response) end)
end

---Show what the receipt RESPONSE says it wrote. A refusal that names
---choices asks for one once the empty rerender stream after it is
---done, and sends the receipt for ITEM, TEXT, MAILBOX and FORK_INTO
---again with it; any other refusal is shown.
---@param item string
---@param text string
---@param mailbox string|nil
---@param fork_into string|nil
---@param response any
function M.handler (item, text, mailbox, fork_into, response)
  local refusal = payload.field_text(response, 'refusal')
  if not refusal then
    vim.notify(table.concat(strings_of(response, 'lines'), '\n'))
    return end
  local skgrepos = strings_of(response, 'choose-skgrepo')
  local mailboxes = strings_of(response, 'choose-mailbox')
  local titles = strings_of(response, 'choose-mailbox-titles')
  if #skgrepos > 0 then
    rerender.after_empty_stream = function ()
      vim.ui.select(skgrepos, { prompt = refusal .. '\nFork into: ' },
        function (choice)
          if choice then M.send(item, text, mailbox, choice) end end)
    end
  elseif #mailboxes > 0 then
    local labels = {}
    for index, pid in ipairs(mailboxes) do
      labels[index] = (titles[index] or pid) .. ' (' .. pid .. ')' end
    rerender.after_empty_stream = function ()
      vim.ui.select(labels, { prompt = refusal .. '\nHide it from: ' },
        function (_choice, index)
          if index then M.send(item, text, mailboxes[index], fork_into) end end)
    end
  else
    vim.notify('skg receipt: ' .. refusal)
  end
end

---Write a read receipt for the node of the headline at or above the
---cursor: one line saying how you handled it ("Read." by default)
---goes under your fork of it, which is made if you hold none, and the
---node is hidden from your fork of the mailbox it came in. Refuses
---while this buffer has unsaved edits, which the redraw would
---replace; other buffers with unsaved edits are left as they are.
function M.receipt ()
  local buf = vim.api.nvim_get_current_buf()
  if not vim.b[buf].skg_view_id then error('Not in a skg view', 0) end
  if vim.bo[buf].modified then
    error('Save or revert this buffer first: the receipt redraws it,'
          .. ' which would lose its unsaved edits', 0) end
  local line = headline_line_at_or_above(metadata.current_line_number())
  local item = line and metadata.node_id(metadata.metadata_sexp_at_line_or_nil(line))
  if not item then error("Point must be on a node's headline", 0) end
  local text = vim.fn.input('Receipt (default ' .. M.default_text .. '): ')
  if text == '' then text = M.default_text end
  M.send(item, text, nil, nil)
end

return M
