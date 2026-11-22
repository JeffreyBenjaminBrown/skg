-- PURPOSE: Git-commit the changed nodes of the owned skgrepos, with
-- one message ('docs/diff-view.org', "Stage and commit"):
-- <localleader>rc (':SkgStageAndCommit', 'stage_and_commit')
-- git-commits every node whose changes are all processed, except one
-- that needs a node whose changes are not; <localleader>rC
-- (':SkgStageAndCommitEverything', 'stage_and_commit_everything')
-- git-commits every changed node. Each refuses while a view has
-- unsaved edits, then opens the commit-message buffer
-- ('open_commit_message_buffer'); sending it ('send_from_buffer')
-- sends the request ('send'), and 'result_message' words the answer.
-- Marking changes processed is skg.processed's. The Lua port of
-- elisp/skg-request-stage-and-commit.el.
--
-- A git-commit of everything while some changed node is not processed
-- makes the server ask for confirmation instead, changing nothing;
-- only the typed word "yes" resends the request confirmed
-- ('confirm_and_resend'). A git-commit of the processed asks nothing.
-- Every answer is followed by a rerender stream, which after a
-- git-commit rerenders the open buffers of the diff view.
-- The message is written in a buffer of its own: ':w' or
-- <localleader>cc sends it, <localleader>ck cancels; the buffer
-- remembers which of the two commands opened it last
-- ('vim.b.skg_commit_processed_only'). It keeps the message until it
-- is moot (M.report_result).

local buffer = require('skg.buffer')
local payload = require('skg.payload')
local processed = require('skg.processed')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')

local M = {}

M.commit_message_buffer_name = 'skg://commit-message'

M.commit_message_bindings = { send = 'cc', cancel = 'ck' }

---The number under KEY in RESPONSE, or nil when it has none.
---@param response any
---@param key string
---@return integer|nil
local function count_field (response, key)
  local text = payload.field_text(response, key)
  return text and tonumber(text) or nil
end

---<localleader>rc commits every node whose changes are all processed,
---except one that needs a node whose changes are not. Open the
---commit-message buffer for it; nothing is asked, and the line shown
---afterward says what went in, what is left, and what waits, and for
---what.
function M.stage_and_commit ()
  M.open_commit_message_buffer(true)
end

---Commit every changed node of the owned skgrepos, processed or not,
---asking first, for a typed "yes", when some is not processed. Open
---the commit-message buffer for it.
function M.stage_and_commit_everything ()
  M.open_commit_message_buffer(false)
end

---Open the commit-message buffer in a split, creating it if there is
---none (a message left there by an earlier attempt is kept), and make
---its send commit the processed nodes when PROCESSED_ONLY, or every
---changed node otherwise.
---@param processed_only boolean
function M.open_commit_message_buffer (processed_only)
  M.require_saved_views()
  local buf = M.commit_message_buffer()
  if buf == nil then buf = M.create_commit_message_buffer() end
  vim.b[buf].skg_commit_processed_only = processed_only
  vim.cmd('split')
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
  vim.notify(string.format(
    'Write the commit message; :w or <localleader>%s commits %s, <localleader>%s cancels',
    M.commit_message_bindings.send,
    processed_only and 'the processed nodes' or 'every changed node',
    M.commit_message_bindings.cancel))
end

---Refuse while some skg view has unsaved edits: the rerender after a
---commit would discard them.
function M.require_saved_views ()
  if #buffer.unsaved_view_buffers(nil) > 0 then
    error('skg: save or revert every skg view before staging and committing') end
end

---The commit-message buffer, or nil when there is none.
---@return integer|nil
function M.commit_message_buffer ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.api.nvim_buf_get_name(buf) == M.commit_message_buffer_name then
      return buf end
  end
  return nil
end

---A new, empty commit-message buffer: ':w' and <localleader>cc send
---its text, <localleader>ck cancels.
---@return integer bufnr
function M.create_commit_message_buffer ()
  local call_showing_refusal = require('skg.keymaps').call_showing_refusal
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(buf, M.commit_message_buffer_name)
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].bufhidden = 'hide'
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = 'text'
  vim.api.nvim_create_autocmd('BufWriteCmd', {
    buffer = buf,
    callback = function () call_showing_refusal(M.send_from_buffer, buf) end })
  vim.keymap.set('n', '<localleader>' .. M.commit_message_bindings.send,
                 function () call_showing_refusal(M.send_from_buffer, buf) end,
                 { buffer = buf, desc = 'Stage and commit with this message' })
  vim.keymap.set('n', '<localleader>' .. M.commit_message_bindings.cancel,
                 function () call_showing_refusal(M.cancel, buf) end,
                 { buffer = buf, desc = 'Cancel the commit' })
  return buf
end

---Commit with the text of BUF, the commit-message buffer, less its
---trailing whitespace: the processed nodes, or every changed node,
---whichever command opened BUF last. Then hide BUF, which keeps the
---text until the message is moot (M.report_result). A blank message
---is refused.
---@param buf integer
function M.send_from_buffer (buf)
  local message = table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n'):gsub('%s+$', '')
  if message == '' then
    error('skg: a commit message is required') end
  M.require_saved_views()
  M.send(message, vim.b[buf].skg_commit_processed_only ~= false, false)
  vim.bo[buf].modified = false
  M.hide(buf)
end

---Kill BUF, the commit-message buffer, and commit nothing.
---@param buf integer
function M.cancel (buf)
  vim.api.nvim_buf_delete(buf, { force = true })
  vim.notify('Commit canceled; nothing was staged or committed.')
end

---Close each window showing BUF, keeping BUF; a window that is the
---last one shows the alternate buffer, or an empty one, instead.
---@param buf integer
function M.hide (buf)
  for _, win in ipairs(vim.fn.win_findbuf(buf)) do
    if #vim.api.nvim_list_wins() > 1 then
      vim.api.nvim_win_close(win, false)
    else
      vim.api.nvim_win_call(win, function ()
        local alternate = vim.fn.bufnr('#')
        if alternate > 0 and alternate ~= buf
           and vim.api.nvim_buf_is_valid(alternate) then
          vim.api.nvim_win_set_buf(win, alternate)
        else vim.cmd('enew') end
      end)
    end
  end
end

---Where the commit message is kept, as a sentence opening with
---FIRST_WORD, or the empty string when its buffer is gone.
---@param first_word string
---@return string
function M.commit_message_kept_sentence (first_word)
  if M.commit_message_buffer() == nil then return '' end
  return string.format('%s message is kept in %s.', first_word,
                       M.commit_message_buffer_name)
end

---Send the request with MESSAGE, with '(processed-only . t)' when
---PROCESSED_ONLY, and '(confirmed . t)' when CONFIRMED.
---@param message string
---@param processed_only boolean
---@param confirmed boolean
function M.send (message, processed_only, confirmed)
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'stage and commit'),
    M.message_lines(message) }
  if processed_only then
    table.insert(request, sexpr.pair(sexpr.symbol('processed-only'),
                                     sexpr.symbol('t'))) end
  if confirmed then
    table.insert(request, sexpr.pair(sexpr.symbol('confirmed'),
                                     sexpr.symbol('t'))) end
  rerender.send_streaming_request('stage and commit', 'stage-and-commit',
    request,
    function (_payload_text, response) M.handler(message, response) end)
end

---'(message-lines "LINE" ...)': MESSAGE one string per line, since
---the server reads one request per line.
---@param message string
---@return table
function M.message_lines (message)
  local lines = { sexpr.symbol('message-lines') }
  for _, line in ipairs(vim.split(message, '\n', { plain = true })) do
    table.insert(lines, line) end
  return lines
end

---On a request for confirmation, ask for it once the empty rerender
---stream that follows is done, and on "yes" resend MESSAGE
---confirmed. Otherwise report what was committed.
---@param message string
---@param response any
function M.handler (message, response)
  if payload.field(response, 'needs-confirmation') ~= nil then
    rerender.after_empty_stream = function ()
      M.confirm_and_resend(message, response) end
  else
    M.report_result(response)
  end
end

---Show the line for RESPONSE, a result. Once something was committed,
---or nothing had changed and nothing failed, the commit message is
---moot and its buffer is deleted; otherwise (nothing processed,
---everything processed waiting, a refusal, an error) the buffer keeps
---it, and the line says so.
---@param response any
function M.report_result (response)
  local commits = payload.field(response, 'commits')
  local message_moot =
    (commits ~= nil and sexpr.is_list(commits) and #commits > 0)
    or (rerender.response_errors_line(response) == nil
        and (count_field(response, 'left-nodes') or 0) == 0)
  local line = M.result_message(response)
  if message_moot then
    local buf = M.commit_message_buffer()
    if buf then vim.api.nvim_buf_delete(buf, { force = true }) end
  else
    local kept = M.commit_message_kept_sentence('Your')
    if kept ~= '' then line = line .. ' ' .. kept end end
  vim.notify(line)
end

---Show the processed summary of RESPONSE, a request for confirmation,
---which only a commit of everything gets. On "yes" resend MESSAGE as a
---commit of everything, confirmed; otherwise, an interrupt at the
---prompt included, say that nothing was committed.
---@param message string
---@param response any
function M.confirm_and_resend (message, response)
  local summary = processed.processed_summary_from_response(response)
  local ok, typed = pcall(vim.fn.input,
    processed.processed_summary_message(summary):gsub('%.$', '')
    .. '. Type yes to commit anyway: ')
  if ok and typed == 'yes' then
    M.send(message, false, true)
  else
    local kept = M.commit_message_kept_sentence('your')
    vim.notify(kept == '' and 'Not committed.'
               or 'Not committed; ' .. kept) end
end

---COUNT and NOUN, made plural by an "s" unless COUNT is 1.
---@param count integer
---@param noun string
---@return string
function M.count_phrase (count, noun)
  return string.format('%d %s%s', count, noun, count == 1 and '' or 's')
end

---The entries of RESPONSE's 'waiting', each '(PID TITLE REASON PID
---TITLE)', as tables { title, reason, other_title }.
---@param response any
---@return table[]
function M.waiting_entries (response)
  local entries = {}
  local waiting = payload.field(response, 'waiting')
  if waiting == nil or not sexpr.is_list(waiting) then return entries end
  for _, entry in ipairs(waiting) do
    if sexpr.is_list(entry) and #entry >= 5 then
      table.insert(entries, { title = sexpr.atom_text(entry[2]),
                              reason = sexpr.atom_text(entry[3]),
                              other_title = sexpr.atom_text(entry[5]) }) end
  end
  return entries
end

---What ENTRY, as M.waiting_entries gives it, waits for, and why.
---@param entry table
---@return string
function M.waiting_clause (entry)
  if entry.reason == 'names-new' then
    return string.format("'%s' names '%s', which is new and does not go in",
                         entry.title, entry.other_title)
  elseif entry.reason == 'still-named' then
    return string.format(
      "'%s' is deleted, but '%s', which does not go in, still names it",
      entry.title, entry.other_title)
  elseif entry.reason == 'merge' then
    return string.format(
      "'%s' is part of a merge with '%s', which does not go in",
      entry.title, entry.other_title)
  end
  return string.format("'%s' waits for '%s' (%s)",
                       entry.title, entry.other_title, entry.reason)
end

---The entries of RESPONSE's 'unmarkable', each '(PID TITLE (CHANGE
---...))', as tables { title, changes }.
---@param response any
---@return table[]
function M.unmarkable_entries (response)
  local entries = {}
  local unmarkable = payload.field(response, 'unmarkable')
  if unmarkable == nil or not sexpr.is_list(unmarkable) then return entries end
  for _, entry in ipairs(unmarkable) do
    if sexpr.is_list(entry) and #entry >= 3 and sexpr.is_list(entry[3]) then
      local changes = {}
      for _, change in ipairs(entry[3]) do
        table.insert(changes, sexpr.atom_text(change)) end
      table.insert(entries, { title = sexpr.atom_text(entry[2]),
                              changes = changes }) end
  end
  return entries
end

---ENTRY, as M.unmarkable_entries gives it: the node's title, and how
---it changed, in the server's words.
---@param entry table
---@return string
function M.unmarkable_clause (entry)
  return string.format("'%s' (%s)", entry.title, table.concat(entry.changes, ', '))
end

---The line shown for a result, '((commits ((SKGREPO COMMIT-ID) ...))
---(committed-nodes N) (left-nodes L) (waiting ((PID TITLE REASON PID
---TITLE) ...)) (unmarkable ((PID TITLE (CHANGE ...)) ...)) (errors
---(...)) (warnings (...)))': what went in, how many changed nodes are
---left, for each processed node that waits, what it waits for, and for
---each node left because it changed in a way no mark covers, how.
---@param response any
---@return string
function M.result_message (response)
  local commits = payload.field(response, 'commits')
  local committed = {}
  if commits ~= nil and sexpr.is_list(commits) then
    for _, commit in ipairs(commits) do
      if sexpr.is_list(commit) and #commit >= 2 then
        table.insert(committed, string.format('%s (%s)',
          sexpr.atom_text(commit[1]),
          sexpr.atom_text(commit[2]):sub(1, 7))) end
    end
  end
  local committed_nodes = count_field(response, 'committed-nodes')
  local left = count_field(response, 'left-nodes') or 0
  local waiting = M.waiting_entries(response)
  local unmarkable = M.unmarkable_entries(response)
  local errors_line = rerender.response_errors_line(response)
  local parts = {}
  if #committed > 0 then
    table.insert(parts, 'Committed '
      .. (committed_nodes and M.count_phrase(committed_nodes, 'node') .. ', ' or '')
      .. 'in ' .. table.concat(committed, ', ') .. '.')
  elseif errors_line or #waiting > 0 or #unmarkable > 0 then
    table.insert(parts, 'Nothing was committed.')
  elseif left == 0 then
    table.insert(parts, 'Nothing has changed, so nothing was committed.')
  else
    table.insert(parts, 'Nothing is processed, so nothing was committed;'
      .. ' <localleader>rC commits everything.') end
  if left > 0 and (#committed > 0 or #waiting > 0 or #unmarkable > 0) then
    table.insert(parts, ' ' .. M.count_phrase(left, 'changed node')
      .. (left == 1 and ' is left' or ' are left'))
    if #waiting > 0 then
      local clauses = {}
      for _, entry in ipairs(waiting) do
        table.insert(clauses, M.waiting_clause(entry)) end
      table.insert(parts, string.format('; %d of them %s: %s', #waiting,
        #waiting == 1 and 'is processed but waits' or 'are processed but wait',
        table.concat(clauses, '; '))) end
    if #unmarkable > 0 then
      local clauses = {}
      for _, entry in ipairs(unmarkable) do
        table.insert(clauses, M.unmarkable_clause(entry)) end
      table.insert(parts, string.format(
        '; %d of them changed in %s no mark covers, which <localleader>rC or Git can commit: %s',
        #unmarkable, #unmarkable == 1 and 'a way' or 'ways',
        table.concat(clauses, '; '))) end
    table.insert(parts, '.') end
  if errors_line then
    table.insert(parts, ' Errors: ' .. errors_line) end
  return table.concat(parts)
end

return M
