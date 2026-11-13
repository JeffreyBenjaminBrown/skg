-- PURPOSE: Stage every change and git-commit it, in every owned
-- skgrepo that has changes, with one message
-- ('docs/diff-view.org', "Stage and commit").
-- When the processed summary is incomplete, or some owned gitrepo
-- has entries already staged, the server asks for confirmation
-- instead and changes nothing; only the typed word "yes" resends the
-- request confirmed. Every answer is followed by a rerender
-- stream, which after a commit rerenders the open buffers of the
-- diff view. The Lua port of elisp/skg-request-stage-and-commit.el.
-- The message is written in a buffer of its own: ':w' or
-- <localleader>cc sends it, <localleader>ck cancels. The buffer keeps
-- the message until it is moot (M.report_result).

local buffer = require('skg.buffer')
local messages = require('skg.messages')
local payload = require('skg.payload')
local processed = require('skg.processed')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')

local M = {}

M.commit_message_buffer_name = 'skg://commit-message'

M.commit_message_bindings = { send = 'cc', cancel = 'ck' }

---Open the commit-message buffer in a split, creating it if there is
---none; a message left there by an earlier attempt is kept.
function M.stage_and_commit ()
  M.require_saved_views()
  local buf = M.commit_message_buffer()
  if buf == nil then buf = M.create_commit_message_buffer() end
  vim.cmd('split')
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
  vim.notify(string.format(
    'Write the commit message; :w or <localleader>%s commits, <localleader>%s cancels',
    M.commit_message_bindings.send, M.commit_message_bindings.cancel))
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

---Stage and commit with the text of BUF, the commit-message buffer,
---less its trailing whitespace; then hide BUF, which keeps the text
---until the message is moot (M.report_result). A blank message is
---refused.
---@param buf integer
function M.send_from_buffer (buf)
  local message = table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n'):gsub('%s+$', '')
  if message == '' then
    error('skg: a commit message is required') end
  M.require_saved_views()
  M.send(message, false)
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

---Send the request with MESSAGE, and '(confirmed . t)' when CONFIRMED.
---@param message string
---@param confirmed boolean
function M.send (message, confirmed)
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'stage and commit'),
    M.message_lines(message) }
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
---or nothing needed committing and nothing failed, the commit message
---is moot and its buffer is deleted; otherwise the buffer keeps it,
---and the line says so.
---@param response any
function M.report_result (response)
  local commits = payload.field(response, 'commits')
  local message_moot =
    (commits ~= nil and sexpr.is_list(commits) and #commits > 0)
    or rerender.response_errors_line(response) == nil
  local line = M.result_message(response)
  if message_moot then
    local buf = M.commit_message_buffer()
    if buf then vim.api.nvim_buf_delete(buf, { force = true }) end
  else
    local kept = M.commit_message_kept_sentence('Your')
    if kept ~= '' then line = line .. ' ' .. kept end end
  vim.notify(line)
end

M.already_staged_buffer_name = '*skg already staged*'

---Show the processed summary of RESPONSE, a request for confirmation,
---and how many entries it lists as already staged in each gitrepo,
---the entries themselves in the buffer M.already_staged_buffer_name.
---On "yes" resend MESSAGE confirmed; otherwise, an interrupt at the
---prompt included, say that nothing was committed.
---@param message string
---@param response any
function M.confirm_and_resend (message, response)
  local already_staged = payload.field(response, 'already-staged')
  local listing = M.already_staged_listing(already_staged)
  if listing ~= '' then
    messages.big_nonfatal_message(M.already_staged_buffer_name,
      'Entries already staged are listed in ' .. M.already_staged_buffer_name,
      listing) end
  local summary = processed.processed_summary_from_response(response)
  local ok, typed = pcall(vim.fn.input,
    processed.processed_summary_message(summary):gsub('%.$', '')
    .. M.already_staged_message(already_staged)
    .. '. Type yes to commit anyway: ')
  if ok and typed == 'yes' then
    M.send(message, true)
  else
    local kept = M.commit_message_kept_sentence('your')
    vim.notify(kept == '' and 'Not committed.'
               or 'Not committed; ' .. kept) end
end

---The gitrepos of ALREADY_STAGED, '((GITREPO PATH...) ...)', each with
---its paths, as strings; empty when there is none.
---@param already_staged any
---@return table
function M.already_staged_gitrepos (already_staged)
  local gitrepos = {}
  if already_staged == nil or not sexpr.is_list(already_staged) then
    return gitrepos end
  for _, gitrepo_and_paths in ipairs(already_staged) do
    local paths = {}
    for i = 2, #gitrepo_and_paths do
      table.insert(paths, sexpr.atom_text(gitrepo_and_paths[i])) end
    table.insert(gitrepos, { gitrepo = sexpr.atom_text(gitrepo_and_paths[1]),
                             paths = paths })
  end
  return gitrepos
end

---The prompt's clause for ALREADY_STAGED, '((GITREPO PATH...) ...)':
---how many paths each gitrepo has staged, and where they are listed;
---or the empty string when there is none.
---@param already_staged any
---@return string
function M.already_staged_message (already_staged)
  local gitrepos = M.already_staged_gitrepos(already_staged)
  if #gitrepos == 0 then return '' end
  local counts = {}
  for _, gitrepo in ipairs(gitrepos) do
    table.insert(counts, string.format('%s: %d %s', gitrepo.gitrepo,
      #gitrepo.paths, #gitrepo.paths == 1 and 'path' or 'paths')) end
  return '. Already staged, so committed too: '
    .. table.concat(counts, ', ')
    .. ' (listed in ' .. M.already_staged_buffer_name .. ')'
end

---ALREADY_STAGED, '((GITREPO PATH...) ...)', as org text: a headline
---per gitrepo, then its paths, one per line; empty when there is none.
---@param already_staged any
---@return string
function M.already_staged_listing (already_staged)
  local lines = {}
  for _, gitrepo in ipairs(M.already_staged_gitrepos(already_staged)) do
    table.insert(lines, '* ' .. gitrepo.gitrepo)
    for _, path in ipairs(gitrepo.paths) do table.insert(lines, path) end
  end
  return table.concat(lines, '\n')
end

---The line shown for '((commits ((SKGREPO COMMIT-ID) ...)) (errors
---(...)) (warnings (...)))'.
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
  local errors_line = rerender.response_errors_line(response)
  local parts = {}
  if #committed > 0 then
    table.insert(parts, 'Committed in ' .. table.concat(committed, ', '))
  else
    table.insert(parts, 'Nothing was committed') end
  if errors_line then
    table.insert(parts, '. Errors: ' .. errors_line)
  else
    table.insert(parts, '.') end
  return table.concat(parts)
end

return M
