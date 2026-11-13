-- PURPOSE: Stage every change and git-commit it, in every owned
-- skgrepo that has changes, with one message
-- ('docs/diff-view.org', "Stage and commit").
-- When the readiness summary is incomplete, or some owned gitrepo
-- has entries already staged, the server asks for confirmation
-- instead and changes nothing; only the typed word "yes" resends the
-- request confirmed. Every answer is followed by a rerender
-- stream, which after a commit rerenders the open buffers of the
-- diff view. The Lua port of elisp/skg-request-stage-and-commit.el.

local buffer = require('skg.buffer')
local messages = require('skg.messages')
local payload = require('skg.payload')
local readiness = require('skg.readiness')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')

local M = {}

function M.stage_and_commit ()
  if #buffer.unsaved_view_buffers(nil) > 0 then
    error('skg: save or revert every skg view before staging and committing') end
  local message = vim.fn.input('Commit message: ')
  if vim.trim(message) == '' then
    error('skg: a commit message is required') end
  M.send(message, false)
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
    vim.notify(M.result_message(response))
  end
end

M.already_staged_buffer_name = '*skg already staged*'

---Show the readiness summary of RESPONSE, a request for confirmation,
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
  local summary = readiness.readiness_summary_from_response(response)
  local ok, typed = pcall(vim.fn.input,
    readiness.readiness_summary_message(summary):gsub('%.$', '')
    .. M.already_staged_message(already_staged)
    .. '. Type yes to commit anyway: ')
  if ok and typed == 'yes' then
    M.send(message, true)
  else
    vim.notify('Not committed.') end
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
