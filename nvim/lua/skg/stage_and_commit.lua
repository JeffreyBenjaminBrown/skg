-- PURPOSE: Stage every change and git-commit it, in every owned
-- skgrepo that has changes, with one message
-- ('docs/diff-view.org', "Stage and commit").
-- When the readiness summary is incomplete, the server answers with
-- it instead and changes nothing; only the typed word "yes" resends
-- the request confirmed. Every answer is followed by a rerender
-- stream, which after a commit redraws the open buffers of the
-- diff view. The Lua port of elisp/skg-request-stage-and-commit.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local lock = require('skg.lock')
local payload = require('skg.payload')
local readiness = require('skg.readiness')
local rerender = require('skg.rerender')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

function M.stage_and_commit ()
  if #buffer.other_unsaved_skg_buffers(nil) > 0 then
    error('skg: save or revert every view before staging and committing') end
  local message = vim.fn.input('Commit message: ')
  if vim.trim(message) == '' then
    error('skg: a commit message is required') end
  M.send(message, false)
end

---Send the request with MESSAGE, and '(confirmed . t)' when CONFIRMED.
---@param message string
---@param confirmed boolean
function M.send (message, confirmed)
  lock.begin_stream('stage and commit')
  lock.lock_all_skg_buffers()
  rerender.register_rerender_stream_handlers()
  state.register_response_handler('stage-and-commit',
    function (_payload_text, response) M.handler(message, response) end,
    true)
  state.lp_reset()
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'stage and commit'),
    sexpr.pair(sexpr.symbol('message'), message) }
  if confirmed then
    table.insert(request, sexpr.pair(sexpr.symbol('confirmed'),
                                     sexpr.symbol('t'))) end
  client.send_string(sexpr.to_string(request) .. '\n')
end

---On a request for confirmation, ask for it, and on "yes" resend
---MESSAGE confirmed once the empty rerender stream that follows is
---done. Otherwise report what was committed.
---@param message string
---@param response any
function M.handler (message, response)
  if payload.field(response, 'needs-confirmation') ~= nil then
    local summary = readiness.readiness_summary_from_response(response)
    local typed = vim.fn.input(
      readiness.readiness_summary_message(summary)
      .. '. Type yes to commit anyway: ')
    if typed == 'yes' then
      rerender.after_empty_stream = function () M.send(message, true) end
    end
  else
    vim.notify(M.result_message(response))
  end
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
  local errors = payload.string_list(payload.field(response, 'errors'))
  local parts = {}
  if #committed > 0 then
    table.insert(parts, 'Committed in ' .. table.concat(committed, ', '))
  else
    table.insert(parts, 'Nothing was committed') end
  if #errors > 0 then
    table.insert(parts, '. Errors: ' .. table.concat(errors, '; '))
  else
    table.insert(parts, '.') end
  return table.concat(parts)
end

return M
