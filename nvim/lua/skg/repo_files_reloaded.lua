-- PURPOSE: Handle the message the server sends unprompted when it has
-- reloaded '.skg' files that changed on disk (another writer, a
-- 'git pull'): 'repo-files-reloaded', with (reloaded ((SKGREPO
-- N) ...)), (warnings (...)) and (view-ids (...)).
--
-- A message with no warnings is a notification of its counts ('skg:
-- reloaded 2 files from repo first'). One that carries a warning (a
-- file that could not be read, a batch that failed validation, a
-- leak) is appended, counts and warnings, to the server-warnings
-- buffer under a headline 'Repo changes', as the startup warnings
-- are, and that buffer is shown. The views it
-- names show a changed node; each one whose buffer holds no unsaved
-- edits is rerendered, through a 'rerender all views' request that
-- excludes every other view; the server also extends each buffer of
-- the diff view among them to the changes it does not show. A buffer
-- with unsaved edits keeps them; the server refuses its save if it
-- would write a node changed on disk since it was rendered. The Lua
-- port of elisp/skg-repo-files-reloaded.el.
--
-- DATA USED/ASSUMED: See docs/api-and-formats.org, "Repo files
-- reloaded".

local lock = require('skg.lock')
local messages = require('skg.messages')
local payload = require('skg.payload')
local server_warnings = require('skg.server_warnings')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---How long to wait before asking again for a rerender that another
---streaming request in flight blocked.
M.retry_milliseconds = 500

---How many times to ask again before giving up on a blocked rerender.
M.retry_limit = 120

---Register the handler as the unprompted one for its response type.
function M.register ()
  state.unprompted_response_handler_map['repo-files-reloaded'] =
    function (_payload_text, response) M.handler(response) end
end

---Show the counts and warnings of a parsed repo-files-reloaded
---RESPONSE and rerender the views it names that have no unsaved edits.
---@param response any
function M.handler (response)
  local count_clauses =
    M.count_clauses(payload.field(response, 'reloaded'))
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  M.show(count_clauses, warnings)
  local view_ids =
    payload.string_list(payload.field(response, 'view-ids'))
  if #view_ids > 0 then
    -- Not from inside the receive callback: the rerender sends a request.
    vim.schedule(function () M.rerender_views_changed_on_disk(view_ids, 0) end)
  end
end

---'reloaded N files from repo NAME' for each (NAME N) of the parsed
---RELOADED list.
---@param reloaded any
---@return string[]
function M.count_clauses (reloaded)
  local clauses = {}
  if reloaded ~= nil and sexpr.is_list(reloaded) then
    for _, skgrepo_and_count in ipairs(reloaded) do
      local skgrepo = sexpr.atom_text(skgrepo_and_count[1])
      local count = sexpr.atom_text(skgrepo_and_count[2])
      table.insert(clauses, 'reloaded ' .. count
                   .. (count == '1' and ' file' or ' files')
                   .. ' from repo ' .. skgrepo)
    end
  end
  return clauses
end

---Notify COUNT_CLAUSES when there are no WARNINGS; otherwise append
---the clauses and the warnings to the server-warnings buffer and show
---it.
---@param count_clauses string[]
---@param warnings string[]
function M.show (count_clauses, warnings)
  if #warnings > 0 then
    local headlines = { '* Repo changes' }
    for _, line in ipairs(count_clauses) do
      table.insert(headlines, '** ' .. line) end
    for _, warning in ipairs(warnings) do
      table.insert(headlines, '** ' .. warning) end
    messages.big_nonfatal_message_appended(
      server_warnings.buffer_name,
      'skg: files changed on disk were reloaded - see '
        .. server_warnings.buffer_name,
      table.concat(headlines, '\n'))
  elseif #count_clauses > 0 then
    vim.notify('skg: ' .. table.concat(count_clauses, '; '))
  end
end

---Each open skg view buffer, as { view_id = ..., buf = ... }.
---@return table[]
function M.open_view_buffers ()
  local open = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      local view_id = vim.b[buf].skg_view_id
      if view_id then table.insert(open, { view_id = view_id, buf = buf }) end
    end
  end
  return open
end

---Rerender each open buffer among VIEW_IDS that holds no unsaved
---edits, excluding every other view. While another streaming request
---is in flight, ask again later; ATTEMPT counts the tries.
---@param view_ids string[]
---@param attempt integer
function M.rerender_views_changed_on_disk (view_ids, attempt)
  local named = {}
  for _, view_id in ipairs(view_ids) do named[view_id] = true end
  local to_rerender, excluded = {}, {}
  for _, open in ipairs(M.open_view_buffers()) do
    if named[open.view_id] and not vim.bo[open.buf].modified then
      table.insert(to_rerender, open.view_id)
    else
      table.insert(excluded, open.view_id) end
  end
  if #to_rerender == 0 then return end
  if lock.stream_in_progress then
    if attempt < M.retry_limit then
      vim.defer_fn(function ()
        M.rerender_views_changed_on_disk(view_ids, attempt + 1)
      end, M.retry_milliseconds)
    else
      vim.notify('skg: views showing files changed on disk were not '
                 .. 'rerendered; open them again to see the changes') end
    return
  end
  require('skg.rerender_all_views').request_rerender_views_excluding(excluded)
end

return M
