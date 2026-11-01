-- PURPOSE: Handle the message the server sends unprompted when it has
-- integrated '.skg' files that changed on disk (another writer, a
-- 'git pull'): 'repo-files-integrated', with (warnings (...)) and
-- (view-ids (...)).
--
-- A message whose warnings only count the files integrated per
-- skgrepo is a notification ('skg: repo first: 2 files integrated').
-- One that carries any other warning (a file that could not be read,
-- a batch that failed validation, a leak) is appended, whole, to the
-- server-warnings buffer under a headline 'Repo changes', as the
-- startup warnings are, and that buffer is shown. The views it
-- names show a changed node; each one whose buffer holds no unsaved
-- edits is rerendered, through a 'rerender all views' request that
-- excludes every other view. A buffer with unsaved edits keeps them;
-- the server refuses its save if it would write a node changed on
-- disk since it was rendered. The Lua port of
-- elisp/skg-repo-files-integrated.el.
--
-- DATA USED/ASSUMED: See docs/api-and-formats.org, "Repo files
-- integrated".

local lock = require('skg.lock')
local messages = require('skg.messages')
local payload = require('skg.payload')
local server_warnings = require('skg.server_warnings')
local state = require('skg.state')

local M = {}

---How long to wait before asking again for a rerender that another
---streaming request in flight blocked.
M.retry_milliseconds = 500

---How many times to ask again before giving up on a blocked rerender.
M.retry_limit = 120

---A warning of a repo-files-integrated message that only counts the
---files integrated in one skgrepo, as 'warnings_of_notice' in
---'server/serve/handlers/repo_files_integrated.rs' words it. The
---captures are the count and the skgrepo.
M.count_pattern =
  "^Integrated (%d+) files? that changed on disk in skgrepo '(.*)'%.$"

---Register the handler as the unprompted one for its response type.
function M.register ()
  state.unprompted_response_handler_map['repo-files-integrated'] =
    function (_payload_text, response) M.handler(response) end
end

---Show the warnings of a parsed repo-files-integrated RESPONSE and
---rerender the views it names that have no unsaved edits.
---@param response any
function M.handler (response)
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  if #warnings > 0 then M.show_warnings(warnings) end
  local view_ids =
    payload.string_list(payload.field(response, 'view-ids'))
  if #view_ids > 0 then
    -- Not from inside the receive callback: the rerender sends a request.
    vim.schedule(function () M.rerender_views_changed_on_disk(view_ids, 0) end)
  end
end

---Notify WARNINGS when each only counts the files integrated in a
---skgrepo; otherwise append them all to the server-warnings buffer
---and show it.
---@param warnings string[]
function M.show_warnings (warnings)
  local clauses = {}
  for _, warning in ipairs(warnings) do
    local count, skgrepo = warning:match(M.count_pattern)
    if not count then
      local headlines = { '* Repo changes' }
      for _, each in ipairs(warnings) do
        table.insert(headlines, '** ' .. each) end
      messages.big_nonfatal_message_appended(
        server_warnings.buffer_name,
        'skg: files changed on disk were integrated - see '
          .. server_warnings.buffer_name,
        table.concat(headlines, '\n'))
      return
    end
    table.insert(clauses, 'repo ' .. skgrepo .. ': ' .. count
                 .. (count == '1' and ' file' or ' files') .. ' integrated')
  end
  vim.notify('skg: ' .. table.concat(clauses, '; '))
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
