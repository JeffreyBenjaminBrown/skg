-- PURPOSE: Show, in M.buffer_name, what the server warned about as it
-- started (unknown skgconfig.toml keys, say) and the buffer
-- corrections it made to each view it refreshed. client.connect sends
-- the startup request once per connection, as soon as the connection
-- is up and so before any view is requested. A non-empty answer is
-- appended to M.buffer_name under a headline 'Startup warnings', erasing
-- nothing (the buffer corrections gathered before a reconnect stay); an
-- empty one opens nothing. The buffer corrections arrive on a save's
-- save-result and on a rerender stream's rerender-done; show_corrections
-- appends them to the buffer, erasing nothing. The Lua port of
-- elisp/skg-request-server-warnings.el.
--
-- DATA USED/ASSUMED: See docs/api-and-formats.org, "Server warnings".
-- The (corrections ...) field is a list of (VIEW_ID MESSAGE ...) entries,
-- one per corrected view; each MESSAGE is a headline line followed by
-- a level-3 subheadline naming the corrected headline, as in
-- docs/api-and-formats.org, "How a message names a headline".

local buffer = require('skg.buffer')
local messages = require('skg.messages')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---The buffer that shows the server's startup warnings and each view's
---buffer corrections, beside the save warnings' 'skg://messages/save-warnings'.
M.buffer_name = 'skg://messages/server-warnings'

---Ask the server on TCP for its startup warnings. A one-shot handler
---shows a non-empty answer. No lp_reset here: this is sent from
---inside client.connect, and the caller's own request follows at once.
---@param tcp any the just-connected tcp handle
function M.request_server_warnings (tcp)
  state.register_response_handler('server-warnings',
    function (_payload, response) M.show_server_warnings(response) end,
    true)
  tcp:write('((request . "server warnings"))\n')
end

---Append the warnings in a parsed server-warnings RESPONSE, if any,
---to M.buffer_name: a level-1 headline 'Startup warnings', each
---warning a level-2 headline under it.
---@param response any
function M.show_server_warnings (response)
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  if #warnings == 0 then return end
  local headlines = { '* Startup warnings' }
  for _, warning in ipairs(warnings) do
    table.insert(headlines, '** ' .. warning) end
  messages.big_nonfatal_message_appended(
    M.buffer_name,
    string.format('The skg server reported %d startup warning%s - see %s',
                  #warnings, #warnings == 1 and '' or 's', M.buffer_name),
    table.concat(headlines, '\n'))
end

---Append CORRECTIONS, the parsed value of a response's 'corrections'
---field (the buffer corrections), to M.buffer_name, and show it. Each
---view gets a level-1 headline, 'Buffer corrections in' and its name
---(its buffer's name, or its view ID when no buffer has it), and each of its messages a level-2 headline. Nil or
---an empty list does nothing.
---@param corrections any
function M.show_corrections (corrections)
  local corrected_views = M.corrected_views(corrections)
  if #corrected_views == 0 then return end
  local headline_count = 0
  for _, corrected_view in ipairs(corrected_views) do
    headline_count = headline_count + #corrected_view.messages end
  messages.big_nonfatal_message_appended(
    M.buffer_name,
    string.format(
      'skg: the server corrected %d headline%s in %d view%s - see %s',
      headline_count, headline_count == 1 and '' or 's',
      #corrected_views, #corrected_views == 1 and '' or 's',
      M.buffer_name),
    M.corrected_views_to_org_string(corrected_views))
end

---CORRECTIONS as a Lua list of { view_id = ..., messages = {...} },
---skipping any entry without messages. The view ID may arrive as a
---symbol or a string; both become text.
---@param corrections any
---@return table[]
function M.corrected_views (corrections)
  local corrected_views = {}
  if corrections == nil or not sexpr.is_list(corrections) then
    return corrected_views end
  for _, entry in ipairs(corrections) do
    if sexpr.is_list(entry) and #entry >= 2
       and not sexpr.is_list(entry[1]) then
      local entry_messages = {}
      for index = 2, #entry do
        if not sexpr.is_list(entry[index]) then
          table.insert(entry_messages, sexpr.atom_text(entry[index]))
        end
      end
      if #entry_messages > 0 then
        table.insert(corrected_views,
                     { view_id = sexpr.atom_text(entry[1]),
                       messages = entry_messages }) end
    end
  end
  return corrected_views
end

---CORRECTED_VIEWS as org text: per view, a level-1 headline naming it
---('Buffer corrections in' the view's name)
---and a level-2 headline per message. A message's own subheadline
---lines (level 3) stay as they are; its trailing newlines go, and the
---Emacs chords it names become Neovim bindings.
---@param corrected_views table[]
---@return string
function M.corrected_views_to_org_string (corrected_views)
  local lines = {}
  for _, corrected_view in ipairs(corrected_views) do
    table.insert(lines,
                 '* Buffer corrections in '
                   .. M.corrected_view_name(corrected_view.view_id))
    for _, message in ipairs(corrected_view.messages) do
      table.insert(lines,
                   '** ' .. messages.with_neovim_bindings(
                     (message:gsub('\n+$', '')) ))
    end
  end
  return table.concat(lines, '\n')
end

---The name of the buffer whose view ID is VIEW_ID, or VIEW_ID itself
---when no buffer has it.
---@param view_id string
---@return string
function M.corrected_view_name (view_id)
  local buf = buffer.find_buffer_by_view_id(view_id)
  if buf then return vim.api.nvim_buf_get_name(buf) end
  return view_id
end

return M
