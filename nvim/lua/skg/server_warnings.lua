-- PURPOSE: Show, in M.buffer_name, what the server warned about as it
-- started (unknown skgconfig.toml keys, say). client.connect sends
-- the startup request once per connection, as soon as the connection
-- is up and so before any view is requested. A non-empty answer is
-- appended to M.buffer_name under a headline 'Startup warnings', erasing
-- nothing (the warnings of an earlier connection stay); an empty one
-- opens nothing. The Lua port of elisp/skg-request-server-warnings.el.
--
-- DATA USED/ASSUMED: See docs/api-and-formats.org, "Server warnings".

local messages = require('skg.messages')
local payload = require('skg.payload')
local state = require('skg.state')

local M = {}

---The buffer that shows the server's startup warnings, beside the
---save warnings' 'skg://messages/save-warnings'.
M.buffer_name = 'skg://messages/server-warnings'

---Ask the server on TCP for its startup warnings. A one-shot handler
---shows a non-empty answer. No lp_reset here: this is sent from
---inside client.connect, and the caller's own request follows at once.
---@param tcp any the just-connected tcp handle
function M.request_server_warnings (tcp)
  state.register_response_handler('server-warnings',
    function (_payload, response) M.server_warnings_handler(response) end,
    true)
  tcp:write('((request . "server warnings"))\n')
end

---Append the warnings in a parsed server-warnings RESPONSE, if any,
---to M.buffer_name: a level-1 headline 'Startup warnings', each
---warning a level-2 headline under it.
---@param response any
function M.server_warnings_handler (response)
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

return M
