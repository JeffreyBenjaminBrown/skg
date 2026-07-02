-- PURPOSE: The small one-shot request/response commands:
-- verify-connection now; rebuild-dbs joins when the stream consumers
-- land. The Lua port of elisp/skg-request-verify-connection.el (and
-- eventually skg-request-rebuild-dbs.el).

local client = require('skg.client')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---The (content "...") value of parsed RESPONSE, or nil.
---Handles both list entries (content "x") and pairs (content . "x").
---@param response any
---@param key string
---@return any|nil
function M.response_field (response, key)
  if not sexpr.is_list(response) then return nil end
  for _, element in ipairs(response) do
    if sexpr.is_pair(element)
       and not sexpr.is_list(element.car)
       and sexpr.atom_text(element.car) == key then
      return element.cdr end
    if sexpr.is_list(element) and #element >= 2
       and not sexpr.is_list(element[1])
       and sexpr.atom_text(element[1]) == key then
      return element[2] end
  end
  return nil
end

---Verify the connection to the Rust server by sending a simple ping;
---the server's confirmation is echoed to the user.
function M.connection_verify ()
  state.register_response_handler('verify-connection',
    function (_payload, response)
      local content = M.response_field(response, 'content')
      local message = 'connected'
      if content ~= nil and not sexpr.is_nil(content) then
        message = sexpr.is_list(content) and sexpr.to_string(content)
                  or sexpr.atom_text(content) end
      vim.notify(message)
    end, true)
  state.lp_reset()
  client.send_string('((request . "verify connection"))\n')
end

return M
