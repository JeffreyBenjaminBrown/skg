-- PURPOSE: Ask the server what an unknown node is: the node its ID
-- resolves to, if any, or else what the git history of every
-- skgrepo says it used to be. Shows the answer, Org text, in a
-- read-only buffer. The Lua port of
-- elisp/skg-request-investigate-unknown-node.el.

local client = require('skg.client')
local messages = require('skg.messages')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

local BUFFER_NAME = 'skg://unknown-node'

---Investigate the unknown headline at point; elsewhere, prompt for
---the ID.
---@param gnid string|nil for tests; taken from point or prompted when nil
function M.investigate_unknown_node (gnid)
  gnid = gnid or metadata.unknown_node_id_at_point()
          or vim.fn.input('Investigate ID: ')
  if gnid == '' then
    vim.notify('No ID to investigate', vim.log.levels.WARN)
    return end
  state.register_response_handler('investigate-unknown-node',
    function (_payload_text, response)
      M.investigate_unknown_node_handler(response)
    end, true)
  state.lp_reset()
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'investigate unknown node'),
    sexpr.pair(sexpr.symbol('id'), gnid) }) .. '\n')
end

---@param response any
function M.investigate_unknown_node_handler (response)
  local content = payload.field_text(response, 'content')
                  or '* investigation failed\n** Empty response\n'
  local errors = payload.string_list(payload.field(response, 'errors'))
  local buf = messages.big_nonfatal_message(
    BUFFER_NAME,
    #errors > 0 and 'The investigation failed' or 'Investigation complete',
    content)
  vim.bo[buf].modifiable = false
  require('skg.keymaps').attach_report(buf)
end

return M
