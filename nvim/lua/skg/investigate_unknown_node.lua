-- PURPOSE: <localleader>tr (':SkgInvestigateUnknownNode',
-- 'investigate_unknown_node') asks the server what an unknown node is
-- ('docs/diff-view.org', "Investigating an unknown graphnode"): the
-- node its ID resolves to, if any, or else what the git history of
-- every skgrepo says it used to be. It takes the ID from the unknown
-- viewnode at point, or asks for one, and shows the answer, Org text,
-- in the read-only buffer 'skg://unknown-node' with the report keys
-- ('investigate_unknown_node_handler'). Removing references to the
-- unknown node is skg.delete_references_to_absent_node's. The Lua
-- port of elisp/skg-request-investigate-unknown-node.el.

local client = require('skg.client')
local messages = require('skg.messages')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

local BUFFER_NAME = 'skg://unknown-node'

---Investigate the unknown viewnode at point; elsewhere, prompt for
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
