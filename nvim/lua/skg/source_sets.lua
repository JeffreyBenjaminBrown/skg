-- PURPOSE: List, query, and switch the per-connection active
-- source-set; switching re-renders every open view in place. The Lua
-- port of elisp/skg-request-source-sets.el.

local client = require('skg.client')
local lock = require('skg.lock')
local payload = require('skg.payload')
local picker = require('skg.picker')
local rerender = require('skg.rerender')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Ask the server for the configured source-sets.
function M.list_source_sets ()
  state.register_response_handler('source-sets',
    function (_payload_text, response)
      local active = payload.field_text(response, 'active')
      local sets =
        payload.string_list(payload.field(response, 'sets'))
      vim.notify(string.format(
        'Active source-set: %s; available: %s',
        active or '?', table.concat(sets, ', ')))
    end, true)
  state.lp_reset()
  client.send_string('((request . "list source sets"))\n')
end

---Ask the server for the active source-set.
function M.active_source_set ()
  state.register_response_handler('active-source-set',
    function (_payload_text, response)
      vim.notify(payload.field_text(response, 'content') or '?')
    end, true)
  state.lp_reset()
  client.send_string('((request . "active source set"))\n')
end

---Set the active source-set for this connection to NAME (prompted
---when absent), after confirmation; the server replies with the
---confirmation followed by the rerender stream.
---@param name string|nil
function M.set_active_source_set (name)
  name = name or picker.prompt_for_source_set()
  if not name then return end
  if #vim.api.nvim_list_uis() > 0
     and vim.fn.confirm(
       'Switch source-set and re-render all SKG buffers?',
       '&Yes\n&No', 2) ~= 1 then
    return end
  state.register_response_handler('active-source-set',
    function (_payload_text, response)
      vim.notify(payload.field_text(response, 'content') or '?')
    end, true)
  lock.begin_stream('rerender')
  lock.lock_all_skg_buffers()
  rerender.register_rerender_stream_handlers()
  state.lp_reset()
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'set active source set'),
    sexpr.pair(sexpr.symbol('name'), name) }) .. '\n')
end

return M
