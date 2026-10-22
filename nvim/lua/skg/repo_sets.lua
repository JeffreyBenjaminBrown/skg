-- PURPOSE: List, query, and switch the per-connection skgrepo
-- restriction; switching re-renders every open view in place. The Lua
-- port of elisp/skg-request-repo-sets.el.

local client = require('skg.client')
local payload = require('skg.payload')
local picker = require('skg.picker')
local rerender = require('skg.rerender')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Ask the server for the configured skgrepo-sets.
function M.list_repo_sets ()
  state.register_response_handler('repo-sets',
    function (_payload_text, response)
      local restriction = payload.field_text(response, 'restriction')
      local sets =
        payload.string_list(payload.field(response, 'sets'))
      vim.notify(string.format(
        'Skgrepo restriction: %s; available: %s',
        restriction or '?', table.concat(sets, ', ')))
    end, true)
  state.lp_reset()
  client.send_string('((request . "list repo sets"))\n')
end

---Ask the server for the skgrepo restriction.
function M.show_skgrepo_restriction ()
  state.register_response_handler('skgrepo-restriction',
    function (_payload_text, response)
      vim.notify(payload.field_text(response, 'content') or '?')
    end, true)
  state.lp_reset()
  client.send_string('((request . "skgrepo restriction"))\n')
end

---Set the skgrepo restriction for this connection to NAME (prompted
---when absent), after confirmation; the server replies with the
---confirmation followed by the rerender stream.
---@param name string|nil
function M.restrict_repo_set (name, approved_pids)
  name = name or picker.prompt_for_repo_set()
  if not name then return end
  if not approved_pids and #vim.api.nvim_list_uis() > 0
     and vim.fn.confirm(
       'Switch repo-set and re-render all SKG buffers?',
       '&Yes\n&No', 2) ~= 1 then
    return end
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'set skgrepo restriction'),
    sexpr.pair(sexpr.symbol('name'), name) }
  if approved_pids and #approved_pids > 0 then
    local approval = { sexpr.symbol('approved-overPrivateText-pids') }
    for _, pid in ipairs(approved_pids) do table.insert(approval, pid) end
    table.insert(request, approval) end
  rerender.send_streaming_request('rerender', 'skgrepo-restriction',
    request,
    function (_payload_text, response)
      vim.notify(payload.field_text(response, 'content') or '?')
    end, nil,
    function (pids) M.restrict_repo_set(name, pids) end)
end

return M
