-- PURPOSE: List, query, and switch the per-connection
-- skgrepo restriction; switching re-renders every open view in
-- place. The Lua port of elisp/skg-request-repo-sets.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local config = require('skg.config')
local payload = require('skg.payload')
local picker = require('skg.picker')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Ask the server for the skgrepo restriction and its choices; shows
---each repo by its name, the most private one with ' (no
---restriction)' appended.
function M.list_repo_sets ()
  state.register_response_handler('repo-sets',
    function (_payload_text, response)
      local restriction = payload.field_text(response, 'restriction')
      local sets =
        payload.string_list(payload.field(response, 'sets'))
      vim.notify(string.format(
        'Skgrepo restriction: %s; available: %s',
        restriction and config.skgrepo_restriction_label(restriction) or '?',
        table.concat(vim.tbl_map(config.skgrepo_restriction_label, sets),
                     ', ')))
    end, true)
  state.lp_reset()
  client.send_string('((request . "list repo sets"))\n')
end

---Ask the server for the skgrepo restriction.
function M.show_skgrepo_restriction ()
  state.register_response_handler('skgrepo-restriction',
    function (_payload_text, response)
      vim.notify(M.skgrepo_restriction_message(response))
    end, true)
  state.lp_reset()
  client.send_string('((request . "skgrepo restriction"))\n')
end

---What to show for a skgrepo-restriction RESPONSE: a refusal carries
---its words in 'content'; otherwise the response names the skgrepo
---restriction, shown by its label.
---@return string
function M.skgrepo_restriction_message (response)
  local content = payload.field_text(response, 'content')
  if content then return content end
  local restriction = payload.field_text(response, 'restriction')
  return 'Skgrepo restriction: '
    .. (restriction and config.skgrepo_restriction_label(restriction) or '?')
end

---Set the skgrepo restriction for this connection to NAME, the most
---private repo to show (prompted when absent), after confirmation;
---the server replies with the confirmation followed by the rerender
---stream. Refuses while any view
---has unsaved edits, which the rerender would replace.
---@param name string|nil
function M.restrict_repo_set (name, approved_pids)
  if #buffer.unsaved_view_buffers(nil) > 0 then
    error('skg: save or revert every skg view before switching the skgrepo restriction')
  end
  name = name or picker.prompt_for_repo_set()
  if not name then return end
  if not approved_pids and #vim.api.nvim_list_uis() > 0
     and vim.fn.confirm(
       'Switch the skgrepo restriction and re-render all SKG buffers?',
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
      vim.notify(M.skgrepo_restriction_message(response))
    end, nil,
    function (pids) M.restrict_repo_set(name, pids) end)
end

return M
