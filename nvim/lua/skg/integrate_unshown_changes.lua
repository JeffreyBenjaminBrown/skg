-- PURPOSE: Integrate the changes not editable in a buffer of the
-- diff view ('docs/diff-view.org', "Integrating unshown
-- changes"). A save never makes an occurrence
-- editable or appends a root on its own; this command asks the server
-- to, for the current buffer. The answer carries the readiness
-- summary, as a save's does, and a rerender stream follows it,
-- carrying the redrawn buffer. The Lua port of
-- elisp/skg-request-integrate-unshown-changes.el.

local client = require('skg.client')
local lock = require('skg.lock')
local payload = require('skg.payload')
local rerender = require('skg.rerender')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

function M.diffView_integrate_unshown_changes ()
  local view_id = vim.b.skg_view_id
  if not view_id then error('skg: not in a skg view') end
  if vim.bo.modified then error('Save the buffer first.') end
  lock.begin_stream('integrate unshown changes')
  lock.lock_all_skg_buffers()
  rerender.register_rerender_stream_handlers()
  state.register_response_handler('integrate-unshown-changes',
    function (_payload_text, response) M.handler(response) end,
    true)
  state.lp_reset()
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'integrate unshown changes'),
    sexpr.pair(sexpr.symbol('view-id'), view_id) }) .. '\n')
end

---Show the readiness summary in RESPONSE, or its errors.
---@param response any
function M.handler (response)
  local errors = payload.string_list(payload.field(response, 'errors'))
  if #errors > 0 then
    vim.notify(table.concat(errors, '; '))
    return end
  local summary = save.readiness_summary_from_response(response)
  if summary then
    vim.notify(save.readiness_summary_message(summary)) end
end

return M
