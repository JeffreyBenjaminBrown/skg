-- PURPOSE: Extend a buffer of the diff view to the changes it does
-- not make editable ('docs/diff-view.org', "Extending the diff
-- view"). A save never makes an occurrence
-- editable or appends a root on its own; this command asks the server
-- to, for the current buffer. The answer carries the readiness
-- summary, as a save's does, and a rerender stream follows it,
-- carrying the rerendered buffer. The Lua port of
-- elisp/skg-request-extend-diff-view.el.

local readiness = require('skg.readiness')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')

local M = {}

function M.diffView_extend ()
  local view_id = vim.b.skg_view_id
  if not view_id then error('skg: not in a skg view') end
  if vim.bo.modified then error('Save the buffer first.') end
  rerender.send_streaming_request('extend diff view',
    'extend-diff-view',
    { sexpr.pair(sexpr.symbol('request'), 'extend diff view'),
      sexpr.pair(sexpr.symbol('view-id'), view_id) },
    function (_payload_text, response) M.handler(response) end)
end

---Show the readiness summary in RESPONSE, or its errors.
---@param response any
function M.handler (response)
  local errors_line = rerender.response_errors_line(response)
  if errors_line then
    vim.notify(errors_line)
    return end
  local summary = readiness.readiness_summary_from_response(response)
  if summary then
    vim.notify(readiness.readiness_summary_message(summary)) end
end

return M
