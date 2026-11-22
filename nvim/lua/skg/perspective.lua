-- PURPOSE: 'cycle_perspective' (':SkgCyclePerspective',
-- '<localleader>vo'): cycle the perspective of the current view: the
-- skgrepo whose list of skgrepos ranks the mirrors it shows in a
-- note's place. A fresh view is drawn from each root's home skgrepo;
-- the cycle then holds each skgrepo home to a mirror in the view
-- whose perspective would show the view differently, so that you can
-- choose among them (a private and a public fork, say). The server
-- computes the cycle afresh at each request, switches the view to the
-- next perspective and redraws it; the answer names the view and the
-- perspective ('handler'), and a rerender stream follows, carrying
-- the redrawn buffer, which keeps the cursor and folds as any redraw
-- the user did not ask for does. Sending the request and receiving
-- the stream are skg.rerender_all_views's. The Lua port of
-- elisp/skg-request-perspective.el.
--
-- DATA USED/ASSUMED: See docs/api-and-formats.org, "Set
-- perspective". The buffer keeps the perspective in
-- 'vim.b.skg_perspective' (nil for the root homes, else its
-- wire name) and 'vim.b.skg_perspective_name' (the server's
-- name for it), which 'buffer.set_perspective' sets; each
-- save sends the first ('save.save_request_sexp') and a fresh view
-- clears both ('buffer.open_org_buffer_from_text').

local buffer = require('skg.buffer')
local log = require('skg.log')
local payload = require('skg.payload')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Show this view from the next perspective: the skgrepo whose list of
---skgrepos (it, then the config's declaration order) ranks the
---mirrors it shows in a note's place. A fresh view is drawn from each
---root's home skgrepo. The cycle then holds each skgrepo home to a
---mirror in the view whose perspective would show this view
---differently, in declaration order, each in every tree; after the
---last, the root homes again. Each switch names the perspective it
---shows; 'buffer.perspective_mode_line' names it too, unless it is
---the root homes, and a save keeps it.
---Refuses while the buffer has unsaved edits, which the redraw would
---replace. APPROVED_PIDS approve the text a privacy challenge asked
---about.
---@param approved_pids string[]|nil
function M.cycle_perspective (approved_pids)
  local buf = vim.api.nvim_get_current_buf()
  local view_id = vim.b[buf].skg_view_id
  if not view_id then error('Not in a skg view', 0) end
  if vim.bo[buf].modified then
    error('Save or revert this buffer first: changing its perspective'
          .. ' redraws it, which would lose its unsaved edits',
          0) end
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'set perspective'),
    sexpr.pair(sexpr.symbol('view-id'), view_id),
    sexpr.pair(sexpr.symbol('perspective'), 'next') }
  if approved_pids and #approved_pids > 0 then
    local approval = { sexpr.symbol('approved-overPrivateText-pids') }
    for _, pid in ipairs(approved_pids) do table.insert(approval, pid) end
    table.insert(request, approval) end
  rerender.send_streaming_request('cycle perspective', 'perspective',
    request,
    function (_payload_text, response) M.handler(response) end, nil,
    function (pids)
      if vim.api.nvim_buf_is_valid(buf) then
        vim.api.nvim_buf_call(buf, function ()
          require('skg.keymaps').call_showing_refusal(
            M.cycle_perspective, pids)
        end) end
    end)
end

---Record the perspective the server's answer RESPONSE names in its
---view's buffer, and name it; or show the server's refusal, which
---leaves the buffer's perspective as it was.
---@param response any
function M.handler (response)
  local ok, err = pcall(function ()
    local refusal = payload.field_text(response, 'content')
    if refusal then
      vim.notify('skg: ' .. refusal)
      return end
    local perspective = payload.field_text(response, 'perspective')
    local name = payload.field_text(response, 'perspective-name')
    local view_id = payload.field_text(response, 'view-id')
    local buf = view_id and buffer.find_buffer_by_view_id(view_id)
    if buf then
      buffer.set_perspective(buf, perspective, name)
      vim.cmd('redrawstatus!') end
    vim.notify('Showing ' .. tostring(name) .. '.')
  end)
  if not ok then
    vim.notify('skg: cycle_perspective: ' .. tostring(err))
    log.log('error', 'perspective', 'handler error: %s',
            tostring(err))
  end
end

return M
