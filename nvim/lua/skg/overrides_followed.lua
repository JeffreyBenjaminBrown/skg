-- PURPOSE: Switch whose overrides the current view follows: its root
-- authors' (a fresh view's), or yours, so that every node you
-- override shows your override, even under a root another wrote. The
-- server switches the view and redraws it; the answer names the view
-- and the choice, and a rerender stream follows, carrying the redrawn
-- buffer, which keeps the cursor and folds as any redraw the user did
-- not ask for does. The Lua port of
-- elisp/skg-request-overrides-followed.el.
--
-- DATA USED/ASSUMED: See docs/api-and-formats.org, "Set overrides
-- followed". The buffer keeps the choice in
-- 'vim.b.skg_overrides_followed' (nil, or "user"), which each save
-- sends ('save.save_request_sexp') and a fresh view clears
-- ('buffer.open_org_buffer_from_text').

local buffer = require('skg.buffer')
local log = require('skg.log')
local payload = require('skg.payload')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Switch whose overrides this view follows, and redraw it.
---A fresh view follows its root authors' overrides: under a root of
---yours, yours; under another's root, that author's. Switched, it
---follows yours in every tree, so each node you override shows your
---override; switched again, it follows its root authors' again.
---'buffer.overrides_followed_mode_line' marks a view that follows
---yours, and a save keeps the choice.
---Refuses while the buffer has unsaved edits, which the redraw would
---replace. APPROVED_PIDS approve the text a privacy challenge asked
---about.
---@param approved_pids string[]|nil
function M.switch_overrides_followed (approved_pids)
  local buf = vim.api.nvim_get_current_buf()
  local view_id = vim.b[buf].skg_view_id
  if not view_id then error('Not in a skg view', 0) end
  if vim.bo[buf].modified then
    error('Save or revert this buffer first: switching whose overrides'
          .. ' it follows redraws it, which would lose its unsaved edits',
          0) end
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'set overrides followed'),
    sexpr.pair(sexpr.symbol('view-id'), view_id),
    sexpr.pair(sexpr.symbol('overrides-followed'),
               vim.b[buf].skg_overrides_followed == 'user'
                 and 'root authors' or 'user') }
  if approved_pids and #approved_pids > 0 then
    local approval = { sexpr.symbol('approved-overPrivateText-pids') }
    for _, pid in ipairs(approved_pids) do table.insert(approval, pid) end
    table.insert(request, approval) end
  rerender.send_streaming_request('overrides followed', 'overrides-followed',
    request,
    function (_payload_text, response) M.handler(response) end, nil,
    function (pids)
      if vim.api.nvim_buf_is_valid(buf) then
        vim.api.nvim_buf_call(buf, function ()
          require('skg.keymaps').call_showing_refusal(
            M.switch_overrides_followed, pids)
        end) end
    end)
end

---Record the choice the server's answer RESPONSE names in its view's
---buffer, and say what it now shows; or show the server's refusal,
---which leaves the buffer's choice as it was.
---@param response any
function M.handler (response)
  local ok, err = pcall(function ()
    local refusal = payload.field_text(response, 'content')
    if refusal then
      vim.notify('skg: ' .. refusal)
      return end
    local followed = payload.field_text(response, 'overrides-followed')
    local view_id = payload.field_text(response, 'view-id')
    local buf = view_id and buffer.find_buffer_by_view_id(view_id)
    if buf then
      vim.b[buf].skg_overrides_followed =
        followed == 'user' and 'user' or nil
      vim.cmd('redrawstatus!') end
    vim.notify(followed == 'user'
      and 'This buffer now shows your overrides wherever you have one.'
          .. ' <localleader>vp switches back.'
      or "This buffer now shows the overrides of each tree's root"
          .. ' author, as a fresh view does.')
  end)
  if not ok then
    vim.notify('skg: switch_overrides_followed: ' .. tostring(err))
    log.log('error', 'overrides-followed', 'handler error: %s',
            tostring(err))
  end
end

return M
