-- PURPOSE: Toggle git diff mode on the server and rerender all views.
-- The Lua port of elisp/skg-request-git-diff-mode.el.

local client = require('skg.client')
local lock = require('skg.lock')
local messages = require('skg.messages')
local payload = require('skg.payload')
local rerender = require('skg.rerender')
local save = require('skg.save')
local state = require('skg.state')

local M = {}

---Toggle diff mode; the server answers with a git-diff-mode ack, then
---streams the rerender protocol. Refuses while any skg buffer has
---unsaved edits.
function M.toggle ()
  local unsaved = save.other_unsaved_skg_buffers(-1)
  if #unsaved > 0 then
    local names = {}
    for _, buf in ipairs(unsaved) do
      table.insert(names, vim.api.nvim_buf_get_name(buf)) end
    error('Cannot toggle diff mode: unsaved skg buffer(s): '
          .. table.concat(names, ', ')) end
  lock.begin_stream('diff-mode toggle')
  lock.lock_all_skg_buffers()
  state.register_response_handler('git-diff-mode',
    function (_payload_text, response)
      local content = payload.field_text(response, 'content')
      if content and content:find('\nWarning:', 1, true) then
        messages.big_nonfatal_message(
          'skg://messages/diff-mode-warnings',
          vim.split(content, '\n')[1], content)
      else
        vim.notify(content or 'toggled') end
    end, true)
  rerender.register_rerender_stream_handlers()
  state.lp_reset()
  client.send_string('((request . "git diff mode toggle"))\n')
end

return M
