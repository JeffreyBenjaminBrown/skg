-- PURPOSE: Ask the server for a shell script that stages every
-- detected node "move" (a .skg file that vanished from one source's
-- git repo and appeared in another) and display it for review. The
-- Lua port of elisp/skg-request-stage-moves.el.

local client = require('skg.client')
local payload = require('skg.payload')
local state = require('skg.state')

local M = {}

function M.stage_moves ()
  state.register_response_handler('stage-moves',
    function (_payload_text, response)
      M.stage_moves_handler(response)
    end, true)
  state.lp_reset()
  client.send_string('((request . "stage moves"))\n')
end

---@param response any
function M.stage_moves_handler (response)
  local content = payload.field_text(response, 'content')
                  or '# stage moves failed\n# Empty response\n'
  local errors = payload.string_list(payload.field(response, 'errors'))
  local name = 'skg://stage-moves'
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(existing) == name then
      buf = existing break end
  end
  if not buf then
    buf = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(buf, name)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(content, '\n'))
  vim.bo[buf].filetype = 'sh'
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  if #errors > 0 then
    vim.notify('Stage-moves completed with errors --'
               .. ' see skg://stage-moves')
  else
    vim.notify('Stage-moves: review skg://stage-moves, then run it'
               .. ' from the data root')
  end
end

return M
