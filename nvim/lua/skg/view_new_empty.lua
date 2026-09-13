-- PURPOSE: Open a blank skg content view for creating new nodes. The
-- Lua port of elisp/skg-view-new-empty.el.

local buffer = require('skg.buffer')

local M = {}

local function unused_buffer_name ()
  local base = 'skg://empty'
  local occupied = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      occupied[vim.api.nvim_buf_get_name(buf)] = true end
  end
  if not occupied[base] then return base end
  local suffix = 2
  while occupied[string.format('%s<%d>', base, suffix)] do
    suffix = suffix + 1 end
  return string.format('%s<%d>', base, suffix)
end

function M.view_new_empty ()
  buffer.open_org_buffer_from_text('', unused_buffer_name())
end

return M
