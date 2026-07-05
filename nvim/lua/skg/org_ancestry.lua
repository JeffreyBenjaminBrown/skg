-- PURPOSE: Show, in a new buffer, only the outline ancestry of the
-- headline at point (parent, grandparent, ... to the root, plus the
-- headline itself). Not skg-specific; works in any org buffer. The
-- Lua port of elisp/skg-view-org-ancestry.el.

local focus = require('skg.focus')
local metadata = require('skg.metadata')

local M = {}

function M.view_org_ancestry ()
  if vim.bo.filetype ~= 'org' then
    error('Not in an org-mode buffer') end
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  local ancestry = {}
  local current = line
  while current do
    table.insert(ancestry, 1, metadata.line_text(current))
    local level = metadata.outline_level(current)
    current = metadata.parent_heading_line(current, level)
  end
  local title = metadata.split_as_stars_metadata_title(
    metadata.line_text(line)).title
  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_buf_set_name(buf, 'skg://ancestry/' .. title)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, ancestry)
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
end

return M
