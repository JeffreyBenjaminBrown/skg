-- PURPOSE: 'view_ancestry' (':SkgViewAncestry', '<localleader>va'):
-- show, in a new buffer, only the outline ancestry of the headline at
-- point (parent, grandparent, ... to the root, plus the headline
-- itself), with heralds if the buffer it was called from shows them.
-- The new buffer is a scratch buffer, not a view: nothing in it is
-- saved. Not skg-specific; works in any org buffer. The Lua port of
-- elisp/skg-view-ancestry.el.

local focus = require('skg.focus')
local heralds = require('skg.heralds')
local metadata = require('skg.metadata')

local M = {}

function M.view_ancestry ()
  if vim.bo.filetype ~= 'org' then
    error('Not in an org-mode buffer') end
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  local ancestry = {}
  local current = line
  while current do
    table.insert(ancestry, 1, metadata.line_text(current))
    local level = metadata.outline_level(current)
    current = metadata.parent_headline_line(current, level)
  end
  local headline = metadata.line_text(line)
  local title = metadata.split_as_stars_metadata_title(headline).title
  local metadata_sexp = metadata.metadata_on_line(headline)
  local vnid = metadata_sexp and metadata.metadata_vnid(metadata_sexp)
  local with_heralds = heralds.enabled_p(vim.api.nvim_get_current_buf())
  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_buf_set_name(buf, 'skg://ancestry/'
    .. (vnid and ('(vnid ' .. vnid .. '): ') or '') .. title)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, ancestry)
  vim.bo[buf].filetype = 'org'
  if with_heralds then heralds.enable(buf) end
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
end

return M
