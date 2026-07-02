-- PURPOSE: Read, edit and act on (by moving the cursor to the focused
-- heading) 'focused' metadata. This is how point survives the
-- save/rerender round-trip. The Lua port of elisp/skg-focus.el.

local compare = require('skg.sexpr.compare')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Add 'focused' to the metadata of the current headline (finding the
---owning headline when the cursor is in a body). No change if already
---focused.
function M.add_focused_marker ()
  local line_number = M.owning_headline_line()
  if not line_number then return end
  metadata.edit_metadata_at_line(line_number,
                                 sexpr.read('(skg focused)'))
end

---The headline owning the cursor position: the cursor line if it is a
---headline, else the nearest headline above. The stand-in for
---org-back-to-heading.
---@return integer|nil
function M.owning_headline_line ()
  for line = metadata.current_line_number(), 1, -1 do
    if metadata.at_heading_p(line) then return line end
  end
  return nil
end

---@param line_number integer|nil
---@return boolean does that headline carry the focused marker?
function M.headline_has_focused_p (line_number)
  local sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  return sexp ~= nil
         and compare.subtree_p(sexp, { sexpr.symbol('skg'),
                                       sexpr.symbol('focused') })
end

---The 1-based line of the first focused headline, or nil.
---@return integer|nil
function M.focused_headline_line ()
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_heading_p(line)
       and metadata.line_text(line):find('focused', 1, true)
       and M.headline_has_focused_p(line) then
      return line end
  end
  return nil
end

---Move the cursor to the first focused headline, or to the beginning
---of the buffer if none is found.
function M.goto_focused_headline ()
  vim.api.nvim_win_set_cursor(0, { M.focused_headline_line() or 1, 0 })
end

---Remove 'focused' from the first focused headline, if any.
function M.remove_focused_marker ()
  local line = M.focused_headline_line()
  if line then
    metadata.edit_metadata_at_line(line,
                                   sexpr.read('(skg (DELETE focused))'))
  end
end

return M
