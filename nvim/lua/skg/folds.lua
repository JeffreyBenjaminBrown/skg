-- PURPOSE: Read, act on, and edit 'folded' / 'bodyFolded' metadata,
-- plus the fold model skg buffers use. The Lua port of
-- elisp/skg-org-fold.el.
--
-- THE FOLD MODEL. org-fold does not exist here; skg buffers use vim
-- folds with a buffer-local foldexpr: a headline of level n starts a
-- fold at level n, and a body (the non-headline lines directly under
-- a headline) starts its own nested fold at level n+1. Closing a
-- headline's fold hides body and children (org's subtree fold);
-- closing just the body's fold hides only the body (org's entry
-- fold, what 'bodyFolded' records). Three consequences worth naming:
-- - a closed vim fold always displays a one-line placeholder, where
--   org hid the text entirely -- cosmetic, recorded as a deviation;
-- - vim cannot CLOSE a single-line fold, so a one-line body cannot be
--   body-folded here (a 'bodyFolded' marker on one degrades
--   gracefully: the body stays visible and the marker is not
--   re-recorded at the next save) -- recorded as a deviation;
-- - fold state is per-WINDOW in vim, so the marker readers/writers
--   here operate on the current window, which is where saves happen.
--
-- The marker PROTOCOL is unchanged from the Emacs client: 'folded' on
-- each headline hidden inside a folded ancestor, 'bodyFolded' on each
-- visible headline whose body is hidden; both round-trip through the
-- server, are acted on after each redraw, and are stripped from the
-- buffer the user edits.

local compare = require('skg.sexpr.compare')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

-- ── the fold expression ────────────────────────────────────────────

---The foldexpr for skg view buffers. Referenced by name from
---set_up_window, evaluated per line by vim.
---@param lnum integer
---@return string
function M.foldexpr (lnum)
  local line = vim.fn.getline(lnum)
  local stars = line:match('^(%*+)%s')
  if stars then return '>' .. #stars end
  local previous = vim.fn.getline(lnum - 1)
  local previous_stars = previous:match('^(%*+)%s')
  if previous_stars then
    -- The first body line after a headline opens the body's own fold,
    -- one level deeper than the headline's.
    return '>' .. (#previous_stars + 1) end
  return '='
end

---Give WINDOW (default current) the skg fold model for its buffer.
---@param window integer|nil
function M.set_up_window (window)
  window = window or vim.api.nvim_get_current_win()
  vim.wo[window][0].foldmethod = 'expr'
  vim.wo[window][0].foldexpr =
    'v:lua.require\'skg.folds\'.foldexpr(v:lnum)'
  vim.wo[window][0].foldlevel = 99
  vim.wo[window][0].foldenable = true
end

---Unfold everything (the stand-in for org-show-all).
function M.show_all ()
  vim.cmd('silent! %foldopen!')
end

---@param lnum integer
---@return boolean is the line hidden inside a closed fold?
---(A closed fold's own first line still displays, so it does not
---count as hidden -- matching org, where a folded subtree's heading
---stays visible.)
function M.line_invisible_p (lnum)
  local fold_start = vim.fn.foldclosed(lnum)
  return fold_start ~= -1 and fold_start ~= lnum
end

---Is the headline at LNUM visible with its body hidden by an entry
---(body) fold rather than by a subtree fold above it? True when the
---first body line is inside a closed fold while the next headline (if
---any) is visible.
---@param lnum integer
---@return boolean
function M.headline_body_hidden_p (lnum)
  local next_line = lnum + 1
  if next_line > vim.api.nvim_buf_line_count(0) then return false end
  if metadata.at_heading_p(next_line) then return false end
  if vim.fn.foldclosed(next_line) == -1 then return false end
  local next_heading = metadata.next_heading_line(lnum)
  return next_heading == nil
         or not M.line_invisible_p(next_heading)
end

-- ── marker predicates ──────────────────────────────────────────────

---@param line_number integer
---@param marker string 'folded' or 'bodyFolded'
---@return boolean
function M.headline_has_bare_marker_p (line_number, marker)
  if not metadata.at_heading_p(line_number) then return false end
  local sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  return sexp ~= nil
         and compare.subtree_p(sexp, { sexpr.symbol('skg'),
                                       sexpr.symbol(marker) })
end

---@param line_number integer
---@return boolean
function M.headline_has_folded_p (line_number)
  return M.headline_has_bare_marker_p(line_number, 'folded')
end

---@param line_number integer
---@return boolean
function M.headline_has_bodyfolded_p (line_number)
  return M.headline_has_bare_marker_p(line_number, 'bodyFolded')
end

-- ── serialize: fold state -> markers ───────────────────────────────

---Annotate the headline at each fold-relevant position: each
---invisible headline gains '(skg folded)'; each visible headline
---whose body is hidden gains '(skg bodyFolded)'. Mutually exclusive:
---a hidden headline's body state is already covered by the ancestor's
---subtree fold.
function M.add_folded_markers ()
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_heading_p(line) then
      if M.line_invisible_p(line) then
        metadata.edit_metadata_at_line(line,
                                       sexpr.read('(skg folded)'))
      elseif M.headline_body_hidden_p(line) then
        metadata.edit_metadata_at_line(line,
                                       sexpr.read('(skg bodyFolded)'))
      end
    end
  end
end

-- ── restore: markers -> fold state ─────────────────────────────────

---Restore the user's fold state from the markers: unfold everything,
---hide every bodyFolded entry, then iteratively fold the parent of
---each still-visible folded-marked headline (looping because folding
---one parent hides more markers).
function M.fold_marked_headlines ()
  M.show_all()
  M.fold_bodies_of_bodyfolded_headlines()
  while true do
    local found = nil
    local last = vim.api.nvim_buf_line_count(0)
    for line = 1, last do
      if metadata.at_heading_p(line)
         and not M.line_invisible_p(line)
         and M.headline_has_folded_p(line) then
        found = line
        break end
    end
    if not found then return end
    local level = metadata.outline_level(found)
    local parent = metadata.parent_heading_line(found, level)
    if not parent then return end
    M.close_fold_at(parent)
  end
end

---Hide the body of every headline tagged bodyFolded. Assumes the
---buffer is fully shown.
function M.fold_bodies_of_bodyfolded_headlines ()
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_heading_p(line)
       and M.headline_has_bodyfolded_p(line) then
      M.hide_entry(line)
    end
  end
end

---Close the body fold of the headline at LNUM, if it has a body.
---(The stand-in for org-fold-hide-entry.)
---@param lnum integer
function M.hide_entry (lnum)
  local body_line = lnum + 1
  if body_line <= vim.api.nvim_buf_line_count(0)
     and not metadata.at_heading_p(body_line) then
    M.close_fold_at(body_line)
  end
end

---Close the innermost fold at LNUM (the subtree fold when LNUM is a
---headline, the body fold when it is a first body line).
---@param lnum integer
function M.close_fold_at (lnum)
  pcall(vim.cmd, string.format('%dfoldclose', lnum))
end

-- ── strip markers, re-applying the folds they encode ───────────────

---Remove all 'folded'/'bodyFolded' markers, re-folding what they
---described: collect the fold positions, unfold all (so edits behave),
---delete the markers, then re-hide the bodyFolded entries and re-fold
---the parents of folded headlines. (Positions are line numbers; the
---marker edits stay within their lines, so no marker objects are
---needed where elisp needed them.)
function M.remove_folded_markers ()
  local parent_lines = {}
  local parent_seen = {}
  local bodyfolded_lines = {}
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_heading_p(line) then
      if M.headline_has_folded_p(line) then
        local level = metadata.outline_level(line)
        local parent = metadata.parent_heading_line(line, level)
        if parent and not parent_seen[parent] then
          parent_seen[parent] = true
          table.insert(parent_lines, parent) end
      end
      if M.headline_has_bodyfolded_p(line) then
        table.insert(bodyfolded_lines, line) end
    end
  end
  M.show_all()
  for line = 1, last do
    if metadata.at_heading_p(line) then
      if M.headline_has_folded_p(line) then
        metadata.edit_metadata_at_line(line,
          sexpr.read('(skg (DELETE folded))')) end
      if M.headline_has_bodyfolded_p(line) then
        metadata.edit_metadata_at_line(line,
          sexpr.read('(skg (DELETE bodyFolded))')) end
    end
  end
  for _, line in ipairs(bodyfolded_lines) do
    if metadata.at_heading_p(line) then M.hide_entry(line) end
  end
  for _, line in ipairs(parent_lines) do
    if metadata.at_heading_p(line) then M.close_fold_at(line) end
  end
end

return M
