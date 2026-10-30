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
-- - vim cannot close a single-line fold, so a one-line body cannot be
--   body-folded here (a 'bodyFolded' atom on one degrades
--   gracefully: the body stays visible and the atom is not
--   re-recorded at the next save) -- recorded as a deviation;
-- - fold state is per-window in vim, so the atom readers/writers
--   here operate on the current window, which is where saves happen.
--
-- The atom protocol is unchanged from the Emacs client: 'folded' on
-- each headline hidden inside a folded ancestor, 'bodyFolded' on each
-- visible headline whose body is hidden; both round-trip through the
-- server, are acted on after each rerender, and are stripped from the
-- buffer the user edits.

local compare = require('skg.sexpr.compare')
local focus = require('skg.focus')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

-- ── the body toggle ────────────────────────────────────────────────

---Toggle the visibility of the body of the headline at point. If the
---headline's subtree is folded, show its body and immediate children
---without opening the children's entries. Otherwise, hide or show the
---body without changing the children's visibility. Works from the
---headline or from inside its body; from the body, the cursor moves to
---the start of the headline. A bodyless headline is left alone, with a
---message, unless its children need to be revealed. A one-line body
---cannot be hidden (vim cannot close a one-line fold), so it is left
---alone too, with a message. Mirrors skg-org-body-toggle.
function M.org_body_toggle ()
  local headline = focus.owning_headline_line()
  if not headline then error('Before the first headline', 0) end
  if headline ~= metadata.current_line_number() then
    vim.api.nvim_win_set_cursor(0, { headline, 0 }) end
  local body_state = M.body_state_at_line(headline)
  local first_child = M.first_child_line(headline)
  if first_child and M.line_invisible_p(first_child) then
    M.show_entry(headline)
    M.show_children(headline)
  elseif body_state == 'absent' then
    vim.notify('This headline has no body.')
  elseif body_state == 'hidden' then
    M.show_entry(headline)
  elseif M.body_line_count(headline) == 1 then
    vim.notify('A one-line body cannot be hidden: vim cannot close a'
               .. ' one-line fold.')
  else
    M.hide_entry(headline)
  end
end

---'absent' when the headline at LNUM has no body, 'hidden' when its
---first body line is inside a closed fold, else 'visible'.
---@param lnum integer
---@return string
function M.body_state_at_line (lnum)
  local body_line = lnum + 1
  if body_line > vim.api.nvim_buf_line_count(0)
     or metadata.at_headline_p(body_line) then
    return 'absent' end
  if vim.fn.foldclosed(body_line) ~= -1 then return 'hidden' end
  return 'visible'
end

---The number of body lines of the headline at LNUM.
---@param lnum integer
---@return integer
function M.body_line_count (lnum)
  local next_headline = metadata.next_headline_line(lnum)
                        or vim.api.nvim_buf_line_count(0) + 1
  return next_headline - lnum - 1
end

---The line of the first child of the headline at LNUM, or nil. The
---stand-in for org-goto-first-child.
---@param lnum integer
---@return integer|nil
function M.first_child_line (lnum)
  local next_headline = metadata.next_headline_line(lnum)
  if next_headline
     and metadata.outline_level(next_headline) > metadata.outline_level(lnum)
  then return next_headline end
  return nil
end

---Show the headline at LNUM and its body: open every closed fold that
---holds its first body line, the headline's own subtree fold among
---them. (Opening the subtree fold leaves the body's fold as it was.)
---The stand-in for org-fold-show-entry.
---@param lnum integer
function M.show_entry (lnum)
  local last = vim.api.nvim_buf_line_count(0)
  local line = lnum + 1 <= last and not metadata.at_headline_p(lnum + 1)
               and lnum + 1 or lnum
  for _ = 1, metadata.outline_level(lnum) + 1 do
    if vim.fn.foldclosed(line) == -1 then return end
    pcall(vim.cmd, string.format('%dfoldopen', line))
  end
end

---Show each immediate child headline of the headline at LNUM, with its
---own subtree folded. A child of one line is left open, since closing
---a one-line fold hides nothing and makes the next close close its
---parent. The stand-in for org-fold-show-children.
---@param lnum integer
function M.show_children (lnum)
  local level = metadata.outline_level(lnum)
  local child = M.first_child_line(lnum)
  while child and metadata.outline_level(child) > level do
    local after = metadata.next_headline_after_subtree(child)
    local subtree_end = (after or vim.api.nvim_buf_line_count(0) + 1) - 1
    if vim.fn.foldclosed(child) == -1 and subtree_end > child then
      M.close_fold_at(child) end
    child = after
  end
end

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

---Unfold everything (the stand-in for org-show-all). Also forces the
---fold expression to re-evaluate ('zx'): after a whole-buffer
---replacement vim recomputes expr folds lazily, and a fold that has
---not materialized yet cannot be closed.
function M.show_all ()
  vim.cmd('silent! normal! zx')
  vim.cmd('silent! %foldopen!')
end

---@param lnum integer
---@return boolean is the line hidden inside a closed fold?
---(A closed fold's own first line still displays, so it does not
---count as hidden -- matching org, where a folded subtree's headline
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
  if metadata.at_headline_p(next_line) then return false end
  if vim.fn.foldclosed(next_line) == -1 then return false end
  local next_headline = metadata.next_headline_line(lnum)
  return next_headline == nil
         or not M.line_invisible_p(next_headline)
end

-- ── atom predicates ──────────────────────────────────────────────

---@param line_number integer
---@param atom string 'folded' or 'bodyFolded'
---@return boolean
function M.headline_has_bare_atom_p (line_number, atom)
  if not metadata.at_headline_p(line_number) then return false end
  local sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  return sexp ~= nil
         and compare.subtree_p(sexp, { sexpr.symbol('skg'),
                                       sexpr.symbol(atom) })
end

---@param line_number integer
---@return boolean
function M.headline_has_folded_p (line_number)
  return M.headline_has_bare_atom_p(line_number, 'folded')
end

---@param line_number integer
---@return boolean
function M.headline_has_bodyfolded_p (line_number)
  return M.headline_has_bare_atom_p(line_number, 'bodyFolded')
end

-- ── serialize: fold state -> atoms ───────────────────────────────

---Annotate the headline at each fold-relevant position: each
---invisible headline gains '(skg folded)'; each visible headline
---whose body is hidden gains '(skg bodyFolded)'. Mutually exclusive:
---a hidden headline's body state is already covered by the ancestor's
---subtree fold.
function M.add_folded_metadata ()
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_headline_p(line) then
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

-- ── restore: atoms -> fold state ─────────────────────────────────

---Restore the user's fold state from the atoms: unfold everything,
---hide every bodyFolded entry, then iteratively fold the parent of
---each still-visible folded-marked headline (looping because folding
---one parent hides more atoms).
function M.fold_marked_headlines ()
  M.show_all()
  M.fold_bodies_of_bodyfolded_headlines()
  local previous_found = nil
  while true do
    local found = nil
    local last = vim.api.nvim_buf_line_count(0)
    for line = 1, last do
      if metadata.at_headline_p(line)
         and not M.line_invisible_p(line)
         and M.headline_has_folded_p(line) then
        found = line
        break end
    end
    if not found then return end
    if found == previous_found then
      -- Closing the parent did not hide this headline (it can happen
      -- when the window lacks the skg fold model); stop rather than
      -- spin.
      return end
    previous_found = found
    local level = metadata.outline_level(found)
    local parent = metadata.parent_headline_line(found, level)
    if not parent then return end
    M.close_fold_at(parent)
  end
end

---Hide the body of every headline tagged bodyFolded. Assumes the
---buffer is fully shown.
function M.fold_bodies_of_bodyfolded_headlines ()
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_headline_p(line)
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
     and not metadata.at_headline_p(body_line) then
    M.close_fold_at(body_line)
  end
end

---Close the innermost fold at LNUM (the subtree fold when LNUM is a
---headline, the body fold when it is a first body line).
---@param lnum integer
function M.close_fold_at (lnum)
  pcall(vim.cmd, string.format('%dfoldclose', lnum))
end

-- ── strip atoms, re-applying the folds they encode ───────────────

---Remove all 'folded'/'bodyFolded' atoms, re-folding what they
---described: collect the fold positions, unfold all (so edits behave),
---delete the atoms, then re-hide the bodyFolded entries and re-fold
---the parents of folded headlines. (Positions are line numbers; the
---marker edits stay within their lines, so no marker objects are
---needed where elisp needed them.)
function M.remove_folded_metadata ()
  local parent_lines = {}
  local parent_seen = {}
  local bodyfolded_lines = {}
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_headline_p(line) then
      if M.headline_has_folded_p(line) then
        local level = metadata.outline_level(line)
        local parent = metadata.parent_headline_line(line, level)
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
    if metadata.at_headline_p(line) then
      if M.headline_has_folded_p(line) then
        metadata.edit_metadata_at_line(line,
          sexpr.read('(skg (DELETE folded))')) end
      if M.headline_has_bodyfolded_p(line) then
        metadata.edit_metadata_at_line(line,
          sexpr.read('(skg (DELETE bodyFolded))')) end
    end
  end
  for _, line in ipairs(bodyfolded_lines) do
    if metadata.at_headline_p(line) then M.hide_entry(line) end
  end
  for _, line in ipairs(parent_lines) do
    if metadata.at_headline_p(line) then M.close_fold_at(line) end
  end
end

return M
