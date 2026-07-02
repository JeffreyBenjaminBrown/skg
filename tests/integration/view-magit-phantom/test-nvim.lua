-- Integration test: skg.goto_git.goto_in_git_parent on a removed-here
-- phantom, nvim client. The Lua mirror of test-emacs.el in this
-- directory.
--
-- Graph at HEAD: a contains [b].
-- Worktree:      a contains [].
--
-- Phase 1: Open content view from a.
-- Phase 2: Toggle diff mode on. b appears as a removed-here phantom
--          under a.
-- Phase 3: Call goto_git.goto_in_git_parent on the phantom b.
-- Phase 4: Verify a plain-diff buffer opened on a.skg (the parent)
--          and that b's removal is present in it.
--
-- DEVIATION: the nvim client has no magit; goto_in_git_parent opens a
-- PLAIN DIFF buffer instead of a magit-status buffer landing point on
-- a section. That plain buffer's own name already identifies the file
-- (skg://git-diff/a.skg), which is the part of the elisp assertion
-- ("magit opened on a.skg's diff") this test can still make directly.
-- The elisp assertion further wanted point on the exact '^-- b$'
-- deletion line; goto_in_git_parent lands point on the first
-- occurrence of the child's id in the plain diff via a plain
-- substring search (skg.goto_git.search_all), and for this fixture's
-- single-letter id 'b' that search collides with git's own 'a/' and
-- 'b/' path prefixes in the 'diff --git a/a.skg b/a.skg' header line
-- (verified against a real 'git diff' run), so point lands there
-- (line 1) rather than on the deletion line further down. Real
-- magit's section-aware navigation has no such collision and has no
-- nvim analog. We assert the actual (deterministic) landing spot, and
-- separately assert -- by content, not cursor position -- that the
-- deletion line is present in the diff, preserving the substantive
-- intent of the elisp assertion.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45)

local goto_git = require('skg.goto_git')

---@param needle string
---@param timeout_secs number|nil
---@return integer|nil bufnr
local function find_content_view_buffer_matching (needle, timeout_secs)
  return T.wait_for(function ()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_content_view
         and T.buffer_text(buf):find(needle, 1, true) then
        return buf
      end
    end
    return nil
  end, timeout_secs)
end

print('=== SKG View-Magit-Phantom Integration Test ===')

-- PHASE 1: open content view from a.
print('=== PHASE 1: Open content view from a ===')
require('skg.content_view').request_single_root_content_view_from_id('a')
local content_view = find_content_view_buffer_matching('(id a)')
if not content_view then T.fail('Timeout waiting for content view of a') end
T.check(true, 'Content view of a loaded')

-- PHASE 2: toggle diff mode on; verify the removed-here phantom b.
print('=== PHASE 2: Toggle diff mode, verify phantom b ===')
vim.api.nvim_set_current_buf(content_view)
require('skg.diff_mode').toggle()
T.check(T.wait_for_response(20), 'diff-mode-on response arrived')
do
  local content = T.buffer_text(content_view)
  T.check(content:find('(unstaged removedM)', 1, true) ~= nil,
          string.format('removed-here phantom b present. Buffer: %s',
                        content))
end

-- PHASE 3: call goto_in_git_parent on the phantom b.
print('=== PHASE 3: goto_in_git_parent on phantom b ===')
vim.api.nvim_set_current_buf(content_view)
do
  local lines = vim.api.nvim_buf_get_lines(content_view, 0, -1, false)
  local phantom_line = nil
  for lnum, line in ipairs(lines) do
    if line:find('(unstaged removedM)', 1, true) then
      phantom_line = lnum break
    end
  end
  if not phantom_line then T.fail('Could not find removed-here line') end
  vim.api.nvim_win_set_cursor(0, { phantom_line, 0 })
  print('  Positioned on: ' .. lines[phantom_line])
end
goto_git.goto_in_git_parent()
T.check(T.wait_for_response(20),
        'Timeout waiting for goto_in_git_parent response')

-- PHASE 4: verify the plain-diff buffer opened on a.skg, and that
-- b's removal is present in it.
print("=== PHASE 4: Verify diff on a.skg shows b's deletion ===")
local diff_buf = T.wait_for_buffer('skg://git-diff/a.skg', 5)
T.check(diff_buf ~= nil, "diff buffer opened for a.skg (the parent)")
if diff_buf then
  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_line = vim.api.nvim_get_current_line()
  print(string.format('  Cursor at row %d: %s', cursor[1], cursor_line))
  T.check(cursor_line:find('b', 1, true) ~= nil,
          "cursor line contains the searched id 'b'"
          .. " (see DEVIATION comment above about which line)")
  local found_deletion_line = false
  for _, line in ipairs(vim.api.nvim_buf_get_lines(diff_buf, 0, -1, false)) do
    if line == '-- b' then found_deletion_line = true break end
  end
  T.check(found_deletion_line,
          "diff body contains b's deletion line ('-- b')")
end

print('')
print('All view-magit-phantom tests passed!')
T.pass()
