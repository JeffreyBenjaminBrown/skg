-- Integration test for focus and folded markers, nvim client. The Lua
-- mirror of test-emacs.el in this directory. It tests, in order:
-- - Creating a buffer with 6 headlines;
-- - Folding headline 3 (which contains headlines 4 and 5);
-- - Positioning point on headline 2;
-- - Saving with save.request_save_buffer;
-- - Verifying that after save, the markers are removed but the
--   folding is preserved and point is on the focused headline.
--
-- NOTE: file system operations (backup/cleanup) are handled by
-- run-test.sh.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20) -- the elisp test's run-at-time timeout was 20s

local buffer = require('skg.buffer')
local folds = require('skg.folds')
local metadata = require('skg.metadata')
local save = require('skg.save')

print('=== SKG Focus and Folded Markers Integration Test ===')

-- PHASE 1: create the test buffer.
print('=== PHASE 1: Creating test buffer ===')
local content = table.concat({
  '* (skg (node (id 1) (source main))) 1',
  '** (skg (node (id 2))) 2',
  '** (skg (node (id 3))) 3',
  '*** (skg (node (id 4))) 4',
  '*** (skg (node (id 5))) 5',
  '** (skg (node (id 6))) 6',
}, '\n')
local buf = buffer.open_org_buffer_from_text(
  content, 'skg://focus-and-folded-markers-test')
print('Created buffer with 6 headlines')

T.write_file('initial.log', T.buffer_text(buf))
print('Saved initial buffer state to initial.log')

-- Position point on headline 3 (the third headline).
local headline_3_line = nil
for line = 1, vim.api.nvim_buf_line_count(buf) do
  if metadata.line_text(line):find('(id 3)', 1, true) then
    headline_3_line = line
    break
  end
end
T.check(headline_3_line ~= nil, 'found headline 3')
vim.api.nvim_win_set_cursor(0, { headline_3_line, 0 })
print('Positioned point on headline 3')

-- Fold headline 3 (closing its subtree hides headlines 4 and 5).
folds.close_fold_at(headline_3_line)
print('Folded headline 3')

-- Move point up to headline 2.
vim.api.nvim_win_set_cursor(0, { headline_3_line - 1, 0 })
print('Moved point to headline 2')

local current_line = vim.api.nvim_get_current_line()
print('Current line: ' .. current_line)
T.check(current_line:find('(id 2)', 1, true) ~= nil, 'on headline 2')

-- Debug: check invisibility of each headline.
print('=== Checking invisibility status ===')
for line = 1, vim.api.nvim_buf_line_count(buf) do
  if metadata.at_heading_p(line) then
    print(string.format('Line: %s | Invisible: %s',
      metadata.line_text(line), tostring(folds.line_invisible_p(line))))
  end
end

-- Debug: manually test folds.add_folded_markers.
print('=== Testing folds.add_folded_markers ===')
folds.add_folded_markers()
local after_folded_markers = T.buffer_text(buf)
T.write_file('after-folded-markers.log', after_folded_markers)
print('Buffer after folds.add_folded_markers:\n' .. after_folded_markers)

-- PHASE 2: save the buffer.
print('=== PHASE 2: Saving buffer ===')
save.request_save_buffer()
T.wait_for_response()

-- Check the result.
local result_content = T.buffer_text(buf)
T.write_file('result.log', result_content)
print('Saved result buffer state to result.log')
print('Result content:\n' .. result_content)

local has_focused_marker =
  result_content:find('%f[%w]focused%f[%W]') ~= nil
local has_folded_marker =
  result_content:find('%f[%w]folded%f[%W]') ~= nil
local final_line = vim.api.nvim_get_current_line()
local line_4_invisible = folds.line_invisible_p(4)
local line_5_invisible = folds.line_invisible_p(5)

T.check(not has_focused_marker, 'focused marker removed from buffer')
T.check(not has_folded_marker, 'folded markers removed from buffer')
T.check(final_line:find('(id 2)', 1, true) ~= nil,
        'point is on headline 2 (focused headline)')
T.check(line_4_invisible, 'headline 4 is invisible (folded)')
T.check(line_5_invisible, 'headline 5 is invisible (folded)')

T.pass('PASS: Integration test successful!')
