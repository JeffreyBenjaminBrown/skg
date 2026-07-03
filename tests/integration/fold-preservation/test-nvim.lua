-- Integration test for skg fold preservation, nvim client.
-- The Lua mirror of test-emacs.el in this directory. It tests:
-- 1. Creating a buffer with content;
-- 2. Folding a headline;
-- 3. Saving the buffer;
-- 4. Verifying folding is preserved and point is at the right location.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30) -- the elisp test's run-at-time timeout was 30s

local buffer = require('skg.buffer')
local folds = require('skg.folds')
local save = require('skg.save')

print('=== SKG Fold Preservation Integration Test ===')

-- PHASE 1: create the buffer with content.
print('=== PHASE 1: Creating buffer with content ===')
local content = table.concat({
  '* (skg (node (source main))) a',
  '* (skg (node (source main))) b',
  '** c',
  '** d',
  '*** d1',
  '*** d2',
  '** e',
}, '\n')
local buf = buffer.open_org_buffer_from_text(
  content, 'skg://fold-preservation-test')
print('Created buffer with content')

-- Position on line 4 ('** d').
vim.api.nvim_win_set_cursor(0, { 4, 0 })
print('Positioned on line 4')

T.check(vim.api.nvim_get_current_line() == '** d',
        "confirmed on line with '** d'")

-- Fold the headline (this should hide d1 and d2). There is no
-- org-cycle in the nvim client; a fully-open buffer's first fold
-- toggle is equivalent to closing that headline's subtree fold
-- directly (see skg/folds.lua's module comment on the vim-fold model).
folds.close_fold_at(4)
print('Folded headline d')

-- Verify that d1 (line 5) is now invisible.
T.check(folds.line_invisible_p(5),
        'confirmed that d1 is invisible after folding')

-- PHASE 2: save the buffer.
print('=== PHASE 2: Saving buffer ===')
print('Calling save.request_save_buffer...')
save.request_save_buffer()
T.wait_for_response()

-- PHASE 3: verify point position.
print('=== PHASE 3: Verifying point position ===')
local cursor = vim.api.nvim_win_get_cursor(0)
T.check(cursor[1] == 4,
        string.format('point is on line 4 after save (got %d)', cursor[1]))

-- Verify we're still on '** d' (may have gained skg metadata from the
-- server).
local line_4_text = vim.api.nvim_get_current_line()
T.check(line_4_text:match('%*%* .* d') ~= nil,
        "still on line with '** d' (got: " .. line_4_text .. ')')

-- PHASE 4: test next-line behavior across a closed fold.
print('=== PHASE 4: Testing next-line behavior ===')
-- Emacs' (next-line) skips invisible (folded) lines; vim's normal 'j'
-- does the same over a closed fold, landing on the next visible line.
vim.cmd('normal! j')
local cursor_2 = vim.api.nvim_win_get_cursor(0)
T.check(cursor_2[1] == 7,
        string.format('point is on line 7 after next-line (got %d)',
                      cursor_2[1]))

local line_7_text = vim.api.nvim_get_current_line()
T.check(line_7_text:match('%*%* .* e') ~= nil,
        "now on line with '** e' (got: " .. line_7_text .. ')')

T.pass('PASS: Integration test successful!')
