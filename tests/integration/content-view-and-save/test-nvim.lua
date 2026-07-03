-- Integration test for skg content view and save, nvim client.
-- The Lua mirror of test-emacs.el in this directory:
-- 1. request a single-root content view of node "1";
-- 2. add content and save;
-- 3. verify UUID generation and content persistence.
-- File system backup/cleanup is handled by run-test.sh.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20)

print('=== SKG Content View and Save Integration Test ===')

-- PHASE 1: the content view.
print("=== PHASE 1: Requesting content view for node '1' ===")
require('skg.content_view')
  .request_single_root_content_view_from_id('1')
local view = T.wait_for_buffer('skg://1')
T.check(view, "a view buffer named after node 1's title opened")

local content = T.buffer_text(view)
print('Content: ' .. content)
-- The exact expected text, like the emacs test: the server's content
-- ends with a newline, which survives as a final empty buffer line.
T.check(content ==
        '* (skg (node (id 1) (source main) (parentIs absent))) 1\n',
        'buffer content exactly matches expected')

-- PHASE 2: save.
print('=== PHASE 2: Testing save buffer ===')
vim.api.nvim_set_current_buf(view)
T.write_file('fetch.log', content)

vim.api.nvim_buf_set_lines(view, -1, -1, false, { '** 2' })
print('Added new content line: ** 2')
T.write_file('edited-unsaved.log', T.buffer_text(view))

print('Calling save.request_save_buffer...')
vim.api.nvim_win_set_cursor(0, { 1, 0 })
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'save response arrived')

local updated = T.buffer_text(view)
T.write_file('saved-rebuilt.log', updated)
print('Updated buffer content: ' .. updated)

T.check(updated:find('%* %(skg %(node %(id 1%) %(source main%)'),
        'the original node survived the save')
local new_uuid =
  updated:match('%*%* %(skg %(node %(id ([^%)]+)%) %(source main%)'
                .. '.-%)%) 2')
T.check(new_uuid, 'the new node gained a server-assigned id')
print('Extracted new UUID: ' .. tostring(new_uuid))

T.pass('PASS: Integration test successful!')
