-- Integration test for skg empty-content save, nvim client.
-- The Lua mirror of test-emacs.el in this directory:
-- open an empty skg buffer, enter '* (skg (node (id 1) (source
-- main))) 1', save, and verify the result lands in data/skg/1.skg.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20)

print('Starting integration test...')

-- The elisp test opens the buffer via skg-open-empty-content-view,
-- which is itself just (skg-open-org-buffer-from-text nil "" "*skg-empty*")
-- ("Retaining for tests. Not user-facing; dominated by
-- skg-view-new-empty", per elisp/skg-buffer.el). The nvim client has
-- no such test-only wrapper (skg.view_new_empty prompts interactively
-- for an owned source, which this test does not want), so call the
-- underlying buffer constructor directly -- the exact port target.
print('open-empty-buffer')
local buffer = require('skg.buffer')
local content_buffer = buffer.open_org_buffer_from_text('', 'skg://skg-empty')
if not content_buffer or not vim.api.nvim_buf_is_valid(content_buffer) then
  T.fail('skg content buffer was not created')
end

vim.api.nvim_set_current_buf(content_buffer)
vim.api.nvim_buf_set_lines(content_buffer, 0, -1, false,
  { '* (skg (node (id 1) (source main))) 1' })
vim.api.nvim_win_set_cursor(0, { 1, 0 })

print('save-buffer')
require('skg.save').request_save_buffer()

-- Wait for the file to be written by the server (mirrors the elisp
-- test's manual polling loop: 40 attempts * 0.25s = 10s).
local target_file = 'data/skg/1.skg'
local found = T.wait_for(function ()
  return vim.fn.filereadable(target_file) == 1
end, 10)
if not found then
  T.fail('1.skg was not created')
end

-- Allow a short moment for the file contents to settle.
vim.wait(100)

print('verify-file')
local file_contents = table.concat(vim.fn.readfile(target_file), '\n')
if not file_contents:find("title:%s*['\"]1['\"]") then
  print('File contents:\n' .. file_contents)
  T.fail('title field does not match expected value')
end
if not file_contents:find("pid:%s*['\"]1['\"]") then
  print('File contents:\n' .. file_contents)
  T.fail('pid field does not contain "1"')
end

T.pass('PASS: Empty buffer save produced 1.skg with expected content')
