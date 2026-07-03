-- Integration test for skg save error handling, nvim client.
-- The Lua mirror of test-emacs.el in this directory:
-- 1. Invalid save (duplicate ID without indefinitive) should show an
--    error buffer.
-- 2. Valid save (with indefinitive) should work normally, and must
--    not leave lock/stream state that wedges the next save.
--
-- NOTE: File system operations (backup/cleanup) are handled by
-- run-test.sh.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local buffer = require('skg.buffer')
local save = require('skg.save')
local lock = require('skg.lock')

local content_buffer_name = 'skg://skg-content-view'
local original_content =
  '* (skg (node (id 1) (source main))) 1\n'
  .. '** (skg (node (id 1))) 1'

local function test_invalid_save ()
  print('=== PHASE 1: Testing invalid save (duplicate ID without'
       .. ' indefinitive) ===')

  local view_uri = buffer.generate_uuid()
  local content_buf = buffer.open_org_buffer_from_text(
    original_content, content_buffer_name, view_uri)
  print('created ' .. content_buffer_name .. ' buffer with invalid'
       .. ' content')

  vim.api.nvim_set_current_buf(content_buf)

  print('Calling save.request_save_buffer with invalid content...')
  save.request_save_buffer()

  T.check(T.wait_for_response(), 'response received for invalid save')

  local error_buf = T.wait_for_buffer('skg://messages/save-errors', 5)
  if not error_buf then
    T.fail('No error buffer was created')
  end
  print('Error buffer was created')

  local error_content = T.buffer_text(error_buf)
  print('Error buffer content: ' .. error_content)
  T.check(error_content:find('NOTHING WAS SAVED', 1, true) ~= nil,
          'Error buffer contains expected error message')

  -- Buffer has no lingering focused/folded markers: focus and fold
  -- information is sent to the server but not shown to the user.
  if not vim.api.nvim_buf_is_valid(content_buf) then
    T.fail('Original ' .. content_buffer_name .. ' buffer was lost')
  end
  local content = T.buffer_text(content_buf)
  T.check(content == original_content,
          'Buffer content unchanged (no lingering markers)')
  if content ~= original_content then
    print('Expected: ' .. original_content)
    print('Got: ' .. content)
  end

  return content_buf
end

local function test_valid_save (content_buf)
  print('=== PHASE 2: Testing valid save (with indefinitive) ===')

  -- Fix up the previously-invalid child line: add 'indef'. Mirrors
  -- the elisp's search-forward + replace-match on the exact text.
  local old_child_line = '** (skg (node (id 1))) 1'
  local new_child_line = '** (skg (node (id 1) indef)) 1'
  local lines = vim.api.nvim_buf_get_lines(content_buf, 0, -1, false)
  local found_line = nil
  for i, line in ipairs(lines) do
    if line:find(old_child_line, 1, true) then
      found_line = i
      break
    end
  end
  if not found_line then
    T.fail('Could not find "' .. old_child_line .. '" line to amend')
  end
  vim.api.nvim_buf_set_lines(content_buf, found_line - 1, found_line,
                             false, { new_child_line })
  print('Amended previously invalid content to use indefinitive, so'
       .. ' it is now valid')

  vim.api.nvim_set_current_buf(content_buf)

  print('Calling save.request_save_buffer with valid content...')
  save.request_save_buffer()

  T.check(T.wait_for_response(), 'response received for valid save')

  local updated_content = T.buffer_text(content_buf)
  print('Updated buffer content: ' .. updated_content)

  -- Should contain cycle and indef markers (inside (node ...)).
  local has_cycle = updated_content:find('cycle', 1, true) ~= nil
  local has_indef = updated_content:find('%f[%w]indef%f[%W]') ~= nil
  if has_cycle and has_indef then
    print('Valid save worked and showed cycle indef')
  else
    print('Expected to contain: "cycle" and "indef"')
    print('Got: ' .. updated_content)
    T.fail('Expected cycle and indef markers not found')
  end

  -- §20.2(c): an invalid save must NOT leak lock/stream state that
  -- wedges the NEXT save. Reaching here already proves the second
  -- save's wait RETURNED -- a wedged pending-count would have hung
  -- until the outer timeout fired. Assert explicitly that no stream
  -- guard or lock overlay lingers after the invalid-then-valid
  -- sequence.
  if lock.stream_in_progress then
    T.fail(string.format(
      '[section 20.2c] lock.stream_in_progress still set after valid'
      .. ' save: %s', tostring(lock.stream_in_progress)))
  end
  if vim.b[content_buf].skg_save_locked then
    T.fail('[section 20.2c] content buffer left save-locked after'
          .. ' the valid save')
  end
  print('PASS [section 20.2c]: invalid-then-valid save did not wedge'
       .. ' the second save (no leaked lock/stream state)')
end

print('=== SKG Valid and Invalid Saves Integration Test ===')
local content_buf = test_invalid_save()
test_valid_save(content_buf)

T.pass('PASS: Integration test successful!')
