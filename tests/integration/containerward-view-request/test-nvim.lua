-- Integration test for containerward-view request functionality, nvim
-- client. The Lua mirror of test-emacs.el in this directory. It
-- tests:
-- 1. Creating initial buffer structure and saving to establish
--    relationships;
-- 2. Replacing the buffer with new content that has no root node;
-- 3. Adding a containerward-view request to node 12;
-- 4. Verifying containerward path [1, 0] is integrated under node 12.
--
-- NOTE: file system operations (backup/cleanup) are handled by
-- run-test.sh.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30) -- the elisp test's run-at-time timeout was 30s

local buffer = require('skg.buffer')
local save = require('skg.save')
local view_requests = require('skg.view_requests')

---ORG_TEXT with each headline reduced to 'STARS TITLE' (TITLE is the
---line's final whitespace-delimited token, all skg metadata dropped).
---Mirrors strip-all-metadata.
---@param text string
---@return string
local function strip_all_metadata (text)
  local result = {}
  for _, line in ipairs(vim.split(text, '\n')) do
    local stars = line:match('^(%*+) ')
    local title = line:match('(%S+)$')
    if stars and title then
      table.insert(result, stars .. ' ' .. title .. '\n')
    end
  end
  return table.concat(result)
end

print('=== SKG Containerward View Request Integration Test ===')

-- PHASE 1: establish relationships on disk. Both phases below reuse
-- the SAME buffer and view uri, mirroring the elisp test's single
-- *skg-content-view* buffer whose skg-view-uri never changes.
print('=== PHASE 1: Establishing relationships on disk ===')
local view_uri = buffer.generate_uuid()
local buffer_name = 'skg://containerward-view-request-test'
local initial_content = table.concat({
  '* (skg (node (id 0) (source main))) 0',
  '** (skg (node (id 1))) 1',
  '*** (skg (node (id 11))) 11',
  '*** (skg (node (id 12))) 12',
  '**** (skg (node (id 121))) 121',
  '*** (skg (node (id 13))) 13',
}, '\n')
local buf = buffer.open_org_buffer_from_text(
  initial_content, buffer_name, view_uri)
print('Created initial buffer with full structure')

print('Saving initial buffer to establish relationships...')
save.request_save_buffer()
T.wait_for_response()
print('Relationships established on disk')

-- PHASE 2: replace the buffer with a structure that omits node 0.
print('=== PHASE 2: Creating new buffer without node 0 ===')
local new_content = table.concat({
  '* (skg (node (id 1) (source main))) 1',
  '** (skg (node (id 11))) 11',
  '** (skg (node (id 12))) 12',
  '*** (skg (node (id 121))) 121',
  '** (skg (node (id 13))) 13',
}, '\n')
buf = buffer.open_org_buffer_from_text(new_content, buffer_name, view_uri)
print('Created new buffer without node 0')

-- Position on line 3 ('** (skg (node (id 12))) 12').
vim.api.nvim_win_set_cursor(0, { 3, 0 })
print('Positioned on node 12 (line 3)')

-- PHASE 3: request the containerward view for node 12.
print('=== PHASE 3: Requesting containerward view for node 12 ===')
print('Calling view_requests.show_paths_through_containers...')
view_requests.show_paths_through_containers() -- this also saves the buffer
T.wait_for_response()

-- PHASE 4: verify the result structure.
print('=== PHASE 4: Verifying result structure ===')
local buffer_content = T.buffer_text(buf)
local stripped_buffer_content = strip_all_metadata(buffer_content)
local expected_without_metadata =
  '* 1\n'
  .. '** 11\n'
  .. '** 12\n'  -- backpath was requested here
  .. '*** 1\n'  -- its first element
  .. '**** 0\n' -- its second element
  .. '*** 121\n'
  .. '** 13\n'
local expected =
  '* (skg (node (id 1) (source main) (parentIs absent) (rels (contains (in 1) (out 3))))) 1\n'
  .. '** (skg (node (id 11) (source main) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 11\n'
  .. '** (skg (node (id 12) (source main) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth contains)))) 12\n'
  .. '*** (skg (node (id 1) (source main) (parentIs independent) indef'
     .. ' (rels (contains (in 1) (out 3 (ancestors 1))) (birth contains)) (viewStats cycle))) 1\n'
  .. '**** (skg (node (id 0) (source main) (parentIs independent) indef'
     .. ' (rels (contains (out 1 (ancestors 1))) (birth contains)))) 0\n'
  .. '*** (skg (node (id 121) (source main) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 121\n'
  .. '** (skg (node (id 13) (source main) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 13\n'

print('Buffer-Content with metadata: ' .. buffer_content)
print('Expected buffer-content with metadata: ' .. expected)
print('Stripped buffer-content: ' .. stripped_buffer_content)
print('Expected buffer-content without metadata: '
      .. expected_without_metadata)

T.check(buffer_content == expected,
        'buffer-content matches expected (with metadata)')
T.check(stripped_buffer_content == expected_without_metadata,
        'stripped buffer-content matches expected structure; '
        .. 'containerward path [1, 0] was correctly integrated '
        .. 'under node 12')

T.pass('PASS: Integration test successful!')
