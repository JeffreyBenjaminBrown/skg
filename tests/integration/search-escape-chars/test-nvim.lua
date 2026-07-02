-- Integration test: literal search of titles with Tantivy operator
-- chars, nvim client. The Lua mirror of test-emacs.el in this
-- directory.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20)

local search = require('skg.search')
local buffer = require('skg.buffer')

---Run QUERY and verify the search buffer contains (id EXPECTED_ID).
---@param query string
---@param expected_id string
local function test_one_escape_query (query, expected_id)
  print(string.format('=== Searching literal: %s ===', query))
  search.request_text_search(query, false, false, false)
  local search_buf = T.wait_for_buffer(buffer.search_buffer_name(query))
  T.check(search_buf ~= nil,
    string.format('search buffer created for query %q', query))
  local content = T.buffer_text(search_buf)
  local needle = string.format('(id %s)', expected_id)
  if content:find(needle, 1, true) then
    print(string.format('PASS: found %s for query %q', needle, query))
  else
    T.fail(string.format(
      'expected %s not found for query %q. Content: %s',
      needle, query, content))
  end
end

print('=== SKG Search Escape-Chars Integration Test ===')
test_one_escape_query('C++ tips',     'c_plus')
test_one_escape_query('[draft] plan', 'brackets')
test_one_escape_query('cat:dog',      'colons')

T.pass('PASS: all escape-char searches succeeded')
