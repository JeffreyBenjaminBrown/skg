-- Integration test: search enrichment (background containerward paths +
-- graphnodestats), nvim client. The Lua mirror of test-emacs.el in this
-- directory.
--
-- Searches for "bravo" (matches leaf-b).
-- Phase 1: search results appear immediately (no paths).
-- Phase 2: after background enrichment, the buffer should contain
-- the containerward path from leaf-b to container-a
-- (i.e. "container alpha" appears in the buffer).
-- Phase 3: the enriched leaf-b line carries node relationship stats.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45)

local search = require('skg.search')
local buffer = require('skg.buffer')

---The full text of BUF's line beginning "* (skg (node (id leaf-b)", or
---nil. The Lua mirror of the elisp search-root-line regex, done
---line-by-line since Lua patterns have no multiline ^/$.
---@param buf integer
---@return string|nil
local function search_root_line (buf)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find('^%* %(skg %(node %(id leaf%-b%)') then
      return line
    end
  end
  return nil
end

---Assert BUF's leaf-b search-root line exists and carries none of the
---special-origin birthHeralds (independent / content / linksToParent):
---a plain search result should just be a content-type root.
---@param buf integer
---@param phase string for failure messages
local function assert_leaf_b_root_is_content (buf, phase)
  local line = search_root_line(buf)
  T.check(line ~= nil,
    'leaf-b search root found during ' .. phase)
  T.check(not line:find('(parentIs independent)', 1, true),
    'leaf-b search root is not independent during ' .. phase)
  T.check(not line:find(
            '(parentIs independent) indef (birthHerald "Ca")', 1, true),
    'leaf-b search root is not content-birthed during ' .. phase)
  T.check(not line:find(
            '(parentIs independent) indef (birthHerald "La")', 1, true),
    'leaf-b search root is not linksToParent-birthed during ' .. phase)
end

print('=== SKG Search Enrichment Integration Test ===')

-- PHASE 1: search and verify results appear.
print("=== PHASE 1: search for 'bravo' ===")
search.request_text_search('bravo', false, false, false)

local buf = T.wait_for_buffer(buffer.search_buffer_name('bravo'))
T.check(buf ~= nil, 'search buffer never created')
assert_leaf_b_root_is_content(buf, 'immediate search results')
print('search buffer created')

-- PHASE 2: wait for enrichment to add "container alpha".
print('=== PHASE 2: waiting for containerward path enrichment ===')
local found = T.wait_for(function ()
  return T.buffer_text(buf):find('container alpha', 1, true) ~= nil
end, 30)

local content = T.buffer_text(buf)
if not found then
  T.fail(string.format(
    "containerward path 'container alpha' not found after 30s. "
    .. 'Buffer content:\n%s', content))
end
assert_leaf_b_root_is_content(buf, 'enriched search results')
print('containerward path enrichment arrived')

-- PHASE 3: verify node relationship stats in enriched results. leaf-b
-- is a root with 1 content, so its line should include (rels "1C").
content = T.buffer_text(buf)
if content:find('(rels "1C")', 1, true) then
  print('node relationship stats present in enriched search results')
  print('Buffer content:\n' .. content)
  T.pass('PASS: Integration test successful!')
else
  T.fail('node relationship stats not found in enriched search results.'
        .. ' Buffer content:\n' .. content)
end
