-- Integration test: a search is answered in one message, nvim client.
-- The Lua mirror of test-emacs.el in this directory.
--
-- Searches for "bravo" (matches leaf-b). The search buffer, when it
-- first appears, already shows leaf-b's containers folder, folded,
-- holding container-a, and leaf-b's node relationship stats.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45)

local search = require('skg.search')
local buffer = require('skg.buffer')

---The full text of BUF's line beginning "* (skg node (gnid leaf-b)"
---once its viewnode ID is dropped, or nil. The Lua mirror of the elisp
---search-root-line regex, done line-by-line since Lua patterns have no
---multiline ^/$.
---@param buf integer
---@return string|nil
local function search_root_line (buf)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if T.without_viewnode_ids(line)
         :find('^%* %(skg node %(gnid leaf%-b%)') then
      return line
    end
  end
  return nil
end

---Assert BUF's leaf-b search-root line exists and carries none of the
---special-origin birth facts (false / content / linksToParent):
---a plain search result should just be a content-type root.
---@param buf integer
local function assert_leaf_b_root_is_content (buf)
  local line = search_root_line(buf)
  T.check(line ~= nil, 'leaf-b search root found')
  T.check(not line:find('noClaim', 1, true),
    'leaf-b search root is not false')
  T.check(not line:find(
            'noClaim writeProtected (view (rels (contains (out 1 (ancestors 1)))) (birth (contains out 1))', 1, true),
    'leaf-b search root is not content-birthed')
  T.check(not line:find(
            'noClaim writeProtected (view (rels (linksTo (out (ancestors 1)))) (birth (linksTo out 1))', 1, true),
    'leaf-b search root is not linksToParent-birthed')
end

print('=== SKG Search In One Message Integration Test ===')

print("=== search for 'bravo' ===")
search.request_text_search('bravo', false, false, false)

local buf = T.wait_for_buffer(buffer.search_buffer_name('bravo'))
T.check(buf ~= nil, 'search buffer never created')
assert_leaf_b_root_is_content(buf)
T.check(T.buffer_text(buf):find('(skgRepos (nodeRepo "Mr Cheese"))', 1, true) ~= nil,
  'spaced repo name is quoted')
do -- leaf-b's first child is its folded containers folder, holding container-a.
  local lines = vim.split(
    T.without_viewnode_ids(table.concat(
      vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')), '\n')
  local root = nil
  for index, line in ipairs(lines) do
    if line:sub(1, #'* (skg node (gnid leaf-b)') == '* (skg node (gnid leaf-b)' then
      root = index
      break
    end
  end
  T.check(root
          and lines[root + 1]:sub(1, #'** (skg folded containerFolder')
              == '** (skg folded containerFolder'
          and lines[root + 2]:find(
                '*** (skg node (gnid container-a) (skgRepos (nodeRepo "Mr Cheese")) writeProtected ',
                1, true) == 1,
          "the first search buffer holds leaf-b's folded containers folder,"
          .. ' holding container-a. Buffer content:\n'
          .. T.buffer_text(buf))
end

-- leaf-b has one container, so its line should include
-- (rels (contains (in 1))).
local content = T.buffer_text(buf)
if content:find('(rels (contains (in 1)))', 1, true) then
  print('node relationship stats present in the search buffer')
  print('Buffer content:\n' .. content)
  T.pass('PASS: Integration test successful!')
else
  T.fail('node relationship stats not found in the search buffer.'
        .. ' Buffer content:\n' .. content)
end
