-- Integration test: opening a content view of `child', whose parent
-- contains it, should render the buffer with `parent' prepended as
-- the first child of `child' (Birth::ContainsParent indefinitive).
-- Because `child' is contained in the graph, the view-root is born as
-- container; that default is implicit in emitted metadata.
--
-- The Lua mirror of test-emacs.el in this directory.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

print('=== SKG content-view-from-non-root Integration Test ===')

print('=== PHASE 1: Requesting content view of `child` ===')
require('skg.content_view')
  .request_single_root_content_view_from_id('child')
local view = T.wait_for_buffer('skg://the child', 15)
if not view then
  -- Fall back to any content-view buffer, the way the elisp test's
  -- find-skg-content-buffer does when the title-derived name misses.
  for _, buf in ipairs(require('skg.buffer').all_skg_buffers()) do
    if not vim.api.nvim_buf_get_name(buf):match('^skg://%?') then
      view = buf
      break
    end
  end
end
T.check(view, 'no content view buffer was created')

print('=== PHASE 2: Verifying parent prepended as first child ===')
local text = T.buffer_text(view)
print('Buffer:\n' .. text)
local lines = vim.api.nvim_buf_get_lines(view, 0, -1, false)

local child_root_prefix = '* (skg (node (id child)'
local root_line = nil
for _, line in ipairs(lines) do
  if line:sub(1, #child_root_prefix) == child_root_prefix then
    root_line = line
    break
  end
end
T.check(root_line, string.format(
  'view-root not as expected; got:\n%s', text))

T.check(not root_line:find('(parentIs independent)', 1, true),
  string.format(
    'contained view-root should be content, not independent; line: %s',
    root_line))
T.check(not root_line:find(
    '(parentIs independent) indef (rels (contains (out 1 (ancestors 1))) (birth contains))', 1, true),
  string.format(
    'contained view-root should be content, not content; line: %s',
    root_line))
T.check(not root_line:find(
    '(parentIs independent) indef (rels (textlinksTo (out (ancestors 1))) (birth textlinksTo))', 1, true),
  string.format(
    'contained view-root should be content, not line: %s', root_line))

local parent_headline_prefix = '** (skg (node (id parent)'
local parent_line = nil
for _, line in ipairs(lines) do
  if line:sub(1, #parent_headline_prefix) == parent_headline_prefix then
    parent_line = line
    break
  end
end
T.check(parent_line, string.format(
  'no level-2 headline for parent; buffer:\n%s', text))

T.check(parent_line:find(
    '(parentIs independent) indef (rels (contains (out 1 (ancestors 1))) (birth contains))', 1, true) ~= nil,
  string.format(
    'parent is not (parentIs independent) indef (rels (contains (out 1 (ancestors 1))) (birth contains)); '
    .. 'line: %s', parent_line))
T.check(parent_line:find(' indef%f[%A]') ~= nil,
  string.format('parent is not indefinitive; line: %s', parent_line))

T.pass('PASS: Integration test successful!')
