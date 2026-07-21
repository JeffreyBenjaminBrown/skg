-- Integration test for git diff view updates, nvim client. The Lua
-- mirror of test-emacs.el in this directory. For an overview, see
-- tests/integration/git-diff-view-updates/data/skg-data/README.org

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45)

local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local INDEPENDENT = sexpr.symbol('independent')
local AFFECTED = sexpr.symbol('affected')

---The first element of LIST (a 1-indexed array of sexps) that is a list
---whose head is the symbol NAME, or nil.
local function child_named (list, name)
  if not list then return nil end
  for _, child in ipairs(list) do
    if type(child) == 'table' and child[1] == sexpr.symbol(name) then
      return child end
  end
  return nil
end

---A backpath graft's ROLENAME from its SEMANTIC relationship facts, or
---nil. RELS_BODY is the cdr of the (rels ...) form. A graft relates
---OUTBOUND to a tracked ancestor: a relation with an (out ...
---(ancestors ...)) side, mapped to its role (contains -> container,
---textlinksTo -> linkSource, subscribes -> subscribee, overrides ->
---overrider, hides -> hider). Only meaningful for a node already known
---to be a graft (parentIs independent).
---@param rels_body any|nil
---@return string|nil
local function graft_role_from_rels (rels_body)
  if not rels_body then return nil end
  local roles = { { 'contains', 'container' }, { 'textlinksTo', 'linkSource' },
                  { 'subscribes', 'subscribee' }, { 'overrides', 'overrider' },
                  { 'hides', 'hider' } }
  for _, pair in ipairs(roles) do
    local form = child_named(rels_body, pair[1])
    if form then
      local out = child_named(form, 'out')
      if out and child_named(out, 'ancestors') then return pair[2] end
    end
  end
  return nil
end

---Classify a parsed metadata SEXP (or nil) into a relation string:
---the graft role of a backpath graft (independent node with an
---outbound-ancestor relation), else the explicit parentIs, else the
---implicit 'affected'. Port of headline--relation-from-sexp.
---@param sexp any|nil
---@return string
local function relation_from_sexp (sexp)
  local parentIs_list =
    metadata.sexp_cdr_at_path(sexp, { 'skg', 'node', 'parentIs' })
  local rels_body =
    metadata.sexp_cdr_at_path(sexp, { 'skg', 'node', 'rels' })
  local independent = parentIs_list and parentIs_list[1] == INDEPENDENT
  local graft_role = independent
    and graft_role_from_rels(rels_body) or nil
  if graft_role then return graft_role end
  if not parentIs_list or parentIs_list[1] == AFFECTED then
    return 'affected' end
  return sexpr.atom_text(parentIs_list[1])
end

---(depth, relation, title) triples for every headline in BUF. Port of
---headline-titles in save_collateral_break_cycle/test-helpers.el.
---@param buf integer
---@return table[]
local function headline_titles (buf)
  local result = {}
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split then
      local depth = #(split.stars:gsub('%s+$', ''))
      local sexp = nil
      if split.metadata ~= '' then
        local ok, parsed = pcall(sexpr.read, split.metadata)
        if ok then sexp = parsed end
      end
      table.insert(result, { depth, relation_from_sexp(sexp), split.title })
    end
  end
  return result
end

---@param triples table[]
---@return string
local function format_triples (triples)
  local parts = {}
  for _, triple in ipairs(triples) do
    table.insert(parts, string.format('(%d %s %q)',
                                      triple[1], triple[2], triple[3]))
  end
  return table.concat(parts, ', ')
end

---@param a table[]
---@param b table[]
---@return boolean
local function triples_equal (a, b)
  if #a ~= #b then return false end
  for i = 1, #a do
    if a[i][1] ~= b[i][1] or a[i][2] ~= b[i][2] or a[i][3] ~= b[i][3] then
      return false end
  end
  return true
end

---Assert BUF's headline titles match EXPECTED exactly. Port of
---assert-headline-titles.
---@param buf integer
---@param expected table[]
---@param phase_label string
local function assert_headline_titles (buf, expected, phase_label)
  local actual = headline_titles(buf)
  print(string.format('[%s] expected: (%s)',
                      phase_label, format_triples(expected)))
  print(string.format('[%s] actual:   (%s)',
                      phase_label, format_triples(actual)))
  T.check(triples_equal(actual, expected),
          phase_label .. ': headline titles match')
end

print('=== SKG Git Diff View Updates Integration Test ===')

-- PHASE 1: open view-a.
print('=== PHASE 1: Open view from a ===')
require('skg.content_view').request_single_root_content_view_from_id('a')
local buf_a = T.wait_for_buffer('skg://a')
if not buf_a then T.fail('phase 1: buffer skg://a not created') end
assert_headline_titles(buf_a,
  { { 1, 'absent', 'a' },
    { 2, 'affected', 'b' },
    { 3, 'affected', 'c' },
    { 3, 'affected', 'd' },
    { 3, 'affected', 'e' } },
  'phase 1: view-a initial')

-- PHASE 2: open view-b. multi_root_view prepends b's containerward
-- ancestry (a) as b's first child before the definitive affected
-- children (c, d, e).
print('=== PHASE 2: Open view from b ===')
require('skg.content_view').request_single_root_content_view_from_id('b')
local buf_b = T.wait_for_buffer('skg://b')
if not buf_b then T.fail('phase 2: buffer skg://b not created') end
assert_headline_titles(buf_b,
  { { 1, 'absent', 'b' },
    { 2, 'container', 'a' },
    { 2, 'affected', 'c' },
    { 2, 'affected', 'd' },
    { 2, 'affected', 'e' } },
  'phase 2: view-b initial')

-- PHASE 3: edit view-b and save. c gets editRequest delete; d is
-- removed from b's children; e is renamed; f added with d as child.
print('=== PHASE 3: Edit view-b and save ===')
vim.api.nvim_set_current_buf(buf_b)
print('Buffer-b before edit: ' .. T.buffer_text(buf_b))
vim.api.nvim_buf_set_lines(buf_b, 0, -1, false, {
  '* (skg (node (id b) (source main))) b',
  '** (skg (node (id c) (source main) (editRequest delete))) c',
  '** (skg (node (id e) (source main))) e, edited',
  '** (skg (node (id f) (source main))) f',
  '*** (skg (node (id d) (source main))) d',
})
vim.api.nvim_win_set_cursor(0, { 1, 0 })
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(), 'phase 3: save response arrived')

-- PHASE 4: verify view-b after save. c should be gone (deleted).
-- d should appear under f.
print('=== PHASE 4: Verify view-b after save ===')
print('Buffer-b after save: ' .. T.buffer_text(buf_b))
assert_headline_titles(buf_b,
  { { 1, 'absent', 'b' },
    { 2, 'affected', 'e, edited' },
    { 2, 'affected', 'f' },
    { 3, 'affected', 'd' } },
  'phase 4: view-b after save')

-- PHASE 5: toggle git diff mode ON. Must be in a content-view buffer
-- for the save-triggered rerender to work.
print('=== PHASE 5: Toggle git diff mode ON ===')
vim.api.nvim_set_current_buf(buf_b)
require('skg.diff_mode').toggle()
T.check(T.wait_for_response(20), 'phase 5: diff-mode-on response arrived')

-- PHASE 6: verify diff markers in view-b. Check for the presence of
-- diff-related strings rather than an exact line-by-line match,
-- because graphStats etc. may vary.
print('=== PHASE 6: Verify diff markers in view-b ===')
do
  local content = T.buffer_text(buf_b)
  print('Buffer-b with diff: ' .. content)
  for _, marker in ipairs({ 'removed', 'e, edited', 'f' }) do
    T.check(content:find(marker, 1, true) ~= nil,
            string.format('phase 6: view-b contains %q', marker))
  end
end

-- PHASE 7: verify diff markers in view-a; a's view should show the
-- same diff markers under b.
print('=== PHASE 7: Verify diff markers in view-a ===')
do
  local content = T.buffer_text(buf_a)
  print('Buffer-a with diff: ' .. content)
  for _, marker in ipairs({ 'removed', 'e, edited', 'f' }) do
    T.check(content:find(marker, 1, true) ~= nil,
            string.format('phase 7: view-a contains %q', marker))
  end
end

-- PHASE 8: commit e's title change, re-save to refresh the diff.
print("=== PHASE 8: Commit e.skg, re-save to refresh diff ===")
do
  local skg_data = os.getenv('SKG_DATA_DIR') or 'data/skg-data'
  print('Committing e.skg in ' .. skg_data)
  vim.fn.system({ 'git', '-C', skg_data, 'add', 'e.skg' })
  vim.fn.system({ 'git', '-C', skg_data, 'commit', '-q', '-m',
                 'update e title' })
  T.check(vim.v.shell_error == 0, 'phase 8: git commit of e.skg succeeded')
end
vim.api.nvim_set_current_buf(buf_b)
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(20), 'phase 8: re-save response arrived')

-- PHASE 9: verify textChanged gone (the title change is now in HEAD).
print('=== PHASE 9: Verify textChanged gone after commit ===')
do
  local content = T.buffer_text(buf_b)
  print('Buffer-b after commit: ' .. content)
  T.check(content:find('textChanged', 1, true) == nil,
          'phase 9: textChanged gone from view-b')
end

-- PHASE 10: toggle git diff mode OFF.
print('=== PHASE 10: Toggle git diff mode OFF ===')
vim.api.nvim_set_current_buf(buf_b)
require('skg.diff_mode').toggle()
T.check(T.wait_for_response(20), 'phase 10: diff-mode-off response arrived')

-- PHASE 11: verify both views are clean (no diff/phantom markers).
print('=== PHASE 11: Verify views are clean (no diff markers) ===')
assert_headline_titles(buf_b,
  { { 1, 'absent', 'b' },
    { 2, 'affected', 'e, edited' },
    { 2, 'affected', 'f' },
    { 3, 'affected', 'd' } },
  'phase 11: view-b clean')
do
  local titles_a = headline_titles(buf_a)
  print('Buffer-a titles after diff-off: ' .. format_triples(titles_a))
  -- a should contain b, b should contain e,f; f contains d. The exact
  -- indefinitive markers vary, so just check titles.
  for _, expected_title in ipairs({ 'a', 'b', 'e, edited', 'f', 'd' }) do
    local found = false
    for _, triple in ipairs(titles_a) do
      if triple[3] == expected_title then found = true break end
    end
    T.check(found,
            string.format('phase 11: view-a contains title %q',
                          expected_title))
  end
  local content = T.buffer_text(buf_a)
  T.check(content:find('diff:', 1, true) == nil,
          'phase 11: no diff markers remain in view-a')
  T.check(content:find('textChanged', 1, true) == nil,
          'phase 11: textChanged remains gone from view-a')
end

print('=== All phases completed! ===')
T.pass()
