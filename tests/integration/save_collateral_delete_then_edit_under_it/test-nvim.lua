-- Integration test for delete-then-edit-under-deleted collateral
-- updates, nvim client. The Lua mirror of test-emacs.el in this
-- directory.
--
-- Buffer 1: multi-root view (scaffolded "1" + standalone subee root).
-- Buffer 2: manually constructed (indef 11 + subee + subee-1).
--
-- Phase 4: delete 11 from buffer 2 -> 11 becomes DeletedNode,
--   scaffolds become DeletedScaff in collateral buffer 1.
-- Phase 6: add subee-2 under subee in buffer 1 -> collateral buffer 2
--   picks up subee-2.
-- Phase 8: add new-root under a deletedScaff in buffer 1 ->
--   Deleted/DeletedScaff round-trip harmlessly through save.
--
-- Exercises DeletedNode / DeletedScaff degradation path in
-- complete_viewtree, editing under a DeletedNode, and re-saving a
-- buffer containing Deleted/DeletedScaff nodes.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(40)

local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

-- ── helpers ───────────────────────────────────────────────────────

---(depth type title) triples for every headline in BUF that carries
---skg metadata; headlines without metadata are skipped. Type is
---derived from the second element of the (skg ...) form: node,
---deleted, deletedScaffold, aliasCol, etc. The port of
---save_collateral_break_cycle/test-helpers.el's
---headline-types-and-titles.
---@param buf integer
---@return table[]
local function headline_types_and_titles (buf)
  local result = {}
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split and split.metadata ~= '' then
      local ok, sexp = pcall(sexpr.read, split.metadata)
      if ok and sexpr.is_list(sexp) and #sexp >= 2 then
        local type_elem = sexp[2]
        local type_sym = nil
        if sexpr.is_list(type_elem) and #type_elem >= 1 then
          type_sym = sexpr.atom_text(type_elem[1])
        elseif sexpr.is_symbol(type_elem) then
          type_sym = sexpr.atom_text(type_elem)
        end
        if type_sym then
          table.insert(result,
            { #(split.stars:match('^%*+')), type_sym, split.title })
        end
      end
    end
  end
  return result
end

---@param triples table[]
---@return string
local function format_types_and_titles (triples)
  local parts = {}
  for _, triple in ipairs(triples) do
    table.insert(parts,
      string.format('(%d %s %q)', triple[1], triple[2], triple[3]))
  end
  return table.concat(parts, ', ')
end

---@param buf integer
---@param expected table[]
---@param phase_label string
local function assert_headline_types_and_titles (buf, expected,
                                                 phase_label)
  local actual = headline_types_and_titles(buf)
  local ok = vim.deep_equal(actual, expected)
  if not ok then
    print(phase_label .. ': expected: '
          .. format_types_and_titles(expected))
    print(phase_label .. ': got:      '
          .. format_types_and_titles(actual))
    print(phase_label .. ': buffer content: ' .. T.buffer_text(buf))
  end
  T.check(ok, phase_label .. ': headline-types-and-titles match')
end

---The 1-based line of the Nth (1-based) headline in BUF whose title
---is TITLE, or nil. The port of goto-nth-headline-with-title.
---@param buf integer
---@param title string
---@param n integer
---@return integer|nil
local function nth_line_with_title (buf, title, n)
  local count = 0
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for line_number, line in ipairs(lines) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split and split.title == title then
      count = count + 1
      if count == n then return line_number end
    end
  end
  return nil
end

---The outline depth (star count) of the headline at ROW in BUF.
---@param buf integer
---@param row integer
---@return integer
local function headline_depth_at (buf, row)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1]
  local split = metadata.split_as_stars_metadata_title(line)
  return #(split.stars:match('^%*+'))
end

---The 1-based line just past the subtree rooted at START_ROW in
---BUF (skipping headlines deeper than START_ROW's), or one past the
---buffer's last line. Mirrors the elisp phases' inline
---"forward-line 1 while deeper" loops.
---@param buf integer
---@param start_row integer
---@return integer
local function end_of_subtree_row (buf, start_row)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local start_depth = headline_depth_at(buf, start_row)
  local row = start_row + 1
  while row <= #lines do
    local split = metadata.split_as_stars_metadata_title(lines[row])
    if not split then break end
    if #(split.stars:match('^%*+')) <= start_depth then break end
    row = row + 1
  end
  return row
end

---Insert NEW_LINES into BUF immediately before 1-based line ROW
---(which may be one past the buffer's last line, meaning "append").
---@param buf integer
---@param row integer
---@param new_lines string[]
local function insert_lines_before (buf, row, new_lines)
  local total = vim.api.nvim_buf_line_count(buf)
  local at = math.min(row, total + 1)
  vim.api.nvim_buf_set_lines(buf, at - 1, at - 1, false, new_lines)
end

-- ── phases ────────────────────────────────────────────────────────

print('=== SKG Collateral Delete Then Edit Under It Integration Test ===')

print('=== PHASE 1: Open buffer 1 (content view of node 1) ===')
require('skg.content_view').request_single_root_content_view_from_id('1')
local buf1 = T.wait_for_buffer('skg://1')
T.check(buf1, 'Buffer skg://1 was created')
print('Buffer 1 initial content:\n' .. T.buffer_text(buf1))

print('=== PHASE 2: Extend buffer 1 with second root and alias ===')
-- Add an aliases view request to node 11, append a standalone subee
-- root with its child subee-1, and save. The child must be supplied
-- explicitly: the standalone subee root is definitive, so the save
-- pipeline generates a save instruction for it. Without subee-1 in
-- the buffer, that instruction would set contains to [], overwriting
-- subee.skg on disk.
vim.api.nvim_set_current_buf(buf1)
do
  local line_11 = nth_line_with_title(buf1, '11', 1)
  T.check(line_11, "phase 2: found node '11' in buffer 1")
  metadata.edit_metadata_at_line(line_11,
    sexpr.read('(skg (node (viewRequests (col aliases))))'))
end
vim.api.nvim_buf_set_lines(buf1, -1, -1, false, {
  '* (skg (node (id subee) (source main))) subee',
  '** (skg (node (id subee-1) (source main))) subee-1' })
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'phase 2: save response arrived')
print('Buffer 1 after multi-root save:\n' .. T.buffer_text(buf1))
assert_headline_types_and_titles(buf1,
  { { 1, 'node', '1' },
    { 2, 'node', '11' },
    { 3, 'aliasCol', '' },
    { 4, 'alias', 'eleven' },
    { 3, 'hiddenCol', '' },
    { 4, 'node', 'subee-1' },
    { 4, 'node', 'also-hidden' },
    { 3, 'subscribeeCol', '' },
    { 4, 'node', 'subee' },
    { 4, 'hiddenOutsideOfSubscribeeCol', '' },
    { 5, 'node', 'also-hidden' },
    { 1, 'node', 'subee' },
    { 2, 'node', 'subee-1' } },
  'phase 2: buffer 1 after multi-root save')

print('=== PHASE 3: Create and save buffer 2 ===')
-- Create buffer 2 manually with two roots: indef 11 and subee.
-- subee-1 must be supplied explicitly: the save pipeline's in-memory
-- node map (built from save_instructions) gives subee empty contains,
-- which takes priority over subee.skg on disk.
local buf2 = vim.api.nvim_create_buf(true, false)
vim.api.nvim_buf_set_name(buf2, 'skg://skg-test-buf2')
vim.bo[buf2].filetype = 'org'
vim.b[buf2].skg_view_uri = require('skg.buffer').generate_uuid()
vim.api.nvim_buf_set_lines(buf2, 0, -1, false, {
  '* (skg (node (id 11) (source main) indef)) 11',
  '* (skg (node (id subee) (source main))) subee',
  '** (skg (node (id subee-1) (source main))) subee-1' })
vim.api.nvim_set_current_buf(buf2)
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'phase 3: save response arrived')
print('Buffer 2 after save:\n' .. T.buffer_text(buf2))
assert_headline_types_and_titles(buf2,
  { { 1, 'node', '11' },
    { 1, 'node', 'subee' },
    { 2, 'node', 'subee-1' } },
  'phase 3: buffer 2 initial')

print('=== PHASE 4: Delete 11 from buffer 2 ===')
-- Remove indef, add editRequest delete, and save buffer 2.
vim.api.nvim_set_current_buf(buf2)
metadata.edit_metadata_at_line(1,
  sexpr.read('(skg (node (DELETE indef) (editRequest delete)))'))
print('Buffer 2 after metadata edit:\n' .. T.buffer_text(buf2))
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'phase 4: save response arrived')

print('=== PHASE 5: Verify both buffers after delete ===')
-- Buffer 2: '11' became DeletedNode, 'subee' is unchanged.
T.check(vim.api.nvim_buf_is_valid(buf2), 'Buffer 2 still exists')
print('Buffer 2 after delete:\n' .. T.buffer_text(buf2))
assert_headline_types_and_titles(buf2,
  { { 1, 'deleted', '11' },
    { 1, 'node', 'subee' },
    { 2, 'node', 'subee-1' } },
  'phase 5: buffer 2 after delete')
-- Buffer 1 (collateral): the delete of 11 propagates to node 1
-- (which contained it). 1.skg's contains has had 11 stripped, so on
-- rerender the 11-subtree is gone entirely from buffer 1 -- no
-- DeletedNode, no DeletedScaff descendants.
T.check(vim.api.nvim_buf_is_valid(buf1), 'Buffer 1 still exists')
print('Buffer 1 after collateral delete:\n' .. T.buffer_text(buf1))
assert_headline_types_and_titles(buf1,
  { { 1, 'node', '1' },
    { 1, 'node', 'subee' },
    { 2, 'node', 'subee-1' } },
  'phase 5: buffer 1 after collateral delete')

print('=== PHASE 6: Add subee-2 in buffer 1 ===')
-- Add subee-2 as a child of the standalone subee root in buffer 1
-- and save. The first "subee" match is now what used to be the
-- second match, because the earlier first match was a child of a
-- now-deleted scaffold.
vim.api.nvim_set_current_buf(buf1)
do
  local subee_row = nth_line_with_title(buf1, 'subee', 1)
  T.check(subee_row, 'phase 6: found the standalone subee root')
  local subee_depth = headline_depth_at(buf1, subee_row)
  local insertion_row = end_of_subtree_row(buf1, subee_row)
  insert_lines_before(buf1, insertion_row,
    { string.rep('*', subee_depth + 1) .. ' subee-2' })
end
print('Buffer 1 after inserting subee-2:\n' .. T.buffer_text(buf1))
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'phase 6: save response arrived')

print('=== PHASE 7: Verify both buffers after adding subee-2 ===')
-- Buffer 1: subee-2 appears under the standalone subee root.
T.check(vim.api.nvim_buf_is_valid(buf1), 'Buffer 1 still exists')
print('Buffer 1 after adding subee-2:\n' .. T.buffer_text(buf1))
assert_headline_types_and_titles(buf1,
  { { 1, 'node', '1' },
    { 1, 'node', 'subee' },
    { 2, 'node', 'subee-1' },
    { 2, 'node', 'subee-2' } },
  'phase 7: buffer 1 after adding subee-2')
-- Buffer 2: gains subee-2 from collateral update.
T.check(vim.api.nvim_buf_is_valid(buf2), 'Buffer 2 still exists')
print('Buffer 2 after collateral update:\n' .. T.buffer_text(buf2))
assert_headline_types_and_titles(buf2,
  { { 1, 'deleted', '11' },
    { 1, 'node', 'subee' },
    { 2, 'node', 'subee-1' },
    { 2, 'node', 'subee-2' } },
  'phase 7: buffer 2 after updating collateral view')

print('=== PHASE 8: Add new-root under deletedScaff in buffer 1 ===')
-- Add new-root as a sibling of the standalone subee in buffer 1.
-- Originally this exercised inserting a node under a DeletedScaff;
-- after the delete pipeline started cleaning up references to the
-- deleted node, buffer 1 no longer contains a DeletedScaff, so this
-- phase now exercises adding a top-level root via metadata-only
-- insertion (the BufferRoot, like a DeletedScaff, supplies no source
-- to inherit, so explicit (source main) is still required).
vim.api.nvim_set_current_buf(buf1)
do
  local subee_row = nth_line_with_title(buf1, 'subee', 1)
  T.check(subee_row, 'phase 8: found the standalone subee root')
  local subee_depth = headline_depth_at(buf1, subee_row)
  local insertion_row = end_of_subtree_row(buf1, subee_row)
  insert_lines_before(buf1, insertion_row,
    { string.rep('*', subee_depth)
      .. ' (skg (node (source main))) new-root' })
end
print('Buffer 1 after inserting new-root:\n' .. T.buffer_text(buf1))
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'phase 8: save response arrived')

print('=== PHASE 9: Verify both buffers after new-root ===')
-- Buffer 1: new-root appears at the top level, everything else
-- unchanged from phase 7.
T.check(vim.api.nvim_buf_is_valid(buf1), 'Buffer 1 still exists')
print('Buffer 1 after new-root save:\n' .. T.buffer_text(buf1))
assert_headline_types_and_titles(buf1,
  { { 1, 'node', '1' },
    { 1, 'node', 'subee' },
    { 2, 'node', 'subee-1' },
    { 2, 'node', 'subee-2' },
    { 1, 'node', 'new-root' } },
  'phase 9: buffer 1 after new-root')
-- We could also verify that buffer 2 is unchanged, but meh.

T.pass('PASS: All phases completed successfully!')
