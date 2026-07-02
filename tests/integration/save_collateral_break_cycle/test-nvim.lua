-- Integration test for collateral buffer updates via save pipeline,
-- nvim client. The Lua mirror of test-emacs.el in this directory.
--
-- Opens two buffers over a containment cycle (a contains b, b
-- contains a), removes 'a' from b's children and saves. The
-- collateral a-view should lose the now-stale indefinitive 'a'
-- underneath 'b', because complete_relevant_children discards
-- children not in the parent's goal_list.
--
-- This test does NOT exercise DeletedNode -- no nodes are deleted
-- from disk, only a containment relationship is modified.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20)

local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

-- ── ported from test-helpers.el (duplicated per test file: creating
-- a shared Lua helper module is outside this port's scope) ──────────

---Classify a graft HERALD (e.g. "Ca") into its backpath ROLENAME, or
---nil. The port of test-helpers.el's headline--graft-role-from-herald.
---@param herald string|nil
---@return string|nil
local function graft_role_from_herald (herald)
  if not herald then return nil end
  local letter = herald:match('([CLHSO])%d*%l')
  if not letter then return nil end
  return ({ C = 'container', L = 'linkSource', S = 'subscribee',
            O = 'overrider', H = 'hider' })[letter]
end

---Classify a parsed metadata SEXP's headline relation: a backpath
---graft's rolename, else the explicit parentIs, else 'affected'. The
---port of test-helpers.el's headline--relation-from-sexp.
---@param sexp any|nil
---@return string
local function relation_from_sexp (sexp)
  local parentIs_list = sexp and metadata.sexp_cdr_at_path(
    sexp, { 'skg', 'node', 'parentIs' }) or nil
  local independent = parentIs_list ~= nil
    and parentIs_list[1] == sexpr.symbol('independent')
  local graft_role = nil
  if independent then
    local birth_list = metadata.sexp_cdr_at_path(
      sexp, { 'skg', 'node', 'birthHerald' })
    local rels_list = metadata.sexp_cdr_at_path(
      sexp, { 'skg', 'node', 'rels' })
    local birth_text = birth_list and birth_list[1]
      and sexpr.atom_text(birth_list[1]) or nil
    local rels_text = rels_list and rels_list[1]
      and sexpr.atom_text(rels_list[1]) or nil
    graft_role = graft_role_from_herald(birth_text)
      or graft_role_from_herald(rels_text)
  end
  if graft_role then return graft_role end
  if parentIs_list == nil
     or parentIs_list[1] == sexpr.symbol('affected') then
    return 'affected'
  end
  return sexpr.atom_text(parentIs_list[1])
end

---(depth relation id) triples for every headline in BUF that carries
---an id; headlines without metadata are skipped. The port of
---test-helpers.el's headline-structure.
---@param buf integer
---@return table[]
local function headline_structure (buf)
  local result = {}
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split then
      local sexp = nil
      if split.metadata ~= '' then
        local ok, parsed = pcall(sexpr.read, split.metadata)
        if ok then sexp = parsed end
      end
      local id = sexp and metadata.node_id(sexp) or nil
      if id then
        table.insert(result,
          { #(split.stars:match('^%*+')), relation_from_sexp(sexp), id })
      end
    end
  end
  return result
end

---@param triples table[]
---@return string
local function format_triples (triples)
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
local function assert_headline_structure (buf, expected, phase_label)
  local actual = headline_structure(buf)
  local ok = vim.deep_equal(actual, expected)
  if not ok then
    print(string.format('%s: expected (%s)', phase_label,
                        format_triples(expected)))
    print(string.format('%s: got      (%s)', phase_label,
                        format_triples(actual)))
    print(phase_label .. ': buffer content: ' .. T.buffer_text(buf))
  end
  T.check(ok, phase_label .. ': headline-structure matches')
end

-- ── phases ────────────────────────────────────────────────────────

print('=== SKG Save Collateral Buffers Integration Test ===')

print('=== PHASE 1: Open buffer A ===')
require('skg.content_view').request_single_root_content_view_from_id('a')
local buf_a = T.wait_for_buffer('skg://a')
T.check(buf_a, 'Buffer skg://a was created')
assert_headline_structure(buf_a,
  { { 1, 'absent', 'a' },
    { 2, 'container', 'b' },
    { 3, 'container', 'a' },
    { 2, 'affected', 'b' },
    { 3, 'affected', 'a' } },
  'phase 1: buffer A initial')

print('=== PHASE 2: Open buffer B ===')
require('skg.content_view').request_single_root_content_view_from_id('b')
local buf_b = T.wait_for_buffer('skg://b')
T.check(buf_b, 'Buffer skg://b was created')
assert_headline_structure(buf_b,
  { { 1, 'absent', 'b' },
    { 2, 'container', 'a' },
    { 3, 'container', 'b' },
    { 2, 'affected', 'a' },
    { 3, 'affected', 'b' } },
  'phase 2: buffer B initial')

print("=== PHASE 3: Remove a from b's children, save ===")
vim.api.nvim_set_current_buf(buf_b)
do -- Keep only the root headline (line 1); drop everything else.
  local first_line = vim.api.nvim_buf_get_lines(buf_b, 0, 1, false)
  vim.api.nvim_buf_set_lines(buf_b, 0, -1, false, first_line)
end
print('Buffer B after edit: ' .. T.buffer_text(buf_b))
vim.api.nvim_win_set_cursor(0, { 1, 0 })
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'phase 3: save response arrived')

print('=== PHASE 4: Verify collateral buffer A ===')
T.check(vim.api.nvim_buf_is_valid(buf_a), 'Buffer A still exists')
print('Buffer A content after collateral update: '
      .. T.buffer_text(buf_a))
assert_headline_structure(buf_a,
  { { 1, 'absent', 'a' },
    -- was container; corrected after b dropped a:
    { 2, 'independent', 'b' },
    { 3, 'container', 'a' },
    { 2, 'affected', 'b' } },
  'phase 4: buffer A after collateral update')

T.pass('PASS: All phases completed successfully!')
