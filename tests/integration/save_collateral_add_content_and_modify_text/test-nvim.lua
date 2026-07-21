-- Integration test for collateral buffer updates: title edit + add
-- child, nvim client. The Lua mirror of test-emacs.el in this
-- directory.
--
-- Opens two buffers over a containment cycle (a contains b, b
-- contains a). From buffer B, edits a's title and adds a new child
-- "c" of a (no metadata -- the server reads it as an ActiveNode with
-- a random UUID). Verifies that buffer B's save response reflects the
-- changes (title, new node with UUID). Then checks collateral buffer
-- A.
--
-- Known limitation: collateral completion currently fails when the
-- saved buffer introduced a brand-new node, because
-- build_child_creation_data can't find a source for the new UUID. So
-- buffer A picks up the title change and the new child c anyway (per
-- the elisp test's phase 5 -- the collateral update DOES succeed
-- here).

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20)

local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

-- ── ported from save_collateral_break_cycle/test-helpers.el
-- (duplicated per test file: creating a shared Lua helper module is
-- outside this port's scope) ─────────────────────────────────────────

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
    -- Birth is now a WHITE span inside (rels ...) (no separate
    -- birthHerald atom); the graft's outbound-ancestor token still
    -- shows in the rels spans' VISIBLE text, which we concatenate here.
    local rels_list = metadata.sexp_cdr_at_path(
      sexp, { 'skg', 'node', 'rels' })
    local pieces = {}
    for _, span in ipairs(rels_list or {}) do
      if type(span) == 'table' and type(span[2]) == 'string' then
        table.insert(pieces, span[2]) end
    end
    graft_role = graft_role_from_herald(table.concat(pieces))
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

---(depth relation title) triples for every headline in BUF, whether
---or not it carries metadata. The port of test-helpers.el's
---headline-titles.
---@param buf integer
---@return table[]
local function headline_titles (buf)
  local result = {}
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split then
      local sexp = nil
      if split.metadata ~= '' then
        local ok, parsed = pcall(sexpr.read, split.metadata)
        if ok then sexp = parsed end
      end
      table.insert(result,
        { #(split.stars:match('^%*+')), relation_from_sexp(sexp),
          split.title })
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

---@param buf integer
---@param expected table[]
---@param phase_label string
local function assert_headline_titles (buf, expected, phase_label)
  local actual = headline_titles(buf)
  local ok = vim.deep_equal(actual, expected)
  if not ok then
    print(string.format('%s: expected (%s)', phase_label,
                        format_triples(expected)))
    print(string.format('%s: got      (%s)', phase_label,
                        format_triples(actual)))
    print(phase_label .. ': buffer content: ' .. T.buffer_text(buf))
  end
  T.check(ok, phase_label .. ': headline-titles matches')
end

-- ── phases ────────────────────────────────────────────────────────

print('=== SKG Collateral Add Content Integration Test ===')

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

print("=== PHASE 3: Edit a's title, add child c, save ===")
assert_headline_titles(buf_b,
  { { 1, 'absent', 'b' },
    { 2, 'container', 'a' },
    { 3, 'container', 'b' },
    { 2, 'affected', 'a' },
    { 3, 'affected', 'b' } },
  'phase 3: buffer B before edit')
vim.api.nvim_set_current_buf(buf_b)
do -- Change a's title on line 4 (the definitive a): replace the
   -- trailing "a" with a longer title.
  local line4 = vim.api.nvim_buf_get_lines(buf_b, 3, 4, false)[1]
  local new_line4 = line4:sub(1, #line4 - 1)
                    .. 'Node a was given this longer title'
  vim.api.nvim_buf_set_lines(buf_b, 3, 4, false, { new_line4 })
end
-- Add a new child of a with no metadata.
vim.api.nvim_buf_set_lines(buf_b, -1, -1, false, { '*** c' })
print('Buffer B after edit: ' .. T.buffer_text(buf_b))
assert_headline_titles(buf_b,
  { { 1, 'absent', 'b' },
    { 2, 'container', 'a' },
    { 3, 'container', 'b' },
    { 2, 'affected', 'Node a was given this longer title' },
    { 3, 'affected', 'b' },
    { 3, 'affected', 'c' } },
  'phase 3: buffer B after edit')
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'phase 3: save response arrived')

print('=== PHASE 4: Verify buffer B after save ===')
T.check(vim.api.nvim_buf_is_valid(buf_b), 'Buffer B still exists')
assert_headline_titles(buf_b,
  { { 1, 'absent', 'b' },
    { 2, 'container', 'Node a was given this longer title' },
    { 3, 'container', 'b' },
    { 2, 'affected', 'Node a was given this longer title' },
    { 3, 'affected', 'b' },
    { 3, 'affected', 'c' } },
  'phase 4: buffer B after save')
do -- The line for c should now have its title and a server id.
  local line6 = vim.api.nvim_buf_get_lines(buf_b, 5, 6, false)[1]
  local split = metadata.split_as_stars_metadata_title(line6 or '')
  T.check(split and split.title == 'c', string.format(
    'phase 4: line for c had title %s, expected "c"',
    split and split.title or 'nil'))
  local sexp = nil
  if split.metadata ~= '' then
    local ok, parsed = pcall(sexpr.read, split.metadata)
    if ok then sexp = parsed end
  end
  local id = sexp and metadata.node_id(sexp) or nil
  T.check(id, 'phase 4: line for c has no node id in metadata')
  print('phase 4: c was assigned id ' .. tostring(id))
end

print('=== PHASE 5: Verify collateral buffer A ===')
T.check(vim.api.nvim_buf_is_valid(buf_a), 'Buffer A still exists')
print('Buffer A content: ' .. T.buffer_text(buf_a))
assert_headline_titles(buf_a,
  { { 1, 'absent', 'Node a was given this longer title' },
    { 2, 'container', 'b' },
    { 3, 'container', 'Node a was given this longer title' },
    { 2, 'affected', 'b' },
    { 3, 'affected', 'Node a was given this longer title' },
    { 2, 'affected', 'c' } },
  'phase 5: buffer A after collateral update')

T.pass('PASS: All phases completed successfully!')
