-- Integration test for collateral buffer updates via save pipeline,
-- nvim client. The Lua mirror of test-emacs.el in this directory.
--
-- Opens two buffers over a containment cycle (a contains b, b
-- contains a), removes 'a' from b's children and saves. The
-- collateral a-view keeps the now-unwarranted 'a' underneath 'b',
-- claiming nothing: the refresh never detaches.
--
-- This test does not exercise DeletedNode -- no nodes are deleted
-- from disk, only a containment relationship is modified.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20)

local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

-- ── ported from test-helpers.el (duplicated per test file: creating
-- a shared Lua helper module is outside this port's scope) ──────────

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

---A chain node's ROLENAME from its semantic relationship facts
---(RELS_BODY = the cdr of (view (rels ...))), or nil. A chain node relates outbound
---to a tracked ancestor: a relation with an (out ... (ancestors ...))
---side, mapped to its role. Port of headline--role-of-chain-node-from-rels.
local function role_of_chain_node_from_rels (rels_body)
  if not rels_body then return nil end
  local roles = { { 'contains', 'container' }, { 'linksTo', 'mentioner' },
                  { 'subscribesTo', 'subscribee' }, { 'overrides', 'overrider' },
                  { 'hidesFromSubs', 'hider' } }
  for _, pair in ipairs(roles) do
    local form = child_named(rels_body, pair[1])
    if form then
      local out = child_named(form, 'out')
      if out and child_named(out, 'ancestors') then return pair[2] end
    end
  end
  return nil
end

---Classify a parsed metadata SEXP's headline relation: a chain node's
---rolename, else 'noClaim' or 'member'. CLAIMS_NOTHING says it stands
---under a write-protected node, where no child claims and none carries
---the atom; CHAIN_NODE says it is a node of the containerward chain
---(see position_of). The port of test-helpers.el's
---headline--relation-from-sexp.
---@param sexp any|nil
---@param claims_nothing boolean|nil
---@param chain_node boolean|nil
---@return string
local function relation_from_sexp (sexp, claims_nothing, chain_node)
  local non_member = claims_nothing
    or (sexp ~= nil and not metadata.node_member_p(sexp))
  local chain_node_role = nil
  if chain_node then
    chain_node_role = role_of_chain_node_from_rels(metadata.sexp_cdr_at_path(
      sexp, { 'skg', 'view', 'rels' }))
  end
  if chain_node_role then return chain_node_role end
  if non_member then return 'noClaim' end
  return 'member'
end

---Whether SEXP's top level holds the symbol NAME.
local function holds_atom (sexp, name)
  if type(sexp) ~= 'table' then return false end
  for _, element in ipairs(sexp) do
    if element == sexpr.symbol(name) then return true end
  end
  return false
end

---The headline at DEPTH, by its position. STACK holds, nearest last,
---{ depth, kind, write_protected, chain_node } for the headlines above
---it, deeper ones already popped. Returns that record for this
---headline and whether it claims nothing: a node under a
---write-protected node claims nothing, and is a chain node when that
---node is a folder's member or itself a chain node. The port of
---test-helpers.el's headline--position-of.
local function position_of (stack, depth, sexp, metadata_str)
  local parent, grandparent = stack[#stack], stack[#stack - 1]
  local kind = nil
  if holds_atom(sexp, 'node') then kind = 'node'
  elseif metadata_str and metadata_str:find('Folder') then kind = 'folder' end
  local under_write_protected_node = kind == 'node' and parent ~= nil
    and parent.kind == 'node' and parent.write_protected
  local chain_node = under_write_protected_node
    and (parent.chain_node or (grandparent ~= nil and grandparent.kind == 'folder'))
  return { depth = depth, kind = kind,
           write_protected = holds_atom(sexp, 'writeProtected'),
           chain_node = chain_node or false },
         under_write_protected_node or false
end

---Pop from STACK every record at DEPTH or deeper.
local function pop_to (stack, depth)
  while #stack > 0 and stack[#stack].depth >= depth do
    table.remove(stack) end
end

---(depth relation id) triples for every headline in BUF that carries
---an id; headlines without metadata are skipped. The port of
---test-helpers.el's headline-structure.
---@param buf integer
---@return table[]
local function headline_structure (buf)
  local result, stack = {}, {}
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split then
      local sexp = nil
      if split.metadata ~= '' then
        local ok, parsed = pcall(sexpr.read, split.metadata)
        if ok then sexp = parsed end
      end
      local gnid = sexp and metadata.node_id(sexp) or nil
      local depth = #(split.stars:match('^%*+'))
      pop_to(stack, depth)
      local record, claims_nothing = position_of(stack, depth, sexp, split.metadata)
      table.insert(stack, record)
      if gnid then
        table.insert(result,
          { depth, relation_from_sexp(sexp, claims_nothing, record.chain_node), gnid })
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
  { { 1, 'member', 'a' },
    { 3, 'member', 'b' },     -- in a's containers folder
    { 4, 'container', 'a' },  -- its chain
    { 5, 'container', 'b' },
    { 2, 'member', 'b' },
    { 3, 'member', 'a' } },
  'phase 1: buffer A initial')

print('=== PHASE 2: Open buffer B ===')
require('skg.content_view').request_single_root_content_view_from_id('b')
local buf_b = T.wait_for_buffer('skg://b')
T.check(buf_b, 'Buffer skg://b was created')
assert_headline_structure(buf_b,
  { { 1, 'member', 'b' },
    { 3, 'member', 'a' },     -- in b's containers folder
    { 4, 'container', 'b' },  -- its chain
    { 5, 'container', 'a' },
    { 2, 'member', 'a' },
    { 3, 'member', 'b' } },
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
  { { 1, 'member', 'a' },
    -- b no longer contains a. The refresh keeps what stood there,
    -- unwarranted: b stays in a's containers folder, with its chain
    -- (a, then b, which no longer contains a), and a stays under b,
    -- claiming nothing.
    { 3, 'member', 'b' },
    { 4, 'container', 'a' },
    { 5, 'noClaim', 'b' },
    { 2, 'member', 'b' },
    { 3, 'noClaim', 'a' } },
  'phase 4: buffer A after collateral update')

T.pass('PASS: All phases completed successfully!')
