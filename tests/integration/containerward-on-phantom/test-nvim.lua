-- Integration test: auto-inserted containerward ancestry on
-- removed-here phantoms, nvim client. The Lua mirror of test-emacs.el
-- in this directory.
--
-- Graph (at HEAD): a contains [b, c], b contains [c].
--
-- Phase 1: Open view from a. Shows a -> {b -> c(indef), c}.
-- Phase 2: Delete the indefinitive c from under b and save.
--          b.skg is updated (contains: []).
-- Phase 3: Toggle diff mode on.
--          Under b, c appears as a removed-here phantom.
-- Phase 4: Document the current missing containerward ancestry
--          behavior.
--
-- The elisp test's headline-classification helpers (from
-- ../save_collateral_break_cycle/test-helpers.el) have no nvim port
-- to reuse yet, so the pieces this test needs are reproduced locally
-- below, ported 1:1 from that elisp file.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45) -- the elisp test's run-at-time timeout was 45s

local content_view = require('skg.content_view')
local diff_mode = require('skg.diff_mode')
local metadata = require('skg.metadata')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')

-- ── headline classification (ported from test-helpers.el) ──────────

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

---A backpath graft's ROLENAME from its SEMANTIC relationship facts
---(RELS_BODY = the cdr of (rels ...)), or nil. A graft relates OUTBOUND
---to a tracked ancestor: a relation with an (out ... (ancestors ...))
---side, mapped to its role. Mirrors headline--graft-role-from-rels.
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

---Classify a headline's parsed metadata SEXP into a relation string: a
---graft role name, 'affected', or the literal parentIs symbol text
---(e.g. 'absent', 'independent'). Mirrors headline--relation-from-sexp.
---@param sexp any|nil
---@return string
local function relation_from_sexp (sexp)
  local parentIs_list = sexp
    and metadata.sexp_cdr_at_path(sexp, { 'skg', 'node', 'parentIs' })
    or nil
  local rels_body = sexp
    and metadata.sexp_cdr_at_path(sexp, { 'skg', 'node', 'rels' })
    or nil
  local independent = parentIs_list ~= nil and parentIs_list[1] ~= nil
    and parentIs_list[1] == sexpr.symbol('independent')
  local graft_role = independent
    and graft_role_from_rels(rels_body)
    or nil
  if graft_role then return graft_role end
  if parentIs_list == nil or #parentIs_list == 0
     or parentIs_list[1] == sexpr.symbol('affected') then
    return 'affected'
  end
  return sexpr.atom_text(parentIs_list[1])
end

---Extract {depth, relation, title} triples for every headline in BUF.
---Mirrors headline-titles.
---@param buf integer
---@return table[]
local function headline_titles (buf)
  local result = {}
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local parts = metadata.split_as_stars_metadata_title(line)
    if parts then
      local depth = #(parts.stars:match('^%*+'))
      local sexp = nil
      if parts.metadata ~= '' then
        local ok, value = pcall(sexpr.read, parts.metadata)
        if ok then sexp = value end
      end
      table.insert(result,
        { depth, relation_from_sexp(sexp), parts.title })
    end
  end
  return result
end

---@param triples table[]
---@return string
local function format_headline_triples (triples)
  local parts = {}
  for _, triple in ipairs(triples) do
    table.insert(parts, string.format('(%d %s %q)',
      triple[1], tostring(triple[2]), triple[3]))
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
      return false
    end
  end
  return true
end

---Assert BUF's headline titles match EXPECTED exactly (a list of
---{depth, relation, title} triples). Mirrors assert-headline-titles.
---@param buf integer
---@param expected table[]
---@param phase_label string
local function assert_headline_titles (buf, expected, phase_label)
  local actual = headline_titles(buf)
  if triples_equal(actual, expected) then
    print(string.format('ok [%s]: headline-titles is (%s)',
      phase_label, format_headline_triples(actual)))
  else
    T.fail(string.format(
      '[%s]: headline-titles mismatch\n  Expected: (%s)\n  Got:      (%s)\n'
      .. '  Buffer content: %s',
      phase_label, format_headline_triples(expected),
      format_headline_triples(actual), T.buffer_text(buf)))
  end
end

-- ── the test phases ─────────────────────────────────────────────────

print('=== SKG Containerward-on-Phantom Integration Test ===')

print('=== PHASE 1: Open view from a ===')
content_view.request_single_root_content_view_from_id('a')
local buf_a = T.wait_for_buffer('skg://a')
T.check(buf_a, "buffer 'skg://a' was created")
-- a -> {b -> c(indef), c}.
assert_headline_titles(buf_a,
  { { 1, 'absent', 'a' },
    { 2, 'affected', 'b' },
    { 3, 'affected', 'c' },
    { 2, 'affected', 'c' } },
  'phase 1: initial view')

print('=== PHASE 2: Remove c from under b and save ===')
vim.api.nvim_set_current_buf(buf_a)
-- Find the indef c under b.
local indef_c_line = nil
for line = 1, vim.api.nvim_buf_line_count(buf_a) do
  if metadata.outline_level(line) == 3 then
    local sexp = metadata.metadata_sexp_at_line_or_nil(line)
    if sexp and metadata.node_indefinitive_p(sexp) then
      indef_c_line = line
      break
    end
  end
end
T.check(indef_c_line ~= nil, 'found indef c under b')
vim.api.nvim_buf_set_lines(buf_a, indef_c_line - 1, indef_c_line, false, {})

-- Buffer should now be: a -> {b, c}
assert_headline_titles(buf_a,
  { { 1, 'absent', 'a' },
    { 2, 'affected', 'b' },
    { 2, 'affected', 'c' } },
  'phase 2: after removing c from b')

save.request_save_buffer()
T.wait_for_response()
print('ok: phase 2: saved')

print('=== PHASE 3: Toggle diff mode ON ===')
vim.api.nvim_set_current_buf(buf_a)
diff_mode.toggle()
T.wait_for_response(20)
-- Verify that a removed-here phantom c appears under b.
local content_after_diff = T.buffer_text(buf_a)
T.check(content_after_diff:find('(unstaged removedM)', 1, true) ~= nil,
        'removed-here phantom present')

print('=== PHASE 4: Note containerward path behavior ===')
-- The old assertion here searched for "content", which matched
-- "contents" in graphStats and did not prove ancestry was present.
-- The broader test-helper cleanup is tracked in
-- TODO/better-than-regex-in-emacs-tests.org.
local final_content = T.buffer_text(buf_a)
T.check(final_content:find('containsParent', 1, true) == nil,
        'no false-positive ancestry assertion')

T.pass('PASS: All phases completed!')
