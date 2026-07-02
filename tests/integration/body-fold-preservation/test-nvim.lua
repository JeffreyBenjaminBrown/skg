-- Integration test: body folding survives a save round-trip, nvim
-- client. The Lua mirror of test-emacs.el in this directory.
--
-- Two scenarios are tested back-to-back, each in its own buffer:
--
-- A) Subtree-fold on a node with body + child: the whole subtree
--    hidden (body + child). After save: body and child should stay
--    hidden.
--
-- B) CHILDREN-state on a node with body + child: the child headline
--    visible, parent body visible, but the child's body hidden.
--    After save: the child's body should still be hidden.
--
-- There is no org-cycle in the nvim client (see skg/folds.lua's
-- module comment on the vim-fold model), so each scenario reaches its
-- BEFORE state directly through the fold-model primitives that
-- describe the same visibility, rather than by emulating repeated
-- org-cycle presses:
--   A) folds.close_fold_at on the root headline closes its whole
--      subtree fold (body + child + child's body), the vim analog of
--      a fully-open root's first fold toggle.
--   B) folds.hide_entry on the child headline closes only its body
--      fold -- the direct stand-in for org-fold-hide-entry, and
--      exactly the net visibility CHILDREN state leaves behind here
--      (root body open, child headline open, child body hidden).
--
-- The assertion compares before/after fold "snapshots". A snapshot is
-- a string with one line per buffer line, prefixed with '[V]'
-- (visible) or '[H]' (hidden), with skg metadata stripped from
-- headlines so the comparison shows only the structure that should
-- round-trip.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(60) -- the elisp test's run-at-time timeout was 60s

local buffer = require('skg.buffer')
local folds = require('skg.folds')
local metadata = require('skg.metadata')
local save = require('skg.save')

---Drop the '(skg ...)' sexp from LINE, keeping '* title' shape.
---@param line string
---@return string
local function bfp_strip_metadata (line)
  local parts = metadata.split_as_stars_metadata_title(line)
  if parts then return parts.stars .. parts.title end
  return line
end

---A multi-line snapshot of BUF: each line prefixed with '[V]' or
---'[H]' for visibility, with skg metadata stripped from headlines.
---@param buf integer
---@return string
local function bfp_snapshot (buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local acc = {}
  for line_number, line in ipairs(lines) do
    local vis = folds.line_invisible_p(line_number) and '[H]' or '[V]'
    table.insert(acc, vis .. ' ' .. bfp_strip_metadata(line) .. '\n')
  end
  return table.concat(acc)
end

---Create a content-view buffer named 'skg://bfp-NAME' with the test
---fixture (a root with a body and one child, the child with its own
---body line).
---@param name string
---@return integer bufnr
local function bfp_build_buffer (name)
  local content = table.concat({
    '* (skg (node (source main))) root',
    'root body line one',
    'root body line two',
    '** (skg (node (source main))) child',
    'child body line',
  }, '\n')
  return buffer.open_org_buffer_from_text(content, 'skg://bfp-' .. name)
end

---Run one save-round-trip scenario. APPLY_FOLDS closes the folds that
---reach the scenario's BEFORE state (see the module comment above).
---EXPECTED_SNAPSHOT_BEFORE is the exact snapshot expected *before*
---save -- a sanity check that the fold operations reach the state we
---think they do. After save we assert the snapshot is unchanged.
---Returns a short diagnostic string on failure, nil on success.
---@param name string
---@param expected_snapshot_before string
---@param apply_folds fun()
---@return string|nil
local function bfp_run_scenario (name, expected_snapshot_before,
                                 apply_folds)
  print('')
  print(string.format('=== Scenario %s ===', name))
  local buf = bfp_build_buffer(name)
  apply_folds()

  local before = bfp_snapshot(buf)
  print(string.format('--- snapshot BEFORE save ---\n%s', before))
  if before ~= expected_snapshot_before then
    T.fail(string.format(
      'Scenario %s: BEFORE snapshot did not match expectation.\n'
      .. 'EXPECTED:\n%s\nACTUAL:\n%s',
      name, expected_snapshot_before, before))
  end

  local ok, err = pcall(save.request_save_buffer)
  if not ok then
    T.fail(string.format('Scenario %s: save raised: %s',
                         name, tostring(err)))
  end
  T.wait_for_response()

  local after = bfp_snapshot(buf)
  print(string.format('--- snapshot AFTER save ---\n%s', after))
  if before == after then
    print(string.format('ok: Scenario %s: fold state preserved', name))
    return nil
  end
  print(string.format(
    'DIFF in scenario %s.\nBEFORE:\n%s\nAFTER:\n%s', name, before, after))
  return string.format('Scenario %s: BEFORE/AFTER differ', name)
end

print('=== SKG Body-Fold-Preservation Integration Test ===')

local diff_a = bfp_run_scenario(
  'A-subtree',
  '[V] * root\n'
  .. '[H] root body line one\n'
  .. '[H] root body line two\n'
  .. '[H] ** child\n'
  .. '[H] child body line\n',
  function () folds.close_fold_at(1) end)

local diff_b = bfp_run_scenario(
  'B-children',
  '[V] * root\n'
  .. '[V] root body line one\n'
  .. '[V] root body line two\n'
  .. '[V] ** child\n'
  .. '[H] child body line\n',
  function () folds.hide_entry(4) end)

local diffs = {}
if diff_a then table.insert(diffs, diff_a) end
if diff_b then table.insert(diffs, diff_b) end

if #diffs > 0 then
  T.fail('Scenarios with broken fold-preservation: '
         .. table.concat(diffs, '; '))
else
  T.pass('PASS: All fold-preservation scenarios passed')
end
