-- Integration test: save a single-root content view that is entirely
-- folded (the nvim analog of org-startup-folded=t / overview mode),
-- point on root. Verifies the root line survives the save round-trip.
-- The Lua mirror of test-emacs.el in this directory.
--
-- DEVIATION: there is no org-startup-folded here; vim folds are
-- per-window state set up explicitly. 'zM' after buffer construction
-- closes every fold (the analog of overview mode): each headline's
-- own line stays visible (vim always shows a closed fold's first
-- line), but everything nested under the root -- its body and every
-- child -- is hidden, exactly mirroring what org-startup-folded=t
-- produces for a single top-level headline.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

print('=== SKG Root-Folded-Save Integration Test ===')

print('=== PHASE 1: request single-root view from server ===')
-- Mirror what the elisp test does: build the buffer directly with the
-- text the server would return for the initial view (rather than
-- going through the async request/response dispatcher), so the flow
-- stays deterministic; then activate the skg fold model so the
-- overview-fold step below takes effect exactly as in the real flow.
local org_text = table.concat({
  '* (skg (node (id rfs-root) (source main))) rfs-root',
  'rfs-root body',
  '** (skg (node (id rfs-c1) (source main))) rfs-c1',
  'rfs-c1 body',
  '** (skg (node (id rfs-c2) (source main))) rfs-c2',
  'rfs-c2 body',
  '*** (skg (node (id rfs-g1) (source main))) rfs-g1',
  'rfs-g1 body',
  '** (skg (node (id rfs-c3) (source main))) rfs-c3',
  'rfs-c3 body',
}, '\n')

local buffer = require('skg.buffer')
local metadata = require('skg.metadata')
local folds = require('skg.folds')

local view_uri = buffer.generate_uuid()
local view = buffer.open_org_buffer_from_text(
  org_text, 'skg://skg-root-folded-save', view_uri)
vim.api.nvim_win_set_cursor(0, { 1, 0 })

-- Close every fold: the nvim analog of org-startup-folded=t/overview.
vim.cmd('silent! normal! zx') -- force the fold expr to materialize
vim.cmd('silent! normal! zM') -- close all folds

print('=== visibility after fold setup (org-startup-folded=t analog) ===')
do
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.at_heading_p(line) then
      print(string.format('  %s | invisible: %s',
        metadata.line_text(line), tostring(folds.line_invisible_p(line))))
    end
  end
end

print('=== PHASE 2: save buffer ===')
local ok, err = pcall(require('skg.save').request_save_buffer)
if not ok then
  print('CAUGHT ERROR during save: ' .. tostring(err))
  T.fail('skg.save.request_save_buffer raised: ' .. tostring(err))
end

T.check(T.wait_for_response(), 'save response arrived')

print('=== PHASE 3: result ===')
local result = T.buffer_text(view)
local first_line = vim.api.nvim_buf_get_lines(view, 0, 1, false)[1] or ''
print(result)
print('First line: ' .. first_line)

T.check(result:find('(id rfs-root)', 1, true) ~= nil,
        'Result kept the root headline')
T.check(first_line:match('^%* ') ~= nil,
        'First line is a level-1 headline: ' .. first_line)

T.pass('PASS: root survived save round-trip')
