-- Integration test for the EXPLICIT fork gesture
-- (skg.view_requests.fork_node), nvim client. The Lua mirror of
-- test-emacs.el in this directory: open owned container Q (whose
-- content is the owned M):
--  - forking a DIRTY buffer is refused (no confirmation appears);
--  - fork_node on M -> a fork-confirmation buffer; APPROVE -> the
--    clone is created (overrides M) and drawn in M's place on reopen;
--  - on a second node M2, fork_node -> DECLINE strips the lingering
--    (viewRequests fork) atom and commits nothing.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(60)

local content_view = require('skg.content_view')
local metadata = require('skg.metadata')
local save = require('skg.save')
local view_requests = require('skg.view_requests')

---Return a live skg view buffer whose text mentions (id ID).
local function buffer_showing (id)
  local needle = '(id ' .. id .. ')'
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_view_uri ~= nil
       and T.buffer_text(buf):find(needle, 1, true) then
      return buf
    end
  end
  return nil
end

---The live buffer named NAME, or nil. No polling -- for asserting the
---ABSENCE of a buffer that should never appear.
local function find_buffer_named (name)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.api.nvim_buf_get_name(buf) == name then
      return buf
    end
  end
  return nil
end

---Move the cursor in the CURRENT buffer onto the first line containing
---SNIPPET anywhere. Fails the test if no line does.
local function goto_line_containing (snippet, description)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for i, line in ipairs(lines) do
    if line:find(snippet, 1, true) then
      vim.api.nvim_win_set_cursor(0, { i, 0 })
      return i
    end
  end
  T.fail(description or ('could not find a line containing: ' .. snippet))
end

---Move the cursor onto the headline carrying (id ID) (source owned) in
---the current buffer. Fails the test if it is not found.
local function goto_owned_headline (id)
  return goto_line_containing(
    string.format('(id %s) (source owned)', id),
    string.format("could not find %s's headline", id))
end

---Move the cursor in the CURRENT buffer onto the first line that
---STARTS WITH SNIPPET (the analog of an elisp "^..."-anchored search).
---Fails the test if no line does.
local function goto_line_starting_with (snippet, description)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for i, line in ipairs(lines) do
    if line:sub(1, #snippet) == snippet then
      vim.api.nvim_win_set_cursor(0, { i, 0 })
      return i
    end
  end
  T.fail(description or ('could not find a line starting with: ' .. snippet))
end

print('Starting explicit-fork integration test...')

-- 1. Open the owned container Q; its content is the owned node M.
content_view.request_single_root_content_view_from_id('Q')
local q_buf = T.wait_for(function () return buffer_showing('Q') end, 10)
T.check(q_buf, "Q's view never appeared")
vim.api.nvim_set_current_buf(q_buf)
T.check(T.buffer_text(q_buf):find('(id M)', 1, true),
        "Q's view does not show its content M")

-- 2. Forking a DIRTY buffer is refused: no confirmation appears.
vim.bo[q_buf].modified = true
goto_owned_headline('M')
view_requests.fork_node()
T.check(not find_buffer_named('skg://fork-confirmation'),
        'a dirty buffer must not produce a fork confirmation')
print('forking a dirty buffer was refused')
vim.bo[q_buf].modified = false

-- 3. Clean buffer: fork M for real -> fork-confirmation.
goto_owned_headline('M')
view_requests.fork_node()

-- 4. The confirmation buffer appears and lists M; APPROVE it.
local confirm_buf = T.wait_for_buffer('skg://fork-confirmation', 10)
T.check(confirm_buf, 'no fork-confirmation buffer appeared')
vim.api.nvim_set_current_buf(confirm_buf)
T.check(T.buffer_text(confirm_buf):find('(id M)', 1, true),
        'confirmation buffer does not list M')
print('fork_node produced a fork-confirmation listing M')
-- Approving before picking a source is refused.
local approved_early_ok = pcall(save.approve_fork)
T.check(not approved_early_ok,
        'approve must be refused until a source is picked')
print('approve refused until a source is picked')
-- Pick the clone's source, then approve.
local clone_line = goto_line_starting_with('* (skg (node (source ',
  'could not find the clone-to-be headline')
metadata.change_source_at_line(clone_line, 'owned')
save.approve_fork()

-- 5. Reopen Q fresh: override substitution now draws the clone in M's
--    place, carrying (overridesHere M).
do
  local live_q_buf = buffer_showing('Q')
  if live_q_buf then
    vim.api.nvim_buf_delete(live_q_buf, { force = true }) end
end
content_view.request_single_root_content_view_from_id('Q')
local substituted = T.wait_for(function ()
  local buf = buffer_showing('Q')
  return buf and T.buffer_text(buf):find('(overridesHere M)', 1, true)
end, 10)
T.check(substituted, "the clone was not drawn in M's place on reopen")
print("approve: the clone is drawn in M's place with (overridesHere M)")

-- 6. DECLINE on a second node M2 (in Q2): the lingering fork atom is
--    stripped and nothing is committed.
content_view.request_single_root_content_view_from_id('Q2')
local q2_buf = T.wait_for(function () return buffer_showing('Q2') end, 10)
T.check(q2_buf, "Q2's view never appeared")
vim.api.nvim_set_current_buf(q2_buf)
goto_owned_headline('M2')
view_requests.fork_node()

local decline_confirm_buf = T.wait_for_buffer('skg://fork-confirmation', 10)
T.check(decline_confirm_buf, 'no fork-confirmation buffer for M2 appeared')
vim.api.nvim_set_current_buf(decline_confirm_buf)
save.decline_fork()

-- The origin buffer's M2 headline no longer carries a fork request.
T.check(vim.api.nvim_buf_is_valid(q2_buf),
        "Q2's buffer vanished after decline")
T.check(not T.buffer_text(q2_buf):find('(viewRequests', 1, true),
        'decline must strip the (viewRequests fork) atom')
print('decline stripped the lingering fork atom')

-- Reopen Q2 fresh: no clone overrides M2 (decline committed nothing).
vim.api.nvim_buf_delete(q2_buf, { force = true })
content_view.request_single_root_content_view_from_id('Q2')
q2_buf = T.wait_for(function () return buffer_showing('Q2') end, 10)
T.check(q2_buf, "Q2's view never reappeared")
-- Give the view a beat, then assert no substitution happened.
T.wait_for(function () return false end, 1)
T.check(not T.buffer_text(q2_buf):find('(overridesHere M2)', 1, true),
        'decline must not commit a clone overriding M2')
print('decline committed nothing (no clone overrides M2)')

-- 7. KILLING the confirmation buffer directly (not via approve/decline)
--    must also strip the lingering fork atom from the origin.
content_view.request_single_root_content_view_from_id('Q3')
local q3_buf = T.wait_for(function () return buffer_showing('Q3') end, 10)
T.check(q3_buf, "Q3's view never appeared")
vim.api.nvim_set_current_buf(q3_buf)
goto_owned_headline('M3')
view_requests.fork_node()

-- The M2 decline left a confirmation buffer open (it reuses one name),
-- so wait until it actually shows M3 before dismissing it.
local m3_confirm_buf = T.wait_for(function ()
  local buf = find_buffer_named('skg://fork-confirmation')
  return buf and T.buffer_text(buf):find('(id M3)', 1, true) and buf
end, 10)
T.check(m3_confirm_buf, 'no fork-confirmation for M3 appeared')
-- Dismiss by killing the buffer directly -- no approve, no decline.
vim.api.nvim_buf_delete(m3_confirm_buf, { force = true })

T.check(vim.api.nvim_buf_is_valid(q3_buf), "Q3's buffer vanished after kill")
T.check(not T.buffer_text(q3_buf):find('(viewRequests', 1, true),
        'killing the confirmation must strip the fork atom')
print('killing the confirmation buffer stripped the fork atom')

T.pass('PASS: Explicit-fork integration test successful!')
