-- Integration test for the fork gesture and its confirmation stage,
-- nvim client. The Lua mirror of test-emacs.el in this directory:
-- open owned P (whose content is foreign N); make N definitive and
-- edit its title; save -> a fork-confirmation buffer appears and
-- nothing commits; approve -> the clone is created (overriding N) and
-- drawn in N's place when P re-renders.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(40)

local content_view = require('skg.content_view')
local metadata = require('skg.metadata')
local save = require('skg.save')

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

print('Starting fork integration test...')

-- 1. Open the owned container P; its content is the foreign node N.
content_view.request_single_root_content_view_from_id('P')
local p_buf = T.wait_for(function () return buffer_showing('P') end, 10)
T.check(p_buf, "P's view never appeared")
vim.api.nvim_set_current_buf(p_buf)
T.check(T.buffer_text(p_buf):find('(id N)', 1, true),
        "P's view does not show its foreign content N")

-- 2. Make N definitive (drop its 'indef' marker) and edit its title
--    -- the fork gesture.
local n_line = goto_line_containing('(id N) (source foreign)',
  "could not find N's headline")
local n_line_text =
  vim.api.nvim_buf_get_lines(p_buf, n_line - 1, n_line, false)[1]
local n_line_edited =
  n_line_text:gsub(' indef', ''):gsub('N%-original', 'N-edited')
vim.api.nvim_buf_set_lines(p_buf, n_line - 1, n_line, false,
                           { n_line_edited })

-- 3. Save -> fork-confirmation (nothing committed).
save.request_save_buffer()

-- 4. The confirmation buffer appears and lists N.
local confirm_buf = T.wait_for_buffer('skg://fork-confirmation', 10)
T.check(confirm_buf, 'no fork-confirmation buffer appeared')
vim.api.nvim_set_current_buf(confirm_buf)
local confirm_text = T.buffer_text(confirm_buf)
T.check(confirm_text:find('(id N)', 1, true),
        'confirmation buffer does not list N')
T.check(confirm_text:find('Fork confirmation', 1, true),
        'confirmation buffer lacks its header')
print('fork-confirmation buffer lists N')

-- 5. Pick the clone's source (required), then approve.
local clone_line = goto_line_starting_with('* (skg (node (source ',
  'could not find the clone-to-be headline')
metadata.change_source_at_line(clone_line, 'owned')
save.approve_fork()

-- 6. The fork commits: when P's saved view re-renders, N is now
--    overridden and subscribed (its graphStats say so). (The saved
--    view still draws N raw -- existing viewnodes are not rewritten;
--    substitution shows on a fresh open, below.)
-- The clone committed: N now has one subscriber and one overrider. N
-- also contains N1,N2 and is contained by P, so its rels carry
-- (contains ...) and (birth contains) too -- match the two
-- commit-signal facts as substrings, not the whole rels form.
local committed = T.wait_for(function ()
  local buf = buffer_showing('P')
  if not buf then return false end
  local s = T.buffer_text(buf)
  return s:find('(subscribes (in 1))', 1, true)
     and s:find('(overrides (in 1))', 1, true)
end, 10)
T.check(committed, 'the fork did not commit after approval')
print('fork committed (N is now overridden and subscribed)')

-- 7. Reopen P fresh: override substitution now draws the clone in N's
--    place, carrying (overridesHere N).
do
  local live_p_buf = buffer_showing('P')
  if live_p_buf then
    vim.api.nvim_buf_delete(live_p_buf, { force = true }) end
end
content_view.request_single_root_content_view_from_id('P')
local substituted = T.wait_for(function ()
  local buf = buffer_showing('P')
  return buf and T.buffer_text(buf):find('(overridesHere N)', 1, true)
end, 10)
T.check(substituted, "the clone was not drawn in N's place on reopen")
print("on reopen, the clone is drawn in N's place with (overridesHere N)")

T.pass('PASS: Fork integration test successful!')
