-- Integration test for the save-lock lifecycle (plan_v2 section
-- 20.2a), nvim client. The Lua mirror of test-emacs.el in this
-- directory.
--
-- Opens THREE overlapping views, edits and saves one, and asserts
-- that the truly-non-collateral view becomes editable once the save
-- stream settles -- i.e. the lock the save took out over every open
-- view was actually released.
--
-- Graph: a contains b; solo is standalone.
--   skg://a    -- the SAVED view.
--   skg://b    -- COLLATERAL to saving a (b's view shows a as
--                 containerward, so its pid set contains a; saving a
--                 re-renders it).
--   skg://solo -- truly NON-COLLATERAL to saving a (its pid set is
--                 {solo}).
--
-- NOTE: this asserts the post-settle end state (every view unlocked,
-- and the non-collateral one genuinely editable). It deliberately
-- does NOT assert the mid-stream timing -- "saved buffer stays locked
-- UNTIL save-result" (section 20.2b) -- which is not observable
-- deterministically from a headless nvim test.
--
-- File system operations (backup/cleanup) are handled by run-test.sh.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local content_view = require('skg.content_view')
local save = require('skg.save')
local lock = require('skg.lock')

local view_bufs = {}

local function phase_1_open_three_views ()
  print('=== PHASE 1: Open three overlapping views ===')
  for _, id in ipairs({ 'a', 'b', 'solo' }) do
    content_view.request_single_root_content_view_from_id(id)
    local buf = T.wait_for_buffer('skg://' .. id)
    if not buf then
      T.fail(string.format('[phase 1]: buffer skg://%s was not'
                           .. ' created', id))
    end
    view_bufs[id] = buf
    print('opened view skg://' .. id)
  end
end

local function phase_2_edit_and_save_a ()
  print('=== PHASE 2: Edit a\'s title and save skg://a ===')
  local buf = view_bufs.a
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  -- end of the root line (a's title): append ' edited'.
  local first_line = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1]
    or ''
  vim.api.nvim_buf_set_lines(buf, 0, 1, false, { first_line .. ' edited' })
  vim.api.nvim_win_set_cursor(0, { 1, #(first_line .. ' edited') })
  print('Buffer skg://a after edit:\n' .. T.buffer_text(buf))
  save.request_save_buffer()
  T.check(T.wait_for_response(), 'save response for a arrived')
end

local function phase_3_verify_locks_released ()
  print('=== PHASE 3: Verify locks released after save ===')

  -- The streaming guard must be clear -- a wedged guard would block
  -- the next op.
  if lock.stream_in_progress then
    T.fail(string.format(
      '[section 20.2a]: lock.stream_in_progress still set after'
      .. ' save: %s', tostring(lock.stream_in_progress)))
  end

  for _, spec in ipairs({
    { name = 'solo', role = 'non-collateral' },
    { name = 'a',    role = 'saved' },
    { name = 'b',    role = 'collateral' },
  }) do
    local buf = view_bufs[spec.name]
    if not buf or not vim.api.nvim_buf_is_valid(buf) then
      T.fail(string.format(
        '[section 20.2a]: buffer skg://%s (%s) no longer exists',
        spec.name, spec.role))
    end
    if vim.b[buf].skg_save_locked then
      T.fail(string.format(
        '[section 20.2a]: %s view skg://%s still save-locked after'
        .. ' the stream settled', spec.role, spec.name))
    end
    print(string.format('%s view skg://%s has no lingering save-lock',
                        spec.role, spec.name))
  end

  -- Prove the non-collateral buffer is genuinely editable: the lock's
  -- 'modifiable' flag is actually cleared, not merely the bookkeeping
  -- variable.
  local solo = view_bufs.solo
  T.check(vim.bo[solo].modifiable == true,
          'skg://solo is modifiable after the save settled')
  local ok, err = pcall(function ()
    vim.api.nvim_buf_set_lines(solo, -1, -1, false,
                               { ';; editable after save' })
  end)
  if not ok then
    T.fail(string.format(
      '[section 20.2a]: editing non-collateral view skg://solo'
      .. ' errored: %s', tostring(err)))
  end
  print('PASS [section 20.2a]: non-collateral view skg://solo is'
       .. ' editable after the save settled')
end

print('=== SKG Save-Lock Lifecycle Integration Test ===')
phase_1_open_three_views()
phase_2_edit_and_save_a()
phase_3_verify_locks_released()

T.pass('PASS: All phases completed successfully!')
