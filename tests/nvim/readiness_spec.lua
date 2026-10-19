-- Coverage for skg.readiness (the commit-readiness gestures) and the
-- readiness summary a save of a total-diff-view buffer shows.
-- Mirrors tests/elisp/test-skg-readiness.el.

local picker = require('skg.picker')
local readiness = require('skg.readiness')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')

---Fill a buffer with LINES, run COMMAND with the cursor on line
---LINE_NUMBER, and return the buffer's lines. The save is stubbed;
---it must be called once. AXIS answers the prompt for a member's
---axis; without it, the prompt must not be shown. A rewritten
---headline without a title ends in a space, as format_headline
---writes it.
local function run (lines, line_number, command, axis)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { line_number, 0 })
  local saves = 0
  local original_save = save.request_save_buffer
  local original_read = picker.completing_read_with_cycle
  save.request_save_buffer = function () saves = saves + 1 end
  picker.completing_read_with_cycle = function ()
    assert.is_not_nil(axis, 'the axis prompt must not be shown')
    return axis
  end
  local ok, err = pcall(command)
  save.request_save_buffer = original_save
  picker.completing_read_with_cycle = original_read
  assert(ok, err)
  assert.are.equal(1, saves)
  local result = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  vim.api.nvim_buf_delete(buf, { force = true })
  return result
end

describe('skg.readiness', function ()
  it('toggles a leaf', function ()
    assert.are.same(
      { '** (skg removedAlias (diff rel-) (readyRequest)) a1' },
      run({ '** (skg removedAlias (diff rel-)) a1' }, 1,
          readiness.cycle_commit_ready))
    assert.are.same(
      { '** (skg (oldText title) (ready) (unreadyRequest)) txt' },
      run({ '** (skg (oldText title) (ready)) txt' }, 1,
          readiness.cycle_commit_ready))
  end)

  it('asks which axis of a member, unless only one applies', function ()
    assert.are.same(
      { '* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel) (unreadyRequest rel))) k' },
      run({ '* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel))) k' },
          1, readiness.cycle_commit_ready, 'rel'))
    assert.are.same(
      { '* (skg (historical (id d) (repo r) (diff node-) (readyRequest node))) d' },
      run({ '* (skg (historical (id d) (repo r) (diff node-))) d' },
          1, readiness.cycle_commit_ready))
  end)

  it('writes one level down on an aggregate', function ()
    assert.are.same({
      '* (skg (node (id ali) (repo r))) ali',
      '** (skg (aliasFolder changed))',
      '*** (skg removedAlias (diff rel-) (ready) (readyRequest)) a1',
      '*** (skg alias) a2',
      '*** (skg alias (diff rel+) (readyRequest)) a3' },
      run({ '* (skg (node (id ali) (repo r))) ali',
            '** (skg (aliasFolder changed))',
            '*** (skg removedAlias (diff rel-) (ready)) a1',
            '*** (skg alias) a2',
            '*** (skg alias (diff rel+)) a3' },
          2, readiness.cycle_commit_ready))
    assert.are.same({
      '* (skg (node (id txt) (repo r) (readyRequest all))) txt',
      '** (skg (oldText title)) txt' },
      run({ '* (skg (node (id txt) (repo r))) txt',
            '** (skg (oldText title)) txt' },
          1, readiness.cycle_commit_ready))
  end)

  it('recursively touches only the leaves', function ()
    assert.are.same({
      '* (skg (node (id r) (repo r) (readyRequest all))) r',
      '** (skg (changesetMarker content) (readyRequest)) ',
      '** (skg (historical (id d) (repo r) (diff node- rel-) (readyRequest node rel))) d',
      '*** (skg (node (id k) (repo r) writeProtected)) k',
      '** (skg (node (id c) (repo r) writeProtected (diff rel+) (readyRequest rel))) c',
      '* (skg (node (id other) (repo r))) other',
      '** (skg (oldText title)) other' },
      run({ '* (skg (node (id r) (repo r))) r',
            '** (skg (changesetMarker content))',
            '** (skg (historical (id d) (repo r) (diff node- rel-))) d',
            '*** (skg (node (id k) (repo r) writeProtected)) k',
            '** (skg (node (id c) (repo r) writeProtected (diff rel+))) c',
            '* (skg (node (id other) (repo r))) other',
            '** (skg (oldText title)) other' },
          1, readiness.cycle_commit_ready_recursive))
  end)

  it('marks home moves, recursively only those like the first', function ()
    local lines = {
      '* (skg (node (id a) (repo b))) a',
      '** (skg (homeMove (old x) (new b)))',
      '** (skg (node (id c) (repo b) writeProtected)) c',
      '*** (skg (homeMove (old x) (new b)))',
      '** (skg (node (id e) (repo b))) e',
      '*** (skg (homeMove (old y) (new b)))' }
    assert.are.same({
      '* (skg (node (id a) (repo b))) a',
      '** (skg (homeMove (old x) (new b)) (readyRequest)) ',
      '** (skg (node (id c) (repo b) writeProtected)) c',
      '*** (skg (homeMove (old x) (new b)))',
      '** (skg (node (id e) (repo b))) e',
      '*** (skg (homeMove (old y) (new b)))' },
      run(lines, 1, readiness.mark_gnode_skgrepo_changes_commitReady))
    assert.are.same({
      '* (skg (node (id a) (repo b))) a',
      '** (skg (homeMove (old x) (new b)) (readyRequest)) ',
      '** (skg (node (id c) (repo b) writeProtected)) c',
      '*** (skg (homeMove (old x) (new b)) (readyRequest)) ',
      '** (skg (node (id e) (repo b))) e',
      '*** (skg (homeMove (old y) (new b)))' },
      run(lines, 1,
          readiness.mark_gnode_skgrepo_changes_commitReady_recursive))
  end)
end)

describe('skg.save readiness summary', function ()
  it('is shown after a save of a total-diff-view buffer', function ()
    local shown = {}
    local original_notify = vim.notify
    vim.notify = function (message) table.insert(shown, message) end
    local buf = vim.api.nvim_create_buf(true, false)
    local ok, err = pcall(function ()
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg (node (id a))) a\n") (errors ()) (warnings ())'
        .. ' (readiness-summary (ready 12) (changed 14) (foreign 2)'
        .. ' (order-only 1) (unshown 3)))'))
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg (node (id a))) a\n") (errors ()) (warnings ())'
        .. ' (readiness-summary (ready 14) (changed 14) (foreign 0)'
        .. ' (order-only 0) (unshown 0)))'))
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg (node (id a))) a\n") (errors ()) (warnings ()))'))
    end)
    vim.notify = original_notify
    vim.api.nvim_buf_delete(buf, { force = true })
    assert(ok, err)
    assert.are.same({
      '12 of 14 changed files are ready, 3 changes are not shown in this view; reopen it, 2 files changed in foreign skgrepos',
      'COMMIT READY. All 14 of 14 changed files are ready.' },
      vim.tbl_filter(function (message)
        return message:find('changed files', 1, true) ~= nil
      end, shown))
  end)
end)
