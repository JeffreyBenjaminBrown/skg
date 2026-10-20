-- Coverage for skg.readiness (the commit-readiness gestures) and the
-- readiness summary a save of a diff-view buffer shows.
-- Mirrors tests/elisp/test-skg-readiness.el.

local picker = require('skg.picker')
local readiness = require('skg.readiness')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')

---Fill a buffer with LINES, run COMMAND with the cursor on line
---LINE_NUMBER, and return the buffer's lines. The save is stubbed;
---it must be called once, or never when ANSWER is 'no'. OBJECT answers
---the prompt for a member's object, and ANSWER the question before a
---wide unmark; without them, those must not be asked. A rewritten
---headline without a title ends in a space, as format_headline
---writes it.
local function run (lines, line_number, command, object, answer)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { line_number, 0 })
  local saves = 0
  local original_save = save.request_save_buffer
  local original_read = picker.completing_read_with_cycle
  local original_input = vim.fn.input
  save.request_save_buffer = function () saves = saves + 1 end
  picker.completing_read_with_cycle = function ()
    assert.is_not_nil(object, 'the object prompt must not be shown')
    return object
  end
  vim.fn.input = function (prompt)
    assert.is_not_nil(answer, 'the wide-unmark question must not be asked')
    assert.are.equal(readiness.WIDE_UNMARK_QUESTION, prompt)
    return answer
  end
  local ok, err = pcall(command)
  save.request_save_buffer = original_save
  picker.completing_read_with_cycle = original_read
  vim.fn.input = original_input
  assert(ok, err)
  assert.are.equal(answer == 'no' and 0 or 1, saves)
  local result = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  vim.api.nvim_buf_delete(buf, { force = true })
  return result
end

describe('skg.readiness', function ()
  it('toggles a leaf', function ()
    assert.are.same(
      { '** (skg removedAlias (diff rel-) (leaves 1) (readyRequest)) a1' },
      run({ '** (skg removedAlias (diff rel-) (leaves 1)) a1' }, 1,
          readiness.cycle_commit_ready))
    assert.are.same(
      { '** (skg (oldText title) (ready) (leaves 1) (unreadyRequest)) txt' },
      run({ '** (skg (oldText title) (ready) (leaves 1)) txt' }, 1,
          readiness.cycle_commit_ready))
    -- Without the server's '(leaves ...)' fact, nothing is named.
    assert.are.same({}, (function ()
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                                 { '** (skg (oldText title)) txt' })
      vim.api.nvim_set_current_buf(buf)
      local requests = readiness.requests_at_line(1)
      vim.api.nvim_buf_delete(buf, { force = true })
      return requests
    end)())
  end)

  it('asks which object of a member, unless only one applies', function ()
    assert.are.same(
      { '* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel) (leaves 2 node rel) (unreadyRequest rel))) k' },
      run({ '* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel) (leaves 2 node rel))) k' },
          1, readiness.cycle_commit_ready, 'rel'))
    assert.are.same(
      { '* (skg (historical (id d) (repo r) (diff node-) (leaves 1 node) (readyRequest node))) d' },
      run({ '* (skg (historical (id d) (repo r) (diff node-) (leaves 1 node))) d' },
          1, readiness.cycle_commit_ready))
  end)

  it('writes one level down on an aggregate', function ()
    assert.are.same({
      '* (skg (node (id ali) (repo r) (leaves 2 all))) ali',
      '** (skg (aliasFolder changed))',
      '*** (skg removedAlias (diff rel-) (ready) (leaves 1) (readyRequest)) a1',
      '*** (skg alias) a2',
      '*** (skg alias (diff rel+) (leaves 1) (readyRequest)) a3' },
      run({ '* (skg (node (id ali) (repo r) (leaves 2 all))) ali',
            '** (skg (aliasFolder changed))',
            '*** (skg removedAlias (diff rel-) (ready) (leaves 1)) a1',
            '*** (skg alias) a2',
            '*** (skg alias (diff rel+) (leaves 1)) a3' },
          2, readiness.cycle_commit_ready))
    assert.are.same({
      '* (skg (node (id txt) (repo r) (leaves 1 all) (readyRequest all))) txt',
      '** (skg (oldText title) (leaves 1)) txt' },
      run({ '* (skg (node (id txt) (repo r) (leaves 1 all))) txt',
            '** (skg (oldText title) (leaves 1)) txt' },
          1, readiness.cycle_commit_ready))
  end)

  it('recursively touches only the leaves', function ()
    -- Below the headline at point, no 'all': the changeset marker's
    -- leaves are its siblings', and so are untouched there.
    assert.are.same({
      '* (skg (node (id r) (repo r) (leaves 3 all) (readyRequest all))) r',
      '** (skg (changesetMarker content) (leaves 2 all))',
      '** (skg (historical (id d) (repo r) (diff node- rel-) (leaves 2 node rel) (readyRequest node rel))) d',
      '*** (skg (node (id k) (repo r) writeProtected)) k',
      '** (skg (node (id c) (repo r) writeProtected (diff rel+) (leaves 1 rel) (readyRequest rel))) c',
      '* (skg (node (id other) (repo r) (leaves 1 all))) other',
      '** (skg (oldText title) (leaves 1)) other' },
      run({ '* (skg (node (id r) (repo r) (leaves 3 all))) r',
            '** (skg (changesetMarker content) (leaves 2 all))',
            '** (skg (historical (id d) (repo r) (diff node- rel-) (leaves 2 node rel))) d',
            '*** (skg (node (id k) (repo r) writeProtected)) k',
            '** (skg (node (id c) (repo r) writeProtected (diff rel+) (leaves 1 rel))) c',
            '* (skg (node (id other) (repo r) (leaves 1 all))) other',
            '** (skg (oldText title) (leaves 1)) other' },
          1, readiness.cycle_commit_ready_recursive))
    -- At point, a changeset marker names its aggregate.
    assert.are.same(
      { '** (skg (changesetMarker content) (leaves 2 all) (readyRequest all)) ' },
      run({ '** (skg (changesetMarker content) (leaves 2 all))' },
          1, readiness.cycle_commit_ready_recursive))
  end)

  it('marks home moves, recursively only those like the first', function ()
    local lines = {
      '* (skg (node (id a) (repo b) (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (leaves 1))',
      '** (skg (node (id c) (repo b) writeProtected (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (leaves 1))',
      '** (skg (node (id e) (repo b) (leaves 1 all))) e',
      '*** (skg (homeSkgrepoMove (old y) (new b)) (leaves 1))' }
    assert.are.same({
      '* (skg (node (id a) (repo b) (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (leaves 1) (readyRequest)) ',
      '** (skg (node (id c) (repo b) writeProtected (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (leaves 1))',
      '** (skg (node (id e) (repo b) (leaves 1 all))) e',
      '*** (skg (homeSkgrepoMove (old y) (new b)) (leaves 1))' },
      run(lines, 1, readiness.toggle_homeSkgrepo_move_commitReady))
    assert.are.same({
      '* (skg (node (id a) (repo b) (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (leaves 1) (readyRequest)) ',
      '** (skg (node (id c) (repo b) writeProtected (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (leaves 1) (readyRequest)) ',
      '** (skg (node (id e) (repo b) (leaves 1 all))) e',
      '*** (skg (homeSkgrepoMove (old y) (new b)) (leaves 1))' },
      run(lines, 1,
          readiness.toggle_homeSkgrepo_move_commitReady_recursive))
  end)

  it('asks nothing before a single-leaf unmark', function ()
    assert.are.same({
      '** (skg (aliasFolder changed) (ready))',
      '*** (skg removedAlias (diff rel-) (ready) (leaves 1) (unreadyRequest)) a1',
      '*** (skg alias) a2' },
      run({ '** (skg (aliasFolder changed) (ready))',
            '*** (skg removedAlias (diff rel-) (ready) (leaves 1)) a1',
            '*** (skg alias) a2' },
          1, readiness.cycle_commit_ready))
    -- A changed node whose only leaf is ready: one leaf, as the server
    -- counts, so nothing is asked.
    assert.are.same({
      '* (skg (node (id txt) (repo r) (leaves 1 all) (unreadyRequest all))) txt',
      '** (skg (oldText title) (ready) (leaves 1)) txt' },
      run({ '* (skg (node (id txt) (repo r) (leaves 1 all))) txt',
            '** (skg (oldText title) (ready) (leaves 1)) txt' },
          1, readiness.cycle_commit_ready))
  end)

  it('asks before a wide unmark, and writes nothing on refusal', function ()
    local lines = {
      '* (skg (historical (id d) (repo r) (diff node- rel-) (ready node rel) (leaves 2 node rel))) d',
      '** (skg (oldText title) (ready) (leaves 1)) d' }
    assert.are.same(lines,
      run(lines, 1, readiness.cycle_commit_ready_recursive, nil, 'no'))
    assert.are.same({
      '* (skg (historical (id d) (repo r) (diff node- rel-) (ready node rel) (leaves 2 node rel) (unreadyRequest node rel))) d',
      '** (skg (oldText title) (ready) (leaves 1) (unreadyRequest)) d' },
      run(lines, 1, readiness.cycle_commit_ready_recursive, nil, 'yes'))
    -- Unmarking a changed node unmarks every leaf it records: two here.
    local node_lines = {
      '* (skg (node (id txt) (repo r) (leaves 2 all))) txt',
      '** (skg (oldText title) (ready) (leaves 1)) txt',
      '** (skg (homeSkgrepoMove (old x) (new r)) (ready) (leaves 1))' }
    assert.are.same(node_lines,
      run(node_lines, 1, readiness.cycle_commit_ready, nil, 'no'))
  end)

  it("warns before a wide unmark by the server's count", function ()
    -- A ready changeset marker over three leaves: asked first.
    local lines = { '** (skg (changesetMarker content) (ready) (leaves 3 all))' }
    assert.are.same(lines,
      run(lines, 1, readiness.cycle_commit_ready, nil, 'no'))
    -- A ready single-leaf headline: nothing is asked.
    assert.are.same(
      { '** (skg (oldText title) (ready) (leaves 1) (unreadyRequest)) txt' },
      run({ '** (skg (oldText title) (ready) (leaves 1)) txt' }, 1,
          readiness.cycle_commit_ready))
  end)

  it('unmarks a ready home move, asking only for several', function ()
    local lines = {
      '* (skg (node (id a) (repo b) (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (ready) (leaves 1))',
      '** (skg (node (id c) (repo b) writeProtected (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (ready) (leaves 1))' }
    for _, line_number in ipairs({ 1, 2 }) do
      assert.are.same({
        '* (skg (node (id a) (repo b) (leaves 1 all))) a',
        '** (skg (homeSkgrepoMove (old x) (new b)) (ready) (leaves 1) (unreadyRequest)) ',
        '** (skg (node (id c) (repo b) writeProtected (leaves 1 all))) c',
        '*** (skg (homeSkgrepoMove (old x) (new b)) (ready) (leaves 1))' },
        run(lines, line_number,
            readiness.toggle_homeSkgrepo_move_commitReady))
    end
    assert.are.same({
      '* (skg (node (id a) (repo b) (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (ready) (leaves 1) (unreadyRequest)) ',
      '** (skg (node (id c) (repo b) writeProtected (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (ready) (leaves 1) (unreadyRequest)) ' },
      run(lines, 1,
          readiness.toggle_homeSkgrepo_move_commitReady_recursive,
          nil, 'yes'))
  end)

  it('walks exactly the headlines one level below the line', function ()
    -- Mirrors test-skg-readiness-do-children-visits-exactly-one-level-below-point.
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
      '* (skg (node (id before))) before',
      '* (skg (node (id parent))) parent',
      'body of parent',
      '** (skg (node (id child1))) child1',
      '*** (skg (node (id grandchild))) grandchild',
      '** (skg (node (id child2))) child2',
      '* (skg (node (id after))) after',
      '** (skg (node (id nephew))) nephew' })
    vim.api.nvim_set_current_buf(buf)
    local lines = readiness.child_lines(2)
    vim.api.nvim_buf_delete(buf, { force = true })
    assert.are.same({ 4, 6 }, lines)
  end)
end)

describe('skg.save readiness summary', function ()
  it('is shown after a save of a diff-view buffer', function ()
    local shown = {}
    local original_notify = vim.notify
    vim.notify = function (message) table.insert(shown, message) end
    local buf = vim.api.nvim_create_buf(true, false)
    local ok, err = pcall(function ()
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg (node (id a))) a\n") (errors ()) (warnings ())'
        .. ' (readiness-summary (ready 12) (changed 14) (foreign 2)'
        .. ' (order-only 1) (notEditable 3)))'))
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg (node (id a))) a\n") (errors ()) (warnings ())'
        .. ' (readiness-summary (ready 14) (changed 14) (foreign 0)'
        .. ' (order-only 0) (notEditable 0)))'))
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg (node (id a))) a\n") (errors ()) (warnings ()))'))
    end)
    vim.notify = original_notify
    vim.api.nvim_buf_delete(buf, { force = true })
    assert(ok, err)
    assert.are.same({
      '12 of 14 changed files are ready, 3 changes are not editable in this view; <localleader>ru integrates them, 2 files changed in foreign skgrepos',
      'COMMIT READY. All 14 of 14 changed files are ready.' },
      vim.tbl_filter(function (message)
        return message:find('changed files', 1, true) ~= nil
      end, shown))
  end)
end)
