-- Coverage for skg.readiness (the commit-readiness gestures) and the
-- readiness summary a save of a diff-view buffer shows.
-- Mirrors tests/elisp/test-skg-readiness.el.

local picker = require('skg.picker')
local readiness = require('skg.readiness')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')

---Fill a buffer with LINES, run COMMAND with the cursor on line
---LINE_NUMBER, and return the buffer's lines. The save is stubbed;
---it must be called once, or never when ANSWER is 'no'. ANSWER
---answers the question before a wide unmark; without it, that must
---not be asked. No gesture prompts for anything else. A rewritten
---headline without a title ends in a space, as format_headline
---writes it.
local function run (lines, line_number, command, answer)
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
    error('a gesture asked which object')
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
      { '** (skg alias noClaim (editRequest (ready)) (view ghostWarrant) (diff (rel removed) (leaves 1))) a1' },
      run({ '** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1' }, 1,
          readiness.cycle_commit_ready))
    assert.are.same(
      { '** (skg (oldText title) (editRequest (unready)) (diff (leaves 1) (ready))) txt' },
      run({ '** (skg (oldText title) (diff (leaves 1) (ready))) txt' }, 1,
          readiness.cycle_commit_ready))
    -- Without the server's '(diff (leaves ...))' fact, nothing is named.
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

  it('writes the request as the last part of the diff form', function ()
    -- Two levels under 'skg' on a node; a headline without the form
    -- gets one.
    local function write (line, ready, objects)
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, { line })
      vim.api.nvim_set_current_buf(buf)
      readiness.write_request(1, ready, objects)
      local result = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1]
      vim.api.nvim_buf_delete(buf, { force = true })
      return result
    end
    assert.are.equal(
      '* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (unready rel)) (diff (rel added) (leaves 1 rel))) k',
      write('* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (ready rel)) (diff (rel added) (leaves 1 rel))) k',
            false, { 'rel' }))
    assert.are.equal(
      '** (skg alias (editRequest (ready)) (diff (rel added) (leaves 1))) a',
      write('** (skg alias (diff (rel added) (leaves 1))) a', true, {}))
    assert.are.equal(
      '** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (ready))) ',
      write('** (skg (homeSkgrepoMove (old x) (new b))) ', true, {}))
  end)

  it('names every listed object, without asking', function ()
    -- A new member lists two objects, one of them ready: the gesture
    -- names both and marks them.
    assert.are.same(
      { '* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (ready node rel)) (diff (node added) (rel added) (leaves 2 node rel) (ready rel))) k' },
      run({ '* (skg node (gnid k) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel) (ready rel))) k' },
          1, readiness.cycle_commit_ready))
    -- A modified node in a new relationship: one gesture marks its
    -- file and the relationship.
    assert.are.same(
      { '* (skg node (gnid m) (skgRepos (nodeRepo r)) (editRequest (ready rel all)) (diff (rel added) (properties changed) (leaves 3 rel all) (ready rel))) m' },
      run({ '* (skg node (gnid m) (skgRepos (nodeRepo r)) (diff (rel added) (properties changed) (leaves 3 rel all) (ready rel))) m' },
          1, readiness.cycle_commit_ready))
    assert.are.same(
      { '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (ready node)) (diff (node removed) (leaves 1 node))) d' },
      run({ '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (leaves 1 node))) d' },
          1, readiness.cycle_commit_ready))
  end)

  it("toggles a modified node by its own '(ready ...)'", function ()
    -- '(ready all)': ready, so the gesture unmarks.
    assert.are.same({
      '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (unready all)) (diff (properties changed) (leaves 1 all) (ready all))) txt',
      '** (skg (oldText title) (diff (leaves 1) (ready))) txt' },
      run({ '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 all) (ready all))) txt',
            '** (skg (oldText title) (diff (leaves 1) (ready))) txt' },
          1, readiness.cycle_commit_ready))
    -- Without it, unready, whatever its children say: the gesture marks.
    assert.are.same({
      '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (ready all)) (diff (properties changed) (leaves 1 all))) txt',
      '** (skg (oldText title) (diff (leaves 1) (ready))) txt' },
      run({ '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 all))) txt',
            '** (skg (oldText title) (diff (leaves 1) (ready))) txt' },
          1, readiness.cycle_commit_ready))
    -- A '(ready rel)' does not make 'all' ready.
    assert.are.same(
      { '* (skg node (gnid m) (skgRepos (nodeRepo r)) (editRequest (ready rel all)) (diff (rel added) (properties changed) (leaves 2 rel all) (ready rel))) m' },
      run({ '* (skg node (gnid m) (skgRepos (nodeRepo r)) (diff (rel added) (properties changed) (leaves 2 rel all) (ready rel))) m' },
          1, readiness.cycle_commit_ready))
  end)

  it('writes one level down on a commit readiness aggregate', function ()
    assert.are.same({
      '* (skg node (gnid ali) (skgRepos (nodeRepo r)) (diff (leaves 2 all))) ali',
      '** (skg aliasFolder)',
      '*** (skg alias noClaim (editRequest (ready)) (view ghostWarrant) (diff (rel removed) (leaves 1) (ready))) a1',
      '*** (skg alias) a2',
      '*** (skg alias (editRequest (ready)) (diff (rel added) (leaves 1))) a3' },
      run({ '* (skg node (gnid ali) (skgRepos (nodeRepo r)) (diff (leaves 2 all))) ali',
            '** (skg aliasFolder)',
            '*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (ready))) a1',
            '*** (skg alias) a2',
            '*** (skg alias (diff (rel added) (leaves 1))) a3' },
          2, readiness.cycle_commit_ready))
    assert.are.same({
      '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (ready all)) (diff (leaves 1 all))) txt',
      '** (skg (oldText title) (diff (leaves 1))) txt' },
      run({ '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (leaves 1 all))) txt',
            '** (skg (oldText title) (diff (leaves 1))) txt' },
          1, readiness.cycle_commit_ready))
  end)

  it('recursively touches only the leaves', function ()
    -- Below the headline at point, no 'all': the changeset marker's
    -- leaves are its siblings', and so are untouched there, and of
    -- the modified m only the relationship is named.
    assert.are.same({
      '* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (ready all)) (diff (leaves 3 all))) r',
      '** (skg (changesetMarker content) (diff (leaves 2 all)))',
      '** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (ready node rel)) (diff (node removed) (rel removed) (leaves 2 node rel))) d',
      '*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k',
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (editRequest (ready rel)) (diff (rel added) (leaves 1 rel))) c',
      '** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (ready rel)) (diff (rel added) (properties changed) (leaves 2 rel all))) m',
      '* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 all))) other',
      '** (skg (oldText title) (diff (leaves 1))) other' },
      run({ '* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (leaves 3 all))) r',
            '** (skg (changesetMarker content) (diff (leaves 2 all)))',
            '** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel))) d',
            '*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k',
            '** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (leaves 1 rel))) c',
            '** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (properties changed) (leaves 2 rel all))) m',
            '* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 all))) other',
            '** (skg (oldText title) (diff (leaves 1))) other' },
          1, readiness.cycle_commit_ready_recursive))
    -- At point, a changeset marker names its commit readiness aggregate.
    assert.are.same(
      { '** (skg (changesetMarker content) (editRequest (ready all)) (diff (leaves 2 all))) ' },
      run({ '** (skg (changesetMarker content) (diff (leaves 2 all)))' },
          1, readiness.cycle_commit_ready_recursive))
  end)

  it('marks home skgrepo moves, recursively only those like the first', function ()
    local lines = {
      '* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))',
      '** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))',
      '** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) e',
      '*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))' }
    assert.are.same({
      '* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (ready)) (diff (leaves 1))) ',
      '** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))',
      '** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) e',
      '*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))' },
      run(lines, 1, readiness.toggle_homeSkgrepo_move_commitReady))
    assert.are.same({
      '* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (ready)) (diff (leaves 1))) ',
      '** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (ready)) (diff (leaves 1))) ',
      '** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) e',
      '*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))' },
      run(lines, 1,
          readiness.toggle_homeSkgrepo_move_commitReady_recursive))
  end)

  it('asks nothing before a single-leaf unmark', function ()
    assert.are.same({
      '** (skg aliasFolder)',
      '*** (skg alias noClaim (editRequest (unready)) (view ghostWarrant) (diff (rel removed) (leaves 1) (ready))) a1',
      '*** (skg alias) a2' },
      run({ '** (skg aliasFolder)',
            '*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (ready))) a1',
            '*** (skg alias) a2' },
          1, readiness.cycle_commit_ready))
    -- A modified node whose only leaf is ready: one leaf, as the
    -- server counts, so nothing is asked.
    assert.are.same({
      '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (unready all)) (diff (properties changed) (leaves 1 all) (ready all))) txt',
      '** (skg (oldText title) (diff (leaves 1) (ready))) txt' },
      run({ '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 all) (ready all))) txt',
            '** (skg (oldText title) (diff (leaves 1) (ready))) txt' },
          1, readiness.cycle_commit_ready))
  end)

  it('asks before a wide unmark, and writes nothing on refusal', function ()
    local lines = {
      '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel) (ready node rel))) d',
      '** (skg (oldText title) (diff (leaves 1) (ready))) d' }
    assert.are.same(lines,
      run(lines, 1, readiness.cycle_commit_ready_recursive, 'no'))
    assert.are.same({
      '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (unready node rel)) (diff (node removed) (rel removed) (leaves 2 node rel) (ready node rel))) d',
      '** (skg (oldText title) (editRequest (unready)) (diff (leaves 1) (ready))) d' },
      run(lines, 1, readiness.cycle_commit_ready_recursive, 'yes'))
    -- Unmarking a modified node unmarks every leaf it records: two here.
    local node_lines = {
      '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 2 all) (ready all))) txt',
      '** (skg (oldText title) (diff (leaves 1) (ready))) txt',
      '** (skg (homeSkgrepoMove (old x) (new r)) (diff (leaves 1) (ready)))' }
    assert.are.same(node_lines,
      run(node_lines, 1, readiness.cycle_commit_ready, 'no'))
  end)

  it("warns before a wide unmark by the server's count", function ()
    -- A ready changeset marker over three leaves: asked first.
    local lines = { '** (skg (changesetMarker content) (diff (leaves 3 all) (ready all)))' }
    assert.are.same(lines,
      run(lines, 1, readiness.cycle_commit_ready, 'no'))
    -- A ready single-leaf headline: nothing is asked.
    assert.are.same(
      { '** (skg (oldText title) (editRequest (unready)) (diff (leaves 1) (ready))) txt' },
      run({ '** (skg (oldText title) (diff (leaves 1) (ready))) txt' }, 1,
          readiness.cycle_commit_ready))
  end)

  it('unmarks a ready home skgrepo move, asking only for several', function ()
    local lines = {
      '* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1) (ready)))',
      '** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1) (ready)))' }
    for _, line_number in ipairs({ 1, 2 }) do
      assert.are.same({
        '* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a',
        '** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (unready)) (diff (leaves 1) (ready))) ',
        '** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c',
        '*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1) (ready)))' },
        run(lines, line_number,
            readiness.toggle_homeSkgrepo_move_commitReady))
    end
    assert.are.same({
      '* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (unready)) (diff (leaves 1) (ready))) ',
      '** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (unready)) (diff (leaves 1) (ready))) ' },
      run(lines, 1,
          readiness.toggle_homeSkgrepo_move_commitReady_recursive,
          'yes'))
  end)

  it('computes a changed folder from its members', function ()
    -- The cases the server pinned when it wrote '(FOLDER changed)'.
    local ready_member =
      '** (skg node (gnid r) (diff (rel added) (leaves 2 node rel) (ready rel))) r'
    local unready_member =
      '** (skg node (gnid u) (diff (rel added) (leaves 2 node rel) (ready node))) u'
    local ready_unknown =
      '** (skg unknown (gnid k) (diff (rel added) (leaves 1) (ready))) k'
    local unready_alias =
      '** (skg alias noClaim (view ghostWarrant) (diff (rel removed))) a'
    local unchanged =
      '** (skg node (gnid n) (diff (node added) (ready node))) n'
    local function folder_change (members)
      local lines = { '* (skg containerFolder) its containers' }
      for _, member in ipairs(members) do table.insert(lines, member) end
      table.insert(lines, '* (skg node (gnid after) (diff (rel added))) after')
      local change = readiness.folder_change_in_lines(lines, 1)
      return change and { ready = change.ready } or nil
    end
    assert.are.same({ ready = true }, folder_change({ ready_member }))
    assert.are.same({ ready = false }, folder_change({ unready_member }))
    assert.are.same({ ready = true }, folder_change({ ready_unknown }))
    assert.are.same({ ready = false }, folder_change({ unready_alias }))
    assert.is_nil(folder_change({ unchanged }))
    assert.are.same({ ready = true }, folder_change(
      { ready_member, ready_unknown, unchanged }))
    assert.are.same({ ready = false }, folder_change(
      { ready_member, unready_member, ready_unknown, unready_alias }))
    -- A member's '(relRepoMove ...)' child is a change of its
    -- relationship too, ready when that child carries a '(ready ...)'.
    local move = '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))'
    assert.are.same({ ready = false }, folder_change(
      { '** (skg node (gnid m)) m', move }))
    assert.are.same({ ready = true }, folder_change(
      { '** (skg node (gnid m)) m',
        '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1) (ready)))' }))
    assert.are.same({ ready = false }, folder_change(
      { ready_member, move }))
    -- Only a folder's members count, and only for a folder.
    assert.is_nil(folder_change({ '** (skg node (gnid m)) m',
                                  '*** ' .. unready_alias:sub(4) }))
    assert.is_nil(readiness.folder_change_in_lines(
      { '* (skg node (gnid p)) p', unready_member }, 1))
  end)

  it('writes one level down on a folder changed by a relRepo move', function ()
    -- The folder is changed by the member's relRepoMove child, but the
    -- gesture names only the leaves one level down.
    assert.are.same({
      '* (skg containerFolder) its containers',
      '** (skg node (gnid m) (skgRepos (nodeRepo r)) (editRequest (ready rel)) (diff (leaves 1 rel))) m',
      '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))',
      '** (skg alias (editRequest (ready)) (diff (rel added) (leaves 1))) a' },
      run({ '* (skg containerFolder) its containers',
            '** (skg node (gnid m) (skgRepos (nodeRepo r)) (diff (leaves 1 rel))) m',
            '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))',
            '** (skg alias (diff (rel added) (leaves 1))) a' },
          1, readiness.cycle_commit_ready))
    -- On a folder whose members carry no change of their relationship,
    -- there is nothing to mark.
    assert.has_error(function ()
      run({ '* (skg aliasFolder)', '** (skg alias) a' },
          1, readiness.cycle_commit_ready)
    end)
  end)

  it('keeps the other edit request beside a readiness request', function ()
    assert.are.same(
      { '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest restoreNode (unready node)) (diff (node removed) (leaves 1 node) (ready node))) d' },
      run({ '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest restoreNode (ready node)) (diff (node removed) (leaves 1 node) (ready node))) d' },
          1, readiness.cycle_commit_ready))
    -- A readiness request goes before '(viewRequests ...)' and the
    -- derived forms, where the server writes it.
    assert.are.same(
      { '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (ready node)) (viewRequests tombstone) (diff (node removed) (leaves 1 node))) d' },
      run({ '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (viewRequests tombstone) (diff (node removed) (leaves 1 node))) d' },
          1, readiness.cycle_commit_ready))
  end)

  it('walks exactly the headlines one level below the line', function ()
    -- Mirrors test-skg-readiness-do-children-visits-exactly-one-level-below-point.
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
      '* (skg node (gnid before)) before',
      '* (skg node (gnid parent)) parent',
      'body of parent',
      '** (skg node (gnid child1)) child1',
      '*** (skg node (gnid grandchild)) grandchild',
      '** (skg node (gnid child2)) child2',
      '* (skg node (gnid after)) after',
      '** (skg node (gnid nephew)) nephew' })
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
        '((content "* (skg node (gnid a)) a\n") (errors ()) (warnings ())'
        .. ' (readiness-summary (ready 12) (changed 14) (foreign 2)'
        .. ' (order-only 1) (notEditable 3)))'))
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg node (gnid a)) a\n") (errors ()) (warnings ())'
        .. ' (readiness-summary (ready 14) (changed 14) (foreign 0)'
        .. ' (order-only 0) (notEditable 0)))'))
      save.handle_save_response(buf, sexpr.read(
        '((content "* (skg node (gnid a)) a\n") (errors ()) (warnings ()))'))
    end)
    vim.notify = original_notify
    vim.api.nvim_buf_delete(buf, { force = true })
    assert(ok, err)
    assert.are.same({
      '12 of 14 changed files are ready, 3 changes are not editable in this view; <localleader>ru integrates them, 2 files changed in foreign skgrepos',
      'Commit ready. All 14 of 14 changed files are ready.' },
      vim.tbl_filter(function (message)
        return message:find('changed files', 1, true) ~= nil
      end, shown))
  end)
end)

describe('skg.readiness moving between change headlines', function ()
  -- Mirrors the 'goto-change' tests of
  -- tests/elisp/test-skg-readiness.el.

  ---Three change headlines (lines 2, 4 and 6), the middle one ready,
  ---among headlines that carry no leaves fact, one of them a ready
  ---folder; line 7 is empty, the buffer's end.
  local change_headlines = {
    '* (skg node (gnid r) (skgRepos (nodeRepo main))) r',
    '** (skg node (gnid a) (skgRepos (nodeRepo main)) (diff (node added) (leaves 1 node))) a',
    '*** (skg node (gnid b) (skgRepos (nodeRepo main))) b',
    '** (skg node (gnid c) (skgRepos (nodeRepo main)) (diff (properties changed) (leaves 2 all) (ready all))) c',
    '** (skg containerFolder) its containers',
    '*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1',
    '' }

  ---Fill a buffer with LINES, put the cursor at the start of line
  ---FROM, and run COMMAND TIMES times. Return the line each run left
  ---the cursor on, or the message of a run that did not move it. A
  ---run that moves leaves the cursor on '(skg'.
  local function goto_changes (lines, from, command, times)
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.api.nvim_set_current_buf(buf)
    vim.api.nvim_win_set_cursor(0, { from, 0 })
    local results = {}
    local original_notify = vim.notify
    local ok, err = pcall(function ()
      for _ = 1, times do
        local shown = nil
        vim.notify = function (message) shown = message end
        local before = vim.api.nvim_win_get_cursor(0)
        command()
        local after = vim.api.nvim_win_get_cursor(0)
        if before[1] == after[1] and before[2] == after[2] then
          table.insert(results, shown)
        else
          local line = vim.api.nvim_buf_get_lines(
            buf, after[1] - 1, after[1], false)[1]
          assert.are.equal('(skg', line:sub(after[2] + 1, after[2] + 4))
          table.insert(results, after[1])
        end
      end
    end)
    vim.notify = original_notify
    vim.api.nvim_buf_delete(buf, { force = true })
    assert(ok, err)
    return results
  end

  it('visits the change headlines of each kind and reports the ends',
     function ()
    assert.are.same({ 2, 4, 6, 'No further change' },
      goto_changes(change_headlines, 1,
                   readiness.diffView_goto_next_change, 4))
    assert.are.same({ 2, 6, 'No further unready change' },
      goto_changes(change_headlines, 1,
                   readiness.diffView_goto_next_unready_change, 3))
    assert.are.same({ 4, 'No further ready change' },
      goto_changes(change_headlines, 1,
                   readiness.diffView_goto_next_ready_change, 2))
    assert.are.same({ 6, 4, 2, 'No earlier change' },
      goto_changes(change_headlines, 7,
                   readiness.diffView_goto_previous_change, 4))
    assert.are.same({ 6, 2, 'No earlier unready change' },
      goto_changes(change_headlines, 7,
                   readiness.diffView_goto_previous_unready_change, 3))
    assert.are.same({ 4, 'No earlier ready change' },
      goto_changes(change_headlines, 7,
                   readiness.diffView_goto_previous_ready_change, 2))
    -- From the middle, each direction starts at the cursor.
    assert.are.same({ 2 },
      goto_changes(change_headlines, 3,
                   readiness.diffView_goto_previous_change, 1))
    assert.are.same({ 6 },
      goto_changes(change_headlines, 3,
                   readiness.diffView_goto_next_unready_change, 1))
  end)

  it('finds no change where no headline carries a leaves fact',
     function ()
    assert.are.same({ 'No further change' },
      goto_changes({ '* (skg node (gnid r) (skgRepos (nodeRepo main))) r',
                     '** (skg node (gnid a) (skgRepos (nodeRepo main))) a' }, 1,
                   readiness.diffView_goto_next_change, 1))
  end)

  it('binds the six commands under <localleader>rn and rp', function ()
    local keymaps = require('skg.keymaps')
    local by_lhs = {}
    for _, binding in ipairs(keymaps.content_view_bindings) do
      by_lhs[binding[1]] = binding[2] end
    local expected = {
      rnc = { 'DiffViewGotoNextChange', 'diffView_goto_next_change' },
      rnu = { 'DiffViewGotoNextUnreadyChange',
              'diffView_goto_next_unready_change' },
      rnr = { 'DiffViewGotoNextReadyChange', 'diffView_goto_next_ready_change' },
      rpc = { 'DiffViewGotoPreviousChange', 'diffView_goto_previous_change' },
      rpu = { 'DiffViewGotoPreviousUnreadyChange',
              'diffView_goto_previous_unready_change' },
      rpr = { 'DiffViewGotoPreviousReadyChange',
              'diffView_goto_previous_ready_change' } }
    for lhs, names in pairs(expected) do
      assert.are.equal(names[1], by_lhs[lhs], lhs)
      local spec = keymaps.commands[names[1]]
      assert.are.equal('skg.readiness', spec.module, lhs)
      assert.are.equal(names[2], spec.fn, lhs)
      assert.is_function(readiness[spec.fn], lhs)
    end
  end)
end)
