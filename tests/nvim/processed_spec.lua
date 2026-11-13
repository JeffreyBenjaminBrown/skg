-- Coverage for skg.processed (the processed gestures) and the
-- processed summary a save of a diff-view buffer shows.
-- Mirrors tests/elisp/test-skg-processed.el.

local metadata = require('skg.metadata')
local picker = require('skg.picker')
local processed = require('skg.processed')
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
    assert.are.equal(processed.WIDE_UNMARK_QUESTION, prompt)
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

---Run COMMAND with the cursor on line LINE_NUMBER of LINES; it must
---refuse, writing nothing and saving nothing.
local function refused (lines, line_number, command)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { line_number, 0 })
  local original_save = save.request_save_buffer
  save.request_save_buffer = function () error('a refused gesture saved') end
  local ok, err = pcall(command)
  save.request_save_buffer = original_save
  local after = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  vim.api.nvim_buf_delete(buf, { force = true })
  assert.is_false(ok)
  assert.is_falsy(tostring(err):find('a refused gesture saved', 1, true))
  assert.are.same(lines, after)
end

describe('skg.processed', function ()
  it('marks a single leaf by a letter of its kind', function ()
    -- A relRepo move is a relationship's leaf, any other the node's own.
    assert.are.same(
      { '** (skg alias noClaim (editRequest (processed)) (view ghostWarrant) (diff (rel removed) (leaves 1))) a1' },
      run({ '** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1' }, 1,
          processed.mark_node_processed))
    assert.are.same(
      { '** (skg (oldText title) (editRequest (unprocessed)) (diff (leaves 1) (processed))) txt' },
      run({ '** (skg (oldText title) (diff (leaves 1) (processed))) txt' }, 1,
          processed.mark_both_unprocessed))
    assert.are.same(
      { '** (skg (relRepoMove (old pub) (new priv)) (editRequest (processed)) (diff (leaves 1))) ' },
      run({ '** (skg (relRepoMove (old pub) (new priv)) (diff (leaves 1)))' }, 1,
          processed.mark_rel_processed))
    refused({ '** (skg (relRepoMove (old pub) (new priv)) (diff (leaves 1)))' }, 1,
            processed.mark_node_processed)
    refused({ '** (skg alias (diff (rel added) (leaves 1))) a' }, 1,
            processed.mark_rel_processed)
    -- Without the server's '(diff (leaves ...))' fact, nothing is named.
    refused({ '** (skg (oldText title)) txt' }, 1, processed.mark_both_processed)
  end)

  it('writes the request as the last part of the diff form', function ()
    -- Two levels under 'skg' on a node; a headline without the form
    -- gets one.
    local function write (line, mark_processed, objects)
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, { line })
      vim.api.nvim_set_current_buf(buf)
      processed.write_processed_request(1, mark_processed, objects)
      local result = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1]
      vim.api.nvim_buf_delete(buf, { force = true })
      return result
    end
    assert.are.equal(
      '* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (unprocessed rel)) (diff (rel added) (leaves 1 rel))) k',
      write('* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) k',
            false, { 'rel' }))
    assert.are.equal(
      '** (skg alias (editRequest (processed)) (diff (rel added) (leaves 1))) a',
      write('** (skg alias (diff (rel added) (leaves 1))) a', true, {}))
    assert.are.equal(
      '** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed))) ',
      write('** (skg (homeSkgrepoMove (old x) (new b))) ', true, {}))
  end)

  it('refuses to write a request on a headline without metadata, as'
     .. ' skg--write-processed-request does', function ()
    for _, line in ipairs({ '* plain', '* (skgrepos are cool) title' }) do
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, { line })
      vim.api.nvim_set_current_buf(buf)
      local ok, refusal = pcall(processed.write_processed_request,
                                1, true, { 'rel' })
      local after = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1]
      vim.api.nvim_buf_delete(buf, { force = true })
      assert.is_false(ok)
      assert.are.equal('Headline has no skg metadata', refusal)
      assert.are.equal(line, after)
    end
  end)

  it('names the letters among the listed objects', function ()
    -- A new member lists two objects, one of them processed: each letter
    -- names its own, without asking.
    local line = '* (skg node (gnid k) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel) (processed rel))) k'
    for command, request in pairs({
        mark_node_processed = '(processed node)',
        mark_rel_processed = '(processed rel)',
        mark_both_processed = '(processed node rel)',
        mark_rel_unprocessed = '(unprocessed rel)',
        mark_node_unprocessed = '(unprocessed node)' }) do
      assert.are.same(
        { '* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest ' .. request
          .. ') (diff (node added) (rel added) (leaves 2 node rel) (processed rel))) k' },
        run({ line }, 1, processed[command]), command)
    end
    -- 'b' on a headline that lists one object names that one.
    assert.are.same(
      { '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (node removed) (leaves 1 node))) d' },
      run({ '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (leaves 1 node))) d' }, 1,
          processed.mark_both_processed))
    -- A letter naming nothing listed refuses.
    refused({ '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (leaves 1 node))) d' }, 1,
            processed.mark_rel_processed)
    refused({ '* (skg node (gnid c) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) c' }, 1,
            processed.mark_node_unprocessed)
  end)

  it('writes one level down on a changed folder, filtered by the letter', function ()
    local aliases = {
      '* (skg node (gnid ali) (skgRepos (nodeRepo r)) (diff (leaves 2 node))) ali',
      '** (skg aliasFolder)',
      '*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1',
      '*** (skg alias) a2',
      '*** (skg alias (diff (rel added) (leaves 1))) a3' }
    assert.are.same({
      aliases[1], aliases[2],
      '*** (skg alias noClaim (editRequest (processed)) (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1',
      aliases[4],
      '*** (skg alias (editRequest (processed)) (diff (rel added) (leaves 1))) a3' },
      run(aliases, 2, processed.mark_node_processed))
    refused(aliases, 2, processed.mark_rel_processed)
    -- A partner folder: 'r' names each member's relationship, 'n' only
    -- the members whose node changed.
    local subscribers = {
      '** (skg subscriberFolder)',
      '*** (skg node (gnid s1) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) s1',
      '*** (skg node (gnid s2) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) s2' }
    assert.are.same({
      subscribers[1],
      '*** (skg node (gnid s1) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) s1',
      '*** (skg node (gnid s2) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (node added) (rel added) (leaves 2 node rel))) s2' },
      run(subscribers, 1, processed.mark_rel_processed))
    assert.are.same({
      subscribers[1], subscribers[2],
      '*** (skg node (gnid s2) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (node added) (rel added) (leaves 2 node rel))) s2' },
      run(subscribers, 1, processed.mark_node_processed))
  end)

  it('takes the letters as they are on a changeset marker at point', function ()
    -- Its 'node' names its members' own leaves, which its '(leaves ...)'
    -- does not list nor count, so unmarking them asks first.
    local lines = { '** (skg (changesetMarker content) (diff (leaves 1 rel) (processed rel)))' }
    local function with (request)
      return { '** (skg (changesetMarker content) (editRequest ' .. request
               .. ') (diff (leaves 1 rel) (processed rel))) ' }
    end
    assert.are.same(with('(processed node)'), run(lines, 1, processed.mark_node_processed))
    assert.are.same(with('(processed node rel)'), run(lines, 1, processed.mark_both_processed))
    assert.are.same(with('(unprocessed rel)'), run(lines, 1, processed.mark_rel_unprocessed))
    assert.are.same(lines, run(lines, 1, processed.mark_node_unprocessed, 'no'))
  end)

  it('honours the letter recursively', function ()
    -- Below the headline at point a changeset marker names only what it
    -- lists, its members naming their own leaves.
    local lines = {
      '* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 node))) r',
      '** (skg (changesetMarker content) (diff (leaves 2 rel)))',
      '** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel))) d',
      '*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k',
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (leaves 1 rel))) c',
      '** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (properties changed) (leaves 3 node rel))) m',
      '*** (skg (oldText title) (diff (leaves 1))) m',
      '* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 node))) other' }
    assert.are.same({
      lines[1],
      '** (skg (changesetMarker content) (editRequest (processed rel)) (diff (leaves 2 rel))) ',
      '** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (node removed) (rel removed) (leaves 2 node rel))) d',
      lines[4],
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c',
      '** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (properties changed) (leaves 3 node rel))) m',
      lines[7], lines[8] },
      run(lines, 1, processed.mark_rel_processed_contentward))
    assert.are.same({
      '* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (properties changed) (leaves 1 node))) r',
      lines[2],
      '** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (node removed) (rel removed) (leaves 2 node rel))) d',
      lines[4], lines[5],
      '** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed node)) (diff (rel added) (properties changed) (leaves 3 node rel))) m',
      '*** (skg (oldText title) (editRequest (processed)) (diff (leaves 1))) m',
      lines[8] },
      run(lines, 1, processed.mark_node_processed_contentward))
    assert.are.same({
      '* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (properties changed) (leaves 1 node))) r',
      '** (skg (changesetMarker content) (editRequest (processed rel)) (diff (leaves 2 rel))) ',
      '** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node rel)) (diff (node removed) (rel removed) (leaves 2 node rel))) d',
      lines[4],
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c',
      '** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed node rel)) (diff (rel added) (properties changed) (leaves 3 node rel))) m',
      '*** (skg (oldText title) (editRequest (processed)) (diff (leaves 1))) m',
      lines[8] },
      run(lines, 1, processed.mark_both_processed_contentward))
    -- At point, a changeset marker takes the letters as they are.
    assert.are.same(
      { '** (skg (changesetMarker content) (editRequest (processed node rel)) (diff (leaves 2 rel))) ' },
      run({ lines[2] }, 1, processed.mark_both_processed_contentward))
  end)

  it('hands new nodes in content position to the server, skipping their subtrees', function ()
    -- The request names the letters and 'newContent' (contentward) or
    -- 'newOutward' (outward), which the server expands from the graph;
    -- the walk goes on past it. A new node in a folder is no content.
    local lines = {
      '* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 node))) r',
      '** (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) n',
      '*** (skg node (gnid n2) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) n2',
      '*** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (properties changed) (leaves 3 node rel))) m',
      '** (skg subscriberFolder)',
      '*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (diff (node added) (rel added) (leaves 2 node rel))) s',
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) c' }
    for command, walk in pairs({ mark_both_processed_contentward = 'newContent',
                                 mark_both_processed_outward = 'newOutward' }) do
      assert.are.same({
        '* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (properties changed) (leaves 1 node))) r',
        '** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed node rel ' .. walk
          .. ')) (diff (node added) (rel added) (leaves 2 node rel))) n',
        lines[3], lines[4], lines[5],
        '*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed node rel)) (diff (node added) (rel added) (leaves 2 node rel))) s',
        '** (skg node (gnid c) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c' },
        run(lines, 1, processed[command]), command)
    end
    -- The letter filters the walk, and the headlines walked in the buffer.
    assert.are.same({
      lines[1],
      '** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed rel newContent)) (diff (node added) (rel added) (leaves 2 node rel))) n',
      lines[3], lines[4], lines[5],
      '*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (node added) (rel added) (leaves 2 node rel))) s',
      '** (skg node (gnid c) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c' },
      run(lines, 1, processed.mark_rel_processed_contentward))
    -- At a new start, the letter names the headline's own objects too.
    local from_n = { unpack(lines, 2) }
    local expected = vim.deepcopy(from_n)
    expected[1] = '** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed node newContent)) (diff (node added) (rel added) (leaves 2 node rel))) n'
    assert.are.same(expected, run(from_n, 1, processed.mark_node_processed_contentward))
    -- The letter goes whole beside the walk, even where the headline
    -- does not list it: without it the walk would name every kind.
    assert.are.same(
      { '* (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed rel newContent)) (diff (node added) (leaves 1 node))) n' },
      run({ '* (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (leaves 1 node))) n' }, 1,
          processed.mark_rel_processed_contentward))
    -- Unmarking at a new start asks first: the client cannot count what
    -- the server will reach.
    assert.are.same(
      { '* (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (unprocessed node newContent)) (diff (node added) (rel added) (leaves 2 node rel) (processed node))) n' },
      run({ '* (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel) (processed node))) n' }, 1,
          processed.mark_node_unprocessed_contentward, 'yes'))
  end)

  it('marks home skgrepo moves, recursively only those like the first', function ()
    local lines = {
      '* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) a',
      '** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))',
      '** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 node))) c',
      '*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))',
      '** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) e',
      '*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))' }
    local marked = '** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed)) (diff (leaves 1))) '
    assert.are.same({ lines[1], marked, lines[3], lines[4], lines[5], lines[6] },
                    run(lines, 1, processed.mark_homeSkgrepo_move_processed))
    assert.are.same({ lines[1], marked, lines[3], '*' .. marked, lines[5], lines[6] },
                    run(lines, 1, processed.mark_homeSkgrepo_move_processed_recursive))
    refused({ lines[1] }, 1, processed.mark_homeSkgrepo_move_processed)
  end)

  it('asks nothing before a single-leaf unmark', function ()
    assert.are.same({
      '** (skg aliasFolder)',
      '*** (skg alias noClaim (editRequest (unprocessed)) (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1',
      '*** (skg alias) a2' },
      run({ '** (skg aliasFolder)',
            '*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1',
            '*** (skg alias) a2' }, 1, processed.mark_node_unprocessed))
    -- A modified node whose only leaf is processed: one leaf, as the
    -- server counts.
    assert.are.same({
      '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (unprocessed node)) (diff (properties changed) (leaves 1 node) (processed node))) txt',
      '** (skg (oldText title) (diff (leaves 1) (processed))) txt' },
      run({ '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 node) (processed node))) txt',
            '** (skg (oldText title) (diff (leaves 1) (processed))) txt' }, 1,
          processed.mark_node_unprocessed))
    -- Of two leaves, 'n' or 'r' alone unmarks one.
    for command, request in pairs({ mark_node_unprocessed = '(unprocessed node)',
                                    mark_rel_unprocessed = '(unprocessed rel)' }) do
      assert.are.same(
        { '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest ' .. request
          .. ') (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d' },
        run({ '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d' },
            1, processed[command]), command)
    end
  end)

  it('asks before a wide unmark, and writes nothing on refusal', function ()
    local lines = {
      '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d',
      '** (skg (oldText title) (diff (leaves 1) (processed))) d' }
    assert.are.same(lines, run(lines, 1, processed.mark_both_unprocessed, 'no'))
    assert.are.same({
      '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (unprocessed node rel)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d',
      '** (skg (oldText title) (editRequest (unprocessed)) (diff (leaves 1) (processed))) d' },
      run(lines, 1, processed.mark_both_unprocessed_contentward, 'yes'))
    -- Unmarking a modified node unmarks every leaf it names: two here.
    local modified = {
      '* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 2 node) (processed node))) txt',
      '** (skg (oldText title) (diff (leaves 1) (processed))) txt',
      '** (skg (homeSkgrepoMove (old x) (new r)) (diff (leaves 1) (processed)))' }
    assert.are.same(modified, run(modified, 1, processed.mark_node_unprocessed, 'no'))
  end)

  it('counts the leaves a request touches as the server does', function ()
    -- An object the headline does not list counts two, so that
    -- unmarking it asks. Mirrors test-skg-processed-leaves-touched.
    local cases = {
      { { count = 3, objects = { 'node', 'rel' } }, { 'node', 'rel' }, 3 },
      { { count = 3, objects = { 'node', 'rel' } }, { 'node' }, 2 },
      { { count = 3, objects = { 'node', 'rel' } }, { 'rel' }, 1 },
      { { count = 2, objects = { 'node' } }, { 'node' }, 2 },
      { { count = 1, objects = { 'rel' } }, { 'node', 'rel' }, 3 },
      { { count = 1, objects = { 'rel' } }, { 'node' }, 2 },
      { { count = 2, objects = { 'node', 'rel' } }, { 'node', 'rel', 'newContent' }, 4 },
      { { count = 1, objects = { 'node' } }, { 'rel', 'newOutward' }, 4 },
      { { count = 1, objects = {} }, {}, 1 },
      { nil, {}, 1 } }
    for i, case in ipairs(cases) do
      assert.are.equal(case[3], processed.leaves_touched(case[1], case[2]), i)
    end
  end)

  it('computes a changed folder from its members', function ()
    -- The cases the server pinned when it wrote '(FOLDER changed)'.
    local processed_member =
      '** (skg node (gnid r) (diff (rel added) (leaves 2 node rel) (processed rel))) r'
    local unprocessed_member =
      '** (skg node (gnid u) (diff (rel added) (leaves 2 node rel) (processed node))) u'
    local processed_unknown =
      '** (skg unknown (gnid k) (diff (rel added) (leaves 1) (processed))) k'
    local unprocessed_alias =
      '** (skg alias noClaim (view ghostWarrant) (diff (rel removed))) a'
    local unchanged =
      '** (skg node (gnid n) (diff (node added) (processed node))) n'
    local function folder_change (members)
      local lines = { '* (skg containerFolder) its containers' }
      for _, member in ipairs(members) do table.insert(lines, member) end
      table.insert(lines, '* (skg node (gnid after) (diff (rel added))) after')
      local change = metadata.folder_change_processed_in_lines(lines, 1)
      return change and { processed = change.processed } or nil
    end
    assert.are.same({ processed = true }, folder_change({ processed_member }))
    assert.are.same({ processed = false }, folder_change({ unprocessed_member }))
    assert.are.same({ processed = true }, folder_change({ processed_unknown }))
    assert.are.same({ processed = false }, folder_change({ unprocessed_alias }))
    assert.is_nil(folder_change({ unchanged }))
    assert.are.same({ processed = true }, folder_change(
      { processed_member, processed_unknown, unchanged }))
    assert.are.same({ processed = false }, folder_change(
      { processed_member, unprocessed_member, processed_unknown, unprocessed_alias }))
    -- A member's '(relRepoMove ...)' child is a change of its
    -- relationship too, processed when that child carries a '(processed ...)'.
    local move = '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))'
    assert.are.same({ processed = false }, folder_change(
      { '** (skg node (gnid m)) m', move }))
    assert.are.same({ processed = true }, folder_change(
      { '** (skg node (gnid m)) m',
        '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1) (processed)))' }))
    assert.are.same({ processed = false }, folder_change(
      { processed_member, move }))
    -- Only a folder's members count, and only for a folder.
    assert.is_nil(folder_change({ '** (skg node (gnid m)) m',
                                  '*** ' .. unprocessed_alias:sub(4) }))
    assert.is_nil(metadata.folder_change_processed_in_lines(
      { '* (skg node (gnid p)) p', unprocessed_member }, 1))
  end)

  it('writes one level down on a folder changed by a relRepo move', function ()
    -- The folder is changed by the member's relRepoMove child, but the
    -- gesture names only the leaves one level down.
    assert.are.same({
      '* (skg containerFolder) its containers',
      '** (skg node (gnid m) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (leaves 1 rel))) m',
      '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))',
      '** (skg alias (editRequest (processed)) (diff (rel added) (leaves 1))) a' },
      run({ '* (skg containerFolder) its containers',
            '** (skg node (gnid m) (skgRepos (nodeRepo r)) (diff (leaves 1 rel))) m',
            '*** (skg (relRepoMove (old a) (new b)) (diff (leaves 1)))',
            '** (skg alias (diff (rel added) (leaves 1))) a' },
          1, processed.mark_both_processed))
    -- On a folder whose members carry no change of their relationship,
    -- there is nothing to mark.
    refused({ '* (skg aliasFolder)', '** (skg alias) a' },
            1, processed.mark_both_processed)
  end)

  it('keeps the other edit request beside a processed request', function ()
    assert.are.same(
      { '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest restoreNode (unprocessed node)) (diff (node removed) (leaves 1 node) (processed node))) d' },
      run({ '* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest restoreNode (processed node)) (diff (node removed) (leaves 1 node) (processed node))) d' },
          1, processed.mark_node_unprocessed))
    -- A processed request goes before '(viewRequests ...)' and the
    -- derived forms, where the server writes it.
    assert.are.same(
      { '* (skg node (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node)) (viewRequests editableView) (diff (node added) (leaves 1 node))) d' },
      run({ '* (skg node (gnid d) (skgRepos (nodeRepo r)) (viewRequests editableView) (diff (node added) (leaves 1 node))) d' },
          1, processed.mark_node_processed))
  end)

  it('replaces the edit request and keeps the processed request',
     function ()
    -- metadata_with_edit_request, the counterpart of
    -- metadata_with_processed_request,
    -- which the merge and restore commands write with. A nil request
    -- removes the edit request (the relRepo removal of
    -- test-apply-relRepo-keeps-the-processed-request), and the form
    -- with it when nothing is left.
    local function with_edit_request (metadata_text, request_text)
      return sexpr.to_string(metadata.metadata_with_edit_request(
        sexpr.read(metadata_text),
        request_text and sexpr.read(request_text)))
    end
    assert.are.same(
      { '(skg node (gnid x) (editRequest (relRepo private) (processed rel)) (diff (rel added)))',
        '(skg node (gnid x) (editRequest (processed rel)) (diff (rel added)))',
        '(skg node (gnid x) (diff (rel added)))',
        '(skg node (gnid x) (editRequest restoreRel) (view noWarrant))' },
      { with_edit_request(
          '(skg node (gnid x) (editRequest (relRepo trusted) (processed rel)) (diff (rel added)))',
          '(relRepo private)'),
        with_edit_request(
          '(skg node (gnid x) (editRequest (relRepo trusted) (processed rel)) (diff (rel added)))',
          nil),
        with_edit_request(
          '(skg node (gnid x) (editRequest delete) (diff (rel added)))', nil),
        with_edit_request('(skg node (gnid x) (view noWarrant))',
                          'restoreRel') })
  end)

  it('walks exactly the headlines one level below the line', function ()
    -- Mirrors test-skg-processed-do-children-visits-exactly-one-level-below-point.
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
    local lines = require('skg.modify_graph').immediate_viewchildren(2)
    vim.api.nvim_buf_delete(buf, { force = true })
    assert.are.same({ 4, 6 }, lines)
  end)
end)

describe('skg.save processed summary', function ()
  it('is shown after a save of a diff-view buffer', function ()
    local shown = {}
    local original_notify = vim.notify
    vim.notify = function (message) table.insert(shown, message) end
    local buf = vim.api.nvim_create_buf(true, false)
    local ok, err = pcall(function ()
      save.handle_save_sexp(buf, sexpr.read(
        '((content "* (skg node (gnid a)) a\n") (errors ()) (warnings ())'
        .. ' (processed-summary (processed 12) (changed 14) (foreign 2)'
        .. ' (order-only 1) (notEditable 3)))'))
      save.handle_save_sexp(buf, sexpr.read(
        '((content "* (skg node (gnid a)) a\n") (errors ()) (warnings ())'
        .. ' (processed-summary (processed 14) (changed 14) (foreign 0)'
        .. ' (order-only 0) (notEditable 0)))'))
      save.handle_save_sexp(buf, sexpr.read(
        '((content "* (skg node (gnid a)) a\n") (errors ()) (warnings ()))'))
    end)
    vim.notify = original_notify
    vim.api.nvim_buf_delete(buf, { force = true })
    assert(ok, err)
    assert.are.same({
      '12 of 14 changed files are processed, 3 changes are not editable in this view; <localleader>ru extends the diff view to them, 2 files changed in foreign skgrepos',
      'Commit ready. All 14 of 14 changed files are processed.' },
      vim.tbl_filter(function (message)
        return message:find('changed files', 1, true) ~= nil
      end, shown))
  end)
end)

describe('skg.processed moving between headlines with change facts', function ()
  -- Mirrors the 'goto-change' tests of
  -- tests/elisp/test-skg-processed.el.

  ---Three headlines with change facts (lines 2, 4 and 6), the middle one processed,
  ---among headlines that carry no leaves fact, one of them a processed
  ---folder; line 7 is empty, the buffer's end.
  local headlines_with_change_facts = {
    '* (skg node (gnid r) (skgRepos (nodeRepo main))) r',
    '** (skg node (gnid a) (skgRepos (nodeRepo main)) (diff (node added) (leaves 1 node))) a',
    '*** (skg node (gnid b) (skgRepos (nodeRepo main))) b',
    '** (skg node (gnid c) (skgRepos (nodeRepo main)) (diff (properties changed) (leaves 2 node) (processed node))) c',
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

  it('visits the headlines with change facts of each kind and reports the ends',
     function ()
    assert.are.same({ 2, 4, 6, 'No further change' },
      goto_changes(headlines_with_change_facts, 1,
                   processed.diffView_goto_next_change, 4))
    assert.are.same({ 2, 6, 'No further unprocessed change' },
      goto_changes(headlines_with_change_facts, 1,
                   processed.diffView_goto_next_unprocessed_change, 3))
    assert.are.same({ 4, 'No further processed change' },
      goto_changes(headlines_with_change_facts, 1,
                   processed.diffView_goto_next_processed_change, 2))
    assert.are.same({ 6, 4, 2, 'No earlier change' },
      goto_changes(headlines_with_change_facts, 7,
                   processed.diffView_goto_previous_change, 4))
    assert.are.same({ 6, 2, 'No earlier unprocessed change' },
      goto_changes(headlines_with_change_facts, 7,
                   processed.diffView_goto_previous_unprocessed_change, 3))
    assert.are.same({ 4, 'No earlier processed change' },
      goto_changes(headlines_with_change_facts, 7,
                   processed.diffView_goto_previous_processed_change, 2))
    -- From the middle, each direction starts at the cursor.
    assert.are.same({ 2 },
      goto_changes(headlines_with_change_facts, 3,
                   processed.diffView_goto_previous_change, 1))
    assert.are.same({ 6 },
      goto_changes(headlines_with_change_facts, 3,
                   processed.diffView_goto_next_unprocessed_change, 1))
  end)

  it('finds no change where no headline carries a leaves fact',
     function ()
    assert.are.same({ 'No further change' },
      goto_changes({ '* (skg node (gnid r) (skgRepos (nodeRepo main))) r',
                     '** (skg node (gnid a) (skgRepos (nodeRepo main))) a' }, 1,
                   processed.diffView_goto_next_change, 1))
  end)

  it('binds the six commands under <localleader>rn and rp', function ()
    local keymaps = require('skg.keymaps')
    local by_lhs = {}
    for _, binding in ipairs(keymaps.content_view_bindings) do
      by_lhs[binding[1]] = binding[2] end
    local expected = {
      rnc = { 'DiffViewGotoNextChange', 'diffView_goto_next_change' },
      rnu = { 'DiffViewGotoNextUnprocessedChange',
              'diffView_goto_next_unprocessed_change' },
      rnp = { 'DiffViewGotoNextProcessedChange', 'diffView_goto_next_processed_change' },
      rpc = { 'DiffViewGotoPreviousChange', 'diffView_goto_previous_change' },
      rpu = { 'DiffViewGotoPreviousUnprocessedChange',
              'diffView_goto_previous_unprocessed_change' },
      rpp = { 'DiffViewGotoPreviousProcessedChange',
              'diffView_goto_previous_processed_change' } }
    for lhs, names in pairs(expected) do
      assert.are.equal(names[1], by_lhs[lhs], lhs)
      local spec = keymaps.commands[names[1]]
      assert.are.equal('skg.processed', spec.module, lhs)
      assert.are.equal(names[2], spec.fn, lhs)
      assert.is_function(processed[spec.fn], lhs)
    end
  end)

  it("binds each mark as m, what, which way; capitals for the walks; no toggles", function ()
    -- '<localleader>m{n,r,b,h}{p,u}'; a capital last letter for
    -- contentward (Emacs's C-u), a capital 'M' for outward (C-u C-u).
    local keymaps = require('skg.keymaps')
    local by_lhs = {}
    for _, binding in ipairs(keymaps.content_view_bindings) do
      by_lhs[binding[1]] = binding[2] end
    local function check (lhs, name, fn)
      assert.are.equal(name, by_lhs[lhs], lhs)
      local spec = keymaps.commands[name]
      assert.are.equal('skg.processed', spec.module, lhs)
      assert.are.equal(fn, spec.fn, lhs)
      assert.is_function(processed[spec.fn], lhs)
    end
    for letter, word in pairs({ n = 'node', r = 'rel', b = 'both' }) do
      for way, adjective in pairs({ p = 'processed', u = 'unprocessed' }) do
        local name = 'Mark' .. word:sub(1, 1):upper() .. word:sub(2)
                     .. adjective:sub(1, 1):upper() .. adjective:sub(2)
        local fn = 'mark_' .. word .. '_' .. adjective
        check('m' .. letter .. way, name, fn)
        check('m' .. letter .. way:upper(), name .. 'Contentward', fn .. '_contentward')
        check('M' .. letter .. way, name .. 'Outward', fn .. '_outward')
      end
    end
    check('mhp', 'MarkHomeSkgrepoMoveProcessed', 'mark_homeSkgrepo_move_processed')
    check('mhU', 'MarkHomeSkgrepoMoveUnprocessedRecursive',
          'mark_homeSkgrepo_move_unprocessed_recursive')
    for _, gone in ipairs({ 'rr', 'rR', 'rO', 'rh', 'rH', 'rnr', 'rpr' }) do
      assert.is_nil(by_lhs[gone], gone) end
    assert.is_nil(processed.cycle_processed)
    assert.is_nil(processed.toggle_homeSkgrepo_move_processed)
  end)
end)
