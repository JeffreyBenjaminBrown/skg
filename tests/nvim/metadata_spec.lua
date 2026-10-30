-- Mirrors tests/elisp/test-skg-metadata.el (the parts belonging to
-- this module; the replace-content<->link cases live with
-- skg.modify_graph and the minibuffer linkstack cases with
-- skg.linkstack).

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local compare = require('skg.sexpr.compare')
local metadata = require('skg.metadata')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local sexpr_cycling = require('skg.sexpr_edit.sexpr_cycling')

local buffer_with = helpers.buffer_with
local buffer_text = helpers.buffer_text

---The first line's parsed metadata sexp, mirroring the elisp test
---helper test-skg--extract-metadata-sexp.
local function first_metadata_sexp ()
  return metadata.metadata_sexp_at_line_or_nil(1)
end

---The metadata sexp whose node id is ID, from any line.
local function metadata_sexp_by_id (gnid)
  for line = 1, vim.api.nvim_buf_line_count(0) do
    local sexp = metadata.metadata_sexp_at_line_or_nil(line)
    if sexp and metadata.node_id(sexp) == gnid then return sexp end
  end
  return nil
end

local function subtree_p (object, structure_text)
  return compare.subtree_p(object, sexpr.read(structure_text))
end

describe('skg.metadata parsing', function ()
  it('splits headlines into stars, metadata and title', function ()
    assert.is_nil(metadata.split_as_stars_metadata_title('title'))
    local plain = metadata.split_as_stars_metadata_title('* title')
    assert.are.same({ stars = '* ', metadata = '', title = 'title' },
                    plain)
    local with_meta = metadata.split_as_stars_metadata_title(
      '** (skg node (gnid 1)) some title')
    assert.are.equal('(skg node (gnid 1))', with_meta.metadata)
    assert.are.equal('some title', with_meta.title)
  end)
end)

describe('skg.metadata commands', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('delete marks the node for deletion', function ()
    buffer_with('* (skg node (gnid 1)) title')
    metadata.delete()
    assert.is_true(subtree_p(first_metadata_sexp(),
      '(skg node (editRequest delete))'))
  end)

  it('delete_recursive skips non-occurrence descendants', function ()
    buffer_with(table.concat({
      '* (skg node (gnid root)) root',
      '** (skg node (gnid child)) child',
      '** (skg aliasFolder) aliases',
      '*** some alias',
      '* (skg node (gnid sibling)) sibling' }, '\n'))
    metadata.delete_recursive()
    assert.is_true(subtree_p(metadata_sexp_by_id('root'),
      '(skg node (editRequest delete))'))
    assert.is_true(subtree_p(metadata_sexp_by_id('child'),
      '(skg node (editRequest delete))'))
    assert.is_false(subtree_p(
      metadata.metadata_sexp_at_line_or_nil(3),
      '(skg node (editRequest delete))'))
    assert.is_false(subtree_p(metadata_sexp_by_id('sibling'),
      '(skg node (editRequest delete))'))
  end)

  it('strips metadata from org text', function ()
    -- Mirrors test-skg-strip-metadata-from-org-text.
    assert.are.equal(
      '* root\nbody line\n** alias title\n** plain child\n',
      metadata.strip_metadata_from_org_text(
        '* (skg node (gnid 1) (skgRepos (nodeRepo public))) root\n'
        .. 'body line\n'
        .. '** (skg alias (diff (rel added))) alias title\n'
        .. '** plain child\n'))
  end)

  it('view_without_metadata opens a stripped projection', function ()
    buffer_with('* (skg node (gnid 1)) root\n** (skg node (gnid 2)) child')
    vim.fn.setpos("'<", { 0, 1, 1, 0 })
    vim.fn.setpos("'>", { 0, 2, 1, 0 })
    metadata.view_without_metadata()
    assert.are.equal('* root\n** child', buffer_text())
    assert.are.equal('org', vim.bo.filetype)
  end)

  it('set_repo replaces repo and writes nothing else', function ()
    -- Mirrors test-skg-set-repo.
    buffer_with('* (skg node (gnid 1) (skgRepos (nodeRepo public))) title')
    local original = picker.prompt_for_repo_change
    picker.prompt_for_repo_change = function (current)
      assert.are.equal('public', current)
      return 'private'
    end
    metadata.set_repo()
    picker.prompt_for_repo_change = original
    local result = first_metadata_sexp()
    assert.is_true(subtree_p(result,
      '(skg node (gnid 1) (skgRepos (nodeRepo private)))'))
    -- The save recomputes '(view repoBoundary)'.
    assert.is_nil(metadata.sexp_cdr_at_path(result, { 'skg', 'view' }))
    assert.is_false(subtree_p(result, '(skg node (skgRepos (nodeRepo public)))'))
  end)

  it('set_repo_recursive prunes noClaim children', function ()
    -- Mirrors test-skg-set-repo-recursive-prunes-noClaim-children.
    buffer_with(table.concat({
      '* (skg node (gnid root) (skgRepos (nodeRepo public))) root',
      '** (skg node (gnid content-child) (skgRepos (nodeRepo public))) content child',
      '*** (skg node (gnid content-grandchild) (skgRepos (nodeRepo public))) content grandchild',
      '** (skg node (gnid mismatched-content) (skgRepos (nodeRepo foreign))) mismatched content',
      '*** (skg node (gnid public-under-mismatch) (skgRepos (nodeRepo public))) public under mismatch',
      '** (skg node (gnid link-child) (skgRepos (nodeRepo public)) noClaim) link child',
      '*** (skg node (gnid under-link) (skgRepos (nodeRepo public))) under link',
      '** (skg aliasFolder) aliases',
      '*** (skg node (gnid under-non-vognode) (skgRepos (nodeRepo public))) under non-vognode' },
      '\n'))
    local original = picker.prompt_for_repo_change
    picker.prompt_for_repo_change = function (current)
      assert.are.equal('public', current)
      return 'private'
    end
    metadata.set_repo_recursive()
    picker.prompt_for_repo_change = original
    for _, gnid in ipairs({ 'root', 'content-child',
                          'content-grandchild',
                          'public-under-mismatch' }) do
      assert.is_true(subtree_p(metadata_sexp_by_id(gnid),
        '(skg node (skgRepos (nodeRepo private)))'), gnid)
      assert.is_nil(metadata.sexp_cdr_at_path(metadata_sexp_by_id(gnid),
                                              { 'skg', 'view' }), gnid)
    end
    assert.is_true(subtree_p(metadata_sexp_by_id('mismatched-content'),
      '(skg node (skgRepos (nodeRepo foreign)))'))
    for _, gnid in ipairs({ 'link-child', 'under-link',
                          'under-non-vognode' }) do
      assert.is_true(subtree_p(metadata_sexp_by_id(gnid),
        '(skg node (skgRepos (nodeRepo public)))'), gnid)
      assert.is_false(subtree_p(metadata_sexp_by_id(gnid),
        '(skg node (skgRepos (nodeRepo private)))'), gnid)
    end
  end)

  ---Run set_repo (RECURSIVE) with the prompt answering NEW_SKGREPO,
  ---and return every message it notified.
  local function notices_of_set_repo (new_skgrepo, recursive)
    local notices = {}
    local original_prompt = picker.prompt_for_repo_change
    local original_notify = vim.notify
    picker.prompt_for_repo_change = function () return new_skgrepo end
    vim.notify = function (text) table.insert(notices, text) end
    local ok, err = pcall(metadata.set_repo, recursive)
    picker.prompt_for_repo_change = original_prompt
    vim.notify = original_notify
    assert(ok, err)
    return notices
  end

  local function some_notice_matches (notices, ...)
    local patterns = { ... }
    for _, notice in ipairs(notices) do
      local all_match = true
      for _, pattern in ipairs(patterns) do
        if not notice:find(pattern, 1, true) then all_match = false end
      end
      if all_match then return true end
    end
    return false
  end

  it('set_repo_recursive warns about a write-protected occurrence', function ()
    -- Mirrors test-set-repo-recursive-warns-about-write-protected.
    buffer_with(table.concat({
      '* (skg node (gnid r) (skgRepos (nodeRepo private))) r',
      '** (skg node (gnid e) (skgRepos (nodeRepo private)) writeProtected) e',
      '*** (skg node (gnid f) (skgRepos (nodeRepo private))) f' }, '\n'))
    local notices = notices_of_set_repo('public', true)
    assert.is_true(some_notice_matches(notices, 'not changed', ': e',
                                       'refuse'))
    assert.is_true(some_notice_matches(notices, 'WARNING', ':messages',
                                       'refuse'))
    assert.is_true(subtree_p(metadata_sexp_by_id('e'),
      '(skg node (skgRepos (nodeRepo private)))'))
    for _, gnid in ipairs({ 'r', 'f' }) do
      assert.is_true(subtree_p(metadata_sexp_by_id(gnid),
        '(skg node (skgRepos (nodeRepo public)))'), gnid)
    end
  end)

  it('set_repo_recursive lets a repeat follow its editable occurrence', function ()
    -- Mirrors test-set-repo-recursive-repeat-follows-its-editable-occurrence.
    buffer_with(table.concat({
      '* (skg node (gnid r) (skgRepos (nodeRepo private))) r',
      '** (skg node (gnid a) (skgRepos (nodeRepo private))) a',
      '*** (skg node (gnid x) (skgRepos (nodeRepo private))) x',
      '** (skg node (gnid b) (skgRepos (nodeRepo private))) b',
      '*** (skg node (gnid x) (skgRepos (nodeRepo private)) writeProtected) x again' },
      '\n'))
    local notices = notices_of_set_repo('trusted', true)
    assert.is_false(some_notice_matches(notices, 'not changed'))
    assert.is_false(some_notice_matches(notices, 'WARNING'))
    assert.is_true(subtree_p(metadata.metadata_sexp_at_line_or_nil(3),
      '(skg node (gnid x) (skgRepos (nodeRepo trusted)))'))
    assert.is_true(subtree_p(metadata.metadata_sexp_at_line_or_nil(5),
      '(skg node (gnid x) (skgRepos (nodeRepo private)) writeProtected)'))
  end)

  it('set_repo on a write-protected occurrence warns and skips', function ()
    -- Mirrors test-set-repo-single-write-protected-warns-and-skips.
    buffer_with('* (skg node (gnid x) (skgRepos (nodeRepo private)) writeProtected) x')
    local notices = notices_of_set_repo('public', false)
    assert.is_true(some_notice_matches(notices, 'WARNING'))
    assert.is_true(subtree_p(first_metadata_sexp(),
      '(skg node (skgRepos (nodeRepo private)))'))
    assert.is_false(subtree_p(first_metadata_sexp(),
      '(skg node (skgRepos (nodeRepo public)))'))
  end)

  it('set_merge_request accepts a bare id', function ()
    buffer_with('* (skg node (gnid acquirer) (skgRepos (nodeRepo public))) title')
    metadata.set_merge_request('acquiree')
    assert.is_true(subtree_p(first_metadata_sexp(),
      '(skg node (gnid acquirer) (skgRepos (nodeRepo public))'
      .. ' (editRequest (merge acquiree)))'))
  end)

  it('set_merge_request accepts a link and replaces old requests',
     function ()
    buffer_with('* (skg node (gnid acquirer) (skgRepos (nodeRepo public))'
                .. ' (editRequest delete)) title')
    metadata.set_merge_request('[[id:acquiree][Acquiree title]]')
    local result = first_metadata_sexp()
    assert.is_true(subtree_p(result,
      '(skg node (editRequest (merge acquiree)))'))
    assert.is_false(subtree_p(result,
      '(skg node (editRequest delete))'))
  end)

  it('the writers place what they write as the server prints it',
     function ()
    -- Mirrors test-skg-write-edit-request-places-it-as-the-server-does:
    -- each command, run on a headline that carries view requests and
    -- derived facts, writes its edit request or 'noClaim' at its rank
    -- in the 'parts' list of 'shared/metadata-fields.json', replacing
    -- any edit request held and keeping a readiness request.
    local flag = require('skg.flag_state')
    local parent = '* (skg node (gnid p) (skgRepos (nodeRepo r))) p'
    local head = '** (skg (vnid 0a) node (gnid X) (skgRepos (nodeRepo r))'
    local tail = ' (viewRequests editableView) (view noWarrant)'
                 .. ' (diff (rel added))) title'
    local function line_after (parts, command)
      buffer_with(parent .. '\n' .. head .. parts .. tail)
      vim.api.nvim_win_set_cursor(0, { 2, 0 })
      command()
      return metadata.line_text(2)
    end
    local cases = {
      { 'delete', '', metadata.delete, ' (editRequest delete)' },
      { 'delete over a merge', ' (editRequest (merge y))',
        metadata.delete, ' (editRequest delete)' },
      { 'delete beside a readiness request', ' (editRequest (merge y) (ready node))',
        metadata.delete, ' (editRequest delete (ready node))' },
      { 'delete_recursive over a merge', ' (editRequest (merge y))',
        metadata.delete_recursive, ' (editRequest delete)' },
      { 'merge', '', function () metadata.set_merge_request('z') end,
        ' (editRequest (merge z))' },
      { 'merge over a delete', ' (editRequest delete)',
        function () metadata.set_merge_request('z') end,
        ' (editRequest (merge z))' },
      { 'merge over a merge', ' (editRequest (merge y))',
        function () metadata.set_merge_request('z') end,
        ' (editRequest (merge z))' },
      { 'the flag stamp', '', function () flag._apply(2, false, false) end,
        ' (editRequest (flag noSearchMatching false))' },
      { 'the flag stamp beside a readiness request', ' (editRequest (ready node))',
        function () flag._apply(2, true, false) end,
        ' (editRequest (flag noSearchMatching true) (ready node))' },
      { 'detach', '', metadata.detach, ' noClaim' },
      { 'detach beside a merge', ' (editRequest (merge y))',
        metadata.detach, ' noClaim (editRequest (merge y))' } }
    local real_notify = vim.notify
    vim.notify = function () end
    local ok, err = pcall(function ()
      for _, case in ipairs(cases) do
        assert.are.equal(head .. case[4] .. tail,
                         line_after(case[2], case[3]), case[1])
      end
    end)
    vim.notify = real_notify
    assert(ok, err)
  end)

  it('the generic edit places the parts it adds by rank', function ()
    -- Mirrors test-skg-edit-metadata-at-point-places-new-parts-by-rank:
    -- a part the edit adds lands where the server writes it
    -- (metadata_with_part_at_its_rank), as focus.lua, folds.lua and
    -- views.lua write them; a part the headline had keeps its
    -- place.
    local function edit (line, edits)
      buffer_with(line)
      metadata.edit_metadata_at_point(sexpr.read(edits))
      return metadata.line_text(1)
    end
    assert.are.same(
      { '* (skg (vnid 1) focused node (gnid k) (viewRequests editableView) (view cycle) (diff (node added))) k',
        '* (skg node (gnid k) (viewRequests editableView definitiveView) (view cycle)) k',
        '* (skg node (gnid k) noClaim (editRequest delete) (diff (node added))) k' },
      { edit('* (skg (vnid 1) node (gnid k) (view cycle) (diff (node added))) k',
             '(skg focused (viewRequests editableView))'),
        edit('* (skg node (gnid k) (viewRequests editableView) (view cycle)) k',
             '(skg (viewRequests definitiveView))'),
        edit('* (skg node (gnid k) (editRequest delete) (diff (node added))) k',
             '(skg noClaim)') })
  end)

  -- The diff view's restore requests; mirrors the restore tests of
  -- tests/elisp/test-skg-metadata.el.

  ---The message with which COMMAND refuses HEADLINE, which it must
  ---leave as it was.
  ---A buffer of HEADLINE's lines with point on the last: the lines
  ---before give it the place it stands in.
  local function buffer_at_last_line (headline)
    buffer_with(headline)
    vim.api.nvim_win_set_cursor(0, { vim.api.nvim_buf_line_count(0), 0 })
  end

  local function last_metadata_sexp ()
    return metadata.metadata_sexp_at_line_or_nil(
      vim.api.nvim_buf_line_count(0))
  end

  local function refusal_of (headline, command)
    buffer_at_last_line(headline)
    local ok, err = pcall(command)
    assert.is_false(ok)
    assert.are.equal(headline, buffer_text())
    return err
  end

  it('restore_deleted_node marks a ghost', function ()
    buffer_with('* (skg (vnid 0d) ghost (gnid g) (skgRepos (nodeRepo main))'
                .. ' (diff (node removed) (rel removed))) g')
    metadata.restore_deleted_node()
    assert.are.equal(
      '* (skg (vnid 0d) ghost (gnid g) (skgRepos (nodeRepo main))'
        .. ' (editRequest restoreNode) (diff (node removed) (rel removed)))'
        .. ' g',
      buffer_text())
  end)

  it('restore_removed_membership marks a removed member, under a node'
     .. ' or a ghost, and a folder member', function ()
    local removed_member = '** (skg (vnid 0c) node (gnid x)'
      .. ' (skgRepos (nodeRepo main)) noClaim'
      .. ' (diff (rel removed) (leaves 1 rel))) x'
    for _, headline in ipairs({
      '* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n' .. removed_member,
      '* (skg ghost (gnid g) (skgRepos (nodeRepo main))'
        .. ' (diff (node removed))) g\n'
        .. removed_member,
      '* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n'
        .. '** (skg node (gnid c) (skgRepos (nodeRepo main))) c\n'
        .. '*** (skg containerFolder) containers\n'
        .. '**** (skg (vnid 0o) node (gnid d) (skgRepos (nodeRepo main))'
        .. ' noClaim writeProtected (diff (rel removed) (properties changed)'
        .. ' (leaves 1 rel all))) d' }) do
      buffer_at_last_line(headline)
      metadata.restore_removed_membership()
      assert.is_true(subtree_p(last_metadata_sexp(),
        '(skg node (editRequest restoreRel))'))
    end
  end)

  it('the merge and restore commands keep the readiness request',
     function ()
    -- Mirrors test-skg-edit-request-commands-keep-the-readiness-request:
    -- each replaces the edit request in '(editRequest ...)' and keeps
    -- the readiness request pending there.
    local function edit_request_after (headline, command)
      buffer_at_last_line(headline)
      command()
      for _, part in ipairs(last_metadata_sexp()) do
        if sexpr.is_list(part) and part[1] == sexpr.symbol('editRequest')
        then return sexpr.to_string(part) end
      end
      return nil
    end
    assert.are.equal('(editRequest (merge b) (ready node))',
      edit_request_after(
        '* (skg node (gnid a) (skgRepos (nodeRepo main))'
        .. ' (editRequest delete (ready node))'
        .. ' (diff (node added) (leaves 1 node))) a',
        function () metadata.set_merge_request('b') end))
    assert.are.equal('(editRequest restoreNode (unready node))',
      edit_request_after(
        '* (skg ghost (gnid g) (skgRepos (nodeRepo main))'
        .. ' (editRequest (unready node)) (diff (node removed) (rel removed)'
        .. ' (leaves 1 node) (ready node))) g',
        metadata.restore_deleted_node))
    assert.are.equal('(editRequest restoreRel (ready rel))',
      edit_request_after(
        '* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n'
        .. '** (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim'
        .. ' (editRequest (ready rel)) (view noWarrant)'
        .. ' (diff (rel removed) (leaves 1 rel))) x',
        metadata.restore_removed_membership))
  end)

  it('the restore commands refuse the wrong kind of headline', function ()
    assert.are.equal(
      'This node is deleted; restore it first (:SkgRestoreDeletedNode)',
      refusal_of('* (skg ghost (gnid g) (skgRepos (nodeRepo main))'
                 .. ' (diff (node removed) (rel removed))) g',
                 metadata.restore_removed_membership))
    for _, headline in ipairs({
      '* (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (rel added))) x',
      '* (skg node (gnid x) (skgRepos (nodeRepo main))) x',
      -- a root stands for no relationship
      '* (skg node (gnid d) (skgRepos (nodeRepo main)) noClaim'
        .. ' writeProtected'
        .. ' (diff (rel removed))) d',
      -- a member is no removed member
      '* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n'
        .. '** (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (rel removed))) x',
      -- a node of the containerward chain
      '* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n'
        .. '** (skg containerFolder) containers\n'
        .. '*** (skg node (gnid c) (skgRepos (nodeRepo main)) writeProtected) c\n'
        .. '**** (skg node (gnid d) (skgRepos (nodeRepo main)) noClaim writeProtected'
        .. ' (diff (rel removed))) d',
      -- a node of the chain under another folder's member
      '* (skg node (gnid r) (skgRepos (nodeRepo main))) r\n'
        .. '** (skg subscriberFolder) subscribers\n'
        .. '*** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected) s\n'
        .. '**** (skg node (gnid d) (skgRepos (nodeRepo main)) noClaim writeProtected'
        .. ' (diff (rel removed))) d' }) do
      assert.are.equal('Only a removed membership can be restored',
        refusal_of(headline, metadata.restore_removed_membership))
    end
    for _, headline in ipairs({
      '* (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (rel removed))) x',
      '* (skg node (gnid x) (skgRepos (nodeRepo main)) (diff (node removed))) x',
      '* (skg unknown (gnid x)) x',
      '* (skg tombstone) x' }) do
      assert.are.equal('Only a node deleted since HEAD can be restored;'
                       .. ' restoreNode belongs on a ghost'
                       .. ' (:SkgRestoreDeletedNode)',
        refusal_of(headline, metadata.restore_deleted_node))
    end
  end)

  ---Open the metadata editor on HEADLINE, the only line of a buffer,
  ---and call BODY(source_buf) in the edit buffer; delete both after.
  local function with_metadata_editor_on (headline, body)
    local sexpr_edit = require('skg.sexpr_edit')
    local source = buffer_with(headline)
    sexpr_edit.view_metadata()
    local ok, err = pcall(body, source)
    if vim.api.nvim_get_current_buf() ~= source then
      vim.api.nvim_buf_delete(0, { force = true }) end
    vim.api.nvim_buf_delete(source, { force = true })
    if not ok then error(err, 0) end
  end

  ---Put the cursor on the value headline under FIELD in the edit buffer.
  local function goto_value_of (field)
    for line = 1, vim.api.nvim_buf_line_count(0) do
      if metadata.line_text(line):match('^%*+ ' .. field .. '$') then
        vim.api.nvim_win_set_cursor(0, { line + 1, 0 })
        return true end
    end
    return false
  end

  it('the metadata editor offers each kind its fields', function ()
    -- Mirrors test-skg-metadata-editor-offers-each-kind-its-fields.
    local sexpr_edit = require('skg.sexpr_edit')
    local view_requests = { 'none (default)', 'editableView',
                            'writeProtectedView', 'writeProtectedSubtreeView',
                            'fork', 'flags', 'folder' }
    local atom_values = { 'false (default)', 'true' }
    for _, case in ipairs({
      { '* (skg node (gnid x) (skgRepos (nodeRepo main))) x',
        { writeProtected = atom_values, noClaim = atom_values,
          editRequest = { 'none (default)', 'delete', 'merge',
                          'relRepo', 'flag' },
          viewRequests = view_requests } },
      { '* (skg node (gnid x) (skgRepos (nodeRepo main)) writeProtected) x',
        { editRequest = { 'none (default)', 'relRepo' } } },
      { '* (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim (diff (rel removed))) x',
        { editRequest = { 'none (default)', 'delete', 'merge',
                          'relRepo', 'flag', 'restoreRel' } } },
      { '* (skg ghost (gnid g) (skgRepos (nodeRepo main))'
          .. ' (diff (node removed))) g',
        { editRequest = { 'none (default)', 'restoreNode' } } },
      { '* (skg unknown (gnid u))',
        { editRequest = { 'none (default)', 'relRepo' } } },
      { '* (skg alias) an alias',
        { editRequest = { 'none (default)', 'relRepo' } } },
      { '* (skg tombstone)', {} },
      { '* (skg subscribeeFolder)', {} },
      { '* (skg (flag noSearchMatching)) no search matching', {} } }) do
      with_metadata_editor_on(case[1], function (source)
        for field, values in pairs(case[2]) do
          assert.is_true(goto_value_of(field), case[1] .. ' ' .. field)
          assert.are.same(values,
            sexpr_cycling.cycle_values_for_field(field),
            case[1] .. ' ' .. field)
        end
        if next(case[2]) == nil then
          for _, field in ipairs({ 'editRequest', 'viewRequests',
                                   'writeProtected', 'noClaim' }) do
            assert.is_false(goto_value_of(field), case[1] .. ' ' .. field)
          end
        end
        sexpr_edit.apply(vim.api.nvim_get_current_buf())
        assert.are.equal(case[1], vim.api.nvim_buf_get_lines(
          source, 0, 1, false)[1])
      end)
    end
  end)

  it('the metadata editor cycles to and saves values with arguments',
     function ()
    -- Mirrors test-skg-metadata-editor-cycles-and-saves-values-with-arguments.
    local sexpr_edit = require('skg.sexpr_edit')
    local config = require('skg.config')
    local real_picker = picker.completing_read_with_cycle
    local real_repo_names = config.repo_names
    local real_input = vim.fn.input
    local ok, err = pcall(function ()
      for _, case in ipairs({
        { '* (skg alias) an alias', 'editRequest', 1,
          '* (skg alias (editRequest (relRepo r))) an alias' },
        { '* (skg unknown (gnid u))', 'editRequest', 1,
          '* (skg unknown (gnid u) (editRequest (relRepo r)))' },
        { '* (skg ghost (gnid g) (skgRepos (nodeRepo main))) g', 'editRequest', 1,
          '* (skg ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest restoreNode)) g' },
        { '* (skg node (gnid x) (skgRepos (nodeRepo main))) x', 'viewRequests', 6,
          '* (skg node (gnid x) (skgRepos (nodeRepo main)) (viewRequests (folder containers))) x' },
        { '* (skg node (gnid x) (skgRepos (nodeRepo main))) x', 'editRequest', 4,
          '* (skg node (gnid x) (skgRepos (nodeRepo main)) (editRequest (flag noSearchMatching true))) x' } }) do
        local answers = { 'containers', 'noSearchMatching', 'true' }
        config.repo_names = function () return { 'r', 'main' } end
        vim.fn.input = function () return '[[id:m][a target]]' end
        picker.completing_read_with_cycle = function (_prompt, choices)
          if vim.tbl_contains(choices, 'r') then return 'r' end
          for i, answer in ipairs(answers) do
            if vim.tbl_contains(choices, answer) then
              table.remove(answers, i)
              return answer end
          end
          return nil
        end
        with_metadata_editor_on(case[1], function (source)
          assert.is_true(goto_value_of(case[2]))
          for _ = 1, case[3] do sexpr_cycling.cycle(1) end
          sexpr_edit.apply(vim.api.nvim_get_current_buf())
          assert.are.equal(case[4], vim.api.nvim_buf_get_lines(
            source, 0, 1, false)[1])
        end)
      end
    end)
    picker.completing_read_with_cycle = real_picker
    config.repo_names = real_repo_names
    vim.fn.input = real_input
    if not ok then error(err, 0) end
  end)

  it('beginning_of_line toggles between title and column zero',
     function ()
    buffer_with('* (skg node (gnid 1)) my title')
    metadata.beginning_of_line()
    local title_col = vim.api.nvim_win_get_cursor(0)[2]
    assert.are.equal(('* (skg node (gnid 1)) '):len(), title_col)
    metadata.beginning_of_line()
    assert.are.equal(0, vim.api.nvim_win_get_cursor(0)[2])
  end)
end)

describe('skg.metadata keybinding surface', function ()
  it('binds the folder commands, and nothing under p', function ()
    -- Mirrors test-skg-folder-keybindings.
    local keymaps = require('skg.keymaps')
    local by_lhs = {}
    for _, binding in ipairs(keymaps.content_view_bindings) do
      by_lhs[binding[1]] = binding[2]
    end
    assert.are.equal('RequestWriteProtected', by_lhs['sw'])
    assert.are.equal('RequestWriteProtectedRecursive', by_lhs['sW'])
    assert.are.equal('ShowFolderOfAliases', by_lhs['la'])
    assert.are.equal('ShowFolderOfOverrides', by_lhs['lo'])
    assert.are.equal('ShowFolderOfSubscribesTo', by_lhs['ls'])
    assert.are.equal('ShowFolderOfFlags', by_lhs['lp'])
    assert.is_nil(by_lhs['cb'])
    assert.are.equal('ShowFolderOfContainers', by_lhs['lc'])
    assert.are.equal('ShowFolderOfMentioners', by_lhs['lm'])
    for lhs, _ in pairs(by_lhs) do
      assert.is_nil(lhs:match('^p'), lhs)
    end
    assert.are.equal('SetMergeRequest', by_lhs['sm'])
    assert.are.equal('RestoreDeletedNode', by_lhs['sn'])
    assert.are.equal('RestoreRemovedMembership', by_lhs['sl'])
    assert.is_nil(by_lhs['rt'])
    assert.is_nil(by_lhs['rT'])
  end)

  it("opens the container folder with '<localleader>lc'", function ()
    -- The folder commands sit under 'l', mirroring Emacs's C-c l.
    local save = require('skg.save')
    helpers.install_fixture_herald_rules()
    local saves = 0
    local original_save = save.request_save_buffer
    save.request_save_buffer = function () saves = saves + 1 end
    local ok, err = pcall(function ()
      require('skg.buffer').open_org_buffer_from_text(
        '* (skg node (gnid a)) a', 'skg://keymap-lc', 'keymap-lc-view_id')
      vim.api.nvim_win_set_cursor(0, { 1, 0 })
      vim.api.nvim_feedkeys((vim.g.maplocalleader or '\\') .. 'lc', 'x', false)
      assert.are.equal(1, saves)
      assert.is_truthy(vim.api.nvim_get_current_line():find(
        '(viewRequests (folder containers))', 1, true))
    end)
    save.request_save_buffer = original_save
    helpers.wipe_skg_buffers()
    if not ok then error(err, 0) end
  end)
end)

-- Mirrors the helper cases of tests/elisp/test-skg-set-relRepo.el
-- (the relRepo gesture's classification, menu, choice and walk); the
-- request and its handler are in relRepo_info_spec.lua.
describe('skg.metadata relRepo gesture', function ()
  local original_repo_names
  before_each(function ()
    local config = require('skg.config')
    original_repo_names = config.repo_names
    config.repo_names = function ()
      return { 'public', 'private', 'trusted' } end
  end)
  after_each(function ()
    require('skg.config').repo_names = original_repo_names
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  ---The line of the headline whose metadata names GNID.
  local function line_of_id (gnid)
    for line = 1, vim.api.nvim_buf_line_count(0) do
      if metadata.line_text(line):find('(gnid ' .. gnid .. ')', 1, true)
      then return line end
    end
    error('no headline of ' .. gnid)
  end

  local function text_of_id (gnid)
    return metadata.line_text(line_of_id(gnid))
  end

  local function refusal (fn, ...)
    local ok, err = pcall(fn, ...)
    assert.is_false(ok)
    return tostring(err)
  end

  it('classifies a content child', function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg node (gnid kid) (skgRepos (nodeRepo public))) kid' })
    assert.are.same({ recorder = 'recorder', member = 'kid',
                      relation = 'contains' }, metadata.rel_at_line(2))
  end)

  it("classifies a writable folder's member", function ()
    buffer_with({
      '* (skg node (gnid anchor) (skgRepos (nodeRepo public))) anchor',
      '** (skg subscribeeFolder)',
      '*** (skg node (gnid seen) (skgRepos (nodeRepo public)) writeProtected) seen' })
    assert.are.same({ recorder = 'anchor', member = 'seen',
                      relation = 'subscribesTo' }, metadata.rel_at_line(3))
  end)

  it('classifies a write-protected child of an editable parent', function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg node (gnid kid) (skgRepos (nodeRepo public)) writeProtected) kid' })
    assert.are.same({ recorder = 'recorder', member = 'kid',
                      relation = 'contains' }, metadata.rel_at_line(2))
  end)

  it('refuses under a write-protected parent', function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public)) writeProtected) recorder',
      '** (skg node (gnid kid) (skgRepos (nodeRepo public))) kid' })
    assert.is_truthy(refusal(metadata.rel_at_line, 2):find('write-protected', 1, true))
  end)

  it("refuses on a write-protected folder's member", function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg subscriberFolder)',
      '*** (skg node (gnid sub) (skgRepos (nodeRepo public))) sub' })
    local err = refusal(metadata.rel_at_line, 3)
    assert.is_truthy(err:find('write-protected', 1, true))
    assert.is_truthy(err:find('subscriberFolder'))
  end)

  it('refuses on a root headline', function ()
    buffer_with('* (skg node (gnid x) (skgRepos (nodeRepo public))) x')
    assert.is_truthy(refusal(metadata.rel_at_line, 1):find('Root headline'))
  end)

  it('refuses off an occurrence', function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg aliasFolder) aliases' })
    refusal(metadata.rel_at_line, 2)
  end)

  it('offers the ladder from the default on', function ()
    local ladder = { 'public', 'private', 'trusted' }
    assert.are.same({ 'private', 'trusted' },
                    metadata.relRepo_choices(ladder, 'private'))
    assert.are.same(ladder, metadata.relRepo_choices(ladder, 'public'))
    assert.are.same({ 'trusted' },
                    metadata.relRepo_choices(ladder, 'trusted'))
  end)

  it('offers the whole ladder without a known default', function ()
    assert.are.same({ 'public', 'private' },
                    metadata.relRepo_choices({ 'public', 'private' }, nil))
    assert.are.same({ 'public', 'private' }, metadata.relRepo_choices(
      { 'public', 'private' }, 'unknown'))
  end)

  it('writes the relRepo request', function ()
    buffer_with('* (skg node (gnid x) (skgRepos (nodeRepo public))) x')
    metadata.apply_relRepo_choice(1, 'trusted')
    assert.is_truthy(buffer_text():find('(relRepo trusted)', 1, true))
  end)

  it('keeps an existing view fact', function ()
    buffer_with(
      '* (skg node (gnid x) (skgRepos (nodeRepo public)) (view cycle)) x')
    metadata.apply_relRepo_choice(1, 'public')
    assert.is_truthy(buffer_text():find('cycle', 1, true))
    assert.is_truthy(buffer_text():find('(relRepo public)', 1, true))
  end)

  it('removes only the pending request with no override', function ()
    buffer_with('* (skg node (gnid x) (skgRepos (nodeRepo public) (relRepo private)) (editRequest (relRepo trusted))) x')
    local message = metadata.apply_relRepo_choice(
      1, metadata.relRepo_no_override)
    assert.is_truthy(message:find('sticky'))
    assert.is_falsy(buffer_text():find('editRequest'))
    assert.is_truthy(buffer_text():find(
      '(skgRepos (nodeRepo public) (relRepo private))', 1, true))
  end)

  it('keeps the readiness request', function ()
    buffer_with('* (skg node (gnid x) (skgRepos (nodeRepo public)) (editRequest (relRepo trusted) (ready rel))) x')
    metadata.apply_relRepo_choice(1, 'private')
    assert.is_truthy(buffer_text():find(
      '(editRequest (relRepo private) (ready rel)))', 1, true))
    metadata.apply_relRepo_choice(1, metadata.relRepo_no_override)
    assert.is_truthy(buffer_text():find('(editRequest (ready rel)))', 1, true))
  end)

  it('is not blocked by a readiness request, but by a delete', function ()
    buffer_with({
      '* (skg node (gnid x) (skgRepos (nodeRepo public)) (editRequest (ready rel))) x',
      '* (skg node (gnid y) (skgRepos (nodeRepo public)) (editRequest (unready))) y',
      '* (skg node (gnid z) (skgRepos (nodeRepo public)) (editRequest delete (ready))) z' })
    assert.is_false(metadata.node_edit_request_at_line_p(1))
    metadata.apply_relRepo_choice(1, 'private')
    assert.is_truthy(metadata.line_text(1):find(
      '(editRequest (relRepo private) (ready rel)))', 1, true))
    assert.is_false(metadata.node_edit_request_at_line_p(2))
    assert.is_true(metadata.node_edit_request_at_line_p(3))
    refusal(metadata.apply_relRepo_choice, 3, 'private')
  end)

  it('changes nothing removing an absent override', function ()
    buffer_with('* (skg node (gnid x) (skgRepos (nodeRepo public))) x')
    local before = buffer_text()
    local message = metadata.apply_relRepo_choice(
      1, metadata.relRepo_no_override)
    assert.is_truthy(message:find('nothing to remove'))
    assert.are.equal(before, buffer_text())
  end)

  it("writes an alias's request in its own metadata", function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg aliasFolder)',
      '*** (skg alias) nickname' })
    metadata.apply_relRepo_choice(3, 'trusted')
    assert.are.equal('*** (skg alias (editRequest (relRepo trusted))) nickname',
                     metadata.line_text(3))
    assert.are.equal('trusted', metadata.relRepo_requested_value(3))
    metadata.apply_relRepo_choice(3, metadata.relRepo_no_override)
    assert.are.equal('*** (skg alias) nickname', metadata.line_text(3))
  end)

  it("keeps an unknown's fact apart from its request", function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg unknown (gnid na) (skgRepos (relRepo private)))' })
    metadata.apply_relRepo_choice(2, 'trusted')
    assert.is_truthy(metadata.line_text(2):find(
      'unknown (gnid na) (skgRepos (relRepo private)) (editRequest (relRepo trusted))',
      1, true))
    metadata.apply_relRepo_choice(2, metadata.relRepo_no_override)
    assert.is_truthy(metadata.line_text(2):find(
      '(skgRepos (relRepo private))', 1, true))
    assert.is_falsy(metadata.line_text(2):find('editRequest'))
  end)

  it("keeps a restricted headline's fact apart from its request", function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg restricted (gnid h) (skgRepos (relRepo private)))' })
    assert.are.equal('h', metadata.rel_at_line(2).member)
    metadata.apply_relRepo_choice(2, 'trusted')
    assert.is_truthy(metadata.line_text(2):find(
      'restricted (gnid h) (skgRepos (relRepo private)) (editRequest (relRepo trusted))',
      1, true))
  end)

  it('refuses the whole walk before writing when a delete is pending',
     function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg node (gnid a) (skgRepos (nodeRepo public))) a',
      '** (skg node (gnid b) (skgRepos (nodeRepo public)) (editRequest delete)) b' })
    refusal(metadata.set_relRepo_recursive_walk, 1, 'content', 'trusted')
    assert.is_falsy(metadata.line_text(2):find('relRepo'))
  end)

  local recursive_content_tree = {
    '* (skg node (gnid r) (skgRepos (nodeRepo public))) r',
    '** (skg node (gnid a) (skgRepos (nodeRepo public))) a',
    '*** (skg node (gnid b) (skgRepos (nodeRepo public))) b',
    '** (skg node (gnid c) (skgRepos (nodeRepo public)) noClaim) c',
    '*** (skg node (gnid d) (skgRepos (nodeRepo public))) d',
    '** (skg node (gnid e) (skgRepos (nodeRepo public)) writeProtected) e',
    '*** (skg node (gnid f) (skgRepos (nodeRepo public))) f',
    '** (skg subscribeeFolder)',
    '*** (skg node (gnid g) (skgRepos (nodeRepo public))) g',
    '**** (skg node (gnid h) (skgRepos (nodeRepo public))) h',
    '** (skg aliasFolder) aliases' }

  it("walks the kind 'content'", function ()
    buffer_with(recursive_content_tree)
    assert.are.equal(3, metadata.set_relRepo_recursive_walk(
      1, 'content', 'trusted'))
    for _, gnid in ipairs({ 'a', 'b', 'e' }) do
      assert.is_truthy(text_of_id(gnid):find('(relRepo trusted)', 1, true))
    end
    for _, gnid in ipairs({ 'r', 'c', 'd', 'f', 'g', 'h' }) do
      assert.is_falsy(text_of_id(gnid):find('relRepo')) end
  end)

  it("walks the kind 'subscribee'", function ()
    buffer_with(recursive_content_tree)
    assert.are.equal(1, metadata.set_relRepo_recursive_walk(
      1, 'subscribee', 'private'))
    assert.is_truthy(text_of_id('g'):find('(relRepo private)', 1, true))
    for _, gnid in ipairs({ 'r', 'a', 'b', 'c', 'd', 'e', 'f', 'h' }) do
      assert.is_falsy(text_of_id(gnid):find('relRepo')) end
  end)

  it("includes the start's own relationship", function ()
    buffer_with(recursive_content_tree)
    assert.are.equal(2, metadata.set_relRepo_recursive_walk(
      line_of_id('a'), 'content', 'trusted'))
    assert.is_truthy(text_of_id('a'):find('(relRepo trusted)', 1, true))
    assert.is_truthy(text_of_id('b'):find('(relRepo trusted)', 1, true))
  end)

  it("walks an overridden member and its content", function ()
    local tree = {
      '* (skg node (gnid anchor) (skgRepos (nodeRepo public))) anchor',
      '** (skg overriddenFolder)',
      '*** (skg node (gnid o) (skgRepos (nodeRepo public))) o',
      '**** (skg node (gnid oc) (skgRepos (nodeRepo public))) oc' }
    buffer_with(tree)
    assert.are.equal(1, metadata.set_relRepo_recursive_walk(
      1, 'overridden', 'private'))
    assert.is_truthy(text_of_id('o'):find('(relRepo private)', 1, true))
    assert.is_falsy(text_of_id('oc'):find('relRepo'))
    buffer_with(tree)
    assert.are.equal(1, metadata.set_relRepo_recursive_walk(
      1, 'content', 'private'))
    assert.is_truthy(text_of_id('oc'):find('(relRepo private)', 1, true))
    assert.is_falsy(text_of_id('o'):find('relRepo'))
  end)

  it('prunes a write-protected folder', function ()
    buffer_with({
      '* (skg node (gnid recorder) (skgRepos (nodeRepo public))) recorder',
      '** (skg subscriberFolder)',
      '*** (skg node (gnid s) (skgRepos (nodeRepo public))) s',
      '**** (skg node (gnid sc) (skgRepos (nodeRepo public))) sc' })
    assert.are.equal(0, metadata.set_relRepo_recursive_walk(
      1, 'content', 'trusted'))
    assert.is_falsy(buffer_text():find('relRepo'))
  end)

  it("skips a writable folder's members under a write-protected anchor",
     function ()
    local tree = {
      '* (skg node (gnid anchor) (skgRepos (nodeRepo public)) writeProtected) anchor',
      '** (skg subscribeeFolder)',
      '*** (skg node (gnid g) (skgRepos (nodeRepo public))) g' }
    buffer_with(tree)
    assert.are.equal(0, metadata.set_relRepo_recursive_walk(
      1, 'subscribee', 'trusted'))
    assert.is_falsy(text_of_id('g'):find('relRepo'))
    buffer_with(tree)
    assert.are.equal(0, metadata.set_relRepo_recursive_walk(
      2, 'subscribee', 'trusted'))
    assert.is_falsy(text_of_id('g'):find('relRepo'))
  end)

  it('removes the overrides throughout the subtree', function ()
    buffer_with({
      '* (skg node (gnid r) (skgRepos (nodeRepo public))) r',
      '** (skg node (gnid a) (skgRepos (nodeRepo public) (relRepo trusted)) (editRequest (relRepo trusted))) a',
      '*** (skg node (gnid b) (skgRepos (nodeRepo public) (relRepo private)) (editRequest (relRepo private))) b' })
    assert.are.equal(2, metadata.set_relRepo_recursive_walk(
      1, 'content', metadata.relRepo_no_override))
    assert.is_falsy(buffer_text():find('editRequest'))
  end)

  it('offers the three writable kinds among the five relations', function ()
    local tree = metadata.relationship_kind_menu_tree()
    local relations, kinds = {}, {}
    for _, relation in ipairs(tree) do
      table.insert(relations, relation.relation)
      for _, role in ipairs(relation.roles) do
        if role.kind then table.insert(kinds, role.kind) end end
    end
    assert.are.same({ 'contains', 'linksTo', 'subscribesTo',
                      'hidesFromSubs', 'overrides' }, relations)
    assert.are.same({ 'content', 'subscribee', 'overridden' }, kinds)
  end)

  it('chooses a settable kind and refuses the others', function ()
    local chosen = nil
    local notified = {}
    local original_notify = vim.notify
    vim.notify = function (message) table.insert(notified, message) end
    metadata.relationship_kind_menu_choose(nil, function (kind)
      chosen = kind end)
    metadata.relationship_kind_menu_choose(
      { role = 'subscriber', kind = false,
        description = metadata.relationship_role_menu_prose
                        .subscriber.description },
      function (kind) chosen = kind end)
    vim.notify = original_notify
    assert.is_nil(chosen)
    assert.are.equal(1, #notified)
    assert.is_truthy(notified[1]:find("child's position", 1, true))
    metadata.relationship_kind_menu_choose(
      { role = 'content', kind = 'content' },
      function (kind) chosen = kind end)
    assert.are.equal('content', chosen)
  end)

  it('runs the whole recursive gesture', function ()
    -- Mirrors test-set-relRepo-recursive-end-to-end.
    buffer_with(recursive_content_tree)
    local original_select = vim.ui.select
    local original_read = picker.completing_read_with_cycle
    local original_notify = vim.notify
    local seen_choices = nil
    vim.ui.select = function (items, _opts, on_choice)
      for _, item in ipairs(items) do
        if item.role == 'content' then return on_choice(item) end end
    end
    picker.completing_read_with_cycle = function (_prompt, choices)
      seen_choices = choices
      return 'trusted' end
    local notified = {}
    vim.notify = function (message) table.insert(notified, message) end
    local ok, err = pcall(metadata.set_relRepo_recursive)
    vim.ui.select = original_select
    picker.completing_read_with_cycle = original_read
    vim.notify = original_notify
    assert.is_true(ok, tostring(err))
    assert.are.same({ 'public', 'private', 'trusted',
                      metadata.relRepo_no_override }, seen_choices)
    for _, gnid in ipairs({ 'a', 'b', 'e' }) do
      assert.is_truthy(text_of_id(gnid):find('(relRepo trusted)', 1, true))
    end
    assert.is_falsy(text_of_id('g'):find('relRepo'))
    assert.are.same(
      { "relRepo set to 'trusted' on 3 'content' relationships. Save to apply." },
      notified)
  end)
end)
