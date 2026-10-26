-- Mirrors tests/elisp/test-skg-metadata.el (the parts belonging to
-- this module; the replace-content<->link cases live with
-- skg.modify_graph and the minibuffer linkstack cases with
-- skg.linkstack) and all of test-skg-metadata-editing.el.

local compare = require('skg.sexpr.compare')
local metadata = require('skg.metadata')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')

local function buffer_with (text)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  return buf
end

local function buffer_text ()
  return table.concat(
    vim.api.nvim_buf_get_lines(0, 0, -1, false), '\n')
end

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

  it('parses metadata into alist and bare values', function ()
    -- Mirrors test-skg-parse-metadata-sexp.
    local simple = metadata.parse_metadata_sexp('(skg (gnid 1) value)')
    assert.are.same({ { key = 'gnid', value = '1' } }, simple.alist)
    assert.are.same({ 'value' }, simple.bare_values)
    local complex =
      metadata.parse_metadata_sexp('(skg a b (c d) (e f))')
    assert.are.same({ { key = 'c', value = 'd' },
                      { key = 'e', value = 'f' } }, complex.alist)
    assert.are.same({ 'a', 'b' }, complex.bare_values)
  end)

  it('keeps graphStats/view/diff sub-sexps whole', function ()
    local parsed = metadata.parse_metadata_sexp(
      '(skg node x (view cycle repoBoundary) (diff (rel added) (ready)))')
    local found = {}
    for _, kv in ipairs(parsed.alist) do found[kv.key] = kv.value end
    assert.are.equal('(view cycle repoBoundary)', found.view)
    assert.are.equal('(diff (rel added) (ready))', found.diff)
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

  it('delete_recursive skips non-unrestrictedNode descendants', function ()
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

  it('set_repo_recursive prunes non-members', function ()
    -- Mirrors test-skg-set-repo-recursive-prunes-non-members.
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
        .. ' (diff (node removed) (rel removed)) (editRequest restoreNode))'
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
      'This node is deleted; restore it first (skg-restore-deleted-node)',
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
                       .. ' (skg-restore-deleted-node)',
        refusal_of(headline, metadata.restore_deleted_node))
    end
  end)

  ---Open the metadata editor on HEADLINE, the only line of a buffer,
  ---and call BODY(source_buf) in the edit buffer; delete both after.
  local function with_metadata_editor_on (headline, body)
    local metadata_edit = require('skg.metadata_edit')
    local source = buffer_with(headline)
    metadata_edit.edit_metadata()
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
    local metadata_edit = require('skg.metadata_edit')
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
        { editRequest = { 'none (default)', 'restoreNode' },
          viewRequests = { 'none (default)', 'tombstone' } } },
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
          local value = vim.trim(metadata.line_text(
            metadata.current_line_number()):gsub('^%*+', ''))
          assert.are.same(values,
            metadata_edit.cycle_values_for_field(field, value),
            case[1] .. ' ' .. field)
        end
        if next(case[2]) == nil then
          for _, field in ipairs({ 'editRequest', 'viewRequests',
                                   'writeProtected', 'noClaim' }) do
            assert.is_false(goto_value_of(field), case[1] .. ' ' .. field)
          end
        end
        metadata_edit.apply(vim.api.nvim_get_current_buf())
        assert.are.equal(case[1], vim.api.nvim_buf_get_lines(
          source, 0, 1, false)[1])
      end)
    end
  end)

  it('the metadata editor cycles to and saves values with arguments',
     function ()
    -- Mirrors test-skg-metadata-editor-cycles-and-saves-values-with-arguments.
    local metadata_edit = require('skg.metadata_edit')
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
        { '* (skg ghost (gnid g) (skgRepos (nodeRepo main))) g', 'viewRequests', 1,
          '* (skg ghost (gnid g) (skgRepos (nodeRepo main)) (viewRequests tombstone)) g' },
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
          for _ = 1, case[3] do metadata_edit.cycle(1) end
          metadata_edit.apply(vim.api.nvim_get_current_buf())
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
    assert.are.equal('RequestGhostTombstone', by_lhs['rt'])
    assert.are.equal('RequestBufferGhostsTombstone', by_lhs['rT'])
  end)

  it("opens the container folder with '<localleader>lc'", function ()
    -- The folder commands sit under 'l', mirroring Emacs's C-c l.
    local helpers = dofile(
      debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
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

describe('skg.metadata editing helpers', function ()
  -- Mirrors tests/elisp/test-skg-metadata-editing.el.
  local example_data = table.concat({
    '* 1',
    '1 body',
    '* (skg) 2',
    '* (skg (k v) value) 3',
    '3 body',
    '* (skg value (k v)) 4' }, '\n')

  local function apply_to_all_lines (fn, argument)
    for line = 1, vim.api.nvim_buf_line_count(0) do
      vim.api.nvim_win_set_cursor(0, { line, 0 })
      fn(argument)
    end
  end

  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('deletes a kv-pair whose key exists', function ()
    buffer_with(example_data)
    apply_to_all_lines(metadata.delete_kv_pair_from_metadata_by_key,
                       'k')
    assert.are.equal(table.concat({
      '* 1', '1 body', '* (skg) 2', '* (skg value) 3', '3 body',
      '* (skg value) 4' }, '\n'), buffer_text())
  end)

  it('leaves metadata alone when the key is na', function ()
    -- "Alone" up to normalization: reconstruction orders kv-pairs
    -- before bare values, exactly as the elisp expectations show
    -- (input '(skg value (k v))' comes back '(skg (k v) value)').
    buffer_with(example_data)
    apply_to_all_lines(metadata.delete_kv_pair_from_metadata_by_key,
                       'h')
    assert.are.equal(table.concat({
      '* 1', '1 body', '* (skg) 2', '* (skg (k v) value) 3', '3 body',
      '* (skg (k v) value) 4' }, '\n'), buffer_text())
  end)

  it('deletes a bare value that exists', function ()
    buffer_with(example_data)
    apply_to_all_lines(metadata.delete_value_from_metadata, 'value')
    assert.are.equal(table.concat({
      '* 1', '1 body', '* (skg) 2', '* (skg (k v)) 3', '3 body',
      '* (skg (k v)) 4' }, '\n'), buffer_text())
  end)

  it('leaves metadata alone when the value is na', function ()
    buffer_with(example_data)
    apply_to_all_lines(metadata.delete_value_from_metadata,
                       'nonexistent')
    assert.are.equal(table.concat({
      '* 1', '1 body', '* (skg) 2', '* (skg (k v) value) 3', '3 body',
      '* (skg (k v) value) 4' }, '\n'), buffer_text())
  end)
end)
