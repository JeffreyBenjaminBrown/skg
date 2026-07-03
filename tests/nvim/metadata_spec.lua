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
local function metadata_sexp_by_id (id)
  for line = 1, vim.api.nvim_buf_line_count(0) do
    local sexp = metadata.metadata_sexp_at_line_or_nil(line)
    if sexp and metadata.node_id(sexp) == id then return sexp end
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
      '** (skg (node (id 1))) some title')
    assert.are.equal('(skg (node (id 1)))', with_meta.metadata)
    assert.are.equal('some title', with_meta.title)
  end)

  it('parses metadata into alist and bare values', function ()
    -- Mirrors test-skg-parse-metadata-sexp.
    local simple = metadata.parse_metadata_sexp('(skg (id 1) value)')
    assert.are.same({ { key = 'id', value = '1' } }, simple.alist)
    assert.are.same({ 'value' }, simple.bare_values)
    local complex =
      metadata.parse_metadata_sexp('(skg a b (c d) (e f))')
    assert.are.same({ { key = 'c', value = 'd' },
                      { key = 'e', value = 'f' } }, complex.alist)
    assert.are.same({ 'a', 'b' }, complex.bare_values)
  end)

  it('keeps graphStats/viewStats sub-sexps whole', function ()
    local parsed = metadata.parse_metadata_sexp(
      '(skg (node x) (viewStats cycle (sourceHerald h)))')
    local found = nil
    for _, kv in ipairs(parsed.alist) do
      if kv.key == 'viewStats' then found = kv.value end
    end
    assert.are.equal('(viewStats cycle (sourceHerald h))', found)
  end)
end)

describe('skg.metadata commands', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('set_indefinitive adds indef to the node section', function ()
    -- Mirrors test-skg-set-indefinitive's three cases.
    buffer_with('* (skg (node (id 1))) title')
    metadata.set_indefinitive()
    local result = first_metadata_sexp()
    assert.is_true(subtree_p(result, '(skg (node indef))'))
    assert.is_true(subtree_p(result, '(skg (node (id 1)))'))
    buffer_with('* plain title')
    metadata.set_indefinitive()
    assert.is_true(subtree_p(first_metadata_sexp(),
                             '(skg (node indef))'))
  end)

  it('delete marks the node for deletion', function ()
    buffer_with('* (skg (node (id 1))) title')
    metadata.delete()
    assert.is_true(subtree_p(first_metadata_sexp(),
      '(skg (node (editRequest delete)))'))
  end)

  it('delete_recursive skips non-activeNode descendants', function ()
    buffer_with(table.concat({
      '* (skg (node (id root))) root',
      '** (skg (node (id child))) child',
      '** (skg aliasCol) aliases',
      '*** some alias',
      '* (skg (node (id sibling))) sibling' }, '\n'))
    metadata.delete_recursive()
    assert.is_true(subtree_p(metadata_sexp_by_id('root'),
      '(skg (node (editRequest delete)))'))
    assert.is_true(subtree_p(metadata_sexp_by_id('child'),
      '(skg (node (editRequest delete)))'))
    assert.is_false(subtree_p(
      metadata.metadata_sexp_at_line_or_nil(3),
      '(skg (node (editRequest delete)))'))
    assert.is_false(subtree_p(metadata_sexp_by_id('sibling'),
      '(skg (node (editRequest delete)))'))
  end)

  it('strips metadata from org text', function ()
    -- Mirrors test-skg-strip-metadata-from-org-text.
    assert.are.equal(
      '* root\nbody line\n** alias title\n** plain child\n',
      metadata.strip_metadata_from_org_text(
        '* (skg (node (id 1) (source public))) root\n'
        .. 'body line\n'
        .. '** (skg alias (staged newM)) alias title\n'
        .. '** plain child\n'))
  end)

  it('view_without_metadata opens a stripped projection', function ()
    buffer_with('* (skg (node (id 1))) root\n** (skg (node (id 2))) child')
    vim.fn.setpos("'<", { 0, 1, 1, 0 })
    vim.fn.setpos("'>", { 0, 2, 1, 0 })
    metadata.view_without_metadata()
    assert.are.equal('* root\n** child', buffer_text())
    assert.are.equal('org', vim.bo.filetype)
  end)

  it('set_source replaces source and refreshes the herald', function ()
    -- Mirrors test-skg-set-source.
    buffer_with('* (skg (node (id 1) (source public))) title')
    local original = picker.prompt_for_source_change
    picker.prompt_for_source_change = function (current)
      assert.are.equal('public', current)
      return 'private'
    end
    metadata.set_source()
    picker.prompt_for_source_change = original
    local result = first_metadata_sexp()
    assert.is_true(subtree_p(result,
      '(skg (node (id 1) (source private)))'))
    assert.is_true(subtree_p(result,
      '(skg (node (viewStats (sourceHerald ⌂:private))))'))
    assert.is_false(subtree_p(result, '(skg (node (source public)))'))
  end)

  it('set_source_recursive prunes non-content parentIs', function ()
    -- Mirrors test-skg-set-source-recursive-prunes-non-content-parentIs.
    buffer_with(table.concat({
      '* (skg (node (id root) (source public) (parentIs absent))) root',
      '** (skg (node (id content-child) (source public))) content child',
      '*** (skg (node (id content-grandchild) (source public))) content grandchild',
      '** (skg (node (id mismatched-content) (source foreign))) mismatched content',
      '*** (skg (node (id public-under-mismatch) (source public))) public under mismatch',
      '** (skg (node (id link-child) (source public) (parentIs independent) (birth backpath linkSource))) link child',
      '*** (skg (node (id under-link) (source public))) under link',
      '** (skg aliasCol) aliases',
      '*** (skg (node (id under-scaffold) (source public))) under scaffold' },
      '\n'))
    local original = picker.prompt_for_source_change
    picker.prompt_for_source_change = function (current)
      assert.are.equal('public', current)
      return 'private'
    end
    metadata.set_source_recursive()
    picker.prompt_for_source_change = original
    for _, id in ipairs({ 'root', 'content-child',
                          'content-grandchild',
                          'public-under-mismatch' }) do
      assert.is_true(subtree_p(metadata_sexp_by_id(id),
        '(skg (node (source private)))'), id)
      assert.is_true(subtree_p(metadata_sexp_by_id(id),
        '(skg (node (viewStats (sourceHerald ⌂:private))))'), id)
    end
    assert.is_true(subtree_p(metadata_sexp_by_id('mismatched-content'),
      '(skg (node (source foreign)))'))
    for _, id in ipairs({ 'link-child', 'under-link',
                          'under-scaffold' }) do
      assert.is_true(subtree_p(metadata_sexp_by_id(id),
        '(skg (node (source public)))'), id)
      assert.is_false(subtree_p(metadata_sexp_by_id(id),
        '(skg (node (source private)))'), id)
    end
  end)

  it('set_merge_request accepts a bare id', function ()
    buffer_with('* (skg (node (id acquirer) (source public))) title')
    metadata.set_merge_request('acquiree')
    assert.is_true(subtree_p(first_metadata_sexp(),
      '(skg (node (id acquirer) (source public)'
      .. ' (editRequest (merge acquiree))))'))
  end)

  it('set_merge_request accepts a link and replaces old requests',
     function ()
    buffer_with('* (skg (node (id acquirer) (source public)'
                .. ' (editRequest delete))) title')
    metadata.set_merge_request('[[id:acquiree][Acquiree title]]')
    local result = first_metadata_sexp()
    assert.is_true(subtree_p(result,
      '(skg (node (editRequest (merge acquiree))))'))
    assert.is_false(subtree_p(result,
      '(skg (node (editRequest delete)))'))
  end)

  it('beginning_of_line toggles between title and column zero',
     function ()
    buffer_with('* (skg (node (id 1))) my title')
    metadata.beginning_of_line()
    local title_col = vim.api.nvim_win_get_cursor(0)[2]
    assert.are.equal(('* (skg (node (id 1))) '):len(), title_col)
    metadata.beginning_of_line()
    assert.are.equal(0, vim.api.nvim_win_get_cursor(0)[2])
  end)
end)

describe('skg.metadata keybinding surface', function ()
  it('binds the collection and path commands distinctly', function ()
    -- Mirrors test-skg-collection-and-path-keybindings: the
    -- UPPER/lower path letters select opposite roles.
    local keymaps = require('skg.keymaps')
    local by_lhs = {}
    for _, binding in ipairs(keymaps.content_view_bindings) do
      by_lhs[binding[1]] = binding[2]
    end
    assert.are.equal('ShowCollectionAliases', by_lhs['ca'])
    assert.are.equal('ShowCollectionOverrides', by_lhs['co'])
    assert.are.equal('ShowCollectionSubscribes', by_lhs['cs'])
    assert.are.equal('ShowPathsThroughContainers', by_lhs['pC'])
    assert.are.equal('ShowPathsThroughLinkSources', by_lhs['pL'])
    assert.are.equal('ShowPathsThroughLinkDests', by_lhs['pl'])
    assert.are.equal('ShowPathsThroughOverriders', by_lhs['pO'])
    assert.are.equal('ShowPathsThroughOverridden', by_lhs['po'])
    assert.are.equal('ShowPathsThroughSubscribees', by_lhs['ps'])
    assert.are.equal('SetMergeRequest', by_lhs['sm'])
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

  it('leaves metadata alone when the key is absent', function ()
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

  it('leaves metadata alone when the value is absent', function ()
    buffer_with(example_data)
    apply_to_all_lines(metadata.delete_value_from_metadata,
                       'nonexistent')
    assert.are.equal(table.concat({
      '* 1', '1 body', '* (skg) 2', '* (skg (k v) value) 3', '3 body',
      '* (skg (k v) value) 4' }, '\n'), buffer_text())
  end)
end)
