-- Mirrors tests/elisp/test-skg-biggest-branch.el, the
-- replace-content<->link cases of test-skg-metadata.el, the
-- view-request command shapes, and the metadata edit buffer's
-- commit/cycle round trips (test-skg-insert-heading.el's empty-node
-- view cases and test-skg-metadata-editing.el's spirit).

local defaults = require('skg.sexpr.unrestrictednode_defaults')
local metadata = require('skg.metadata')
local metadata_edit = require('skg.metadata_edit')
local modify_graph = require('skg.modify_graph')
local view_requests = require('skg.view_requests')

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

---Stub the save module so auto-saving commands can run serverless.
local saved_count
local function stub_save ()
  saved_count = 0
  package.loaded['skg.save'] =
    { request_save_buffer = function () saved_count = saved_count + 1 end }
end
local function unstub_save ()
  package.loaded['skg.save'] = nil
end

---Stub the config's owned skgrepos.
local config = require('skg.config')
local real_owned_skgrepos = config.owned_repos
local function stub_owned_repos (skgrepos)
  config.owned_repos = function () return skgrepos end
end

describe('skg.modify_graph.goto_biggest_branch', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('picks the largest sibling', function ()
    buffer_with(table.concat({
      '* root',
      '** small', '*** a',
      '** big', '*** b', '*** c', '**** d',
      '** medium', '*** e', '*** f' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 0 }) -- on 'small'
    modify_graph.goto_biggest_branch()
    assert.are.equal(4, vim.api.nvim_win_get_cursor(0)[1]) -- 'big'
  end)

  it('falls back to children when there are no siblings', function ()
    buffer_with(table.concat({
      '* only root',
      '** child one', '*** x',
      '** child two', '*** y', '*** z' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    modify_graph.goto_biggest_branch()
    assert.are.equal(4, vim.api.nvim_win_get_cursor(0)[1])
  end)

  it('errors when there is nothing to compare', function ()
    buffer_with('* alone')
    assert.has_error(function ()
      modify_graph.goto_biggest_branch() end)
  end)
end)

describe('skg.modify_graph replacements', function ()
  before_each(function ()
    stub_save()
    stub_owned_repos({ 'public' })
  end)
  after_each(function ()
    unstub_save()
    config.owned_repos = real_owned_skgrepos
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  local container_and_leaf = table.concat({
    '* (skg (node (id parent) (repo public))) container',
    '** (skg (node (id child) (repo public))) the leaf title',
    'leaf body',
    '*** (skg aliasFolder) non-vognode viewnodes under it' }, '\n')

  it('replaces content with a link, from the body too', function ()
    buffer_with(container_and_leaf)
    vim.api.nvim_win_set_cursor(0, { 3, 2 }) -- in the body
    modify_graph.replace_content_with_link()
    assert.are.equal(table.concat({
      '* (skg (node (id parent) (repo public))) container',
      '** [[id:child][the leaf title]]' }, '\n'), buffer_text())
    assert.are.equal(1, saved_count)
  end)

  it('rejects a node with no id', function ()
    buffer_with(table.concat({
      '* (skg (node (id parent) (repo public))) container',
      '** (skg (node (repo public))) no id here' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    local ok, err = pcall(modify_graph.replace_content_with_link)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('no ID', 1, true))
  end)

  it('rejects a foreign container', function ()
    buffer_with(table.concat({
      '* (skg (node (id parent) (repo foreign-src))) container',
      '** (skg (node (id child) (repo public))) leaf' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    local ok, err = pcall(modify_graph.replace_content_with_link)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('not owned', 1, true))
  end)

  it('rejects a write-protected container', function ()
    buffer_with(table.concat({
      '* (skg (node (id parent) (repo public) writeProtected)) container',
      '** (skg (node (id child) (repo public))) leaf' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    local ok, err = pcall(modify_graph.replace_content_with_link)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('write-protected', 1, true))
  end)

  it('replaces a link with content, warning about existing nodes',
     function ()
    buffer_with(table.concat({
      '* (skg (node (id parent) (repo public))) container',
      '** (skg (node (id old-node) (repo public)))'
      .. ' see [[id:target][target label]]' }, '\n'))
    local notified = {}
    local original_notify = vim.notify
    vim.notify = function (msg) table.insert(notified, msg) end
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    modify_graph.replace_link_with_content()
    vim.notify = original_notify
    assert.are.equal(table.concat({
      '* (skg (node (id parent) (repo public))) container',
      '** (skg (node (id target) writeProtected (viewRequests'
      .. ' editableView))) target label' }, '\n'), buffer_text())
    local warned = false
    for _, msg in ipairs(notified) do
      if tostring(msg):find('orphan') then warned = true end
    end
    assert.is_true(warned)
    assert.are.equal(1, saved_count)
  end)

  it('rejects a leaf with several links, a non-id link, and'
     .. ' descendants', function ()
    buffer_with(table.concat({
      '* (skg (node (id parent) (repo public))) container',
      '** (skg (node (repo public))) [[id:a][one]] and'
      .. ' [[id:b][two]]' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    local ok, err = pcall(modify_graph.replace_link_with_content)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('exactly one link', 1, true))
    buffer_with(table.concat({
      '* (skg (node (id parent) (repo public))) container',
      '** (skg (node (repo public))) [[https://x][web link]]' },
      '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    ok, err = pcall(modify_graph.replace_link_with_content)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('not an id link', 1, true))
    buffer_with(table.concat({
      '* (skg (node (id parent) (repo public))) container',
      '** (skg (node (repo public))) [[id:a][one]]',
      '*** a descendant' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    ok, err = pcall(modify_graph.replace_link_with_content)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('viewdescendants', 1, true))
  end)
end)

describe('skg.view_requests', function ()
  before_each(stub_save)
  after_each(function ()
    unstub_save()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('stamps the request atom and auto-saves', function ()
    buffer_with('* (skg (node (id n1))) title')
    view_requests.show_folderOf_aliases()
    assert.is_truthy(buffer_text():find(
      '(viewRequests (folder aliases))', 1, true))
    assert.are.equal(1, saved_count)
    buffer_with('* (skg (node (id n2))) title')
    view_requests.show_containerward_tree()
    assert.is_truthy(buffer_text():find(
      '(viewRequests (roleTree container))', 1, true))
  end)

  it('set_editable stamps without saving', function ()
    buffer_with('* (skg (node (id n3) writeProtected)) title')
    view_requests.set_editable()
    assert.is_truthy(buffer_text():find(
      '(viewRequests editableView)', 1, true))
    assert.are.equal(0, saved_count)
  end)

  it('fork refuses a modified buffer', function ()
    local buf = buffer_with('* (skg (node (id n4))) title')
    vim.bo[buf].modified = true
    local notified = nil
    local original_notify = vim.notify
    vim.notify = function (msg) notified = msg end
    view_requests.fork_node()
    vim.notify = original_notify
    assert.is_truthy(tostring(notified):find('Save the buffer'))
    assert.are.equal(0, saved_count)
    vim.bo[buf].modified = false
  end)
end)

describe('skg.metadata_edit', function ()
  before_each(function () stub_owned_repos({ 'public' }) end)
  after_each(function ()
    config.owned_repos = real_owned_skgrepos
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      local name = vim.api.nvim_buf_get_name(buf)
      if name:find('skg://metadata%-edit') then
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end
    end
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('opens the expanded view and commits an edit back', function ()
    local source_buf = buffer_with(
      '* (skg (node (id abc) (repo public))) my title')
    metadata_edit.edit_metadata()
    local edit_buf = vim.api.nvim_get_current_buf()
    assert.is_truthy(vim.api.nvim_buf_get_name(edit_buf)
                     :find('metadata%-edit'))
    -- Flip write-protected's value to true, then commit.
    for line = 1, vim.api.nvim_buf_line_count(edit_buf) do
      if metadata.line_text(line):match('^%*+ writeProtected$') then
        vim.api.nvim_buf_set_lines(edit_buf, line, line + 1, false,
                                   { '**** true' })
        break
      end
    end
    metadata_edit.commit(edit_buf)
    assert.are.equal(source_buf, vim.api.nvim_get_current_buf())
    assert.are.equal(
      '* (skg (node (id abc) (repo public) writeProtected)) my title',
      buffer_text())
  end)

  it('builds the empty-node view and commits just the repo',
     function ()
    -- Mirrors the empty-node skeleton cases of
    -- test-skg-insert-heading-repo-prompt.el.
    local source_buf = buffer_with('* just a plain headline')
    metadata_edit.edit_metadata()
    local edit_buf = vim.api.nvim_get_current_buf()
    local text = table.concat(
      vim.api.nvim_buf_get_lines(edit_buf, 0, -1, false), '\n')
    assert.is_truthy(text:find('%* title'))
    assert.is_truthy(text:find('just a plain headline', 1, true))
    assert.is_truthy(text:find('%*%*%*%* public'))
    metadata_edit.commit(edit_buf)
    assert.are.equal(source_buf, vim.api.nvim_get_current_buf())
    assert.are.equal(
      '* (skg (node (repo public))) just a plain headline',
      buffer_text())
  end)

  it('cycles field values by their parent field', function ()
    buffer_with(table.concat({
      metadata_edit.help_text, '',
      '* skg', '** node', '*** affectsParent',
      '**** true (default)' }, '\n'))
    vim.api.nvim_win_set_cursor(0, { 6, 0 })
    metadata_edit.cycle(1)
    assert.are.equal('**** false', metadata.line_text(6))
    metadata_edit.cycle(-1)
    assert.are.equal('**** true (default)', metadata.line_text(6))
  end)
end)

describe('skg.ancestry and view_new_empty', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('shows only the ancestry of point', function ()
    -- The docstring example of skg-view-ancestry.
    local buf = buffer_with(table.concat({
      '* a', '** b', '** c', '*** d', '*** e', '*** f' }, '\n'))
    vim.bo[buf].filetype = 'org'
    vim.api.nvim_win_set_cursor(0, { 5, 0 }) -- on 'e'
    require('skg.ancestry').view_ancestry()
    assert.are.equal('* a\n** c\n*** e', buffer_text())
  end)

  it('view_new_empty opens a blank skg content view', function ()
    require('skg.view_new_empty').view_new_empty()
    assert.are.equal('', buffer_text())
    assert.is_truthy(vim.b[vim.api.nvim_get_current_buf()]
                     .skg_view_id)
    assert.is_true(vim.b[vim.api.nvim_get_current_buf()]
                   .skg_content_view)
  end)
end)
