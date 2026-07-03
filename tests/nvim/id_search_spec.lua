-- Mirrors the navigation half of tests/elisp/test-skg-id-search.el
-- (the linkstack half is mirrored in linkstack_spec.lua) plus the
-- label-reduction cases of test-skg-search-make-link.el.

local id_search = require('skg.id_search')

local navigation_text = table.concat({
  '* no id',
  '* (skg (node (id 1))) 1 has a [[id:2][link to 2]]',
  '* (skg (node (id 2))) 2',
  '* no id',
  '' }, '\n')

local function buffer_with (text)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  return buf
end

local function cursor ()
  return vim.api.nvim_win_get_cursor(0)
end

describe('skg.id_search navigation', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('id_next hops metadata, link, metadata, then stays', function ()
    buffer_with(navigation_text)
    id_search.id_next()
    assert.are.same({ 2, 2 }, cursor())  -- line 2's (skg
    id_search.id_next()
    assert.are.same({ 2, 30 }, cursor()) -- line 2's [[id:2...
    id_search.id_next()
    assert.are.same({ 3, 2 }, cursor())  -- line 3's (skg
    id_search.id_next()
    assert.are.same({ 3, 2 }, cursor())  -- nowhere further
  end)

  it('id_prev hops backward and then stays', function ()
    buffer_with(navigation_text)
    vim.api.nvim_win_set_cursor(0, { 5, 0 })
    id_search.id_prev()
    assert.are.same({ 3, 2 }, cursor())
    id_search.id_prev()
    assert.are.same({ 2, 30 }, cursor())
    id_search.id_prev()
    assert.are.same({ 2, 2 }, cursor())
    id_search.id_prev()
    assert.are.same({ 2, 2 }, cursor())
  end)

  it('finds the nearest id from anywhere on the line', function ()
    buffer_with(navigation_text)
    -- From title text before the link: the probe order (point, then
    -- next-on-line, then previous) finds the LINK first, exactly as
    -- the elisp skg-nearest-id would.
    vim.api.nvim_win_set_cursor(0, { 2, 25 })
    local hit = id_search.nearest_id()
    assert.are.equal('2', hit.id)
    -- The cursor is restored.
    assert.are.same({ 2, 25 }, cursor())
    -- From a spot with nothing after it on the line, the metadata id
    -- behind point wins.
    vim.api.nvim_win_set_cursor(0, { 3, 23 })
    assert.are.equal('2', id_search.nearest_id().id)
    vim.api.nvim_win_set_cursor(0, { 1, 3 })
    assert.is_nil(id_search.nearest_id())
  end)

  it('recognizes UUIDs, filenames, links and metadata at point',
     function ()
    buffer_with(table.concat({
      'raw 3861db2c-7f16-4814-b403-52b5e05d5e0a uuid',
      'file 557a869b-02ba-4c59-a5d3-5fb469a12353.skg here',
      '* (skg (node (id meta-id))) title with [[id:link-id][lab]]' },
      '\n'))
    vim.api.nvim_win_set_cursor(0, { 1, 10 })
    assert.are.equal('3861db2c-7f16-4814-b403-52b5e05d5e0a',
                     id_search.id_at_point().id)
    vim.api.nvim_win_set_cursor(0, { 2, 10 })
    assert.are.equal('557a869b-02ba-4c59-a5d3-5fb469a12353',
                     id_search.id_at_point().id)
    vim.api.nvim_win_set_cursor(0, { 3, 5 }) -- inside metadata
    assert.are.equal('meta-id', id_search.id_at_point().id)
    vim.api.nvim_win_set_cursor(0, { 3, 42 }) -- inside the link
    assert.are.equal('link-id', id_search.id_at_point().id)
  end)
end)

describe('skg.id_search.replace_links_with_labels', function ()
  -- Mirrors the four label-reduction cases of
  -- test-skg-search-make-link.el.
  it('passes through text with no link', function ()
    assert.are.equal('plain text',
      id_search.replace_links_with_labels('plain text'))
  end)

  it('reduces a single link', function ()
    assert.are.equal('before label after',
      id_search.replace_links_with_labels(
        'before [[id:x][label]] after'))
  end)

  it('reduces multiple links', function ()
    assert.are.equal('one two',
      id_search.replace_links_with_labels(
        '[[id:1][one]] [[id:2][two]]'))
  end)

  it('reduces mixed text and links', function ()
    assert.are.equal('a b c d',
      id_search.replace_links_with_labels(
        'a [[id:1][b]] c [[id:2][d]]'))
  end)
end)
