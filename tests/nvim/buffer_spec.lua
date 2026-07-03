-- Mirrors the applicable parts of tests/elisp/test-skg-close-all.el
-- and test-skg-view-uri-persistence.el (whose permanent-local
-- concerns mostly dissolve here: vim.b survives filetype changes by
-- construction), plus the buffer-naming helpers of skg-buffer.el.

local buffer = require('skg.buffer')

describe('skg.buffer naming', function ()
  it('extracts the top headline title, metadata stripped', function ()
    assert.are.equal('hello world',
      buffer.extract_top_headline_title(
        '* (skg (node (id abc) (source main))) hello world\n** child'))
    assert.are.equal('plain title',
      buffer.extract_top_headline_title('* plain title'))
    assert.is_nil(buffer.extract_top_headline_title('no headline here'))
  end)

  it('shortens id links in buffer names', function ()
    assert.are.equal('[[label]] rest',
      buffer.normalize_buffer_name_links('[[id:abc][label]] rest'))
  end)

  it('sanitizes and truncates names', function ()
    assert.are.equal('a b',
      buffer.sanitize_buffer_name('  a\nb  '))
    local long = string.rep('x', 100)
    local sanitized = buffer.sanitize_buffer_name(long)
    assert.are.equal(80, #sanitized)
    assert.are.equal('...', sanitized:sub(-3))
  end)

  it('names content views from their title', function ()
    assert.are.equal('skg://my title',
      buffer.content_view_buffer_name(
        '* (skg (node (id x))) my title\nbody'))
    assert.has_error(function ()
      buffer.content_view_buffer_name('not org at all') end)
  end)

  it('names search buffers from the terms', function ()
    assert.are.equal('skg://?dog cat',
                     buffer.search_buffer_name('dog cat'))
  end)
end)

describe('skg.buffer lifecycle and registry', function ()
  local function wipe_skg_buffers ()
    for _, buf in ipairs(buffer.all_skg_buffers()) do
      vim.bo[buf].modified = false
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end

  before_each(function ()
    require('skg.herald_rules').install_rules(
      require('skg.sexpr.parse').read('(skg (node (id)))'))
    wipe_skg_buffers()
  end)
  after_each(wipe_skg_buffers)

  it('opens a view buffer with uri, filetype and clean state',
     function ()
    local buf = buffer.open_org_buffer_from_text(
      '* (skg (node (id x))) t\n** child', 'skg://t', 'uri-1')
    assert.are.equal('uri-1', vim.b[buf].skg_view_uri)
    assert.are.equal('org', vim.bo[buf].filetype)
    assert.are.equal('acwrite', vim.bo[buf].buftype)
    assert.is_false(vim.bo[buf].modified)
    assert.are.equal(buf, vim.api.nvim_get_current_buf())
  end)

  it('reuses the buffer bearing the same name', function ()
    local first = buffer.open_org_buffer_from_text(
      '* t\nold', 'skg://t', 'uri-1')
    local second = buffer.open_org_buffer_from_text(
      '* t\nnew', 'skg://t', 'uri-2')
    assert.are.equal(first, second)
    assert.are.equal('uri-2', vim.b[second].skg_view_uri)
    assert.are.equal('new',
      vim.api.nvim_buf_get_lines(second, 1, 2, false)[1])
  end)

  it('finds buffers by uri', function ()
    local buf = buffer.open_org_buffer_from_text(
      '* t', 'skg://t', 'uri-findme')
    assert.are.equal(buf, buffer.find_buffer_by_uri('uri-findme'))
    assert.is_nil(buffer.find_buffer_by_uri('no-such-uri'))
  end)

  it('generates fresh uuids when no uri is supplied', function ()
    local buf = buffer.open_org_buffer_from_text(
      '* t2', 'skg://t2', nil)
    local uri = vim.b[buf].skg_view_uri
    assert.is_truthy(uri:match(
      '^%x%x%x%x%x%x%x%x%-%x%x%x%x%-4%x%x%x%-%x%x%x%x%-%x+$'))
  end)

  it('the uri survives a filetype change', function ()
    -- The analog of permanent-local persistence across mode changes.
    local buf = buffer.open_org_buffer_from_text(
      '* t3', 'skg://t3', 'uri-3')
    vim.bo[buf].filetype = 'text'
    vim.bo[buf].filetype = 'org'
    assert.are.equal('uri-3', vim.b[buf].skg_view_uri)
  end)

  it('close_all spares a real file the user merely opened', function ()
    -- Mirrors test-skg-close-all.el's core concern.
    local view = buffer.open_org_buffer_from_text(
      '* doomed', 'skg://doomed', 'uri-doomed')
    local file_buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(file_buf, vim.fn.tempname() .. '.skg.org')
    vim.api.nvim_buf_set_lines(file_buf, 0, -1, false,
      { '* (skg (node (id real))) looks skg-ish but is a file' })
    vim.bo[file_buf].filetype = 'org'
    buffer.close_all_skg_buffers()
    assert.is_false(vim.api.nvim_buf_is_valid(view))
    assert.is_true(vim.api.nvim_buf_is_valid(file_buf))
    vim.api.nvim_buf_delete(file_buf, { force = true })
  end)

  it('warns when another view has unsaved edits', function ()
    local warned = nil
    local original_notify = vim.notify
    vim.notify = function (msg) warned = msg end
    local first = buffer.open_org_buffer_from_text(
      '* one', 'skg://one', 'uri-one')
    vim.api.nvim_buf_set_lines(first, 1, 1, false, { 'edited' })
    assert.is_true(vim.bo[first].modified)
    local second = buffer.open_org_buffer_from_text(
      '* two', 'skg://two', 'uri-two')
    warned = nil
    vim.api.nvim_buf_set_lines(second, 1, 1, false, { 'edit two' })
    vim.wait(200, function () return warned ~= nil end, 10)
    vim.notify = original_notify
    assert.is_truthy(warned)
    assert.is_truthy(tostring(warned):find('unsaved modifications'))
  end)
end)
