local buffer = require('skg.buffer')
local recovery = require('skg.recovery')

describe('skg.recovery', function ()
  local paths = {}

  before_each(function ()
    require('skg.herald_rules').install_rules(
      require('skg.sexpr.parse').read('(skg (node (id)))'))
  end)

  after_each(function ()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf)
         and (buffer.buffer_p(buf)
              or vim.api.nvim_buf_get_name(buf):find('skg-recovery-test', 1, true)) then
        vim.bo[buf].modified = false
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
      end
    end
    for _, path in ipairs(paths) do pcall(vim.fn.delete, path) end
    paths = {}
  end)

  it('round-trips exact snapshots and includes a readable diff', function ()
    local baseline = '* old\n#+end_src\n\tλ\n'
    local current = '* new\n#+begin_src json\n\t雪'
    local buf = buffer.open_org_buffer_from_text(
      baseline, 'skg://recovery-source', 'recovery-uri')
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                               vim.split(current, '\n'))
    local document = recovery.document(buf)
    local encoded = {}
    for json in document:gmatch('#%+begin_src json\n([^\n]+)\n#%+end_src') do
      table.insert(encoded, vim.json.decode(json)) end
    assert.are.same({ baseline, current }, encoded)
    assert.is_truthy(document:find('%-%* old'))
    assert.is_true(vim.bo[buf].modified)
  end)

  it('writes an ordinary file without cleaning the source', function ()
    local path = vim.fn.tempname() .. '-skg-recovery-test.org'
    table.insert(paths, path)
    local source = buffer.open_org_buffer_from_text(
      '* baseline\n', 'skg://recovery-live', 'live-uri')
    vim.api.nvim_buf_set_lines(source, 1, 1, false, { 'unsaved' })
    recovery.show_unsaved_changes(path, true)
    local archive = vim.api.nvim_get_current_buf()
    assert.are.equal(path, vim.api.nvim_buf_get_name(archive))
    assert.is_nil(vim.b[archive].skg_view_uri)
    assert.is_true(vim.bo[source].modified)
    assert.are.equal('live-uri', vim.b[source].skg_view_uri)
    vim.api.nvim_buf_delete(source, { force = true })
    assert.is_true(vim.api.nvim_buf_is_valid(archive))
  end)
end)
