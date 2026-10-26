-- Mirrors tests/elisp/test-skg-detach-attach.el: under a parent that
-- reads claims, skg.metadata.detach (<localleader>st) writes 'noClaim'
-- on the headline at point and skg.metadata.attach (<localleader>sa)
-- removes it. Both refuse under a parent that does not read claims,
-- say so when there is nothing to do, and never save.

local keymaps = require('skg.keymaps')
local metadata = require('skg.metadata')

local function buffer_with (text, line)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { line, 0 })
  return buf
end

local function buffer_text ()
  return table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), '\n')
end

local editable_parent = '* (skg node (gnid p) (skgRepos (nodeRepo r))) p'

describe('skg.metadata detach and attach', function ()
  local real_notify
  local notified

  before_each(function ()
    notified = {}
    real_notify = vim.notify
    vim.notify = function (message) table.insert(notified, message) end
  end)

  after_each(function ()
    vim.notify = real_notify
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('detach writes noClaim', function ()
    buffer_with(editable_parent
      .. '\n** (skg node (gnid c) (skgRepos (nodeRepo r))) c', 2)
    metadata.detach()
    assert.are.equal('** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) c',
                     metadata.line_text(2))
    assert.is_truthy(notified[1]:match('^Detached'))
  end)

  it('attach removes noClaim', function ()
    buffer_with(editable_parent
      .. '\n** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) c', 2)
    metadata.attach()
    assert.are.equal('** (skg node (gnid c) (skgRepos (nodeRepo r))) c',
                     metadata.line_text(2))
    assert.is_truthy(notified[1]:match('^Attached'))
  end)

  it('detach gives a headline without metadata the node atom', function ()
    buffer_with(editable_parent .. '\n** a typed child', 2)
    metadata.detach()
    assert.are.equal('** (skg node noClaim) a typed child',
                     metadata.line_text(2))
  end)

  it('detaches under an editable folder', function ()
    buffer_with(editable_parent
      .. '\n** (skg subscribeeFolder)'
      .. '\n*** (skg node (gnid s) (skgRepos (nodeRepo r))) s', 3)
    metadata.detach()
    assert.are.equal(
      '*** (skg node (gnid s) (skgRepos (nodeRepo r)) noClaim) s',
      metadata.line_text(3))
  end)

  it('both refuse where no claim is read, changing nothing', function ()
    for _, case in ipairs({
        { '* (skg node (gnid p) (skgRepos (nodeRepo r)) writeProtected) p'
          .. '\n** (skg node (gnid c) (skgRepos (nodeRepo r))) c', 2 },
        { editable_parent .. '\n** (skg subscriberFolder)'
          .. '\n*** (skg node (gnid s) (skgRepos (nodeRepo r)) noClaim) s', 3 },
        { editable_parent, 1 } }) do
      for _, command in ipairs({ metadata.detach, metadata.attach }) do
        buffer_with(case[1], case[2])
        local ok, err = pcall(command)
        assert.is_false(ok)
        assert.is_truthy(tostring(err):match('does not read claims'))
        assert.are.equal(case[1], buffer_text())
        vim.api.nvim_buf_delete(0, { force = true })
      end
    end
    buffer_with('* placeholder', 1)
  end)

  it('both say so on a no-op, changing nothing', function ()
    for _, case in ipairs({
        { '** (skg node (gnid c) (skgRepos (nodeRepo r))) c',
          metadata.attach, 'nothing to attach' },
        { '** (skg node (gnid c) (skgRepos (nodeRepo r)) noClaim) c',
          metadata.detach, 'nothing to detach' } }) do
      local text = editable_parent .. '\n' .. case[1]
      buffer_with(text, 2)
      notified = {}
      case[2]()
      assert.is_truthy(notified[1]:match(case[3]))
      assert.are.equal(text, buffer_text())
      vim.api.nvim_buf_delete(0, { force = true })
    end
    buffer_with('* placeholder', 1)
  end)

  it('refuses a kind that claims nothing', function ()
    buffer_with(editable_parent .. '\n** (skg aliasFolder)', 2)
    local ok, err = pcall(metadata.detach)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):match('claims no membership'))
  end)

  it('is bound to <localleader>st and <localleader>sa', function ()
    local bound = {}
    for _, binding in ipairs(keymaps.content_view_bindings) do
      bound[binding[1]] = binding[2] end
    assert.are.equal('Detach', bound.st)
    assert.are.equal('Attach', bound.sa)
    assert.are.equal('detach', keymaps.commands.Detach.fn)
    assert.are.equal('attach', keymaps.commands.Attach.fn)
  end)
end)
