-- Mirrors tests/elisp/test-skg-describe-heralds.el: what the client
-- sends for the headline at point, and how it reads and shows what
-- comes back. The sentences are the server's, checked by its unit
-- tests.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local describe_heralds = require('skg.describe_heralds')
local sexpr = require('skg.sexpr.parse')

describe('skg.describe_heralds', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('sends the metadata of the headline whose body the line is in', function ()
    helpers.buffer_with({
      '* (skg (vnid 1) node (gnid p)) p',
      '** (skg (vnid 2) node (gnid c) (view (rels (linksTo (in 5 (substantive 2)))))) c',
      'its body' })
    assert.are.equal(
      '((request . "describe heralds") (metadata . '
        .. '"(skg (vnid 2) node (gnid c) (view (rels (linksTo '
        .. '(in 5 (substantive 2))))))"))\n',
      describe_heralds.request_at_line(3))
  end)

  it('adds a pending detachment and a folder change', function ()
    helpers.buffer_with({
      '* (skg (vnid 1) node (gnid p)) p',
      '** (skg (vnid 2) node (gnid c) noClaim) c',
      '** (skg (vnid 3) aliasFolder) aliases',
      '*** (skg (vnid 4) alias (diff (rel added))) another name' })
    assert.truthy(describe_heralds.request_at_line(2):find(
      '(metadata . "(skg (vnid 2) node (gnid c) noClaim pendingDetachment)")',
      1, true))
    assert.truthy(describe_heralds.request_at_line(3):find(
      '(folder-change . "unprocessed")', 1, true))
  end)

  it('refuses outside a headline', function ()
    helpers.buffer_with({ 'before any headline', '* (skg (vnid 1) node) p' })
    assert.has_error(function () describe_heralds.request_at_line(1) end,
                     'Not on or under a headline')
  end)

  it('reads lines or an error', function ()
    assert.are.same({ 'a', 'b c' }, describe_heralds.lines_of_response(
      sexpr.read('((response-type describe-heralds) (lines "a" "b c"))')))
    assert.are.same({ error = 'no' }, describe_heralds.lines_of_response(
      sexpr.read('((response-type describe-heralds) (error "no"))')))
  end)

  it('shows two short lines as a message and more in a buffer', function ()
    local real_notify, shown = vim.notify, nil
    vim.notify = function (message) shown = message end
    local ok, err = pcall(function ()
      describe_heralds.show({ 'aK: the headline above it (a) forks this one.' })
      assert.are.equal('aK: the headline above it (a) forks this one.', shown)
      shown = nil
      describe_heralds.show({ 'one.', 'two.', 'three.' })
      assert.is_nil(shown)
      assert.are.same({ 'one.', '', 'two.', '', 'three.', '' },
        vim.api.nvim_buf_get_lines(0, 0, -1, false))
      vim.cmd('close')
    end)
    vim.notify = real_notify
    assert(ok, err)
  end)
end)
