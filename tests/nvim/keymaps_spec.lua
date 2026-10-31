-- The dispatcher of skg.keymaps: a refusal, an 'error ()' a command
-- raises, is shown as one line at error level, as Emacs shows a
-- 'user-error' ('docs/nvim-client-deviations.org').

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local keymaps = require('skg.keymaps')

describe('skg.keymaps refusals', function ()
  local real_notify = vim.notify
  local shown

  before_each(function ()
    shown = {}
    vim.notify = function (message, level)
      table.insert(shown, { message = message, level = level }) end
  end)

  after_each(function ()
    vim.notify = real_notify
    keymaps.commands.TestRefusal = nil
    package.loaded['skg.test_refusal'] = nil
  end)

  it('strips the FILE:LINE: prefixes error() adds', function ()
    assert.are.equal('Cannot do it',
      keymaps.refusal_line('/x/skg/save.lua:67: /x/skg/save.lua:123: Cannot do it'))
    assert.are.equal('Cannot do it', keymaps.refusal_line('Cannot do it'))
    assert.are.equal('Cannot read b.lua:3: here',
      keymaps.refusal_line('a.lua:1: Cannot read b.lua:3: here'))
  end)

  it('shows a refusal from a command it invokes as one line', function ()
    package.loaded['skg.test_refusal'] = {
      refuse = function () error('Cannot do it here') end }
    keymaps.commands.TestRefusal = { module = 'skg.test_refusal',
                                     fn = 'refuse', desc = 'refuses' }
    assert.has_no.errors(function () keymaps.invoke('TestRefusal') end)
    assert.are.same({ { message = 'Cannot do it here',
                        level = vim.log.levels.ERROR } }, shown)
  end)

  it('shows the skgrepo-set switch refusal of an unsaved view as one line,'
     .. ' from its key and from its Ex command', function ()
    local buf = helpers.buffer_with({ '* (skg node (gnid a)) a' })
    vim.b[buf].skg_view_id = 'view-a'
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '* (skg node (gnid a)) a!' })
    assert.is_true(vim.bo[buf].modified)
    keymaps.apply_bindings(buf, { { 'vl', 'RestrictRepoSet' } })
    local key = vim.api.nvim_replace_termcodes(
      vim.g.maplocalleader or '\\', true, false, true)
    vim.api.nvim_feedkeys(key .. 'vl', 'x', false)
    keymaps.define_ex_commands()
    vim.cmd('SkgRestrictRepoSet')
    local refusal = 'skg: save or revert every skg view before switching'
      .. ' the skgrepo-set'
    assert.are.same({ { message = refusal, level = vim.log.levels.ERROR },
                      { message = refusal, level = vim.log.levels.ERROR } },
                    shown)
    vim.bo[buf].modified = false
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)
