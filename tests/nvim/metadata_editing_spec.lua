-- Mirrors tests/elisp/test-skg-metadata-editing.el: the metadata
-- editor's herald hints, and a round trip through the editor.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local herald_rules = require('skg.herald_rules')
local heralds = require('skg.heralds')
local sexpr_edit = require('skg.sexpr_edit')

describe('the metadata editor', function ()
  after_each(function ()
    herald_rules.install_rules(nil)
    vim.cmd('silent! %bwipeout!')
  end)

  it('appends herald hints as virtual text, never buffer text', function ()
    -- The metadata view appends, per field/value headline, the herald
    -- it produces -- as virtual text, colored tokens inside plain
    -- parens. Fields producing no herald get none.
    helpers.install_fixture_herald_rules()
    local buf = vim.api.nvim_create_buf(true, false)
    local lines = { '* skg', '** node', '** skgRepos', '*** nodeRepo',
                    '**** main', '** writeProtected', '** noClaim',
                    '*** true' }
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    sexpr_edit.decorate_with_heralds(buf)
    local function hint_on_line (needle)
      for row, line in ipairs(lines) do
        if line == needle then
          local marks = vim.api.nvim_buf_get_extmarks(
            buf, sexpr_edit.herald_hints_namespace,
            { row - 1, 0 }, { row - 1, -1 }, { details = true })
          if #marks == 0 then return nil end
          return heralds.chunks_text(marks[1][4].virt_text) end
      end
      error('no line ' .. needle)
    end
    assert.are.equal(' (\u{262e})', hint_on_line('** writeProtected'))
    assert.is_nil(hint_on_line('** noClaim'))
    assert.is_nil(hint_on_line('**** main'))
    assert.is_nil(hint_on_line('*** nodeRepo'))
    -- The buffer text is untouched: applying must not see hints.
    assert.are.same(lines, vim.api.nvim_buf_get_lines(buf, 0, -1, false))
  end)

  it('adds no hint without a rule table', function ()
    herald_rules.install_rules(nil)
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                               { '* skg', '** writeProtected' })
    sexpr_edit.decorate_with_heralds(buf)
    assert.are.same({}, vim.api.nvim_buf_get_extmarks(
      buf, sexpr_edit.herald_hints_namespace, 0, -1, {}))
  end)

  it('opened and applied untouched, sends back what the server wrote',
     function ()
    local headline = '** (skg (vnid 0b) focused folded node (gnid n)'
      .. ' (skgRepos (nodeRepo main) (relRepo pub)) noClaim writeProtected'
      .. ' (overridesHere o) (editRequest (relRepo pub) (ready node))'
      .. ' (viewRequests editableView) (view cycle)'
      .. ' (diff (rel added) (leaves 1 rel))) a title'
    local source = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(source, 0, -1, false,
                               { '* (skg node (gnid p)) p', headline })
    vim.api.nvim_set_current_buf(source)
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    sexpr_edit.view_metadata()
    local edit_buf = vim.api.nvim_get_current_buf()
    assert.are_not.equal(source, edit_buf)
    sexpr_edit.apply(edit_buf)
    assert.are.equal(source, vim.api.nvim_get_current_buf())
    assert.are.same({ '* (skg node (gnid p)) p', headline },
                    vim.api.nvim_buf_get_lines(source, 0, -1, false))
  end)
end)
