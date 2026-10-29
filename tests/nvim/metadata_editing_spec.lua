-- Mirrors tests/elisp/test-skg-metadata-editing.el: a round trip
-- through the metadata editor.

local sexpr_edit = require('skg.sexpr_edit')

describe('the metadata editor', function ()
  after_each(function ()
    vim.cmd('silent! %bwipeout!')
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
