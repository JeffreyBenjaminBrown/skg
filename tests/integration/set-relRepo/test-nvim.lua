-- Integration test for the relRepo gesture, Neovim client. The Lua
-- mirror of test-emacs.el in this directory: node r (home public)
-- contains k (home private), so the relationship's default relRepo is
-- private, and the prompt offers private, secret and the no-override
-- choice, which it can know only if the info request names the
-- recorder. Choosing secret and saving records it.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(25)

local metadata = require('skg.metadata')
local picker = require('skg.picker')

require('skg.single_root_content_view')
  .request_single_root_content_view_from_id('r')
local view = T.wait_for_buffer('skg://r')
T.check(view, 'the view of r opened')
vim.api.nvim_set_current_buf(view)

local function line_of_k ()
  for line = 1, vim.api.nvim_buf_line_count(view) do
    local text = vim.api.nvim_buf_get_lines(view, line - 1, line, false)[1]
    if text:find('(gnid k)', 1, true) then return line, text end
  end
  return nil, ''
end

T.check(T.wait_for(function () return line_of_k() end), 'k is drawn')
vim.api.nvim_win_set_cursor(0, { (line_of_k()), 0 })

local seen_choices = nil
picker.completing_read_with_cycle = function (_prompt, choices)
  seen_choices = choices
  return 'secret'
end
require('skg.relRepo_info').set_relRepo()
T.check(T.wait_for(function ()
          return select(2, line_of_k()):find('(relRepo secret)', 1, true)
        end), 'a relRepo request was written: ' .. select(2, line_of_k()))
T.check(vim.deep_equal(seen_choices,
                       { 'private', 'secret', metadata.relRepo_no_override }),
        'the prompt offered the skgrepos from the default on: '
          .. vim.inspect(seen_choices))

vim.api.nvim_win_set_cursor(0, { 1, 0 })
require('skg.save').request_save_buffer()
T.check(T.wait_for_response(15), 'the save response arrived')
T.check(T.wait_for(function ()
          local _, text = line_of_k()
          return text:find('(skgRepos (nodeRepo private) (relRepo secret))',
                           1, true)
                 and not text:find('editRequest', 1, true)
        end), 'the save recorded the relRepo: ' .. select(2, line_of_k()))

T.pass('PASS: the relRepo gesture requested and the save recorded it')
