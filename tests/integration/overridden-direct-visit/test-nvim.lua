-- Regression test for ordinary navigation to an overridden node.
-- The Lua mirror of test-emacs.el in this directory: visiting Z must
-- open raw Z without injecting overrider R as an independent sibling.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local content_view = require('skg.content_view')
local buffer = require('skg.buffer')

---Return a live skg view buffer whose text mentions (id ID).
local function raw_view_buffer_showing (id)
  local needle = '(id ' .. id .. ')'
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      local uri = vim.b[buf].skg_view_uri
      if uri and T.buffer_text(buf):find(needle, 1, true) then
        return buf
      end
    end
  end
  return nil
end

print('Starting overridden direct-visit integration test...')

-- Visiting overridden Z opens raw Z.
content_view.request_single_root_content_view_from_id('Z')
local raw_buf = T.wait_for(function ()
  return raw_view_buffer_showing('Z')
end, 10)
T.check(raw_buf, 'ordinary visit did not open a raw view of Z')
local raw_content = T.buffer_text(raw_buf)
T.check(raw_content:find('cooking', 1, true),
        "raw view lacks Z's title")
T.check(not raw_content:match('\n%*%* %(skg %(node %(id R%).-affectsParent false'),
        'ordinary visit injected overrider R as an independent sibling')
print('ordinary visit opened raw Z without an overrider sibling')

T.pass('PASS: Integration test successful!')
