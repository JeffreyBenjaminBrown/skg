-- Integration test for the override-choice menu fetch path, nvim
-- client. The Lua mirror of test-emacs.el in this directory:
-- visiting overridden Z yields the menu buffer (registered under the
-- server-assigned "override-menu:Z" URI, showing the overrider R);
-- visiting Z with override-choice bypass yields the raw node.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local content_view = require('skg.content_view')
local buffer = require('skg.buffer')

---Return a live, non-menu skg view buffer whose text mentions (id ID).
local function raw_view_buffer_showing (id)
  local needle = '(id ' .. id .. ')'
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      local uri = vim.b[buf].skg_view_uri
      if uri and not uri:match('^override%-menu:')
         and T.buffer_text(buf):find(needle, 1, true) then
        return buf
      end
    end
  end
  return nil
end

print('Starting override-menu integration test...')

-- Visiting overridden Z yields the menu.
content_view.request_single_root_content_view_from_id('Z')
local menu_buf = T.wait_for(function ()
  return buffer.find_buffer_by_uri('override-menu:Z')
end, 10)
T.check(menu_buf, 'no buffer with the override-menu:Z URI appeared')
local menu_content = T.buffer_text(menu_buf)
T.check(menu_content:find('(id Z)', 1, true),
        'menu lacks the requested root Z')
T.check(menu_content:find('(id R)', 1, true),
        'menu lacks the overrider R')
print('menu buffer shows Z with overrider R')

-- Bypass opens the raw node, as its own buffer.
content_view.request_single_root_content_view_from_id('Z', true)
local raw_buf = T.wait_for(function ()
  return raw_view_buffer_showing('Z')
end, 10)
T.check(raw_buf, 'bypass did not open a raw view of Z')
local raw_content = T.buffer_text(raw_buf)
-- The raw view may legitimately show R in an overriderCol; that is
-- fine either way (no assertion), mirroring the elisp test's
-- permissive check.
T.check(raw_content:find('the overridden node', 1, true),
        "raw view lacks Z's title")
print('bypass opened the raw node Z')

T.pass('PASS: Integration test successful!')
