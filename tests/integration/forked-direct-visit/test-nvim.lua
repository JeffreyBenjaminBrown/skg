-- Regression test for ordinary navigation to a forked node.
-- The Lua mirror of test-emacs.el in this directory: visiting Z must
-- open raw Z without injecting its fork R as an independent sibling.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local content_view = require('skg.single_root_content_view')
local buffer = require('skg.buffer')

---Return a live skg view buffer whose text mentions (gnid ID).
local function raw_view_buffer_showing (gnid)
  local needle = '(gnid ' .. gnid .. ')'
  return T.first_buffer_where(function (buf)
    return vim.b[buf].skg_view_id
           and T.buffer_text(buf):find(needle, 1, true)
  end)
end

print('Starting forked direct-visit integration test...')

-- Visiting forked Z opens raw Z.
content_view.request_single_root_content_view_from_id('Z')
local raw_buf = T.wait_for(function ()
  return raw_view_buffer_showing('Z')
end, 10)
T.check(raw_buf, 'ordinary visit did not open a raw view of Z')
local raw_content = T.buffer_text(raw_buf)
T.check(raw_content:find('cooking', 1, true),
        "raw view lacks Z's title")
T.check(not raw_content:match(
          '\n%*%* %(skg [^\n]-node %(gnid R%)[^\n]-noClaim'),
        'ordinary visit injected the fork R as an independent sibling')
print('ordinary visit opened raw Z without its fork as a sibling')

T.pass('PASS: Integration test successful!')
