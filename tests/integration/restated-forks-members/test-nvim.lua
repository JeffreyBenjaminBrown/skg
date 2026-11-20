-- Integration test: a member of a forks folder that restates its
-- note, in Neovim. The Lua mirror of test-emacs.el in this directory.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(40)
local herald_rules = require('skg.herald_rules')
local fork_dedup = require('skg.fork_dedup')

---@return integer|nil the 0-based row of the line showing (gnid GNID)
local function row_showing (buf, gnid)
  for row, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:match('^%*+ %(skg ') and line:find('(gnid ' .. gnid .. ')', 1, true) then
      return row - 1 end
  end
  return nil
end

---Each cover's text, in buffer order, the marks hiding whole lines left out.
local function covers (buf)
  local result = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
                          buf, fork_dedup.namespace, 0, -1, { details = true })) do
    if mark[4].virt_text then table.insert(result, mark[4].virt_text[1][1]) end
  end
  return result
end

local function read (path)
  return table.concat(vim.fn.readfile(path), '\n')
end

T.client.connect()
herald_rules.request_herald_rules()
T.check(T.wait_for_response(10), 'herald rules arrived')

print("=== PHASE 1: F's view ===")
require('skg.single_root_content_view').request_single_root_content_view_from_id('F')
local buf = T.wait_for_buffer('skg://Same words', 15)
T.check(buf, "F's view opened")
T.check(T.wait_for_response(15), 'the view arrived')
vim.api.nvim_set_current_buf(buf)
print("F's view:\n" .. T.buffer_text(buf))
T.check(T.wait_for(function () return #covers(buf) == 2 end, 5),
        'G is covered: ' .. vim.inspect(covers(buf)))
T.check(vim.deep_equal(covers(buf),
                       { "<b's title, unchanged>", "<b's body, unchanged>" }),
        'the covers read as expected: ' .. vim.inspect(covers(buf)))
local g_row = row_showing(buf, 'G')
local g_line = vim.api.nvim_buf_get_lines(buf, g_row, g_row + 1, false)[1]
T.check(vim.endswith(g_line, ' Same words'), "the buffer keeps G's real title")

print('=== PHASE 2: toggle ===')
fork_dedup.toggle(buf)
T.check(#covers(buf) == 0, 'the toggle uncovered G')
fork_dedup.toggle(buf)
T.check(#covers(buf) == 2, 'the toggle covered G again')

print('=== PHASE 3: retitle G, save ===')
vim.api.nvim_buf_set_lines(buf, g_row, g_row + 1, false,
  { (g_line:gsub('Same words$', 'G retitled')) })
vim.cmd('silent write')
T.check(T.wait_for_response(20), 'the save got a response')
print("F's view after the save:\n" .. T.buffer_text(buf))
T.check(T.wait_for(function ()
          return vim.deep_equal(covers(buf), { "<b's body, unchanged>" }) end, 5),
        'after the save only the body is covered: ' .. vim.inspect(covers(buf)))

local g = read('data/main/G.skg')
local f = read('data/main/F.skg')
T.check(g:find('title: G retitled', 1, true), "G's file has its new title:\n" .. g)
T.check(g:find('A long body\n  that its forks restate.', 1, true),
        "G's file keeps its body:\n" .. g)
T.check(not (f .. g):find('unchanged>', 1, true),
        "no cover's text reached a file:\n" .. f .. '\n' .. g)
T.check(f:find('title: Same words', 1, true), "F's title is unchanged:\n" .. f)

T.pass('PASS: Integration test successful!')
