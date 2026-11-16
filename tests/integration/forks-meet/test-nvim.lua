-- Integration test: forks of one note meet, in Neovim
-- ('docs/forks.org', "Forks of one note meet"; run-test.sh gives the
-- data). The Lua mirror of test-emacs.el in this directory, through
-- the user's own gestures ('dd', 'p', ':s', ':w'):
-- 1. R's view. R's forks folder is flat. N stands editable in it, its
--    herald bK (a forkee), its first child its folder of what the user
--    keeps elsewhere (X1 to X3, which R contains); T, which forks N,
--    stands there too, an
--    indirect member, editable, its herald with neither bK nor Kb,
--    showing Y and Z, what R neither contains nor hides; no member
--    shows a forks folder; each containers folder's member (Shelf) is
--    folded.
-- 2. X4, which N shows there, is deleted ('dd') and the view saved
--    (':w'): X4 stands in N's folder of what R hides in it, right after
--    N's folder of what the user keeps elsewhere.
-- 3. N's view. Y is deleted from under T, put after N's last child
--    ('p') and raised to N's content (':s'), and the view saved: T
--    stands first in N's forks folder, claiming, its herald bKb (a
--    mutual); R follows; Y stands in N's content, and so in T's folder
--    of what the user keeps elsewhere.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(60)

local folds = require('skg.folds')
local herald_rules = require('skg.herald_rules')
local heralds = require('skg.heralds')

-- ── helpers ───────────────────────────────────────────────────────

---@param buf integer
---@return string[]
local function lines_of (buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

---The 1-based number of the NTH (from 0, default 0) headline of BUF
---showing GNID; fails if there is none.
---@param buf integer
---@param gnid string
---@param nth integer|nil
---@return integer
local function line_showing (buf, gnid, nth)
  local needle = '(gnid ' .. gnid .. ')'
  local seen = 0
  for number, line in ipairs(lines_of(buf)) do
    if line:match('^%*+ %(skg ') and line:find(needle, 1, true) then
      if seen == (nth or 0) then return number end
      seen = seen + 1
    end
  end
  T.fail('no line ' .. (nth or 0) .. ' showing ' .. gnid .. ' in:\n'
         .. T.buffer_text(buf))
  return 0
end

---The 1-based number of the first headline of BUF whose metadata
---names the folder ATOM, at or after line FROM (default 1), or nil.
---@param buf integer
---@param atom string
---@param from integer|nil
---@return integer|nil
local function folder_line (buf, atom, from)
  local lines = lines_of(buf)
  for number = from or 1, #lines do
    if lines[number]:match('^%*+ %(skg ') and lines[number]:find(atom, 1, true) then
      return number end
  end
  return nil
end

---@param buf integer
---@param number integer
---@return string
local function line_at (buf, number)
  return vim.api.nvim_buf_get_lines(buf, number - 1, number, false)[1] or ''
end

---@param buf integer
---@param number integer
---@return integer the star count of the headline at line NUMBER
local function depth_at (buf, number)
  return #(line_at(buf, number):match('^%*+') or '')
end

---The herald the user sees on line NUMBER of BUF, or nil.
---@param buf integer
---@param number integer
---@return string|nil
local function herald_at (buf, number)
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
      buf, heralds.namespace, { number - 1, 0 }, { number - 1, -1 },
      { details = true })) do
    if mark[4].virt_text then
      return heralds.chunks_text(mark[4].virt_text) end
  end
  return nil
end

---@param buffer_name string
---@return integer the view's buffer, current, once its response is in
local function wait_for_view (buffer_name)
  local buf = T.wait_for_buffer(buffer_name, 15)
  T.check(buf, 'the buffer ' .. buffer_name .. ' opened')
  T.check(T.wait_for_response(15), 'the view arrived')
  vim.api.nvim_set_current_buf(buf)
  return buf
end

---Save the current view with ':w' and wait for its redrawing.
local function write_view ()
  vim.cmd('silent write')
  T.check(T.wait_for_response(20), 'the save got a response')
end

T.client.connect()
herald_rules.request_herald_rules()
T.check(T.wait_for_response(10), 'herald rules arrived')

-- ── phase 1: R's view ─────────────────────────────────────────────

print("=== PHASE 1: R's view ===")
require('skg.single_root_content_view').request_single_root_content_view_from_id('R')
local r_buf = wait_for_view('skg://R')
print("R's view:\n" .. T.buffer_text(r_buf))
do
  local n_line = line_showing(r_buf, 'N')
  local herald = T.wait_for(function () return herald_at(r_buf, n_line) end, 5)
  print("N's herald in R's forks folder: " .. tostring(herald))
  T.check(not line_at(r_buf, n_line):find('writeProtected', 1, true)
          and herald and herald:find('bK', 1, true)
          and not herald:find('Kb', 1, true),
          "N stands editable in R's forks folder, a forkee (bK)")
end
do
  local forks_folders = 0
  local at = folder_line(r_buf, 'forksFolder')
  while at do
    forks_folders = forks_folders + 1
    at = folder_line(r_buf, 'forksFolder', at + 1)
  end
  T.check(forks_folders == 1,
          "R's view holds one forks folder; found " .. forks_folders)
end
do
  local t_line = line_showing(r_buf, 'T')
  local herald = herald_at(r_buf, t_line)
  print("T's herald in R's forks folder: " .. tostring(herald))
  T.check(not line_at(r_buf, t_line):find('writeProtected', 1, true)
          and line_at(r_buf, t_line):find('noClaim', 1, true)
          and herald and not herald:find('Kb', 1, true)
          and not herald:find('bK', 1, true),
          "T stands editable in R's forks folder, an indirect member (no b)")
  local y_line = line_showing(r_buf, 'Y')
  T.check(depth_at(r_buf, y_line) == depth_at(r_buf, t_line) + 1
          and not folds.line_invisible_p(y_line),
          'T shows Y')
  local z_line = line_showing(r_buf, 'Z')
  T.check(depth_at(r_buf, z_line) == depth_at(r_buf, t_line) + 1
          and not folds.line_invisible_p(z_line),
          'T shows Z, which R does not hide')
end
do
  local containers_folders = 0
  local at = folder_line(r_buf, 'containerFolder')
  while at do
    containers_folders = containers_folders + 1
    T.check(not folds.line_invisible_p(at),
            "a containers folder's headline shows")
    T.check(line_at(r_buf, at + 1):find('(gnid Shelf)', 1, true)
            and folds.line_invisible_p(at + 1),
            "a containers folder's member, Shelf, is folded")
    at = folder_line(r_buf, 'containerFolder', at + 1)
  end
  T.check(containers_folders == 2,
          'N and T each carry a containers folder; found '
          .. containers_folders)
end
print("PASS [phase 1]: R's view")

-- ── phase 2: cut X4 from N in R's view ────────────────────────────

print("=== PHASE 2: Cut X4 from N in R's view ===")
vim.api.nvim_win_set_cursor(0, { line_showing(r_buf, 'X4'), 0 })
vim.cmd('normal! dd')
write_view()
print("R's view after the save:\n" .. T.buffer_text(r_buf))
do
  local n_line = line_showing(r_buf, 'N')
  local n_depth = depth_at(r_buf, n_line)
  T.check(line_at(r_buf, n_line + 1):find('integratedElsewhereFolder', 1, true)
          and depth_at(r_buf, n_line + 1) == n_depth + 1,
          "N's first child is its folder of what the user keeps elsewhere")
  local hidden_in_line = n_line + 2
  while depth_at(r_buf, hidden_in_line) > n_depth + 1 do
    hidden_in_line = hidden_in_line + 1
  end
  T.check(line_at(r_buf, hidden_in_line):find('hiddenInForkPartnerFolder', 1, true)
          and depth_at(r_buf, hidden_in_line) == n_depth + 1,
          "N's second child is now its folder of what R hides in it")
  T.check(line_at(r_buf, hidden_in_line + 1):find('(gnid X4)', 1, true),
          'that folder holds X4')
end
print("PASS [phase 2]: X4 stands in N's folder of what R hides in it")

-- ── phase 3: move Y from T into N, in N's view ────────────────────

print("=== PHASE 3: Move Y from T into N, in N's view ===")
require('skg.single_root_content_view').request_single_root_content_view_from_id('N')
local n_buf = wait_for_view('skg://N')
print("N's view:\n" .. T.buffer_text(n_buf))
vim.api.nvim_win_set_cursor(0, { line_showing(n_buf, 'Y'), 0 })
vim.cmd('normal! dd')
vim.api.nvim_win_set_cursor(0, { vim.api.nvim_buf_line_count(n_buf), 0 })
vim.cmd('normal! p')
vim.cmd([[silent s/^\*\+ /** /]])
write_view()
print("N's view after the save:\n" .. T.buffer_text(n_buf))
do
  local forks_folder = folder_line(n_buf, 'forksFolder')
  T.check(forks_folder and depth_at(n_buf, forks_folder) == 2,
          'N shows its forks folder')
  local first = forks_folder + 1
  local line = line_at(n_buf, first)
  local herald = T.wait_for(function () return herald_at(n_buf, first) end, 5)
  print("T's herald, first in N's forks folder: " .. tostring(herald))
  T.check(line:find('(gnid T)', 1, true)
          and not line:find('noClaim', 1, true)
          and not line:find('writeProtected', 1, true)
          and herald and herald:find('bKb', 1, true),
          "T stands first in N's forks folder, an editable mutual (bKb)")
  local member_depth = depth_at(n_buf, first)
  local next_member = first + 1
  while depth_at(n_buf, next_member) > member_depth do
    next_member = next_member + 1 end
  T.check(depth_at(n_buf, next_member) == member_depth
          and line_at(n_buf, next_member):find('(gnid R)', 1, true),
          "R follows T in N's forks folder")
  T.check(depth_at(n_buf, line_showing(n_buf, 'Y')) == member_depth + 2,
          "Y stands first in T's folder of what the user keeps elsewhere")
  T.check(depth_at(n_buf, line_showing(n_buf, 'Y', 1)) == 2,
          "Y stands in N's own content")
end
print("PASS [phase 3]: T is a mutual, first, and Y is N's")

T.pass('PASS: Integration test successful!')
