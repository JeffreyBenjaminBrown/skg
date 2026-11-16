-- Integration test: cycling a view's perspective
-- ('perspective.cycle_perspective'). The Lua mirror of
-- test-emacs.el in this directory; see run-test.sh for the data.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(60)

local buffer = require('skg.buffer')
local lock = require('skg.lock')
local perspective = require('skg.perspective')
local save = require('skg.save')

---The view of R, once it opens.
local buf

---The line number showing GNID in VIEW, or nil.
local function line_of_gnid (view, gnid)
  local needle = '(gnid ' .. gnid .. ')'
  for number, line in ipairs(vim.api.nvim_buf_get_lines(view, 0, -1, false)) do
    if line:find(needle, 1, true) then return number end
  end
  return nil
end

---The line number showing GNID in M's place (marked '(inPlaceOf M)')
---in VIEW, or nil. M, drawn editable in the forks folder of the
---mirror standing in its place, shows the other mirrors there too,
---as forkers of M.
local function line_in_ms_place (view, gnid)
  local needle = '(gnid ' .. gnid .. ')'
  for number, line in ipairs(vim.api.nvim_buf_get_lines(view, 0, -1, false)) do
    if line:find(needle, 1, true) and line:find('(inPlaceOf M)', 1, true) then
      return number end
  end
  return nil
end

---Wait for PREDICATE; FAIL, showing BUF's text, unless it holds.
local function check_eventually (predicate, timeout_secs, message)
  if not T.wait_for(predicate, timeout_secs) then
    T.fail(message .. ':\n' .. T.buffer_text(buf)) end
  print('ok: ' .. message)
end

print('=== SKG Perspective Integration Test ===')

require('skg.single_root_content_view')
  .request_single_root_content_view_from_id('R')
buf = T.wait_for(function ()
  for _, candidate in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(candidate)
       and vim.b[candidate].skg_view_id
       and line_in_ms_place(candidate, 'A') then
      return candidate end
  end
  return nil
end, 10)
T.check(buf, 'the view of R opened, showing A in M\'s place')
vim.api.nvim_set_current_buf(buf)
T.check(vim.b[buf].skg_perspective == nil,
        "a fresh view is drawn from its root authors' perspective")

-- The cursor on A's headline; the switch must leave it on U's, which
-- stands in the same place.
vim.api.nvim_win_set_cursor(0, { line_in_ms_place(buf, 'A'), 0 })
perspective.cycle_perspective()
check_eventually(function ()
    return vim.b[buf].skg_perspective == 'user'
           and line_in_ms_place(buf, 'U')
           and lock.stream_in_progress == nil
  end, 10, 'switched, the view shows U')
T.check(not line_in_ms_place(buf, 'A'),
        'switched, the view no longer shows A')
T.check(vim.api.nvim_win_get_cursor(0)[1] == line_in_ms_place(buf, 'U'),
        "the cursor stayed on the headline M's mirror stands on")
T.check(buffer.perspective_mode_line(buf) == ':your perspective',
        'the statusline mark says the view shows your perspective')

-- Unsaved edits refuse the switch. The body line goes directly under
-- U's headline: the end of the buffer is under foreign K, and an edit
-- there would fork K.
local u_line = line_in_ms_place(buf, 'U')
vim.api.nvim_buf_set_lines(buf, u_line, u_line, false,
                           { 'a body line for U' })
vim.api.nvim_win_set_cursor(0, { u_line, 0 })
local switched, refusal = pcall(perspective.cycle_perspective)
T.check(not switched and tostring(refusal):find('unsaved edits', 1, true)
        and lock.stream_in_progress == nil,
        'a buffer with unsaved edits is not switched')

-- A save keeps the choice: it names it, and its redraw follows it.
save.request_save_buffer()
check_eventually(function ()
    return not vim.bo[buf].modified
           and T.buffer_text(buf):find('a body line for U', 1, true)
           and lock.stream_in_progress == nil
           and T.state.lp_pending_count == 0
  end, 20, 'the save finished')
check_eventually(function ()
    return line_in_ms_place(buf, 'U') and not line_in_ms_place(buf, 'A')
           and vim.b[buf].skg_perspective == 'user'
  end, 1, 'after the save the view still shows your perspective')

-- The next perspective is third's, which shows B.
perspective.cycle_perspective()
check_eventually(function ()
    return vim.b[buf].skg_perspective == 'owner of third'
           and line_in_ms_place(buf, 'B')
           and lock.stream_in_progress == nil
  end, 10, "the next perspective, third's, shows B")
T.check(buffer.perspective_mode_line(buf) == ":third's perspective",
        "the statusline mark names third's perspective")

-- After the last, the root authors' again, showing A.
perspective.cycle_perspective()
check_eventually(function ()
    return vim.b[buf].skg_perspective == nil
           and line_in_ms_place(buf, 'A')
           and lock.stream_in_progress == nil
  end, 10, 'switched back, the view shows A again')
T.check(buffer.perspective_mode_line(buf) == '',
        'the statusline mark is gone')

-- The root too: a view of M, which A (M's own author's), U (yours)
-- and B (third's) each fork, shows M as its root from the root
-- authors' perspective and each fork from its author's, re-pointed
-- where the root stands, so the cursor stays on it; the buffer keeps
-- its name.
require('skg.single_root_content_view')
  .request_single_root_content_view_from_id('M')
buf = T.wait_for(function ()
  for _, candidate in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(candidate)
       and vim.b[candidate].skg_view_id
       and line_of_gnid(candidate, 'M') == 1 then
      return candidate end
  end
  return nil
end, 10)
T.check(buf, 'the view of M opened')
vim.api.nvim_set_current_buf(buf)
local name = vim.api.nvim_buf_get_name(buf)
for _, step in ipairs({ { 'user', 'U', ':your perspective' },
                        { 'owner of foreign', 'A', ":foreign's perspective" },
                        { 'owner of third', 'B', ":third's perspective" },
                        { nil, 'M', '' } }) do
  local followed, shown, mark = step[1], step[2], step[3]
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  perspective.cycle_perspective()
  check_eventually(function ()
      return vim.b[buf].skg_perspective == followed
             and line_of_gnid(buf, shown) == 1
             and lock.stream_in_progress == nil
    end, 10, 'the root is ' .. shown)
  local root = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1]
  T.check((shown == 'M') == (not root:find('(inPlaceOf M)', 1, true)),
          'the root ' .. shown .. ' stands for M as it should')
  T.check(vim.api.nvim_win_get_cursor(0)[1] == 1,
          'the cursor stayed on the root, now ' .. shown)
  T.check(buffer.perspective_mode_line(buf) == mark,
          'the statusline mark for ' .. shown)
  T.check(vim.api.nvim_buf_get_name(buf) == name, 'the buffer keeps its name')
end

T.pass('PASS: Integration test successful!')
