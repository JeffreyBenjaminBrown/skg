-- Integration test: cycling a view's perspective, whose overrides it
-- follows ('override_perspective.override_perspective'). The Lua mirror of
-- test-emacs.el in this directory; see run-test.sh for the data.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(60)

local buffer = require('skg.buffer')
local lock = require('skg.lock')
local override_perspective = require('skg.override_perspective')
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

---Wait for PREDICATE; FAIL, showing BUF's text, unless it holds.
local function check_eventually (predicate, timeout_secs, message)
  if not T.wait_for(predicate, timeout_secs) then
    T.fail(message .. ':\n' .. T.buffer_text(buf)) end
  print('ok: ' .. message)
end

print('=== SKG Override Perspective Integration Test ===')

require('skg.single_root_content_view')
  .request_single_root_content_view_from_id('R')
buf = T.wait_for(function ()
  for _, candidate in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(candidate)
       and vim.b[candidate].skg_view_id
       and line_of_gnid(candidate, 'A') then
      return candidate end
  end
  return nil
end, 10)
T.check(buf, 'the view of R opened, showing A in M\'s place')
vim.api.nvim_set_current_buf(buf)
T.check(vim.b[buf].skg_overrides_followed == nil,
        "a fresh view follows its root authors' overrides")

-- The cursor on A's headline; the switch must leave it on U's, which
-- stands in the same place.
vim.api.nvim_win_set_cursor(0, { line_of_gnid(buf, 'A'), 0 })
override_perspective.override_perspective()
check_eventually(function ()
    return vim.b[buf].skg_overrides_followed == 'user'
           and line_of_gnid(buf, 'U')
           and lock.stream_in_progress == nil
  end, 10, 'switched, the view shows U')
T.check(not line_of_gnid(buf, 'A'),
        'switched, the view no longer shows A')
T.check(vim.api.nvim_win_get_cursor(0)[1] == line_of_gnid(buf, 'U'),
        "the cursor stayed on the headline M's overrider stands on")
T.check(buffer.override_perspective_mode_line(buf) == ':your overrides',
        'the statusline mark says the view follows your overrides')

-- Unsaved edits refuse the switch. The body line goes directly under
-- U's headline: the end of the buffer is under foreign K, and an edit
-- there would fork K.
local u_line = line_of_gnid(buf, 'U')
vim.api.nvim_buf_set_lines(buf, u_line, u_line, false,
                           { 'a body line for U' })
vim.api.nvim_win_set_cursor(0, { u_line, 0 })
local switched, refusal = pcall(override_perspective.override_perspective)
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
    return line_of_gnid(buf, 'U') and not line_of_gnid(buf, 'A')
           and vim.b[buf].skg_overrides_followed == 'user'
  end, 1, 'after the save the view still follows your overrides')

-- The next perspective is third's, which shows B.
override_perspective.override_perspective()
check_eventually(function ()
    return vim.b[buf].skg_overrides_followed == 'owner of third'
           and line_of_gnid(buf, 'B')
           and lock.stream_in_progress == nil
  end, 10, "the next perspective, third's, shows B")
T.check(buffer.override_perspective_mode_line(buf) == ":third's overrides",
        "the statusline mark names third's overrides")

-- After the last, the root authors' again, showing A.
override_perspective.override_perspective()
check_eventually(function ()
    return vim.b[buf].skg_overrides_followed == nil
           and line_of_gnid(buf, 'A')
           and lock.stream_in_progress == nil
  end, 10, 'switched back, the view shows A again')
T.check(buffer.override_perspective_mode_line(buf) == '',
        'the statusline mark is gone')

-- The root too: a view of M, which A (M's own author's), U (yours)
-- and B (third's) each override, shows M as its root from the root
-- authors' perspective and each override from its author's, re-pointed
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
for _, step in ipairs({ { 'user', 'U', ':your overrides' },
                        { 'owner of foreign', 'A', ":foreign's overrides" },
                        { 'owner of third', 'B', ":third's overrides" },
                        { nil, 'M', '' } }) do
  local followed, shown, mark = step[1], step[2], step[3]
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  override_perspective.override_perspective()
  check_eventually(function ()
      return vim.b[buf].skg_overrides_followed == followed
             and line_of_gnid(buf, shown) == 1
             and lock.stream_in_progress == nil
    end, 10, 'the root is ' .. shown)
  local root = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1]
  T.check((shown == 'M') == (not root:find('(overridesHere M)', 1, true)),
          'the root ' .. shown .. ' stands for M as it should')
  T.check(vim.api.nvim_win_get_cursor(0)[1] == 1,
          'the cursor stayed on the root, now ' .. shown)
  T.check(buffer.override_perspective_mode_line(buf) == mark,
          'the statusline mark for ' .. shown)
  T.check(vim.api.nvim_buf_get_name(buf) == name, 'the buffer keeps its name')
end

T.pass('PASS: Integration test successful!')
