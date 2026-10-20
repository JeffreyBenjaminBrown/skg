-- Integration test: the diff view, end to end, nvim client. The
-- Lua mirror of test-emacs.el in this directory; see run-test.sh for
-- the data.
--
-- Phase 1: Open the diff view; its buffers open.
-- Phase 2: Mark every change commit-ready, through the recursive
--          gesture on each root of the "all changes" buffer, and see
--          "COMMIT READY".
-- Phase 3: Stage and commit; every owned skgrepo is committed, the
--          foreign one is not, and the buffers keep their roots,
--          which show no change any more.
-- Phase 4: With d referenced again by hand, open a view of r and
--          investigate the unknown node d.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(120)

local data_dir = vim.fn.fnamemodify(
  debug.getinfo(1, 'S').source:sub(2), ':p:h') .. '/data/'

local notified = {} -- every notification, oldest first
vim.notify = function (msg) table.insert(notified, tostring(msg)) end

---@param pattern string a Lua pattern
---@return string|nil the newest notification matching PATTERN
local function newest_notification (pattern)
  for i = #notified, 1, -1 do
    if notified[i]:find(pattern) then return notified[i] end
  end
  return nil
end

---@return integer[] the buffers of the diff view
local function diffView_buffers ()
  local found = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.api.nvim_buf_get_name(buf):find('diff-view: ', 1, true) then
      table.insert(found, buf) end
  end
  return found
end

---@return integer|nil the "all changes" buffer
local function all_changes_buffer ()
  for _, buf in ipairs(diffView_buffers()) do
    if vim.api.nvim_buf_get_name(buf):find('diff-view: all changes', 1, true) then
      return buf end
  end
  return nil
end

---@param skgrepo string data-root-relative
---@return string the summary line of SKGREPO's HEAD commit
local function git_head_summary (skgrepo)
  return vim.trim(vim.fn.system(
    { 'git', '-C', data_dir .. skgrepo, 'log', '-1', '--format=%s' }))
end

print('=== SKG Total Diff View Integration Test ===')

-- PHASE 1: open the diff view.
print('=== PHASE 1: Open the diff view ===')
require('skg.diff_view').diffView_open()
local all_changes = T.wait_for(function ()
  local buf = all_changes_buffer()
  if buf and T.buffer_text(buf):find('(id r)', 1, true) then return buf end
  return nil
end)
if not all_changes then T.fail('Timeout waiting for the "all changes" buffer') end
T.wait_for(function () return #diffView_buffers() >= 3 end, 10)
T.check(#diffView_buffers() >= 3,
        'the all-changes, merges and skgrepo-changes buffers opened')
do
  local text = T.buffer_text(all_changes)
  for _, expected in ipairs({
      '(oldText title)',                    -- t's text edit
      '(homeMove (old alpha) (new beta))',  -- m's move
      '(historical (id d)',                 -- d's deletion
      'acquiredFolder' }) do                -- x's nodeMerge
    T.check(text:find(expected, 1, true) ~= nil,
            'the "all changes" buffer shows ' .. expected)
  end
end

-- PHASE 2: mark every root, recursively. Each gesture saves, which
-- redraws the buffer, so the roots are found afresh. A root an
-- earlier gesture already readied is skipped: the gesture toggles, so
-- it would unready it.
print('=== PHASE 2: Mark everything commit-ready ===')
vim.api.nvim_set_current_buf(all_changes)
local function root_lines ()
  local found = {}
  for lnum, line in ipairs(vim.api.nvim_buf_get_lines(all_changes, 0, -1, false)) do
    if line:find('^%* ') then table.insert(found, lnum) end
  end
  return found
end
for i = 1, #root_lines() do
  local lnum = root_lines()[i]
  local line = vim.api.nvim_buf_get_lines(all_changes, lnum - 1, lnum, false)[1]
  if not line:find('(ready', 1, true) then
    local before = #notified
    vim.api.nvim_win_set_cursor(0, { lnum, 0 })
    require('skg.readiness').cycle_commit_ready_recursive()
    T.check(T.wait_for(function ()
      for j = before + 1, #notified do
        if notified[j]:find('changed files are ready', 1, true) then return true end
      end
      return nil
    end, 20), 'a readiness summary after the gesture on root ' .. i)
    T.wait_for_response(20)
  end
end
do
  local summary = newest_notification('changed files are ready')
  T.check(summary and summary:find('^COMMIT READY%.'),
          'commit-ready after marking every root: ' .. tostring(summary)
          .. '\n' .. T.buffer_text(all_changes))
end

-- PHASE 3: stage and commit.
print('=== PHASE 3: Stage and commit ===')
do
  local original_input = vim.fn.input
  vim.fn.input = function () return 'integration commit' end
  require('skg.stage_and_commit').stage_and_commit()
  vim.fn.input = original_input
end
T.check(T.wait_for(function () return newest_notification('^Committed in') end, 20),
        'the commit was reported')
T.wait_for_response(20)
T.check(git_head_summary('owned/alpha') == 'integration commit', 'alpha was committed')
T.check(git_head_summary('owned/beta') == 'integration commit', 'beta was committed')
T.check(git_head_summary('foreign/gamma') == 'Initial commit',
        'the foreign skgrepo was not committed')
T.check(T.wait_for(function ()
  for _, buf in ipairs(diffView_buffers()) do
    local text = T.buffer_text(buf)
    if not text:find('^%* ') then return nil end
    for _, change in ipairs({ '(diff ', 'changed)', 'changesetMarker',
                              'oldText', 'homeMove', 'acquiredFolder' }) do
      if text:find(change, 1, true) then return nil end
    end
  end
  return true
end, 10), 'the buffers of the diff view keep their roots, unchanged')

-- PHASE 4: reference d again, by hand; have the server reread the
-- files; open a view of r; investigate d there.
print('=== PHASE 4: Investigate the deleted node ===')
T.write_file(data_dir .. 'owned/alpha/r.skg',
  'pid: r\ntitle: the root\ncontains:\n- t\n- m\n- x\n- d\n')
require('skg.misc_requests').rebuild_session_lifetime_stores()
T.check(T.wait_for(function () return newest_notification('views are now invalid') end, 30),
        'the server reread the files')
require('skg.content_view').request_single_root_content_view_from_id('r')
local view = T.wait_for(function ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_content_view
       and not vim.api.nvim_buf_get_name(buf):find('diff-view: ', 1, true)
       and T.buffer_text(buf):find('(unknown (id d))', 1, true) then
      return buf end
  end
  return nil
end)
if not view then T.fail('Timeout waiting for a view of r drawing d as unknown') end
vim.api.nvim_set_current_buf(view)
for lnum, line in ipairs(vim.api.nvim_buf_get_lines(view, 0, -1, false)) do
  if line:find('(unknown (id d))', 1, true) then
    vim.api.nvim_win_set_cursor(0, { lnum, 0 }) break end
end
require('skg.investigate_unknown_node').investigate_unknown_node()
local answer = T.wait_for_buffer('skg://unknown-node', 20)
if not answer then T.fail('No investigation buffer') end
T.wait_for(function ()
  return T.buffer_text(answer):find('vanished at commit', 1, true) end)
do
  local text = T.buffer_text(answer)
  print(text)
  for _, expected in ipairs({
      '* d', 'title when last present: doomed', 'vanished at commit',
      'integration commit', 'r (via contains)' }) do
    T.check(text:find(expected, 1, true) ~= nil,
            'the investigation shows ' .. expected)
  end
end

print('')
print('All diff-view tests passed!')
T.pass()
