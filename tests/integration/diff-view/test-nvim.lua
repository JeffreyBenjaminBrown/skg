-- Integration test: the diff view, end to end, nvim client. The
-- Lua mirror of test-emacs.el in this directory; see run-test.sh for
-- the data.
--
-- Phase 1: Open the diff view; its buffers open.
-- Phase 2: Mark and unmark with each letter and each walk, and see
--          what the server says is processed; then mark every change
--          processed, through '<localleader>mbP' on each root of the
--          "all changes" buffer, and see "Commit ready".
-- Phase 3: Stage and commit; every owned skgrepo is committed, the
--          foreign one is not, and the buffers keep their roots,
--          which show no change any more.
-- Phase 4: With d referenced again by hand, rebuild, which
--          rerenders every view; open a view of r beside the
--          buffers of the diff view, one of which r is a root of,
--          and investigate the unknown node d.

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

print('=== SKG Diff View Integration Test ===')

-- PHASE 1: open the diff view.
print('=== PHASE 1: Open the diff view ===')
require('skg.diff_view').diffView_open()
local all_changes = T.wait_for(function ()
  local buf = all_changes_buffer()
  if buf and T.buffer_text(buf):find('(gnid r)', 1, true) then return buf end
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
      '(homeSkgrepoMove (old alpha) (new beta))',  -- m's move
      'ghost (gnid d)',                    -- d's deletion
      'acquiredFolder',                     -- x's nodeMerge
      '(gnid n3)' }) do                     -- new content, three deep
    T.check(text:find(expected, 1, true) ~= nil,
            'the "all changes" buffer shows ' .. expected)
  end
end

-- PHASE 2: mark with each letter and each walk, then every root.
print('=== PHASE 2: Mark everything processed ===')
vim.api.nvim_set_current_buf(all_changes)
local processed = require('skg.processed')

---@param needle string plain text
---@return integer the first line of the "all changes" buffer holding NEEDLE
local function line_holding (needle)
  for lnum, line in ipairs(vim.api.nvim_buf_get_lines(all_changes, 0, -1, false)) do
    if line:find(needle, 1, true) then return lnum end
  end
  T.fail('No line holds ' .. needle .. ':\n' .. T.buffer_text(all_changes))
end

---Run COMMAND on the first headline holding NEEDLE, answering yes if it
---asks, and wait for the processed summary of its save.
local function mark_at (needle, command, what)
  vim.api.nvim_win_set_cursor(0, { line_holding(needle), 0 })
  local before = #notified
  local original_input = vim.fn.input
  vim.fn.input = function () return 'yes' end
  command()
  vim.fn.input = original_input
  T.check(T.wait_for(function ()
    for j = before + 1, #notified do
      if notified[j]:find('changed files are processed', 1, true) then return true end
    end
    return nil
  end, 20), 'a processed summary after ' .. what)
end

---Check that the headline holding NEEDLE holds PROCESSED (plain text),
---or, when that is nil, no '(processed ...)'.
local function expect_processed (needle, processed_text, what)
  local line = vim.api.nvim_buf_get_lines(
    all_changes, line_holding(needle) - 1, line_holding(needle), false)[1]
  if processed_text then
    T.check(line:find(processed_text, 1, true) ~= nil, what .. ': ' .. line)
  else
    T.check(not line:find('(processed', 1, true), what .. ': ' .. line) end
end

-- Each letter, without a walk, on the deleted node d.
mark_at('ghost (gnid d)', processed.mark_node_processed, 'mnp on d')
expect_processed('ghost (gnid d)', '(processed node))', 'mnp on d')
mark_at('ghost (gnid d)', processed.mark_rel_processed, 'mrp on d')
expect_processed('ghost (gnid d)', '(processed node rel))', 'mrp on d')
mark_at('ghost (gnid d)', processed.mark_both_unprocessed, 'mbu on d')
expect_processed('ghost (gnid d)', nil, 'mbu on d')
-- The walks, on the new node n: outward naming relationships, then
-- contentward naming nodes; n3, three deep, follows both.
mark_at('(gnid n)', processed.mark_rel_processed_outward, 'Mrp on n')
expect_processed('(gnid n)', '(processed rel))', 'Mrp on n')
expect_processed('(gnid n3)', '(processed rel))', 'Mrp on n, at n3')
mark_at('(gnid n)', processed.mark_node_processed_contentward, 'mnP on n')
expect_processed('(gnid n)', '(processed node rel))', 'mnP on n')
expect_processed('(gnid n3)', '(processed node rel))', 'mnP on n, at n3')
-- m's home skgrepo move.
mark_at('(gnid m)', processed.mark_homeSkgrepo_move_processed, 'mhp on m')
expect_processed('(homeSkgrepoMove (old alpha) (new beta))', '(processed)', 'mhp on m')

-- Every root, contentward. Each gesture saves, which rerenders the
-- buffer, so the roots are found afresh.
local function root_lines ()
  local found = {}
  for lnum, line in ipairs(vim.api.nvim_buf_get_lines(all_changes, 0, -1, false)) do
    if line:find('^%* ') then table.insert(found, lnum) end
  end
  return found
end
for i = 1, #root_lines() do
  local before = #notified
  vim.api.nvim_win_set_cursor(0, { root_lines()[i], 0 })
  processed.mark_both_processed_contentward()
  T.check(T.wait_for(function ()
    for j = before + 1, #notified do
      if notified[j]:find('changed files are processed', 1, true) then return true end
    end
    return nil
  end, 20), 'a processed summary after the gesture on root ' .. i)
end
do
  local summary = newest_notification('changed files are processed')
  T.check(summary and summary:find('^Commit ready%.'),
          'processed after marking every root: ' .. tostring(summary)
          .. '\n' .. T.buffer_text(all_changes))
end

-- PHASE 3: stage and commit.
print('=== PHASE 3: Stage and commit ===')
do
  local stage_and_commit = require('skg.stage_and_commit')
  stage_and_commit.stage_and_commit()
  local message_buf = stage_and_commit.commit_message_buffer()
  vim.api.nvim_buf_set_lines(message_buf, 0, -1, false,
                             { 'integration commit', '' })
  vim.api.nvim_buf_call(message_buf, function () vim.cmd('write') end)
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
    for _, change in ipairs({ '(diff ', 'changesetMarker',
                              'oldText', 'homeSkgrepoMove', 'acquiredFolder' }) do
      if text:find(change, 1, true) then return nil end
    end
  end
  return true
end, 10), 'the buffers of the diff view keep their roots, unchanged')

-- PHASE 4: reference d again, by hand; have the server reread the
-- files, which rerenders every view; open a view of r; investigate d
-- there.
print('=== PHASE 4: Investigate the deleted node ===')
T.write_file(data_dir .. 'owned/alpha/r.skg',
  'pid: r\ntitle: the root\ncontains:\n- t\n- m\n- x\n- d\n')
do
  local rerender = require('skg.rerender_all_views')
  local request_rerender_all_views = rerender.request_rerender_all_views
  local rerender_sent = false
  rerender.request_rerender_all_views = function ()
    rerender_sent = true
    return request_rerender_all_views() end
  require('skg.misc_requests').rebuild_session_lifetime_stores()
  T.check(T.wait_for(function () return newest_notification('rebuilt successfully') end, 30),
          'the server reread the files')
  T.check(T.wait_for(function ()
    return rerender_sent and not require('skg.lock').stream_in_progress end, 30),
    'every view was rerendered after the rebuild')
  rerender.request_rerender_all_views = request_rerender_all_views
end
-- The buffers of the diff view survive the rebuild, and r is a root of
-- one. A view request for r does not switch to it: a content view of r
-- opens beside them.
require('skg.single_root_content_view').request_single_root_content_view_from_id('r')
local view = T.wait_for(function ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_content_view
       and not vim.api.nvim_buf_get_name(buf):find('diff-view: ', 1, true)
       and T.buffer_text(buf):find('unknown (gnid d)', 1, true) then
      return buf end
  end
  return nil
end)
if not view then T.fail('Timeout waiting for a view of r showing d as unknown') end
T.check(#diffView_buffers() > 0,
        'a content view of r opened beside the buffers of the diff view')
vim.api.nvim_set_current_buf(view)
for lnum, line in ipairs(vim.api.nvim_buf_get_lines(view, 0, -1, false)) do
  if line:find('unknown (gnid d)', 1, true) then
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
