-- Integration test for skg-goto-in-magit, nvim client. The Lua
-- mirror of test-emacs.el in this directory.
--
-- DEVIATION: the nvim client has no magit. goto_git.goto_in_git opens
-- neogit asynchronously (hard to assert headless), so this test
-- exercises the same request_file_path -> open_plain_diff_at chain
-- goto_in_git itself falls back to when neogit is unavailable,
-- calling it directly instead: request the file's path, then open a
-- plain 'git diff HEAD -- FILE' buffer for it. The buffer opening (or
-- not) is the observable analog of the file showing as modified (or
-- not) in a magit status listing.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local goto_git = require('skg.goto_git')

local captured_notifications = {}
do
  local original_notify = vim.notify
  vim.notify = function (message, ...)
    table.insert(captured_notifications, message)
    return original_notify(message, ...)
  end
end

---@param needle string
---@return boolean was NEEDLE reported via vim.notify since the test began?
local function notified_containing (needle)
  for _, message in ipairs(captured_notifications) do
    if tostring(message):find(needle, 1, true) then return true end
  end
  return false
end

---The open content-view buffer whose text contains NEEDLE, waiting up
---to TIMEOUT_SECS. Port of the elisp tests' repeated
---(cl-find-if (derived-mode-p 'skg-content-view-mode) ...) idiom,
---using the skg_content_view buffer-local flag as the mode analog.
---@param needle string
---@param timeout_secs number|nil
---@return integer|nil bufnr
local function find_content_view_buffer_matching (needle, timeout_secs)
  return T.wait_for(function ()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_content_view
         and T.buffer_text(buf):find(needle, 1, true) then
        return buf
      end
    end
    return nil
  end, timeout_secs)
end

---Request the on-disk path and open a plain diff for the node at
---point, synchronously. The direct-node analog of goto_git.goto_in_git
---(see the file header for why we bypass it).
---@return integer|nil diff_buf
local function open_plain_diff_for_node_at_point ()
  local info = goto_git.node_info_at_point()
  if not info then return nil end
  local resolved_path
  goto_git.request_file_path(info.id, info.source,
    function (path) resolved_path = path end)
  if not T.wait_for_response() then return nil end
  if not resolved_path then return nil end
  print('Resolved path: ' .. resolved_path)
  local before = vim.api.nvim_list_bufs()
  goto_git.open_plain_diff_at(resolved_path, nil)
  -- open_plain_diff_at only switches to a NEW buffer when the file has
  -- changes; otherwise it just notifies. Report whichever happened.
  local current = vim.api.nvim_get_current_buf()
  for _, buf in ipairs(before) do
    if buf == current then return nil end -- unchanged: no new buffer
  end
  return current
end

print('=== SKG View-Magit Integration Test ===')

-- PHASE 0: connect and request a content view of x.
print('=== PHASE 0: Init and request content view ===')
require('skg.content_view').request_single_root_content_view_from_id('x')
local content_view = find_content_view_buffer_matching('(id x)')
if not content_view then T.fail('Timeout waiting for content view of x') end
T.check(true, 'Content view of x loaded')

-- PHASE 1: from line 1 (node x, modified file), the diff buffer
-- should open showing x's uncommitted title change.
print('=== PHASE 1: diff for x (modified file) ===')
vim.api.nvim_set_current_buf(content_view)
vim.api.nvim_win_set_cursor(0, { 1, 0 })
print('Buffer content: ' .. T.buffer_text(content_view))
local diff_buf_x = open_plain_diff_for_node_at_point()
T.check(diff_buf_x ~= nil, 'diff buffer opened for x (it is modified)')
if diff_buf_x then
  T.check(vim.api.nvim_buf_get_name(diff_buf_x) == 'skg://git-diff/x.skg',
          "diff buffer is named 'skg://git-diff/x.skg'")
  local diff_text = T.buffer_text(diff_buf_x)
  print('x.skg diff: ' .. diff_text)
  -- DEVIATION: magit's per-file status wording ('modified   x.skg')
  -- has no analog in a plain unified diff; a non-empty diff for x's
  -- own file is the observable equivalent of "modified".
  T.check(diff_text:find('title', 1, true) ~= nil,
          "x.skg's diff shows the title change")
end

-- PHASE 2: from line 2 (node y, unchanged file), no diff buffer
-- should open; the client should report there is nothing to show.
print('=== PHASE 2: diff for y (unchanged file) ===')
vim.api.nvim_set_current_buf(content_view)
local lines = vim.api.nvim_buf_get_lines(content_view, 0, -1, false)
vim.api.nvim_win_set_cursor(0, { 2, 0 })
print('Line 2: ' .. (lines[2] or ''))
local diff_buf_y = open_plain_diff_for_node_at_point()
T.check(diff_buf_y == nil, 'no diff buffer opened for y (it is unchanged)')
T.check(notified_containing('has no unstaged or staged changes'),
        "notify reported y.skg has no changes")

print('')
print('All view-magit tests passed!')
T.pass()
