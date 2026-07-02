-- Integration test: skg-goto-in-magit on a phantom whose file is gone
-- from disk, nvim client. The Lua mirror of test-emacs.el in this
-- directory.
--
-- HEAD:       a contains [b]; a.skg and b.skg both committed.
-- Worktree:   a no longer contains b; b.skg deleted from disk.
--
-- Phase 1: Open content view from a.
-- Phase 2: Toggle diff mode on. b appears as a phantom under a.
-- Phase 3: Resolve b's on-disk path and open its plain diff.
-- Phase 4: Verify the resolved path is exactly skg-data/b.skg (NOT
--          the outer skg project repo, and NOT a double-prefixed
--          nonexistent directory), and that point is on the buffer
--          for b.skg.
--
-- Regression under test: a relative-config-path launch where
-- config.data_root stays relative through config loading. In
-- get_file_path's response, canonicalize on a deleted file fails
-- (file gone), the raw path stays relative, strip_prefix against an
-- absolute data_root fails, and the fallback emits the full relative
-- path -- which the client then expands against its own config
-- directory (also absolute), producing a double-prefixed nonexistent
-- directory. The elisp test caught this via magit-toplevel landing on
-- the wrong repo; see run-test.sh for why this test launches the
-- server from a relative config path to reproduce the condition.
--
-- DEVIATION: the nvim client has no magit. skg.goto_git.goto_in_git
-- opens neogit asynchronously (hard to assert headless), so this test
-- exercises the request_file_path -> open_plain_diff_at chain
-- goto_in_git itself falls back to when neogit is unavailable,
-- calling it directly. This lets the test assert the resolved path
-- ITSELF, which is strictly more precise for this regression than
-- inspecting magit's toplevel afterward: a wrong/double-prefixed path
-- fails the comparison directly at its source, rather than via
-- magit's directory walk landing somewhere unexpected. b's own id is
-- not searched for within its own diff (there is no meaningful
-- "which file" ambiguity to resolve, unlike the parent-diff case in
-- view-magit-phantom): the diff buffer's name already identifies the
-- file.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45)

local goto_git = require('skg.goto_git')

do -- Mirror the elisp test's skg-config-dir: the directory holding
   -- the config file the server was actually launched with
   -- (data/skgconfig.toml.test), so the client's own path-resolution
   -- fallback matches what a real launch would use.
  local this_file = vim.fn.fnamemodify(
    debug.getinfo(1, 'S').source:sub(2), ':p')
  local test_dir = vim.fn.fnamemodify(this_file, ':h')
  require('skg.config').config_file_path =
    test_dir .. '/data/skgconfig.toml.test'
end

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

print('=== SKG View-Magit-Phantom-Deleted-File Integration Test ===')

-- PHASE 1: open content view from a.
print('=== PHASE 1: Open content view from a ===')
require('skg.content_view').request_single_root_content_view_from_id('a')
local content_view = find_content_view_buffer_matching('(id a)')
if not content_view then T.fail('Timeout waiting for content view of a') end
T.check(true, 'Content view of a loaded')

-- PHASE 2: toggle diff mode on; verify phantom b (removedX axis --
-- b.skg is deleted from disk).
print('=== PHASE 2: Toggle diff mode, verify phantom b ===')
vim.api.nvim_set_current_buf(content_view)
require('skg.diff_mode').toggle()
T.check(T.wait_for_response(20), 'diff-mode-on response arrived')
do
  local content = T.buffer_text(content_view)
  T.check(content:find('(id b)', 1, true) ~= nil,
          string.format('No (id b) in buffer. Content: %s', content))
  T.check(content:find('removedX', 1, true) ~= nil,
          string.format('No removedX axis on phantom. Content: %s',
                        content))
end

-- PHASE 3: resolve phantom b's path and open its plain diff.
print('=== PHASE 3: Resolve path and open diff for phantom b ===')
vim.api.nvim_set_current_buf(content_view)
do
  local lines = vim.api.nvim_buf_get_lines(content_view, 0, -1, false)
  local phantom_line = nil
  for lnum, line in ipairs(lines) do
    if line:find('(id b)', 1, true) then phantom_line = lnum break end
  end
  if not phantom_line then T.fail('Could not find phantom b line') end
  vim.api.nvim_win_set_cursor(0, { phantom_line, 0 })
  print('  Positioned on: ' .. lines[phantom_line])
end

local info = goto_git.node_info_at_point()
T.check(info ~= nil, 'node info (id, source) extracted at phantom b')

local resolved_path
goto_git.request_file_path(info.id, info.source,
  function (path) resolved_path = path end)
T.check(T.wait_for_response(20),
        'Timeout waiting for get-file-path response')
T.check(resolved_path ~= nil, 'a path was resolved for phantom b')
print('  Resolved path: ' .. tostring(resolved_path))

-- PHASE 4: verify the resolved path and the opened diff buffer.
print('=== PHASE 4: Verify resolved path and diff buffer ===')
if resolved_path then
  local this_file = vim.fn.fnamemodify(
    debug.getinfo(1, 'S').source:sub(2), ':p')
  local test_dir = vim.fn.fnamemodify(this_file, ':h')
  local expected_path =
    vim.fn.fnamemodify(test_dir .. '/data/skg-data/b.skg', ':p')
  local normalized_resolved = vim.fn.fnamemodify(resolved_path, ':p')
  T.check(normalized_resolved == expected_path, string.format(
    "resolved path is skg-data/b.skg, not a double-prefixed or"
    .. " wrong-repo path.\n  Expected: %s\n  Got:      %s",
    expected_path, normalized_resolved))

  goto_git.open_plain_diff_at(resolved_path, nil)
  local diff_buf = T.wait_for_buffer('skg://git-diff/b.skg', 5)
  T.check(diff_buf ~= nil, "diff buffer opened, named 'skg://git-diff/b.skg'")
  if diff_buf then
    local content = T.buffer_text(diff_buf)
    print('  b.skg diff: ' .. content)
    T.check(content:find('deleted file', 1, true) ~= nil,
            "diff shows b.skg as a deleted file")
  end
end

print('')
print('All view-magit-phantom-deleted-file tests passed!')
T.pass()
