-- Integration test for the fork-confirmation buffer's EDITABLE clone
-- source, nvim client. The Lua mirror of test-emacs.el in this
-- directory: open owned P (whose content is foreign N); make N
-- definitive and edit its title; save -> a fork-confirmation buffer.
-- The clone's source is inferred as "owned"; rotate it to "owned2" in
-- the confirmation buffer, then approve. The clone must land in
-- "owned2" (the rotated source), not in "owned".

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(40)

local content_view = require('skg.content_view')
local metadata = require('skg.metadata')
local save = require('skg.save')

---Return a live skg view buffer whose text mentions (id ID).
local function buffer_showing (id)
  local needle = '(id ' .. id .. ')'
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_view_uri ~= nil
       and T.buffer_text(buf):find(needle, 1, true) then
      return buf
    end
  end
  return nil
end

---Move the cursor in the CURRENT buffer onto the first line containing
---SNIPPET anywhere. Fails the test if no line does.
local function goto_line_containing (snippet, description)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for i, line in ipairs(lines) do
    if line:find(snippet, 1, true) then
      vim.api.nvim_win_set_cursor(0, { i, 0 })
      return i
    end
  end
  T.fail(description or ('could not find a line containing: ' .. snippet))
end

---Move the cursor in the CURRENT buffer onto the first line that
---STARTS WITH SNIPPET (the analog of an elisp "^..."-anchored search).
---Fails the test if no line does.
local function goto_line_starting_with (snippet, description)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for i, line in ipairs(lines) do
    if line:sub(1, #snippet) == snippet then
      vim.api.nvim_win_set_cursor(0, { i, 0 })
      return i
    end
  end
  T.fail(description or ('could not find a line starting with: ' .. snippet))
end

---Names of the .skg files directly inside DIR (relative to the test's
---working directory), or {} if DIR does not exist.
local function skg_files_in (dir)
  if vim.fn.isdirectory(dir) == 0 then return {} end
  local names = {}
  for _, path in ipairs(vim.fn.glob(dir .. '/*.skg', false, true)) do
    table.insert(names, vim.fn.fnamemodify(path, ':t'))
  end
  return names
end

print('Starting fork source-rotation integration test...')

-- 1. Open owned P; its content is the foreign node N.
content_view.request_single_root_content_view_from_id('P')
local p_buf = T.wait_for(function () return buffer_showing('P') end, 10)
T.check(p_buf, "P's view never appeared")
vim.api.nvim_set_current_buf(p_buf)

-- 2. Make N definitive and edit its title -- the fork gesture.
local n_line = goto_line_containing('(id N) (source foreign)',
  "could not find N's headline")
local n_line_text =
  vim.api.nvim_buf_get_lines(p_buf, n_line - 1, n_line, false)[1]
local n_line_edited =
  n_line_text:gsub(' indef', ''):gsub('N%-original', 'N-edited')
vim.api.nvim_buf_set_lines(p_buf, n_line - 1, n_line, false,
                           { n_line_edited })

-- 3. Save -> fork-confirmation (nothing committed).
save.request_save_buffer()

-- 4. The confirmation buffer appears. Rotate the clone-to-be's source
--    from the inferred "owned" to "owned2", then approve.
local confirm_buf = T.wait_for_buffer('skg://fork-confirmation', 10)
T.check(confirm_buf, 'no fork-confirmation buffer appeared')
vim.api.nvim_set_current_buf(confirm_buf)
-- The elisp original asserts '(source owned)' here, which is STALE:
-- the server always pre-fills the PICK-A-SOURCE placeholder and only
-- SUGGESTS the inferred source in a comment line (fork.rs; documented
-- in COMMANDS.org and glossary.md). The emacs test fails on this
-- today -- see the problems.org entry filed with the vim-client port.
-- This mirror asserts the documented behavior instead.
local confirmation_text = T.buffer_text(confirm_buf)
T.check(confirmation_text:find('(source PICK-A-SOURCE)', 1, true),
        'clone-to-be carries the PICK-A-SOURCE placeholder')
T.check(confirmation_text:find('Suggested source for the clone'
                               .. ' below: owned', 1, true),
        "the inferred source 'owned' is suggested in the comment")
-- Move to the clone-to-be parent (the first, level-1 headline) and
-- rotate its source -- what <localleader>ss does interactively.
local clone_line = goto_line_starting_with('* (skg',
  'could not find the clone-to-be headline')
metadata.change_source_at_line(clone_line, 'owned2')
T.check(T.buffer_text(confirm_buf):find('(source owned2)', 1, true),
        'rotation did not set source owned2')
print("rotated the clone's source to owned2")

-- 5. Approve: re-save the origin with the chosen source.
save.approve_fork()

-- 6. The clone must land in owned2 (rotated), NOT owned (inferred).
local committed = T.wait_for(function ()
  return #skg_files_in('data/owned2') == 1
end, 10)
T.check(committed, string.format(
  'no clone appeared in owned2; owned2=%s owned=%s',
  vim.inspect(skg_files_in('data/owned2')),
  vim.inspect(skg_files_in('data/owned'))))

local owned_files = skg_files_in('data/owned')
T.check(#owned_files == 1 and owned_files[1] == 'P.skg', string.format(
  'owned should still hold only P.skg (the clone went to owned2), got %s',
  vim.inspect(owned_files)))

local clone_full_path = vim.fn.glob('data/owned2/*.skg', false, true)[1]
local clone_content = table.concat(vim.fn.readfile(clone_full_path), '\n')
T.check(clone_content:find('overrides_view_of', 1, true),
        'the clone in owned2 should override N')
print('the clone landed in the rotated source owned2 and overrides N')

T.pass('PASS: Fork source-rotation integration test successful!')
