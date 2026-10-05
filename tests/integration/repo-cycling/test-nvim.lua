-- Integration test for skgrepo cycling in the metadata edit buffer,
-- nvim client. The Lua mirror of test-emacs.el in this directory.
-- Verifies that S-left / S-right (metadata_edit.cycle) cycle through
-- owned skgrepos only, excluding the foreign (foreign)
-- skgrepo.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(20)

local metadata = require('skg.metadata')
local metadata_edit = require('skg.metadata_edit')

do -- SKG_CONFIG_FILE, the analog of the elisp test's skg-config-dir
   -- setup: it lets config.owned_repos() see the four fixture
   -- skgrepos (public/personal/private owned, foreign not).
  local config_file = os.getenv('SKG_CONFIG_FILE')
  if config_file then
    require('skg.config').config_file_path =
      vim.fn.fnamemodify(config_file, ':p')
  end
end

---The skgrepo value headline text in the metadata edit buffer (the
---child of the '*** repo' headline). Port of
---repo-value-in-edit-buffer.
---@return string|nil
local function repo_value_in_edit_buffer ()
  local last = vim.api.nvim_buf_line_count(0)
  for line = 1, last do
    if metadata.line_text(line):match('^%*%*%* repo$') then
      local value_line = metadata.next_headline_line(line)
      if value_line then
        return vim.trim(
          metadata.line_text(value_line):match('^%*+%s*(.*)$') or '')
      end
      return nil
    end
  end
  return nil
end

---@param message string
local function fail (message) T.fail(message) end

print('=== SKG Repo Cycling Integration Test ===')

-- PHASE 1: open content view for node 'x'.
print('=== PHASE 1: Open content view ===')
require('skg.content_view').request_single_root_content_view_from_id('x')
local content_view = T.wait_for(function ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_content_view
       and T.buffer_text(buf):find('(id x)', 1, true) then
      return buf
    end
  end
  return nil
end)
if not content_view then fail('Content view not created') end
vim.api.nvim_set_current_buf(content_view)
T.check(true, 'Content view opened')

-- PHASE 2: open the metadata edit buffer, go to the skgrepo value.
print('=== PHASE 2: Open metadata edit buffer ===')
vim.api.nvim_win_set_cursor(0, { 1, 0 })
if not metadata.at_headline_p() then fail('Not on a headline') end
metadata_edit.edit_metadata() -- switches to the edit buffer, cursor
                               -- lands on the skgrepo value already
local initial_skgrepo = repo_value_in_edit_buffer()
print(string.format('  Initial repo: %s', tostring(initial_skgrepo)))
T.check(initial_skgrepo == 'public',
        "metadata edit buffer opened, repo is 'public'")

-- PHASE 3: S-right should change skgrepo to the next owned skgrepo.
print('=== PHASE 3: Cycle right ===')
metadata_edit.cycle(1)
local after_right = repo_value_in_edit_buffer()
print(string.format('  After S-right: %s', tostring(after_right)))
T.check(after_right == 'personal',
        "S-right changed repo to 'personal'")

-- PHASE 4: S-left should change skgrepo back to 'public'.
print('=== PHASE 4: Cycle left ===')
metadata_edit.cycle(-1) -- point is still on the skgrepo value
local after_left = repo_value_in_edit_buffer()
print(string.format('  After S-left: %s', tostring(after_left)))
T.check(after_left == 'public', "S-left changed repo back to 'public'")

-- PHASE 5: three S-rights from 'public' wraps back to 'public'.
print('=== PHASE 5: Three S-rights wraps around ===')
-- Currently at 'public'. Three owned skgrepos: public, personal, private.
metadata_edit.cycle(1) -- -> personal
metadata_edit.cycle(1) -- -> private
metadata_edit.cycle(1) -- -> public (wrap)
local after_wrap = repo_value_in_edit_buffer()
print(string.format('  After 3x S-right: %s', tostring(after_wrap)))
T.check(after_wrap == 'public',
        "Three S-rights wraps back to 'public'")
local cycle_values = metadata_edit.cycle_values_for_field('repo', 'public')
local foreign_offered = false
for _, value in ipairs(cycle_values or {}) do
  if value == 'foreign' then foreign_offered = true end
end
T.check(not foreign_offered, "'foreign' never offered while cycling")

print('=== All phases passed ===')
T.pass()
