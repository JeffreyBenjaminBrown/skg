-- Integration test: the diff view's motions between changes, nvim
-- client. The Lua mirror of test-emacs.el in this directory; see
-- run-test.sh for the data.
--
-- Phase 1: Open the diff view; a's containers, mentioners and
--          forks folders each hold a write-protected member
--          with change facts.
-- Phase 2: <localleader>rnc visits every headline with change facts
--          but those three members, and <localleader>rnC visits them
--          too. Each member's node stands editable elsewhere, with a
--          change the plain motion visits there (Jeff's requirement).
-- Phase 3: <localleader>se on y inside the mentioners folder and save:
--          y's old text now stands only inside that folder, and the
--          plain motion visits it and y there.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(90)

local notified = {} -- every notification, oldest first
vim.notify = function (msg) table.insert(notified, tostring(msg)) end

local change_facts = require('skg.change_facts')
local metadata = require('skg.metadata')
local processed = require('skg.processed')

---@return integer|nil the "all changes" buffer
local function all_changes_buffer ()
  return T.first_buffer_where(function (buf)
    return vim.api.nvim_buf_get_name(buf):find('diff-view: all changes', 1, true)
  end)
end

---The lines of the current buffer a motion visits: every one with
---change facts when ANYWHERE, and otherwise the plain motion's. Walks
---by the commands themselves, from the top.
local function visited_lines (anywhere)
  local command = anywhere and processed.diffView_goto_next_change_anywhere
                            or processed.diffView_goto_next_change
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  local visited = {}
  if processed.diffView_change_start_on_line(1, nil, anywhere) then
    table.insert(visited, 1) end
  while true do
    local before = vim.api.nvim_win_get_cursor(0)[1]
    command()
    local after = vim.api.nvim_win_get_cursor(0)[1]
    if after == before then break end
    table.insert(visited, after)
  end
  return visited
end

---Fail unless each line of ALL not in VISITED is a ☮ member of a folder
---of other files whose node stands editable elsewhere in the buffer,
---where VISITED holds what it names: for 'node', a line there or under
---it; for 'rel', a line under it standing for the folder's viewparent.
---The skipped lines.
local function check_skipped_changes_are_drawn_editable (visited, all)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local is_visited = {}
  for _, v in ipairs(visited) do is_visited[v] = true end
  local skipped = {}
  for _, line_number in ipairs(all) do
    if not is_visited[line_number] then
      table.insert(skipped, line_number)
      local sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
      T.check(processed.diffView_change_of_other_files_p(line_number, sexp),
              'skipped line ' .. line_number .. ' is a ☮ member of an (other files) folder')
      local gnid = lines[line_number]:match('%(gnid ([^)]+)%)')
      local objects = change_facts.processed_leaves(sexp).objects
      local partner = lines[metadata.parent_line(metadata.parent_line(line_number))]
                        :match('%(gnid ([^)]+)%)')
      local found = false
      for start = 1, #lines do
        if lines[start]:match('^%*+ %(skg')
           and lines[start]:match('%(gnid ([^)]+)%)') == gnid then
          local occurrence = metadata.metadata_sexp_at_line_or_nil(start)
          if metadata.occurrence_sexp_p(occurrence)
             and not metadata.node_write_protected_p(occurrence) then
            local level = #lines[start]:match('^(%*+)')
            local stop = start + 1
            while stop <= #lines do
              local stars = lines[stop]:match('^(%*+) ')
              if stars and #stars <= level then break end
              stop = stop + 1
            end
            local node_drawn = not vim.tbl_contains(objects, 'node')
            local rel_drawn = not vim.tbl_contains(objects, 'rel')
            for _, v in ipairs(visited) do
              if v >= start and v < stop then node_drawn = true end
              if v > start and v < stop
                 and lines[v]:match('%(gnid ([^)]+)%)') == partner then
                rel_drawn = true end
            end
            if node_drawn and rel_drawn then found = true end
          end
        end
      end
      T.check(found, 'skipped line ' .. line_number .. ' (' .. tostring(gnid)
                     .. ') has an editable occurrence with a visited change')
    end
  end
  return skipped, lines
end

print('=== SKG Diff View Motions Integration Test ===')

-- PHASE 1: open the diff view.
print('=== PHASE 1: Open the diff view ===')
require('skg.diff_view').diffView_open()
local all_changes = T.wait_for(function ()
  local buf = all_changes_buffer()
  if buf and T.buffer_text(buf):find('forksFolder', 1, true) then return buf end
  return nil
end)
if not all_changes then T.fail('Timeout waiting for the "all changes" buffer') end
vim.api.nvim_set_current_buf(all_changes)
for _, folder in ipairs({ 'containerFolder', 'mentionerFolder', 'forksFolder' }) do
  T.check(T.buffer_text(all_changes):find(folder, 1, true) ~= nil,
          'the buffer shows a ' .. folder)
end

-- PHASE 2: the plain motion skips the three members; the capital does not.
print('=== PHASE 2: Plain and capital motions ===')
do
  local visited = visited_lines(false)
  local skipped, lines =
    check_skipped_changes_are_drawn_editable(visited, visited_lines(true))
  local gnids = {}
  for _, line_number in ipairs(skipped) do
    table.insert(gnids, lines[line_number]:match('%(gnid ([^)]+)%)')) end
  table.sort(gnids)
  T.check(vim.deep_equal(gnids, { 'r2', 's', 'y' }),
          'rnc skips r2, s and y, got ' .. vim.inspect(gnids))
  T.check(#visited > 3, 'rnc visits the rest')
end

-- PHASE 3: y made editable inside the mentioners folder.
print('=== PHASE 3: An editable member ===')
do
  local lines = vim.api.nvim_buf_get_lines(all_changes, 0, -1, false)
  local in_folder, y_line = false, nil
  for lnum, line in ipairs(lines) do
    if line:find('mentionerFolder', 1, true) then in_folder = true
    elseif in_folder and line:find('(gnid y)', 1, true) then y_line = lnum break end
  end
  T.check(y_line ~= nil, 'y stands in the mentioners folder')
  vim.api.nvim_win_set_cursor(0, { y_line, 0 })
  require('skg.views').request_editable_view()
  local before = #notified
  require('skg.save').request_save_buffer()
  local moved = T.wait_for(function ()
    local summary = false
    for j = before + 1, #notified do
      if notified[j]:find('changed nodes are processed', 1, true) then summary = true end
    end
    if not summary then return nil end
    local text = T.buffer_text(all_changes)
    local folder = text:find('mentionerFolder', 1, true)
    local old_text = folder and text:find('(oldText', folder, true)
    local next_folder = folder and text:find('forksFolder', folder, true)
    return old_text and next_folder and old_text < next_folder or nil
  end, 20)
  T.check(moved, "y's old text moved into the mentioners folder:\n"
                 .. T.buffer_text(all_changes))
  local visited = visited_lines(false)
  local _, after = check_skipped_changes_are_drawn_editable(visited, visited_lines(true))
  local folder_line, member_line, old_text_line
  for lnum, line in ipairs(after) do
    if not folder_line and line:find('mentionerFolder', 1, true) then folder_line = lnum
    elseif folder_line and not member_line and line:find('(gnid y)', 1, true) then member_line = lnum
    elseif member_line and not old_text_line and line:find('(oldText', 1, true) then old_text_line = lnum end
  end
  T.check(vim.tbl_contains(visited, member_line) and vim.tbl_contains(visited, old_text_line),
          'rnc visits y in the folder (line ' .. tostring(member_line)
          .. ') and its old text (line ' .. tostring(old_text_line) .. ')')
end

print('')
print('All diff-view motion tests passed!')
T.pass()
