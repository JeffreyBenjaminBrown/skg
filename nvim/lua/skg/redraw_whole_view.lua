-- PURPOSE: Redraw a whole view from the server's new text -- the
-- saved view after ':w', or another view a save or a rerender
-- changed -- keeping each window's cursor and the folds where they
-- were. The entry points: 'apply_streamed_view_update', which the
-- save's collateral-view and the rerender's rerender-view messages
-- share;
-- 'replace_buffer_keeping_viewnode_points_and_folds', for a redraw
-- the user did not ask for; and 'replace_buffer_with_new_content',
-- for the saved buffer, whose cursor 'current_save_point_position'
-- measured before the save and 'restore_save_point_position' puts
-- back. Rewriting only the headlines a save changed is
-- skg.redraw_delta's; choosing between that and a whole redraw, for
-- the saved buffer, is skg.save's ('redraw_saved_buffer'). The Lua
-- port of the redraw part of elisp/skg-request-save.el.

local buffer = require('skg.buffer')
local focus = require('skg.focus')
local folds = require('skg.folds')
local length_prefix = require('skg.length_prefix')
local lock = require('skg.lock')
local log = require('skg.log')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local redraw_delta = require('skg.redraw_delta')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Apply one streamed view update: unlock and replace the buffer for
---its view ID. Shared by the save (collateral-view) and rerender
---(rerender-view) streams. If KEEP_UNSAVED_EDITS is true, a buffer
---with unsaved edits is left as it is, and named in a notification.
---@param _payload_text string
---@param response any
---@param log_category string
---@param handler_name string
---@param keep_unsaved_edits boolean|nil
function M.apply_streamed_view_update (_payload_text, response,
                                       log_category, handler_name,
                                       keep_unsaved_edits)
  local ok, err = pcall(function ()
    local view_id = payload.field_text(response, 'view-id')
    local content = payload.field(response, 'content')
    local buf = view_id and buffer.find_buffer_by_view_id(view_id) or nil
    if buf and content ~= nil and not sexpr.is_list(content) then
      if keep_unsaved_edits and vim.bo[buf].modified then
        vim.notify('skg: not redrawn by the save, having unsaved edits: '
                   .. vim.api.nvim_buf_get_name(buf))
      else
        lock.unlock_after_save(buf)
        M.replace_buffer_keeping_viewnode_points_and_folds(
          buf, sexpr.atom_text(content))
      end
    end
  end)
  if not ok then
    log.log('error', log_category, '%s handler error: %s',
            handler_name, tostring(err))
    length_prefix.tell_answer_not_applied(
      handler_name, err,
      log_category == 'save'
        and 'the server may have saved, but a view was not redrawn'
        or 'a view was not redrawn')
  end
end

---Replace BUF with NEW_CONTENT, a redraw the user did not ask for,
---keeping each window's cursor and scroll, and the folds, where they
---were.
---The server draws such a redraw with the folds of the view's last
---save, since folds reach it only in a save. So the fold state of each
---headline here, in the window whose folds a redraw sets, is recorded
---by viewnode ID, and decides the fold atoms of NEW_CONTENT
---('M.content_with_fold_atoms'); what only a headline new in
---NEW_CONTENT decides keeps the server's.
---A save marks the headline at the cursor focused, and restores the cursor
---relative to it ('M.restore_save_point_position'); here the server
---drew the view on its own, so its focused headline is the one the
---last save left. Instead each window's cursor is recorded as its
---headline's viewnode ID, which a rerender keeps
---('docs/api-and-formats.org', "(vnid X)"), with the offsets a save
---measures, and restored by them. A cursor whose viewnode is gone goes
---to the start of the nearest headline that remains. A cursor whose
---headline has no viewnode ID (one typed since the last save, which a
---save held back marks focused) goes to the focused headline, as the
---server drew it. A buffer shown in no window has no cursor to keep.
---@param buf integer
---@param new_content string
function M.replace_buffer_keeping_viewnode_points_and_folds (buf,
                                                             new_content)
  local old_headlines = redraw_delta.viewnode_id_headlines(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false))
  local positions_by_window = {}
  for _, window in ipairs(vim.fn.win_findbuf(buf)) do
    positions_by_window[window] = vim.api.nvim_win_call(window, function ()
      return M.viewnode_point_position(old_headlines) end) end
  local fold_window = buffer.first_window_showing(buf)
  if fold_window then
    local fold_states_by_viewnode_id =
      vim.api.nvim_win_call(fold_window, function ()
        return M.fold_states_by_viewnode_id(old_headlines) end)
    new_content = M.content_with_fold_atoms(new_content,
                                            fold_states_by_viewnode_id) end
  M.replace_buffer_with_new_content(buf, new_content, nil)
  local focused_line = M.focused_line_after_redraw(buf)
  local old_ids = {}
  for _, headline in ipairs(old_headlines) do
    table.insert(old_ids, headline.id) end
  local headline_lines = redraw_delta.headline_indices_by_viewnode_id(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), old_ids)
  for window, position in pairs(positions_by_window) do
    vim.api.nvim_win_call(window, function ()
      M.goto_viewnode_point_position(position, headline_lines,
                                     focused_line) end) end
end

---A table from the viewnode ID of each of OLD_HEADLINES
---('redraw_delta.viewnode_id_headlines' of the current buffer) to the
---fold state of its headline in the current window
---('folds.fold_state_of_headline_at_line').
---@param old_headlines table[]
---@return table<string, string>
function M.fold_states_by_viewnode_id (old_headlines)
  local fold_states = {}
  for _, headline in ipairs(old_headlines) do
    fold_states[headline.id] =
      folds.fold_state_of_headline_at_line(headline.index) end
  return fold_states
end

---CONTENT, a view's text, with the fold atoms that
---FOLD_STATES_BY_VIEWNODE_ID ('M.fold_states_by_viewnode_id' of the
---buffer it will replace) decides. A headline's 'folded' atom says its
---parent's fold hides it, so its parent's state decides it
---('M.folded_atom_decision'); its 'bodyFolded' atom, its own
---('M.bodyFolded_atom_decision'). An atom whose deciding headline the
---table lacks, being new in CONTENT, is kept as the server drew it.
---Other lines, and the focus atom, are kept.
---@param content string
---@param fold_states_by_viewnode_id table<string, string>
---@return string
function M.content_with_fold_atoms (content, fold_states_by_viewnode_id)
  local lines = vim.split(content, '\n', { plain = true })
  local headlines = M.headline_levels_and_viewnode_ids(lines)
  local ancestors = {} -- { level, id }, nearest last
  for position, headline in ipairs(headlines) do
    while #ancestors > 0 and ancestors[#ancestors].level >= headline.level do
      table.remove(ancestors) end
    if headline.id then
      local parent = ancestors[#ancestors]
      local later = headlines[position + 1]
      local decisions = {}
      table.insert(decisions, M.folded_atom_decision(
        parent ~= nil,
        parent and parent.id and fold_states_by_viewnode_id[parent.id]))
      table.insert(decisions, M.bodyFolded_atom_decision(
        fold_states_by_viewnode_id[headline.id],
        later ~= nil and later.level > headline.level))
      lines[headline.index] =
        M.line_with_fold_atoms(lines[headline.index], decisions) end
    table.insert(ancestors, headline)
  end
  return table.concat(lines, '\n')
end

---Each headline among LINES, a view's lines, as { index, level, id },
---in order; id is nil for a headline without one.
---@param lines string[]
---@return table[]
function M.headline_levels_and_viewnode_ids (lines)
  local headlines = {}
  for index, line in ipairs(lines) do
    local stars = line:match('^(%*+) ')
    if stars then
      table.insert(headlines, {
        index = index, level = #stars,
        id = redraw_delta.viewnode_id_at_headline_start(line) }) end end
  return headlines
end

---Whether a headline is 'folded', as { atom = 'folded', value = BOOL },
---given whether it HAS_PARENT and the PARENT_FOLD_STATE that parent had
---('folds.fold_state_of_headline_at_line'): it is when its parent hid
---its children or was hidden, and is not when its parent showed them.
---Nil, leaving the server's atom, for a top-level headline or one whose
---parent had no state, being new.
---@param has_parent boolean
---@param parent_fold_state string|nil
---@return table|nil
function M.folded_atom_decision (has_parent, parent_fold_state)
  if not has_parent then return nil end
  if parent_fold_state == 'hidden' or parent_fold_state == 'collapsed' then
    return { atom = 'folded', value = true } end
  if parent_fold_state == 'open' or parent_fold_state == 'bodyHidden' then
    return { atom = 'folded', value = false } end
  return nil
end

---Whether a headline is 'bodyFolded', as { atom = 'bodyFolded',
---value = BOOL }, given the FOLD_STATE it had
---('folds.fold_state_of_headline_at_line') and whether it HAS_CHILDREN
---now: it is when only its body was hidden, or when it was collapsed
---and its children are gone, so that the fold hides its body still.
---Nil, leaving the server's atom, when it had no state, being new.
---@param fold_state string|nil
---@param has_children boolean
---@return table|nil
function M.bodyFolded_atom_decision (fold_state, has_children)
  if fold_state == 'bodyHidden' then
    return { atom = 'bodyFolded', value = true } end
  if fold_state == 'collapsed' then
    return { atom = 'bodyFolded', value = not has_children } end
  if fold_state == 'hidden' or fold_state == 'open' then
    return { atom = 'bodyFolded', value = false } end
  return nil
end

---LINE, a headline line beginning with its viewnode ID, with each atom
---DECISIONS names ('M.folded_atom_decision', 'M.bodyFolded_atom_decision')
---removed, and those whose value is true placed after the viewnode ID.
---@param line string
---@param decisions table[]
---@return string
function M.line_with_fold_atoms (line, decisions)
  if #decisions == 0 then return line end
  local viewnode_id_end = select(2, line:find('^%*+ %(skg %(vnid [^)]*%)'))
  local decided_atoms, true_atoms = {}, {}
  for _, decision in ipairs(decisions) do
    decided_atoms[decision.atom] = true
    if decision.value then
      table.insert(true_atoms, ' ' .. decision.atom) end end
  local without_decided_atoms = redraw_delta.without_fold_and_focus_atoms(
    line, line:find('(skg', 1, true), decided_atoms)
  return without_decided_atoms:sub(1, viewnode_id_end)
         .. table.concat(true_atoms)
         .. without_decided_atoms:sub(viewnode_id_end + 1)
end

---The line of BUF's focused headline once a whole redraw has acted on
---its atoms: where that redraw left the cursor of the window it acted
---in, the first window showing BUF ('buffer.act_on_fold_and_focus_atoms').
---@param buf integer
---@return integer|nil
function M.focused_line_after_redraw (buf)
  local window = buffer.first_window_showing(buf)
  return window and vim.api.nvim_win_get_cursor(window)[1]
end

---Where the current window's cursor stands: the save's measures
---('M.current_save_point_position'), with viewnode_ids, the viewnode
---IDs of OLD_HEADLINES ('redraw_delta.viewnode_id_headlines' of the
---current buffer), the one owning the cursor first and the rest by
---their distance in lines from the cursor, nearest first, and
---owner_viewnode_id, the owning one's, nil when it has none. Nil when
---the cursor precedes every headline.
---@param old_headlines table[]
---@return table|nil
function M.viewnode_point_position (old_headlines)
  local owner_line = focus.owning_headline_line()
  if not owner_line then return nil end
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local owner_id = nil
  local others = {}
  for _, headline in ipairs(old_headlines) do
    if headline.index == owner_line then owner_id = headline.id
    else table.insert(others, headline) end end
  table.sort(others, function (a, b)
    return math.abs(a.index - cursor_line)
           < math.abs(b.index - cursor_line) end)
  local position = M.current_save_point_position()
  position.owner_viewnode_id = owner_id
  position.viewnode_ids = {}
  if owner_id then table.insert(position.viewnode_ids, owner_id) end
  for _, other in ipairs(others) do
    table.insert(position.viewnode_ids, other.id) end
  return position
end

---Move the current window's cursor to POSITION
---('M.viewnode_point_position'): to its offsets below its own headline
---if HEADLINE_LINES (a table from viewnode ID to line) holds that,
---else to the start of the first of its other headlines that it
---holds, keeping its screen line either way. A position whose
---headline had no viewnode ID, or none of whose headlines remain, goes
---to FOCUSED_LINE.
---@param position table|nil
---@param headline_lines table
---@param focused_line integer|nil
function M.goto_viewnode_point_position (position, headline_lines,
                                         focused_line)
  local owner_id = position and position.owner_viewnode_id
  if owner_id then
    for _, id in ipairs(position.viewnode_ids) do
      local line = headline_lines[id]
      if line then
        vim.api.nvim_win_set_cursor(0, { line, 0 })
        if id == owner_id then
          M.restore_save_point_position(position)
        else
          M.restore_save_point_position({
            lines_below_focused_headline = 0,
            column = 0,
            screen_lines_below_window_start =
              position.screen_lines_below_window_start }) end
        return end end end
  if focused_line then
    vim.api.nvim_win_set_cursor(0, { focused_line, 0 }) end
end

---Point position data that survives the save's rerender: the line offset
---below the focused headline (stable because that headline rides the
---wire as metadata), the byte column within the line, and the screen
---line below the window top.
---@return table
function M.current_save_point_position ()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local headline_line = focus.owning_headline_line() or cursor[1]
  return {
    lines_below_focused_headline = cursor[1] - headline_line,
    column = cursor[2],
    screen_lines_below_window_start = math.max(0, vim.fn.winline() - 1),
  }
end

---Replace BUF's contents with NEW_CONTENT from the server, then act
---on the atoms it carries ('buffer.act_on_fold_and_focus_atoms'),
---restore point/scroll, and clear the modified flag.
---@param buf integer
---@param new_content string
---@param save_point_position table|nil
function M.replace_buffer_with_new_content (buf, new_content,
                                            save_point_position)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(new_content, '\n'))
  buffer.act_on_fold_and_focus_atoms(buf, function ()
    M.restore_save_point_position(save_point_position) end)
  vim.bo[buf].modified = false
  buffer.capture_clean_baseline(buf)
  require('skg.link_annotations').refresh(buf)
  vim.notify('Buffer updated with processed content from Rust')
end

---Restore point and window row from SAVE_POINT_POSITION, if given.
---Assumes the cursor starts on the focused headline.
---@param save_point_position table|nil
function M.restore_save_point_position (save_point_position)
  if not save_point_position then return end
  local line_offset = save_point_position.lines_below_focused_headline
  if line_offset then
    M.restore_point_below_focused_headline(
      line_offset, save_point_position.column)
  end
  local screen_lines =
    save_point_position.screen_lines_below_window_start
  if screen_lines then
    M.put_cursor_on_screen_line(screen_lines) end
end

---Scroll the current window so that the cursor's line is SCREEN_LINES
---screen lines below its top (0 being the top), or as near as the
---lines above it allow. The top line climbs from the cursor's line a
---line at a time, a closed fold counting as one, until 'winline()'
---reaches SCREEN_LINES, so closed folds and wrapped lines above the
---cursor are counted as the measure ('winline()') counted them.
---@param screen_lines integer
function M.put_cursor_on_screen_line (screen_lines)
  local topline = vim.api.nvim_win_get_cursor(0)[1]
  local fold_start = vim.fn.foldclosed(topline)
  if fold_start ~= -1 then topline = fold_start end
  vim.fn.winrestview({ topline = topline })
  while topline > 1 and vim.fn.winline() - 1 < screen_lines do
    local above = topline - 1
    fold_start = vim.fn.foldclosed(above)
    if fold_start ~= -1 then above = fold_start end
    vim.fn.winrestview({ topline = above })
    if vim.fn.winline() - 1 > screen_lines then
      vim.fn.winrestview({ topline = topline })
      return end
    topline = above end
end

---Move LINE_OFFSET lines below the focused headline, then to COLUMN
---(byte offset; nil means 0). The target line is clamped to the
---focused headline's own entry, and the column to the line's end.
---@param line_offset integer
---@param column integer|nil
function M.restore_point_below_focused_headline (line_offset, column)
  local headline_line = vim.api.nvim_win_get_cursor(0)[1]
  local entry_end -- first line past the focused headline's entry
  local next_headline = metadata.next_headline_line(headline_line)
  if next_headline then
    entry_end = next_headline
  else
    entry_end = vim.api.nvim_buf_line_count(0) + 1 end
  local target = headline_line + line_offset
  if target >= entry_end then target = entry_end - 1 end
  if target < 1 then target = 1 end
  local line_length = #metadata.line_text(target)
  local target_column = math.min(column or 0, line_length)
  vim.api.nvim_win_set_cursor(0, { target, target_column })
end

return M
