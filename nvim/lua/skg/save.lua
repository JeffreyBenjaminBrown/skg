-- PURPOSE: The save pipeline: ship the buffer to the server, drive
-- the four-handler response stream (save-lock -> save-relax-lock ->
-- collateral-view* -> save-result | fork-confirmation), restore
-- point/scroll/folds after the redraw, and run the fork-confirmation
-- flow. The Lua port of elisp/skg-request-save.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local focus = require('skg.focus')
local folds = require('skg.folds')
local lock = require('skg.lock')
local log = require('skg.log')
local messages = require('skg.messages')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Sentinel source the server pre-fills for each clone-to-be in the
---fork-confirmation buffer. The user must replace it (<localleader>ss
---on the clone-to-be headline) before approving. Must match
---FORK_SOURCE_PLACEHOLDER in server/from_text/fork.rs.
M.fork_source_placeholder = 'PICK-A-SOURCE'

---@return integer[] skg view buffers other than BUF with unsaved edits
function M.other_unsaved_skg_buffers (buf)
  local result = {}
  for _, other in ipairs(vim.api.nvim_list_bufs()) do
    if other ~= buf and vim.api.nvim_buf_is_valid(other)
       and vim.b[other].skg_view_uri ~= nil
       and vim.bo[other].modified then
      table.insert(result, other) end
  end
  return result
end

---If other skg buffers have unsaved edits this save's collateral
---rerenders might overwrite, warn loudly and ask before sending;
---errors (aborting the save) if the user declines. Skipped when no UI
---is attached (the analog of elisp's noninteractive), where there is
---no user to ask.
---@param buf integer
function M.confirm_save_despite_other_unsaved (buf)
  if #vim.api.nvim_list_uis() == 0 then return end
  local others = M.other_unsaved_skg_buffers(buf)
  if #others == 0 then return end
  local names = {}
  for _, other in ipairs(others) do
    table.insert(names, vim.api.nvim_buf_get_name(other)) end
  local choice = vim.fn.confirm(
    string.format(
      'DANGER: %d other skg buffer(s) have unsaved edits (%s) this'
      .. ' save may overwrite. Save anyway?',
      #others, table.concat(names, ', ')),
    '&Yes\n&No', 2)
  if choice ~= 1 then
    error('Save aborted: other skg buffers have unsaved edits') end
end

---Send the current buffer to the server. Before sending, 'folded'
---and 'focused' markers are added (for the wire) and then removed
---from what the user sees. If the save edited any FOREIGN node the
---server replies fork-confirmation instead of save-result, committing
---nothing, unless FORK_APPROVED rides along; FORK_SOURCES then pairs
---each forked node's id with the owned source chosen for its clone.
---@param fork_approved boolean|nil
---@param fork_sources table[]|nil {{id, source}, ...}
function M.request_save_buffer (fork_approved, fork_sources)
  local save_buf = vim.api.nvim_get_current_buf()
  M.confirm_save_despite_other_unsaved(save_buf)
  local saved_uri = vim.b[save_buf].skg_view_uri
  if not saved_uri then
    -- A nil view-uri causes an unfiltered save AND the server won't
    -- update its in-Rust graph: slow and wasted. Refuse.
    error(string.format(
      "Cannot save: view uri is nil in buffer '%s'. Re-open the view.",
      vim.api.nvim_buf_get_name(save_buf))) end
  local focused_line = focus.owning_headline_line()
  local focused_had_metadata = focused_line ~= nil
    and metadata.line_text(focused_line):match('^%*+ %(skg') ~= nil
  local save_point_position = M.current_save_point_position()
  folds.add_folded_markers()
  focus.add_focused_marker()
  local buffer_contents = table.concat(
    vim.api.nvim_buf_get_lines(save_buf, 0, -1, false), '\n')
  local request_line =
    M.save_request_string(saved_uri, save_point_position,
                          fork_approved, fork_sources)
  do -- The server needs these markers, but the user doesn't.
    focus.remove_focused_marker()
    folds.remove_folded_markers()
    if not focused_had_metadata then
      M.strip_bare_skg_at_headline(focus.owning_headline_line()) end
  end
  lock.begin_stream('save')
  -- Lock ALL skg view buffers before sending, eliminating the race
  -- window between the send and the server's early response.
  lock.lock_all_skg_buffers()
  state.register_response_handler('save-lock',
    function (_payload_text, response)
      M.save_lock_handler(saved_uri, response)
    end, true)
  -- save-relax-lock: same handling, but with the EXACT collateral set
  -- (post-SavePlan). Registered NON-one-shot so it does not add to
  -- the pending count: an invalid save errors before the server
  -- reaches the point that emits it, and a one-shot count would leak.
  -- save-result removes it.
  state.register_response_handler('save-relax-lock',
    function (_payload_text, response)
      M.save_lock_handler(saved_uri, response)
    end, false)
  state.register_response_handler('collateral-view',
    function (payload_text, response)
      M.apply_streamed_view_update(payload_text, response,
                                   'save', 'collateral-view')
    end, false)
  state.register_response_handler('save-result',
    function (_payload_text, response)
      M.save_result_handler(save_buf, response)
    end, true)
  -- fork-confirmation: the ALTERNATIVE terminal message. The server
  -- sends exactly one of the two; whichever fires removes the other
  -- (the fork handler also decrements the count of the never-fired
  -- save-result one-shot), so the pending count balances either way.
  state.register_response_handler('fork-confirmation',
    function (_payload_text, response)
      M.fork_confirmation_handler(save_buf, response)
    end, false)
  state.lp_reset()
  client.send_string(request_line)
  client.send_string(string.format('Content-Length: %d\r\n\r\n',
                                   #buffer_contents))
  client.send_string(buffer_contents)
end

---The save-buffer request line.
---@param view_uri string
---@param position table
---@param fork_approved boolean|nil
---@param fork_sources table[]|nil
---@return string
function M.save_request_string (view_uri, position, fork_approved,
                                fork_sources)
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'save buffer'),
    sexpr.pair(sexpr.symbol('view-uri'), view_uri),
    sexpr.pair(sexpr.symbol('point-lines-below-focused-headline'),
               tostring(position.lines_below_focused_headline)),
    sexpr.pair(sexpr.symbol('point-column'),
               tostring(position.column)),
    sexpr.pair(sexpr.symbol('point-screen-lines-below-window-start'),
               tostring(position.screen_lines_below_window_start)) }
  if fork_approved then
    table.insert(request,
      sexpr.pair(sexpr.symbol('fork-approved'), 'true')) end
  if fork_sources then
    local pairs_sexp = {}
    for _, pair in ipairs(fork_sources) do
      table.insert(pairs_sexp, sexpr.pair(pair[1], pair[2])) end
    table.insert(request,
      { sexpr.symbol('fork-sources'), pairs_sexp }) end
  return sexpr.to_string(request) .. '\n'
end

---Point position data that survives the save redraw: the line offset
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

---Remove a bare '(skg)' left on LINE_NUMBER by the marker add/remove
---cycle, when the headline had no metadata to begin with.
---@param line_number integer|nil
function M.strip_bare_skg_at_headline (line_number)
  if not line_number then return end
  local line = metadata.line_text(line_number)
  local stars, rest = line:match('^(%*+ )%(skg%) (.*)$')
  if stars then
    metadata.replace_line(line_number, stars .. rest)
  end
end

---Handle save-lock / save-relax-lock: unlock buffers not in the
---collateral set (the saved buffer stays locked until save-result).
---@param saved_uri string
---@param response any
function M.save_lock_handler (saved_uri, response)
  local ok, err = pcall(function ()
    local lock_views = payload.field(response, 'lock-views')
    if lock_views ~= nil then
      lock.unlock_non_collateral_buffers(
        saved_uri, payload.string_list(lock_views)) end
  end)
  if not ok then
    -- Keep the saved buffer locked until save-result; free the rest.
    lock.unlock_non_collateral_buffers(saved_uri, nil)
    log.log('error', 'save', 'save-lock handler error: %s',
            tostring(err))
  end
end

---Apply one streamed view update: unlock and replace the buffer for
---its view uri. Shared by the save (collateral-view) and rerender
---(rerender-view) streams.
---@param _payload_text string
---@param response any
---@param log_category string
---@param handler_name string
function M.apply_streamed_view_update (_payload_text, response,
                                       log_category, handler_name)
  local ok, err = pcall(function ()
    local uri = payload.field_text(response, 'view-uri')
    local content = payload.field(response, 'content')
    local buf = uri and buffer.find_buffer_by_uri(uri) or nil
    if buf and content ~= nil and not sexpr.is_list(content) then
      lock.unlock_after_save(buf)
      M.replace_buffer_with_new_content(
        buf, sexpr.atom_text(content), nil)
    end
  end)
  if not ok then
    log.log('error', log_category, '%s handler error: %s',
            handler_name, tostring(err))
  end
end

---Handle the terminal save-result: remove the streaming handlers and
---the unfired fork-confirmation alternative, end the stream, unlock
---everything, and apply the saved buffer's final content.
---@param save_buf integer
---@param response any
function M.save_result_handler (save_buf, response)
  state.response_handler_map['collateral-view'] = nil
  state.response_handler_map['save-relax-lock'] = nil
  state.response_handler_map['fork-confirmation'] = nil
  lock.end_stream()
  lock.unlock_all_save_locked()
  local ok, err = pcall(M.handle_save_response, save_buf, response)
  lock.unlock_all_save_locked()
  if not ok then
    log.log('error', 'save', 'parsing save response: %s',
            tostring(err))
  end
end

---@param save_buf integer
---@param response any
function M.handle_save_response (save_buf, response)
  local content = payload.field(response, 'content')
  local content_text =
    (content ~= nil and not sexpr.is_list(content))
    and sexpr.atom_text(content) or nil
  local errors = payload.string_list(payload.field(response, 'errors'))
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  if content_text then
    M.replace_buffer_with_new_content(
      save_buf, content_text,
      M.save_point_position_from_response(response))
  end
  if #errors > 0 or #warnings > 0 then
    M.show_save_errors_and_warnings(errors, warnings,
                                    content_text ~= nil)
  end
end

---@param response any
---@return table|nil
function M.save_point_position_from_response (response)
  local function nat (key)
    local value = payload.field_text(response, key)
    return value and tonumber(value) or nil
  end
  return {
    lines_below_focused_headline =
      nat('point-lines-below-focused-headline'),
    column = nat('point-column'),
    screen_lines_below_window_start =
      nat('point-screen-lines-below-window-start'),
  }
end

---Replace BUF's contents with NEW_CONTENT from the server, then act
---on the markers it carries: move to the focused headline and drop
---its marker, restore folds and drop their markers, restore
---point/scroll, clear the modified flag, and re-arm the first-change
---warning. Fold restoration is per-window (vim folds live on
---windows): it runs in the first window showing BUF; an undisplayed
---buffer still gets its markers stripped, it just has no fold state
---to restore into.
---@param buf integer
---@param new_content string
---@param save_point_position table|nil
function M.replace_buffer_with_new_content (buf, new_content,
                                            save_point_position)
  buffer.disarm_first_change_warning(buf)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(new_content, '\n'))
  local window = nil
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_buf(win) == buf then
      window = win break end
  end
  local function act_on_markers ()
    folds.show_all()
    -- Focus first, while everything is visible.
    focus.goto_focused_headline()
    focus.remove_focused_marker()
    local cursor = vim.api.nvim_win_get_cursor(0)
    folds.fold_marked_headlines()
    folds.remove_folded_markers()
    vim.api.nvim_win_set_cursor(0, cursor)
    M.restore_save_point_position(save_point_position)
  end
  if window then
    vim.api.nvim_win_call(window, act_on_markers)
  else
    vim.api.nvim_buf_call(buf, act_on_markers)
  end
  vim.bo[buf].modified = false
  buffer.arm_first_change_warning(buf)
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
    -- Put the cursor's line SCREEN_LINES below the window top
    -- (modulo wrapped lines, which winrestview counts as one).
    local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
    vim.fn.winrestview({
      topline = math.max(1, cursor_line - screen_lines) })
  end
end

---Move LINE_OFFSET lines below the focused headline, then to COLUMN
---(byte offset; nil means 0). The target line is clamped to the
---focused headline's own entry, and the column to the line's end.
---@param line_offset integer
---@param column integer|nil
function M.restore_point_below_focused_headline (line_offset, column)
  local headline_line = vim.api.nvim_win_get_cursor(0)[1]
  local entry_end -- first line past the focused headline's entry
  local next_heading = metadata.next_heading_line(headline_line)
  if next_heading then
    entry_end = next_heading
  else
    entry_end = vim.api.nvim_buf_line_count(0) + 1 end
  local target = headline_line + line_offset
  if target >= entry_end then target = entry_end - 1 end
  if target < 1 then target = 1 end
  local line_length = #metadata.line_text(target)
  local target_column = math.min(column or 0, line_length)
  vim.api.nvim_win_set_cursor(0, { target, target_column })
end

---@param errors string[]
---@param warnings string[]
---@param content_present boolean
function M.show_save_errors_and_warnings (errors, warnings,
                                          content_present)
  local has_errors = #errors > 0
  local has_warnings = #warnings > 0
  local buffer_name, message_text
  if has_errors and has_warnings then
    buffer_name = 'skg://messages/save-errors-and-warnings'
    message_text = 'Save reported errors and warnings'
  elseif has_errors then
    buffer_name = 'skg://messages/save-errors'
    message_text = 'Save failed - errors shown in '
                   .. 'skg://messages/save-errors'
  else
    buffer_name = 'skg://messages/save-warnings'
    if content_present then
      message_text = 'Save succeeded with warnings - see '
                     .. 'skg://messages/save-warnings'
    else
      message_text = 'Save produced warnings - see '
                     .. 'skg://messages/save-warnings' end
  end
  messages.big_nonfatal_message(
    buffer_name, message_text,
    messages.errors_and_warnings_to_org_string(errors, warnings))
end

-- ── the fork-confirmation flow ─────────────────────────────────────

---Handle a fork-confirmation message: the save edited foreign
---node(s) and was not pre-approved, so NOTHING was committed. Show
---the confirmation buffer; interactively, ask whether to approve.
---Terminal like save_result_handler: removes the streaming handlers
---AND the unfired save-result one-shot (decrementing its pending
---count), ends the stream, unlocks.
---@param save_buf integer
---@param response any
function M.fork_confirmation_handler (save_buf, response)
  state.response_handler_map['collateral-view'] = nil
  state.response_handler_map['save-relax-lock'] = nil
  state.response_handler_map['fork-confirmation'] = nil
  if state.response_handler_map['save-result'] then
    state.response_handler_map['save-result'] = nil
    state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
  end
  lock.end_stream()
  lock.unlock_all_save_locked()
  local ok, err = pcall(function ()
    local content = payload.field_text(response, 'content')
    local to_minibuffer = payload.field_text(response, 'to-minibuffer')
    local confirm_buf =
      M.show_fork_confirmation(content or '', save_buf)
    if to_minibuffer then vim.notify(to_minibuffer) end
    if #vim.api.nvim_list_uis() > 0 then
      -- In headless runs (tests) the caller drives approve_fork /
      -- decline_fork directly; interactively, ask now.
      vim.api.nvim_buf_call(confirm_buf, function ()
        if vim.fn.confirm('Fork the listed node(s)?',
                          '&Yes\n&No', 2) == 1 then
          M.approve_fork()
        else
          M.decline_fork() end
      end)
    end
  end)
  if not ok then
    log.log('error', 'save', 'fork-confirmation handler error: %s',
            tostring(err))
  end
end

---Show CONTENT in the fork-confirmation buffer, recording SAVE_BUF as
---its origin; returns the buffer. It is a navigable, editable content
---view (so the user can rotate each clone's source), but NOT an
---ordinary save target: it has no view uri (tripping the nil-uri save
---guard) and its ':w' refuses. Only approve (<localleader>cc) and
---decline (<localleader>ck) act on it. Dismissing it without
---approving strips the origin's lingering fork atom, so the next save
---does not silently re-fork.
---@param content string
---@param save_buf integer
---@return integer bufnr
function M.show_fork_confirmation (content, save_buf)
  local name = 'skg://fork-confirmation'
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(existing)
       and vim.api.nvim_buf_get_name(existing) == name then
      buf = existing break end
  end
  if not buf then
    buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, name)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(content, '\n'))
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = 'org'
  vim.b[buf].skg_view_uri = nil -- not a registered view
  vim.b[buf].skg_fork_origin = save_buf
  vim.b[buf].skg_fork_suppress_strip = false
  require('skg.heralds').enable(buf)
  require('skg.keymaps').attach_content_view(buf)
  vim.keymap.set('n', '<localleader>cc', M.approve_fork,
                 { buffer = buf, desc = 'Approve the listed forks' })
  vim.keymap.set('n', '<localleader>ck', M.decline_fork,
                 { buffer = buf, desc = 'Decline the forks' })
  if not vim.b[buf].skg_fork_autocmds then
    vim.b[buf].skg_fork_autocmds = true
    vim.api.nvim_create_autocmd('BufWriteCmd', {
      buffer = buf,
      callback = function ()
        vim.notify('This is the fork-confirmation buffer; approve'
                   .. ' with <localleader>cc or decline with'
                   .. ' <localleader>ck')
      end })
    vim.api.nvim_create_autocmd({ 'BufDelete', 'BufWipeout' }, {
      buffer = buf,
      callback = function ()
        if not vim.b[buf].skg_fork_suppress_strip then
          local origin = vim.b[buf].skg_fork_origin
          if origin and vim.api.nvim_buf_is_valid(origin) then
            vim.api.nvim_buf_call(origin, function ()
              metadata.strip_fork_requests_in_buffer() end)
          end
        end
      end })
  end
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
  return buf
end

---Walk the fork-confirmation buffer for {id, source} pairs: each
---level-1 headline is a clone-to-be carrying (source SOURCE) and no
---id; each of its level-2 children is an original carrying (id N).
---@param buf integer
---@return table[]
function M.fork_sources_from_confirmation_buffer (buf)
  local pairs_found = {}
  vim.api.nvim_buf_call(buf, function ()
    local parent_source = nil
    for line = 1, vim.api.nvim_buf_line_count(buf) do
      local level = metadata.outline_level(line)
      if level then
        local sexp = metadata.metadata_sexp_at_line_or_nil(line)
        if level == 1 then
          -- Always rebind on a level-1 headline -- even a
          -- metadata-less one -- so it cannot leak a prior
          -- clone-to-be's source to a later fork's child.
          parent_source = sexp and metadata.node_source(sexp) or nil
        elseif level == 2 and sexp and parent_source then
          local id = metadata.node_id(sexp)
          if id then
            table.insert(pairs_found, { id, parent_source }) end
        end
      end
    end
  end)
  return pairs_found
end

---Approve the forks listed in the fork-confirmation buffer: extract
---each clone's chosen source, kill the confirmation buffer, and
---re-save the originating buffer with the forks approved. Refuses
---while any clone-to-be still carries the placeholder source.
function M.approve_fork ()
  local buf = vim.api.nvim_get_current_buf()
  local origin = vim.b[buf].skg_fork_origin
  local fork_sources = M.fork_sources_from_confirmation_buffer(buf)
  if not origin or not vim.api.nvim_buf_is_valid(origin) then
    error('The buffer that requested these forks is no longer open')
  end
  for _, pair in ipairs(fork_sources) do
    if pair[2] == M.fork_source_placeholder then
      error('Pick a source for each clone first: point on a'
            .. ' clone-to-be headline, then <localleader>ss') end
  end
  -- The fork atom must survive to the re-save (which commits the
  -- fork; the server drops it on re-render): suppress the kill-hook
  -- strip.
  vim.b[buf].skg_fork_suppress_strip = true
  vim.api.nvim_buf_delete(buf, { force = true })
  vim.api.nvim_set_current_buf(origin)
  M.request_save_buffer(true, fork_sources)
end

---Decline the forks; nothing was written. Strips any lingering
---explicit fork atom from the origin buffer and leaves this
---confirmation buffer open for reference.
function M.decline_fork ()
  local buf = vim.api.nvim_get_current_buf()
  local origin = vim.b[buf].skg_fork_origin
  if origin and vim.api.nvim_buf_is_valid(origin) then
    vim.api.nvim_buf_call(origin, function ()
      metadata.strip_fork_requests_in_buffer() end)
  end
  vim.notify('Fork declined; nothing was saved. This buffer is left'
             .. ' open for reference.')
end

return M
