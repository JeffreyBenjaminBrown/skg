-- PURPOSE: The save pipeline: ship the buffer to the server, drive
-- the four-handler response stream (save-lock -> save-relax-lock ->
-- collateral-view* -> save-result | fork-confirmation), restore
-- point/scroll/folds after the rerender, and run the fork-confirmation
-- flow. The Lua port of elisp/skg-request-save.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local focus = require('skg.focus')
local folds = require('skg.folds')
local length_prefix = require('skg.length_prefix')
local lock = require('skg.lock')
local log = require('skg.log')
local messages = require('skg.messages')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local processed = require('skg.processed')
local redraw_delta = require('skg.redraw_delta')
local redraw_whole_view = require('skg.redraw_whole_view')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Send the current buffer to the server. Before sending, 'folded'
---and 'focused' atoms are added (for the wire) and then removed
---from what the user sees. If the save edited any foreign node the
---server replies fork-confirmation instead of save-result, skgsave-committing
---nothing, unless FORK_APPROVED rides along; FORK_REPOS then pairs
---each forked node's id with the owned skgrepo chosen for its clone.
---@param approved_forks boolean|nil
---@param fork_repos table[]|nil {{id, repo}, ...}
---@param approved_hoist_pids string[]|nil
---@param strandeds_review table|nil the relationships, each
---  {relation, recorder, member}, the user chose to reset to default after a
---  strandeds-confirmation (possibly none); nil when not reviewed
---@param delete_from string|nil 'visible' or 'all': where the save's
---  deletes may strip the deleted IDs, as the user chose after a
---  delete-from-confirmation; it rides this save and its retries only,
---  never cached
function M.request_save_buffer (approved_forks, fork_skgrepos,
                                approved_hoist_pids,
                                strandeds_review,
                                delete_from)
  local save_buf = vim.api.nvim_get_current_buf()
  local saved_view_id = vim.b[save_buf].skg_view_id
  if not saved_view_id then
    -- A nil view-id causes an unfiltered save and the server won't
    -- update its in-Rust graph: slow and wasted. Refuse.
    error(string.format(
      "Cannot save: view ID is nil in buffer '%s'. Re-open the view.",
      vim.api.nvim_buf_get_name(save_buf))) end
  lock.begin_stream('save')
  lock.lock_all_skg_buffers()
  local ok, err = pcall(
    M.send_save_buffer, save_buf, saved_view_id, approved_forks, fork_skgrepos,
    approved_hoist_pids, strandeds_review, delete_from)
  if not ok then
    M.cancel_locally_failed_save()
    error(err) end
end

---Serialize and send a save once 'lock.stream_in_progress' is set and
---the buffers are locked.
function M.send_save_buffer (save_buf, saved_view_id, approved_forks, fork_skgrepos,
                             approved_hoist_pids,
                             strandeds_review, delete_from)
  local focused_line = focus.owning_headline_line()
  local focused_had_metadata = focused_line ~= nil
    and metadata.line_text(focused_line):match('^%*+ %(skg') ~= nil
  local save_point_position = redraw_whole_view.current_save_point_position()
  local buffer_contents = M.buffer_snapshot_with_save_metadata(
    save_buf, focused_had_metadata)
  local wire_content = M.serialize_save_envelope(
    buffer_contents, M.saved_buffer_baseline(save_buf),
    M.other_view_buffer_snapshots(save_buf))
  local request_line =
    M.save_request_sexp(saved_view_id, save_point_position,
                          approved_forks, fork_skgrepos,
                          approved_hoist_pids,
                          strandeds_review,
                          delete_from,
                          vim.b[save_buf].skg_diffView_buffer_kind,
                          vim.b[save_buf].skg_perspective)
  state.register_response_handler('save-lock',
    function (_payload_text, response)
      M.broad_save_lock_handler(response)
    end, true)
  -- save-relax-lock carries collateral targets plus every dirty input checked
  -- during preparation. Registered non-one-shot so it does not add to
  -- the pending count: an invalid save errors before the server
  -- reaches the point that emits it, and a one-shot count would leak.
  -- save-result removes it.
  state.register_response_handler('save-relax-lock',
    function (_payload_text, response)
      M.save_relax_lock_handler(saved_view_id, response)
    end, false)
  state.register_response_handler('collateral-view',
    M.collateral_view_handler, false)
  state.register_response_handler('save-result',
    function (_payload_text, response)
      M.save_result_handler(save_buf, response)
    end, true)
  -- fork-confirmation: the alternative terminal message. The server
  -- sends exactly one of the two; whichever fires removes the other
  -- (the fork handler also decrements the count of the never-fired
  -- save-result one-shot), so the pending count balances either way.
  state.register_response_handler('fork-confirmation',
    function (_payload_text, response)
      require('skg.save.confirmations').fork_confirmation_handler(save_buf, response, strandeds_review,
                                  delete_from)
    end, false)
  -- Another alternative terminal: the save's deletes would strip
  -- records the skgrepo restriction keeps out. Ask where to delete
  -- from, and retry this same save with the choice.
  state.register_response_handler('delete-from-confirmation',
    function (_payload_text, response)
      require('skg.save.confirmations').delete_from_confirmation_handler(
        save_buf, response, approved_forks, fork_skgrepos,
        approved_hoist_pids, strandeds_review)
    end, false)
  -- Another alternative terminal: a home skgrepo move would strand
  -- relationships. Retry this same save with the user's review.
  state.register_response_handler('strandeds-confirmation',
    function (_payload_text, response)
      require('skg.save.confirmations').strandeds_confirmation_handler(
        save_buf, response, approved_forks, fork_skgrepos,
        approved_hoist_pids, delete_from)
    end, false)
  state.register_response_handler('telescope-hoist-confirmation',
    function (_payload_text, response)
      require('skg.save.confirmations').telescope_hoist_confirmation_handler(
        save_buf, response, approved_forks, fork_skgrepos,
        strandeds_review, delete_from)
    end, false)
  -- The terminal message of a save whose rerender is withheld. The
  -- save was written; approval asks for a rerender of the views it
  -- names, not for the save again.
  state.register_response_handler('overPrivateText-telescope-confirmation',
    function (_payload_text, response)
      require('skg.save.confirmations').save_text_release_confirmation_handler(response)
    end, false)
  state.lp_reset()
  client.send_string(request_line)
  client.send_string(string.format('Content-Length: %d\r\n\r\n',
                                   #wire_content))
  client.send_string(wire_content)
end

---Return the current text with transient save atoms, restoring the buffer.
---The save lock is synchronously suspended only for the client's internal
---atom edits and restored before any request is sent. A clean buffer
---stays clean, so the atom edits never look like a user's first edit.
---@param save_buf integer
---@param focused_had_metadata boolean
---@return string
function M.buffer_snapshot_with_save_metadata (save_buf, focused_had_metadata)
  local was_save_locked = vim.b[save_buf].skg_save_locked == true
  local was_modified = vim.bo[save_buf].modified
  if was_save_locked then vim.bo[save_buf].modifiable = true end
  local ok, result = pcall(function ()
    folds.add_folded_metadata()
    focus.add_focused_metadata()
    return buffer.text(save_buf)
  end)
  -- Cleanup is best-effort even when atom insertion or capture failed.
  pcall(focus.remove_focused_metadata)
  pcall(folds.remove_folded_metadata)
  if not focused_had_metadata then
    pcall(M.strip_bare_skg_at_focused_headline,
          focus.owning_headline_line()) end
  if was_save_locked then vim.bo[save_buf].modifiable = false end
  if not was_modified then vim.bo[save_buf].modified = false end
  if not ok then error(result) end
  return result
end

---The text of a save's envelope: the saved buffer's text, its baseline,
---and the other views' snapshots ('other_view_buffer_snapshots').
---@param buffer_contents string
---@param baseline string
---@param other_view_buffer_snapshots table
---@return string
function M.serialize_save_envelope (buffer_contents, baseline,
                                    other_view_buffer_snapshots)
  return sexpr.to_string({
    { sexpr.symbol('saved-buffer'), buffer_contents },
    { sexpr.symbol('baseline'), baseline },
    { sexpr.symbol('other-views'), other_view_buffer_snapshots },
  })
end

---SAVE_BUF's baseline, 'vim.b.skg_clean_baseline', which every save
---carries. Every view a client can save has one, so its absence is an
---error, not something to send.
---@param save_buf integer
---@return string
function M.saved_buffer_baseline (save_buf)
  local baseline = vim.b[save_buf].skg_clean_baseline
  if type(baseline) ~= 'string' then
    error('Cannot save: this view has no baseline (the text the server'
          .. ' last drew). Reopen it.') end
  return baseline
end

---@param save_buf integer
---@return table[]
function M.other_view_buffer_snapshots (save_buf)
  local result = {}
  for _, other in ipairs(vim.api.nvim_list_bufs()) do
    local view_id = vim.api.nvim_buf_is_valid(other)
                and vim.b[other].skg_view_id or nil
    if other ~= save_buf and view_id then
      local entry = {
        { sexpr.symbol('view-id'), view_id },
        { sexpr.symbol('dirty'),
          sexpr.symbol(vim.bo[other].modified and 'true' or 'false') },
      }
      if vim.bo[other].modified then
        local baseline = vim.b[other].skg_clean_baseline
        table.insert(entry, {
          sexpr.symbol('baseline'),
          type(baseline) == 'string'
            and { sexpr.symbol('present'), baseline }
            or sexpr.symbol('unavailable'),
        })
        table.insert(entry, {
          sexpr.symbol('current'), buffer.text(other),
        })
      end
      table.insert(result, entry)
    end
  end
  return result
end

---Undo a save whose request never reached the server: remove its
---handlers, balancing the pending count for the one-shots, and unlock.
function M.cancel_locally_failed_save ()
  M.end_save_stream({ 'save-lock', 'save-result' })
end

---The save's handlers that are not one-shot: the streaming ones and the
---alternatives to save-result. Whichever terminal message arrives
---removes them all.
M.save_handler_types_not_one_shot = {
  'save-relax-lock', 'collateral-view', 'fork-confirmation',
  'strandeds-confirmation', 'delete-from-confirmation',
  'telescope-hoist-confirmation', 'overPrivateText-telescope-confirmation' }

---End the save stream: remove the save's handlers that are not
---one-shot, and those of UNFIRED_ONE_SHOT_TYPES with the pending count
---balanced for each one still registered; then end the stream and
---unlock every buffer. The save-result handler passes no one-shot
---types, since the dispatcher removes save-result itself after it
---runs; an alternative terminal message passes 'save-result', which
---will never fire.
---@param unfired_one_shot_types string[]
function M.end_save_stream (unfired_one_shot_types)
  state.unregister_response_handlers(M.save_handler_types_not_one_shot)
  state.unregister_response_handlers(unfired_one_shot_types)
  lock.end_stream()
  lock.unlock_all_save_locked()
end

---The save-buffer request line.
---@param view_id string
---@param position table
---@param approved_forks boolean|nil
---@param fork_repos table[]|nil
---@param approved_hoist_pids string[]|nil
---@param strandeds_review table|nil
---@param delete_from string|nil 'visible' or 'all'
---@param diffView_buffer_kind string|nil which buffer of the diff view
---  the saved buffer is ('vim.b.skg_diffView_buffer_kind', "all
---  changes" say), sent as (diff-view-buffer . NAME); nil for any
---  other view
---@param perspective string|nil the saved view's perspective
---  ('vim.b.skg_perspective'):
---  "skgrepo NAME", sent as (perspective . NAME);
---  nil, the root homes, sends nothing
---@return string
function M.save_request_sexp (view_id, position, approved_forks,
                                fork_skgrepos, approved_hoist_pids,
                                strandeds_review,
                                delete_from,
                                diffView_buffer_kind,
                                perspective)
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'save buffer'),
    sexpr.pair(sexpr.symbol('view-id'), view_id) }
  if diffView_buffer_kind then
    table.insert(request,
      sexpr.pair(sexpr.symbol('diff-view-buffer'), diffView_buffer_kind)) end
  if perspective then
    table.insert(request,
      sexpr.pair(sexpr.symbol('perspective'), perspective)) end
  table.insert(request,
    sexpr.pair(sexpr.symbol('point-lines-below-focused-headline'),
               tostring(position.lines_below_focused_headline)))
  table.insert(request,
    sexpr.pair(sexpr.symbol('point-column'), tostring(position.column)))
  table.insert(request,
    sexpr.pair(sexpr.symbol('point-screen-lines-below-window-start'),
               tostring(position.screen_lines_below_window_start)))
  if approved_forks then
    table.insert(request,
      sexpr.pair(sexpr.symbol('approved-forks'), 'true')) end
  if fork_skgrepos then
    local pairs_sexp = {}
    for _, pair in ipairs(fork_skgrepos) do
      table.insert(pairs_sexp, sexpr.pair(pair[1], pair[2])) end
    table.insert(request,
      { sexpr.symbol('fork-repos'), pairs_sexp }) end
  if approved_hoist_pids then
    local field = { sexpr.symbol('approved-hoist-pids') }
    for _, pid in ipairs(approved_hoist_pids) do
      table.insert(field, pid) end
    table.insert(request, field) end
  if strandeds_review then
    table.insert(request,
      sexpr.pair(sexpr.symbol('strandeds-reviewed'), 'true'))
    if #strandeds_review > 0 then
      local field = { sexpr.symbol('reset-strandeds') }
      for _, triple in ipairs(strandeds_review) do
        table.insert(field, { triple[1], triple[2], triple[3] }) end
      table.insert(request, field) end end
  if delete_from then
    table.insert(request,
      sexpr.pair(sexpr.symbol('delete-from'), delete_from)) end
  return sexpr.to_string(request) .. '\n'
end

---Remove a bare '(skg)' left on LINE_NUMBER by the atom add/remove
---cycle, when the headline had no metadata to begin with.
---@param line_number integer|nil
function M.strip_bare_skg_at_focused_headline (line_number)
  if not line_number then return end
  local line = metadata.line_text(line_number)
  local stars, rest = line:match('^(%*+ )%(skg%) (.*)$')
  if stars then
    metadata.replace_line(line_number, stars .. rest)
  end
end

---Validate the broad save-lock acknowledgement without narrowing locks.
---@param response any
function M.broad_save_lock_handler (response)
  local ok, err = pcall(function ()
    local lock_views = payload.field(response, 'lock-views')
    if lock_views == nil then error('missing lock-views') end
    if not sexpr.is_list(lock_views) then
      error('lock-views is not a list') end
    payload.string_list(lock_views)
  end)
  if not ok then
    -- A malformed acknowledgement cannot safely authorize any unlock.
    log.log('error', 'save', 'broad save-lock handler error: %s',
            tostring(err))
    length_prefix.tell_answer_not_applied(
      'save-lock', err, 'views stay locked until the save ends')
  end
end

---Narrow broad locks to SAVED_VIEW_ID and the view IDs in RESPONSE.
---@param saved_view_id string
---@param response any
function M.save_relax_lock_handler (saved_view_id, response)
  local ok, err = pcall(function ()
    local lock_views = payload.field(response, 'lock-views')
    if lock_views == nil then error('missing lock-views') end
    if not sexpr.is_list(lock_views) then
      error('lock-views is not a list') end
    lock.unlock_non_collateral_buffers(
      saved_view_id, payload.string_list(lock_views))
  end)
  if not ok then
    -- Retain every lock when the keep-set cannot be trusted.
    log.log('error', 'save', 'save-relax-lock handler error: %s',
            tostring(err))
    length_prefix.tell_answer_not_applied(
      'save-relax-lock', err, 'views stay locked until the save ends')
  end
end

---Handle one streamed collateral-view update: unlock and replace the
---buffer for its view ID, unless that buffer has unsaved edits. The
---server never redraws a dirty view; this guard keeps a client from
---losing those edits if it did.
---@param payload_text string
---@param response any
function M.collateral_view_handler (payload_text, response)
  redraw_whole_view.apply_streamed_view_update(
    payload_text, response, 'save', 'collateral-view', true)
end

---Handle the terminal save-result: remove the streaming handlers and
---the unfired fork-confirmation alternative, end the stream, unlock
---everything, and apply the saved buffer's final content.
---@param save_buf integer
---@param response any
function M.save_result_handler (save_buf, response)
  M.end_save_stream({})
  local ok, err = pcall(M.handle_save_sexp, save_buf, response)
  lock.unlock_all_save_locked()
  if not ok then
    log.log('error', 'save', 'parsing save response: %s',
            tostring(err))
    length_prefix.tell_answer_not_applied('save-result', err)
  end
end

---@param save_buf integer
---@param response any
function M.handle_save_sexp (save_buf, response)
  local content = payload.field(response, 'content')
  local content_text =
    (content ~= nil and not sexpr.is_list(content))
    and sexpr.atom_text(content) or nil
  local errors = payload.string_list(payload.field(response, 'errors'))
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  if content_text then
    M.redraw_saved_buffer(
      save_buf, content_text,
      payload.field(response, 'changed-headlines'),
      M.save_point_position_from_response(response))
  end
  if #errors > 0 or #warnings > 0 then
    M.show_save_errors_and_warnings(errors, warnings,
                                    content_text ~= nil)
  end
  local summary = processed.processed_summary_from_response(response)
  if summary then -- the saved buffer is one of the diff view's
    vim.notify(processed.processed_summary_message(summary)) end
  -- What the save hid and stopped hiding, and each removal from a
  -- forker that hid nothing, as the server words them
  -- ('docs/api-and-formats.org', "Save buffer"), one line each.
  local hides = payload.string_list(payload.field(response, 'hides'))
  if #hides > 0 then vim.notify(table.concat(hides, '\n')) end
end

---Show NEW_CONTENT in BUF. When CHANGED_HEADLINES, the value of the
---response's '(changed-headlines (V ...))', is present (an empty list
---included), rewrite only the headlines it names
---('redraw_delta.patch_buffer_with_delta'), leaving folds and the
---cursor as they are; otherwise, or when the patch fails, redraw the
---whole buffer, restoring folds and the cursor from the atoms and
---SAVE_POINT_POSITION.
---@param buf integer
---@param new_content string
---@param changed_headlines any|nil
---@param save_point_position table|nil
function M.redraw_saved_buffer (buf, new_content, changed_headlines,
                                save_point_position)
  local viewnode_ids = changed_headlines ~= nil
                       and payload.string_list(changed_headlines) or nil
  if viewnode_ids
     and redraw_delta.patch_buffer_with_delta(buf, new_content,
                                              viewnode_ids) then
    log.log('debug', 'save', 'redraw: patched %d headlines', #viewnode_ids)
    vim.bo[buf].modified = false
    buffer.capture_clean_baseline(buf)
    require('skg.link_annotations').refresh(buf)
    vim.notify('Buffer updated with processed content from Rust')
  else
    log.log('debug', 'save', 'redraw: whole buffer (%s)',
            viewnode_ids and 'the patch failed' or 'no delta')
    redraw_whole_view.replace_buffer_with_new_content(
      buf, new_content, save_point_position)
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

return M
