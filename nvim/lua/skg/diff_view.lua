-- PURPOSE: <localleader>vd (':SkgDiffViewOpen', 'diffView_open') opens
-- the diff view: every change in the owned skgrepos since their last
-- git-commits, in up to four buffers ('docs/diff-view.org', "The four
-- buffers"; 'M.buffers'), each named "skg://diff-view: " followed by
-- its name. It sends one "diff view" request per buffer, in order
-- ('request_diffView_buffers'), and opens each the server does not
-- answer with '(empty . t)', showing its processed summary
-- ('handle_diffView_response').
-- Marking changes processed and moving between them are
-- skg.processed's; extending a buffer to the changes it does not make
-- editable, skg.extend_diff_view's; saving a buffer of the diff view,
-- skg.save's; git-committing, skg.stage_and_commit's.
-- The Lua port of elisp/skg-request-diff-view.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local length_prefix = require('skg.length_prefix')
local log = require('skg.log')
local messages = require('skg.messages')
local payload = require('skg.payload')
local processed = require('skg.processed')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---The buffers of the diff view, in the order they open.
M.buffers = { 'all changes', 'merges', 'skgrepo changes',
              'order-only changes' }

---The request sexp string for the diff view's BUFFER_NAME.
---@param buffer_name string
---@param view_id string
---@return string
function M.request_string (buffer_name, view_id)
  return sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'diff view'),
    sexpr.pair(sexpr.symbol('buffer'), buffer_name),
    sexpr.pair(sexpr.symbol('view-id'), view_id) }) .. '\n'
end

---Open the diff view: every change in the owned skgrepos
---since their last commits, in up to four buffers. Buffers that
---would be empty are not opened.
function M.diffView_open ()
  M.request_diffView_buffers(1, 0)
end

---Request buffer number INDEX of M.buffers; its answer opens it,
---unless empty, and then requests the next. OPENED counts the
---buffers opened so far.
---@param index integer
---@param opened integer
function M.request_diffView_buffers (index, opened)
  local buffer_name = M.buffers[index]
  if not buffer_name then
    if opened == 0 then
      vim.notify('The diff view is empty: nothing changed since the last commits.')
    end
    return end
  local view_id = buffer.generate_uuid()
  state.register_response_handler('diff-view',
    function (_payload_text, response)
      local outcome = M.handle_diffView_response(response, buffer_name, view_id)
      -- A refusal applies to every buffer, so it ends the sequence.
      -- The dispatcher drops this one-shot handler after it returns,
      -- so the next request is sent afterward.
      if outcome ~= 'failed' then
        vim.schedule(function ()
          M.request_diffView_buffers(index + 1,
                            outcome == 'opened' and opened + 1 or opened)
        end) end
    end, true)
  state.lp_reset()
  client.send_string(M.request_string(buffer_name, view_id))
end

---Open the diff view's BUFFER_NAME from RESPONSE under
---VIEW_ID, unless the server answered '(empty . t)'. Shows its
---processed summary, as a save does, and any errors and warnings.
---@return string 'opened', 'empty' or 'failed' (no buffer, and errors)
function M.handle_diffView_response (response, buffer_name, view_id)
  local ok, outcome = pcall(function ()
    local empty = payload.field_text(response, 'empty') == 't'
    local content = payload.field_text(response, 'content')
    local errors = payload.string_list(payload.field(response, 'errors'))
    local warnings =
      payload.string_list(payload.field(response, 'warnings'))
    local opened = not empty and content ~= nil and content ~= ''
    if opened then
      local buf = buffer.open_org_buffer_from_text(
        content, 'skg://diff-view: ' .. buffer_name, view_id)
      -- Its save names it ('save.save_request_sexp').
      vim.b[buf].skg_diffView_buffer_kind = buffer_name
      local summary = processed.processed_summary_from_response(response)
      if summary then
        vim.notify(processed.processed_summary_message(summary)) end end
    if #errors > 0 or #warnings > 0 then
      messages.big_nonfatal_message(
        'skg://messages/diff-view',
        'The diff view\'s "' .. buffer_name
          .. '" buffer reported problems',
        messages.errors_and_warnings_to_org_string(errors, warnings)) end
    if opened then return 'opened'
    elseif empty or #errors == 0 then return 'empty'
    else return 'failed' end
  end)
  if not ok then
    log.log('error', 'view', 'parsing diff view response: %s',
            tostring(outcome))
    length_prefix.tell_answer_not_applied(
      'diff-view', outcome, 'the diff view was not shown')
    return 'failed' end
  return outcome
end

return M
