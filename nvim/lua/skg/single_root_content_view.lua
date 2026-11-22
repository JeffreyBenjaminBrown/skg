-- PURPOSE: Open a view of one node: ask the server for a single-root
-- content view of it ('request_single_root_content_view_from_id') and
-- open the reply as a view buffer ('handle_content_view_sexp'), or,
-- when the node is already a root of an open view, switch to that
-- buffer. Errors and warnings go to a messages buffer; a privacy
-- challenge asks, then sends again with the approval. The workhorse
-- behind ':SkgGoto' ('<localleader>gg') and ':SkgGotoById'
-- ('<localleader>gi'), whose commands, finding the ID, are
-- skg.id_search's; setting up the buffer is skg.buffer's. The Lua
-- port of elisp/skg-request-single-root-content-view.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local length_prefix = require('skg.length_prefix')
local log = require('skg.log')
local messages = require('skg.messages')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---The request sexp string for a single-root content view of NODE_ID.
---@param node_gnid string
---@param view_id string
---@param approved_pids string[]|nil
---@return string
function M.single_root_view_request_string (node_gnid, view_id, approved_pids)
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'single root content view'),
    sexpr.pair(sexpr.symbol('id'), node_gnid),
    sexpr.pair(sexpr.symbol('view-id'), view_id) }
  if approved_pids and #approved_pids > 0 then
    local approval = { sexpr.symbol('approved-overPrivateText-pids') }
    for _, pid in ipairs(approved_pids) do
      table.insert(approval, pid) end
    table.insert(request, approval) end
  return sexpr.to_string(request) .. '\n'
end

---Ask the server for a single-root content view of NODE_GNID.
---The server opens the requested node itself.
---@param node_gnid string
function M.request_single_root_content_view_from_id (node_gnid,
                                                     approved_pids,
                                                     existing_view_id)
  local view_id = existing_view_id or buffer.generate_uuid()
  state.register_response_handler('content-view',
    function (payload_text, response)
      state.unregister_response_handlers(
        { 'overPrivateText-telescope-confirmation' })
      M.handle_content_view_sexp(payload_text, response, view_id)
    end, true)
  -- Alternative to content-view. It is non-one-shot so the pending
  -- response count represents only the one terminal reply.
  length_prefix.register_overPrivateText_confirmation_instead_of(
    'content-view',
    'This view includes text selected below a node home repo. Include it?',
    function (pids)
      M.request_single_root_content_view_from_id(node_gnid, pids, view_id)
    end)
  state.lp_reset()
  client.send_string(
    M.single_root_view_request_string(
      node_gnid, view_id, approved_pids))
end

---Handle a content-view response: either a (switch-to-view ID)
---redirect to an already-open buffer, or content plus errors and
---warnings. VIEW_ID is the client-generated uuid.
---@param payload_text string
---@param response any
---@param view_id string
function M.handle_content_view_sexp (payload_text, response, view_id)
  local ok, err = pcall(function ()
    local switch_view_id = payload.field_text(response, 'switch-to-view')
    if switch_view_id then
      -- The requested id is already a root of an open view.
      local buf = buffer.find_buffer_by_view_id(switch_view_id)
      if not buf then
        log.log('warn', 'view',
                'server said switch to view %s, but no buffer found',
                switch_view_id)
      elseif buf == vim.api.nvim_get_current_buf() then
        vim.notify(
          'Already viewing this node (it is a root of this view)')
      else
        vim.api.nvim_set_current_buf(buf) end
      return end
    local content = payload.field(response, 'content')
    local content_text =
      (content ~= nil and not sexpr.is_list(content))
      and sexpr.atom_text(content) or nil
    local errors = payload.string_list(payload.field(response, 'errors'))
    local warnings =
      payload.string_list(payload.field(response, 'warnings'))
    if content_text and content_text ~= '' then
      buffer.open_org_buffer_from_text(
        content_text,
        buffer.content_view_buffer_name(content_text),
        view_id) end
    local has_errors = #errors > 0
    local has_warnings = #warnings > 0
    if has_errors or has_warnings then
      local headline
      if has_errors and has_warnings then
        headline = 'Content view reported errors and warnings'
      elseif has_errors then
        headline = 'Content view failed'
      else
        headline = 'Content view completed with warnings' end
      messages.big_nonfatal_message(
        'skg://messages/content-view', headline,
        messages.errors_and_warnings_to_org_string(errors, warnings))
    end
  end)
  if not ok then
    vim.notify('skg content view error: ' .. tostring(err))
    log.log('error', 'view', 'parsing content view response: %s',
            tostring(err))
    log.log('error', 'view', 'sexp string was: %s',
            payload_text:sub(1, 200))
  end
end

return M
