-- PURPOSE: Ask the server for a single-root content view of a node
-- and open the reply as a view buffer. The workhorse behind every
-- goto command. The Lua port of
-- elisp/skg-request-single-root-content-view.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local log = require('skg.log')
local messages = require('skg.messages')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---The request sexp string for a single-root content view of NODE_ID.
---When BYPASS_OVERRIDE, the request carries (override-choice .
---"bypass").
---@param node_id string
---@param view_uri string
---@param bypass_override boolean|nil
---@return string
function M.request_string (node_id, view_uri, bypass_override)
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'single root content view'),
    sexpr.pair(sexpr.symbol('id'), node_id),
    sexpr.pair(sexpr.symbol('view-uri'), view_uri) }
  if bypass_override then
    table.insert(request,
      sexpr.pair(sexpr.symbol('override-choice'), 'bypass')) end
  return sexpr.to_string(request) .. '\n'
end

---Ask the server for a single-root content view of NODE_ID.
---When BYPASS_OVERRIDE, an overridden node opens itself instead of
---the override-choice menu (recursive content beneath the root still
---substitutes).
---@param node_id string
---@param bypass_override boolean|nil
function M.request_single_root_content_view_from_id (node_id,
                                                     bypass_override)
  local view_uri = buffer.generate_uuid()
  state.register_response_handler('content-view',
    function (payload_text, response)
      M.handle_content_view(payload_text, response, view_uri)
    end, true)
  state.lp_reset()
  client.send_string(
    M.request_string(node_id, view_uri, bypass_override))
end

---Handle a content-view response: either a (switch-to-view URI)
---redirect to an already-open buffer, or content plus errors and
---warnings. VIEW_URI is the client-generated uuid; the server may
---override it (it does for override-choice menus).
---@param payload_text string
---@param response any
---@param view_uri string
function M.handle_content_view (payload_text, response, view_uri)
  local ok, err = pcall(function ()
    local switch_uri = payload.field_text(response, 'switch-to-view')
    if switch_uri then
      -- The requested id is already a root of an open view.
      local buf = buffer.find_buffer_by_uri(switch_uri)
      if not buf then
        log.log('warn', 'view',
                'server said switch to view %s, but no buffer found',
                switch_uri)
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
    local to_minibuffer = payload.field_text(response, 'to-minibuffer')
    local server_uri = payload.field_text(response, 'view-uri')
    local effective_uri = server_uri or view_uri
    if content_text and content_text ~= '' then
      buffer.open_org_buffer_from_text(
        content_text,
        buffer.content_view_buffer_name(content_text),
        effective_uri) end
    if to_minibuffer then
      -- Echo area only -- never buffer text, never a popped window.
      vim.notify(to_minibuffer) end
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
