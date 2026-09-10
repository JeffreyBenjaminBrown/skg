-- PURPOSE: Global state for the skg client.
-- The Lua port of elisp/skg-state.el. (One difference of homes: the
-- config path lives in skg.config here, not in state; see that file's
-- header.)

local M = {}

if not vim.g.skg_client_session_id then
  vim.g.skg_client_session_id = string.format(
    'nvim-%d-%s', vim.fn.getpid(),
    vim.fn.sha256(tostring(vim.uv.hrtime())):sub(1, 24))
end

M.client_session_id = vim.g.skg_client_session_id
M.connection_handshake_state = nil
M.server_session_id = nil
M.active_source_set_name = vim.g.skg_active_source_set_name
  or 'server-default'
M.maintenance_archive_folder = nil
M.maintenance_archive_identity = nil
M.maintenance_state = nil
M.rebuilding = M.rebuilding or false
M.graph_write_admission = nil
M.client_constructor_admission = 'open'
M.owner_publication_revision = nil
M.graph_transition_status = nil
M.pending_incidents = {}
M.pending_query_waits_raw = nil
M.maintenance_client_incident = M.maintenance_client_incident or nil
M.pending_maintenance_offer = M.pending_maintenance_offer or nil
M.pending_recovery_incidents = M.pending_recovery_incidents or {}

---Update rebuilding only when VALUE is explicit server status metadata.
---An omitted field preserves the last authoritative value.
---@param value string|nil
function M.update_rebuilding_status (value)
  if value == nil then return M.rebuilding end
  if value == 'true' then M.rebuilding = true
  elseif value == 'nil' then M.rebuilding = false
  else error('Invalid rebuilding status ' .. tostring(value)) end
  vim.cmd('redrawstatus')
  return M.rebuilding
end

---Install explicit process-wide graph status from a current-session response.
---Report-local selected-* fields never update the current graph identity.
---@param response any
function M.update_global_server_status (response)
  local payload = require('skg.payload')
  local fields = {
    'current-graph-generation', 'current-manifest-revision',
    'graph-write-admission', 'graph-transition-status', 'rebuilding',
    'pending-incidents', 'pending-query-waits', 'owner-publication-revision',
  }
  local present = false
  for _, key in ipairs(fields) do
    if payload.field(response, key) ~= nil then present = true break end
  end
  if not present then return end
  if payload.field(response, 'current-graph-generation') ~= nil
     or payload.field(response, 'current-manifest-revision') ~= nil
     or payload.field(response, 'graph-write-admission') ~= nil
     or payload.field(response, 'graph-transition-status') ~= nil
     or payload.field(response, 'pending-incidents') ~= nil
     or payload.field(response, 'pending-query-waits') ~= nil
     or payload.field(response, 'owner-publication-revision') ~= nil then
    M.require_current_server_session(response)
  end
  local owner_revision = payload.field(response, 'owner-publication-revision')
  if owner_revision ~= nil then
    if type(owner_revision) ~= 'number' or owner_revision < 0
       or owner_revision ~= math.floor(owner_revision) then
      error('Invalid owner publication revision') end
    if M.owner_publication_revision ~= nil
       and owner_revision < M.owner_publication_revision then
      return end
  elseif M.owner_publication_revision ~= nil then
    -- A legacy frame cannot overwrite current global authority after a
    -- versioned publication. Its operation-specific outcome is still
    -- handled by the ordinary response dispatcher.
    return
  end
  if owner_revision ~= nil then
    M.owner_publication_revision = owner_revision
  end
  local admission = payload.field_text(response, 'graph-write-admission')
  if admission then
    if admission ~= 'open' and admission ~= 'closed' then
      error('Invalid graph write admission ' .. admission) end
    M.graph_write_admission = admission
    if admission == 'open' and M.client_constructor_admission == 'closed' then
      -- Publication reopens fresh editable views even while old incident
      -- reports remain visible; per-buffer restrictions stay local.
      M.client_constructor_admission = 'open'
    end
  end
  local transition = payload.field_text(response, 'graph-transition-status')
  if transition then M.graph_transition_status = transition end
  local incidents = payload.field(response, 'pending-incidents')
  if incidents ~= nil then M.pending_incidents = incidents end
  local pending_waits = payload.field(response, 'pending-query-waits')
  if pending_waits ~= nil then
    M.pending_query_waits_raw = pending_waits
    local query_wait = package.loaded['skg.query_wait']
    if query_wait and query_wait.ingest_pending then
      query_wait.ingest_pending(pending_waits)
    end
  end
  local config = require('skg.config')
  config.store_state = config.store_state or {}
  local graph = payload.field(response, 'current-graph-generation')
  if graph ~= nil then
    if type(graph) ~= 'number' or graph < 0 or graph ~= math.floor(graph) then
      error('Invalid current graph generation') end
    config.store_state.graph_generation = graph
  end
  local revision = payload.field(response, 'current-manifest-revision')
  if revision ~= nil then
    if type(revision) ~= 'number' or revision < 0
       or revision ~= math.floor(revision) then
      error('Invalid current manifest revision') end
    config.store_state.manifest_revision = revision
  end
  M.update_rebuilding_status(payload.field_text(response, 'rebuilding'))
end

---Return the authority a new view request may ask the server to grant.
---@return string
function M.requested_view_write_authority ()
  if M.graph_write_admission == 'open' then
    if M.client_constructor_admission == 'open' then return 'editable' end
    return 'read-only'
  end
  if M.graph_write_admission == 'closed' then return 'read-only' end
  if M.graph_write_admission == 'closing' then return 'read-only' end
  error('Skg view admission is unavailable before verification')
end

function M.require_client_constructor_admission ()
  if M.client_constructor_admission ~= 'open' then
    error('Skg client constructor admission is closed') end
end

---Return RESPONSE's mandatory view authority, rejecting obsolete grants.
---@param response any
---@return string
function M.view_write_authority_from_response (response)
  local payload = require('skg.payload')
  local authority = payload.field_text(response, 'view-write-authority')
  if authority ~= 'editable' and authority ~= 'read-only' then
    error('Skg response omitted valid view-write-authority') end
  if authority == 'editable'
     and (M.graph_write_admission == 'closed'
          or M.client_constructor_admission == 'closed') then
    error('Skg refused editable view authority while admission is closed') end
  return authority
end

---Require RESPONSE to carry authority from the active server instance.
---@param response any
---@param record table|nil
---@return string
function M.require_current_server_session (response, record)
  local payload = require('skg.payload')
  local session_id = payload.field_text(response, 'server-session-id')
  if not session_id or session_id ~= M.server_session_id then
    error(string.format(
      'Skg refused authority from server session %s; current session is %s',
      tostring(session_id), tostring(M.server_session_id))) end
  if record and record.server_session_id ~= session_id then
    error('Skg buffer belongs to an earlier server session; reopen it') end
  return session_id
end

---The persistent TCP connection to the Rust backend: a vim.uv tcp
---handle, or nil when disconnected.
M.tcp = nil

---Sent request records, keyed by connection-local request ID.
M.request_records = {}
M.request_draft = nil
M.request_queue = {}
M.active_request_id = nil
M.dispatching_request_id = nil
M.next_request_number = 0

---Handlers for unsolicited server-owned operation frames, keyed by kind.
M.server_push_handlers = {}

function M.register_server_push_handler (frame_kind, handler)
  M.server_push_handlers[frame_kind] = handler
end

function M.remove_server_push_handler (frame_kind)
  M.server_push_handlers[frame_kind] = nil
end

local function fresh_request_id ()
  M.next_request_number = M.next_request_number + 1
  return string.format('nvim-%d-%d', vim.fn.getpid(), M.next_request_number)
end

function M.new_internal_request (handlers, incident_id)
  local record = {
    id = fresh_request_id(), handlers = handlers or {},
    incident_id = incident_id,
  }
  M.request_records[record.id] = record
  for _, entry in pairs(record.handlers) do
    if entry.one_shot then
      M.lp_pending_count = M.lp_pending_count + 1 end
  end
  return record
end

function M.ensure_request_draft ()
  if not M.request_draft then
    M.request_draft = { id = fresh_request_id(), handlers = {} } end
  return M.request_draft
end

---Register HANDLER for FRAME_KIND on the request being constructed.
---@param frame_kind string
---@param handler fun(payload_text: string, parsed_response: any)
---@param one_shot boolean|nil
function M.register_response_handler (frame_kind, handler, one_shot)
  local record = M.ensure_request_draft()
  record.handlers[frame_kind] =
    { handler = handler, one_shot = one_shot or false }
  if one_shot then
    M.lp_pending_count = M.lp_pending_count + 1 end
end

function M.request_record_for_edit ()
  if M.dispatching_request_id then
    return M.request_records[M.dispatching_request_id] end
  return M.request_draft
end

function M.remove_response_handler (frame_kind)
  local record = M.request_record_for_edit()
  if not record then return end
  local entry = record.handlers[frame_kind]
  if entry and entry.one_shot then
    M.lp_pending_count = math.max(0, M.lp_pending_count - 1) end
  record.handlers[frame_kind] = nil
end

function M.response_handler_registered (frame_kind)
  local record = M.request_record_for_edit()
  return record and record.handlers[frame_kind] or nil
end

function M.set_request_terminal_handler (handler)
  M.ensure_request_draft().terminal_handler = handler
end

function M.set_request_failure_handler (handler)
  M.ensure_request_draft().failure_handler = handler
end

function M.set_request_finalizer (finalizer)
  M.ensure_request_draft().finalizer = finalizer
end

function M.take_request_record ()
  local record = M.request_draft
    or { id = fresh_request_id(), handlers = {} }
  M.request_draft = nil
  M.request_records[record.id] = record
  return record
end

function M.dispatch_next_request ()
  if M.active_request_id or #M.request_queue == 0 then return end
  local queued = table.remove(M.request_queue, 1)
  M.active_request_id = queued.id
  local ok, err = pcall(queued.send, queued.wire)
  if not ok then M.fail_all_requests('request send failed: ' .. tostring(err)) end
end

function M.enqueue_request (record, wire, send)
  table.insert(M.request_queue,
               { id = record.id, wire = wire, send = send })
  M.dispatch_next_request()
end

function M.enqueue_priority_request (record, wire, send)
  table.insert(M.request_queue, 1,
               { id = record.id, wire = wire, send = send })
  M.dispatch_next_request()
end

local function finalize_record (record, reason)
  if not record or record.finalized then return end
  record.finalized = true
  for _, entry in pairs(record.handlers) do
    if entry.one_shot then
      M.lp_pending_count = math.max(0, M.lp_pending_count - 1) end
  end
  if record.finalizer then
    local ok, err = pcall(record.finalizer, reason)
    if not ok then
      vim.schedule(function ()
        vim.notify('skg request finalizer failed: ' .. tostring(err),
                   vim.log.levels.ERROR) end)
    end
  end
end

function M.finish_request (request_id, terminal_status)
  local record = M.request_records[request_id]
  if record then
    if record.terminal_handler then
      pcall(record.terminal_handler, terminal_status) end
    finalize_record(record, terminal_status or 'terminal')
    M.request_records[request_id] = nil
  end
  if M.active_request_id == request_id then
    M.active_request_id = nil
    M.dispatch_next_request() end
end


function M.fail_all_requests (reason)
  M.request_queue = {}
  M.active_request_id = nil
  M.dispatching_request_id = nil
  local records = {}
  for _, record in pairs(M.request_records) do
    table.insert(records, record) end
  if M.request_draft then table.insert(records, M.request_draft) end
  M.request_draft = nil
  for _, record in ipairs(records) do
    if not record.finalized then
      if record.failure_handler then
        pcall(record.failure_handler, reason) end
      finalize_record(record, reason)
    end
  end
  M.request_records = {}
  M.lp_pending_count = 0
end

function M.transport_failed (reason)
  M.fail_all_requests(reason)
end

function M.clear_request_coordinator ()
  M.fail_all_requests('request coordinator reset')
  M.request_records = {}
  M.request_draft = nil
  M.request_queue = {}
  M.active_request_id = nil
  M.dispatching_request_id = nil
  M.lp_pending_count = 0
end

-- Length-prefixed (Content-Length) receiver state. Lua strings are
-- byte strings, so the accumulator needs no unibyte special-casing.
do
  ---Byte accumulator for length-prefixed responses.
  M.lp_buffer = ''
  ---If nil, expect a header; an integer is an ordinary body length; an
  ---artifact table retains total and descriptor byte lengths.
  M.lp_bytes_left = nil
  ---Number of one-shot responses still expected. Incremented by
  ---register_response_handler for one-shot handlers, decremented by
  ---the dispatcher after processing one.
  M.lp_pending_count = 0
end

---Reset the LP state machine to expect a fresh message. Does not
---reset lp_pending_count -- that is managed by
---register_response_handler and the dispatcher.
function M.lp_reset ()
  M.lp_buffer = ''
  M.lp_bytes_left = nil
end

---Hooks run when the connection resets (sentinel fire or a
---busy-initializing teardown). The lock/stream modules register here
---so a server crash mid-save cannot leave buffers permanently locked.
---@type fun()[]
M.connection_reset_hooks = {}

---Run every connection-reset hook, ignoring individual failures.
function M.run_connection_reset_hooks ()
  for _, hook in ipairs(M.connection_reset_hooks) do
    pcall(hook) end
end

---Close the TCP handle if open. Used by connection_end and reload.
function M.close_connection ()
  if M.tcp and not M.tcp:is_closing() then
    M.tcp:read_stop()
    M.tcp:close() end
  M.tcp = nil
end

---The linkstack: a stack of {id, title} string pairs.
---@type string[][]
M.id_stack = {}

return M
