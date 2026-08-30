-- PURPOSE: Global state for the skg client.
-- The Lua port of elisp/skg-state.el. (One difference of homes: the
-- config path lives in skg.config here, not in state; see that file's
-- header.)

local M = {}

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
  queued.send(queued.wire)
end

function M.enqueue_request (record, wire, send)
  table.insert(M.request_queue,
               { id = record.id, wire = wire, send = send })
  M.dispatch_next_request()
end

function M.finish_request (request_id)
  local record = M.request_records[request_id]
  if record then
    for _, entry in pairs(record.handlers) do
      if entry.one_shot then
        M.lp_pending_count = math.max(0, M.lp_pending_count - 1) end
    end
    M.request_records[request_id] = nil
  end
  if M.active_request_id == request_id then
    M.active_request_id = nil
    M.dispatch_next_request() end
end

function M.clear_request_coordinator ()
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
  ---If nil, expecting a header; if an integer, body bytes remaining.
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
