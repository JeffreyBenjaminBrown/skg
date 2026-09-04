-- PURPOSE: Read length-prefixed frames and dispatch by request identity.
-- The Lua port of elisp/skg-length-prefix.el. (Like the elisp, this
-- file does not ADD length prefixes to outgoing messages; that is
-- easier and done inline where messages are sent.)
--
-- One wire fact the dispatcher relies on: the server's sexp printer
-- (the Rust 'sexp' crate) prints string atoms BARE unless they
-- contain quotes, whitespace, or look numeric -- so 'response-type'
-- and its values arrive as bare atoms (symbols to our reader). The
-- dispatcher extracts the type via atom_text, which also tolerates a
-- quoted spelling.

local log = require('skg.log')
local state = require('skg.state')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Consume the message stream in chunks: accumulate CHUNK's bytes,
---then step the LP machine until it must wait or finishes a message;
---each completed message dispatches by request-id. Continues while
---buffered data remains.
---@param chunk string
function M.handle_generic_chunk (chunk)
  state.lp_buffer = state.lp_buffer .. chunk
  while true do
    local step = M.step(state.lp_buffer, state.lp_bytes_left)
    if step.kind == 'header' then
      state.lp_bytes_left = step.length
      state.lp_buffer = step.remainder
    elseif step.kind == 'need_more' then
      return
    elseif step.kind == 'done' then
      state.lp_buffer = step.remainder
      state.lp_bytes_left = nil
      M.dispatch_frame(step.payload)
      if #state.lp_buffer == 0 then return end
    elseif step.kind == 'error' then
      state.lp_buffer = ''
      state.lp_bytes_left = nil
      state.fail_all_requests(step.message)
      error(step.message)
    end
  end
end

---Parse PAYLOAD, dispatch it to its request record, and clean up once.
---@param payload string
function M.dispatch_frame (payload)
  local parsed_ok, response = pcall(sexpr.read, payload)
  if not parsed_ok then
    log.log('error', 'dispatch', 'could not parse frame: %s',
            tostring(response))
    state.fail_all_requests('response parse failed: ' .. tostring(response))
    return end
  local request_id = M.field_atom(response, 'request-id')
  local incident_id = M.field_atom(response, 'incident-id')
  local frame_kind = M.field_atom(response, 'frame-kind')
    or M.field_atom(response, 'response-type')
  local terminal_status = M.field_atom(response, 'terminal-status')
  local server_push = M.field_atom(response, 'server-push')
  if not request_id then
    if server_push == 'true' then
      local handler = state.server_push_handlers[frame_kind]
      if handler then
        local handler_ok, handler_error = pcall(handler, payload, response)
        if not handler_ok then
          log.log('error', 'dispatch',
                  'server-push dispatch error: %s for payload: %s',
                  tostring(handler_error), payload:sub(1, 80)) end
      else
        log.log('warn', 'dispatch',
                'no server-push handler for frame %s',
                tostring(frame_kind)) end
      return end
    log.log('warn', 'dispatch', 'response missing request-id: %s',
            payload:sub(1, 80))
    return end
  local record = state.request_records[request_id]
  if not record then
    log.log('warn', 'dispatch', 'unknown/stale request-id: %s', request_id)
    return end
  if incident_id ~= record.incident_id then
    vim.notify('SKG protocol failure: incident identity changed',
               vim.log.levels.ERROR)
    if record.failure_handler then
      pcall(record.failure_handler, 'incident identity mismatch') end
    state.finish_request(request_id, 'protocol-failed')
    return end
  local entry = record.handlers[frame_kind]
  state.dispatching_request_id = request_id
  local handler_ok, handler_error = pcall(function ()
    if entry then
      entry.handler(payload, response)
    elseif frame_kind == 'error' then
      vim.notify('SKG request failed: '
        .. (M.field_atom(response, 'content') or payload),
        vim.log.levels.ERROR)
    else
      log.log('warn', 'dispatch',
              'no handler for frame %s on request %s',
              tostring(frame_kind), request_id) end
  end)
  state.dispatching_request_id = nil
  if entry and entry.one_shot then
    record.handlers[frame_kind] = nil
    state.lp_pending_count = math.max(0, state.lp_pending_count - 1) end
  if terminal_status then
    state.finish_request(request_id, terminal_status)
  elseif not handler_ok then
    if record.failure_handler then
      pcall(record.failure_handler, tostring(handler_error)) end
    state.finish_request(request_id, 'handler-failed')
  end
  if not handler_ok then
    log.log('error', 'dispatch', 'dispatch error: %s for payload: %s',
            tostring(handler_error), payload:sub(1, 80)) end
end

function M.field_atom (response, field_name)
  if not sexpr.is_list(response) then return nil end
  for _, element in ipairs(response) do
    if sexpr.is_list(element) and #element >= 2
       and not sexpr.is_list(element[1])
       and sexpr.atom_text(element[1]) == field_name
       and not sexpr.is_list(element[2]) then
      return sexpr.atom_text(element[2]) end
  end
  return nil
end

---The response-type name of parsed RESPONSE, or nil.
---@param response any
---@return string|nil
function M.response_type_of (response)
  return M.field_atom(response, 'response-type')
end

---One pure step of the LP machine over BUF with BYTES_LEFT (nil =
---need header; N = need N body bytes). Returns a table tagged by
---'kind': need_more {}, header {length, remainder},
---done {payload, remainder}, or error {message}.
---@param buf string
---@param bytes_left integer|nil
---@return table
function M.step (buf, bytes_left)
  if bytes_left == nil then
    local header = M.try_parse_header(buf)
    if header.kind == 'incomplete' then
      return { kind = 'need_more' } end
    if header.kind == 'error' then return header end
    return { kind = 'header',
             length = header.length, remainder = header.remainder }
  end
  local body = M.try_consume_body(buf, bytes_left)
  if body.kind == 'incomplete' then return { kind = 'need_more' } end
  return body
end

---Split BUF at the '\r\n\r\n' header terminator and read its
---Content-Length. Returns {kind='incomplete'}, {kind='error',
---message}, or {kind='ok'|'header'...} -- specifically
---{kind='ok', length, remainder} where remainder is the (possibly
---partial) body bytes after the header.
---@param buf string
---@return table
function M.try_parse_header (buf)
  local separator_start, separator_end =
    buf:find('\r\n\r\n', 1, true)
  if not separator_start then return { kind = 'incomplete' } end
  local header = buf:sub(1, separator_end)
  local remainder = buf:sub(separator_end + 1)
  local length = header:match('Content%-Length: (%d+)')
  if not length then
    return { kind = 'error',
             message =
               'Malformed header in length-prefixed response' } end
  return { kind = 'ok', length = tonumber(length),
           remainder = remainder }
end

---If BUF holds at least BYTES_LEFT bytes, split off the payload.
---@param buf string
---@param bytes_left integer
---@return table {kind='done', payload, remainder} or {kind='incomplete'}
function M.try_consume_body (buf, bytes_left)
  if #buf < bytes_left then return { kind = 'incomplete' } end
  return { kind = 'done',
           payload = buf:sub(1, bytes_left),
           remainder = buf:sub(bytes_left + 1) }
end

return M
