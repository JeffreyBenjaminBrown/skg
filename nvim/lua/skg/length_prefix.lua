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
      state.lp_bytes_left = step.body_state or step.length
      state.lp_buffer = step.remainder
    elseif step.kind == 'need_more' then
      return
    elseif step.kind == 'done' then
      state.lp_buffer = step.remainder
      state.lp_bytes_left = nil
      M.dispatch_frame(step.payload, step.artifact_bytes)
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
---@param artifact_bytes string|nil exact opaque bytes following the descriptor
function M.dispatch_frame (payload, artifact_bytes)
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
        local handler_ok, handler_error =
          pcall(handler, payload, response, artifact_bytes)
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
      entry.handler(payload, response, artifact_bytes)
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
---need header; N = need N body bytes; a table = artifact boundary).
---Returns a table tagged by
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
             length = header.length, body_state = header.body_state,
             descriptor_length = header.descriptor_length,
             remainder = header.remainder }
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
  local header = buf:sub(1, separator_start - 1)
  local remainder = buf:sub(separator_end + 1)
  local lengths = M.header_values(header, 'Content-Length')
  if #lengths ~= 1 or not lengths[1]:match('^%d+$') then
    return { kind = 'error',
             message =
               'Malformed header in length-prefixed response' } end
  local length = tonumber(lengths[1])
  local content_types = M.header_values(header, 'Content-Type')
  local descriptor_lengths = M.header_values(header, 'Descriptor-Length')
  local artifact_type = 'application/x-skg-artifact-bundle'
  local declares_artifact = #descriptor_lengths > 0
  for _, value in ipairs(content_types) do
    if value == artifact_type then declares_artifact = true end end
  if declares_artifact then
    local descriptor = #descriptor_lengths == 1
      and descriptor_lengths[1]:match('^%d+$')
      and tonumber(descriptor_lengths[1]) or nil
    if #content_types ~= 1 or content_types[1] ~= artifact_type
       or not descriptor or descriptor > length then
      return { kind = 'error',
               message = 'Malformed artifact-bundle header' } end
    return {
      kind = 'ok', length = length,
      descriptor_length = descriptor,
      body_state = {
        kind = 'artifact', length = length,
        descriptor_length = descriptor },
      remainder = remainder }
  end
  return { kind = 'ok', length = length, body_state = length,
           remainder = remainder }
end

---Return all exact NAME header values from HEADER.
function M.header_values (header, name)
  local values = {}
  local prefix = name .. ': '
  for line in (header .. '\r\n'):gmatch('(.-)\r\n') do
    if line:sub(1, #prefix) == prefix then
      table.insert(values, line:sub(#prefix + 1)) end
  end
  return values
end

---If BUF holds the complete body, preserve an artifact tail as opaque bytes.
---@param buf string
---@param bytes_left integer|table
---@return table {kind='done', payload, remainder} or {kind='incomplete'}
function M.try_consume_body (buf, bytes_left)
  if type(bytes_left) == 'table' then
    if bytes_left.kind ~= 'artifact'
       or type(bytes_left.length) ~= 'number'
       or type(bytes_left.descriptor_length) ~= 'number'
       or bytes_left.descriptor_length > bytes_left.length then
      return { kind = 'error', message = 'Invalid artifact receiver state' }
    end
    if #buf < bytes_left.length then return { kind = 'incomplete' } end
    local body = buf:sub(1, bytes_left.length)
    local descriptor = body:sub(1, bytes_left.descriptor_length)
    if not M.valid_utf8(descriptor) then
      return { kind = 'error',
               message = 'Artifact descriptor is not valid UTF-8' } end
    return {
      kind = 'done', payload = descriptor,
      artifact_bytes = body:sub(bytes_left.descriptor_length + 1),
      remainder = buf:sub(bytes_left.length + 1) }
  end
  if #buf < bytes_left then return { kind = 'incomplete' } end
  return { kind = 'done',
           payload = buf:sub(1, bytes_left),
           remainder = buf:sub(bytes_left + 1) }
end

---True exactly when BYTES is structurally valid UTF-8.
function M.valid_utf8 (bytes)
  local index = 1
  local function continuation (offset, low, high)
    local byte = bytes:byte(index + offset)
    return byte and byte >= (low or 0x80) and byte <= (high or 0xbf)
  end
  while index <= #bytes do
    local first = bytes:byte(index)
    local width
    if first <= 0x7f then
      width = 1
    elseif first >= 0xc2 and first <= 0xdf
           and continuation(1) then
      width = 2
    elseif first == 0xe0 and continuation(1, 0xa0, 0xbf)
           and continuation(2) then
      width = 3
    elseif first >= 0xe1 and first <= 0xec
           and continuation(1) and continuation(2) then
      width = 3
    elseif first == 0xed and continuation(1, 0x80, 0x9f)
           and continuation(2) then
      width = 3
    elseif first >= 0xee and first <= 0xef
           and continuation(1) and continuation(2) then
      width = 3
    elseif first == 0xf0 and continuation(1, 0x90, 0xbf)
           and continuation(2) and continuation(3) then
      width = 4
    elseif first >= 0xf1 and first <= 0xf3
           and continuation(1) and continuation(2)
           and continuation(3) then
      width = 4
    elseif first == 0xf4 and continuation(1, 0x80, 0x8f)
           and continuation(2) and continuation(3) then
      width = 4
    else
      return false
    end
    index = index + width
  end
  return true
end

return M
