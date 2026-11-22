-- PURPOSE: Every answer from the server passes through here on its
-- way to the command that asked for it. 'handle_generic_chunk'
-- (which skg.client calls with each chunk read) splits the byte
-- stream into length-prefixed messages; 'dispatch_by_type' hands each
-- to the handler skg.state's maps hold for its response-type.
-- 'tell_answer_not_applied' tells the user a handler failed, and
-- 'register_overPrivateText_confirmation_instead_of' asks before a
-- request shows overPrivateText telescope text. Registering the
-- handlers is each command's own. The Lua port of
-- elisp/skg-length-prefix.el. (Like the elisp, this file does not add
-- length prefixes to outgoing messages; that is easier and done
-- inline where messages are sent.)
--
-- One wire fact the dispatcher relies on: the server's sexp printer
-- (the Rust 'sexp' crate) prints string atoms bare unless they
-- contain quotes, whitespace, or look numeric -- so 'response-type'
-- and its values arrive as bare atoms (symbols to our reader). The
-- dispatcher extracts the type via atom_text, which also tolerates a
-- quoted spelling.

local log = require('skg.log')
local payload = require('skg.payload')
local state = require('skg.state')
local sexpr = require('skg.sexpr.parse')

local M = {}

---What tell_answer_not_applied says happened, unless told otherwise.
M.answer_not_applied_default_consequence =
  'the server may have saved, but the buffer was not updated'

---Say (vim.notify) that applying the server's ANSWER (its response
---type) failed with ERR, and what that means: CONSEQUENCE, by default
---M.answer_not_applied_default_consequence. A handler that catches an
---error while applying an answer calls this as well as log.log, which
---is off by default (log.log_file): an error only logged leaves the
---user thinking the command did nothing.
---@param answer string
---@param err any
---@param consequence string|nil
function M.tell_answer_not_applied (answer, err, consequence)
  vim.notify(string.format(
               "skg: %s: applying the server's %s answer failed (%s)",
               consequence or M.answer_not_applied_default_consequence,
               answer, tostring(err)),
             vim.log.levels.ERROR)
end

---Ask the prompt of RESPONSE, an overPrivateText-telescope-confirmation
---(DEFAULT_PROMPT if it carries none), and if the user approves, call
---ON_APPROVAL with the pids it lists, as strings: the telescopes whose
---text the retried request may show.
---@param response any
---@param default_prompt string
---@param on_approval fun(pids: string[])
function M.when_overPrivateText_approved (response, default_prompt,
                                          on_approval)
  local prompt = payload.field_text(response, 'prompt') or default_prompt
  local pids = payload.string_list(payload.field(response, 'pids'))
  if vim.fn.confirm(prompt, '&Include\n&Decline', 2) == 1 then
    on_approval(pids) end
end

---Register the handler of an overPrivateText-telescope-confirmation,
---which the server sends instead of ANSWER_TYPE, a one-shot answer,
---when that answer would show text from overPrivateText telescopes. It
---is not one-shot, so only ANSWER_TYPE counts toward the pending
---responses. When it fires, both handlers are removed (balancing
---ANSWER_TYPE's count), and if the user approves
---('when_overPrivateText_approved'), RETRY is called with the pids
---approved.
---@param answer_type string
---@param default_prompt string
---@param retry fun(pids: string[])
function M.register_overPrivateText_confirmation_instead_of (
    answer_type, default_prompt, retry)
  state.register_response_handler('overPrivateText-telescope-confirmation',
    function (_payload_text, response)
      state.unregister_response_handlers(
        { 'overPrivateText-telescope-confirmation', answer_type })
      M.when_overPrivateText_approved(response, default_prompt, retry)
    end, false)
end

---Consume the message stream in chunks: accumulate CHUNK's bytes,
---then step the LP machine until it must wait or finishes a message;
---each completed message dispatches by response-type. Continues while
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
      M.dispatch_by_type(step.payload)
      if #state.lp_buffer == 0 then return end
    elseif step.kind == 'error' then
      state.lp_buffer = ''
      state.lp_bytes_left = nil
      error(step.message)
    end
  end
end

---Parse PAYLOAD as a sexp, extract its response-type, and dispatch
---via the handler map, or else, for a message the server sends
---unprompted, the unprompted handler map. A one-shot handler is removed after a
---successful call (an error leaves it registered, mirroring elisp,
---where the removal is skipped when the handler signals).
---@param payload string
function M.dispatch_by_type (payload)
  local ok, err = pcall(function ()
    local response = sexpr.read(payload)
    local response_type = M.response_type_of(response)
    if not response_type then
      log.log('warn', 'dispatch', 'response missing response-type: %s',
              payload:sub(1, 80))
      return end
    local entry = state.response_handler_map[response_type]
    if not entry then
      local unprompted =
        state.unprompted_response_handler_map[response_type]
      if unprompted then
        unprompted(payload, response)
      else
        log.log('warn', 'dispatch', 'no handler for response type: %s',
                response_type) end
      return end
    entry.handler(payload, response)
    if response_type == 'save-result'
       or response_type == 'rerender-done'
       or response_type == 'skgrepo-restriction'
       or response_type == 'delete-references-result'
       or response_type == 'rebuild-session-lifetime-stores' then
      local annotations = package.loaded['skg.link_annotations']
      if annotations then annotations.invalidate_all() end
    end
    if entry.one_shot then
      state.response_handler_map[response_type] = nil
      state.lp_pending_count =
        math.max(0, state.lp_pending_count - 1) end
  end)
  if not ok then
    log.log('error', 'dispatch', 'dispatch error: %s for payload: %s',
            tostring(err), payload:sub(1, 80))
    local read_ok, response = pcall(sexpr.read, payload)
    M.tell_answer_not_applied(
      (read_ok and M.response_type_of(response)) or 'unreadable', err,
      'the server may have acted (saved, say), but the client did not'
        .. ' finish handling its answer')
  end
end

---The response-type name of parsed RESPONSE, or nil.
---@param response any
---@return string|nil
function M.response_type_of (response)
  if not sexpr.is_list(response) then return nil end
  for _, element in ipairs(response) do
    if sexpr.is_list(element) and #element >= 2
       and not sexpr.is_list(element[1])
       and sexpr.atom_text(element[1]) == 'response-type'
       and not sexpr.is_list(element[2]) then
      return sexpr.atom_text(element[2]) end
  end
  return nil
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
