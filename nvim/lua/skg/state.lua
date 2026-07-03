-- PURPOSE: Global state for the skg client.
-- The Lua port of elisp/skg-state.el. (One difference of homes: the
-- config path lives in skg.config here, not in state; see that file's
-- header.)

local M = {}

---The persistent TCP connection to the Rust backend: a vim.uv tcp
---handle, or nil when disconnected.
M.tcp = nil

---Map from response-type names (strings) to handler entries
---{ handler = fn(payload_text, parsed_response), one_shot = boolean }.
---
---PITFALL (inherited from the Emacs client): keyed by response-type,
---not by request instance. If the user fires two requests of the same
---type in rapid succession, the second registration replaces the
---first's handler. In practice this hasn't been a problem because
---responses are fast. If it ever matters, the fix is a per-request
---token or a queue.
M.response_handler_map = {}

---Register HANDLER for RESPONSE_TYPE (a string). If ONE_SHOT, the
---handler is removed after first use and the pending count is
---incremented. Replaces any existing handler for the same type.
---@param response_type string
---@param handler fun(payload_text: string, parsed_response: any)
---@param one_shot boolean|nil
function M.register_response_handler (response_type, handler, one_shot)
  M.response_handler_map[response_type] =
    { handler = handler, one_shot = one_shot or false }
  if one_shot then
    M.lp_pending_count = M.lp_pending_count + 1 end
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
