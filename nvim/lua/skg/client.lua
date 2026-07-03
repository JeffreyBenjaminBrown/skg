-- PURPOSE: Connection lifecycle for the persistent TCP link to the
-- Rust server. The Lua port of elisp/skg-client.el (minus the require
-- hub, which lua/skg/init.lua plays).
--
-- Concurrency model: vim.uv callbacks may not touch most editor
-- state, so the read callback forwards each chunk through
-- vim.schedule; scheduled callbacks run in FIFO order, preserving the
-- byte stream's order, and everything downstream (LP machine,
-- handlers, buffer updates) runs on the main loop -- the same
-- single-threaded discipline Emacs's process filter gave the elisp
-- client.

local config = require('skg.config')
local length_prefix = require('skg.length_prefix')
local log = require('skg.log')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---The server port, read from skgconfig.toml at init.
---@type integer|nil
M.port = nil

---Seconds to wait for the TCP connect to complete. (Emacs's
---make-network-process with :nowait nil blocks the same way.)
M.connect_timeout_ms = 3000

---Connect, persistently, to the Rust TCP server. Idempotent: creates
---a new connection only if none exists or the existing one is dead.
---Errors with a user-facing message if the server is unreachable.
---@return any the tcp handle
function M.connect ()
  if state.tcp and not state.tcp:is_closing() then return state.tcp end
  if not M.port then
    if not config.config_file() then
      error('skg: not initialized; run :SkgInit <skgconfig.toml>') end
    M.port = config.port_from_toml(config.config_file()) end
  local tcp = vim.uv.new_tcp()
  local connect_result = nil
  tcp:connect('127.0.0.1', M.port, function (err)
    connect_result = err or 'ok'
  end)
  vim.wait(M.connect_timeout_ms,
           function () return connect_result ~= nil end, 10)
  if connect_result ~= 'ok' then
    pcall(function () tcp:close() end)
    state.tcp = nil
    error(M.server_unavailable_message(
      connect_result or 'connection timed out')) end
  state.tcp = tcp
  tcp:read_start(function (err, chunk)
    -- The uv read callback: forward data (or closure) to the main
    -- loop. A nil chunk without error means EOF; both tear down like
    -- the elisp sentinel.
    if err or chunk == nil then
      vim.schedule(function ()
        M.sentinel(err and ('connection error: ' .. err)
                   or 'connection closed') end)
    else
      vim.schedule(function () M.handle_rust_response(chunk) end) end
  end)
  return state.tcp
end

---A user-facing message for a failed server connection.
---@param err string
---@return string
function M.server_unavailable_message (err)
  local config_file = config.config_file()
  local logs_dir
  if config_file then
    logs_dir = vim.fn.fnamemodify(config_file, ':h') .. '/logs'
  else
    logs_dir = vim.fn.getcwd() .. '/logs' end
  return string.format(
    'Could not connect to the SKG server on port %s.\n'
    .. 'The server may have failed during startup.\n'
    .. 'Look for the startup error in:\n  %s\n  %s\n'
    .. 'Connection error: %s',
    M.port and tostring(M.port) or '[unknown]',
    logs_dir .. '/server-to-user.log',
    logs_dir .. '/cargo-watch.log',
    tostring(err))
end

---Route a chunk from the server to the LP machine -- unless it is the
---plain-text (un-prefixed) busy-initializing signal, which tears down
---any in-flight streams and tells the user the server is still
---starting.
---@param chunk string
function M.handle_rust_response (chunk)
  local trimmed = chunk:gsub('^%s+', '')
  if trimmed:sub(1, #'((busy-initializing') == '((busy-initializing' then
    local ok, parsed = pcall(sexpr.read, trimmed)
    local message = 'skg server is still initializing'
    if ok and sexpr.is_pair(parsed[1]) then
      message = tostring(parsed[1].cdr) end
    vim.notify(message)
    state.run_connection_reset_hooks()
    state.response_handler_map = {}
    state.lp_pending_count = 0
    state.lp_reset()
  else
    length_prefix.handle_generic_chunk(chunk) end
end

---Clean up when the TCP connection closes: run the reset hooks (which
---unlock all save-locked buffers and end any stream) and clear the
---handler map, so stale non-one-shot handlers (like collateral-view
---or rerender-view) cannot linger after a server crash.
---@param event string
function M.sentinel (event)
  log.log('info', 'tcp', 'connection closed: %s', event)
  state.close_connection()
  state.run_connection_reset_hooks()
  state.response_handler_map = {}
  state.lp_pending_count = 0
end

---Send TEXT on the persistent connection, connecting first if needed.
---@param text string
function M.send_string (text)
  local tcp = M.connect()
  tcp:write(text)
end

---Manually close the connection to the Rust server.
function M.connection_end ()
  state.close_connection()
end

return M
