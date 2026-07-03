-- Shared helpers for the skg nvim specs.
-- (The analog of elisp/skg-test-utils.el, plus the loopback fake
-- server several protocol specs use.)

local M = {}

---Install the pinned herald-rule fixture (the analog of
---'skg-test-install-herald-rules'), so specs need no server.
function M.install_fixture_herald_rules ()
  local path =
    _G.skg_test_repo_root() .. '/tests/elisp/herald-rules.sexp'
  local handle = assert(io.open(path, 'r'))
  local text = handle:read('*a')
  handle:close()
  require('skg.herald_rules').install_rules(
    require('skg.sexpr.parse').read(text))
end

---An LP-framed message around PAYLOAD.
---@param payload string
---@return string
function M.framed (payload)
  return string.format('Content-Length: %d\r\n\r\n%s',
                       #payload, payload)
end

---A minimal TCP server on 127.0.0.1. ON_REQUEST(line, respond) runs
---for each newline-terminated request line; respond(text) writes raw
---bytes back. Returns {port, close}.
---@param on_request fun(line: string, respond: fun(text: string))
---@return table
function M.fake_server (on_request)
  local server = vim.uv.new_tcp()
  server:bind('127.0.0.1', 0)
  local connections = {}
  server:listen(16, function ()
    local connection = vim.uv.new_tcp()
    server:accept(connection)
    table.insert(connections, connection)
    local pending = ''
    connection:read_start(function (err, chunk)
      if err or chunk == nil then return end
      pending = pending .. chunk
      while true do
        local line, rest = pending:match('^([^\n]*)\n(.*)$')
        if not line then break end
        pending = rest
        on_request(line, function (text) connection:write(text) end)
      end
    end)
  end)
  return {
    port = server:getsockname().port,
    close = function ()
      for _, connection in ipairs(connections) do
        pcall(function ()
          connection:read_stop()
          connection:close() end)
      end
      pcall(function () server:close() end)
    end }
end

---Point the client at a fake server; returns it. Callers own closing
---it and calling M.reset_client_state after.
---@param on_request fun(line: string, respond: fun(text: string))
---@return table
function M.connect_to_fake_server (on_request)
  local server = M.fake_server(on_request)
  require('skg.client').port = server.port
  return server
end

---Tear down connection and handler state between specs.
function M.reset_client_state ()
  local state = require('skg.state')
  state.close_connection()
  state.response_handler_map = {}
  state.lp_pending_count = 0
  state.lp_reset()
  state.connection_reset_hooks = {}
  require('skg.client').port = nil
end

---Delete every skg view buffer without ceremony.
function M.wipe_skg_buffers ()
  local buffer = require('skg.buffer')
  for _, buf in ipairs(buffer.all_skg_buffers()) do
    vim.bo[buf].modified = false
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end
end

return M
