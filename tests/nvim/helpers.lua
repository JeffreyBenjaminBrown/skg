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

local nonterminal_frame_kinds = {
  ['save-lock'] = true,
  ['save-relax-lock'] = true,
  ['collateral-view'] = true,
  ['search-results'] = true,
  ['request-snapshot'] = true,
  ['rerender-lock'] = true,
  ['rerender-view'] = true,
  ['git-diff-mode'] = true,
  ['active-source-set'] = true,
}

local authorization_frame_kinds = {
  ['fork-confirmation'] = true,
  ['telescope-hoist-confirmation'] = true,
  ['ugly-telescope-confirmation'] = true,
}

---An LP-framed server message. Inside a fake-server callback it adds
---the request envelope belonging to the line which triggered the callback.
---@param payload string
---@return string
function M.framed (payload)
  if M.current_request_id
     and not payload:find('(request-id ', 1, true) then
    local frame_kind = payload:match(
      '%(%s*"?response%-type"?%s+"?([^"%s%)]+)"?%)') or 'unknown'
    local terminal_status = nil
    if authorization_frame_kinds[frame_kind] then
      terminal_status = 'needs-authorization'
    elseif frame_kind == 'error' then
      terminal_status = 'failed'
    elseif not nonterminal_frame_kinds[frame_kind] then
      terminal_status = 'complete' end
    local envelope = string.format(
      ' (request-id %q) (frame-kind %s)',
      M.current_request_id, frame_kind)
    if terminal_status then
      envelope = envelope
        .. string.format(' (terminal-status %s)', terminal_status) end
    payload = payload:sub(1, -2) .. envelope .. ')'
  end
  return string.format('Content-Length: %d\r\n\r\n%s',
                       #payload, payload)
end

local function envelope_framed_for_request (message, request_id)
  if not request_id or not message:find('^Content%-Length:') then
    return message end
  local boundary = message:find('\r\n\r\n', 1, true)
  if not boundary then return message end
  local payload = message:sub(boundary + 4)
  if payload:find('(request-id ', 1, true) then return message end
  local previous = M.current_request_id
  M.current_request_id = request_id
  local result = M.framed(payload)
  M.current_request_id = previous
  return result
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
        if pending:find('^Content%-Length:') then
          local boundary = pending:find('\r\n\r\n', 1, true)
          if not boundary then break end
          local length = tonumber(pending:sub(1, boundary - 1):match(
            '^Content%-Length:%s*(%d+)$'))
          if not length or #pending < boundary + 3 + length then break end
          pending = pending:sub(boundary + 4 + length)
        end
        local line, rest = pending:match('^([^\n]*)\n(.*)$')
        if not line then break end
        pending = rest
        local request_id = line:match(
          '%(%s*request%-id%s+%.%s+"([^"]+)"%)')
        M.current_request_id = request_id
        local respond = function (text)
          connection:write(envelope_framed_for_request(text, request_id))
        end
        if line:find('(role . "interactive")', 1, true) then
          respond(M.framed(
            '((response-type verify-connection) (content "connected")'
            .. ' (source-inventory ()) (telescope-warnings ())'
            .. ' (pending-recovery-incidents ()) (active-source-set all)'
            .. ' (graph-generation 1) (manifest-revision 1)'
            .. ' (maintenance-epoch 0) (maintenance-state idle)'
            .. ' (census-required true)'
            .. ' (maintenance-archive-folder archive)'
            .. ' (maintenance-archive-identity /tmp/archive)'
            .. ' (typedb-health healthy) (tantivy-health healthy))'))
        elseif line:find('(request . "client census")', 1, true)
            or line:find('(request . "client census texts")', 1, true) then
          respond(M.framed(
            '((response-type client-census) (census-complete true)'
            .. ' (write-enabled true) (text-required-buffer-ids ())'
            .. ' (stale-buffer-ids ()) (restored-buffer-ids ()))'))
        else
          on_request(line, respond)
        end
        M.current_request_id = nil
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
  state.connection_handshake_state = nil
  state.clear_request_coordinator()
  state.lp_reset()
  state.connection_reset_hooks = {}
  require('skg.client').port = nil
end

---Delete every registered Skg-owned or legacy URI-bearing buffer.
function M.wipe_skg_buffers ()
  local buffer = require('skg.buffer')
  local registry = require('skg.buffer_registry')
  local seen = {}
  local owned = registry.buffers()
  for _, buf in ipairs(owned) do seen[buf] = true end
  for _, buf in ipairs(buffer.all_skg_buffers()) do
    if not seen[buf] then table.insert(owned, buf) end
    seen[buf] = true
  end
  for _, buf in ipairs(owned) do
    vim.bo[buf].modified = false
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end
end

return M
