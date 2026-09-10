-- Mirrors tests/elisp/test-skg-client.el (the connect-failure message
-- test), plus a loopback fake-server suite that exercises the full
-- networking stack -- connect, request, LP-framed reply, dispatch,
-- busy-initializing, sentinel cleanup -- without a real skg server.
-- (The real-server counterparts live in tests/integration/.)

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local client = require('skg.client')
local config = require('skg.config')
local state = require('skg.state')

local fake_server = helpers.fake_server
local framed = helpers.framed

describe('skg.client', function ()
  local server

  before_each(function ()
    state.close_connection()
    state.clear_request_coordinator()
    state.lp_reset()
    state.connection_reset_hooks = {}
  end)

  after_each(function ()
    if server then server.close() server = nil end
    state.close_connection()
    client.port = nil
    config.config_file_path = nil
  end)

  it('preserves constructor admission across connection reset with a retained report', function ()
    for _, barrier in ipairs({ 'open', 'closing', 'closed' }) do
      state.maintenance_client_incident = { incident_id = 'report', phase = 'finalizing-archive' }
      state.client_constructor_admission = barrier
      client.handle_rust_response('((busy-initializing . "still starting"))\n')
      assert.are.equal(barrier, state.client_constructor_admission)
      client.sentinel('closed')
      assert.are.equal(barrier, state.client_constructor_admission)
    end
    state.client_constructor_admission = 'open'
    state.maintenance_client_incident = nil
  end)

  it('reports a helpful message when the server is unreachable',
     function ()
    -- Mirrors test-skg-connect-failure-points-to-startup-logs.
    local config_dir = vim.fn.tempname()
    vim.fn.mkdir(config_dir, 'p')
    local config_file = config_dir .. '/skgconfig.toml'
    vim.fn.writefile({ 'port = 1' }, config_file) -- nothing listens on 1
    config.config_file_path = config_file
    client.port = nil
    local ok, err = pcall(client.connect)
    assert.is_false(ok)
    local message = tostring(err)
    assert.is_truthy(message:find(
      'Could not connect to the SKG server on port 1', 1, true))
    assert.is_truthy(message:find(
      config_dir .. '/logs/server-to-user.log', 1, true))
    assert.is_truthy(message:find(
      config_dir .. '/logs/cargo-watch.log', 1, true))
    vim.fn.delete(config_dir, 'rf')
  end)

  it('connects, sends a request, and dispatches the framed reply',
     function ()
    server = fake_server(function (line, respond)
      if line:find('verify connection', 1, true) then
        respond(framed(
          '((response-type verify-connection) (content "pong"))'))
      end
    end)
    client.port = server.port
    local seen = nil
    state.register_response_handler('verify-connection',
      function (payload) seen = payload end, true)
    client.submit_request('((request . "verify connection"))\n')
    vim.wait(2000, function () return seen ~= nil end, 10)
    assert.is_truthy(seen)
    assert.is_truthy(seen:find('pong', 1, true))
  end)

  it('handles a reply split across many small chunks', function ()
    local payload =
      '((response-type verify-connection) (content "chunked ñ"))'
    server = fake_server(function (_line, respond)
      -- Send one byte at a time: worst-case reassembly.
      local message = framed(payload)
      for i = 1, #message do respond(message:sub(i, i)) end
    end)
    client.port = server.port
    local seen = nil
    state.register_response_handler('verify-connection',
      function (payload_text) seen = payload_text end, true)
    client.submit_request('((request . "verify connection"))\n')
    vim.wait(2000, function () return seen ~= nil end, 10)
    assert.is_truthy(seen:find('chunked ñ', 1, true))
  end)

  it('tears down streams and handlers on busy-initializing',
     function ()
    server = fake_server(function (_line, respond)
      respond('((busy-initializing . "still loading the graph"))\n')
    end)
    client.port = server.port
    local reset_ran = false
    table.insert(state.connection_reset_hooks,
                 function () reset_ran = true end)
    state.register_response_handler('verify-connection',
      function () end, true)
    client.submit_request('((request . "verify connection"))\n')
    vim.wait(2000, function () return reset_ran end, 10)
    assert.is_true(reset_ran)
    assert.are.same({}, state.request_records)
    assert.are.equal(0, state.lp_pending_count)
  end)

  it('runs the sentinel cleanup when the server closes the socket',
     function ()
    server = fake_server(function () end)
    client.port = server.port
    client.connect()
    state.register_response_handler('collateral-view',
      function () end, false)
    local reset_ran = false
    table.insert(state.connection_reset_hooks,
                 function () reset_ran = true end)
    server.close()
    vim.wait(2000, function () return reset_ran end, 10)
    assert.is_true(reset_ran)
    assert.are.same({}, state.request_records)
    assert.is_nil(state.tcp)
  end)

  it('is idempotent while a live connection exists', function ()
    server = fake_server(function () end)
    client.port = server.port
    local first = client.connect()
    local second = client.connect()
    assert.are.equal(first, second)
  end)
end)
