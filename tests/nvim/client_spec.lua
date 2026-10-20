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
    state.response_handler_map = {}
    state.lp_pending_count = 0
    state.lp_reset()
    state.connection_reset_hooks = {}
  end)

  after_each(function ()
    if server then server.close() server = nil end
    state.close_connection()
    client.port = nil
    config.config_file_path = nil
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
    client.send_string('((request . "verify connection"))\n')
    vim.wait(2000, function () return seen ~= nil end, 10)
    assert.is_truthy(seen)
    assert.is_truthy(seen:find('pong', 1, true))
  end)

  it('handles a reply split across many small chunks', function ()
    local payload =
      '((response-type verify-connection) (content "chunked ñ"))'
    local message = framed(payload)
    server = fake_server(function (_line, respond)
      -- Send one byte at a time: worst-case reassembly.
      for i = 1, #message do respond(message:sub(i, i)) end
    end)
    client.port = server.port
    local seen = nil
    state.register_response_handler('verify-connection',
      function (payload_text) seen = payload_text end, true)
    client.send_string('((request . "verify connection"))\n')
    vim.wait(2000, function () return seen ~= nil end, 10)
    assert.are.equal(payload, seen)
  end)

  it('tears down streams and handlers on busy-initializing',
     function ()
    server = fake_server(function (_line, respond)
      respond('((busy-initializing . "still loading the graph"))\n')
    end, false) -- a busy server answers the warnings request so too
    client.port = server.port
    local reset_ran = false
    table.insert(state.connection_reset_hooks,
                 function () reset_ran = true end)
    state.register_response_handler('verify-connection',
      function () end, true)
    client.send_string('((request . "verify connection"))\n')
    vim.wait(2000, function () return reset_ran end, 10)
    assert.is_true(reset_ran)
    assert.are.same({}, state.response_handler_map)
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
    assert.are.same({}, state.response_handler_map)
    assert.is_nil(state.tcp)
  end)

  it('is idempotent while a live connection exists', function ()
    server = fake_server(function () end)
    client.port = server.port
    local first = client.connect()
    local second = client.connect()
    assert.are.equal(first, second)
  end)

  it('keeps a live connection when init names the same port', function ()
    server = fake_server(function () end)
    client.port = server.port
    local tcp = client.connect()
    client.end_connection_to_another_port(server.port)
    assert.are.equal(tcp, state.tcp)
  end)

  it('ends a live connection to another port when no skg buffer is open',
     function ()
    helpers.wipe_skg_buffers()
    server = fake_server(function () end)
    client.port = server.port
    client.connect()
    client.end_connection_to_another_port(server.port + 1)
    assert.is_nil(state.tcp)
  end)

  it('refuses another port while skg buffers are open', function ()
    server = fake_server(function () end)
    client.port = server.port
    local tcp = client.connect()
    local view = vim.api.nvim_create_buf(true, false)
    vim.b[view].skg_view_id = 'test-view'
    local ok, message = pcall(client.end_connection_to_another_port,
                              server.port + 1)
    vim.api.nvim_buf_delete(view, { force = true })
    assert.is_false(ok)
    assert.is_truthy(message:find('SkgCloseAllSkgBuffers', 1, true))
    assert.are.equal(tcp, state.tcp)
  end)

  describe('server warnings', function ()
    local server_warnings = require('skg.server_warnings')

    local function warnings_buffer ()
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_get_name(buf) == server_warnings.buffer_name then
          return buf end
      end
      return nil
    end

    local function wipe_warnings_buffer ()
      local buf = warnings_buffer()
      if buf then vim.api.nvim_buf_delete(buf, { force = true }) end
    end

    before_each(wipe_warnings_buffer)
    after_each(wipe_warnings_buffer)

    it('a new connection asks once, and a non-empty answer opens the'
       .. ' buffer', function ()
      server = fake_server(function () end,
        { "skgconfig.toml sets 'max_role_tree_depth', a key skg does not"
          .. " know, so it has no effect. Its current spelling is"
          .. " 'max_roleTree_depth'." })
      client.port = server.port
      client.connect()
      vim.wait(2000, function () return warnings_buffer() ~= nil end, 10)
      local buf = warnings_buffer()
      assert.is_truthy(buf)
      local text = table.concat(
        vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
      assert.is_truthy(text:find(
        "* skgconfig.toml sets 'max_role_tree_depth'", 1, true))
      assert.is_truthy(text:find("'max_roleTree_depth'", 1, true))
    end)

    it('an empty answer opens nothing', function ()
      server = fake_server(function () end)
      client.port = server.port
      client.connect()
      assert.is_truthy(state.response_handler_map['server-warnings'])
      vim.wait(2000, function ()
        return state.response_handler_map['server-warnings'] == nil end, 10)
      assert.is_nil(state.response_handler_map['server-warnings'])
      assert.is_nil(warnings_buffer())
    end)
  end)
end)
