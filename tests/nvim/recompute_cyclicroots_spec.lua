local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local client = require('skg.client')
local misc_requests = require('skg.misc_requests')
local state = require('skg.state')

describe('skg recompute cyclic roots command', function ()
  it('sends a fresh operation UUID and verified session on each invocation',
     function ()
    local old_ensure = misc_requests.ensure_connection_handshake
    local old_connect = client.connect
    local old_register = state.register_response_handler
    local old_submit = client.submit_request
    local seen = {}
    state.server_session_id = helpers.server_session_id
    misc_requests.ensure_connection_handshake = function () return true end
    client.connect = function () end
    state.register_response_handler = function () end
    client.submit_request = function (wire) table.insert(seen, wire) end

    misc_requests.recompute_cyclicroots()
    misc_requests.recompute_cyclicroots()

    assert.are.equal(2, #seen)
    local first = seen[1]:match('operation%-id%s+%.%s+"([^"]+)"')
    local second = seen[2]:match('operation%-id%s+%.%s+"([^"]+)"')
    assert.is_truthy(first)
    assert.is_truthy(second)
    assert.is_truthy(seen[1]:find('recompute cyclic roots', 1, true))
    assert.is_truthy(seen[1]:find('server%-session%-id'))
    assert.is_truthy(seen[1]:find(helpers.server_session_id, 1, true))
    assert.is_truthy(first:match('^%x%x%x%x%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%x%x%x%x%x%x%x%x$'))
    assert.are_not.equal(first, second)

    client.submit_request = old_submit
    state.register_response_handler = old_register
    client.connect = old_connect
    misc_requests.ensure_connection_handshake = old_ensure
  end)

  it('refuses before verification without registering or submitting',
     function ()
    local old_ensure = misc_requests.ensure_connection_handshake
    local old_connect = client.connect
    local old_register = state.register_response_handler
    local old_submit = client.submit_request
    local registered, submitted = false, false
    misc_requests.ensure_connection_handshake = function () return false end
    client.connect = function () end
    state.register_response_handler = function () registered = true end
    client.submit_request = function () submitted = true end

    local ok = pcall(misc_requests.recompute_cyclicroots)
    assert.is_false(ok)
    assert.is_false(registered)
    assert.is_false(submitted)

    client.submit_request = old_submit
    state.register_response_handler = old_register
    client.connect = old_connect
    misc_requests.ensure_connection_handshake = old_ensure
  end)
end)
