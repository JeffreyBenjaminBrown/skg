local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local client = require('skg.client')
local misc_requests = require('skg.misc_requests')
local state = require('skg.state')

describe('skg strip command', function ()
  it('sends a fresh operation UUID and verified session on each invocation',
  function ()
    local old_misc_connect = misc_requests.ensure_connection_handshake
    local old_client_connect = client.connect
    local old_response_handler = state.register_response_handler
    local old_submit = client.submit_request
    local seen = {}
    state.server_session_id = helpers.server_session_id
    misc_requests.ensure_connection_handshake = function () return true end
    client.connect = function () return state.tcp end
    state.register_response_handler = function () end
    client.submit_request = function (wire) table.insert(seen, wire) end
    misc_requests.strip_body_whitespace()
    misc_requests.strip_body_whitespace()
    assert.are.equal(2, #seen)
    assert.is_truthy(seen[1]:find('operation%-id'))
    assert.is_truthy(seen[1]:find('server%-session%-id'))
    assert.is_truthy(seen[1]:find(helpers.server_session_id, 1, true))
    local first = seen[1]:match('operation%-id%s+%.%s+"([^"]+)"')
    local second = seen[2]:match('operation%-id%s+%.%s+"([^"]+)"')
    assert.is_truthy(first)
    assert.is_truthy(second)
    assert.are_not.equal(first, second)
    client.submit_request = old_submit
    state.register_response_handler = old_response_handler
    client.connect = old_client_connect
    misc_requests.ensure_connection_handshake = old_misc_connect
  end)
end)
