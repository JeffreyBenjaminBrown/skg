local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local buffer = require('skg.buffer')
local client = require('skg.client')
local query_wait = require('skg.query_wait')
local state = require('skg.state')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
pcall(function () require('orgmode').setup({}) end)

local function f (name, value)
  return { sexpr.symbol(name), value }
end

local function raw_text (buf)
  local text = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
  return vim.bo[buf].endofline and text .. '\n' or text
end

describe('skg query waits', function ()
  local old_submit
  local old_connect
  local old_open
  local old_record

  before_each(function ()
    state.query_waits = {}
    state.rebuilding = true
    state.server_session_id = helpers.server_session_id
    state.connection_handshake_state = 'verified'
    state.maintenance_client_incident = {
      incident_id = 'incident-1', epoch = 7,
    }
    state.active_source_set_name = 'all'
    old_submit, old_connect = client.submit_request, client.connect
    old_open = buffer.open_org_buffer_from_text
    old_record = registry.record
    client.connect = function () return state.tcp end
    client.submit_request = function (wire)
      state.last_query_wait_wire = wire
      return 'transport-1'
    end
  end)

  after_each(function ()
    client.submit_request, client.connect = old_submit, old_connect
    buffer.open_org_buffer_from_text = old_open
    registry.record = old_record
    state.query_waits = {}
    state.maintenance_client_incident = nil
    state.rebuilding = false
    state.graph_transition_status = nil
    state.graph_write_admission = nil
    helpers.wipe_skg_buffers()
  end)

  it('uses an operation UUID separate from transport request identity', function ()
    local fake_buf = vim.api.nvim_create_buf(true, false)
    local placeholder_args
    registry.record = function (buf)
      if buf == fake_buf then
        return {
          id = 'buffer-1', view_uri = 'search:wait:query-1',
          server_session_id = state.server_session_id,
          application_token = 1, graph_generation = 1,
          presentation_generation = 0, server_revision = 1,
          source_set = 'all', last_fetched_sha256 = string.rep('a', 64),
        }
      end
      return old_record(buf)
    end
    buffer.open_org_buffer_from_text = function (text, name, view_uri, options)
      placeholder_args = { text = text, name = name, view_uri = view_uri,
                           options = options }
      return fake_buf
    end
    local id = query_wait.submit('apple', false, false, false, nil,
                                 'query-1')
    assert.are.equal('query-1', id)
    assert.is_truthy(state.last_query_wait_wire:find('query%-operation%-id'))
    assert.is_truthy(state.last_query_wait_wire:find('incident%-id'))
    assert.is_truthy(state.last_query_wait_wire:find('maintenance%-epoch'))
    assert.is_truthy(state.last_query_wait_wire:find('base%-content%-sha256'))
    assert.is_falsy(state.last_query_wait_wire:find('request%-id'))
    assert.is_falsy(placeholder_args.text:find('query-1', 1, true))
    assert.is_falsy(placeholder_args.name:find('query-1', 1, true))
    assert.are.equal('search:wait:query-1', placeholder_args.view_uri)
    vim.api.nvim_buf_delete(fake_buf, { force = true })
  end)

  it('chooses current outside a rebuilding state', function ()
    state.rebuilding = false
    state.maintenance_client_incident = nil
    state.graph_transition_status = 'idle'
    state.graph_write_admission = 'open'
    assert.are.equal('current', query_wait.policy_choice())
  end)

  it('offers a wait during any coordinated reconciliation phase', function ()
    local old_confirm = vim.fn.confirm
    vim.fn.confirm = function () return 2 end
    state.rebuilding = false
    state.graph_transition_status = 'preparing'
    assert.are.equal('wait', query_wait.policy_choice())
    state.graph_transition_status = 'idle'
    state.graph_write_admission = 'closed'
    assert.are.equal('wait', query_wait.policy_choice())
    vim.fn.confirm = old_confirm
  end)

  it('does not select an older report-only incident as a target', function ()
    state.maintenance_client_incident = nil
    state.pending_incidents = {
      { { 'incident-id', 'old' }, { 'maintenance-epoch', 4 },
        { 'phase', 'active' } },
    }
    assert.has_error(function ()
      query_wait.submit('apple', false, false, false, nil, 'query-old')
    end)
  end)

  it('applies an exact read-only result and ACKs duplicate delivery', function ()
    local sent = {}
    client.submit_request = function (wire) table.insert(sent, wire) end
    local id = query_wait.submit('apple', false, false, false, nil,
                                 'query-result')
    local record = state.query_waits[id]
    local before = registry.record(record.buffer)
    local content = '* result\nexact\n'
    local digest = vim.fn.sha256(content)
    local response_text = string.format(
      '((response-type query-wait-result) (query-operation-id %q)'
      .. ' (server-session-id %q) (view-write-authority read-only)'
      .. ' (view-uri %q) (client-buffer-id %q)'
      .. ' (expected-client-application-token %d)'
      .. ' (resulting-client-application-token %d)'
      .. ' (query-recipe-digest %q) (base-content-sha256 %q)'
      .. ' (expected-graph-generation %d)'
      .. ' (expected-presentation-generation %d)'
      .. ' (expected-server-revision %d)'
      .. ' (graph-generation %d) (presentation-generation %d)'
      .. ' (server-revision %d) (source-set %q) (freshness current)'
      .. ' (result-digest %q) (content %q))',
      id, state.server_session_id, before.view_uri, before.id,
      before.application_token, before.application_token + 1,
      record.recipe_digest, before.last_fetched_sha256,
      before.graph_generation, before.presentation_generation,
      before.server_revision, before.graph_generation + 1,
      before.presentation_generation + 1, before.server_revision + 1,
      before.source_set, digest, content)
    local response = sexpr.read(response_text)
    query_wait.result_handler('', response)
    assert.are.equal('delivered', record.status)
    assert.are.equal(content, raw_text(record.buffer))
    local ack_count = #sent
    query_wait.result_handler('', response)
    assert.is_true(#sent > ack_count)
    assert.are.equal(content, raw_text(record.buffer))
  end)

  it('rejects stale destination and result content digest', function ()
    local sent = {}
    client.submit_request = function (wire) table.insert(sent, wire) end
    local id = query_wait.submit('apple', false, false, false, nil,
                                 'query-reject')
    local record = state.query_waits[id]
    local current = registry.record(record.buffer)
    local response_text = string.format(
      '((response-type query-wait-result) (query-operation-id %q)'
      .. ' (server-session-id %q) (view-write-authority read-only)'
      .. ' (view-uri %q) (client-buffer-id %q)'
      .. ' (expected-client-application-token %d)'
      .. ' (resulting-client-application-token %d)'
      .. ' (query-recipe-digest %q) (base-content-sha256 %q)'
      .. ' (expected-graph-generation %d)'
      .. ' (expected-presentation-generation %d)'
      .. ' (expected-server-revision %d)'
      .. ' (graph-generation %d) (presentation-generation %d)'
      .. ' (server-revision %d) (source-set %q) (freshness current)'
      .. ' (result-digest %q) (content %q))',
      id, state.server_session_id, current.view_uri .. '-stale', current.id,
      current.application_token, current.application_token + 1,
      record.recipe_digest, string.rep('0', 64), current.graph_generation,
      current.presentation_generation, current.server_revision,
      current.graph_generation + 1, current.presentation_generation + 1,
      current.server_revision + 1,
      current.source_set, string.rep('a', 64), '* changed\n')
    query_wait.result_handler('', sexpr.read(response_text))
    assert.are.equal('destination-rejected', record.status)
    assert.is_truthy(record.reason:find('view%-uri'))
    local rejected_digest = response_text:gsub(
      '%-stale', '', 1):gsub(
      '%(base%-content%-sha256 "' .. string.rep('0', 64) .. '"%)',
      '(base-content-sha256 "' .. current.last_fetched_sha256 .. '")')
    assert.is_truthy(rejected_digest:find(current.view_uri, 1, true))
    record.status, record.reason = 'pending', nil
    query_wait.result_handler('', sexpr.read(rejected_digest))
    assert.are.equal('destination-rejected', record.status)
    assert.are.equal('result digest does not match content', record.reason)
  end)

  it('ingests restart summaries without destinations and verifies status recipes',
     function ()
    state.owner_publication_revision = nil
    state.update_global_server_status({
      f('server-session-id', state.server_session_id),
      f('owner-publication-revision', 1),
      f('pending-query-waits', {
        { f('query-operation-id', 'restart-ready'), f('status', 'ready') },
        { f('query-operation-id', 'restart-blocked'), f('status', 'blocked') },
      }),
    })
    assert.are.equal('ready', state.query_waits['restart-ready'].status)
    assert.is_nil(state.query_waits['restart-ready'].buffer)

    local old_open = buffer.open_org_buffer_from_text
    local old_register = state.register_response_handler
    local wire, handler
    buffer.open_org_buffer_from_text = function ()
      error('restart recovery must not create a destination') end
    state.register_response_handler = function (_, callback)
      handler = callback end
    client.submit_request = function (request) wire = request end
    query_wait.recover('restart-ready')
    assert.is_truthy(wire:find('query wait status', 1, true))
    assert.is_truthy(wire:find('query-operation-id', 1, true))
    assert.is_truthy(wire:find('server-session-id', 1, true))
    assert.is_falsy(wire:find('view-uri', 1, true))
    assert.is_falsy(wire:find('client-buffer-id', 1, true))

    local recipe_text = '((kind . "text-search") (terms . "apple"))'
    local digest = vim.fn.sha256(recipe_text)
    handler('', sexpr.read(string.format(
      '((status ready) (server-session-id %q) (query-recipe %q)'
      .. ' (query-recipe-digest %q))', state.server_session_id,
      recipe_text, digest)))
    assert.are.equal(recipe_text,
                     state.query_waits['restart-ready'].recipe_text)
    assert.are.equal(digest,
                     state.query_waits['restart-ready'].recipe_digest)

    assert.has_error(function ()
      handler('', sexpr.read(string.format(
        '((status ready) (server-session-id %q) (query-recipe %q)'
        .. ' (query-recipe-digest %q))', state.server_session_id,
        recipe_text, string.rep('a', 64)))) end)
    assert.is_nil(state.query_waits['restart-blocked'].recipe_text)
    buffer.open_org_buffer_from_text = old_open
    state.register_response_handler = old_register
  end)

  it('accepts unsolicited failed status for a coarse record once', function ()
    local notifications = 0
    local old_notify = vim.notify
    vim.notify = function () notifications = notifications + 1 end
    local response = sexpr.read(string.format(
      '((server-session-id %q) (query-operation-id "push-failed")'
      .. ' (status failed) (reason "recipe rejected"))',
      state.server_session_id))
    query_wait.status_push_handler('', response)
    query_wait.status_push_handler('', response)
    local record = state.query_waits['push-failed']
    assert.are.equal('failed', record.status)
    assert.is_nil(record.buffer)
    assert.are.equal(1, notifications)
    vim.notify = old_notify
  end)

end)
