local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local config = require('skg.config')
local misc = require('skg.misc_requests')
local registry = require('skg.buffer_registry')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')
local client = require('skg.client')

local old_session = '11111111-2222-4333-8444-555555555555'
local new_session = 'aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee'

local function f (name, value)
  return { sexpr.symbol(name), value }
end

local function handshake (session_id, version)
  return {
    f('protocol-version', version or 2),
    f('server-session-id', session_id),
    f('source-inventory', {}), f('active-source-set', 'all'),
    f('maintenance-archive-folder', 'archive'),
    f('maintenance-archive-identity', '/tmp/archive'),
    f('maintenance-epoch', 0), f('maintenance-state', 'idle'),
    f('census-required', 'true'), f('graph-generation', 7),
    f('manifest-revision', 9), f('typedb-health', 'healthy'),
    f('tantivy-health', 'healthy'), f('content', 'connected'),
    f('current-graph-generation', 7), f('current-manifest-revision', 9),
    f('graph-write-admission', 'open'),
    f('graph-transition-status', 'idle'), f('rebuilding', 'nil'),
    f('pending-incidents', {}),
  }
end

local function wipe ()
  helpers.wipe_skg_buffers()
  helpers.reset_client_state()
  state.server_session_id = nil
  state.connection_handshake_state = nil
end

describe('skg protocol-v2 server sessions', function ()
  before_each(wipe)
  after_each(wipe)

  it('rejects a protocol mismatch before touching buffer text or undo',
     function ()
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'authored text' })
    vim.bo[buf].modified = true
    local before = registry.raw_text(buf)
    local undo_before = vim.api.nvim_buf_call(buf, vim.fn.undotree).seq_cur
    local ok = pcall(misc.install_connection_verification,
                     nil, handshake(new_session, 1), {})
    assert.is_false(ok)
    assert.are.equal(before, registry.raw_text(buf))
    assert.are.equal(undo_before,
      vim.api.nvim_buf_call(buf, vim.fn.undotree).seq_cur)
    assert.is_nil(state.server_session_id)
    assert.are.equal('failed', state.connection_handshake_state)
  end)

  it('keeps old origin through equal counters and stale census detaches it',
     function ()
    state.server_session_id = old_session
    state.active_source_set_name = 'all'
    config.store_state = { graph_generation = 7 }
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                              { 'old live view', 'local edit' })
    vim.bo[buf].modified = false
    registry.register(buf, 'content-view', {
      lifecycle = 'live-view', disposable = false, view_uri = 'old-view',
      server_session_id = old_session, graph_generation = 7,
      server_revision = 4, application_token = 3,
    })
    vim.api.nvim_buf_set_lines(buf, -1, -1, false, { 'preserved' })
    local text_before = registry.raw_text(buf)
    local undo_before = vim.api.nvim_buf_call(buf, vim.fn.undotree).seq_cur
    local census
    local real_submit = misc.submit_buffer_census
    misc.submit_buffer_census = function () census = registry.census() end
    state.server_session_id = nil
    misc.install_connection_verification(nil, handshake(new_session), {})
    misc.submit_buffer_census = real_submit
    assert.are.equal(old_session, census[1].server_session_id)
    require('skg.maintenance').handle_census_stale({ census[1].buffer_id })
    assert.are.equal(text_before, registry.raw_text(buf))
    assert.are.equal(undo_before,
      vim.api.nvim_buf_call(buf, vim.fn.undotree).seq_cur)
    assert.is_nil(registry.record(buf).view_uri)
    assert.are.equal(old_session, registry.record(buf).server_session_id)
  end)

  it('rejects a delayed old-session redraw before changing text or undo',
     function ()
    state.server_session_id = old_session
    config.store_state = { graph_generation = 7 }
    local buf = buffer.open_org_buffer_from_text(
      'keep local edit', 'skg://stale-reply', 'view', {
        server_session_id = old_session, graph_generation = 7,
        server_revision = 4, application_token = 3,
      })
    vim.api.nvim_buf_set_lines(buf, -1, -1, false, { 'more editing' })
    local text_before = registry.raw_text(buf)
    local undo_before = vim.api.nvim_buf_call(buf, vim.fn.undotree).seq_cur
    state.server_session_id = new_session
    local ok = pcall(save.replace_buffer_with_new_content,
      buf, 'stale server text', nil, {
        server_session_id = old_session, application_token = 4,
      })
    assert.is_false(ok)
    assert.are.equal(text_before, registry.raw_text(buf))
    assert.are.equal(undo_before,
      vim.api.nvim_buf_call(buf, vim.fn.undotree).seq_cur)
  end)

  it('binds the origin session before the final save fingerprint', function ()
    local line = save.save_request_string('view', {
      lines_below_focused_headline = 0, column = 0,
      screen_lines_below_window_start = 0,
    }, nil, nil, nil, nil, {
      id = 'buffer', kind = 'content-view', graph_generation = 7,
      server_revision = 4, application_token = 3,
      server_session_id = old_session,
    }, 'operation', 'fingerprint')
    assert.is_truthy(line:find(
      '(server-session-id . "' .. old_session .. '")'
      .. ' (request-base-fingerprint . "fingerprint")', 1, true))
  end)

  it('advertises v2 and stamps ordinary frames after verification', function ()
    local seen
    local server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('(request . "probe")', 1, true) then
        seen = line
        respond(helpers.framed('((response-type probe))'))
      end
    end)
    client.connect()
    assert.is_true(vim.wait(2000, function ()
      return state.connection_handshake_state == 'verified' end, 10))
    state.register_response_handler('probe', function () end, true)
    client.submit_request('((request . "probe"))\n')
    assert.is_true(vim.wait(2000, function () return seen ~= nil end, 10))
    assert.is_truthy(helpers.last_handshake_line:find(
      '(protocol-version . 2)', 1, true))
    assert.is_truthy(seen:find(
      '(server-session-id . "' .. helpers.server_session_id .. '")',
      1, true))
    server.close()
  end)

  it('holds editable admission closed until authoritative publication reopens it',
     function ()
    state.server_session_id = new_session
    state.graph_write_admission = 'open'
    state.client_constructor_admission = 'closed'
    assert.are.equal('read-only', state.requested_view_write_authority())
    assert.has_error(function ()
      state.view_write_authority_from_response({
        f('view-write-authority', 'editable'),
      })
    end)
    state.update_global_server_status({
      f('server-session-id', new_session),
      f('current-graph-generation', 8),
      f('current-manifest-revision', 10),
      f('graph-write-admission', 'open'),
      f('graph-transition-status', 'idle'), f('rebuilding', 'nil'),
      f('pending-incidents', {}),
    })
    assert.are.equal('open', state.client_constructor_admission)
    assert.are.equal('editable', state.requested_view_write_authority())
  end)
end)
