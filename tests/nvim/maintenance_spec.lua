local maintenance = require('skg.maintenance')
local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local incident_id = '12345678-1234-4234-8234-123456789abc'

local function f (name, value)
  return { sexpr.symbol(name), value }
end

local function settlement (buffer_id, requirement, acknowledged)
  return {
    f('buffer-id', buffer_id), f('buffer-key', 'none'),
    f('kind', 'content-view'), f('view-uri', 'view'),
    f('dirty', 'nil'), f('impacted', 'true'),
    f('parse-uncertain', 'nil'), f('observed-ids', {}),
    f('resolved-primary-ids', {}), f('base-graph-generation', 1),
    f('base-presentation-generation', 3),
    f('base-server-revision', 4), f('base-application-token', 7),
    f('planned-disposition', 'retained-clean'),
    f('required-ack', requirement),
    f('acknowledged', acknowledged and 'true' or 'nil'),
  }
end

local function new_buffer ()
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '* Original' })
  vim.bo[buf].endofline = true
  vim.bo[buf].modified = false
  vim.b[buf].skg_view_uri = 'view'
  registry.register(buf, 'content-view', {
    last_fetched = '* Original\n', graph_generation = 1,
    presentation_generation = 3, server_revision = 4,
    application_token = 7,
  })
  registry.lock_for_maintenance(buf, 9)
  return buf
end

local function reset ()
  state.maintenance_client_incident = nil
  state.pending_maintenance_offer = nil
  state.maintenance_state = nil
  state.connection_handshake_state = nil
  state.request_draft = nil
  for _, buf in ipairs(registry.buffers()) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.bo[buf].modifiable = true
      vim.bo[buf].modified = false
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
end

describe('skg Neovim maintenance handshake', function ()
  local original_defer
  local original_archive_finalize
  local original_client_submit

  before_each(function ()
    reset()
    original_defer = maintenance.defer
    original_archive_finalize = require('skg.recovery_archive').finalize
    original_client_submit = require('skg.client').submit_request
  end)

  after_each(function ()
    maintenance.defer = original_defer
    require('skg.recovery_archive').finalize = original_archive_finalize
    require('skg.client').submit_request = original_client_submit
    reset()
  end)

  it('requires the exact settlement inventory', function ()
    local one = settlement('one', 'release-ack', false)
    local two = settlement('two', 'release-ack', false)
    assert.are.same({ one, two },
      maintenance.validate_settlements({ one, two }, { 'two', 'one' }))
    assert.has_error(function ()
      maintenance.validate_settlements({ one, one }, { 'one', 'two' }) end)
    assert.has_error(function ()
      maintenance.validate_settlements({ one }, { 'one', 'two' }) end)
  end)

  it('echoes every base and rendered-result authority in an application ACK',
     function ()
    local record = settlement('one', 'application-ack', false)
    table.insert(record, f('application', {
      f('content', '* Rendered\n'),
      f('content-sha256', string.rep('a', 64)),
      f('resulting-graph-generation', 2),
      f('resulting-presentation-generation', 8),
      f('resulting-server-revision', 5),
      f('resulting-application-token', 8),
    }))
    local fields = maintenance.ack_fields(record)
    local wire = {}
    for _, entry in ipairs(fields) do wire[entry[1]] = entry[2] end
    for _, key in ipairs({
      'base-graph-generation', 'base-presentation-generation',
      'base-server-revision', 'base-application-token', 'content-sha256',
      'resulting-graph-generation', 'resulting-presentation-generation',
      'resulting-server-revision', 'resulting-application-token',
    }) do assert.is_not_nil(wire[key]) end
  end)

  it('uses status acknowledgement bits without repeating local actions',
     function ()
    local one = settlement('one', 'release-ack', false)
    local two = settlement('two', 'release-ack', true)
    state.maintenance_client_incident = {
      registered_buffer_ids = { 'one', 'two' },
      settlements = {
        settlement('one', 'release-ack', false),
        settlement('two', 'release-ack', false),
      },
      locally_applied = { two = true },
    }
    local scheduled
    local settle_runs = 0
    local real_settle = maintenance.settle_next
    maintenance.settle_next = function () settle_runs = settle_runs + 1 end
    maintenance.defer = function (callback) scheduled = callback end
    maintenance.install_settlements({ one, two })
    scheduled()
    maintenance.settle_next = real_settle
    local incident = state.maintenance_client_incident
    assert.are.equal('one', payload.field_text(
      incident.pending_settlements[1], 'buffer-id'))
    assert.are.equal('two', payload.field_text(
      incident.acknowledged_settlements[1], 'buffer-id'))
    assert.are.equal(1, settle_runs)
    incident.locally_applied = {}
    assert.has_error(function ()
      maintenance.install_settlements({ one, two }) end)
  end)

  it('retries an ACK without reapplying the local transition', function ()
    local record = settlement('one', 'release-ack', false)
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9, phase = 'settling-views',
      pending_settlements = { record }, locally_applied = { one = true },
    }
    local applied, sent = 0, nil
    local real_apply = maintenance.apply_settlement
    local real_send = maintenance.send_settlement_ack
    maintenance.apply_settlement = function () applied = applied + 1 end
    maintenance.send_settlement_ack = function (value) sent = value end
    maintenance.settle_next()
    maintenance.apply_settlement = real_apply
    maintenance.send_settlement_ack = real_send
    assert.are.equal(0, applied)
    assert.are.equal(record, sent)
    assert.are.equal(record,
      state.maintenance_client_incident.in_flight_settlement)
  end)

  it('unlocks only the exact terminal census and waits for its ACK',
     function ()
    local buf = new_buffer()
    local buffer_id = registry.record(buf).id
    local manifest = string.rep('a', 64)
    state.maintenance_state = { epoch = 9, state = 'active' }
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9, phase = 'completing',
      registered_buffer_ids = { buffer_id },
      g1_graph_generation = 2, g1_manifest_revision = 6,
      final_archive = { manifest_sha256 = manifest, path = '/archive' },
    }
    local scheduled
    local terminal_ack_runs = 0
    local real_terminal_ack = maintenance.send_terminal_ack
    maintenance.send_terminal_ack = function ()
      terminal_ack_runs = terminal_ack_runs + 1 end
    maintenance.defer = function (callback) scheduled = callback end
    maintenance.handle_terminal(nil, {
      f('status', 'terminal'), f('incident-id', incident_id),
      f('maintenance-epoch', 9), f('disposition', 'completed'),
      f('manifest-sha256', manifest),
      f('unlock-buffer-ids', { buffer_id }),
      f('selected-graph-generation', 2),
      f('selected-manifest-revision', 6),
    })
    scheduled()
    maintenance.send_terminal_ack = real_terminal_ack
    assert.is_nil(registry.record(buf).maintenance_epoch)
    assert.are.equal('terminal-received',
      state.maintenance_client_incident.phase)
    assert.are.equal('terminal', state.maintenance_state.state)
    assert.are.equal(1, terminal_ack_runs)
  end)

  it('finishes the exact evidence and terminal acknowledgement chain',
     function ()
    local archive_module = require('skg.recovery_archive')
    local client = require('skg.client')
    local manifest = string.rep('a', 64)
    local transfer = string.rep('b', 64)
    local artifact = string.rep('c', 64)
    local requests = {}
    archive_module.finalize = function ()
      return {
        manifest_sha256 = manifest,
        transfer_manifest_sha256 = transfer,
        artifact_bytes_sha256 = artifact,
        path = '/archive',
      }
    end
    client.submit_request = function (wire, _content, request_incident_id)
      table.insert(requests, {
        wire = wire, incident_id = request_incident_id,
      })
    end
    maintenance.defer = function (callback) callback() end
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9, phase = 'requesting-evidence',
      registered_buffer_ids = {}, g1_graph_generation = 2,
      g1_manifest_revision = 6,
      server_evidence_sha256 = string.rep('d', 64),
      archive = {}, settlements = {},
    }

    maintenance.handle_evidence(nil, {
      f('incident-id', incident_id), f('maintenance-epoch', 9),
    }, 'opaque evidence')
    assert.matches('maintenance archive finalized', requests[1].wire,
      1, true)
    assert.are.equal(incident_id, requests[1].incident_id)

    maintenance.handle_final_archive_ack(nil, {
      f('status', 'archive-finalized'), f('manifest-sha256', manifest),
      f('transfer-manifest-sha256', transfer),
      f('artifact-bytes-sha256', artifact),
    })
    assert.matches('complete maintenance', requests[2].wire, 1, true)

    maintenance.handle_terminal(nil, {
      f('status', 'terminal'), f('incident-id', incident_id),
      f('maintenance-epoch', 9), f('disposition', 'completed'),
      f('manifest-sha256', manifest), f('unlock-buffer-ids', {}),
      f('selected-graph-generation', 2),
      f('selected-manifest-revision', 6),
    })
    assert.matches('acknowledge terminal maintenance', requests[3].wire,
      1, true)

    maintenance.handle_terminal_ack(nil, { f('status', 'idle') })
    assert.is_nil(state.maintenance_client_incident)
    assert.are.equal('idle', state.maintenance_state.state)
  end)

  it('does not detach active settlement debt during reconnect census',
     function ()
    local buf = new_buffer()
    local buffer_id = registry.record(buf).id
    state.maintenance_state = { epoch = 9, state = 'active' }
    state.maintenance_client_incident = {
      epoch = 9, registered_buffer_ids = { buffer_id },
    }
    maintenance.handle_census_stale({ buffer_id })
    assert.are.equal('view', registry.record(buf).view_uri)
  end)

  it('adopts the handshake epoch before submitting reconnect census',
     function ()
    local buf = new_buffer()
    registry.unlock_after_maintenance(buf, 9)
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9,
      registered_buffer_ids = { registry.record(buf).id },
    }
    local misc = require('skg.misc_requests')
    local real_submit = misc.submit_buffer_census
    local census_submitted = false
    misc.submit_buffer_census = function () census_submitted = true end
    misc.install_connection_verification(nil, {
      f('source-inventory', {}), f('active-source-set', 'all'),
      f('maintenance-archive-folder', 'archive'),
      f('maintenance-archive-identity', '/archive'),
      f('maintenance-epoch', 9), f('maintenance-state', 'active'),
      f('census-required', 'true'), f('graph-generation', 1),
      f('manifest-revision', 2), f('typedb-health', 'healthy'),
      f('tantivy-health', 'healthy'), f('content', 'connected'),
    }, {})
    misc.submit_buffer_census = real_submit
    assert.is_true(census_submitted)
    assert.are.equal('active', state.maintenance_state.state)
    assert.are.equal(9, registry.record(buf).maintenance_epoch)
    assert.is_false(vim.bo[buf].modifiable)
  end)

  it('resumes maintenance only after a completed reconnect census',
     function ()
    local misc = require('skg.misc_requests')
    local real_stale = maintenance.handle_census_stale
    local real_resume = maintenance.resume_after_census
    local stale, resumed
    maintenance.handle_census_stale = function (ids) stale = ids end
    maintenance.resume_after_census = function () resumed = true end
    misc.handle_buffer_census_response({}, {
      f('text-required-buffer-ids', {}),
      f('stale-buffer-ids', { 'protected' }),
      f('census-complete', 'true'),
    })
    vim.wait(200, function () return resumed == true end, 10)
    maintenance.handle_census_stale = real_stale
    maintenance.resume_after_census = real_resume
    assert.are.same({ 'protected' }, stale)
    assert.is_true(resumed)
    assert.are.equal('verified', state.connection_handshake_state)
  end)
end)
