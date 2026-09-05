local maintenance = require('skg.maintenance')
local config = require('skg.config')
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

local function replace_field (record, name, value)
  for _, entry in ipairs(record) do
    if sexpr.is_list(entry) and sexpr.atom_text(entry[1]) == name then
      entry[2] = value
      return
    end
  end
  error('missing test field ' .. name)
end

local function bootstrap_response (status, buffer_ids)
  local response = {
    f('status', status),
    f('allocated-incident-id', incident_id), f('maintenance-epoch', 9),
    f('requested-paths', {}), f('requested-ids', {}),
    f('origin', 'pull'), f('started-at-utc', 'now'),
    f('archive-directory-name', 'archive'), f('source-set', 'all'),
    f('g0-graph-generation', 1), f('g0-manifest-revision', 2),
  }
  if buffer_ids then
    table.insert(response, f('registered-buffer-ids', buffer_ids))
    table.insert(response, f('lock-census-sha256',
      maintenance.lock_census_sha256(buffer_ids)))
  end
  return response
end

local function new_buffer ()
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '* Original' })
  vim.bo[buf].endofline = true
  vim.bo[buf].modified = false
  vim.b[buf].skg_view_uri = 'view'
  registry.register(buf, 'content-view', {
    lifecycle = 'live-view', disposable = false,
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
  config.source_inventory = nil
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
  local original_archive_inspect
  local original_client_submit
  local original_client_priority_submit
  local original_client_connect
  local original_begin

  before_each(function ()
    reset()
    original_defer = maintenance.defer
    original_archive_finalize = require('skg.recovery_archive').finalize
    original_archive_inspect = require('skg.recovery_archive').inspect
    original_client_submit = require('skg.client').submit_request
    original_client_priority_submit =
      require('skg.client').submit_priority_request
    original_client_connect = require('skg.client').connect
    original_begin = maintenance.begin
  end)

  after_each(function ()
    maintenance.defer = original_defer
    require('skg.recovery_archive').finalize = original_archive_finalize
    require('skg.recovery_archive').inspect = original_archive_inspect
    require('skg.client').submit_request = original_client_submit
    require('skg.client').submit_priority_request =
      original_client_priority_submit
    require('skg.client').connect = original_client_connect
    maintenance.begin = original_begin
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

  it('sends exact explicit targets and starts the server-owned origin worker',
     function ()
    local client_module = require('skg.client')
    local requests = {}
    client_module.submit_request = function (wire, _content, request_incident)
      table.insert(requests, { wire = wire, incident = request_incident })
    end
    maintenance.begin('explicit-partial-reload', nil,
      { 'source/A.skg' }, { 'alias', 'B' }, function () end)
    assert.matches('begin maintenance', requests[1].wire, 1, true)
    assert.matches('paths', requests[1].wire, 1, true)
    assert.matches('source/A.skg', requests[1].wire, 1, true)
    assert.matches('ids', requests[1].wire, 1, true)
    assert.matches('alias', requests[1].wire, 1, true)

    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9,
      offer = { origin = 'explicit-partial-reload' },
    }
    maintenance.run_explicit_origin(state.maintenance_client_incident)
    assert.matches('run maintenance origin', requests[2].wire, 1, true)
    assert.are.equal(incident_id, requests[2].incident)
    maintenance.handle_origin_started(nil, {
      f('status', 'origin-operation-started'),
      f('incident-id', incident_id), f('maintenance-epoch', 9),
    })
    assert.are.equal('waiting-for-origin-observation',
      state.maintenance_client_incident.phase)

    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 10,
      offer = { origin = 'full-rebuild' },
    }
    maintenance.run_explicit_origin(state.maintenance_client_incident)
    assert.matches('run maintenance origin', requests[3].wire, 1, true)
    assert.are.equal(incident_id, requests[3].incident)
  end)

  it('refuses dirty raw files before every maintenance origin', function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, 'raw-maintenance-preflight.skg')
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'pid: dirty' })
    registry.register(buf, 'raw-skg-file', {
      lifecycle = 'ordinary-file', disposable = false,
    })
    vim.bo[buf].modified = true
    local submitted = false
    require('skg.client').submit_request = function () submitted = true end
    local ok, reason = pcall(
      maintenance.begin, 'explicit-partial-reload')
    assert.is_false(ok)
    assert.matches('raw-maintenance-preflight.skg', tostring(reason), 1, true)
    assert.is_false(submitted)
  end)

  it('begins full rebuild through maintenance instead of a raw request',
     function ()
    local misc = require('skg.misc_requests')
    local arguments
    maintenance.begin = function (...)
      arguments = { ... }
    end
    assert.is_true(misc.rebuild_dbs())
    assert.are.equal('full-rebuild', arguments[1])
    assert.is_function(arguments[5])
  end)

  it('carries opaque origin context and structured bootstrap fields',
     function ()
    local client_module = require('skg.client')
    local context = { local_only = '/client/repository' }
    local key = string.rep('a', 64)
    local wire
    local census
    client_module.submit_request = function (request_wire)
      wire = request_wire
    end
    client_module.connect = function () return 'tcp' end
    local misc = require('skg.misc_requests')
    local real_census = misc.submit_buffer_census
    misc.submit_buffer_census = function (...)
      census = { ... }
    end
    maintenance.begin('pull', nil, nil, nil, nil, context, {
      { sexpr.symbol('pull-repositories'), {
        {
          sexpr.pair(sexpr.symbol('repository-key'), key),
          { sexpr.symbol('sources'), { 'one' } },
        },
      } },
    })
    local parsed = sexpr.read(wire)
    local repositories = payload.field(parsed, 'pull-repositories')
    assert.are.equal(key,
      payload.field_text(repositories[1], 'repository-key'))
    assert.are.same({ 'one' }, payload.string_list(
      payload.field(repositories[1], 'sources')))
    assert.is_nil(wire:find('/client/repository', 1, true))

    local handler = state.request_draft.handlers['maintenance-offer'].handler
    handler(nil, bootstrap_response(
      'install-maintenance-epoch-and-submit-locked-census'))
    misc.submit_buffer_census = real_census
    assert.are.equal(context,
      state.maintenance_client_incident.origin_context)
    assert.are.equal('awaiting-locked-census',
      state.maintenance_client_incident.phase)
    assert.are.equal('tcp', census[1])
    assert.are.equal(incident_id, census[2])
    assert.are.equal(9, census[3])
  end)

  it('freezes an incident-bound census before publishing the archive',
     function ()
    local client_module = require('skg.client')
    local misc = require('skg.misc_requests')
    local real_census = misc.submit_buffer_census
    local priority, published
    client_module.connect = function () return 'tcp' end
    client_module.submit_request = function () end
    client_module.submit_priority_request = function (...)
      priority = { ... }
    end
    misc.submit_buffer_census = function () end
    maintenance.begin('pull')
    local initial_handler =
      state.request_draft.handlers['maintenance-offer'].handler
    initial_handler(nil, bootstrap_response(
      'install-maintenance-epoch-and-submit-locked-census'))
    misc.submit_buffer_census = real_census

    maintenance.resume_after_census(incident_id, 9)
    assert.are.equal('tcp', priority[1])
    assert.matches('maintenance locked census', priority[2], 1, true)
    assert.matches('maintenance%-epoch', priority[2])
    assert.are.equal(incident_id, priority[5])

    local real_publish = maintenance.publish_initial
    maintenance.publish_initial = function () published = true end
    priority[3]['maintenance-offer'].handler(nil, bootstrap_response(
      'locked-census-accepted-publish-initial-archive', {}))
    maintenance.publish_initial = real_publish
    assert.is_true(published)
    assert.are.equal('preparing-archive',
      state.maintenance_client_incident.phase)
    assert.are.same({},
      state.maintenance_client_incident.registered_buffer_ids)
    assert.are.equal(maintenance.lock_census_sha256({}),
      state.maintenance_client_incident.lock_census_sha256)
  end)

  it('registers buffers born during maintenance under the active lock',
     function ()
    state.maintenance_state = { epoch = 9, state = 'active' }
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '* New view' })
    registry.register(buf, 'content-view', {
      lifecycle = 'live-view', disposable = false,
      last_fetched = '* New view\n',
    })
    assert.are.equal(9, registry.record(buf).maintenance_epoch)
    assert.is_false(vim.bo[buf].modifiable)
  end)

  it('dispatches reconnect phases through the registered origin adapter',
     function ()
    local seen
    maintenance.register_origin_operation_handler('test-origin',
      function (incident, phase, response)
        seen = { incident = incident, phase = phase, response = response }
        return true
      end)
    local response = {
      f('status', 'active'), f('active-incident-id', incident_id),
      f('maintenance-epoch', 9), f('origin', 'test-origin'),
      f('requested-paths', {}), f('requested-ids', {}),
      f('phase', 'running-external-mutation'),
    }
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9,
      offer = { origin = 'test-origin' },
    }
    maintenance.resume_active(response)
    assert.are.equal(state.maintenance_client_incident, seen.incident)
    assert.are.equal('running-external-mutation', seen.phase)
    assert.are.equal(response, seen.response)
    maintenance.origin_operation_handlers['test-origin'] = nil
  end)

  it('adopts an archive-backed incident in a replacement editor', function ()
    state.maintenance_archive_folder = '/archives'
    state.maintenance_archive_identity = '/server/archives'
    local initial_sha = string.rep('a', 64)
    require('skg.recovery_archive').inspect = function (path)
      assert.are.equal('/archives/archive', path)
      return {
        incident_id = incident_id, name = 'archive', path = path,
        status = 'archive-ready', initial_manifest_sha256 = initial_sha,
      }
    end
    maintenance.adopt_active({
      f('active-incident-id', incident_id), f('maintenance-epoch', 9),
      f('archive-status', 'archive-ready'),
      f('archive-directory-name', 'archive'),
      f('initial-manifest-sha256', initial_sha),
      f('origin', 'explicit-partial-reload'), f('started-at-utc', 'now'),
      f('source-set', 'all'), f('g0-graph-generation', 1),
      f('g0-manifest-revision', 2), f('requested-paths', { 'one.skg' }),
      f('requested-ids', { 'node' }), f('registered-buffer-ids', { 'gone' }),
    })
    local incident = state.maintenance_client_incident
    assert.is_true(incident.adopted)
    assert.are.equal(initial_sha, incident.archive.manifest_sha256)
    assert.are.same({ 'gone' }, incident.registered_buffer_ids)
    assert.are.equal('explicit-partial-reload', incident.offer.origin)
  end)

  it('dispatches an exact asynchronous candidate selection', function ()
    state.active_source_set_name = 'main'
    vim.g.skg_active_source_set_name = 'main'
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9,
      registered_buffer_ids = {}, locally_applied = {},
    }
    maintenance.defer = function () end
    maintenance.server_status_handler('', {
      f('status', 'candidate-selected'), f('incident-id', incident_id),
      f('maintenance-epoch', 9), f('g1-graph-generation', 2),
      f('g1-manifest-revision', 6), f('tantivy-generation', 4),
      f('server-evidence-sha256', string.rep('d', 64)),
      f('source-set', 'all'),
      f('maintenance-archive-folder', 'replacement-archives'),
      f('maintenance-archive-identity', '/server/replacement-archives'),
      f('source-inventory', {{
        f('name', 'replacement'), f('abbreviation', 'rep'),
        f('owned', 'true'), f('position', 0),
        f('configured-path', 'replacement-notes'),
        f('directory', '/data/replacement-notes'),
        f('directory-identity', '/data/replacement-notes'),
      }}),
      f('view-settlements', {}),
    })
    assert.are.equal('settling-views',
      state.maintenance_client_incident.phase)
    assert.are.equal(2, state.maintenance_client_incident.g1_graph_generation)
    assert.are.equal('all', state.active_source_set_name)
    assert.are.equal('replacement-archives',
      state.maintenance_archive_folder)
    assert.are.equal('replacement', config.source_inventory[1].name)
    assert.has_error(function ()
      maintenance.server_status_handler('', {
        f('status', 'candidate-selected'), f('incident-id', 'wrong'),
        f('maintenance-epoch', 9),
      })
    end)
  end)

  it('retains the exact blocked reason and recovery command', function ()
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9, phase = 'waiting-for-server',
      offer = { origin = 'pull' }, requested_paths = {}, requested_ids = {},
    }
    local notification
    local original_notify = vim.notify
    vim.notify = function (message) notification = message end
    local ok, reason = pcall(maintenance.resume_active, {
        f('status', 'active'), f('active-incident-id', incident_id),
        f('maintenance-epoch', 9), f('phase', 'blocked-store-health'),
        f('origin', 'pull'), f('requested-paths', {}),
        f('requested-ids', {}),
        f('blocking-reason', 'TypeDB is unavailable'),
      })
    vim.notify = original_notify
    assert.is_true(ok, tostring(reason))
    local incident = state.maintenance_client_incident
    assert.are.equal('server-blocked', incident.phase)
    assert.are.equal('blocked-store-health', incident.server_phase)
    assert.are.equal('TypeDB is unavailable', incident.blocking_reason)
    assert.matches('SkgRetryMaintenance', notification, 1, true)
  end)

  it('retries blocked maintenance with the exact incident envelope',
     function ()
    local client_module = require('skg.client')
    local submitted
    client_module.submit_request = function (wire, _content, incident)
      submitted = { wire = wire, incident = incident }
    end
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9, phase = 'server-blocked',
      server_phase = 'blocked-invalid-after-mutation',
      blocking_reason = 'malformed source',
    }
    maintenance.retry()
    assert.are.equal('maintenance-retry-pending',
      state.maintenance_client_incident.phase)
    assert.are.equal(incident_id, submitted.incident)
    assert.matches('retry maintenance', submitted.wire, 1, true)
    assert.matches('maintenance%-epoch %. 9', submitted.wire)
    local handler = state.request_draft.handlers['maintenance-status'].handler
    handler(nil, {
      f('status', 'maintenance-retry-queued'),
      f('incident-id', incident_id), f('maintenance-epoch', 9),
      f('recovery-mode', 'targeted'),
    })
    local incident = state.maintenance_client_incident
    assert.are.equal('waiting-for-origin-observation', incident.phase)
    assert.is_nil(incident.server_phase)
    assert.is_nil(incident.blocking_reason)
  end)

  it('retires invalid post-pull dirty work before allowing retry', function ()
    local retirement = settlement('dirty', 'retirement-ack', false)
    replace_field(retirement, 'dirty', 'true')
    replace_field(retirement, 'planned-disposition', 'interrupted')
    table.insert(retirement, f('settlement-resolution', 'pending'))
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9, phase = 'server-blocked',
      registered_buffer_ids = { 'dirty', 'clean' }, locally_applied = {},
    }
    local scheduled, applied, sent
    local real_apply = maintenance.apply_settlement
    local real_send = maintenance.send_preselection_retirement_ack
    maintenance.defer = function (callback) scheduled = callback end
    maintenance.apply_settlement = function (record) applied = record end
    maintenance.send_preselection_retirement_ack = function (record)
      sent = record end
    maintenance.install_preselection_retirements({ retirement })
    scheduled()
    assert.are.equal(retirement, applied)
    assert.are.equal(retirement, sent)
    assert.is_true(state.maintenance_client_incident.locally_applied.dirty)
    assert.has_error(function ()
      maintenance.handle_preselection_retirement_ack(nil, {
        f('status', 'view-settlement-recorded'),
        f('buffer-id', 'dirty'), f('required-ack', 'retirement-ack'),
      })
    end)
    maintenance.handle_preselection_retirement_ack(nil, {
      f('status', 'all-invalid-dirty-buffers-retired'),
      f('buffer-id', 'dirty'), f('required-ack', 'retirement-ack'),
    })
    assert.are.equal('client-acknowledged', payload.field_text(
      state.maintenance_client_incident.preselection_retirements[1],
      'settlement-resolution'))
    scheduled()
    maintenance.apply_settlement = real_apply
    maintenance.send_preselection_retirement_ack = real_send
    assert.are.equal('server-blocked',
      state.maintenance_client_incident.phase)
    assert.are.same({},
      state.maintenance_client_incident.pending_preselection_retirements)
  end)

  it('offers a deduplicated explicit ID API without prompting when clean',
     function ()
    local captured
    maintenance.begin = function (...)
      captured = { ... }
    end
    assert.is_true(maintenance.reload_targets({
      ids = { 'alias', 'B', 'alias' },
    }))
    assert.are.equal('explicit-partial-reload', captured[1])
    assert.is_nil(captured[2])
    assert.are.same({}, captured[3])
    assert.are.same({ 'B', 'alias' }, captured[4])
    assert.is_function(captured[5])
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

  it('accepts an absent-census resolution without inventing a local action',
     function ()
    local record = settlement('gone', 'application-ack', true)
    table.insert(record, f('settlement-resolution', 'census-absent'))
    local old = settlement('gone', 'application-ack', false)
    state.maintenance_client_incident = {
      registered_buffer_ids = { 'gone' }, settlements = { old },
      locally_applied = {},
    }
    local scheduled
    local real_settle = maintenance.settle_next
    maintenance.settle_next = function () end
    maintenance.defer = function (callback) scheduled = callback end
    maintenance.install_settlements({ record })
    scheduled()
    maintenance.settle_next = real_settle
    assert.are.equal('gone', payload.field_text(
      state.maintenance_client_incident.acknowledged_settlements[1],
      'buffer-id'))
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

  it('records the acknowledged settlement used to finalize the archive',
     function ()
    local record = settlement('one', 'release-ack', false)
    table.insert(record, f('settlement-resolution', 'pending'))
    state.maintenance_client_incident = {
      settlements = { record }, pending_settlements = { record },
      acknowledged_settlements = {}, in_flight_settlement = record,
    }
    local scheduled
    local settle_runs = 0
    local real_settle = maintenance.settle_next
    maintenance.settle_next = function () settle_runs = settle_runs + 1 end
    maintenance.defer = function (callback) scheduled = callback end
    assert.has_error(function ()
      maintenance.handle_settlement_ack(nil, {
        f('status', 'invalid-dirty-buffer-retired'),
        f('buffer-id', 'one'), f('required-ack', 'release-ack'),
      })
    end)
    maintenance.handle_settlement_ack(nil, {
      f('status', 'all-views-settled'),
      f('buffer-id', 'one'), f('required-ack', 'release-ack'),
    })
    scheduled()
    maintenance.settle_next = real_settle
    assert.are.equal(1, settle_runs)
    local final = state.maintenance_client_incident.settlements[1]
    assert.are.equal('true', payload.field_text(final, 'acknowledged'))
    assert.are.equal('client-acknowledged',
      payload.field_text(final, 'settlement-resolution'))
  end)

  it('unlocks only the exact terminal census and waits for its ACK',
     function ()
    local buf = new_buffer()
    local buffer_id = registry.record(buf).id
    local manifest = string.rep('a', 64)
    local terminal_callback_runs = 0
    state.maintenance_state = { epoch = 9, state = 'active' }
    state.maintenance_client_incident = {
      incident_id = incident_id, epoch = 9, phase = 'completing',
      registered_buffer_ids = { buffer_id },
      g1_graph_generation = 2, g1_manifest_revision = 6,
      final_archive = { manifest_sha256 = manifest, path = '/archive' },
      terminal_callback = function ()
        terminal_callback_runs = terminal_callback_runs + 1 end,
      terminal_callback_fired = false,
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
    assert.are.equal(1, terminal_callback_runs)
    assert.is_true(state.maintenance_client_incident.terminal_callback_fired)
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
