local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
local lock = require('skg.lock')

local function f (name, value)
  return { sexpr.symbol(name), value }
end

local function make_buffer (kind, options)
  options = options or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buf, 'skg://maintenance-' .. tostring(buf))
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '* Original', 'body' })
  vim.bo[buf].endofline = true
  vim.bo[buf].modified = false
  vim.b[buf].skg_view_uri = options.view_uri or 'view:test'
  registry.register(buf, kind, {
    disposable = options.disposable == true,
    lifecycle = options.lifecycle or 'live-view',
    continuation_id = options.continuation_id,
    recipe = options.recipe,
    root_ids = options.root_ids,
    last_fetched = '* Original\nbody\n',
    graph_generation = 7,
    presentation_generation = 3,
    server_revision = 11,
    application_token = 5,
  })
  return buf
end

local function settlement (buf, dirty, required_ack)
  local record = registry.record(buf)
  return {
    f('buffer-id', record.id),
    f('buffer-key', 'none'),
    f('kind', record.kind),
    f('view-uri', record.view_uri or 'none'),
    f('dirty', dirty and 'true' or 'nil'),
    f('impacted', 'true'),
    f('parse-uncertain', 'nil'),
    f('observed-ids', {}),
    f('resolved-primary-ids', {}),
    f('base-graph-generation', record.graph_generation),
    f('base-presentation-generation', record.presentation_generation),
    f('base-server-revision', record.server_revision),
    f('base-application-token', record.application_token),
    f('planned-disposition', 'refreshed'),
    f('required-ack', required_ack),
  }
end

local function wipe_registered_buffers ()
  for _, buf in ipairs(registry.buffers()) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.bo[buf].modifiable = true
      vim.bo[buf].modified = false
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
end

describe('skg maintenance buffer transitions', function ()
  before_each(wipe_registered_buffers)
  after_each(wipe_registered_buffers)

  it('requires every constructor to state lifecycle and disposability',
     function ()
    local buf = vim.api.nvim_create_buf(false, true)
    assert.has_error(function ()
      registry.register(buf, 'derived-report', { disposable = true }) end,
      'Skg buffer constructor omitted its lifecycle')
    assert.has_error(function ()
      registry.register(buf, 'derived-report', { lifecycle = 'client-local' })
    end, 'Skg buffer constructor omitted its disposable policy')
    registry.register(buf, 'derived-report', {
      lifecycle = 'client-local', disposable = true,
    })
    assert.are.equal('derived-report', registry.record(buf).kind)
  end)

  it('initializes only unbound new-empty authority from the first handshake',
     function ()
    local config = require('skg.config')
    local state = require('skg.state')
    local old_store_state = config.store_state
    local old_source_set = state.active_source_set_name
    config.store_state = nil
    state.active_source_set_name = 'server-default'
    local unbound = vim.api.nvim_create_buf(false, true)
    registry.register(unbound, 'new-empty-content-view', {
      lifecycle = 'live-view', disposable = false, view_uri = 'new',
      recipe = { kind = 'new-empty' }, last_fetched = '',
    })
    local old = vim.api.nvim_create_buf(false, true)
    registry.register(old, 'new-empty-content-view', {
      lifecycle = 'live-view', disposable = false, view_uri = 'old',
      recipe = { kind = 'new-empty' }, last_fetched = '',
      graph_generation = 3,
    })
    local content = vim.api.nvim_create_buf(false, true)
    registry.register(content, 'content-view', {
      lifecycle = 'live-view', disposable = false, view_uri = 'content',
      recipe = { kind = 'single-root', root_id = 'root' },
      root_ids = { 'root' }, last_fetched = '',
    })
    registry.adopt_unbound_new_empty_authority(
      7, 'all', '11111111-2222-4333-8444-555555555555')
    assert.are.equal(7, registry.record(unbound).graph_generation)
    assert.are.equal('all', registry.record(unbound).source_set)
    assert.are.equal('11111111-2222-4333-8444-555555555555',
      registry.record(unbound).server_session_id)
    assert.are.equal(3, registry.record(old).graph_generation)
    assert.are.equal('server-default', registry.record(old).source_set)
    assert.are.equal(0, registry.record(content).graph_generation)
    config.store_state = old_store_state
    state.active_source_set_name = old_source_set
  end)

  it('reuses a conventional name only under explicit disposable policy',
     function ()
    local name = 'skg://durable-namesake'
    local durable = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(durable, name)
    vim.api.nvim_buf_set_lines(durable, 0, -1, false,
      { 'keep this report' })
    vim.bo[durable].modified = false
    registry.register(durable, 'durable-report', {
      lifecycle = 'client-local', disposable = false,
      last_fetched = registry.raw_text(durable),
    })
    local fresh = registry.acquire_generated_buffer(name, true, true)
    assert.are_not.equal(durable, fresh)
    assert.are.equal('keep this report',
      vim.api.nvim_buf_get_lines(durable, 0, 1, false)[1])

    local disposable_name = 'skg://disposable-namesake'
    local disposable = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(disposable, disposable_name)
    vim.bo[disposable].modified = false
    registry.register(disposable, 'derived-report', {
      lifecycle = 'client-local', disposable = true,
    })
    assert.are.equal(disposable, registry.acquire_generated_buffer(
      disposable_name, true, true))
  end)

  it('enumerates every direct product buffer constructor', function ()
    local owners = {}
    local files = vim.fn.globpath(
      _G.skg_test_repo_root() .. '/nvim/lua/skg', '**/*.lua', false, true)
    for _, file in ipairs(files) do
      local owner = '<top-level>'
      for _, line in ipairs(vim.fn.readfile(file)) do
        local exported = line:match('^function M%.([%w_]+)%s*%(')
        local private = line:match('^local function ([%w_]+)%s*%(')
        if exported then owner = 'M.' .. exported
        elseif private then owner = private end
        if line:find('vim.api.nvim_create_buf(', 1, true) then
          owners[owner] = true end
      end
    end
    local actual = {}
    for owner in pairs(owners) do table.insert(actual, owner) end
    table.sort(actual)
    local expected = {
      'M.acquire_generated_buffer', 'M.open_edit_buffer',
      'M.open_interrupted_view', 'M.open_org_buffer_from_text',
      'M.start_terminal', 'M.view_id_stack', 'verify_sidecar',
    }
    table.sort(expected)
    assert.are.same(expected, actual)
  end)

  it('registers only a direct configured raw skg file', function ()
    local config = require('skg.config')
    local old_inventory = config.source_inventory
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, '/client/source/node.skg')
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'pid: node' })
    vim.bo[buf].modified = false
    config.source_inventory = {
      { name = 'main', path = '/client/source' },
    }
    local record = registry.register_raw_file_if_configured(buf)
    config.source_inventory = old_inventory
    assert.are.equal('raw-skg-file', record.kind)
    assert.are.equal('ordinary-file', record.lifecycle)
    assert.is_false(record.disposable)
  end)

  it('emits the complete normalized reconnect descriptor', function ()
    local buf = make_buffer('search-view', {
      lifecycle = 'live-view',
      continuation_id = 'continuation-1',
      root_ids = { 'z-root', 'a-root', 'z-root' },
      recipe = {
        kind = 'search', terms = 'dog', regex = true,
        body = false, operators = true,
      },
    })
    vim.b[buf].skg_record_source_set = 'private'
    vim.b[buf].skg_logical_dirty = true
    vim.b[buf].skg_presentation_stale = true
    vim.b[buf].skg_search_stale = true
    vim.b[buf].skg_herald_bearing = true
    registry.lock_for_maintenance(buf, 9)
    local descriptor = registry.census()[1]
    assert.are.equal('live-view', descriptor.lifecycle)
    assert.are.equal('continuation-1', descriptor.continuation_id)
    assert.are.same({ 'a-root', 'z-root' }, descriptor.root_ids)
    assert.are.equal('private', descriptor.source_set)
    assert.are.equal(9, descriptor.maintenance_epoch)
    assert.is_true(descriptor.dirty)
    assert.is_true(descriptor.logical_dirty)
    assert.is_true(descriptor.presentation_stale)
    assert.is_true(descriptor.search_stale)
    assert.is_true(descriptor.herald_bearing)
    assert.are.equal(
      '((body "nil") (kind "search") (operators "true")'
        .. ' (regex "true") (terms "dog"))', descriptor.recipe)
    local wire = sexpr.read(registry.census_payload())[1]
    assert.are.same({ 'a-root', 'z-root' },
      payload.string_list(payload.field(wire, 'root-ids')))
    assert.are.equal(descriptor.recipe, payload.field_text(wire, 'recipe'))
  end)

  it('marks only named queued view uris presentation-stale', function ()
    local named = make_buffer('content-view', { view_uri = 'view:named' })
    local other = make_buffer('content-view', { view_uri = 'view:other' })
    require('skg.rerender').refresh_queued_handler(nil, sexpr.read(
      '((queued-view-uris ("view:named")))'))
    assert.is_true(registry.record(named).presentation_stale)
    assert.is_false(registry.record(other).presentation_stale)
  end)

  it('binds an attached workflow to its origin and dirties the parent',
     function ()
    local origin = make_buffer('content-view')
    local child = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(child, 0, -1, false, { '* Draft metadata' })
    vim.bo[child].modified = false
    registry.register(child, 'metadata-editor', {
      lifecycle = 'attached-workflow',
      disposable = false,
      continuation_id = 'continuation-1',
      origin_buffer = origin,
      origin_location = '((line 1) (metadata-start 2) (metadata-length 8))',
      recipe = { kind = 'metadata-editor' },
    })
    local origin_record = registry.record(origin)
    local child_record = registry.record(child)
    assert.is_true(origin_record.logical_dirty)
    assert.is_true(registry.dirty(origin))
    assert.is_true(child_record.logical_dirty)
    assert.are.equal(origin_record.id, child_record.origin_buffer_id)
    assert.are.equal(origin_record.view_uri, child_record.origin_view_uri)
    assert.are.equal(
      origin_record.application_token, child_record.origin_application_token)
    local descriptors = registry.census()
    local child_descriptor
    for _, descriptor in ipairs(descriptors) do
      if descriptor.buffer_id == child_record.id then
        child_descriptor = descriptor break end
    end
    assert.are.equal(child_record.origin_location,
      child_descriptor.origin_location)
    registry.register(origin, 'content-view', {
      lifecycle = 'live-view',
      disposable = false,
      view_uri = origin_record.view_uri,
      recipe = { kind = 'single-root', root_id = 'origin' },
      application_token = origin_record.application_token,
      last_fetched = registry.raw_text(origin),
    })
    assert.is_true(registry.record(origin).logical_dirty)
    vim.api.nvim_buf_delete(child, { force = true })
    assert.is_false(registry.record(origin).logical_dirty)
  end)

  it('releases exact dirty search text against G1 without unlocking it',
     function ()
    local buf = make_buffer('search-view')
    vim.api.nvim_buf_set_lines(buf, 2, 2, false, { 'authored' })
    vim.b[buf].skg_logical_dirty = true
    local exact = settlement(buf, true, 'release-ack')
    local before = registry.raw_text(buf)
    registry.lock_for_maintenance(buf, 4)
    local record = registry.release_across_maintenance(buf, exact, 4, 8)
    assert.are.equal(before, registry.raw_text(buf))
    assert.are.equal(8, record.graph_generation)
    assert.is_true(record.presentation_stale)
    assert.is_true(record.search_stale)
    assert.is_false(vim.bo[buf].modifiable)
  end)

  it('releases a raced save lock without releasing maintenance', function ()
    local buf = make_buffer('content-view')
    lock.lock_for_save(buf)
    registry.lock_for_maintenance(buf, 4)
    lock.unlock_after_save(buf)
    assert.is_false(vim.b[buf].skg_save_locked)
    assert.are.equal(4, registry.record(buf).maintenance_epoch)
    assert.is_false(vim.bo[buf].modifiable)
    registry.unlock_after_maintenance(buf, 4)
    assert.is_true(vim.bo[buf].modifiable)
  end)

  it('retains independent maintenance restrictions in either release order',
     function ()
    local buf = make_buffer('content-view')
    registry.lock_for_maintenance(buf, 9, 'incident-a')
    registry.lock_for_maintenance(buf, 10, 'incident-b')
    registry.lock_for_maintenance(buf, 10, 'incident-b')
    assert.are.equal(2, vim.tbl_count(
      vim.b[buf].skg_maintenance_restrictions))
    registry.unlock_after_maintenance(buf, 10, 'incident-b')
    assert.are.equal(9, registry.record(buf).maintenance_epoch)
    assert.is_false(vim.bo[buf].modifiable)
    registry.unlock_after_maintenance(buf, 9, 'incident-a')
    assert.is_nil(registry.record(buf).maintenance_epoch)
    assert.is_true(vim.bo[buf].modifiable)

    registry.lock_for_maintenance(buf, 9, 'incident-a')
    registry.lock_for_maintenance(buf, 10, 'incident-b')
    registry.unlock_after_maintenance(buf, 9, 'incident-a')
    assert.are.equal(10, registry.record(buf).maintenance_epoch)
    assert.is_false(vim.bo[buf].modifiable)
    registry.unlock_after_maintenance(buf, 10, 'incident-b')
    assert.is_true(vim.bo[buf].modifiable)
  end)

  it('restores an originally read-only buffer after its restriction settles',
     function ()
    local buf = make_buffer('content-view')
    vim.bo[buf].modifiable = false
    registry.lock_for_maintenance(buf, 9, 'incident-read-only')
    registry.unlock_after_maintenance(buf, 9, 'incident-read-only')
    assert.is_false(vim.bo[buf].modifiable)
    assert.is_nil(registry.record(buf).maintenance_epoch)
  end)

  it('finds a unique obligation by epoch through the old API', function ()
    local state = require('skg.state')
    local old_incident = state.maintenance_client_incident
    state.maintenance_client_incident = nil
    local buf = make_buffer('content-view')
    registry.lock_for_maintenance(buf, 9, 'incident-a')
    registry.lock_for_maintenance(buf, 10, 'incident-b')
    state.maintenance_client_incident = { incident_id = 'incident-b' }
    registry.unlock_after_maintenance(buf, 9)
    assert.are.equal(10, registry.record(buf).maintenance_epoch)
    registry.unlock_after_maintenance(buf, 10, 'incident-b')
    assert.is_nil(registry.record(buf).maintenance_epoch)
    state.maintenance_client_incident = old_incident
  end)

  it('migrates a legacy epoch obligation when identity becomes available',
     function ()
    local state = require('skg.state')
    local old_incident = state.maintenance_client_incident
    state.maintenance_client_incident = nil
    local buf = make_buffer('content-view')
    registry.lock_for_maintenance(buf, 11)
    state.maintenance_client_incident = { incident_id = 'incident-a' }
    registry.lock_for_maintenance(buf, 11)
    assert.are.equal(1, vim.tbl_count(
      vim.b[buf].skg_maintenance_restrictions))
    assert.are.equal(11,
      vim.b[buf].skg_maintenance_restrictions['incident:incident-a'])
    registry.unlock_after_maintenance(buf, 11, 'incident-a')
    assert.is_nil(registry.record(buf).maintenance_epoch)
    state.maintenance_client_incident = old_incident
  end)

  it('repeats pending, maintenance and stale warnings on buffer entry',
     function ()
    local buf = make_buffer('search-view')
    local state = require('skg.state')
    local old_offer = state.pending_maintenance_offer
    local old_notify = vim.notify
    local notice
    state.pending_maintenance_offer = { candidate_id = 'candidate' }
    registry.lock_for_maintenance(buf, 4)
    vim.b[buf].skg_presentation_stale = true
    vim.b[buf].skg_search_stale = true
    vim.b[buf].skg_herald_bearing = true
    vim.notify = function (message) notice = message end
    registry.notify_status(buf)
    vim.notify = old_notify
    state.pending_maintenance_offer = old_offer
    assert.is_truthy(notice:find('every Skg view save is blocked', 1, true))
    assert.is_truthy(notice:find('maintenance-locked for epoch 4', 1, true))
    assert.is_truthy(notice:find('generated heralds', 1, true))
    assert.is_truthy(notice:find('Search membership and ranking', 1, true))
  end)

  it('rejects a settlement whose frozen base changed', function ()
    local buf = make_buffer('content-view')
    local exact = settlement(buf, false, 'release-ack')
    registry.lock_for_maintenance(buf, 4)
    vim.b[buf].skg_application_token = 6
    assert.has_error(function ()
      registry.release_across_maintenance(buf, exact, 4, 8) end)
    assert.are.equal(7, registry.record(buf).graph_generation)
  end)

  it('retires in place without changing text or native undo history',
     function ()
    local buf = make_buffer('content-view')
    vim.api.nvim_buf_set_lines(buf, 2, 2, false, { 'authored' })
    vim.b[buf].skg_logical_dirty = true
    local exact = settlement(buf, true, 'retirement-ack')
    local before_text = registry.raw_text(buf)
    local before_undo = vim.api.nvim_buf_call(buf, vim.fn.undotree)
    registry.lock_for_maintenance(buf, 4)
    local record = registry.retire_for_maintenance(
      buf, exact, 4, '12345678-1234-4234-8234-123456789abc')
    local after_undo = vim.api.nvim_buf_call(buf, vim.fn.undotree)
    assert.are.equal(before_text, registry.raw_text(buf))
    assert.are.equal(before_undo.seq_last, after_undo.seq_last)
    assert.is_nil(record.view_uri)
    assert.are.equal('detached-recovery', record.lifecycle)
    assert.is_truthy(vim.api.nvim_buf_get_name(buf):find(
      '%[recovery 12345678/' .. record.id:sub(1, 8) .. '%]'))
    assert.is_false(vim.bo[buf].modifiable)
  end)

  it('closes only an exact clean disposable buffer', function ()
    local retained = make_buffer('derived-report')
    local retained_settlement = settlement(retained, false, 'close-ack')
    registry.lock_for_maintenance(retained, 4)
    assert.has_error(function ()
      registry.close_for_maintenance(retained, retained_settlement, 4) end)
    assert.is_true(vim.api.nvim_buf_is_valid(retained))

    local disposable = make_buffer('override-choice-menu', {
      disposable = true, view_uri = 'view:disposable',
    })
    local exact = settlement(disposable, false, 'close-ack')
    registry.lock_for_maintenance(disposable, 4)
    registry.close_for_maintenance(disposable, exact, 4)
    assert.is_false(vim.api.nvim_buf_is_valid(disposable))
  end)

  it('applies exact rendered authority while preserving lock and view',
     function ()
    local buf = make_buffer('search-view')
    vim.api.nvim_set_current_buf(buf)
    vim.api.nvim_win_set_cursor(0, { 2, 2 })
    local exact = settlement(buf, false, 'application-ack')
    local content = '* Re-rendered\nnew body\nlast\n'
    local application = {
      f('content', content),
      f('content-sha256', vim.fn.sha256(content)),
      f('resulting-graph-generation', 8),
      f('resulting-presentation-generation', 4),
      f('resulting-server-revision', 12),
      f('resulting-application-token', 6),
      f('warnings', {}),
    }
    registry.lock_for_maintenance(buf, 4)
    local record = registry.apply_maintenance_rendered_view(
      buf, exact, application, 4, 8)
    assert.are.equal(content, registry.raw_text(buf))
    assert.are.same({ 2, 2 }, vim.api.nvim_win_get_cursor(0))
    assert.are.equal(6, record.application_token)
    assert.are.equal(12, record.server_revision)
    assert.are.equal(8, record.graph_generation)
    assert.are.equal(4, record.presentation_generation)
    assert.are.equal(vim.fn.sha256(content), record.last_fetched_sha256)
    assert.is_true(record.search_stale)
    assert.is_false(record.presentation_stale)
    assert.is_false(vim.bo[buf].modifiable)
    assert.are.equal(content, payload.field(application, 'content'))
  end)
end)
