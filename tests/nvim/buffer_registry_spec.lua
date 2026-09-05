local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')

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
    disposable = options.disposable,
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
