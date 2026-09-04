-- Mirrors tests/elisp/test-skg-reload.el (the parts meaningful in Lua:
-- module refresh and herald-rule preservation across a reload; the
-- elisp concerns about permanent-local vars and keymap identity have
-- no Lua analog -- see the header of nvim/lua/skg/reload.lua).

local reload = require('skg.reload')

describe('skg.reload', function ()
  it('replaces skg module tables with fresh ones', function ()
    local log_before = require('skg.log')
    log_before.marker_that_should_not_survive = true
    reload.reload()
    local log_after = require('skg.log')
    assert.is_nil(log_after.marker_that_should_not_survive)
    assert.are_not.equal(log_before, log_after)
  end)

  it('leaves non-skg modules alone', function ()
    local plenary_before = package.loaded['plenary.busted']
    reload.reload()
    assert.are.equal(plenary_before, package.loaded['plenary.busted'])
  end)

  it('reports no herald rules when none are loaded', function ()
    package.loaded['skg.herald_rules'] = nil
    assert.is_nil(reload.herald_rules_if_loaded())
  end)

  it('preserves the herald rule table across a reload', function ()
    -- Mirrors test-skg-reload.el's herald-table preservation.
    local herald_rules = require('skg.herald_rules')
    local rules = require('skg.sexpr.parse').read(
      '(skg (focused) (node (id)))')
    herald_rules.install_rules(rules)
    reload.reload()
    local reloaded = require('skg.herald_rules')
    assert.are.equal(rules, reloaded.get_rules())
  end)

  it('preserves the herald rule table even when the reload fails',
     function ()
    -- The unwind-protect case: a load error must not cost the session
    -- its only copy of the table.
    local herald_rules = require('skg.herald_rules')
    local rules = require('skg.sexpr.parse').read('(skg (folded))')
    herald_rules.install_rules(rules)
    local real_require = _G.require
    _G.require = function (name)
      if name == 'skg' then error('simulated load error') end
      return real_require(name)
    end
    local ok = pcall(reload.reload)
    _G.require = real_require
    assert.is_false(ok) -- the reload error still propagates
    local reloaded = require('skg.herald_rules')
    assert.are.equal(rules, reloaded.get_rules())
  end)

  it('carries maintenance retry authority into the deliberate reconnect',
     function ()
    local state = require('skg.state')
    local config = require('skg.config')
    local root = require('skg')
    local client = require('skg.client')
    local incident = {
      incident_id = '12345678-1234-4234-8234-123456789abc',
      epoch = 9,
      phase = 'view-settlement-ack-pending',
      archive = { manifest_sha256 = string.rep('a', 64) },
    }
    state.maintenance_client_incident = incident
    state.maintenance_state = { epoch = 9, state = 'active' }
    state.pending_maintenance_offer = { candidate_id = 'candidate' }
    state.active_source_set_name = 'private'
    state.next_request_number = 41
    state.id_stack = { { 'id', 'title' } }
    config.config_file_path = '/tmp/skgconfig.toml'
    config.source_inventory = { { name = 'private' } }
    config.store_state = { graph_generation = 7 }
    root.config_path = '/tmp/skgconfig.toml'
    client.port = 1739
    local finalized = 0
    state.set_request_failure_handler(function ()
      incident.phase = 'reconnect-required' end)
    state.set_request_finalizer(function () finalized = finalized + 1 end)
    local last_request_number = state.next_request_number

    reload.reload()

    local reloaded_state = require('skg.state')
    local reloaded_config = require('skg.config')
    assert.are.equal(1, finalized)
    assert.are.equal(incident, reloaded_state.maintenance_client_incident)
    assert.are.equal('reconnect-required',
      reloaded_state.maintenance_client_incident.phase)
    assert.are.equal('active', reloaded_state.maintenance_state.state)
    assert.are.equal('private', reloaded_state.active_source_set_name)
    assert.are.equal(last_request_number, reloaded_state.next_request_number)
    assert.are.same({ { 'id', 'title' } }, reloaded_state.id_stack)
    assert.are.equal('/tmp/skgconfig.toml',
      reloaded_config.config_file_path)
    assert.are.equal(7, reloaded_config.store_state.graph_generation)
    assert.are.equal(1739, require('skg.client').port)
    assert.are.equal('/tmp/skgconfig.toml', require('skg').config_path)
    assert.is_truthy(
      reloaded_state.server_push_handlers['maintenance-offer'])

    reloaded_state.maintenance_client_incident = nil
    reloaded_state.maintenance_state = nil
    reloaded_state.pending_maintenance_offer = nil
    reloaded_config.config_file_path = nil
    reloaded_config.source_inventory = nil
    reloaded_config.store_state = nil
    require('skg.client').port = nil
    require('skg').config_path = nil
  end)
end)
