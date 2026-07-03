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
end)
