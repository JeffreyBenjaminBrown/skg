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
    assert.is_nil(reload.herald_rules_if_loaded())
  end)

  -- Herald-rule preservation across reload (including across a FAILED
  -- reload, the unwind-protect case of test-skg-reload.el) is asserted
  -- in herald_rules_spec.lua once skg.herald_rules lands, where the
  -- rule table and its installer actually exist.
end)
