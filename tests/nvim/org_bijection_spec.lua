-- Mirrors tests/elisp/test-sexp-org-bijection.el.

local sexpr = require('skg.sexpr.parse')
local bijection = require('skg.sexpr.org_bijection')

local sym = sexpr.symbol

---Each case: {sexp, org text, description}. The whitespace-in-symbol
---case builds its sexp programmatically, as the elisp test does with
---'intern' (such symbols cannot come from the reader).
local bijective_cases = {
  { sexpr.read('(a)'), '* a', 'simplest case' },
  { sexpr.read('(a b)'), '* a\n** b', 'two elements' },
  { sexpr.read('(a (b a))'), '* a\n** b\n*** a', 'nested list' },
  { sexpr.read('(a (b a) c)'),
    '* a\n** b\n*** a\n** c', 'nested with sibling' },
  { { sym('a'), sym('b c'), sym('d') },
    '* a\n** b c\n** d', 'whitespace in symbol' },
  { sexpr.read('(a (b c) d)'),
    '* a\n** b\n*** c\n** d', 'standard nested structure' },
  { sexpr.read('(root (child1 grandchild1 grandchild2) child2'
               .. ' (child3 (deep nested)))'),
    '* root\n** child1\n*** grandchild1\n*** grandchild2\n** child2'
    .. '\n** child3\n*** deep\n**** nested',
    'complex structure' } }

describe('skg.sexpr.org_bijection bijective cases', function ()
  for _, case in ipairs(bijective_cases) do
    local sexp, org_text, description = case[1], case[2], case[3]
    it('sexp_to_org: ' .. description, function ()
      assert.are.equal(org_text, bijection.sexp_to_org(sexp))
    end)
    it('org_to_sexp: ' .. description, function ()
      assert.are.same(sexp, bijection.org_to_sexp(org_text))
    end)
  end
end)

describe('skg.sexpr.org_bijection org_to_sexp lossy cases', function ()
  it('discards body text', function ()
    assert.are.same(sexpr.read('(a b)'),
      bijection.org_to_sexp('* a\nsome body text\n** b\nmore body'))
  end)

  it('discards blank lines', function ()
    assert.are.same(sexpr.read('(a b)'),
      bijection.org_to_sexp('* a\n\n** b\n\n'))
  end)
end)

describe('skg.sexpr.org_bijection sexp_to_org rejections', function ()
  it('rejects a bare atom', function ()
    assert.has_error(function ()
      bijection.sexp_to_org(sym('a')) end)
  end)

  it('rejects the empty list', function ()
    assert.has_error(function ()
      bijection.sexp_to_org(sexpr.NIL) end)
  end)

  it('rejects non-atom-headed input: ((a) b)', function ()
    assert.has_error(function ()
      bijection.sexp_to_org(sexpr.read('((a) b)')) end)
  end)

  it('rejects non-paren-terse input: (a (b) c)', function ()
    assert.has_error(function ()
      bijection.sexp_to_org(sexpr.read('(a (b) c)')) end)
  end)
end)

describe('skg.sexpr.org_bijection org_to_sexp rejections', function ()
  it('rejects input with no headlines', function ()
    assert.has_error(function ()
      bijection.org_to_sexp('just some text') end)
  end)

  it('rejects empty input', function ()
    assert.has_error(function () bijection.org_to_sexp('') end)
  end)

  it('rejects multiple roots', function ()
    assert.has_error(function ()
      bijection.org_to_sexp('* a\n** b\n* c') end)
  end)

  it('rejects a level jump', function ()
    assert.has_error(function ()
      bijection.org_to_sexp('* a\n*** b') end)
  end)

  it('rejects a nested level jump', function ()
    assert.has_error(function ()
      bijection.org_to_sexp('* a\n** b\n**** c') end)
  end)

  it('rejects an empty headline', function ()
    assert.has_error(function () bijection.org_to_sexp('* ') end)
  end)
end)

describe('skg.sexpr.org_bijection validation helpers', function ()
  it('atom_headed_p accepts and rejects correctly', function ()
    assert.is_true(bijection.atom_headed_p(sexpr.read('(a)')))
    assert.is_true(bijection.atom_headed_p(
      sexpr.read('(a (a (a (a))))')))
    assert.is_true(bijection.atom_headed_p(
      sexpr.read('(a b (a b (a b)) a b)')))
    assert.is_false(bijection.atom_headed_p(sexpr.read('((a))')))
    assert.is_false(bijection.atom_headed_p(sexpr.read('((a) b)')))
    assert.is_false(bijection.atom_headed_p(
      sexpr.read('(a ((b) a))')))
  end)

  it('paren_terse_p accepts and rejects correctly', function ()
    assert.is_true(bijection.paren_terse_p(sexpr.read('(a)')))
    assert.is_true(bijection.paren_terse_p(sexpr.read('(a b)')))
    assert.is_true(bijection.paren_terse_p(sexpr.read('(a (b c) d)')))
    assert.is_true(bijection.paren_terse_p(
      sexpr.read('(a (b (c d)) e (f g))')))
    assert.is_false(bijection.paren_terse_p(sexpr.read('(a (b) c)')))
    assert.is_false(bijection.paren_terse_p(
      sexpr.read('(a (b (c)))')))
  end)
end)

describe('skg.sexpr.org_bijection numeric headlines', function ()
  -- No direct elisp counterpart; pins text_to_atom, which elisp
  -- implemented with string-to-number round-trip checks.
  it('turns numeric headline text into numbers, and back', function ()
    assert.are.same({ sym('a'), 7 }, bijection.org_to_sexp('* a\n** 7'))
    assert.are.equal('* a\n** 7',
      bijection.sexp_to_org({ sym('a'), 7 }))
    -- '007' does not round-trip numerically, so it stays a symbol.
    assert.are.same({ sym('a'), sym('007') },
      bijection.org_to_sexp('* a\n** 007'))
  end)
end)
