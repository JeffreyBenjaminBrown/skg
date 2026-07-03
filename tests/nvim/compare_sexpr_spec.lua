-- Mirrors tests/elisp/test-skg-compare-sexpr.el.

local sexpr = require('skg.sexpr.parse')
local compare = require('skg.sexpr.compare')

local function subtree_p (object_text, structure_text)
  return compare.subtree_p(sexpr.read(object_text),
                           sexpr.read(structure_text))
end

describe('skg.sexpr.compare.subtree_p', function ()
  it('simple subtree match: (a b c) contains (a)', function ()
    assert.is_true(subtree_p('(a b c)', '(a)'))
  end)

  it('unordered elements match: (a b c) contains (a c b)', function ()
    assert.is_true(subtree_p('(a b c)', '(a c b)'))
  end)

  it('partial element match: (a b c) contains (a c)', function ()
    assert.is_true(subtree_p('(a b c)', '(a c)'))
  end)

  it('wrong head fails: (a b c) does not contain (c a b)', function ()
    assert.is_false(subtree_p('(a b c)', '(c a b)'))
  end)

  it('nested simple match: (a (b c d) e) contains (a e)', function ()
    assert.is_true(subtree_p('(a (b c d) e)', '(a e)'))
  end)

  it('nested with sublist: (a (b c d) e) contains (a (b))', function ()
    assert.is_true(subtree_p('(a (b c d) e)', '(a (b))'))
  end)

  it('nested partial sublist: (a (b c d) e) contains (a (b c))',
     function ()
    assert.is_true(subtree_p('(a (b c d) e)', '(a (b c))'))
  end)

  it('nested wrong order in sublist: (a (b c d) e) lacks (a (c b))',
     function ()
    assert.is_false(subtree_p('(a (b c d) e)', '(a (c b))'))
  end)

  it('atoms match when equal', function ()
    assert.is_true(subtree_p('foo', 'foo'))
  end)

  it("atoms don't match when different", function ()
    assert.is_false(subtree_p('foo', 'bar'))
  end)

  it('list does not match atom', function ()
    assert.is_false(subtree_p('(a b)', 'a'))
  end)
end)

describe('skg.sexpr.compare.values_equal', function ()
  -- No elisp counterpart (elisp used the built-in 'equal').
  it('compares atoms, lists and pairs deeply', function ()
    assert.is_true(compare.values_equal(
      sexpr.read('(a (b . "x") 7 "s")'),
      sexpr.read('(a (b . "x") 7 "s")')))
    assert.is_false(compare.values_equal(
      sexpr.read('(a (b . "x"))'),
      sexpr.read('(a (b . "y"))')))
    assert.is_false(compare.values_equal(
      sexpr.read('(a b)'), sexpr.read('(a b c)')))
  end)
end)
