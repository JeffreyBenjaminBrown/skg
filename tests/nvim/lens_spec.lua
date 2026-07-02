-- Mirrors tests/elisp/test-skg-lens.el. Elisp compares propertized
-- strings with 'equal' (which ignores properties), so the text
-- comparisons here go through token_texts; the property assertions
-- (get-text-property N 'skg-color / 'skg-abut) become color_at(N+1)
-- and the token's abut flag.

local sexpr = require('skg.sexpr.parse')
local lens = require('skg.sexpr.lens')

local function transform (object_text, rules_text)
  return lens.transform_sexp_flat(sexpr.read(object_text),
                                  sexpr.read(rules_text))
end

local function texts (object_text, rules_text)
  return lens.token_texts(transform(object_text, rules_text))
end

local RED = sexpr.symbol('RED')
local GREEN = sexpr.symbol('GREEN')
local BLUE = sexpr.symbol('BLUE')
local YELLOW = sexpr.symbol('YELLOW')
local ORANGE = sexpr.symbol('ORANGE')

describe('skg.sexpr.lens basic rules', function ()
  it('one flat rule that applies', function ()
    assert.are.same({ 'c' }, texts('(a b)', '(a (b c))'))
  end)

  it('one flat rule that does not apply', function ()
    assert.are.same({}, texts('(a c)', '(a (b c))'))
  end)

  it('one nested rule that applies', function ()
    assert.are.same({ 'd' }, texts('(a (b c))', '(a (b (c d)))'))
  end)

  it('two flat rules apply, output in rule order', function ()
    assert.are.same({ 'cc', 'bb' },
                    texts('(a b c)', '(a (c cc) (b bb))'))
  end)

  it('complex rules with multiple nested transformations', function ()
    assert.are.same({ 'b22', 'd33', 'c11' },
      texts('(a (b b1 b2 b3) (c c1 (d d1 d2 d3) c2 c3))',
            '(a (b c)'          -- does not apply; b is a label in object
            .. ' (b (b2 b22))'
            .. ' (c (b (b3 b33))' -- does not apply; not where b3 is
            .. '    (d (d3 d33))'
            .. '    (c1 c11)))'))
  end)
end)

describe('skg.sexpr.lens ANY and IT', function ()
  it('one flat ANY rule that applies', function ()
    assert.are.same({ 'c' }, texts('(a b)', '(a (ANY c))'))
  end)

  it('one flat ANY rule that does not apply', function ()
    assert.are.same({}, texts('(a b)', '(c (ANY d))'))
  end)

  it('nested ANY applies even if the object label is bare', function ()
    assert.are.same({ 'd' }, texts('(a (b))', '(a (b (ANY d)))'))
  end)

  it('ANY considers only the chain of labels', function ()
    assert.are.same({}, texts('(a (b c))', '(a (b (c (ANY d))))'))
  end)

  it('two flat rules, one ANY, both apply', function ()
    assert.are.same({ 'x', 'cc' },
                    texts('(a b c)', '(a (ANY x) (c cc))'))
  end)

  it('IT captures the matched tail value', function ()
    assert.are.same({ 'x:c:y' },
                    texts('(a (b c))', '(a (b (ANY x IT y)))'))
  end)

  it('a single IT can match multiple values', function ()
    assert.are.same({ 'x:c:y', 'x:d:y' },
                    texts('(a (b c d))', '(a (b (ANY x IT y)))'))
  end)
end)

describe('skg.sexpr.lens string prefixes', function ()
  it('prepends the prefix to child outputs', function ()
    assert.are.same({ 'B: cc' },
                    texts('(a (b c))', '(a (b "B: " (c "cc")))'))
  end)

  it('emits the prefix alone when no child fires', function ()
    assert.are.same({ 'B: ' },
                    texts('(a (b c))', '(a (b "B: " (d "dd")))'))
  end)

  it('prepends the prefix to every matching output', function ()
    assert.are.same({ 'B: cc', 'B: dd' },
      texts('(a (b c d))', '(a (b "B: " (c "cc") (d "dd")))'))
  end)
end)

describe('skg.sexpr.lens colors and ABUT', function ()
  it('accepts ORANGE alongside the other colors', function ()
    local out = transform('(a b)', '(a (ORANGE b "bb"))')
    assert.are.same({ 'bb' }, lens.token_texts(out))
    assert.are.equal(ORANGE, lens.color_at(out[1], 1))
  end)

  it('ABUT marks the emitted token', function ()
    local out = transform('(a b)', '(a (b ABUT "bb"))')
    assert.are.same({ 'bb' }, lens.token_texts(out))
    assert.is_true(out[1].abut)
  end)

  it('without ABUT the mark is absent', function ()
    local out = transform('(a b)', '(a (b "bb"))')
    assert.is_false(out[1].abut)
  end)

  it('a deeper color overrides a shallower one', function ()
    -- From the skg-transform-sexp-flat docstring: in
    -- (a (RED b (c d) (GREEN e f))), d is red but f is green.
    -- (Expected output verified against the elisp engine directly.)
    local out = transform('(a (b c e))',
                          '(a (RED b (c d) (GREEN e f)))')
    assert.are.same({ 'd', 'f' }, lens.token_texts(out))
    assert.are.equal(RED, lens.color_at(out[1], 1))
    assert.are.equal(GREEN, lens.color_at(out[2], 1))
  end)
end)

describe('skg.sexpr.lens INTERC', function ()
  it('joins slots with the separator, prefix prepended', function ()
    assert.are.same({ '3{8' },
      texts('(a (pair (left 3) (right 8)))',
            '(a (INTERC "{" pair (left (ANY IT)) (right (ANY IT))))'))
  end)

  it('still emits the separator when one slot is empty', function ()
    assert.are.same({ '3{' },
      texts('(a (pair (left 3)))',
            '(a (INTERC "{" pair (left (ANY IT)) (right (ANY IT))))'))
    assert.are.same({ '{8' },
      texts('(a (pair (right 8)))',
            '(a (INTERC "{" pair (left (ANY IT)) (right (ANY IT))))'))
  end)

  it('suppresses the token when every slot is empty', function ()
    assert.are.same({},
      texts('(a (pair (other 99)))',
            '(a (INTERC "{" pair (left (ANY IT)) (right (ANY IT))))'))
  end)

  it('emits nothing when the label is absent', function ()
    assert.are.same({},
      texts('(a (otherPair))',
            '(a (INTERC "{" pair (left (ANY IT))))'))
  end)

  it('preserves per-sub-rule colors on the output token', function ()
    local out = transform(
      '(a (pair (left 3) (right 8)))',
      '(a (INTERC "{" pair (YELLOW left (ANY IT))'
      .. ' (BLUE right (ANY IT))))')
    assert.are.same({ '3{8' }, lens.token_texts(out))
    assert.are.equal(YELLOW, lens.color_at(out[1], 1))
    assert.is_nil(lens.color_at(out[1], 2)) -- separator: no color
    assert.are.equal(BLUE, lens.color_at(out[1], 3))
  end)

  it('with its own color, colors separator and prefix', function ()
    local out = transform(
      '(a (pair (left 3) (right 8)))',
      '(a (BLUE INTERC "{" pair "P:" (YELLOW left (ANY IT))'
      .. ' (right (ANY IT))))')
    assert.are.same({ 'P:3{8' }, lens.token_texts(out))
    assert.are.equal(BLUE, lens.color_at(out[1], 1))   -- prefix 'P'
    assert.are.equal(BLUE, lens.color_at(out[1], 2))   -- prefix ':'
    assert.are.equal(YELLOW, lens.color_at(out[1], 3)) -- '3'
    assert.are.equal(BLUE, lens.color_at(out[1], 4))   -- separator
    assert.are.equal(BLUE, lens.color_at(out[1], 5))   -- '8' inherited
  end)

  it('empty separator concatenates slots', function ()
    assert.are.same({ 'stage:-X-M' },
      texts('(a (stage removedX removedM))',
            '(a (INTERC "" stage "stage:"'
            .. ' (removedX "-X") (removedM "-M")))'))
  end)

  it('emits one token per matching child', function ()
    assert.are.same({ 'stage:-X', 'stage:M' },
      texts('(a (stage removedX) (stage newM))',
            '(a (INTERC "" stage "stage:"'
            .. ' (removedX "-X") (newM "M")))'))
  end)
end)
