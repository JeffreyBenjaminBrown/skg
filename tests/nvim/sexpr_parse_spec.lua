-- Tests for nvim/lua/skg/sexpr/parse.lua. Partially mirrors
-- tests/elisp/test-skg-sexpr-search.el (the find-sexp-end cases); the
-- reader/printer tests are new, since Emacs got those for free from
-- 'read'/'prin1'. Wire shapes come from api-and-formats.md and the
-- pinned herald-rules fixture.

local sexpr = require('skg.sexpr.parse')

local function read1 (text)
  local value = sexpr.read(text)
  return value
end

local function round_trips (text)
  local first = read1(text)
  local printed = sexpr.to_string(first)
  local second = read1(printed)
  assert.are.same(first, second,
    'round trip failed for: ' .. text .. ' printed as: ' .. printed)
end

describe('skg.sexpr.parse reading atoms', function ()
  it('interns symbols so equality is eq-like', function ()
    assert.are.equal(read1('foo'), sexpr.symbol('foo'))
    assert.are.equal(sexpr.symbol('foo'), sexpr.symbol('foo'))
    assert.are_not.equal(sexpr.symbol('foo'), sexpr.symbol('bar'))
  end)

  it('reads strings with the server escapes', function ()
    assert.are.equal('plain', read1('"plain"'))
    assert.are.equal('with "quotes" in', read1('"with \\"quotes\\" in"'))
    assert.are.equal('back\\slash', read1('"back\\\\slash"'))
    assert.are.equal('two\nlines', read1('"two\nlines"'))
  end)

  it('reads integers and floats', function ()
    assert.are.equal(7, read1('7'))
    assert.are.equal(-42, read1('-42'))
    assert.are.equal(1.5, read1('1.5'))
  end)

  it('keeps UUID-shaped tokens as symbols', function ()
    local uuid = read1('3861db2c-7f16-4814-b403-52b5e05d5e0a')
    assert.is_true(sexpr.is_symbol(uuid))
    assert.are.equal('3861db2c-7f16-4814-b403-52b5e05d5e0a', uuid.name)
  end)

  it('accepts multibyte and punctuation-bearing atoms', function ()
    assert.are.equal('⌂:public', read1('⌂:public').name)
    assert.are.equal('override-menu:PID', read1('override-menu:PID').name)
    assert.are.equal("don't", read1("don't").name)
  end)

  it('reads nil and () as the shared empty list', function ()
    assert.are.equal(sexpr.NIL, read1('nil'))
    assert.are.equal(sexpr.NIL, read1('()'))
    assert.is_true(sexpr.is_nil(read1('nil')))
  end)
end)

describe('skg.sexpr.parse reading lists and pairs', function ()
  it('reads nested lists', function ()
    local value = read1('(a (b c) (d (e)))')
    assert.are.same(
      { sexpr.symbol('a'),
        { sexpr.symbol('b'), sexpr.symbol('c') },
        { sexpr.symbol('d'), { sexpr.symbol('e') } } },
      value)
  end)

  it('reads a dotted pair', function ()
    local value = read1('(request . "verify connection")')
    assert.is_true(sexpr.is_pair(value))
    assert.are.equal(sexpr.symbol('request'), value.car)
    assert.are.equal('verify connection', value.cdr)
  end)

  it('right-folds a multi-element dotted tail', function ()
    local value = read1('(a b . c)')
    assert.is_true(sexpr.is_pair(value))
    assert.are.equal(sexpr.symbol('a'), value.car)
    assert.is_true(sexpr.is_pair(value.cdr))
    assert.are.equal(sexpr.symbol('b'), value.cdr.car)
    assert.are.equal(sexpr.symbol('c'), value.cdr.cdr)
  end)

  it('does not mistake atom-internal dots for pair dots', function ()
    local value = read1('(a 1.5 x.y)')
    assert.are.same(
      { sexpr.symbol('a'), 1.5, sexpr.symbol('x.y') }, value)
  end)

  it('reports the position after each read', function ()
    local text = '(a b) (c d)'
    local first, after = sexpr.read(text)
    assert.are.same({ sexpr.symbol('a'), sexpr.symbol('b') }, first)
    local second = sexpr.read(text, after)
    assert.are.same({ sexpr.symbol('c'), sexpr.symbol('d') }, second)
  end)

  it('errors on malformed input', function ()
    assert.has_error(function () read1('(unterminated') end)
    assert.has_error(function () read1('"unterminated') end)
    assert.has_error(function () read1(')') end)
    assert.has_error(function () read1('( . x)') end)
    assert.has_error(function () read1('') end)
  end)
end)

describe('skg.sexpr.parse printing', function ()
  it('round-trips the wire shapes of api-and-formats.md', function ()
    round_trips('((request . "text search") (terms . "dog cat") '
                .. '(regex . "false"))')
    round_trips('((response-type save-result) (content "* (skg (node '
                .. '(id 7))) hi") (errors ()) (warnings ("w1" "w2")))')
    round_trips('((busy-initializing . "still loading"))')
    round_trips('((response-type "titles-by-ids") (content '
                .. '((3861db2c-aaaa . "title one") (deadbeef . "two"))))')
    round_trips('(skg (node (id 7) (source main) (unstaged newX newM)))')
    round_trips('(skg (node (id 9) (source main) indef '
                .. '(staged removedM) (unstaged newM)))')
    round_trips('(skg alias (staged newM))')
    round_trips('(skg (textChanged staged unstaged))')
    round_trips('(skg (node (id x) (viewStats (sourceHerald ⌂:public) '
                .. 'cycle (overridesHere 4))))')
    round_trips('(skg (node (id x) (rels "2(1,1)L 3S")))')
  end)

  it('escapes strings exactly as the server does', function ()
    assert.are.equal('"a \\"b\\" c\\\\d"',
                     sexpr.to_string('a "b" c\\d'))
  end)

  it('prints pairs with the dot notation the server parses', function ()
    assert.are.equal('(request . "verify connection")',
      sexpr.to_string(
        sexpr.pair(sexpr.symbol('request'), 'verify connection')))
  end)

  it('prints integers without decimal points', function ()
    assert.are.equal('(id 7)',
      sexpr.to_string({ sexpr.symbol('id'), 7 }))
  end)

  it('prints the empty list readably', function ()
    assert.are.equal('()', sexpr.to_string(sexpr.NIL))
    assert.are.equal(sexpr.NIL, read1(sexpr.to_string(sexpr.NIL)))
  end)
end)

describe('skg.sexpr.parse on the pinned herald-rules fixture', function ()
  it('round-trips the whole rule table', function ()
    local fixture_path =
      _G.skg_test_repo_root() .. '/tests/elisp/herald-rules.sexp'
    local handle = assert(io.open(fixture_path, 'r'))
    local text = handle:read('*a')
    handle:close()
    local rules = read1(text)
    assert.is_true(sexpr.is_list(rules))
    assert.are.equal(sexpr.symbol('skg'), rules[1])
    assert.are.same(rules, read1(sexpr.to_string(rules)))
  end)
end)

describe('skg.sexpr.parse.find_sexp_end', function ()
  -- Mirrors the find-sexp-end coverage in test-skg-sexpr-search.el.
  it('finds the end of a simple sexp', function ()
    assert.are.equal(3, sexpr.find_sexp_end('(a)'))
    assert.are.equal(14, sexpr.find_sexp_end('(skg (node x)) title'))
  end)

  it('handles herald characters that confuse Emacs forward-sexp',
     function ()
    local line = '(skg (node (id 7) (viewStats (sourceHerald ⌂:pub))))'
    assert.are.equal(#line, sexpr.find_sexp_end(line))
  end)

  it('returns nil on unbalanced text', function ()
    assert.is_nil(sexpr.find_sexp_end('(a (b)'))
    assert.is_nil(sexpr.find_sexp_end('no parens here'))
  end)

  it('ignores parens inside strings', function ()
    local line = '(skg (node (rels ")))((("))) tail'
    local ending = sexpr.find_sexp_end(line)
    assert.are.equal('(skg (node (rels ")))(((")))',
                     line:sub(1, ending))
  end)

  it('respects a start position', function ()
    local text = 'junk (a) (b)'
    assert.are.equal(8, sexpr.find_sexp_end(text, 5))
  end)
end)

describe('skg.sexpr.parse.atom_text', function ()
  it('renders atoms uniformly', function ()
    assert.are.equal('foo', sexpr.atom_text(sexpr.symbol('foo')))
    assert.are.equal('bar', sexpr.atom_text('bar'))
    assert.are.equal('7', sexpr.atom_text(7))
    assert.has_error(function () sexpr.atom_text({}) end)
  end)
end)
