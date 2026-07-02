-- Mirrors tests/elisp/test-skg-edit-nested-sexp.el.

local sexpr = require('skg.sexpr.parse')
local edit_dsl = require('skg.sexpr.edit_dsl')

---Run the DSL on textual sexps and compare printed results, so
---failures read like the elisp test expectations.
local function edits_to (target_text, instructions_text, expected_text)
  local target = target_text and sexpr.read(target_text) or nil
  local result = edit_dsl.edit_nested_sexp(
    target, sexpr.read(instructions_text))
  assert.are.equal(expected_text, sexpr.to_string(result))
end

describe('skg.sexpr.edit_dsl.edit_nested_sexp', function ()
  it('deletes from the top level', function ()
    edits_to('(skg a b c d)', '(skg (DELETE b c))', '(skg a d)')
  end)

  it('need not start with skg', function ()
    edits_to('(smurf a b c d)', '(smurf (DELETE b c))', '(smurf a d)')
  end)

  it('respects nesting when deleting', function ()
    edits_to('(skg a (b c) (d e) f)',
             '(skg (DELETE (b) d e f))',
             '(skg a (d e))')
  end)

  it('deletes and merges in one instruction set', function ()
    edits_to('(skg a b (c d))',
             '(skg (DELETE b) (c e))',
             '(skg a (c d e))')
  end)

  it('replaces a bare atom', function ()
    edits_to('(skg a b c)', '(skg (REPLACE b x))', '(skg a x c)')
  end)

  it('replaces a bare atom under any head symbol', function ()
    edits_to('(smurf a b c)', '(smurf (REPLACE b x))', '(smurf a x c)')
  end)

  it('replaces a nested list by its head', function ()
    edits_to('(skg (a b) (c d) (e f))',
             '(skg (REPLACE (c) (c x)))',
             '(skg (a b) (c x) (e f))')
  end)

  it('handles many operations across many branches', function ()
    edits_to(
      '(skg (a a1 a2 (a3 a31 a32) (a4 a41 a42))'
      .. ' (b b1 b2 (b3 b31 b32) (b4 b41 b42))'
      .. ' (c c1 c2 (c3 c31 c32) (c4 c41 c42))'
      .. ' (d d1 d2 (d3 d31 d32) (d4 d41 d42)))',
      '(skg (a (DELETE a1 (a4)) (a3 (DELETE a31)))'
      .. ' (b (REPLACE b1 x) (REPLACE (b4) (b4 z)) (b3 (REPLACE b31 y)))'
      .. ' (c NOW (c3 HEY) (c4 THERE))'
      .. ' (d (ENSURE d5) (ENSURE (d6 y)) (ENSURE (d3 x))))',
      '(skg (a a2 (a3 a32))'
      .. ' (b x b2 (b3 y b32) (b4 z))'
      .. ' (c c1 c2 (c3 c31 c32 HEY) (c4 c41 c42 THERE) NOW)'
      .. ' (d d1 d2 (d3 x) (d4 d41 d42) d5 (d6 y)))')
  end)

  it('treats a nil target as merge-only (ENSURE unwrapped)', function ()
    edits_to(nil,
             '(skg (DELETE c) (REPLACE (a) (a x))'
             .. ' (ENSURE (a a1)) (ENSURE (d d1))'
             .. ' (a a2) (c c2) (ENSURE (c c3)))',
             '(skg (a a1 a2) (d d1) (c c3))')
  end)

  it('returns target unchanged on differing head symbols', function ()
    edits_to('(a)', '(b c)', '(a)')
  end)

  it('treats an empty-list target like nil', function ()
    edits_to('()',
             '(b (c d) (ENSURE e) (ENSURE (f g)))',
             '(b (c d) e (f g))')
  end)

  it('does not mutate its inputs', function ()
    -- No elisp counterpart; Lua tables make this worth pinning.
    local target = sexpr.read('(skg a (b c))')
    local instructions = sexpr.read('(skg (DELETE a) (b d))')
    edit_dsl.edit_nested_sexp(target, instructions)
    assert.are.equal('(skg a (b c))', sexpr.to_string(target))
    assert.are.equal('(skg (DELETE a) (b d))',
                     sexpr.to_string(instructions))
  end)
end)
