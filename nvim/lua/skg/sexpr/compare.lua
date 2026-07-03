-- PURPOSE: Compare s-expressions.
-- The Lua port of elisp/skg-sexpr/skg-compare-sexpr.el, plus the
-- deep-equality helper that elisp got for free as 'equal'.

local parse = require('skg.sexpr.parse')

local M = {}

---Deep equality over sexp values: atoms by value (symbols are
---interned, so '==' suffices), lists and pairs element-wise. The
---stand-in for elisp 'equal'.
---@param left any
---@param right any
---@return boolean
function M.values_equal (left, right)
  if left == right then return true end
  if parse.is_pair(left) and parse.is_pair(right) then
    return M.values_equal(left.car, right.car)
           and M.values_equal(left.cdr, right.cdr) end
  if parse.is_list(left) and parse.is_list(right) then
    if #left ~= #right then return false end
    for i = 1, #left do
      if not M.values_equal(left[i], right[i]) then return false end end
    return true end
  return false
end

---Is STRUCTURE a semantic subtree of OBJECT?
---Each path in STRUCTURE has to correspond to some path in OBJECT.
---
---Examples:
---  subtree_p((a b c), (a))             => true
---  subtree_p((a b c), (a c b))         => true
---  subtree_p((a b c), (a c))           => true
---  subtree_p((a b c), (c a b))         => false
---  subtree_p((a (b c d) e), (a e))     => true
---  subtree_p((a (b c d) e), (a (b)))   => true
---  subtree_p((a (b c d) e), (a (b c))) => true
---  subtree_p((a (b c d) e), (a (c b))) => false
---
---Order applies in each flat list only in so far as to distinguish
---the first element (always an atom) from the others, which are
---unordered relative to each other. Everything from the tail of each
---subexpr of STRUCTURE must correspond to something in OBJECT with
---the same 'label ancestry', where e.g. the label ancestry of x in
---(a (b c (d (e f) (g x) h i))) is (a b d g).
---@param object any
---@param structure any
---@return boolean
function M.subtree_p (object, structure)
  local structure_is_atom =
    not parse.is_list(structure) or parse.is_nil(structure)
  if structure_is_atom then
    -- Atoms must match exactly. (Elisp counts nil as an atom too;
    -- here the empty list plays nil's role.)
    return M.values_equal(object, structure) end
  if not parse.is_list(object) then return false end
  if not M.values_equal(object[1], structure[1]) then
    -- Heads must match exactly.
    return false end
  for i = 2, #structure do
    -- Each element in structure's tail must match some element in
    -- object's tail.
    local structure_element = structure[i]
    local found = false
    for j = 2, #object do
      if M.subtree_p(object[j], structure_element) then
        found = true
        break end
    end
    if not found then return false end
  end
  return true
end

return M
