-- PURPOSE: Edit s-expressions with 'DELETE', 'REPLACE', 'ENSURE', and
-- (the default, so no keyword) merge operations.
-- The Lua port of elisp/skg-sexpr/skg-sexpr-dsl-for-edits.el. Purely
-- functional: inputs are never mutated; every result is fresh.

local parse = require('skg.sexpr.parse')
local compare = require('skg.sexpr.compare')

local M = {}

local DELETE = parse.symbol('DELETE')
local REPLACE = parse.symbol('REPLACE')
local ENSURE = parse.symbol('ENSURE')

---Is VALUE a list headed by the operation symbol OPERATION?
---@param value any
---@param operation table an interned symbol
---@return boolean
local function is_operation (value, operation)
  return parse.is_list(value) and value[1] == operation
end

---Edit TARGET s-expression according to INSTRUCTIONS.
---TARGET and INSTRUCTIONS are s-expressions (they may start with
---different symbols).
---
---INSTRUCTIONS can contain four types of operations:
---1. DELETE: (DELETE elem1 elem2 ...) - removes specified elements
---2. REPLACE: (REPLACE old new) - replaces old with new
---3. ENSURE: (ENSURE elem) - replaces if found, inserts if not
---4. Merge: any other list/atom - recursively merges into target
---
---For DELETE/REPLACE patterns:
--- - Atom: matches exact atom
--- - List (x): matches any list whose car is x
---
---Special cases:
--- - If target is nil or the empty list: ignore DELETE/REPLACE,
---   ENSURE works normally, merge the rest (elisp's nil target; here
---   both Lua nil and parse.NIL play that role)
--- - If target is a list whose head is the empty list: same treatment
---   (elisp's "empty list target", a list whose car is nil)
--- - If target and instructions have different cars: return target
---   unchanged
---@param target any
---@param instructions any
---@return any the edited s-expression
function M.edit_nested_sexp (target, instructions)
  local target_is_nil = target == nil or parse.is_nil(target)
  local target_head_is_nil =
    not target_is_nil and parse.is_list(target)
    and (target[1] == nil or parse.is_nil(target[1]))
  if target_is_nil or target_head_is_nil then
    return M.merge_instructions_into_nothing(instructions) end
  if not compare.values_equal(target[1], instructions[1]) then
    return target end
  local elements = {}
  for i = 2, #target do table.insert(elements, target[i]) end
  for i = 2, #instructions do
    elements = M.process_single_instruction(elements, instructions[i])
  end
  return M.with_head(instructions[1], elements)
end

---Process INSTRUCTIONS against an absent target: DELETE and REPLACE
---are ignored, ENSURE works normally, everything else merges. The
---result starts with the instructions' own head.
---@param instructions any
---@return any
function M.merge_instructions_into_nothing (instructions)
  local elements = {}
  for i = 2, #instructions do
    local instruction = instructions[i]
    if is_operation(instruction, DELETE)
       or is_operation(instruction, REPLACE) then
      -- skipped entirely on an absent target
    elseif is_operation(instruction, ENSURE) then
      elements = M.apply_ensure(elements, instruction[2])
    else
      elements = M.apply_merge(elements, instruction) end
  end
  return M.with_head(instructions[1], elements)
end

---Dispatch one INSTRUCTION against ELEMENTS (a target's tail).
---@param elements any[]
---@param instruction any
---@return any[]
function M.process_single_instruction (elements, instruction)
  if is_operation(instruction, DELETE) then
    for i = 2, #instruction do
      elements = M.delete_single_element(elements, instruction[i]) end
    return elements end
  if is_operation(instruction, REPLACE) then
    return M.apply_replace(elements, instruction[2], instruction[3]) end
  if is_operation(instruction, ENSURE) then
    return M.apply_ensure(elements, instruction[2]) end
  return M.apply_merge(elements, instruction)
end

---Remove the elements matching DELETE_SPEC: an atom deletes its exact
---(deep-equal) matches; a singleton list (x) deletes any list whose
---head is x.
---@param elements any[]
---@param delete_spec any
---@return any[]
function M.delete_single_element (elements, delete_spec)
  local kept = {}
  if parse.is_list(delete_spec) then
    local key = delete_spec[1]
    for _, element in ipairs(elements) do
      if not (parse.is_list(element)
              and compare.values_equal(element[1], key)) then
        table.insert(kept, element) end
    end
  else
    for _, element in ipairs(elements) do
      if not compare.values_equal(element, delete_spec) then
        table.insert(kept, element) end
    end
  end
  return kept
end

---Replace OLD_SPEC's matches with NEW_VALUE: an atom spec matches
---that exact atom; a singleton list (x) matches any list headed by x.
---@param elements any[]
---@param old_spec any
---@param new_value any
---@return any[]
function M.apply_replace (elements, old_spec, new_value)
  local result = {}
  local spec_is_list = parse.is_list(old_spec)
  local key = spec_is_list and old_spec[1] or nil
  for _, element in ipairs(elements) do
    local matches
    if spec_is_list then
      matches = parse.is_list(element)
                and compare.values_equal(element[1], key)
    else
      matches = compare.values_equal(element, old_spec) end
    table.insert(result, matches and new_value or element)
  end
  return result
end

---Ensure ENSURE_SPEC exists among ELEMENTS. An atom is appended
---unless already present; a list (x ...) replaces any list headed by
---x, or is appended if none is.
---@param elements any[]
---@param ensure_spec any
---@return any[]
function M.apply_ensure (elements, ensure_spec)
  if parse.is_list(ensure_spec) then
    local key = ensure_spec[1]
    if M.find_list_with_head(elements, key) then
      return M.apply_replace(elements, { key }, ensure_spec) end
    return M.appended(elements, ensure_spec)
  end
  if M.member(elements, ensure_spec) then return M.copied(elements) end
  return M.appended(elements, ensure_spec)
end

---Merge MERGE_ELEMENT into ELEMENTS. An atom is appended unless
---already present. A list (key ...) recursively edits every list
---headed by key -- then singleton lists (a label with no remaining
---content, e.g. emptied by nested DELETEs) are dropped from the whole
---level -- or is appended if no such list exists.
---@param elements any[]
---@param merge_element any
---@return any[]
function M.apply_merge (elements, merge_element)
  if parse.is_list(merge_element) then
    local key = merge_element[1]
    if M.find_list_with_head(elements, key) then
      local recursed = {}
      for _, element in ipairs(elements) do
        if parse.is_list(element)
           and compare.values_equal(element[1], key) then
          table.insert(recursed,
                       M.edit_nested_sexp(element, merge_element))
        else
          table.insert(recursed, element) end
      end
      return M.without_empty_sections(recursed)
    end
    return M.appended(elements, merge_element)
  end
  if M.member(elements, merge_element) then return M.copied(elements) end
  return M.appended(elements, merge_element)
end

---Drop singleton lists (a label and nothing else) from ELEMENTS.
---@param elements any[]
---@return any[]
function M.without_empty_sections (elements)
  local kept = {}
  for _, element in ipairs(elements) do
    if not (parse.is_list(element) and #element == 1) then
      table.insert(kept, element) end
  end
  return kept
end

-- ── small functional helpers ───────────────────────────────────────

---@param head any
---@param elements any[]
---@return any[]
function M.with_head (head, elements)
  local result = { head }
  for _, element in ipairs(elements) do table.insert(result, element) end
  return result
end

---@param elements any[]
---@param key any
---@return any|nil the first list in ELEMENTS whose head is KEY
function M.find_list_with_head (elements, key)
  for _, element in ipairs(elements) do
    if parse.is_list(element)
       and compare.values_equal(element[1], key) then
      return element end
  end
  return nil
end

---Deep-equal membership, the stand-in for elisp 'member'.
---@param elements any[]
---@param value any
---@return boolean
function M.member (elements, value)
  for _, element in ipairs(elements) do
    if compare.values_equal(element, value) then return true end
  end
  return false
end

---@param elements any[]
---@return any[] a shallow copy
function M.copied (elements)
  local result = {}
  for _, element in ipairs(elements) do table.insert(result, element) end
  return result
end

---@param elements any[]
---@param value any
---@return any[] a shallow copy with VALUE appended
function M.appended (elements, value)
  local result = M.copied(elements)
  table.insert(result, value)
  return result
end

return M
