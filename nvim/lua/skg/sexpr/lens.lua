-- PURPOSE: Transform s-expressions using an abstract, declarative set
-- of pattern-matching rules. Currently, it is used to turn a text-only
-- metadata sexp into a shorter, colorful visual representation, but it
-- might turn out to be useful for other things.
-- The Lua port of elisp/skg-sexpr/skg-lens.el.
--
-- TOKENS. The elisp engine emits propertized strings: an 'skg-color'
-- text property on character ranges, and an 'skg-abut' property on a
-- token's first character. Lua strings carry no properties, so here a
-- token is a table
--     { chunks = { { text = STRING, color = COLOR|nil }, ... },
--       abut = BOOLEAN }
-- which is exactly the shape nvim virtual text wants (a chunk list
-- with per-chunk highlights); the herald renderer maps colors to
-- highlight groups at display time. Rule semantics are unchanged:
-- see the elisp docstring of 'skg-transform-sexp-flat', reproduced in
-- spirit across the functions below, and the test suite, which
-- demonstrates the rule grammar perhaps better than any explanation.

local parse = require('skg.sexpr.parse')
local compare = require('skg.sexpr.compare')

local M = {}

local ANY = parse.symbol('ANY')
local IT = parse.symbol('IT')
local ABUT = parse.symbol('ABUT')
local INTERC = parse.symbol('INTERC')

local color_keywords = {
  [parse.symbol('RED')] = true,
  [parse.symbol('GREEN')] = true,
  [parse.symbol('BLUE')] = true,
  [parse.symbol('YELLOW')] = true,
  [parse.symbol('ORANGE')] = true }

---@param value any
---@return boolean
function M.is_color_keyword (value)
  return color_keywords[value] == true
end

-- ── tokens ─────────────────────────────────────────────────────────

---@param text string
---@param color table|nil a color keyword symbol
---@return table a single-chunk token
function M.token_of (text, color)
  return { chunks = { { text = text, color = color } }, abut = false }
end

---@param token table
---@return string the token's full text, colors dropped
function M.token_text (token)
  local pieces = {}
  for _, chunk in ipairs(token.chunks) do
    table.insert(pieces, chunk.text) end
  return table.concat(pieces)
end

---The color at 1-based byte position POSITION of TOKEN, or nil.
---(The test-suite analog of elisp's get-text-property on skg-color;
---the herald renderer iterates chunks directly instead.)
---@param token table
---@param position integer
---@return table|nil
function M.color_at (token, position)
  local passed = 0
  for _, chunk in ipairs(token.chunks) do
    if position <= passed + #chunk.text then return chunk.color end
    passed = passed + #chunk.text
  end
  return nil
end

---@param tokens table[]
---@return string[] just the texts, for equal-like comparisons
function M.token_texts (tokens)
  local texts = {}
  for _, token in ipairs(tokens) do
    table.insert(texts, M.token_text(token)) end
  return texts
end

-- ── the engine ─────────────────────────────────────────────────────

---Apply RULES to OBJECT, generating a list of output tokens. No side
---effects. Both OBJECT and RULES are nested sexps whose first element
---at any level is an atom (the 'label'), except that a color
---directive may precede a rule's label. See the file header and the
---test suite for the rule grammar (ANY, IT, ABUT, INTERC, colors).
---@param object any
---@param rules any
---@return table[] tokens
function M.transform_sexp_flat (object, rules)
  local header = parse.is_list(rules) and M.rule_header(rules) or nil
  if not parse.is_list(object)
     or not header
     or not compare.values_equal(object[1], header.label) then
    return {} end
  return M.transform_from(object, header, nil)
end

---Parse RULE once and classify it.
---Grammar:
---  RULE        ::= [COLOR] (INTERC-BODY | SIMPLE-BODY)
---  INTERC-BODY ::= INTERC SEP [LABEL] CHILDREN...
---  SIMPLE-BODY ::= LABEL [ABUT] CHILDREN...
---Classification for simple bodies: 'non_leaf' if any child is a
---list; 'any_leaf' if the label is ANY (and it is a leaf);
---'atomic_leaf' otherwise.
---@param rule any[]
---@return table header {kind, color, label, separator, abut, children}
function M.rule_header (rule)
  local index = 1
  local color = nil
  if M.is_color_keyword(rule[index]) then
    color = rule[index]
    index = index + 1 end
  if rule[index] == INTERC then
    local separator = rule[index + 1]
    index = index + 2
    local label = nil
    local candidate = rule[index]
    if parse.is_symbol(candidate)
       and not M.is_color_keyword(candidate) then
      label = candidate
      index = index + 1 end
    return { kind = 'interc', color = color, label = label,
             separator = separator, abut = false,
             children = M.elements_from(rule, index) }
  end
  local label = rule[index]
  index = index + 1
  local abut = false
  if rule[index] == ABUT then
    abut = true
    index = index + 1 end
  local children = M.elements_from(rule, index)
  local kind = 'atomic_leaf'
  for _, child in ipairs(children) do
    if parse.is_list(child) then
      kind = 'non_leaf'
      break end
  end
  if kind == 'atomic_leaf' and label == ANY then kind = 'any_leaf' end
  return { kind = kind, color = color, label = label,
           separator = nil, abut = abut, children = children }
end

---Transform OBJECT using HEADER (a parsed rule) with CURRENT_COLOR.
---String-literal children of the header are collected into a prefix.
---When any list child fires, the prefix is concatenated (no
---separator) before each of its outputs. When no list child fires but
---the prefix is non-empty, the prefix is emitted alone -- so rules
---like (RED deleted "DELETED" (id) (source)) serve as a label for the
---structure even when their sub-rules are vacuous.
---@param object any[]
---@param header table
---@param current_color table|nil
---@return table[] tokens
function M.transform_from (object, header, current_color)
  local new_color = header.color or current_color
  local prefix = M.string_children_concatenated(header.children)
  local results = {}
  for _, rule_child in ipairs(header.children) do
    if parse.is_list(rule_child) then
      for _, token in ipairs(
          M.dispatch(object, rule_child, new_color)) do
        table.insert(results, token) end
    end
  end
  if prefix == '' then return results end
  if #results == 0 then
    return { M.token_of(prefix, new_color) } end
  local prefixed = {}
  for _, token in ipairs(results) do
    table.insert(prefixed, M.with_prefix(token, prefix, new_color)) end
  return prefixed
end

---PREFIX prepended to TOKEN as a colored chunk. The result loses any
---abut flag, matching elisp, where concatenation moved the skg-abut
---property off the first character and renderers read only that.
---@param token table
---@param prefix string
---@param color table|nil
---@return table
function M.with_prefix (token, prefix, color)
  local chunks = { { text = prefix, color = color } }
  for _, chunk in ipairs(token.chunks) do table.insert(chunks, chunk) end
  return { chunks = chunks, abut = false }
end

---Dispatch RULE_CHILD application to OBJECT with CURRENT_COLOR.
---@param object any
---@param rule_child any
---@param current_color table|nil
---@return table[] tokens
function M.dispatch (object, rule_child, current_color)
  if not parse.is_list(rule_child) then return {} end
  local header = M.rule_header(rule_child)
  if header.kind == 'interc' then
    return M.apply_interc(object, header, current_color) end
  if header.kind == 'any_leaf' then
    return M.apply_any_leaf(object, header, current_color) end
  if header.kind == 'atomic_leaf' then
    return M.apply_ordinary_leaf(object, header, current_color) end
  return M.apply_non_leaf(object, header, current_color)
end

---Apply an INTERC HEADER to OBJECT. With a LABEL: one combined token
---per child of OBJECT matching the label. Without one: the sub-rules
---run against OBJECT itself, at most one token. Each list sub-rule
---contributes one slot (the concatenation of its outputs, possibly
---empty); slots are joined with SEPARATOR; a literal-string prefix is
---prepended; the token is suppressed if every slot is empty. The
---separator and prefix take the rule's inherited color context;
---each slot preserves its sub-rule's own colors per chunk.
---@param object any[]
---@param header table
---@param current_color table|nil
---@return table[] tokens
function M.apply_interc (object, header, current_color)
  local new_color = header.color or current_color
  local prefix = M.string_children_concatenated(header.children)
  local targets
  if header.label then
    targets = M.object_children_with_label(object, header.label)
  else
    targets = { object } end
  local results = {}
  for _, target in ipairs(targets) do
    local slots = {}
    local any_slot_nonempty = false
    for _, rule_child in ipairs(header.children) do
      if parse.is_list(rule_child) then
        local slot_chunks = {}
        for _, token in ipairs(
            M.dispatch(target, rule_child, new_color)) do
          for _, chunk in ipairs(token.chunks) do
            table.insert(slot_chunks, chunk) end
        end
        table.insert(slots, slot_chunks)
        for _, chunk in ipairs(slot_chunks) do
          if #chunk.text > 0 then any_slot_nonempty = true end end
      end
    end
    if any_slot_nonempty then
      table.insert(results,
        M.interc_token(prefix, header.separator, slots, new_color))
    end
  end
  return results
end

---Build an INTERC token: PREFIX + JOIN(SLOTS, SEPARATOR). PREFIX and
---SEPARATOR both carry COLOR; each slot's own chunk colors survive.
---@param prefix string
---@param separator string
---@param slots table[][] chunk lists
---@param color table|nil
---@return table
function M.interc_token (prefix, separator, slots, color)
  local chunks = {}
  if #prefix > 0 then
    table.insert(chunks, { text = prefix, color = color }) end
  for i, slot_chunks in ipairs(slots) do
    if i > 1 and #separator > 0 then
      table.insert(chunks, { text = separator, color = color }) end
    for _, chunk in ipairs(slot_chunks) do
      table.insert(chunks, chunk) end
  end
  return { chunks = chunks, abut = false }
end

---@param object any[]
---@param header table
---@param current_color table|nil
---@return table[] tokens
function M.apply_non_leaf (object, header, current_color)
  local results = {}
  for _, match in ipairs(
      M.object_children_with_label(object, header.label)) do
    for _, token in ipairs(
        M.transform_from(match, header, current_color)) do
      table.insert(results, token) end
  end
  return results
end

---@param object any[]
---@param header table
---@param current_color table|nil
---@return table[] tokens
function M.apply_ordinary_leaf (object, header, current_color)
  local color = header.color or current_color
  local results = {}
  if #header.children == 0 then return results end
  for _ in ipairs(
      M.object_atomic_matches(object, header.label)) do
    local token = M.joined_token(header.children, color)
    token.abut = header.abut
    table.insert(results, token)
  end
  return results
end

---@param object any[]
---@param header table
---@param current_color table|nil
---@return table[] tokens
function M.apply_any_leaf (object, header, current_color)
  local color = header.color or current_color
  local tokens = header.children
  if #tokens == 0 then return {} end
  local uses_it = false
  for _, token in ipairs(tokens) do
    if token == IT then uses_it = true break end
  end
  if not uses_it then
    local token = M.joined_token(tokens, color)
    token.abut = header.abut
    return { token } end
  local results = {}
  for i = 2, #object do
    local tail_value = object[i]
    local substituted = {}
    for _, token in ipairs(tokens) do
      table.insert(substituted, token == IT and tail_value or token) end
    local token = M.joined_token(substituted, color)
    token.abut = header.abut
    table.insert(results, token)
  end
  return results
end

---Join rule-child atoms with ':' into one COLOR-colored token.
---@param values any[]
---@param color table|nil
---@return table
function M.joined_token (values, color)
  local pieces = {}
  for _, value in ipairs(values) do
    table.insert(pieces, M.value_display_text(value)) end
  return M.token_of(table.concat(pieces, ':'), color)
end

---Symbols and strings are common (rule child labels); numbers reach
---here via ANY/IT matches on numeric atoms in the input (e.g. counts
---like '(containers 3)'). Anything else renders as its sexp text.
---@param value any
---@return string
function M.value_display_text (value)
  if parse.is_symbol(value) then return value.name end
  if type(value) == 'string' then return value end
  if type(value) == 'number' then return parse.number_to_text(value) end
  local ok, rendered = pcall(parse.to_string, value)
  return ok and rendered or vim.inspect(value)
end

---@param children any[]
---@return string the concatenation of the string literals
function M.string_children_concatenated (children)
  local pieces = {}
  for _, child in ipairs(children) do
    if type(child) == 'string' then table.insert(pieces, child) end
  end
  return table.concat(pieces)
end

---@param object any[]
---@param label any
---@return any[][] the list children of OBJECT headed by LABEL
function M.object_children_with_label (object, label)
  local matches = {}
  for i = 2, #object do
    local element = object[i]
    if parse.is_list(element)
       and compare.values_equal(element[1], label) then
      table.insert(matches, element) end
  end
  return matches
end

---@param object any[]
---@param label any
---@return any[] the atom elements of OBJECT equal to LABEL
function M.object_atomic_matches (object, label)
  local matches = {}
  for i = 2, #object do
    local element = object[i]
    if not parse.is_list(element)
       and compare.values_equal(element, label) then
      table.insert(matches, element) end
  end
  return matches
end

---@param list any[]
---@param from integer
---@return any[] the elements of LIST from index FROM on
function M.elements_from (list, from)
  local result = {}
  for i = from, #list do table.insert(result, list[i]) end
  return result
end

return M
