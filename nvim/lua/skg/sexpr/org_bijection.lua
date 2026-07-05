-- PURPOSE: Roughly-bijective transformations between atom-headed
-- paren-terse sexps and org text.
-- The Lua port of elisp/skg-sexpr/skg-sexpr-org-bijection.el.
--
-- INPUT REQUIREMENTS
-- - An input sexp must be:
--   - A non-empty list
--   - "Atom-headed": every list (and sublist) has a bare atom as its
--     first member, including at the top level.
--   - "Paren-terse": no proper sub-sexp wraps a single atom in parens.
-- - Input org text must have:
--   - At least one headline
--   - Exactly one root (level-1) headline
--   - No empty headline text
--   - "Child-homogeneity": the first child cannot be indented more
--     than one level deeper than its parent.
--
-- TRANSFORMATION NOTES
-- - Body text in org is discarded (hence "roughly" bijective).
-- - Multi-word headlines become symbols with spaces (constructed
--   programmatically; such symbols cannot round-trip through the
--   sexp READER, but the edit-buffer pipeline never needs them to).
-- - Childless non-root headlines become bare atoms, not lists.
--
-- ENTRY POINTS: sexp_to_org, org_to_sexp.

local parse = require('skg.sexpr.parse')

local M = {}

---Like elisp 'atom': anything but a list -- except that the empty
---list plays elisp-nil's role and nil counted as an atom there.
---@param value any
---@return boolean
local function is_atom (value)
  return not parse.is_list(value) or parse.is_nil(value)
end

-- ── sexp to org ────────────────────────────────────────────────────

---Convert SEXP to org-mode text. SEXP must be a list that is both
---atom-headed and paren-terse; errors otherwise.
---@param sexp any
---@return string
function M.sexp_to_org (sexp)
  if not parse.is_list(sexp) then
    error('sexp_to_org: input must be a list, got '
          .. vim.inspect(sexp)) end
  if #sexp == 0 then
    error('sexp_to_org: input must be a non-empty list') end
  if not M.atom_headed_p(sexp) then
    error('sexp_to_org: input is not atom-headed: '
          .. parse.to_string(sexp)) end
  if not M.paren_terse_p(sexp) then
    error('sexp_to_org: input is not paren-terse: '
          .. parse.to_string(sexp)) end
  return M.convert_sexp(sexp, 1)
end

---Is SEXP atom-headed? (It, and every sublist at any level, has a
---bare atom as its first member.)
---@param sexp any
---@return boolean
function M.atom_headed_p (sexp)
  if is_atom(sexp) then return true end
  if #sexp == 0 then return false end
  if not is_atom(sexp[1]) then return false end
  for i = 2, #sexp do
    if not M.atom_headed_p(sexp[i]) then return false end
  end
  return true
end

---Is SEXP paren-terse? (No proper sub-sexp puts parens around a
---single atom.)
---@param sexp any
---@return boolean
function M.paren_terse_p (sexp)
  if is_atom(sexp) then return true end
  for i = 2, #sexp do
    if not M.paren_terse_proper_subsexp_p(sexp[i]) then return false end
  end
  return true
end

---Paren-terseness for a proper subsexp: it cannot be a list
---containing a single atom.
---@param element any
---@return boolean
function M.paren_terse_proper_subsexp_p (element)
  if is_atom(element) then return true end
  if #element == 1 and is_atom(element[1]) then return false end
  for i = 2, #element do
    if not M.paren_terse_proper_subsexp_p(element[i]) then
      return false end
  end
  return true
end

---Convert validated SEXP to org text starting at headline LEVEL.
---@param sexp any[]
---@param level integer
---@return string
function M.convert_sexp (sexp, level)
  local lines = { M.make_headline(level, sexp[1]) }
  for i = 2, #sexp do
    local element = sexp[i]
    if parse.is_list(element) and not parse.is_nil(element) then
      table.insert(lines, M.convert_sexp(element, level + 1))
    else
      table.insert(lines, M.make_headline(level + 1, element)) end
  end
  return table.concat(lines, '\n')
end

---An org headline at LEVEL whose text renders TEXT (a symbol, string
---or number).
---@param level integer
---@param text any
---@return string
function M.make_headline (level, text)
  return string.rep('*', level) .. ' ' .. parse.atom_text(text)
end

-- ── org to sexp ────────────────────────────────────────────────────

---Convert ORG_TEXT to a sexp. Discards all non-headline lines before
---transforming. Errors if there are no headlines, multiple root
---headlines, a level jump greater than 1, or an empty headline.
---@param org_text string
---@return any
function M.org_to_sexp (org_text)
  local headlines = M.extract_headlines(org_text)
  if #headlines == 0 then
    error('org_to_sexp: no headlines found in input') end
  M.validate_single_root(headlines)
  M.validate_no_empty_headlines(headlines)
  M.validate_child_homogeneity(headlines)
  local sexp = M.build_tree(headlines, 1, 1)
  return sexp
end

---The headlines of ORG_TEXT as a list of {level, text} pairs,
---non-headline content discarded.
---@param org_text string
---@return table[]
function M.extract_headlines (org_text)
  local headlines = {}
  for line in (org_text .. '\n'):gmatch('([^\n]*)\n') do
    local stars, text = line:match('^(%*+) +(.+)$')
    if stars then
      table.insert(headlines, { level = #stars, text = text }) end
  end
  return headlines
end

---@param headlines table[]
function M.validate_single_root (headlines)
  if headlines[1].level ~= 1 then
    error(string.format(
      'org_to_sexp: first headline must be at level 1, got level %d',
      headlines[1].level)) end
  for i = 2, #headlines do
    if headlines[i].level == 1 then
      error('org_to_sexp: multiple root headlines found') end
  end
end

---@param headlines table[]
function M.validate_no_empty_headlines (headlines)
  for _, headline in ipairs(headlines) do
    if headline.text:match('^%s*$') then
      error(string.format(
        'org_to_sexp: empty headline text at level %d',
        headline.level)) end
  end
end

---@param headlines table[]
function M.validate_child_homogeneity (headlines)
  local previous_level = headlines[1].level
  for i = 2, #headlines do
    local level = headlines[i].level
    if level > previous_level + 1 then
      error(string.format(
        'org_to_sexp: level jump from %d to %d (max allowed is 1)',
        previous_level, level)) end
    previous_level = level
  end
end

---TEXT as an atom, preserving numbers: if TEXT renders back
---identically from its numeric reading, it becomes a number,
---otherwise a symbol (possibly containing spaces).
---@param text string
---@return any
function M.text_to_atom (text)
  local number = tonumber(text)
  if number ~= nil and text == parse.number_to_text(number) then
    return number end
  return parse.symbol(text)
end

---Build a sexp tree from HEADLINES[INDEX...] at EXPECTED_LEVEL.
---Returns the sexp and the index of the first unconsumed headline.
---A non-root headline with no children becomes a bare atom; the root
---or any headline with children becomes a list.
---@param headlines table[]
---@param index integer
---@param expected_level integer
---@return any sexp
---@return integer next_index
function M.build_tree (headlines, index, expected_level)
  local head_atom = M.text_to_atom(headlines[index].text)
  local children = {}
  local child_level = expected_level + 1
  local i = index + 1
  while i <= #headlines and headlines[i].level >= child_level do
    if headlines[i].level == child_level then
      local child, next_index =
        M.build_tree(headlines, i, child_level)
      table.insert(children, child)
      i = next_index
    else
      i = i + 1 end
  end
  if #children > 0 then
    local sexp = { head_atom }
    for _, child in ipairs(children) do table.insert(sexp, child) end
    return sexp, i end
  if expected_level == 1 then return { head_atom }, i end
  return head_atom, i
end

return M
