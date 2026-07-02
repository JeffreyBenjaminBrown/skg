-- PURPOSE: Expand/strip default fields for ActiveNode metadata
-- editing. After sexp_to_org, expand inserts missing editable fields
-- with defaults; before org_to_sexp, strip removes default-valued
-- fields. The Lua port of elisp/skg-sexpr/skg-activeNode-defaults.el.
--
-- ENTRY POINTS: activeNode_sexp_p, expand_defaults_in_org,
-- strip_defaults_from_org, headlines_to_org.
--
-- Headlines here are the {level, text} tables produced by
-- skg.sexpr.org_bijection's extract_headlines.

local parse = require('skg.sexpr.parse')
local compare = require('skg.sexpr.compare')
local bijection = require('skg.sexpr.org_bijection')

local M = {}

---Canonical order for node fields. Fields not in this list go last.
M.canonical_field_order = {
  'id', 'source',
  'indef', 'parentIs', 'birth', 'editRequest', 'viewRequests' }

---Editable field names mapped to their default value text.
M.editable_defaults = {
  { name = 'indef', default = 'false (default)' },
  { name = 'parentIs', default = 'affected (default)' },
  { name = 'birth', default = 'unremarkable (default)' },
  { name = 'editRequest', default = 'none (default)' },
  { name = 'viewRequests', default = 'none (default)' } }

---@param name string
---@return string|nil the default value text for editable field NAME
function M.editable_default_for (name)
  for _, entry in ipairs(M.editable_defaults) do
    if entry.name == name then return entry.default end
  end
  return nil
end

---Is SEXP an ActiveNode sexp?
---@param sexp any
---@return boolean
function M.activeNode_sexp_p (sexp)
  return compare.subtree_p(sexp,
           { parse.symbol('skg'), { parse.symbol('node') } })
         or compare.subtree_p(sexp,
              { parse.symbol('skg'), parse.symbol('node') })
end

---Convert HEADLINES ({level, text} tables) to org text.
---@param headlines table[]
---@return string
function M.headlines_to_org (headlines)
  local lines = {}
  for _, headline in ipairs(headlines) do
    table.insert(lines,
      string.rep('*', headline.level) .. ' ' .. headline.text)
  end
  return table.concat(lines, '\n')
end

-- ── Expand ─────────────────────────────────────────────────────────

---Expand default fields in ORG_TEXT for ActiveNode metadata editing:
---reorder the '** node' section's fields to canonical order, insert
---missing editable fields with defaults, and expand bare boolean
---atoms to have a value child. If DEFAULT_SOURCE is given, insert it
---as the source default and mark a matching existing source value
---with '(default)'. If DISPLAY_TITLE is given and non-empty, prepend
---a display-only title group.
---@param org_text string
---@param default_source string|nil
---@param display_title string|nil
---@return string
function M.expand_defaults_in_org (org_text, default_source,
                                   display_title)
  local headlines = bijection.extract_headlines(org_text)
  local expanded = M.expand_headlines(headlines, default_source)
  local with_title = M.maybe_prepend_title(expanded, display_title)
  return M.headlines_to_org(with_title)
end

---@param headlines table[]
---@param display_title string|nil
---@return table[]
function M.maybe_prepend_title (headlines, display_title)
  if display_title and not display_title:match('^%s*$') then
    local result = { { level = 1, text = 'title' },
                     { level = 2, text = display_title } }
    for _, headline in ipairs(headlines) do
      table.insert(result, headline) end
    return result
  end
  return headlines
end

---@param headlines table[]
---@param default_source string|nil
---@return table[]
function M.expand_headlines (headlines, default_source)
  local node_index = M.find_node_headline(headlines)
  local child_level = headlines[node_index].level + 1
  local before_node = vim.list_slice(headlines, 1, node_index)
  local after_node = vim.list_slice(headlines, node_index + 1)
  local children, remainder =
    M.group_children(after_node, child_level)
  local expanded_children =
    M.expand_and_reorder(children, child_level, default_source)
  return M.concatenated(before_node, expanded_children, remainder)
end

---The index of the 'node' headline in HEADLINES; errors if absent.
---@param headlines table[]
---@return integer
function M.find_node_headline (headlines)
  for index, headline in ipairs(headlines) do
    if headline.text == 'node' then return index end
  end
  error("skg.activenode_defaults: no 'node' headline found")
end

---Split HEADLINES into the node's children and the remainder.
---Children are groups starting at CHILD_LEVEL (each group: the field
---headline plus its deeper descendants); the remainder begins at the
---first headline shallower than CHILD_LEVEL.
---@param headlines table[]
---@param child_level integer
---@return table[][] children
---@return table[] remainder
function M.group_children (headlines, child_level)
  local children = {}
  local current_group = nil
  local index = 1
  while index <= #headlines do
    local headline = headlines[index]
    if headline.level == child_level then
      if current_group then table.insert(children, current_group) end
      current_group = { headline }
      index = index + 1
    elseif headline.level > child_level then
      if current_group then
        table.insert(current_group, headline) end
      index = index + 1
    else
      break end
  end
  if current_group then table.insert(children, current_group) end
  return children, vim.list_slice(headlines, index)
end

---Reorder CHILDREN (headline groups) to canonical order and insert
---missing defaults. Returns a flat headline list.
---@param children table[][]
---@param child_level integer
---@param default_source string|nil
---@return table[]
function M.expand_and_reorder (children, child_level, default_source)
  local field_map = {}
  for _, group in ipairs(children) do
    table.insert(field_map, { name = group[1].text, group = group })
  end
  local function existing_entry (name)
    for _, entry in ipairs(field_map) do
      if entry.name == name then return entry end
    end
    return nil
  end
  local ordered = {}
  local seen = {}
  for _, field_name in ipairs(M.canonical_field_order) do
    local existing = existing_entry(field_name)
    if existing then
      seen[field_name] = true
      for _, headline in ipairs(
          M.maybe_expand_field(existing.group, child_level,
                               field_name, default_source)) do
        table.insert(ordered, headline) end
    elseif M.editable_default_for(field_name) then
      seen[field_name] = true
      table.insert(ordered, { level = child_level, text = field_name })
      table.insert(ordered, { level = child_level + 1,
                              text = M.editable_default_for(field_name) })
    elseif field_name == 'source' and default_source then
      seen[field_name] = true
      table.insert(ordered, { level = child_level, text = 'source' })
      table.insert(ordered, { level = child_level + 1,
                              text = default_source .. ' (default)' })
    end
  end
  for _, entry in ipairs(field_map) do
    -- Remaining fields not in canonical order (readonly stats etc.)
    if not seen[entry.name] then
      for _, headline in ipairs(entry.group) do
        table.insert(ordered, headline) end
    end
  end
  return ordered
end

---Expand GROUP for display: a bare 'indef' boolean gains a 'true'
---child; a source value matching DEFAULT_SOURCE gains ' (default)'.
---@param group table[]
---@param child_level integer
---@param field_name string
---@param default_source string|nil
---@return table[]
function M.maybe_expand_field (group, child_level, field_name,
                               default_source)
  if #group == 1 and field_name == 'indef' then
    return { group[1], { level = child_level + 1, text = 'true' } } end
  if field_name == 'source' and default_source and #group == 2 then
    local value = vim.trim(group[2].text)
    if value == default_source then
      return { group[1],
               { level = child_level + 1,
                 text = default_source .. ' (default)' } } end
  end
  return group
end

-- ── Strip ──────────────────────────────────────────────────────────

---Strip default fields from ORG_TEXT before converting back to a
---sexp: remove fields at their default value and collapse boolean
---true values back to bare atoms.
---@param org_text string
---@return string
function M.strip_defaults_from_org (org_text)
  local headlines = bijection.extract_headlines(org_text)
  return M.headlines_to_org(M.strip_headlines(headlines))
end

---@param headlines table[]
---@return table[]
function M.strip_headlines (headlines)
  local metadata_headlines = M.remove_title_headlines(headlines)
  local node_index = M.find_node_headline(metadata_headlines)
  local child_level = metadata_headlines[node_index].level + 1
  local before_node = vim.list_slice(metadata_headlines, 1, node_index)
  local after_node = vim.list_slice(metadata_headlines, node_index + 1)
  local children, remainder =
    M.group_children(after_node, child_level)
  local stripped_children = M.strip_children(children, child_level)
  return M.concatenated(before_node, stripped_children, remainder)
end

---Remove the display-only title group from HEADLINES, if present.
---@param headlines table[]
---@return table[]
function M.remove_title_headlines (headlines)
  if #headlines > 0 and headlines[1].level == 1
     and vim.trim(headlines[1].text) == 'title' then
    local index = 2
    while index <= #headlines and headlines[index].level > 1 do
      index = index + 1 end
    return vim.list_slice(headlines, index)
  end
  return headlines
end

---@param children table[][]
---@param child_level integer
---@return table[]
function M.strip_children (children, child_level)
  local result = {}
  for _, group in ipairs(children) do
    local field_name = vim.trim(group[1].text)
    local stripped = M.strip_one_field(group, field_name, child_level)
    if stripped then
      for _, headline in ipairs(stripped) do
        table.insert(result, headline) end
    end
  end
  return result
end

---Strip one field GROUP named FIELD_NAME. Returns nil to remove the
---field, or the headlines to keep.
---@param group table[]
---@param field_name string
---@param child_level integer
---@return table[]|nil
function M.strip_one_field (group, field_name, child_level)
  local value_text = nil
  if #group > 1 then value_text = vim.trim(group[2].text) end
  if field_name == 'indef' then
    if value_text == nil or M.default_false_p(value_text) then
      return nil end
    if value_text == 'true' then
      -- Collapse to bare atom (no children).
      return { { level = child_level, text = field_name } } end
    return group end
  if field_name == 'parentIs' then
    if value_text == nil or M.default_affected_p(value_text) then
      return nil end
    return group end
  if field_name == 'birth' then
    if value_text == nil or M.default_birth_p(value_text) then
      return nil end
    return group end
  if field_name == 'editRequest' then
    if value_text == nil or M.default_none_p(value_text) then
      return nil end
    if value_text == 'delete' then return group end
    if value_text == 'merge' then
      -- (merge ID) structured as: editRequest / merge / XYZ.
      -- Extract the ID, handling org links in the ID child.
      if #group > 2 then
        local id = M.extract_id_from_text(vim.trim(group[3].text))
        return { { level = child_level, text = field_name },
                 { level = child_level + 1, text = 'merge' },
                 { level = child_level + 2, text = id } } end
      return group end
    if value_text:sub(1, #'merge ') == 'merge ' then
      -- The user typed "merge <ID>" as a single headline text;
      -- restructure to nested merge / ID.
      local id = M.extract_id_from_text(
        vim.trim(value_text:sub(#'merge' + 1)))
      return { { level = child_level, text = field_name },
               { level = child_level + 1, text = 'merge' },
               { level = child_level + 2, text = id } } end
    return group end
  if field_name == 'viewRequests' then
    if value_text == nil then
      -- Childless: drop the field, key and all.
      return nil end
    if M.default_none_p(value_text) and #group == 2 then return nil end
    return group end
  if field_name == 'source' then
    if value_text == nil then return nil end
    return { { level = child_level, text = field_name },
             { level = child_level + 1,
               text = M.strip_default_suffix(value_text) } } end
  return group
end

---@param text string
---@return boolean
function M.default_false_p (text)
  local trimmed = vim.trim(text)
  return trimmed == 'false (default)' or trimmed == 'false'
end

---@param text string
---@return boolean
function M.default_affected_p (text)
  local trimmed = vim.trim(text)
  return trimmed == 'affected (default)' or trimmed == 'affected'
end

---@param text string
---@return boolean
function M.default_birth_p (text)
  local trimmed = vim.trim(text)
  return trimmed == 'unremarkable (default)'
         or trimmed == 'unremarkable'
end

---@param text string
---@return boolean
function M.default_none_p (text)
  local trimmed = vim.trim(text)
  return trimmed == 'none (default)' or trimmed == 'none'
end

---@param text string
---@return string TEXT without a trailing ' (default)'
function M.strip_default_suffix (text)
  local trimmed = vim.trim(text)
  local without = trimmed:match('^(.*) %(default%)$')
  return without or trimmed
end

---An ID from TEXT, handling org links like [[id:X][label]].
---@param text string
---@return string
function M.extract_id_from_text (text)
  local trimmed = vim.trim(text)
  local id = trimmed:match('%[%[id:([^%]]+)%]%[')
  return id or trimmed
end

---@param ... table[] headline lists
---@return table[] their concatenation
function M.concatenated (...)
  local result = {}
  for _, list in ipairs({ ... }) do
    for _, element in ipairs(list) do table.insert(result, element) end
  end
  return result
end

return M
