-- PURPOSE: Fill in and strip the fields a user may set on a headline,
-- for the metadata editor (skg.metadata_edit), and say what each
-- field's cycle runs through. The fields of each kind of headline,
-- their defaults and their values come from
-- 'shared/metadata-fields.json', whose comments describe them. After
-- sexp_to_org, filling in inserts each missing field at its default;
-- before org_to_sexp, stripping removes each field at its default and
-- writes a value that took arguments as a list. The Lua port of
-- elisp/skg-sexpr/skg-metadata-fields.el.
--
-- ENTRY POINTS: kind_of_sexp, fillIn_defaults_in_org,
-- strip_defaults_from_org, cycle_values, value_arguments,
-- headlines_to_org.
--
-- Headlines here are the {level, text} tables produced by
-- skg.sexpr.org_bijection's extract_headlines.

local parse = require('skg.sexpr.parse')
local bijection = require('skg.sexpr.org_bijection')
local shared = require('skg.shared')

local M = {}

-- ── The table ──────────────────────────────────────────────────────

---The kind of the metadata SEXP, '(skg ...)', from
---'shared/metadata-fields.json': the kind of the first atom of SEXP,
---bare or heading a list, that the table names, or, when it names
---none, the occurrence's, since the server reads such metadata as a
---node's. Nil when SEXP is no '(skg ...)'.
---@param sexp any
---@return table|nil
function M.kind_of_sexp (sexp)
  if not (parse.is_list(sexp) and sexp[1] == parse.symbol('skg')) then
    return nil end
  for i = 2, #sexp do
    local atom = sexp[i]
    if parse.is_list(atom) then atom = atom[1] end
    if parse.is_symbol(atom) then
      local kind = shared.metadata_kind(parse.atom_text(atom))
      if kind then return kind end
    end
  end
  return shared.metadata_kind('node')
end

---The names of the fields a user may set on KIND, in the table's order.
---@param kind table
---@return string[]
function M.field_names (kind)
  local names = {}
  for _, field in ipairs(kind.fields) do table.insert(names, field.name) end
  return names
end

---@param kind table|nil
---@param name string
---@return table|nil the field of KIND named NAME
function M.field (kind, name)
  if not kind then return nil end
  for _, field in ipairs(kind.fields) do
    if field.name == name then return field end
  end
  return nil
end

---@param field table
---@return string FIELD's default as the editor shows it: 'DEFAULT (default)'
function M.shown_default (field)
  return field.default .. ' (default)'
end

---@param field table
---@param text string
---@return boolean is TEXT FIELD's default, shown or bare?
function M.default_p (field, text)
  local trimmed = vim.trim(text)
  return trimmed == field.default or trimmed == M.shown_default(field)
end

---@param field table|nil
---@param word string|nil
---@return table|nil the entry of FIELD's values whose value is WORD
function M.value_entry (field, word)
  if not field then return nil end
  for _, entry in ipairs(field.values) do
    if entry.value == word then return entry end
  end
  return nil
end

---What the cycle of KIND's field FIELD_NAME runs through: its default as
---shown, then each value the headline may take, given whether it is
---EDITABLE (not write-protected) and a REMOVED_MEMBER. Nil when KIND has
---no such field.
---@param kind table|nil
---@param field_name string
---@param editable boolean
---@param removed_member boolean
---@return string[]|nil
function M.cycle_values (kind, field_name, editable, removed_member)
  local field = M.field(kind, field_name)
  if not field then return nil end
  local values = { M.shown_default(field) }
  for _, entry in ipairs(field.values) do
    local condition = entry.when
    if condition == nil
       or (condition == 'editable' and editable)
       or (condition == 'removedMember' and removed_member) then
      table.insert(values, entry.value) end
  end
  return values
end

---The arguments the VALUE of KIND's field FIELD_NAME takes, each a table
---with 'of' ('id' or 'skgrepo') or 'one_of' (a list); nil for a value
---that takes none.
---@param kind table|nil
---@param field_name string
---@param value string
---@return table[]|nil
function M.value_arguments (kind, field_name, value)
  local entry = M.value_entry(M.field(kind, field_name), value)
  if entry and entry.arguments and #entry.arguments > 0 then
    return entry.arguments end
  return nil
end

-- ── Headlines ↔ org text ──────────────────────────────────────────

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

---The index in HEADLINES of the headline whose children are KIND's
---fields: the root, '(skg ...)', since every kind's fields stand
---beside its atom at the top level.
---@param headlines table[]
---@param kind table
---@return integer
function M.container_index (headlines, kind)
  for index, headline in ipairs(headlines) do
    if headline.level == 1 then return index end
  end
  error('skg.sexpr.metadata_fields: no headline holds the fields of a '
        .. kind.kind)
end

---HEADLINES with the children of KIND's field-holding headline replaced
---by TRANSFORM(children, child_level), a flat headline list; children is
---a list of headline groups, one per child.
---@param headlines table[]
---@param kind table
---@param transform fun(children: table[][], child_level: integer): table[]
---@return table[]
function M.map_fields (headlines, kind, transform)
  local index = M.container_index(headlines, kind)
  local child_level = headlines[index].level + 1
  local children, remainder =
    M.group_children(vim.list_slice(headlines, index + 1), child_level)
  return M.concatenated(vim.list_slice(headlines, 1, index),
                        transform(children, child_level),
                        remainder)
end

-- ── Fill in ────────────────────────────────────────────────────────

---ORG_TEXT, the metadata of a headline of KIND as org text, with each
---field KIND's headlines may set shown: in canonical order, a missing
---one at its default, a bare atom with a 'true' child. If
---DEFAULT_SKGREPO is given, insert it as the skgrepo default and mark a
---matching existing skgrepo value with '(default)'. If DISPLAY_TITLE is
---given and non-empty, prepend a display-only title group.
---@param org_text string
---@param kind table
---@param default_skgrepo string|nil
---@param display_title string|nil
---@return string
function M.fillIn_defaults_in_org (org_text, kind, default_skgrepo,
                                   display_title)
  local headlines = bijection.extract_headlines(org_text)
  local filledIn = headlines
  if #kind.fields > 0 then
    filledIn = M.map_fields(headlines, kind,
      function (children, child_level)
        return M.fillIn_and_reorder(children, child_level, kind,
                                    default_skgrepo) end)
  end
  return M.headlines_to_org(M.maybe_prepend_title(filledIn, display_title))
end

---@param headlines table[]
---@param display_title string|nil
---@return table[]
function M.maybe_prepend_title (headlines, display_title)
  if display_title and not display_title:match('^%s*$') then
    return M.concatenated({ { level = 1, text = 'title' },
                            { level = 2, text = display_title } },
                          headlines)
  end
  return headlines
end

---CHILDREN (headline groups at CHILD_LEVEL) in canonical order, flat,
---each of KIND's fields filled in. The canonical order is the order the
---server writes: what precedes the kind's atom (the viewnode ID and the
---markers) and the atom itself, which say what the headline is; then
---its gnid and skgRepos; then KIND's fields; then the other children,
---in the order they came.
---@param children table[][]
---@param child_level integer
---@param kind table
---@param default_skgrepo string|nil
---@return table[]
function M.fillIn_and_reorder (children, child_level, kind,
                               default_skgrepo)
  local leading_count = 0
  for index, group in ipairs(children) do
    if vim.tbl_contains(kind.atoms, vim.trim(group[1].text)) then
      leading_count = index
      break end
  end
  local canonical = { 'gnid', 'skgRepos' }
  for _, name in ipairs(M.field_names(kind)) do
    table.insert(canonical, name) end
  local leading = {}
  local group_by_name = {}
  local others = {}
  for index, group in ipairs(children) do
    local name = vim.trim(group[1].text)
    if index <= leading_count then
      for _, headline in ipairs(group) do
        table.insert(leading, headline) end
    elseif vim.tbl_contains(canonical, name) then
      if group_by_name[name] == nil then group_by_name[name] = group end
    else
      for _, headline in ipairs(group) do
        table.insert(others, headline) end
    end
  end
  local filledIn = {}
  for _, name in ipairs(canonical) do
    for _, headline in ipairs(
        M.fillIn_one(name, group_by_name[name], child_level, kind,
                     default_skgrepo)) do
      table.insert(filledIn, headline) end
  end
  return M.concatenated(leading, filledIn, others)
end

---The headlines that show the child NAME of KIND, whose group is GROUP
---or nil when absent: a missing field at its default, a bare atom with a
---'true' child, a node's skgrepo matching DEFAULT_SKGREPO marked
---'(default)', or DEFAULT_SKGREPO so marked when the node has none.
---@param name string
---@param group table[]|nil
---@param child_level integer
---@param kind table
---@param default_skgrepo string|nil
---@return table[]
function M.fillIn_one (name, group, child_level, kind, default_skgrepo)
  local field = M.field(kind, name)
  if field and group == nil then
    return { { level = child_level, text = name },
             { level = child_level + 1, text = M.shown_default(field) } } end
  if field and name == 'editRequest'
     and M.only_readiness_requests_p(group, child_level) then
    return M.concatenated(
      { group[1],
        { level = child_level + 1, text = M.shown_default(field) } },
      vim.list_slice(group, 2)) end
  if field and #group == 1 and field.shape == 'atom' then
    return { group[1], { level = child_level + 1, text = 'true' } } end
  if name == 'skgRepos' and default_skgrepo
     and vim.tbl_contains(kind.atoms, 'node') then
    return M.skgRepos_with_default(group, child_level, default_skgrepo) end
  return group or {}
end

---Whether GROUP, an editRequest at CHILD_LEVEL, holds nothing but a
---readiness request, '(ready ...)' or '(unready ...)', which the
---readiness commands set and the editor keeps. Its fill-in then shows
---the other request at its default beside it.
---@param group table[]
---@param child_level integer
---@return boolean
function M.only_readiness_requests_p (group, child_level)
  local value_groups =
    M.group_children(vim.list_slice(group, 2), child_level + 1)
  if #value_groups == 0 then return false end
  for _, value_group in ipairs(value_groups) do
    local text = vim.trim(value_group[1].text)
    if text ~= 'ready' and text ~= 'unready' then return false end
  end
  return true
end

---GROUP, a node's skgRepos (or nil when absent), with its nodeRepo
---shown as DEFAULT_SKGREPO marked '(default)' when it is missing or
---equals DEFAULT_SKGREPO.
---@param group table[]|nil
---@param child_level integer
---@param default_skgrepo string
---@return table[]
function M.skgRepos_with_default (group, child_level, default_skgrepo)
  local shown_default = { { level = child_level + 1, text = 'nodeRepo' },
                          { level = child_level + 2,
                            text = default_skgrepo .. ' (default)' } }
  if group == nil then
    return M.concatenated({ { level = child_level, text = 'skgRepos' } },
                          shown_default) end
  local repo_groups =
    M.group_children(vim.list_slice(group, 2), child_level + 1)
  local result = { group[1] }
  local has_nodeRepo = false
  for _, repo_group in ipairs(repo_groups) do
    if vim.trim(repo_group[1].text) == 'nodeRepo' then
      has_nodeRepo = true
      if #repo_group == 2
         and vim.trim(repo_group[2].text) == default_skgrepo then
        repo_group = shown_default end
    end
    for _, headline in ipairs(repo_group) do
      table.insert(result, headline) end
  end
  if not has_nodeRepo then
    return M.concatenated({ group[1] }, shown_default,
                          vim.list_slice(result, 2)) end
  return result
end

-- ── Strip ──────────────────────────────────────────────────────────

---ORG_TEXT, the metadata of a headline of KIND as org text, ready for
---org_to_sexp: without the display-only title group, each field at its
---default removed, each bare atom set to 'true' collapsed back to the
---atom, each value that took arguments written as a list.
---@param org_text string
---@param kind table
---@return string
function M.strip_defaults_from_org (org_text, kind)
  local headlines =
    M.remove_title_headlines(bijection.extract_headlines(org_text))
  if #kind.fields > 0 then
    headlines = M.map_fields(headlines, kind,
      function (children, child_level)
        local result = {}
        for _, group in ipairs(children) do
          for _, headline in ipairs(
              M.strip_one(group, child_level, kind) or {}) do
            table.insert(result, headline) end
        end
        return result end)
  end
  return M.headlines_to_org(headlines)
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

---The headlines that GROUP, a child of KIND at CHILD_LEVEL, saves as:
---nil for a field at its default.
---@param group table[]
---@param child_level integer
---@param kind table
---@return table[]|nil
function M.strip_one (group, child_level, kind)
  local name = vim.trim(group[1].text)
  local field = M.field(kind, name)
  local value_text = nil
  if #group > 1 then value_text = vim.trim(group[2].text) end
  if field and field.shape == 'atom' then
    if value_text == nil or M.default_p(field, value_text) then
      return nil end
    if value_text == 'true' then
      return { { level = child_level, text = name } } end
    return group end
  if field then
    local value_groups =
      M.group_children(vim.list_slice(group, 2), child_level + 1)
    local values = {}
    for _, value_group in ipairs(value_groups) do
      if not M.default_p(field, value_group[1].text) then
        for _, headline in ipairs(M.value_as_saved(value_group, field)) do
          table.insert(values, headline) end
      end
    end
    if #values == 0 then return nil end
    return M.concatenated({ { level = child_level, text = name } }, values)
  end
  if name == 'skgRepos' then
    return M.skgRepos_as_saved(group, child_level) end
  return group
end

---GROUP, a skgRepos at CHILD_LEVEL, as it saves: each skgrepo without
---a '(default)' suffix, one with no value dropped, and nil when none is
---left.
---@param group table[]
---@param child_level integer
---@return table[]|nil
function M.skgRepos_as_saved (group, child_level)
  local result = { group[1] }
  for _, repo_group in ipairs(
      M.group_children(vim.list_slice(group, 2), child_level + 1)) do
    if #repo_group > 1 then
      table.insert(result, repo_group[1])
      table.insert(result,
        { level = repo_group[2].level,
          text = M.strip_default_suffix(repo_group[2].text) })
      for i = 3, #repo_group do
        table.insert(result, repo_group[i]) end
    end
  end
  if #result == 1 then return nil end
  return result
end

---VALUE_GROUP, one value of FIELD, as it saves: a value that takes
---arguments, typed on one line as 'VALUE ARG...', becomes the value with
---one child per argument; an id argument typed as a link becomes the
---link's id.
---@param value_group table[]
---@param field table
---@return table[]
function M.value_as_saved (value_group, field)
  local head = value_group[1]
  local level = head.level
  local text = vim.trim(head.text)
  local word = text:match('^(%S+)')
  local entry = M.value_entry(field, word)
  local arguments = entry and entry.arguments
  if not arguments or #arguments == 0 then return value_group end
  if #value_group > 1 then
    local result = { head }
    local argument_index = 0 -- of the child, among the arguments
    for i = 2, #value_group do
      local child = value_group[i]
      if child.level == level + 1 then
        argument_index = argument_index + 1
        local argument = arguments[argument_index]
        if argument and argument.of == 'id' then
          child = { level = child.level,
                    text = M.extract_id_from_text(child.text) } end
      end
      table.insert(result, child)
    end
    return result
  end
  if text == word then return value_group end
  local rest = vim.trim(text:sub(#word + 1))
  local argument_texts
  if #arguments > 1 then
    argument_texts = vim.split(rest, '%s+', { trimempty = true })
  elseif arguments[1].of == 'id' then
    argument_texts = { M.extract_id_from_text(rest) }
  else
    argument_texts = { rest } end
  local result = { { level = level, text = word } }
  for _, argument_text in ipairs(argument_texts) do
    table.insert(result, { level = level + 1, text = argument_text }) end
  return result
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
  local gnid = trimmed:match('%[%[id:([^%]]+)%]%[')
  return gnid or trimmed
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
