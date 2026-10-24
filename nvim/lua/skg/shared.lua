-- PURPOSE: Read the data files in 'shared/', which the server's tests
-- and both clients read: 'herald-styles.json' (the herald styles and
-- how relationship heralds use them), 'relations.json' (the
-- node-node relations) and 'metadata-fields.json' (the fields a user
-- may set on each kind of headline). The Emacs analog is
-- elisp/skg-shared.el.

local M = {}

---The 'shared/' directory, found relative to this file
---(nvim/lua/skg/shared.lua).
M.directory = vim.fn.fnamemodify(
  debug.getinfo(1, 'S').source:sub(2), ':p:h:h:h:h') .. '/shared/'

---@param file_name string
---@return table the parsed JSON (JSON null becomes nil)
local function read_json (file_name)
  local lines = vim.fn.readfile(M.directory .. file_name)
  return vim.json.decode(table.concat(lines, '\n'),
                         { luanil = { object = true, array = true } })
end

M.herald_styles = read_json('herald-styles.json')
M.relations = read_json('relations.json').relations
---The headline kinds in 'shared/metadata-fields.json', each with the
---fields a user may set on it.
M.metadata_kinds = read_json('metadata-fields.json').kinds

---The style names, sorted.
---@return string[]
function M.style_names ()
  local names = {}
  for name, _ in pairs(M.herald_styles.styles) do
    table.insert(names, name) end
  table.sort(names)
  return names
end

---The Neovim highlight group of style NAME: 'SkgHerald' and the name
---capitalized, e.g. 'SkgHeraldNonstandard'.
---@param name string
---@return string
function M.style_highlight_group (name)
  return 'SkgHerald' .. name:sub(1, 1):upper() .. name:sub(2)
end

---The relations, sorted by display position.
---@return table[]
function M.relations_in_display_order ()
  local sorted = vim.deepcopy(M.relations)
  table.sort(sorted, function (a, b)
    return a.display_position < b.display_position end)
  return sorted
end

---The relation named NAME, or nil.
---@param name string
---@return table|nil
function M.relation (name)
  for _, relation in ipairs(M.relations) do
    if relation.name == name then return relation end end
  return nil
end

---The kind in 'shared/metadata-fields.json' known by the atom ATOM, or
---nil.
---@param atom string
---@return table|nil
function M.metadata_kind (atom)
  for _, kind in ipairs(M.metadata_kinds) do
    if vim.tbl_contains(kind.atoms, atom) then return kind end end
  return nil
end

return M
