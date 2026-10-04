-- PURPOSE: Read the data files in 'shared/', which the server's tests
-- and both clients read: 'herald-styles.json' (the herald styles and
-- how relationship heralds use them) and 'relations.json' (the
-- node-node relations). The Emacs analog is elisp/skg-shared.el.

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

return M
