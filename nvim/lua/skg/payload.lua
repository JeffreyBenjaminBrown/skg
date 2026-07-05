-- PURPOSE: Small helpers for walking parsed server payloads.
-- The Lua stand-ins for the '(cadr (assoc 'key response))' idiom the
-- elisp client uses everywhere. Handles both entry shapes the server
-- produces: list entries '(key value)' and dotted pairs
-- '(key . value)'.

local sexpr = require('skg.sexpr.parse')

local M = {}

---The value under KEY in parsed RESPONSE, or nil.
---@param response any
---@param key string
---@return any|nil
function M.field (response, key)
  if not sexpr.is_list(response) then return nil end
  for _, element in ipairs(response) do
    if sexpr.is_pair(element)
       and not sexpr.is_list(element.car)
       and sexpr.atom_text(element.car) == key then
      return element.cdr end
    if sexpr.is_list(element) and #element >= 2
       and not sexpr.is_list(element[1])
       and sexpr.atom_text(element[1]) == key then
      return element[2] end
  end
  return nil
end

---The value under KEY rendered as text, or nil. (The server's sexp
---printer leaves space-free strings unquoted, so a value the client
---expects as a string can arrive as a symbol; this normalizes.)
---@param response any
---@param key string
---@return string|nil
function M.field_text (response, key)
  local value = M.field(response, key)
  if value == nil or sexpr.is_list(value) then return nil end
  return sexpr.atom_text(value)
end

---A sexp list of message strings as a Lua list of strings.
---Nil and the empty list both give {}.
---@param value any
---@return string[]
function M.string_list (value)
  local result = {}
  if value ~= nil and sexpr.is_list(value) then
    for _, element in ipairs(value) do
      if not sexpr.is_list(element) then
        table.insert(result, sexpr.atom_text(element)) end
    end
  end
  return result
end

return M
