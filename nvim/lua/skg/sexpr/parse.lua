-- PURPOSE: An s-expression reader and printer for the skg wire
-- protocol and headline metadata. The Lua stand-in for the elisp
-- built-ins 'read' and 'prin1-to-string', which the Emacs client leans
-- on for every server payload and every '(skg ...)' metadata block.
--
-- The grammar matched here is pinned by the two peers, not by full
-- elisp:
-- - what the server EMITS: 'quote_herald' in server/org_to_text.rs
--   escapes only backslash and double-quote inside strings; bare atoms
--   may contain any non-delimiter bytes (e.g. the sourceHerald atom
--   '⌂:public'); counts are bare integers; dotted notation appears in
--   a few responses (busy-initializing, titles-by-ids).
-- - what the server PARSES: the Rust 'sexp' crate (Cargo.toml), which
--   has strings, integers, floats, symbols and lists -- NO comment
--   syntax and no quote ('x) syntax. Accordingly this reader treats
--   ';' and "'" as ordinary atom characters. (Emacs 'read' would not,
--   but nothing on the wire contains them outside strings.)
--
-- VALUE REPRESENTATION
-- - proper list  -> plain Lua array table
-- - symbol       -> interned table {name = ...}; interning makes '=='
--                   behave like elisp 'eq', and 'assert.are.same' work
-- - string       -> Lua string
-- - number       -> Lua number (integers and floats)
-- - dotted pair  -> M.pair(car, cdr); '(a b . c)' right-folds to
--                   pair(a, pair(b, c)), true cons semantics. On the
--                   wire pairs only ever appear as 2-element alist
--                   entries like '(request . "text search")'.
-- - nil / '()'   -> M.NIL, one shared empty list. Do not mutate it.
--
-- Lua strings are byte sequences, so UTF-8 atoms and strings pass
-- through untouched; all positions in this file are byte positions.

local M = {}

local symbol_metatable = {
  __tostring = function (self) return self.name end }

local pair_metatable = {
  __tostring = function (self)
    return '(' .. tostring(self.car) .. ' . '
               .. tostring(self.cdr) .. ')' end }

local interned_symbols = {}

---The interned symbol named NAME. Two calls with the same name return
---the same table, so '==' compares like elisp 'eq'.
---@param name string
---@return table
function M.symbol (name)
  local existing = interned_symbols[name]
  if existing then return existing end
  local fresh = setmetatable({ name = name }, symbol_metatable)
  interned_symbols[name] = fresh
  return fresh
end

---A cons pair, printed '(car . cdr)'.
---@param car any
---@param cdr any
---@return table
function M.pair (car, cdr)
  return setmetatable({ car = car, cdr = cdr }, pair_metatable)
end

---The empty list, also the reading of the atom 'nil'. Shared; never
---mutate it.
M.NIL = setmetatable({}, { __newindex = function ()
  error('skg.sexpr: attempted to mutate the shared NIL') end })

---@param value any
---@return boolean
function M.is_symbol (value)
  return type(value) == 'table'
         and getmetatable(value) == symbol_metatable
end

---@param value any
---@return boolean
function M.is_pair (value)
  return type(value) == 'table'
         and getmetatable(value) == pair_metatable
end

---True for proper lists (plain arrays), including NIL.
---@param value any
---@return boolean
function M.is_list (value)
  if type(value) ~= 'table' then return false end
  local mt = getmetatable(value)
  return mt == nil or value == M.NIL
end

---True for NIL and for any empty list.
---@param value any
---@return boolean
function M.is_nil (value)
  return M.is_list(value) and next(value) == nil
end

---The text of an atom: a symbol's name, a number rendered as the
---server rendered it, or a string as-is. The analog of the server's
---'atom_to_string'. Errors on lists and pairs.
---@param value any
---@return string
function M.atom_text (value)
  if M.is_symbol(value) then return value.name end
  if type(value) == 'string' then return value end
  if type(value) == 'number' then return M.number_to_text(value) end
  error('skg.sexpr.atom_text: not an atom: ' .. vim.inspect(value))
end

-- ── Reading ────────────────────────────────────────────────────────

---Read the first s-expression (or atom) in TEXT, starting at byte
---START_POSITION (default 1). Returns the value and the byte position
---just after it. Errors on malformed input, like elisp 'read'.
---@param text string
---@param start_position integer|nil
---@return any value
---@return integer position_after
function M.read (text, start_position)
  local position = M.skip_whitespace(text, start_position or 1)
  if position > #text then
    error('skg.sexpr.read: nothing to read') end
  return M.read_value(text, position)
end

---Dispatch on the byte at POSITION: list, string, or atom.
---@param text string
---@param position integer
---@return any value
---@return integer position_after
function M.read_value (text, position)
  local byte = text:sub(position, position)
  if byte == '(' then return M.read_list(text, position + 1) end
  if byte == ')' then
    error('skg.sexpr.read: unexpected ) at byte ' .. position) end
  if byte == '"' then return M.read_string(text, position + 1) end
  return M.read_atom(text, position)
end

---Read list elements until ')'. POSITION is just after the '('.
---A delimited '.' token introduces a dotted tail: exactly one value
---then ')', right-folded into pairs.
---@param text string
---@param position integer
---@return any value
---@return integer position_after
function M.read_list (text, position)
  local elements = {}
  while true do
    position = M.skip_whitespace(text, position)
    if position > #text then
      error('skg.sexpr.read: unterminated list') end
    local byte = text:sub(position, position)
    if byte == ')' then
      if #elements == 0 then return M.NIL, position + 1 end
      return elements, position + 1 end
    if M.dot_token_at(text, position) then
      if #elements == 0 then
        error('skg.sexpr.read: dotted tail with no preceding element') end
      local tail
      tail, position = M.read(text, position + 1)
      position = M.skip_whitespace(text, position)
      if text:sub(position, position) ~= ')' then
        error('skg.sexpr.read: expected ) after dotted tail') end
      local folded = tail
      for i = #elements, 1, -1 do
        folded = M.pair(elements[i], folded) end
      return folded, position + 1 end
    local value
    value, position = M.read_value(text, position)
    table.insert(elements, value)
  end
end

---Read a string body. POSITION is just after the opening quote.
---Escapes: backslash makes the next byte literal, covering the \\ and
---\" the server emits. (A literal newline inside a string is legal and
---passes through.)
---@param text string
---@param position integer
---@return string value
---@return integer position_after
function M.read_string (text, position)
  local chunks = {}
  while true do
    if position > #text then
      error('skg.sexpr.read: unterminated string') end
    local byte = text:sub(position, position)
    if byte == '"' then
      return table.concat(chunks), position + 1 end
    if byte == '\\' then
      position = position + 1
      if position > #text then
        error('skg.sexpr.read: dangling backslash in string') end
      table.insert(chunks, text:sub(position, position))
    else
      table.insert(chunks, byte) end
    position = position + 1
  end
end

---Read a bare atom: number if the whole token is numeric, else symbol.
---@param text string
---@param position integer
---@return any value
---@return integer position_after
function M.read_atom (text, position)
  local token_end = position
  while token_end <= #text
        and not M.is_delimiter(text:sub(token_end, token_end)) do
    token_end = token_end + 1 end
  local token = text:sub(position, token_end - 1)
  if token == '' then
    error('skg.sexpr.read: empty atom at byte ' .. position) end
  if token == 'nil' then return M.NIL, token_end end
  local number = M.token_as_number(token)
  if number ~= nil then return number, token_end end
  return M.symbol(token), token_end
end

---A number if TOKEN is entirely numeric, else nil. Deliberately
---stricter than Lua's 'tonumber' (no hex, no leading/trailing junk),
---so UUID-shaped tokens like '3861db2c-...' stay symbols.
---@param token string
---@return number|nil
function M.token_as_number (token)
  if token:match('^-?%d+$') then return tonumber(token) end
  if token:match('^-?%d+%.%d*$')
     or token:match('^-?%.%d+$')
     or token:match('^-?%d+%.?%d*[eE][+-]?%d+$') then
    return tonumber(token) end
  return nil
end

---Is BYTE a token delimiter?
---@param byte string
---@return boolean
function M.is_delimiter (byte)
  return byte == '(' or byte == ')' or byte == '"'
         or byte == ' ' or byte == '\t'
         or byte == '\n' or byte == '\r'
end

---Is a delimited '.' (the dotted-pair marker) at POSITION?
---@param text string
---@param position integer
---@return boolean
function M.dot_token_at (text, position)
  if text:sub(position, position) ~= '.' then return false end
  local following = text:sub(position + 1, position + 1)
  return following == '' or M.is_delimiter(following)
end

---@param text string
---@param position integer
---@return integer
function M.skip_whitespace (text, position)
  local after = text:find('[^ \t\n\r]', position)
  return after or (#text + 1)
end

-- ── Printing ───────────────────────────────────────────────────────

---Render VALUE as s-expression text the server's parser (and Emacs
---'read') accepts. The analog of 'prin1-to-string'. The empty list
---prints as '()' rather than elisp's 'nil'; both read back as NIL.
---@param value any
---@return string
function M.to_string (value)
  if M.is_symbol(value) then return value.name end
  if type(value) == 'string' then return M.quote_string(value) end
  if type(value) == 'number' then return M.number_to_text(value) end
  if M.is_pair(value) then
    return '(' .. M.to_string(value.car)
           .. ' . ' .. M.to_string(value.cdr) .. ')' end
  if M.is_list(value) then
    local rendered = {}
    for _, element in ipairs(value) do
      table.insert(rendered, M.to_string(element)) end
    return '(' .. table.concat(rendered, ' ') .. ')' end
  error('skg.sexpr.to_string: unrepresentable value: '
        .. vim.inspect(value))
end

---Quote and escape a string exactly as the server's 'quote_herald'
---does: backslash and double-quote only.
---@param text string
---@return string
function M.quote_string (text)
  return '"' .. text:gsub('\\', '\\\\'):gsub('"', '\\"') .. '"'
end

---Integers render without a decimal point (Lua 5.1's tostring would
---give '7' anyway, but LuaJIT can carry floats that are whole).
---@param number number
---@return string
function M.number_to_text (number)
  if number == math.floor(number)
     and number == number -- not NaN
     and math.abs(number) ~= math.huge then
    return string.format('%d', number) end
  return tostring(number)
end

-- ── Line-level scanning ────────────────────────────────────────────

---The 1-based byte index of the final byte (the closing paren) of the
---first complete s-expression in TEXT at or after START_POSITION
---(default 1), or nil if no complete sexp is found. The port of
---'skg-find-sexp-end' (whose 0-based result-plus-one equals this
---1-based inclusive index; callers slice text:sub(1, result)).
---One deliberate improvement: parens inside double-quoted strings are
---not counted, where the elisp paren counter counts blindly. Herald
---strings like "2(1,1)L" are balanced so the two agree on real data;
---on a string with unbalanced parens the elisp version corrupts and
---this one does not.
---@param text string
---@param start_position integer|nil
---@return integer|nil
function M.find_sexp_end (text, start_position)
  local depth = 0
  local inside_string = false
  local position = start_position or 1
  while position <= #text do
    local byte = text:sub(position, position)
    if inside_string then
      if byte == '\\' then position = position + 1
      elseif byte == '"' then inside_string = false end
    elseif byte == '"' then inside_string = true
    elseif byte == '(' then depth = depth + 1
    elseif byte == ')' then
      depth = depth - 1
      if depth == 0 then return position end
    end
    position = position + 1
  end
  return nil
end

return M
