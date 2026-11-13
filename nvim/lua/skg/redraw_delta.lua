-- PURPOSE: Redraw a view by its delta: rewrite only the headline
-- lines a response names in '(changed-headlines (V ...))', instead of
-- replacing the whole buffer ('docs/api-and-formats.org', "The redraw
-- delta"). The new lines are the server's lines in their stripped
-- form, without the fold and focus atoms, which the buffer keeps as
-- vim folds and the cursor. After the rewrite the buffer must equal
-- the stripped form of the new text; when it does not, the caller
-- redraws the whole buffer. The Lua port of elisp/skg-redraw-delta.el.
--
-- The stripped form is one rule the server and both clients
-- implement. All three test against
-- 'tests/shared/fold-and-focus-atom-strip-cases.txt', whose header
-- states the rule.
--
-- PITFALL: Each rewrite replaces only the span of the line that
-- changed ('nvim_buf_set_text'). Replacing the whole line
-- ('nvim_buf_set_lines') opens closed expr folds: the fold starting
-- at the line, and folds after it. The span rewrite keeps every fold,
-- and keeps the cursor on its character of the line.

local heralds = require('skg.heralds')
local log = require('skg.log')
local metadata = require('skg.metadata')

local M = {}

---The bare atoms the stripped form of a headline line removes.
---@type table<string, boolean>
M.fold_and_focus_atoms = { focused = true, folded = true, bodyFolded = true }

-- ── patching ─────────────────────────────────────────────────────────

---Rewrite in BUF each headline line VIEWNODE_IDS names, by that
---viewnode ID's line in the stripped form of NEW_CONTENT, then
---re-herald those lines and what depends on them
---(M.reherald_after_rewrites). True when BUF then equals the stripped
---form of NEW_CONTENT; the caller redraws the whole buffer otherwise.
---False without editing when some viewnode ID is missing from BUF or
---from NEW_CONTENT; false after editing when BUF differs from the
---stripped form, or on an error, which it logs. Leaves folds and the
---cursor as they are.
---@param buf integer
---@param new_content string
---@param viewnode_ids string[]
---@return boolean
function M.patch_buffer_with_delta (buf, new_content, viewnode_ids)
  local ok, patched = pcall(M.patch_buffer_with_delta_or_error,
                            buf, new_content, viewnode_ids)
  if not ok then
    log.log('error', 'save', 'redraw patch error: %s', tostring(patched))
    return false end
  return patched
end

---M.patch_buffer_with_delta, without its handling of errors.
---@param buf integer
---@param new_content string
---@param viewnode_ids string[]
---@return boolean
function M.patch_buffer_with_delta_or_error (buf, new_content, viewnode_ids)
  local stripped_lines = M.stripped_lines(new_content)
  local buffer_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local stripped_indices = M.headline_indices_by_viewnode_id(
    stripped_lines, viewnode_ids)
  local buffer_indices = M.headline_indices_by_viewnode_id(
    buffer_lines, viewnode_ids)
  for _, viewnode_id in ipairs(viewnode_ids) do
    if not (stripped_indices[viewnode_id]
            and buffer_indices[viewnode_id]) then
      return false end
  end
  vim.bo[buf].modifiable = true
  local rewritten_indices = {}
  heralds.call_without_watching(buf, function ()
    -- A rewrite keeps the line count, so the indices stay valid.
    for _, viewnode_id in ipairs(viewnode_ids) do
      local index = buffer_indices[viewnode_id]
      M.rewrite_line(buf, index, buffer_lines[index],
                     stripped_lines[stripped_indices[viewnode_id]])
      table.insert(rewritten_indices, index)
    end
  end)
  if heralds.enabled_p(buf) then
    M.reherald_after_rewrites(buf, rewritten_indices) end
  return vim.deep_equal(vim.api.nvim_buf_get_lines(buf, 0, -1, false),
                        stripped_lines)
end

---A table from each of VIEWNODE_IDS to the index in LINES of the
---headline line that begins with it, read in one pass.
---@param lines string[]
---@param viewnode_ids string[]
---@return table<string, integer>
function M.headline_indices_by_viewnode_id (lines, viewnode_ids)
  local wanted = {}
  for _, viewnode_id in ipairs(viewnode_ids) do wanted[viewnode_id] = true end
  local indices = {}
  for _, headline in ipairs(M.viewnode_id_headlines(lines)) do
    if wanted[headline.id] then indices[headline.id] = headline.index end
  end
  return indices
end

---Each headline line of LINES that begins with a viewnode ID, as
---{ id = ID, index = INDEX }, INDEX its 1-based index, in order.
---@param lines string[]
---@return table[]
function M.viewnode_id_headlines (lines)
  local headlines = {}
  for index, line in ipairs(lines) do
    local viewnode_id = M.viewnode_id_at_headline_start(line)
    if viewnode_id then
      table.insert(headlines, { id = viewnode_id, index = index }) end
  end
  return headlines
end

---The X of LINE when it begins '^\*+ (skg (vnid X)', the renderer's
---form ('viewnode_to_string_where_it_stands'), else nil.
---@param line string
---@return string|nil
function M.viewnode_id_at_headline_start (line)
  if line:byte(1) ~= 42 then return nil end -- '*'; most lines are bodies
  return line:match('^%*+ %(skg %(vnid ([^)]*)%)')
end

---Make the line at 1-based INDEX of BUF, whose text is OLD_LINE, read
---NEW_LINE, replacing only the span between the prefix and the suffix
---the two share, so that the cursor and extmarks in the shared parts
---keep their place (and closed folds stay closed, see the PITFALL at
---the top).
---@param buf integer
---@param index integer
---@param old_line string
---@param new_line string
function M.rewrite_line (buf, index, old_line, new_line)
  if old_line == new_line then return end
  local prefix_length = M.common_prefix_length(old_line, new_line)
  local suffix_length = math.min(
    M.common_suffix_length(old_line, new_line),
    #old_line - prefix_length,
    #new_line - prefix_length)
  vim.api.nvim_buf_set_text(
    buf, index - 1, prefix_length, index - 1, #old_line - suffix_length,
    { new_line:sub(prefix_length + 1, #new_line - suffix_length) })
end

---Is the byte at 1-based POSITION of TEXT inside a UTF-8 character,
---after its first byte?
---@param text string
---@param position integer
---@return boolean
local function continuation_byte_p (text, position)
  local byte = text:byte(position)
  return byte ~= nil and byte >= 0x80 and byte < 0xC0
end

---How many bytes STRING_A and STRING_B share at their start, ending on
---a character boundary of both.
---@param string_a string
---@param string_b string
---@return integer
function M.common_prefix_length (string_a, string_b)
  local length = 0
  local limit = math.min(#string_a, #string_b)
  while length < limit
        and string_a:byte(length + 1) == string_b:byte(length + 1) do
    length = length + 1 end
  while length > 0 and (continuation_byte_p(string_a, length + 1)
                        or continuation_byte_p(string_b, length + 1)) do
    length = length - 1 end
  return length
end

---How many bytes STRING_A and STRING_B share at their end, starting on
---a character boundary of both.
---@param string_a string
---@param string_b string
---@return integer
function M.common_suffix_length (string_a, string_b)
  local length = 0
  local limit = math.min(#string_a, #string_b)
  while length < limit
        and string_a:byte(#string_a - length)
            == string_b:byte(#string_b - length) do
    length = length + 1 end
  while length > 0
        and continuation_byte_p(string_a, #string_a - length + 1) do
    length = length - 1 end
  return length
end

---Re-apply, once each, the herald of each headline at one of the
---1-based INDICES of BUF and of each headline whose herald reads one of
---them: the folders among its two nearest ancestors, which read their
---members and their members' children
---(metadata.folder_change_processed_in_lines; M.watch_buffer does this
---for an edit), and the headlines one or two levels below it, since a
---pending detachment reads its parent and grandparent
---(metadata.pending_detachment_in_lines).
---@param buf integer
---@param indices integer[]
function M.reherald_after_rewrites (buf, indices)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local reherald_indices = {}
  for _, index in ipairs(indices) do
    reherald_indices[index] = true
    for _, ancestor_index in ipairs(
          metadata.ancestor_headline_indices_in_lines(lines, index, 2)) do
      if lines[ancestor_index]:find('Folder', 1, true) then
        reherald_indices[ancestor_index] = true end
    end
    for _, descendant_index in ipairs(
          M.children_and_grandchildren_indices_in_lines(lines, index)) do
      reherald_indices[descendant_index] = true end
  end
  for index in pairs(reherald_indices) do
    vim.api.nvim_buf_clear_namespace(buf, heralds.namespace, index - 1, index)
    heralds.apply_to_line(buf, index - 1, lines)
  end
end

---The indices in LINES of the headlines one or two levels below the
---headline LINES[INDEX].
---@param lines string[]
---@param index integer
---@return integer[]
function M.children_and_grandchildren_indices_in_lines (lines, index)
  local descendant_indices = {}
  local level = metadata.headline_level_on_line(lines[index])
  if not level then return descendant_indices end
  for descendant_index = index + 1, #lines do
    local line = lines[descendant_index]
    local line_level = line:byte(1) == 42 -- '*'
                       and metadata.headline_level_on_line(line)
    if line_level then
      if line_level <= level then break end
      if line_level <= level + 2 then
        table.insert(descendant_indices, descendant_index) end
    end
  end
  return descendant_indices
end

-- ── the stripped form ────────────────────────────────────────────────

---The lines of the stripped form of TEXT: each headline line stripped,
---the other lines kept.
---@param text string
---@return string[]
function M.stripped_lines (text)
  local lines = vim.split(text, '\n', { plain = true })
  for index, line in ipairs(lines) do
    lines[index] = M.stripped_line(line) end
  return lines
end

---The stripped form of LINE: a headline line without the fold and focus
---atoms at depth 1 of its '(skg ...)' form, each removed with the one
---space before it. Any other line is its own stripped form.
---@param line string
---@return string
function M.stripped_line (line)
  if not (line:find('focused', 1, true) -- most lines hold none
          or line:find('olded', 1, true)) then
    return line end
  local form_start = line:match('^%*+ ()%(skg[ )]')
  if not form_start then return line end
  return M.without_fold_and_focus_atoms(line, form_start)
end

local QUOTE, OPEN, CLOSE, SPACE = 34, 40, 41, 32

---LINE without each fold and focus atom, and the space before it, at
---depth 1 of the form starting at the 1-based FORM_START. Depth counts
---the parentheses outside string literals. The text after the form is
---kept as it is.
---@param line string
---@param form_start integer
---@return string
function M.without_fold_and_focus_atoms (line, form_start)
  local kept_pieces = {}
  local copied_from = 1
  local depth = 0
  local position = form_start
  while position <= #line do
    local byte = line:byte(position)
    if byte == QUOTE then
      position = M.end_of_string_literal(line, position)
    elseif byte == OPEN then
      depth = depth + 1
      position = position + 1
    elseif byte == CLOSE then
      depth = depth - 1
      if depth == 0 then break end
      position = position + 1
    elseif byte == SPACE and depth == 1 then
      local atom_end = M.end_of_bare_atom(line, position + 1)
      if M.fold_and_focus_atoms[line:sub(position + 1, atom_end - 1)] then
        table.insert(kept_pieces, line:sub(copied_from, position - 1))
        copied_from = atom_end
        position = atom_end
      else
        position = position + 1 end
    else
      position = position + 1 end
  end
  table.insert(kept_pieces, line:sub(copied_from))
  return table.concat(kept_pieces)
end

---The position just past the string literal of LINE whose opening
---quote is at QUOTE_POSITION, or just past LINE if it is not closed. A
---backslash escapes the byte after it.
---@param line string
---@param quote_position integer
---@return integer
function M.end_of_string_literal (line, quote_position)
  local position = quote_position + 1
  while position <= #line do
    local byte = line:byte(position)
    if byte == 92 then -- a backslash
      position = position + 2
    elseif byte == QUOTE then
      return position + 1
    else
      position = position + 1 end
  end
  return #line + 1
end

---Bytes that end a bare atom: the server's whitespace, ASCII's (space,
---tab, newline, form feed, carriage return), parentheses and double
---quotes.
local bare_atom_delimiters =
  { [32] = true, [9] = true, [10] = true, [12] = true, [13] = true,
    [OPEN] = true, [CLOSE] = true, [QUOTE] = true }

---The position just past the bare atom of LINE starting at START: the
---run of bytes that are not whitespace, parentheses or double quotes.
---@param line string
---@param start integer
---@return integer
function M.end_of_bare_atom (line, start)
  local position = start
  while position <= #line
        and not bare_atom_delimiters[line:byte(position)] do
    position = position + 1 end
  return position
end

return M
