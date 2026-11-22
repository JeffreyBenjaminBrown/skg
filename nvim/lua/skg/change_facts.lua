-- PURPOSE: Read a headline's change facts, the '(diff ...)' parts
-- of its metadata the diff view draws: what changed ('processed_form',
-- 'processed_leaves', 'processed_node_added_p') and whether the user
-- marked it processed ('objects_processed_p'); and whether a folder
-- is changed, by its members' relationships, and processed
-- ('folder_change_processed_in_lines'). The heralds, the herald
-- descriptions and the processed gestures (skg.processed) read them;
-- writing a processed request is skg.metadata's. The Lua port of the
-- change-facts part of elisp/skg-metadata.el.

local metadata = require('skg.metadata')

local M = {}

---The '(diff ...)' form of METADATA_SEXP, which every kind keeps at
---the top level, or nil.
---@param metadata_sexp any
---@return any|nil
function M.processed_diff_form (metadata_sexp)
  return metadata.child_with_head(metadata_sexp, 'diff')
end

---The (KEY ...) part of METADATA_SEXP's '(diff ...)' form, or nil.
---@param metadata_sexp any
---@param key string
---@return any|nil
function M.processed_form (metadata_sexp, key)
  local diff = M.processed_diff_form(metadata_sexp)
  if not diff then return nil end
  return metadata.child_with_head(diff, key)
end

---The '(leaves N OBJECT...)' fact of the headline with METADATA_SEXP,
---as { count = N, objects = OBJECTS }, or nil when it names no leaf.
---@param metadata_sexp any
---@return table|nil
function M.processed_leaves (metadata_sexp)
  local form = M.processed_form(metadata_sexp, 'leaves')
  if not form then return nil end
  local atoms = metadata.arguments(form)
  return { count = tonumber(atoms[1]) or 0,
           objects = vim.list_slice(atoms, 2) }
end

---@param metadata_sexp any
---@return boolean is the headline with METADATA_SEXP a new node? Its
---'(diff ...)' says '(node added)'.
function M.processed_node_added_p (metadata_sexp)
  return metadata.arguments(M.processed_form(metadata_sexp, 'node'))[1]
         == 'added'
end

---@param metadata_sexp any
---@param objects string[]
---@return boolean is every one of OBJECTS processed? It is when the
---headline carries a '(processed ...)' that names each of them. A
---headline that lists no object (one naming a single leaf) is processed
---when it carries any '(processed ...)'.
function M.objects_processed_p (metadata_sexp, objects)
  local processed = M.processed_form(metadata_sexp, 'processed')
  if not processed then return false end
  local processed_objects = {}
  for _, object in ipairs(metadata.arguments(processed)) do
    processed_objects[object] = true
  end
  for _, object in ipairs(objects) do
    if not processed_objects[object] then return false end
  end
  return true
end

---Whether a folder member carries a change of its relationship to
---the folder's viewparent, and if so whether that change is processed.
---MEMBER_SEXP is the member's metadata; CHILD_SEXPS that of its own
---child headlines. The member carries such a change when its
---'(diff ...)' has a '(rel ...)', or when one of its children is a
---'(relRepoMove ...)'. The change is processed when both of these hold:
---if the member has a '(rel ...)', its '(diff ...)' has a '(processed
---...)' that, when its '(leaves ...)' lists 'rel', names 'rel'; if
---it has a '(relRepoMove ...)' child, that child's '(diff ...)' has
---a '(processed ...)'.
---@param member_sexp any
---@param child_sexps any[]
---@return boolean|nil nil when the member carries no such change,
---else whether it is processed
function M.membership_change_processed (member_sexp, child_sexps)
  local changed, processed = false, true
  if M.processed_form(member_sexp, 'rel') then
    changed = true
    local leaves = M.processed_leaves(member_sexp)
    local objects = {}
    if leaves and vim.tbl_contains(leaves.objects, 'rel') then
      objects = { 'rel' } end
    processed = M.objects_processed_p(member_sexp, objects)
  end
  for _, child in ipairs(child_sexps) do
    if metadata.child_with_head(child, 'relRepoMove') then
      changed = true
      processed = processed and M.processed_form(child, 'processed') ~= nil end
  end
  if not changed then return nil end
  return processed
end

---Whether the folder whose headline is LINES[INDEX] is changed, and
---if so whether it is processed. LINES are a buffer's lines, 1-based. The
---folder is changed when one of its members (its direct child
---headlines) carries a change of its relationship to the folder's
---viewparent (M.membership_change_processed), and processed when every such
---change is processed. Nil when the headline is no folder or no member
---carries such a change; else { processed = BOOLEAN }.
---@param lines string[]
---@param index integer
---@return table|nil
function M.folder_change_processed_in_lines (lines, index)
  local level = metadata.headline_level_on_line(lines[index])
  if not (level and metadata.folder_atom(metadata.metadata_on_line(lines[index]))) then
    return nil end
  local members = {} -- each { sexp, child_sexps }
  for i = index + 1, #lines do
    local line_level = metadata.headline_level_on_line(lines[i])
    if line_level then
      if line_level <= level then break end
      if line_level == level + 1 then
        table.insert(members, { sexp = metadata.metadata_on_line(lines[i]),
                                child_sexps = {} })
      elseif line_level == level + 2 and #members > 0 then
        local child = metadata.metadata_on_line(lines[i])
        if child then
          table.insert(members[#members].child_sexps, child) end
      end
    end
  end
  local changed, processed = false, true
  for _, member in ipairs(members) do
    local member_processed = member.sexp
      and M.membership_change_processed(member.sexp, member.child_sexps)
    if member_processed ~= nil then
      changed = true
      processed = processed and member_processed end
  end
  if not changed then return nil end
  return { processed = processed }
end

---M.folder_change_processed_in_lines for the headline at LINE_NUMBER of the
---current buffer.
---@param line_number integer
---@return table|nil
function M.folder_change_processed_at_line (line_number)
  return M.folder_change_processed_in_lines(
    vim.api.nvim_buf_get_lines(0, 0, -1, false), line_number)
end

return M
