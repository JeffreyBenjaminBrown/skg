-- PURPOSE: The commit-readiness gestures of the total diff view
-- ('TODO/total-diff-view/2_plan.org', "gestures"). Each writes
-- readiness requests, '(readyRequest AXES)' or '(unreadyRequest
-- AXES)', into headline metadata, then saves. The save consumes
-- them, setting or clearing the marks, and answers with the buffer
-- redrawn and the readiness summary.
-- The Lua port of elisp/skg-readiness.el, which describes which
-- headlines name leaves and which are aggregates. Neovim has no
-- prefix argument, so each recursive form is its own command.

local metadata = require('skg.metadata')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')

local M = {}

local VOGNODE_KINDS = { node = true, deleted = true, unknown = true,
                        historical = true }
local PROPERTY_HEADS = { oldText = true, homeMove = true,
                         relRepoMove = true }

-- ── reading a headline's change facts ──────────────────────────────

---The head symbol's name of a list, or nil.
---@param value any
---@return string|nil
local function head_name (value)
  if sexpr.is_list(value) and value[1] ~= nil
     and not sexpr.is_list(value[1]) then
    return sexpr.atom_text(value[1]) end
  return nil
end

---The list in METADATA_SEXP that holds its change facts: the kind
---form of a vognode or phantom, else the whole sexp.
---@param metadata_sexp any
---@return any
function M.host (metadata_sexp)
  for i = 2, #metadata_sexp do
    if VOGNODE_KINDS[head_name(metadata_sexp[i]) or ''] then
      return metadata_sexp[i] end
  end
  return metadata_sexp
end

---@param metadata_sexp any
---@return boolean is it a vognode's or a phantom's?
function M.vognode_p (metadata_sexp)
  return M.host(metadata_sexp) ~= metadata_sexp
end

---The (KEY ...) form among METADATA_SEXP's change facts, or nil.
---@param metadata_sexp any
---@param key string
---@return any|nil
function M.form (metadata_sexp, key)
  local host = M.host(metadata_sexp)
  for i = 2, #host do
    if head_name(host[i]) == key then return host[i] end
  end
  return nil
end

---The atoms after the head of FORM, as strings.
---@param form any|nil
---@return string[]
local function arguments (form)
  local result = {}
  if form then
    for i = 2, #form do
      if not sexpr.is_list(form[i]) then
        table.insert(result, sexpr.atom_text(form[i])) end
    end
  end
  return result
end

---The axes on which the headline with METADATA_SEXP names a leaf: a
---sublist of {'node', 'rel'} for a vognode or phantom, {'single'}
---for a property that names one, {} when it names none.
---@param metadata_sexp any
---@return string[]
function M.leaf_axes (metadata_sexp)
  local diff = M.form(metadata_sexp, 'diff')
  if M.vognode_p(metadata_sexp) then
    local axes = {}
    for _, axis in ipairs({ 'node', 'rel' }) do
      for _, atom in ipairs(arguments(diff)) do
        if vim.startswith(atom, axis) then
          table.insert(axes, axis) break end
      end
    end
    return axes end
  if diff then return { 'single' } end
  for i = 2, #metadata_sexp do
    local head = head_name(metadata_sexp[i])
    if PROPERTY_HEADS[head or ''] then return { 'single' } end
    if head == 'changesetMarker' then
      local kind = arguments(metadata_sexp[i])[1]
      if kind == 'content' or kind == 'relRepo' then
        return { 'single' } end
    end
  end
  return {}
end

---@param metadata_sexp any
---@param axes string[]
---@return boolean is every one of AXES ready?
function M.axes_ready_p (metadata_sexp, axes)
  local ready = M.form(metadata_sexp, 'ready')
  if not ready then return false end
  local ready_axes = {}
  for _, axis in ipairs(arguments(ready)) do ready_axes[axis] = true end
  for _, axis in ipairs(axes) do
    if axis ~= 'single' and not ready_axes[axis] then return false end
  end
  return true
end

---@param metadata_sexp any
---@return boolean is it a folder drawn because of a change?
function M.changed_folder_p (metadata_sexp)
  for i = 2, #metadata_sexp do
    local head = head_name(metadata_sexp[i])
    if head and vim.endswith(head, 'Folder')
       and vim.tbl_contains(arguments(metadata_sexp[i]), 'changed') then
      return true end
  end
  return false
end

---Whether the headline is ready: a leaf on all its axes, or a folder
---that carries (ready). False for an aggregate node, whose readiness
---is not drawn.
---@param metadata_sexp any
---@return boolean
function M.ready_p (metadata_sexp)
  local axes = M.leaf_axes(metadata_sexp)
  if #axes > 0 then return M.axes_ready_p(metadata_sexp, axes) end
  return M.changed_folder_p(metadata_sexp)
         and M.form(metadata_sexp, 'ready') ~= nil
end

---The {old, new} skgrepos of METADATA_SEXP when it is a home move.
---@param metadata_sexp any
---@return string[]|nil
function M.home_move (metadata_sexp)
  for i = 2, #metadata_sexp do
    local move = metadata_sexp[i]
    if head_name(move) == 'homeMove' then
      local pair = {}
      for j = 2, #move do
        pair[head_name(move[j])] = arguments(move[j])[1] end
      return { pair.old, pair.new } end
  end
  return nil
end

-- ── walking headlines ──────────────────────────────────────────────

---The lines of the headlines one level below the one at LINE_NUMBER.
---@param line_number integer
---@return integer[]
function M.child_lines (line_number)
  local level = metadata.outline_level(line_number)
  local lines = {}
  local line = metadata.next_headline_line(line_number)
  while line and (metadata.outline_level(line) or 0) > level do
    if metadata.outline_level(line) == level + 1 then
      table.insert(lines, line) end
    line = metadata.next_headline_line(line)
  end
  return lines
end

---The line at LINE_NUMBER and the lines of every headline below it.
---@param line_number integer
---@return integer[]
function M.subtree_lines (line_number)
  local level = metadata.outline_level(line_number)
  local lines = { line_number }
  local line = metadata.next_headline_line(line_number)
  while line and (metadata.outline_level(line) or 0) > level do
    table.insert(lines, line)
    line = metadata.next_headline_line(line)
  end
  return lines
end

-- ── the gestures ───────────────────────────────────────────────────

---Replace the readiness request of the headline at LINE_NUMBER with
---one that marks (READY true) or unmarks the leaves on AXES.
---@param line_number integer
---@param ready boolean
---@param axes string[]
function M.write_request (line_number, ready, axes)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  local host = M.host(metadata_sexp)
  local request = { sexpr.symbol(ready and 'readyRequest'
                                 or 'unreadyRequest') }
  for _, axis in ipairs(axes) do
    if axis ~= 'single' then
      table.insert(request, sexpr.symbol(axis)) end
  end
  local edits = { { sexpr.symbol('DELETE'),
                    { sexpr.symbol('readyRequest') },
                    { sexpr.symbol('unreadyRequest') } },
                  request }
  local instruction = { sexpr.symbol('skg') }
  if host == metadata_sexp then
    vim.list_extend(instruction, edits)
  else
    table.insert(instruction,
                 vim.list_extend({ host[1] }, edits)) end
  metadata.edit_metadata_at_line(line_number, instruction)
end

---Whether every child of the headline at LINE_NUMBER that names a
---leaf, or is a changed folder, is ready, and at least one does.
---@param line_number integer
---@return boolean
function M.children_ready_p (line_number)
  local some = false
  for _, line in ipairs(M.child_lines(line_number)) do
    local child = metadata.metadata_sexp_at_line_or_nil(line)
    if child and (#M.leaf_axes(child) > 0
                  or M.changed_folder_p(child)) then
      some = true
      if not M.ready_p(child) then return false end
    end
  end
  return some
end

---The one-level gesture on the headline at LINE_NUMBER. Returns how
---many requests it wrote.
---@param line_number integer
---@return integer
function M.cycle_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return 0 end
  local axes = M.leaf_axes(metadata_sexp)
  if #axes > 0 then
    local chosen = axes
    if #axes > 1 then
      local axis = picker.completing_read_with_cycle(
        'Which change (node = ∃ its existence, rel = ~ its relationship here)? ',
        { 'node', 'rel' }, { require_match = true })
      if axis ~= 'node' and axis ~= 'rel' then return 0 end
      chosen = { axis } end
    M.write_request(line_number,
                    not M.axes_ready_p(metadata_sexp, chosen), chosen)
    return 1 end
  if M.changed_folder_p(metadata_sexp) then
    local ready = not M.ready_p(metadata_sexp)
    local written = 0
    for _, line in ipairs(M.child_lines(line_number)) do
      local child = metadata.metadata_sexp_at_line_or_nil(line)
      local child_axes = child and M.leaf_axes(child) or {}
      if #child_axes > 0 then
        M.write_request(line, ready, child_axes)
        written = written + 1 end
    end
    return written end
  if M.vognode_p(metadata_sexp) then
    M.write_request(line_number, not M.children_ready_p(line_number),
                    { 'all' })
    return 1 end
  return 0
end

---The recursive gesture on the headline at LINE_NUMBER: a request on
---every leaf named in the subtree, readying them unless the headline
---is ready. On a changed node, also one naming every leaf it owns.
---Returns how many requests it wrote.
---@param line_number integer
---@return integer
function M.cycle_subtree_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return 0 end
  local aggregate_node = M.vognode_p(metadata_sexp)
                         and #M.leaf_axes(metadata_sexp) == 0
  local headline_ready
  if aggregate_node then
    headline_ready = M.children_ready_p(line_number)
  else
    headline_ready = M.ready_p(metadata_sexp) end
  local ready = not headline_ready
  local written = 0
  if aggregate_node then
    M.write_request(line_number, ready, { 'all' })
    written = 1 end
  for _, line in ipairs(M.subtree_lines(line_number)) do
    local headline = metadata.metadata_sexp_at_line_or_nil(line)
    local axes = headline and M.leaf_axes(headline) or {}
    if #axes > 0 then
      M.write_request(line, ready, axes)
      written = written + 1 end
  end
  return written
end

---Save, or report that nothing was written.
---@param written integer
local function save_if_written (written)
  if written == 0 then
    error('Nothing here can be marked commit-ready') end
  require('skg.save').request_save_buffer()
end

---Toggle the commit-readiness of the change at point, then save. On a
---headline that names one leaf, toggle it; on a content member that
---names two (∃ and ~), ask which. On a changed folder, toggle each
---leaf one level down; on a changed node, every leaf it owns.
function M.cycle_commit_ready ()
  save_if_written(M.cycle_at_line(metadata.current_line_number()))
end

---Mark (or, if the headline at point is ready, unmark) every leaf
---named in the subtree at point, then save.
function M.cycle_commit_ready_recursive ()
  save_if_written(M.cycle_subtree_at_line(metadata.current_line_number()))
end

---The line of the home move at LINE_NUMBER (that headline, or the
---child of the node there that is its move) and its {old, new}.
---@param line_number integer
---@return integer|nil, string[]|nil
function M.home_move_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  local pair = metadata_sexp and M.home_move(metadata_sexp)
  if pair then return line_number, pair end
  for _, line in ipairs(M.child_lines(line_number)) do
    local child = metadata.metadata_sexp_at_line_or_nil(line)
    pair = child and M.home_move(child)
    if pair then return line, pair end
  end
  return nil, nil
end

---Mark commit-ready the home-skgrepo move of the node at point (or of
---the move at point); with RECURSIVE, also the move of every
---descendant that moved between the same two skgrepos. Then save.
---@param recursive boolean|nil
function M.mark_gnode_skgrepo_changes_commitReady (recursive)
  local move_line, pair =
    M.home_move_at_line(metadata.current_line_number())
  if not move_line then
    error("This node's home skgrepo did not change") end
  local written = 1
  if not recursive then
    M.write_request(move_line, true, { 'single' })
  else
    written = 0
    local node_line =
      metadata.parent_headline_line(move_line,
                                    metadata.outline_level(move_line))
    for _, line in ipairs(M.subtree_lines(node_line)) do
      local headline = metadata.metadata_sexp_at_line_or_nil(line)
      local other = headline and M.home_move(headline)
      if other and other[1] == pair[1] and other[2] == pair[2] then
        M.write_request(line, true, { 'single' })
        written = written + 1 end
    end
  end
  vim.notify(string.format('Marked %d home move(s) ready.', written))
  require('skg.save').request_save_buffer()
end

function M.mark_gnode_skgrepo_changes_commitReady_recursive ()
  M.mark_gnode_skgrepo_changes_commitReady(true)
end

return M
