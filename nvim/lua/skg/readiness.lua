-- PURPOSE: The commit-readiness gestures of the diff view
-- ('TODO/total-diff-view/02_plan.org', "gestures"). Each writes
-- readiness requests, '(readyRequest OBJECTS)' or '(unreadyRequest
-- OBJECTS)', into headline metadata, then saves. The save consumes
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
local PROPERTY_HEADS = { oldText = true, homeSkgrepoMove = true,
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

---The objects on which the headline with METADATA_SEXP names a leaf: a
---sublist of {'node', 'rel'} for a vognode or phantom, {'single'}
---for a property that names one, {} when it names none.
---@param metadata_sexp any
---@return string[]
function M.leaf_objects (metadata_sexp)
  local diff = M.form(metadata_sexp, 'diff')
  if M.vognode_p(metadata_sexp) then
    local objects = {}
    for _, object in ipairs({ 'node', 'rel' }) do
      for _, atom in ipairs(arguments(diff)) do
        if vim.startswith(atom, object) then
          table.insert(objects, object) break end
      end
    end
    return objects end
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
---@param objects string[]
---@return boolean is every one of OBJECTS ready?
function M.objects_ready_p (metadata_sexp, objects)
  local ready = M.form(metadata_sexp, 'ready')
  if not ready then return false end
  local ready_objects = {}
  for _, object in ipairs(arguments(ready)) do ready_objects[object] = true end
  for _, object in ipairs(objects) do
    if object ~= 'single' and not ready_objects[object] then return false end
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

---Whether the headline is ready: a leaf on all its objects, or a folder
---that carries (ready). False for an aggregate node, whose readiness
---is not drawn.
---@param metadata_sexp any
---@return boolean
function M.ready_p (metadata_sexp)
  local objects = M.leaf_objects(metadata_sexp)
  if #objects > 0 then return M.objects_ready_p(metadata_sexp, objects) end
  return M.changed_folder_p(metadata_sexp)
         and M.form(metadata_sexp, 'ready') ~= nil
end

---The {old, new} skgrepos of METADATA_SEXP when it is a home move.
---@param metadata_sexp any
---@return string[]|nil
function M.home_move (metadata_sexp)
  for i = 2, #metadata_sexp do
    local move = metadata_sexp[i]
    if head_name(move) == 'homeSkgrepoMove' then
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
---one that marks (READY true) or unmarks the leaves on OBJECTS.
---@param line_number integer
---@param ready boolean
---@param objects string[]
function M.write_request (line_number, ready, objects)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  local host = M.host(metadata_sexp)
  local request = { sexpr.symbol(ready and 'readyRequest'
                                 or 'unreadyRequest') }
  for _, object in ipairs(objects) do
    if object ~= 'single' then
      table.insert(request, sexpr.symbol(object)) end
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
    if child and (#M.leaf_objects(child) > 0
                  or M.changed_folder_p(child)) then
      some = true
      if not M.ready_p(child) then return false end
    end
  end
  return some
end

-- A gesture first gathers its requests, each { line, ready, objects },
-- then writes them all or none.

---The requests of the one-level gesture on the headline at
---LINE_NUMBER. Empty when the object prompt is dismissed.
---@param line_number integer
---@return table[]
function M.requests_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return {} end
  local objects = M.leaf_objects(metadata_sexp)
  if #objects > 0 then
    local chosen = objects
    if #objects > 1 then
      local object = picker.completing_read_with_cycle(
        'Which change (node = ∃ its existence, rel = ~ its relationship here)? ',
        { 'node', 'rel' }, { require_match = true })
      if object ~= 'node' and object ~= 'rel' then return {} end
      chosen = { object } end
    return { { line = line_number,
               ready = not M.objects_ready_p(metadata_sexp, chosen),
               objects = chosen } } end
  if M.changed_folder_p(metadata_sexp) then
    local ready = not M.ready_p(metadata_sexp)
    local requests = {}
    for _, line in ipairs(M.child_lines(line_number)) do
      local child = metadata.metadata_sexp_at_line_or_nil(line)
      local child_objects = child and M.leaf_objects(child) or {}
      if #child_objects > 0 then
        table.insert(requests,
                     { line = line, ready = ready, objects = child_objects }) end
    end
    return requests end
  if M.vognode_p(metadata_sexp) then
    return { { line = line_number,
               ready = not M.children_ready_p(line_number),
               objects = { 'all' } } } end
  return {}
end

---The requests of the recursive gesture on the headline at
---LINE_NUMBER: one on every leaf named in the subtree, readying them
---unless the headline is ready. On a changed node, also one naming
---every leaf it owns.
---@param line_number integer
---@return table[]
function M.requests_for_subtree_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return {} end
  local aggregate_node = M.vognode_p(metadata_sexp)
                         and #M.leaf_objects(metadata_sexp) == 0
  local headline_ready
  if aggregate_node then
    headline_ready = M.children_ready_p(line_number)
  else
    headline_ready = M.ready_p(metadata_sexp) end
  local ready = not headline_ready
  local requests = {}
  if aggregate_node then
    table.insert(requests,
                 { line = line_number, ready = ready, objects = { 'all' } }) end
  for _, line in ipairs(M.subtree_lines(line_number)) do
    local headline = metadata.metadata_sexp_at_line_or_nil(line)
    local objects = headline and M.leaf_objects(headline) or {}
    if #objects > 0 then
      table.insert(requests, { line = line, ready = ready, objects = objects }) end
  end
  return requests
end

---Whether the unmark requests among REQUESTS name more than one
---leaf. A request naming 'all' names every leaf its node owns.
---@param requests table[]
---@return boolean
function M.unmarks_several_leaves_p (requests)
  local leaves = 0
  for _, request in ipairs(requests) do
    if not request.ready then
      if vim.tbl_contains(request.objects, 'all') then return true end
      leaves = leaves + #request.objects end
  end
  return leaves > 1
end

M.WIDE_UNMARK_QUESTION =
  'Unmarking this node commit-ready will unmark every contributing'
  .. ' decision. You might discard a lot of hard-won information.'
  .. ' Continue (yes/no)? '

---Write REQUESTS, first asking when they unmark more than one leaf;
---only the typed word "yes" proceeds. On refusal, write nothing and
---return false.
---@param requests table[]
---@return boolean
function M.write_requests_if_confirmed (requests)
  if M.unmarks_several_leaves_p(requests)
     and vim.fn.input(M.WIDE_UNMARK_QUESTION) ~= 'yes' then
    return false end
  for _, request in ipairs(requests) do
    M.write_request(request.line, request.ready, request.objects)
  end
  return true
end

---Write REQUESTS if confirmed, then save; or report that there were
---none.
---@param requests table[]
local function write_and_save (requests)
  if #requests == 0 then
    error('Nothing here can be marked commit-ready') end
  if M.write_requests_if_confirmed(requests) then
    require('skg.save').request_save_buffer() end
end

---Toggle the commit-readiness of the change at point, then save. On a
---headline that names one leaf, toggle it; on a content member that
---names two (∃ and ~), ask which. On a changed folder, toggle each
---leaf one level down; on a changed node, every leaf it owns. Before
---unmarking more than one leaf, ask.
function M.cycle_commit_ready ()
  write_and_save(M.requests_at_line(metadata.current_line_number()))
end

---Mark (or, if the headline at point is ready, unmark) every leaf
---named in the subtree at point, then save. Before unmarking more
---than one leaf, ask.
function M.cycle_commit_ready_recursive ()
  write_and_save(
    M.requests_for_subtree_at_line(metadata.current_line_number()))
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

---Toggle the commit-readiness of the home-skgrepo move of the node at
---point (or of the move at point): unmark it if it is ready, else
---mark it. With RECURSIVE, do the same to the move of every
---descendant that moved between the same two skgrepos. Before
---unmarking more than one move, ask. Then save.
---@param recursive boolean|nil
function M.toggle_homeSkgrepo_move_commitReady (recursive)
  local move_line, pair =
    M.home_move_at_line(metadata.current_line_number())
  if not move_line then
    error("This node's home skgrepo did not change") end
  local ready = not M.ready_p(
    metadata.metadata_sexp_at_line_or_nil(move_line))
  local requests = {}
  if not recursive then
    requests = { { line = move_line, ready = ready, objects = { 'single' } } }
  else
    local node_line =
      metadata.parent_headline_line(move_line,
                                    metadata.outline_level(move_line))
    for _, line in ipairs(M.subtree_lines(node_line)) do
      local headline = metadata.metadata_sexp_at_line_or_nil(line)
      local other = headline and M.home_move(headline)
      if other and other[1] == pair[1] and other[2] == pair[2] then
        table.insert(requests,
                     { line = line, ready = ready, objects = { 'single' } }) end
    end
  end
  if not M.write_requests_if_confirmed(requests) then return end
  if ready then
    vim.notify(string.format('Marked %d home move(s) ready.', #requests))
  else
    vim.notify(string.format('Unmarked %d home move(s).', #requests)) end
  require('skg.save').request_save_buffer()
end

function M.toggle_homeSkgrepo_move_commitReady_recursive ()
  M.toggle_homeSkgrepo_move_commitReady(true)
end

return M
