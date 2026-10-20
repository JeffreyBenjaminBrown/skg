-- PURPOSE: The commit-readiness gestures of the diff view
-- ('docs/diff-view.org', "Readiness requests and gestures"). Each writes
-- readiness requests, '(readyRequest OBJECTS)' or '(unreadyRequest
-- OBJECTS)', into headline metadata, then saves. The save consumes
-- them, setting or clearing the marks, and answers with the buffer
-- redrawn and the readiness summary.
-- The Lua port of elisp/skg-readiness.el, which describes how the
-- server's '(leaves N OBJECT...)' fact says which leaves a headline
-- names. Neovim has no prefix argument, so each recursive form is its
-- own command.
-- It also formats the readiness summary, which the save,
-- stage-and-commit and integrate-unshown-changes show.

local metadata = require('skg.metadata')
local modify_graph = require('skg.modify_graph')
local payload = require('skg.payload')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')

local M = {}

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

---The list in METADATA_SEXP that holds its change facts: the form
---among its elements that holds its '(leaves ...)' fact, if one does,
---else the whole sexp.
---@param metadata_sexp any
---@return any
function M.host (metadata_sexp)
  for i = 2, #metadata_sexp do
    local element = metadata_sexp[i]
    if sexpr.is_list(element) then
      for j = 2, #element do
        if head_name(element[j]) == 'leaves' then return element end
      end
    end
  end
  return metadata_sexp
end

---@param metadata_sexp any
---@return boolean does it hold its change facts in a nested form, as
---a vognode or a phantom does?
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

---The '(leaves N OBJECT...)' fact of the headline with METADATA_SEXP,
---as { count = N, objects = OBJECTS }, or nil when it names no leaf.
---@param metadata_sexp any
---@return table|nil
function M.leaves (metadata_sexp)
  local form = M.form(metadata_sexp, 'leaves')
  if not form then return nil end
  local atoms = arguments(form)
  return { count = tonumber(atoms[1]) or 0,
           objects = vim.list_slice(atoms, 2) }
end

---The same, for a headline below the one a gesture acts on: nil when
---its objects include 'all'.
---@param metadata_sexp any
---@return table|nil
function M.leaves_below (metadata_sexp)
  local leaves = M.leaves(metadata_sexp)
  if leaves and vim.tbl_contains(leaves.objects, 'all') then return nil end
  return leaves
end

---@param metadata_sexp any
---@return boolean is it a changed node's whose request may name only
---'all'? Its readiness is not drawn.
function M.aggregate_node_p (metadata_sexp)
  local leaves = M.leaves(metadata_sexp)
  return M.vognode_p(metadata_sexp) and leaves ~= nil
         and #leaves.objects == 1 and leaves.objects[1] == 'all'
end

---@param metadata_sexp any
---@param objects string[]
---@return boolean is every one of OBJECTS ready? 'all' and no object
---at all are ready when '(ready ...)' is there.
function M.objects_ready_p (metadata_sexp, objects)
  local ready = M.form(metadata_sexp, 'ready')
  if not ready then return false end
  local ready_objects = {}
  for _, object in ipairs(arguments(ready)) do ready_objects[object] = true end
  for _, object in ipairs(objects) do
    if object ~= 'all' and not ready_objects[object] then return false end
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

---Whether the headline is ready: a headline that names leaves, on all
---the objects it lists, or a folder that carries (ready). False for an
---aggregate node, whose readiness is not drawn.
---@param metadata_sexp any
---@return boolean
function M.ready_p (metadata_sexp)
  if M.aggregate_node_p(metadata_sexp) then return false end
  local leaves = M.leaves(metadata_sexp)
  if leaves then return M.objects_ready_p(metadata_sexp, leaves.objects) end
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
  return modify_graph.headlines_with_level_between(
    metadata.outline_level(line_number) + 1,
    line_number, modify_graph.subtree_end(line_number))
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
    table.insert(request, sexpr.symbol(object))
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
    if child and (M.leaves_below(child) or M.changed_folder_p(child)) then
      some = true
      if not M.ready_p(child) then return false end
    end
  end
  return some
end

---A request on the headline at LINE_NUMBER, with how many leaves it
---touches as the server counts them: N of the headline's '(leaves N
---OBJECT...)' when OBJECTS are the ones it lists, else one for each of
---'node' and 'rel'. A headline without the fact, which only the
---home-move gesture can reach, counts one.
---@param line_number integer
---@param ready boolean
---@param objects string[]
---@return table
function M.request_at_line (line_number, ready, objects)
  local leaves = M.leaves(metadata.metadata_sexp_at_line_or_nil(line_number))
  local count = #objects
  if vim.deep_equal(objects, leaves and leaves.objects or {}) then
    count = leaves and leaves.count or 1 end
  return { line = line_number, ready = ready, objects = objects,
           count = count }
end

-- A gesture first gathers its requests, each { line, ready, objects,
-- count }, then writes them all or none.

---The requests of the one-level gesture on the headline at
---LINE_NUMBER. Empty when the object prompt is dismissed.
---@param line_number integer
---@return table[]
function M.requests_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return {} end
  if M.aggregate_node_p(metadata_sexp) then
    return { M.request_at_line(line_number,
                               not M.children_ready_p(line_number),
                               { 'all' }) } end
  local leaves = M.leaves(metadata_sexp)
  if leaves then
    local chosen = leaves.objects
    if #chosen > 1 then
      local object = picker.completing_read_with_cycle(
        'Which change (node = ∃ its existence, rel = ~ its relationship here)? ',
        { 'node', 'rel' }, { require_match = true })
      if object ~= 'node' and object ~= 'rel' then return {} end
      chosen = { object } end
    return { M.request_at_line(line_number,
                               not M.objects_ready_p(metadata_sexp, chosen),
                               chosen) } end
  if M.changed_folder_p(metadata_sexp) then
    local ready = not M.ready_p(metadata_sexp)
    local requests = {}
    for _, line in ipairs(M.child_lines(line_number)) do
      local child = metadata.metadata_sexp_at_line_or_nil(line)
      local child_leaves = child and M.leaves_below(child)
      if child_leaves then
        table.insert(requests,
                     M.request_at_line(line, ready, child_leaves.objects)) end
    end
    return requests end
  return {}
end

---The requests of the recursive gesture on the headline at
---LINE_NUMBER: one on every headline in the subtree that names leaves,
---readying them unless the headline is ready.
---@param line_number integer
---@return table[]
function M.requests_for_subtree_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return {} end
  local headline_ready
  if M.aggregate_node_p(metadata_sexp) then
    headline_ready = M.children_ready_p(line_number)
  else
    headline_ready = M.ready_p(metadata_sexp) end
  local ready = not headline_ready
  local requests = {}
  for _, line in ipairs(M.subtree_lines(line_number)) do
    local headline = metadata.metadata_sexp_at_line_or_nil(line)
    local leaves = nil
    if headline and line == line_number then
      leaves = M.leaves(headline)
    elseif headline then
      leaves = M.leaves_below(headline) end
    if leaves then
      table.insert(requests, M.request_at_line(line, ready, leaves.objects)) end
  end
  return requests
end

---Whether the unmark requests among REQUESTS touch more than one leaf.
---@param requests table[]
---@return boolean
function M.unmarks_several_leaves_p (requests)
  local leaves = 0
  for _, request in ipairs(requests) do
    if not request.ready then leaves = leaves + request.count end
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
    requests = { M.request_at_line(move_line, ready, {}) }
  else
    local node_line =
      metadata.parent_headline_line(move_line,
                                    metadata.outline_level(move_line))
    for _, line in ipairs(M.subtree_lines(node_line)) do
      local headline = metadata.metadata_sexp_at_line_or_nil(line)
      local other = headline and M.home_move(headline)
      if other and other[1] == pair[1] and other[2] == pair[2] then
        table.insert(requests, M.request_at_line(line, ready, {})) end
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

-- ── the readiness summary ──────────────────────────────────────────

---The save response's '(readiness-summary (ready N) (changed M)
---(foreign F) (order-only O) (notEditable K))' as a table of counts, or
---nil when it has none.
---@param response any
---@return table|nil
function M.readiness_summary_from_response (response)
  if not sexpr.is_list(response) then return nil end
  for _, element in ipairs(response) do
    if sexpr.is_list(element) and element[1] ~= nil
       and not sexpr.is_list(element[1])
       and sexpr.atom_text(element[1]) == 'readiness-summary' then
      local function count (key)
        local value = payload.field_text(element, key)
        return value and tonumber(value) or 0
      end
      return { ready = count('ready'), changed = count('changed'),
               foreign = count('foreign'),
               order_only = count('order-only'),
               not_editable = count('notEditable') } end
  end
  return nil
end

---The line shown after a save of a buffer of the diff view.
---@param summary table as readiness_summary_from_response gives it
---@return string
function M.readiness_summary_message (summary)
  local complete = summary.ready == summary.changed
  local parts = {}
  if complete then
    table.insert(parts, string.format(
      'COMMIT READY. All %d of %d changed files are ready',
      summary.ready, summary.changed))
  else
    table.insert(parts, string.format(
      '%d of %d changed files are ready',
      summary.ready, summary.changed)) end
  if summary.not_editable > 0 then
    table.insert(parts, string.format(
      ', %d changes are not editable in this view; <localleader>ru integrates them',
      summary.not_editable)) end
  if summary.foreign > 0 then
    table.insert(parts, string.format(
      ', %d files changed in foreign skgrepos', summary.foreign)) end
  if complete then table.insert(parts, '.') end
  return table.concat(parts)
end

return M
