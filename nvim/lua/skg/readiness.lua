-- PURPOSE: The commit-readiness gestures of the diff view
-- ('docs/diff-view.org', "Readiness requests and gestures"). Each writes
-- readiness requests, '(ready OBJECTS)' or '(unready OBJECTS)', into
-- the '(editRequest ...)' of headline metadata, then saves. The save
-- consumes them, setting or clearing the marks, and answers with the
-- buffer rerendered and the readiness summary.
-- The Lua port of elisp/skg-readiness.el, which describes how the
-- '(leaves N OBJECT...)' part of the server's '(diff ...)' form says
-- which leaves a headline names. Neovim has no prefix argument, so each recursive form is its
-- own command.
-- It also formats the readiness summary, which the save,
-- stage-and-commit and integrate-unshown-changes show.

local metadata = require('skg.metadata')
local modify_graph = require('skg.modify_graph')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local shared = require('skg.shared')

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

---The element of LIST, after its head, whose head is KEY, or nil.
---@param list any
---@param key string
---@return any|nil
local function child_with_head (list, key)
  for i = 2, #list do
    if head_name(list[i]) == key then return list[i] end
  end
  return nil
end

---The '(diff ...)' form of METADATA_SEXP, which every kind keeps at
---the top level, or nil.
---@param metadata_sexp any
---@return any|nil
function M.diff_form (metadata_sexp)
  return child_with_head(metadata_sexp, 'diff')
end

---The (KEY ...) part of METADATA_SEXP's '(diff ...)' form, or nil.
---@param metadata_sexp any
---@param key string
---@return any|nil
function M.form (metadata_sexp, key)
  local diff = M.diff_form(metadata_sexp)
  if not diff then return nil end
  return child_with_head(diff, key)
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

---The same, for a headline below the one a gesture acts on, without
---'all': nil when 'all' was its only object.
---@param metadata_sexp any
---@return table|nil
function M.leaves_below (metadata_sexp)
  local leaves = M.leaves(metadata_sexp)
  if not (leaves and vim.tbl_contains(leaves.objects, 'all')) then
    return leaves end
  local objects = vim.tbl_filter(function (object)
    return object ~= 'all' end, leaves.objects)
  if #objects == 0 then return nil end
  return { count = #objects, objects = objects }
end

---@param metadata_sexp any
---@param objects string[]
---@return boolean is every one of OBJECTS ready? It is when the
---headline carries a '(ready ...)' that names each of them. A
---headline that lists no object (one naming a single leaf) is ready
---when it carries any '(ready ...)'.
function M.objects_ready_p (metadata_sexp, objects)
  local ready = M.form(metadata_sexp, 'ready')
  if not ready then return false end
  local ready_objects = {}
  for _, object in ipairs(arguments(ready)) do
    ready_objects[object] = true
  end
  for _, object in ipairs(objects) do
    if not ready_objects[object] then return false end
  end
  return true
end

---Whether a folder member carries a change of its relationship to
---the folder's viewparent, and if so whether that change is ready.
---MEMBER_SEXP is the member's metadata; CHILD_SEXPS that of its own
---child headlines. The member carries such a change when its
---'(diff ...)' has a '(rel ...)', or when one of its children is a
---'(relRepoMove ...)'. The change is ready when both of these hold:
---if the member has a '(rel ...)', its '(diff ...)' has a '(ready
---...)' that, when its '(leaves ...)' lists 'rel', names 'rel'; if
---it has a '(relRepoMove ...)' child, that child's '(diff ...)' has
---a '(ready ...)'.
---@param member_sexp any
---@param child_sexps any[]
---@return boolean|nil nil when the member carries no such change,
---else whether it is ready
function M.membership_change_ready (member_sexp, child_sexps)
  local changed, ready = false, true
  if M.form(member_sexp, 'rel') then
    changed = true
    local leaves = M.leaves(member_sexp)
    local objects = {}
    if leaves and vim.tbl_contains(leaves.objects, 'rel') then
      objects = { 'rel' } end
    ready = M.objects_ready_p(member_sexp, objects)
  end
  for _, child in ipairs(child_sexps) do
    if child_with_head(child, 'relRepoMove') then
      changed = true
      ready = ready and M.form(child, 'ready') ~= nil end
  end
  if not changed then return nil end
  return ready
end

---The level of the headline LINE (its star count), or nil when LINE
---is no headline.
---@param line string|nil
---@return integer|nil
local function headline_level (line)
  local stars = line and line:match('^(%*+)%s')
  return stars and #stars or nil
end

---Whether the folder whose headline is LINES[INDEX] is changed, and
---if so whether it is ready. LINES are a buffer's lines, 1-based. The
---folder is changed when one of its members (its direct child
---headlines) carries a change of its relationship to the folder's
---viewparent (M.membership_change_ready), and ready when every such
---change is ready. Nil when the headline is no folder or no member
---carries such a change; else { ready = BOOLEAN }.
---@param lines string[]
---@param index integer
---@return table|nil
function M.folder_change_in_lines (lines, index)
  local level = headline_level(lines[index])
  if not (level and metadata.folder_atom(metadata.metadata_sexp_of_line(lines[index]))) then
    return nil end
  local members = {} -- each { sexp, child_sexps }
  for i = index + 1, #lines do
    local line_level = headline_level(lines[i])
    if line_level then
      if line_level <= level then break end
      if line_level == level + 1 then
        table.insert(members, { sexp = metadata.metadata_sexp_of_line(lines[i]),
                                child_sexps = {} })
      elseif line_level == level + 2 and #members > 0 then
        local child = metadata.metadata_sexp_of_line(lines[i])
        if child then
          table.insert(members[#members].child_sexps, child) end
      end
    end
  end
  local changed, ready = false, true
  for _, member in ipairs(members) do
    local member_ready = member.sexp
      and M.membership_change_ready(member.sexp, member.child_sexps)
    if member_ready ~= nil then
      changed = true
      ready = ready and member_ready end
  end
  if not changed then return nil end
  return { ready = ready }
end

---M.folder_change_in_lines for the headline at LINE_NUMBER of the
---current buffer.
---@param line_number integer
---@return table|nil
function M.folder_change_at_line (line_number)
  return M.folder_change_in_lines(
    vim.api.nvim_buf_get_lines(0, 0, -1, false), line_number)
end

-- ── a pending detachment ───────────────────────────────────────────
-- A child the user marked 'noClaim' under an editable parent stays in
-- the buffer but leaves the parent's list at the next save. Until then
-- it is warranted and claims nothing, which no part of its own
-- metadata says, so the heralds ask here and render it as a request.

---@param metadata_sexp any
---@param parent_sexp any|nil
---@param grandparent_sexp any|nil
---@return boolean is METADATA_SEXP, under a parent with PARENT_SEXP
---whose own parent has GRANDPARENT_SEXP, a pending detachment: does it
---carry 'noClaim', does the graph warrant it, and is its parent
---editable?
function M.pending_detachment_p (metadata_sexp, parent_sexp,
                                 grandparent_sexp)
  return sexpr.is_list(metadata_sexp)
         and not metadata.node_member_p(metadata_sexp)
         and M.warranted_by_the_graph_p(metadata_sexp)
         and M.editable_parent_p(parent_sexp, grandparent_sexp)
end

---@param metadata_sexp any
---@return boolean is it warranted by the graph: does its '(view ...)'
---carry neither 'noWarrant' nor 'ghostWarrant', and its '(diff ...)'
---no '(rel removed)'? What HEAD alone warrants is no detachment.
function M.warranted_by_the_graph_p (metadata_sexp)
  local view_atoms = arguments(child_with_head(metadata_sexp, 'view'))
  return not (vim.tbl_contains(view_atoms, 'noWarrant')
              or vim.tbl_contains(view_atoms, 'ghostWarrant')
              or metadata.relationship_removed_p(metadata_sexp))
end

---@param sexp any|nil
---@return boolean is it a node that is not write-protected?
local function editable_node_p (sexp)
  return metadata.occurrence_sexp_p(sexp)
         and not metadata.node_write_protected_p(sexp)
end

---@param parent_sexp any|nil
---@param grandparent_sexp any|nil
---@return boolean does a headline under a parent with PARENT_SEXP,
---whose own parent has GRANDPARENT_SEXP, stand under an editable
---parent: a node that is not write-protected, or a folder
---'shared/herald-styles.json' does not list as write-protected whose
---anchor is such a node?
function M.editable_parent_p (parent_sexp, grandparent_sexp)
  local folder = metadata.folder_atom(parent_sexp)
  if folder then
    return not vim.tbl_contains(
             shared.herald_styles.folder_heralds.write_protected, folder)
           and editable_node_p(grandparent_sexp) end
  return editable_node_p(parent_sexp)
end

---M.pending_detachment_p of the headline LINES[INDEX], LINES being a
---buffer's lines, 1-based. Reads headline levels by their stars, and
---looks for the parents only of a headline carrying 'noClaim'.
---@param lines string[]
---@param index integer
---@return boolean
function M.pending_detachment_in_lines (lines, index)
  local metadata_sexp = metadata.metadata_sexp_of_line(lines[index])
  if not (metadata_sexp and not metadata.node_member_p(metadata_sexp)) then
    return false end
  local ancestors = M.ancestor_sexps_in_lines(lines, index, 2)
  return M.pending_detachment_p(metadata_sexp, ancestors[1], ancestors[2])
end

---The metadata of the COUNT nearest ancestor headlines of the
---headline LINES[INDEX], nearest first: element N is nil where the
---Nth ancestor has no metadata or there is none.
---@param lines string[]
---@param index integer
---@param count integer
---@return table
function M.ancestor_sexps_in_lines (lines, index, count)
  local ancestors = {}
  for generation, ancestor_index
      in ipairs(M.ancestor_indices_in_lines(lines, index, count)) do
    ancestors[generation] =
      metadata.metadata_sexp_of_line(lines[ancestor_index]) end
  return ancestors
end

---The indices in LINES of the COUNT nearest ancestor headlines of the
---headline LINES[INDEX], nearest first; fewer when it has fewer.
---LINES are a buffer's lines, 1-based.
---@param lines string[]
---@param index integer
---@param count integer
---@return integer[]
function M.ancestor_indices_in_lines (lines, index, count)
  local ancestor_indices = {}
  local level = headline_level(lines[index])
  local ancestor_index = index
  for _ = 1, count do
    if not (level and level > 1) then break end
    repeat ancestor_index = ancestor_index - 1
    until ancestor_index < 1
          or (headline_level(lines[ancestor_index]) or level) < level
    if ancestor_index < 1 then break end
    level = headline_level(lines[ancestor_index])
    table.insert(ancestor_indices, ancestor_index)
  end
  return ancestor_indices
end

---Whether the headline is ready: one that names leaves, on all the
---objects it lists. (A changed folder's readiness depends on its
---members: M.ready_at_line.)
---@param metadata_sexp any
---@return boolean
function M.ready_p (metadata_sexp)
  local leaves = M.leaves(metadata_sexp)
  return leaves ~= nil and M.objects_ready_p(metadata_sexp, leaves.objects)
end

---Whether the headline at LINE_NUMBER is ready: a changed folder when
---every change of its members' relationships is, any other headline
---as M.ready_p says.
---@param line_number integer
---@return boolean
function M.ready_at_line (line_number)
  local folder_change = M.folder_change_at_line(line_number)
  if folder_change then return folder_change.ready end
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  return metadata_sexp ~= nil and M.ready_p(metadata_sexp)
end

-- ── moving between headlines with change facts ─────────────────────────────
-- A headline with change facts is one whose metadata carries a '(leaves ...)'
-- fact. It is ready when its '(ready ...)' names every object that
-- fact lists, and otherwise unready. Outside the diff view no
-- headline carries one, so these commands report no change there.

function M.diffView_goto_next_change ()
  M.diffView_goto_change('next', nil)
end

function M.diffView_goto_next_unready_change ()
  M.diffView_goto_change('next', 'unready')
end

function M.diffView_goto_next_ready_change ()
  M.diffView_goto_change('next', 'ready')
end

function M.diffView_goto_previous_change ()
  M.diffView_goto_change('previous', nil)
end

function M.diffView_goto_previous_unready_change ()
  M.diffView_goto_change('previous', 'unready')
end

function M.diffView_goto_previous_ready_change ()
  M.diffView_goto_change('previous', 'ready')
end

---Move point to the nearest headline with change facts after point (DIRECTION
---'next') or before it (DIRECTION 'previous'), in buffer order,
---without wrapping: one whose readiness is READINESS ('ready' or
---'unready'), or any headline with change facts when READINESS is nil. Point
---lands where id_next puts it on a headline, the opening paren of its
---metadata. When there is none, say so and leave point.
---@param direction string
---@param readiness string|nil
function M.diffView_goto_change (direction, readiness)
  local target = M.diffView_change_position(direction, readiness)
  if target then
    vim.api.nvim_win_set_cursor(0, target)
  else
    vim.notify('No '
               .. (direction == 'next' and 'further' or 'earlier')
               .. (readiness and (' ' .. readiness) or '')
               .. ' change') end
end

---Where diffView_goto_change would move point, as {line, col}, or nil.
---@param direction string
---@param readiness string|nil
---@return table|nil
function M.diffView_change_position (direction, readiness)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local col = M.diffView_change_start_on_line(cursor[1], readiness)
  if col and (direction == 'next' and col > cursor[2]
              or direction == 'previous' and col < cursor[2]) then
    return { cursor[1], col } end
  local step = direction == 'next' and metadata.next_headline_line
                                   or metadata.previous_headline_line
  local line_number = step(cursor[1])
  while line_number do
    col = M.diffView_change_start_on_line(line_number, readiness)
    if col then return { line_number, col } end
    line_number = step(line_number)
  end
  return nil
end

---On a headline with change facts at LINE_NUMBER whose readiness is READINESS
---(any when nil), the 0-based column of its metadata's opening
---paren; otherwise nil.
---@param line_number integer
---@param readiness string|nil
---@return integer|nil
function M.diffView_change_start_on_line (line_number, readiness)
  local stars = metadata.line_text(line_number):match('^(%*+ )%(skg')
  if not stars then return nil end
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not (metadata_sexp and M.leaves(metadata_sexp)) then return nil end
  if readiness == 'ready' and not M.ready_p(metadata_sexp)
     or readiness == 'unready' and M.ready_p(metadata_sexp) then
    return nil end
  return #stars
end

---The {old, new} skgrepos of METADATA_SEXP when it is a home skgrepo move.
---@param metadata_sexp any
---@return string[]|nil
function M.homeSkgrepoMove (metadata_sexp)
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

---The heads of the top-level forms the server writes after
---'(editRequest ...)'; a new one is placed before the first of them.
local FORMS_AFTER_EDIT_REQUEST = { viewRequests = true, view = true,
                                   diff = true }

---@param request any a part of an '(editRequest ...)'
---@return boolean is it a readiness request, '(ready OBJECT...)' or
---'(unready OBJECT...)'?
local function readiness_request_p (request)
  local head = head_name(request)
  return head == 'ready' or head == 'unready'
end

---METADATA_SEXP with its '(editRequest ...)' holding the requests
---REQUESTS_FROM returns when given those it holds (none when the
---headline has no such form). An '(editRequest ...)' left empty goes. A
---headline without the form gets one, where the server writes it:
---before its '(viewRequests ...)', '(view ...)' and '(diff ...)'.
---@param metadata_sexp any
---@param requests_from fun(requests: any[]): any[]
---@return any
local function with_edit_request_form (metadata_sexp, requests_from)
  local old_form = child_with_head(metadata_sexp, 'editRequest')
  local new_form = { sexpr.symbol('editRequest') }
  for _, request in ipairs(requests_from(
                             old_form and { unpack(old_form, 2) } or {})) do
    table.insert(new_form, request) end
  if #new_form == 1 then new_form = nil end
  local unplaced_form = not old_form and new_form or nil
  local result = { metadata_sexp[1] }
  for i = 2, #metadata_sexp do
    local element = metadata_sexp[i]
    if element == old_form then
      if new_form then table.insert(result, new_form) end
    else
      if unplaced_form and FORMS_AFTER_EDIT_REQUEST[head_name(element)] then
        table.insert(result, unplaced_form)
        unplaced_form = nil end
      table.insert(result, element) end
  end
  if unplaced_form then table.insert(result, unplaced_form) end
  return result
end

---METADATA_SEXP with READINESS_REQUEST, '(ready OBJECT...)' or
---'(unready OBJECT...)', in its '(editRequest ...)' in place of any
---readiness request there, after the headline's other request if it
---has one. A headline without the form gets one, where the server
---writes it: before its '(viewRequests ...)', '(view ...)' and '(diff
---...)'.
---@param metadata_sexp any
---@param readiness_request any
---@return any
function M.with_readiness_request (metadata_sexp, readiness_request)
  return with_edit_request_form(metadata_sexp, function (requests)
    local kept = vim.tbl_filter(
      function (request) return not readiness_request_p(request) end,
      requests)
    table.insert(kept, readiness_request)
    return kept end)
end

---METADATA_SEXP with EDIT_REQUEST as the edit request of its
---'(editRequest ...)': any edit request there replaced, its readiness
---request kept after it. A nil EDIT_REQUEST removes the edit request,
---and with it an '(editRequest ...)' left empty. A headline without the
---form gets one, where M.with_readiness_request puts it. The
---counterpart of M.with_readiness_request.
---@param metadata_sexp any
---@param edit_request any|nil
---@return any
function M.with_edit_request (metadata_sexp, edit_request)
  return with_edit_request_form(metadata_sexp, function (requests)
    local kept = vim.tbl_filter(readiness_request_p, requests)
    if edit_request ~= nil then table.insert(kept, 1, edit_request) end
    return kept end)
end

---Replace the readiness request of the headline at LINE_NUMBER with
---one that marks (READY true) or unmarks the leaves on OBJECTS: '(ready
---OBJECTS)' or '(unready OBJECTS)' in its '(editRequest ...)'
---(M.with_readiness_request).
---@param line_number integer
---@param ready boolean
---@param objects string[]
function M.write_request (line_number, ready, objects)
  local split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line_number))
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not (split and metadata_sexp) then return end
  local request = { sexpr.symbol(ready and 'ready' or 'unready') }
  for _, object in ipairs(objects) do
    table.insert(request, sexpr.symbol(object))
  end
  metadata.replace_line(line_number, metadata.format_headline(
    split.stars,
    sexpr.to_string(M.with_readiness_request(metadata_sexp, request)),
    split.title))
end

---A request on the headline at LINE_NUMBER, with how many leaves it
---touches as the server counts them: N of the headline's '(leaves N
---OBJECT...)' when OBJECTS are the ones it lists, else one for each of
---'node' and 'rel'. A headline without the fact, which only the
---home skgrepo move gesture can reach, counts one.
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
---LINE_NUMBER: on a headline that names leaves, one naming every
---object it lists; on a changed folder, one on each headline one
---level down that names leaves, naming the objects it lists but
---'all', readying them unless the folder is ready.
---@param line_number integer
---@return table[]
function M.requests_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return {} end
  local leaves = M.leaves(metadata_sexp)
  if leaves then
    return { M.request_at_line(
               line_number,
               not M.objects_ready_p(metadata_sexp, leaves.objects),
               leaves.objects) } end
  local folder_change = M.folder_change_at_line(line_number)
  if folder_change then
    local ready = not folder_change.ready
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
  local ready = not M.ready_at_line(line_number)
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
---headline that names leaves, toggle every object it lists: on a new
---member its existence and its relationship (∃ and ~), on a modified
---node (M) every leaf its file records, beside its relationship when
---that changed. On a changed folder, toggle each leaf one level down.
---Before unmarking more than one leaf, ask.
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

---The line of the home skgrepo move at LINE_NUMBER (that headline, or the
---child of the node there that is its move) and its {old, new}.
---@param line_number integer
---@return integer|nil, string[]|nil
function M.homeSkgrepoMove_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  local pair = metadata_sexp and M.homeSkgrepoMove(metadata_sexp)
  if pair then return line_number, pair end
  for _, line in ipairs(M.child_lines(line_number)) do
    local child = metadata.metadata_sexp_at_line_or_nil(line)
    pair = child and M.homeSkgrepoMove(child)
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
    M.homeSkgrepoMove_at_line(metadata.current_line_number())
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
      local other = headline and M.homeSkgrepoMove(headline)
      if other and other[1] == pair[1] and other[2] == pair[2] then
        table.insert(requests, M.request_at_line(line, ready, {})) end
    end
  end
  if not M.write_requests_if_confirmed(requests) then return end
  if ready then
    vim.notify(string.format('Marked %d home skgrepo move(s) ready.', #requests))
  else
    vim.notify(string.format('Unmarked %d home skgrepo move(s).', #requests)) end
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
      'Commit ready. All %d of %d changed files are ready',
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
