-- PURPOSE: The commit-readiness gestures of the diff view
-- ('docs/diff-view.org', "Readiness requests and gestures"). Each writes
-- readiness requests, '(readyRequest OBJECTS)' or '(unreadyRequest
-- OBJECTS)', into headline metadata, then saves. The save consumes
-- them, setting or clearing the marks, and answers with the buffer
-- redrawn and the readiness summary.
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

---The list in METADATA_SEXP that holds its '(diff ...)' form: the
---form among its elements that holds one, as a vognode's or a
---phantom's does, if one does, else the whole sexp, as a property's.
---@param metadata_sexp any
---@return any
function M.host (metadata_sexp)
  for i = 2, #metadata_sexp do
    local element = metadata_sexp[i]
    if sexpr.is_list(element) and child_with_head(element, 'diff') then
      return element end
  end
  return metadata_sexp
end

---The '(diff ...)' form of METADATA_SEXP, or nil.
---@param metadata_sexp any
---@return any|nil
function M.diff_form (metadata_sexp)
  return child_with_head(M.host(metadata_sexp), 'diff')
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
---@return boolean is every one of OBJECTS ready? One is when the
---headline's '(ready ...)' names it, or is a bare '(ready)', which
---names the headline's only leaf or aggregate.
function M.objects_ready_p (metadata_sexp, objects)
  local ready = M.form(metadata_sexp, 'ready')
  if not ready then return false end
  local ready_objects = {}
  local bare = true
  for _, object in ipairs(arguments(ready)) do
    ready_objects[object] = true
    bare = false
  end
  for _, object in ipairs(objects) do
    if not bare and not ready_objects[object] then return false end
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
---the objects it lists, or a folder that carries (ready).
---@param metadata_sexp any
---@return boolean
function M.ready_p (metadata_sexp)
  local leaves = M.leaves(metadata_sexp)
  if leaves then return M.objects_ready_p(metadata_sexp, leaves.objects) end
  -- A folder's bare '(ready)' stands outside any '(diff ...)'.
  return M.changed_folder_p(metadata_sexp)
         and child_with_head(metadata_sexp, 'ready') ~= nil
end

-- ── moving between change headlines ─────────────────────────────
-- A change headline is one whose metadata carries a '(leaves ...)'
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

---Move point to the nearest change headline after point (DIRECTION
---'next') or before it (DIRECTION 'previous'), in buffer order,
---without wrapping: one whose readiness is READINESS ('ready' or
---'unready'), or any change headline when READINESS is nil. Point
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

---On a change headline at LINE_NUMBER whose readiness is READINESS
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
---one that marks (READY true) or unmarks the leaves on OBJECTS. The
---request is the last part of the headline's '(diff ...)' form, which
---lies under '(node ...)' or another phantom's form, or at top level
---on a property. A headline without the form gets one.
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
  local diff_edit = { sexpr.symbol('diff') }
  if M.diff_form(metadata_sexp) then
    table.insert(diff_edit, { sexpr.symbol('DELETE'),
                              { sexpr.symbol('readyRequest') },
                              { sexpr.symbol('unreadyRequest') } })
  end -- else a missing form is appended whole, so it holds no DELETE
  table.insert(diff_edit, request)
  local instruction = { sexpr.symbol('skg') }
  if host == metadata_sexp then
    table.insert(instruction, diff_edit)
  else
    table.insert(instruction, { host[1], diff_edit }) end
  metadata.edit_metadata_at_line(line_number, instruction)
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
---LINE_NUMBER: on a headline that names leaves, one naming every
---object it lists.
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
  local ready = not M.ready_p(metadata_sexp)
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
