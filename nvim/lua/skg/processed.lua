-- PURPOSE: Marking the changes of the diff view processed, and moving
-- between them ('docs/diff-view.org', "Processed requests and
-- gestures"). A gesture is <localleader>m, then 'n' (the node), 'r'
-- (its relationship to its viewparent) or 'b' (both), then 'p'
-- (processed) or 'u' (unprocessed); <localleader>mh then 'p' or 'u'
-- marks a home-skgrepo move. Each writes processed requests,
-- '(processed OBJECTS)' or '(unprocessed OBJECTS)', into the
-- '(editRequest ...)' of headline metadata, then saves; the save
-- consumes them, setting or clearing the processed marks, and answers
-- with the buffer rerendered and the processed summary. The motions
-- <localleader>rn and <localleader>rp, then 'c' (any), 'u' or 'p',
-- move to the next or previous headline with change facts.
-- Entry points: 'mark', behind 'mark_node_processed' and the other
-- seventeen ':SkgMark{Node,Rel,Both}*' commands;
-- 'mark_homeSkgrepo_move', behind the four
-- ':SkgMarkHomeSkgrepoMove*'; 'diffView_goto_change', behind the
-- twelve ':SkgDiffViewGoto*'; and 'processed_summary_message', the
-- processed summary that the save, the diff view, stage-and-commit and
-- extend-diff-view show. What a headline's change facts say, and
-- whether a folder's change is processed, are read in
-- skg.change_facts; how a request is placed in '(editRequest ...)',
-- in skg.metadata ('metadata_with_processed_request'); the save is
-- skg.save's. The Lua port of elisp/skg-processed.el.
--
-- elisp/skg-processed.el describes how the '(leaves N OBJECT...)'
-- part of the server's '(diff ...)' form says which leaves a headline
-- names, and what a letter names on each kind of headline. Neovim has
-- no prefix argument, so each recursive form is its own command: a
-- capital last letter for contentward ('mnP'), a capital 'M' for
-- outward ('Mnp'); on a home-skgrepo move, a capital last letter
-- ('mhP') marks every move like it in the subtree. On a motion, a
-- capital last letter ('rnC')
-- also visits the ☮ members of "(other files)" folders, which the
-- plain motions skip.

local change_facts = require('skg.change_facts')
local metadata = require('skg.metadata')
local modify_graph = require('skg.modify_graph')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local shared = require('skg.shared')

local M = {}

M.WIDE_UNMARK_QUESTION =
  'Unmarking this node processed will unmark every contributing'
  .. ' decision. You might discard a lot of hard-won information.'
  .. ' Continue (yes/no)? '

---What a gesture with LETTER_OBJECTS says when it names nothing.
---@param letter_objects string[]
---@return string
local function nothing_to_mark_text (letter_objects)
  if #letter_objects == 2 then
    return 'Neither its node nor its relationship has a change to mark here' end
  if letter_objects[1] == 'node' then
    return 'Its node has no change to mark here' end
  return 'Its relationship to its viewparent has no change to mark here'
end

---Write REQUESTS if confirmed, then save; or refuse when there are
---none.
---@param requests table[]
---@param letter_objects string[]
local function write_and_save (requests, letter_objects)
  if #requests == 0 then
    error(nothing_to_mark_text(letter_objects), 0) end
  if M.write_requests_if_confirmed(requests) then
    require('skg.save').request_save_buffer() end
end

---Mark (PROCESSED true) or unmark what LETTER_OBJECTS, {'node'},
---{'rel'} or {'node', 'rel'}, name on the headline at point, then
---save. On a changed folder, act on each headline one level down.
---With WALK_OBJECT, 'newContent' or 'newOutward', act on every headline
---in the subtree at point, except that at a new node in content
---position the server walks the graph instead of the buffer, the
---letters filtering what the walk marks ('requests_for_subtree_at_line').
---Before unmarking more than one leaf, ask. Works on write-protected
---occurrences too.
---@param letter_objects string[]
---@param processed boolean
---@param walk_object string|nil
function M.mark (letter_objects, processed, walk_object)
  if not metadata.at_headline_p() then error('Not on a headline', 0) end
  local line_number = metadata.current_line_number()
  local requests
  if walk_object then
    requests = M.requests_for_subtree_at_line(
      line_number, letter_objects, processed, walk_object)
  else
    requests = M.requests_at_line(line_number, letter_objects, processed) end
  write_and_save(requests, letter_objects)
end

-- The commands: mark_{node,rel,both}_{processed,unprocessed}, each
-- also '_contentward' (Emacs's C-u) and '_outward' (C-u C-u).
do
  local letters = { node = { 'node' }, rel = { 'rel' },
                    both = { 'node', 'rel' } }
  local walks = { [''] = false, _contentward = 'newContent',
                  _outward = 'newOutward' }
  for letter, letter_objects in pairs(letters) do
    for way, processed in pairs({ processed = true, unprocessed = false }) do
      for suffix, walk_object in pairs(walks) do
        M['mark_' .. letter .. '_' .. way .. suffix] = function ()
          M.mark(letter_objects, processed, walk_object or nil)
        end
      end
    end
  end
end

---Mark (PROCESSED true) or unmark the home-skgrepo move of the node at
---point (or the move at point). With RECURSIVE, do the same to the move
---of every descendant that moved between the same two skgrepos. Before
---unmarking more than one move, ask. Then save.
---@param processed boolean
---@param recursive boolean|nil
function M.mark_homeSkgrepo_move (processed, recursive)
  if not metadata.at_headline_p() then error('Not on a headline', 0) end
  local move_line, pair =
    M.homeSkgrepo_move_at_line(metadata.current_line_number())
  if not move_line then
    error("This node's home skgrepo did not change", 0) end
  local requests = {}
  if not recursive then
    requests = { M.request_at_line(move_line, processed, {}) }
  else
    local node_line =
      metadata.parent_headline_line(move_line,
                                    metadata.outline_level(move_line))
    for _, line in ipairs(M.subtree_lines(node_line)) do
      local headline = metadata.metadata_sexp_at_line_or_nil(line)
      local other = headline and M.homeSkgrepo_move(headline)
      if other and other[1] == pair[1] and other[2] == pair[2] then
        table.insert(requests, M.request_at_line(line, processed, {})) end
    end
  end
  if not M.write_requests_if_confirmed(requests) then return end
  if processed then
    vim.notify(string.format('Marked %d home skgrepo move(s) processed.', #requests))
  else
    vim.notify(string.format('Unmarked %d home skgrepo move(s).', #requests)) end
  require('skg.save').request_save_buffer()
end

function M.mark_homeSkgrepo_move_processed ()
  M.mark_homeSkgrepo_move(true, false)
end

function M.mark_homeSkgrepo_move_unprocessed ()
  M.mark_homeSkgrepo_move(false, false)
end

function M.mark_homeSkgrepo_move_processed_recursive ()
  M.mark_homeSkgrepo_move(true, true)
end

function M.mark_homeSkgrepo_move_unprocessed_recursive ()
  M.mark_homeSkgrepo_move(false, true)
end

-- ── reading a headline's change facts ──────────────────────────────

---The letter object naming the one leaf of a headline that names one:
---'rel' for a relRepo move and for an unknown or restricted viewnode,
---which shows no node, only its relationship to its viewparent; else
---'node' (an alias, extra ID or flag, an old text, a home skgrepo move,
---a node's other changes). (skg--processed-single-leaf-kind)
---@param metadata_sexp any
---@return string
function M.single_leaf_kind (metadata_sexp)
  if metadata.sexp_cdr_at_path(metadata_sexp, { 'skg', 'relRepoMove' }) then
    return 'rel' end
  for i = 2, #metadata_sexp do
    local element = metadata_sexp[i]
    if not sexpr.is_list(element) then
      local text = sexpr.atom_text(element)
      if text == 'unknown' or text == 'restricted' then return 'rel' end end
  end
  return 'node'
end

---What a gesture with LETTER_OBJECTS names on the headline with
---METADATA_SEXP: nil when it names nothing there, else the request's
---objects ({} for a bare request). On a headline that lists objects,
---the letters among them. On one that names a single leaf, the bare
---request, when the leaf is of a kind a letter names (single_leaf_kind).
---On a changeset marker
---AT_GESTURE (the headline the gesture acts on), the letters
---themselves: its 'node' names its members' own leaves, which its
---'(leaves ...)' does not list; below the gesture those members name
---them under their own headlines. (skg--processed-objects-named-by-letters)
---@param metadata_sexp any
---@param letter_objects string[]
---@param at_gesture boolean|nil
---@return string[]|nil
function M.objects_named_by_letters (metadata_sexp, letter_objects, at_gesture)
  local leaves = change_facts.processed_leaves(metadata_sexp)
  if not leaves then return nil end
  if #leaves.objects == 0 then
    local kind = M.single_leaf_kind(metadata_sexp)
    if vim.tbl_contains(letter_objects, kind) then return {} end
    return nil end
  if at_gesture and metadata.sexp_cdr_at_path(
       metadata_sexp, { 'skg', 'changesetMarker' }) then
    return vim.deepcopy(letter_objects) end
  local objects = vim.tbl_filter(function (object)
    return vim.tbl_contains(leaves.objects, object) end, letter_objects)
  if #objects == 0 then return nil end
  return objects
end

---Whether the headline is processed: one that names leaves, on all the
---objects it lists. (Whether a changed folder is processed depends on its
---members: M.processed_at_line.)
---@param metadata_sexp any
---@return boolean
function M.processed_p (metadata_sexp)
  local leaves = change_facts.processed_leaves(metadata_sexp)
  return leaves ~= nil
         and change_facts.objects_processed_p(metadata_sexp, leaves.objects)
end

---Whether the headline at LINE_NUMBER is processed: a changed folder when
---every change of its members' relationships is, any other headline
---as M.processed_p says.
---@param line_number integer
---@return boolean
function M.processed_at_line (line_number)
  local folder_change = change_facts.folder_change_processed_at_line(line_number)
  if folder_change then return folder_change.processed end
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  return metadata_sexp ~= nil and M.processed_p(metadata_sexp)
end

---The {old, new} skgrepos of METADATA_SEXP when it is a home skgrepo move.
---@param metadata_sexp any
---@return string[]|nil
function M.homeSkgrepo_move (metadata_sexp)
  local function skgrepo (side)
    local values = metadata.sexp_cdr_at_path(
      metadata_sexp, { 'skg', 'homeSkgrepoMove', side })
    return values and values[1] ~= nil and sexpr.atom_text(values[1]) or nil
  end
  if not metadata.sexp_cdr_at_path(metadata_sexp,
                                   { 'skg', 'homeSkgrepoMove' }) then
    return nil end
  return { skgrepo('old'), skgrepo('new') }
end

---The line of the home skgrepo move at LINE_NUMBER (that headline, or the
---child of the node there that is its move) and its {old, new}.
---@param line_number integer
---@return integer|nil, string[]|nil
function M.homeSkgrepo_move_at_line (line_number)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  local pair = metadata_sexp and M.homeSkgrepo_move(metadata_sexp)
  if pair then return line_number, pair end
  for _, line in ipairs(modify_graph.immediate_viewchildren(line_number)) do
    local child = metadata.metadata_sexp_at_line_or_nil(line)
    pair = child and M.homeSkgrepo_move(child)
    if pair then return line, pair end
  end
  return nil, nil
end

-- ── walking headlines ──────────────────────────────────────────────

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
-- A gesture first gathers its requests, each { line, processed, objects,
-- count }, then writes them all or none.

---The requests of the one-level gesture with LETTER_OBJECTS on the
---headline at LINE_NUMBER, marking (PROCESSED true) or unmarking: on a
---headline that names leaves, one, naming what objects_named_by_letters
---says; on a changed folder, one on each child that names some of them.
---@param line_number integer
---@param letter_objects string[]
---@param processed boolean
---@return table[]
function M.requests_at_line (line_number, letter_objects, processed)
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata_sexp then return {} end
  local named = M.objects_named_by_letters(metadata_sexp, letter_objects, true)
  if named then
    return { M.request_at_line(line_number, processed, named) } end
  if change_facts.processed_leaves(metadata_sexp)
     or not change_facts.folder_change_processed_at_line(line_number) then
    return {} end
  local requests = {}
  for _, line in ipairs(modify_graph.immediate_viewchildren(line_number)) do
    local child = metadata.metadata_sexp_at_line_or_nil(line)
    local child_named = child and M.objects_named_by_letters(child, letter_objects)
    if child_named then
      table.insert(requests, M.request_at_line(line, processed, child_named)) end
  end
  return requests
end

---The requests of the recursive gesture with LETTER_OBJECTS on the
---headline at LINE_NUMBER, marking (PROCESSED true) or unmarking: one
---on every headline in the subtree that names some of them. At a new
---node in content position ('hands_to_the_server_p'), the request names
---LETTER_OBJECTS and WALK_OBJECT, 'newContent' or 'newOutward', which
---the server expands from the graph, through new nodes only, whatever
---the buffer shows or write-protects, the letters naming both the
---headline's own objects and the kinds of leaf the walk marks; so the
---walk skips the rest of that node's subtree. The letters are sent
---whole there even where the headline lists fewer, since without them
---the walk would name every kind.
---@param line_number integer
---@param letter_objects string[]
---@param processed boolean
---@param walk_object string
---@return table[]
function M.requests_for_subtree_at_line (line_number, letter_objects,
                                         processed, walk_object)
  local requests = {}
  local handed_over_level = nil -- while in a handed-over subtree, its level
  for _, line in ipairs(M.subtree_lines(line_number)) do
    local level = metadata.outline_level(line) or 0
    if handed_over_level and level <= handed_over_level then
      handed_over_level = nil end
    if not handed_over_level then
      local headline = metadata.metadata_sexp_at_line_or_nil(line)
      local at_root = line == line_number
      local handed_over = false
      if headline then
        if at_root then handed_over = change_facts.processed_node_added_p(headline)
        else handed_over = M.hands_to_the_server_p(line, headline) end end
      local named = nil
      if handed_over then
        named = vim.list_extend(vim.deepcopy(letter_objects), { walk_object })
      elseif headline then
        named = M.objects_named_by_letters(headline, letter_objects, at_root) end
      if named then
        table.insert(requests, M.request_at_line(line, processed, named)) end
      if handed_over then handed_over_level = level end
    end
  end
  return requests
end

---Does the recursive gesture hand the headline at LINE_NUMBER, with
---METADATA_SEXP, to the server? It does a new node whose viewparent is
---not a folder, so that it stands in its viewparent's content.
---@param line_number integer
---@param metadata_sexp any
---@return boolean
function M.hands_to_the_server_p (line_number, metadata_sexp)
  if not change_facts.processed_node_added_p(metadata_sexp) then return false end
  local parent = metadata.parent_line(line_number)
  return parent ~= nil and metadata.folder_atom(
    metadata.metadata_sexp_at_line_or_nil(parent)) == nil
end

---A request on the headline at LINE_NUMBER, with how many leaves it
---touches (M.leaves_touched).
---@param line_number integer
---@param processed boolean
---@param objects string[]
---@return table
function M.request_at_line (line_number, processed, objects)
  local leaves = change_facts.processed_leaves(
    metadata.metadata_sexp_at_line_or_nil(line_number))
  return { line = line_number, processed = processed, objects = objects,
           count = M.leaves_touched(leaves, objects) }
end

---How many leaves a request naming OBJECTS touches on a headline whose
---'(leaves N OBJECT...)' is LEAVES ({count, objects}), as the server
---counts them. Of the objects listed there: N when OBJECTS name them
---all; one for 'rel' alone, since it names the one relationship to the
---viewparent; N less that one for 'node' alone. An object the headline
---does not list ('node' on a changeset marker, 'newContent',
---'newOutward') counts two, so that unmarking it always asks first: the
---client cannot count what the server will reach. A headline without
---the fact, which only the home skgrepo move gesture can reach, and a
---bare request count one. (skg--processed-leaves-touched)
---@param leaves table|nil
---@param objects string[]
---@return integer
function M.leaves_touched (leaves, objects)
  local listed = leaves and leaves.objects or {}
  local named_listed, unlisted = {}, 0
  for _, object in ipairs(objects) do
    if vim.tbl_contains(listed, object) then
      table.insert(named_listed, object)
    else unlisted = unlisted + 1 end
  end
  local count
  if not leaves or #objects == 0 then count = 1
  elseif #named_listed == 0 then count = 0
  elseif #named_listed == #listed then count = leaves.count
  elseif #named_listed == 1 and named_listed[1] == 'rel' then count = 1
  else count = leaves.count - 1 end
  return 2 * unlisted + count
end

---Whether the unmark requests among REQUESTS touch more than one leaf.
---@param requests table[]
---@return boolean
function M.unmarks_several_leaves_p (requests)
  local leaves = 0
  for _, request in ipairs(requests) do
    if not request.processed then leaves = leaves + request.count end
  end
  return leaves > 1
end

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
    M.write_processed_request(request.line, request.processed, request.objects)
  end
  return true
end

---Replace the processed request of the headline at LINE_NUMBER with
---one that marks (PROCESSED true) or unmarks the leaves on OBJECTS: '(processed
---OBJECTS)' or '(unprocessed OBJECTS)' in its '(editRequest ...)'
---(metadata.metadata_with_processed_request). Refuses a line that is
---no headline or a headline without metadata, as
---skg--write-processed-request does.
---@param line_number integer
---@param processed boolean
---@param objects string[]
function M.write_processed_request (line_number, processed, objects)
  if not metadata.at_headline_p(line_number) then
    error('Not on a headline', 0) end
  if not metadata.metadata_sexp_at_line_or_nil(line_number) then
    error('Headline has no skg metadata', 0) end
  local request = { sexpr.symbol(processed and 'processed' or 'unprocessed') }
  for _, object in ipairs(objects) do
    table.insert(request, sexpr.symbol(object))
  end
  metadata.rewrite_metadata_at_line(line_number, function (metadata_sexp)
    return metadata.metadata_with_processed_request(metadata_sexp, request)
  end)
end

-- ── moving between headlines with change facts ─────────────────────────────
-- A headline with change facts is one whose metadata carries a '(leaves ...)'
-- fact. It is processed when its '(processed ...)' names every object that
-- fact lists, and otherwise unprocessed. Outside the diff view no
-- headline carries one, so these commands report no change there.
--
-- The plain commands skip a write-protected occurrence (☮, but not a ghost)
-- standing directly in a folder whose changed label says "(other files)"
-- ('other_files' in 'shared/herald-styles.json'): the member records that
-- relationship in its own file, and its change is drawn, and visited, at
-- its node's editable occurrence. The commands ending in '_anywhere' (a
-- capital last letter) visit every headline with change facts. Only the
-- headline and its parent are read: in a diff view extended to every
-- change, the plain commands reach each change at least once.

function M.diffView_goto_next_change ()
  M.diffView_goto_change('next', nil, false)
end

function M.diffView_goto_next_unprocessed_change ()
  M.diffView_goto_change('next', 'unprocessed', false)
end

function M.diffView_goto_next_processed_change ()
  M.diffView_goto_change('next', 'processed', false)
end

function M.diffView_goto_previous_change ()
  M.diffView_goto_change('previous', nil, false)
end

function M.diffView_goto_previous_unprocessed_change ()
  M.diffView_goto_change('previous', 'unprocessed', false)
end

function M.diffView_goto_previous_processed_change ()
  M.diffView_goto_change('previous', 'processed', false)
end

function M.diffView_goto_next_change_anywhere ()
  M.diffView_goto_change('next', nil, true)
end

function M.diffView_goto_next_unprocessed_change_anywhere ()
  M.diffView_goto_change('next', 'unprocessed', true)
end

function M.diffView_goto_next_processed_change_anywhere ()
  M.diffView_goto_change('next', 'processed', true)
end

function M.diffView_goto_previous_change_anywhere ()
  M.diffView_goto_change('previous', nil, true)
end

function M.diffView_goto_previous_unprocessed_change_anywhere ()
  M.diffView_goto_change('previous', 'unprocessed', true)
end

function M.diffView_goto_previous_processed_change_anywhere ()
  M.diffView_goto_change('previous', 'processed', true)
end

---Move point to the nearest headline with change facts after point (DIRECTION
---'next') or before it (DIRECTION 'previous'), in buffer order,
---without wrapping: one that is PROCESSED ('processed' or
---'unprocessed'), or any headline with change facts when PROCESSED is nil.
---Unless ANYWHERE, skip one M.diffView_change_of_other_files_p calls a
---change of other files. Point lands at the start of that headline's
---line, where it is easiest to see. When there is none, say so and
---leave point; if the command with ANYWHERE would find one, say which
---key that is.
---@param direction string
---@param processed string|nil
---@param anywhere boolean
function M.diffView_goto_change (direction, processed, anywhere)
  local target = M.diffView_change_position(direction, processed, anywhere)
  if target then
    vim.api.nvim_win_set_cursor(0, target)
  else
    vim.notify(M.diffView_no_change_message(
      direction, processed,
      not anywhere
        and M.diffView_change_position(direction, processed, true) ~= nil))
  end
end

---What a motion between changes in DIRECTION, of kind PROCESSED, says
---when it finds none. OTHERS_EXIST when it skipped some that its
---'_anywhere' command, whose key it names, would visit.
---@param direction string
---@param processed string|nil
---@param others_exist boolean
---@return string
function M.diffView_no_change_message (direction, processed, others_exist)
  local kind = (direction == 'next' and 'further' or 'earlier')
               .. (processed and (' ' .. processed) or '')
  if not others_exist then return 'No ' .. kind .. ' change' end
  local last_letter = processed == 'processed' and 'P'
                      or processed == 'unprocessed' and 'U' or 'C'
  return 'No ' .. kind
         .. ' change but ☮ ones in "(other files)" folders; <localleader>r'
         .. (direction == 'next' and 'n' or 'p') .. last_letter
         .. ' visits them'
end

---Where diffView_goto_change would move point, as {line, col}, or nil.
---@param direction string
---@param processed string|nil
---@param anywhere boolean
---@return table|nil
function M.diffView_change_position (direction, processed, anywhere)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local col = M.diffView_change_start_on_line(cursor[1], processed, anywhere)
  if col and (direction == 'next' and col > cursor[2]
              or direction == 'previous' and col < cursor[2]) then
    return { cursor[1], col } end
  local step = direction == 'next' and metadata.next_headline_line
                                   or metadata.previous_headline_line
  local line_number = step(cursor[1])
  while line_number do
    col = M.diffView_change_start_on_line(line_number, processed, anywhere)
    if col then return { line_number, col } end
    line_number = step(line_number)
  end
  return nil
end

---On a headline with change facts at LINE_NUMBER that is PROCESSED
---(any when nil), and, unless ANYWHERE, is no change of other files
---(M.diffView_change_of_other_files_p), column 0, the start of its line;
---otherwise nil.
---@param line_number integer
---@param processed string|nil
---@param anywhere boolean
---@return integer|nil
function M.diffView_change_start_on_line (line_number, processed, anywhere)
  local stars = metadata.line_text(line_number):match('^(%*+ )%(skg')
  if not stars then return nil end
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not (metadata_sexp and change_facts.processed_leaves(metadata_sexp)) then
    return nil end
  if processed == 'processed' and not M.processed_p(metadata_sexp)
     or processed == 'unprocessed' and M.processed_p(metadata_sexp) then
    return nil end
  if not anywhere
     and M.diffView_change_of_other_files_p(line_number, metadata_sexp) then
    return nil end
  return 0
end

---Is the headline at LINE_NUMBER, with METADATA_SEXP, a write-protected
---occurrence (☮, not a ghost), its parent a folder
---'shared/herald-styles.json' lists in 'folder_heralds.other_files', or
---a forksFolder where it is born only of forking the folder's
---viewparent (metadata.born_only_forking_its_folders_viewparent_p),
---which it records in its own file? Its changes are drawn at its node's
---own editable occurrence, so the plain motions skip it there. A ghost
---is visited: a deleted forker's fork, say, is drawn nowhere else. So
---is a forkee or a mutual in a forksFolder: the viewparent records
---that fork. Mirrors skg--diffView-change-of-other-files-p.
---@param line_number integer
---@param metadata_sexp any
---@return boolean
function M.diffView_change_of_other_files_p (line_number, metadata_sexp)
  if not (metadata.occurrence_sexp_p(metadata_sexp)
          and metadata.node_write_protected_p(metadata_sexp)) then
    return false end
  local parent = metadata.parent_line(line_number)
  local folder = parent and metadata.folder_atom(
    metadata.metadata_sexp_at_line_or_nil(parent))
  return folder ~= nil
         and (vim.tbl_contains(
                shared.herald_styles.folder_heralds.other_files, folder)
              or (folder == 'forksFolder'
                  and metadata.born_only_forking_its_folders_viewparent_p(
                        metadata_sexp)))
end

-- ── the processed summary ──────────────────────────────────────────

---The save response's '(processed-summary (processed N) (changed M)
---(foreign F) (order-only O) (waiting W) (notEditable K))' as a table
---of counts, or nil when it has none.
---@param response any
---@return table|nil
function M.processed_summary_from_response (response)
  if not sexpr.is_list(response) then return nil end
  for _, element in ipairs(response) do
    if sexpr.is_list(element) and element[1] ~= nil
       and not sexpr.is_list(element[1])
       and sexpr.atom_text(element[1]) == 'processed-summary' then
      local function count (key)
        local value = payload.field_text(element, key)
        return value and tonumber(value) or 0
      end
      return { processed = count('processed'), changed = count('changed'),
               foreign = count('foreign'),
               order_only = count('order-only'),
               waiting = count('waiting'),
               not_editable = count('notEditable') } end
  end
  return nil
end

---The line shown after a save of a buffer of the diff view. It counts
---nodes, the unit <localleader>rc commits; 'waiting' of the processed
---would wait. "Commit ready" means <localleader>rc would commit every
---changed node.
---@param summary table as processed_summary_from_response gives it
---@return string
function M.processed_summary_message (summary)
  local waiting = summary.waiting or 0
  local complete = summary.processed == summary.changed and waiting == 0
  local parts = {}
  if complete then
    table.insert(parts, string.format(
      'Commit ready. All %d of %d changed nodes are processed',
      summary.processed, summary.changed))
  else
    table.insert(parts, string.format(
      '%d of %d changed nodes are processed',
      summary.processed, summary.changed)) end
  if waiting > 0 then
    table.insert(parts, string.format(' (%d of them %s)', waiting,
                                      waiting == 1 and 'waits' or 'wait')) end
  if summary.not_editable > 0 then
    table.insert(parts, string.format(
      ', %d changes are not editable in this view; <localleader>ru extends the diff view to them',
      summary.not_editable)) end
  if summary.foreign > 0 then
    table.insert(parts, string.format(
      ', %d %s changed in foreign skgrepos', summary.foreign,
      summary.foreign == 1 and 'node' or 'nodes')) end
  if complete then table.insert(parts, '.') end
  return table.concat(parts)
end

return M
