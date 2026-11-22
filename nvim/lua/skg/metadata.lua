-- PURPOSE: Read and edit the '(skg ...)' metadata a view's headline
-- carries after its stars, and the commands that are only such an
-- edit, each asking the next save to do something, none saving:
-- 'delete' (':SkgDelete', <localleader>dd), 'delete_recursive'
-- (':SkgDeleteRecursive', dR), 'strip_attribution'
-- (':SkgStripAttribution'), 'detach' (':SkgDetach', st) and 'attach'
-- (':SkgAttach', sa), 'set_repo' (':SkgSetRepo', ss) and
-- 'set_repo_recursive' (':SkgSetRepoRecursive', sS), and
-- 'set_merge_request' (':SkgSetMergeRequest', sm). Also
-- 'view_without_metadata' (':SkgViewWithoutMetadata', vw: the
-- selection, metadata stripped, in a new buffer), 'beginning_of_line'
-- ('^': line start or title start) and 'todo_cycle' ('cit', 'ciT').
--
-- For code: 'split_as_stars_metadata_title' and
-- 'metadata_sexp_at_line_or_nil' read a headline's metadata;
-- 'edit_metadata_at_line' merges an edit (skg.sexpr.edit_dsl) into it,
-- each part it adds landing where the server writes it;
-- 'write_edit_request_at_line' sets its edit request, and
-- 'metadata_with_processed_request' its processed request (for
-- skg.processed); 'pending_detachment_in_lines' tells the heralds a
-- 'noClaim' child leaves its parent's list at the next save.
--
-- Elsewhere: reading the '(diff ...)' change facts is
-- skg.change_facts'; the diff view's restore requests
-- (':SkgRestoreDeletedNode', ':SkgRestoreRemovedMembership') are
-- skg.restore_requests'; the relRepo gestures (':SkgSetRelRepo',
-- ':SkgSetRelRepoRecursive') are skg.relRepo_info's. Marking a change
-- processed is skg.processed's; editing the metadata as an org tree
-- (':SkgViewMetadata') is skg.sexpr_edit's; drawing it as heralds is
-- skg.heralds'; the s-expression reader is skg.sexpr.parse.
-- The Lua port of elisp/skg-metadata.el, but for its change-facts,
-- restore and relRepo sections, ported in those three modules.
--
-- The elisp file's biggest hazard -- org-fold's fragility check
-- crashing on programmatic edits to folded headline lines, worked
-- around by 'skg-replace-current-line' -- does not exist here:
-- replacing a line with nvim_buf_set_lines (M.replace_line) is always
-- safe, and vim folds recompute from the foldexpr.
--
-- Cursor-and-line conventions: like the elisp, the commands operate
-- on the current buffer around the cursor. Lines are 1-based.

local compare = require('skg.sexpr.compare')
local edit_dsl = require('skg.sexpr.edit_dsl')
local metadata_fields = require('skg.sexpr.metadata_fields')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local shared = require('skg.shared')

local M = {}

local SKG = sexpr.symbol('skg')
local NODE = sexpr.symbol('node')

-- ── reading the parts of a list ─────────────────────────────────────

---The head symbol's name of a list, or nil.
---@param value any
---@return string|nil
function M.head_name (value)
  if sexpr.is_list(value) and value[1] ~= nil
     and not sexpr.is_list(value[1]) then
    return sexpr.atom_text(value[1]) end
  return nil
end

---The element of LIST, after its head, whose head is KEY, or nil.
---@param list any
---@param key string
---@return any|nil
function M.child_with_head (list, key)
  for i = 2, #list do
    if M.head_name(list[i]) == key then return list[i] end
  end
  return nil
end

---The atoms after the head of FORM, as strings.
---@param form any|nil
---@return string[]
function M.arguments (form)
  local result = {}
  if form then
    for i = 2, #form do
      if not sexpr.is_list(form[i]) then
        table.insert(result, sexpr.atom_text(form[i])) end
    end
  end
  return result
end

-- ── line-level primitives ──────────────────────────────────────────

---@return integer the cursor's 1-based line number
function M.current_line_number ()
  return vim.api.nvim_win_get_cursor(0)[1]
end

---@param line_number integer|nil defaults to the cursor line
---@return string that line's text
function M.line_text (line_number)
  local row = (line_number or M.current_line_number()) - 1
  return vim.api.nvim_buf_get_lines(0, row, row + 1, false)[1] or ''
end

---The level of the headline LINE, a buffer line's text (its star
---count), or nil when LINE is no headline: stars and then whitespace.
---The one test of a headline. Mirrors skg--headline-level-on-line.
---@param line string|nil
---@return integer|nil
function M.headline_level_on_line (line)
  local stars = line and line:match('^(%*+)%s')
  return stars and #stars or nil
end

---@param line_number integer|nil defaults to the cursor line
---@return boolean is that line an org headline?
function M.at_headline_p (line_number)
  return M.headline_level_on_line(M.line_text(line_number)) ~= nil
end

---@param line_number integer|nil
---@return integer|nil the headline's level (star count), or nil
function M.outline_level (line_number)
  return M.headline_level_on_line(M.line_text(line_number))
end

---The current headline in its entirety, including asterisks and
---metadata, without the trailing newline. Assumes point is on a
---headline; does not move point.
---@return string
function M.get_current_headline_text ()
  return M.line_text()
end

---@param line_number integer
---@param new_content string
function M.replace_line (line_number, new_content)
  vim.api.nvim_buf_set_lines(0, line_number - 1, line_number, false,
                             { new_content })
end

---The 1-based line of the next headline after LINE_NUMBER, or nil.
---The stand-in for outline-next-heading.
---@param line_number integer
---@return integer|nil
function M.next_headline_line (line_number)
  local last = vim.api.nvim_buf_line_count(0)
  for candidate = line_number + 1, last do
    if M.at_headline_p(candidate) then return candidate end
  end
  return nil
end

---The 1-based line of the nearest headline before LINE_NUMBER, or
---nil. The stand-in for outline-previous-heading.
---@param line_number integer
---@return integer|nil
function M.previous_headline_line (line_number)
  for candidate = line_number - 1, 1, -1 do
    if M.at_headline_p(candidate) then return candidate end
  end
  return nil
end

---The 1-based line of the nearest headline at or above LINE_NUMBER
---whose level is below LEVEL, or nil. The stand-in for
---org-up-heading-safe.
---@param line_number integer
---@param level integer
---@return integer|nil
function M.parent_headline_line (line_number, level)
  for candidate = line_number - 1, 1, -1 do
    local candidate_level = M.outline_level(candidate)
    if candidate_level and candidate_level < level then
      return candidate end
  end
  return nil
end

---The line after the subtree rooted at LINE_NUMBER (exclusive scan by
---level), or nil at buffer end. The stand-in for
---skg--goto-next-headline-after-subtree.
---@param line_number integer
---@return integer|nil
function M.next_headline_after_subtree (line_number)
  local prune_level = M.outline_level(line_number)
  local candidate = M.next_headline_line(line_number)
  while candidate
        and (M.outline_level(candidate) or 0) > prune_level do
    candidate = M.next_headline_line(candidate)
  end
  return candidate
end

-- ── headline splitting and reconstruction ──────────────────────────

---How a headline's '(skg ...)' metadata opens, after the stars, as a
---Lua pattern: '(skg' then a space or the closing parenthesis, so that a
---title such as '(skgrepos are cool)' is not read as metadata. Mirrors
---skg--metadata-opening-regexp.
M.metadata_opening_pattern = '^%(skg[ )]'

---Split HEADLINE_TEXT into { stars, metadata, title }, where metadata
---is the complete (skg ...) sexp text or '' when na. Returns nil
---when HEADLINE_TEXT is not a headline. Handles nested parens.
---@param headline_text string
---@return table|nil
function M.split_as_stars_metadata_title (headline_text)
  local trimmed = headline_text:gsub('^%s+', '')
  local stars = trimmed:match('^(%*+%s+)')
  if not stars then return nil end
  local after_stars = trimmed:sub(#stars + 1)
  if after_stars:match(M.metadata_opening_pattern) then
    local sexp_end = sexpr.find_sexp_end(after_stars)
    if not sexp_end then return nil end
    return { stars = stars,
             metadata = after_stars:sub(1, sexp_end),
             title = vim.trim(after_stars:sub(sexp_end + 1)) }
  end
  return { stars = stars, metadata = '', title = after_stars }
end

---Format a headline from STARS, METADATA (a complete (skg ...) sexp
---text, or ''), and TITLE.
---@param stars string
---@param metadata string
---@param title string
---@return string
function M.format_headline (stars, metadata, title)
  if metadata == '' then
    return string.format('%s(skg) %s', stars, title) end
  return string.format('%s%s %s', stars, metadata, title)
end

-- ── metadata reading helpers ───────────────────────────────────────

---The parsed metadata sexp of the headline at LINE_NUMBER of the
---current buffer, or nil (M.metadata_on_line). Mirrors
---skg--metadata-sexp-at-point-or-nil.
---@param line_number integer|nil
---@return any|nil
function M.metadata_sexp_at_line_or_nil (line_number)
  return M.metadata_on_line(M.line_text(line_number))
end

---The parsed metadata sexp of LINE, a buffer line's text, when it is
---a headline carrying metadata that reads; else nil. The one headline
---reader; the twin of skg--metadata-on-line, which also gives where
---the metadata stands, since Emacs's heralds need it.
---@param line string|nil
---@return any|nil
function M.metadata_on_line (line)
  if not (line and line:match('^%*+%s')) then return nil end
  local split = M.split_as_stars_metadata_title(line)
  if not split or split.metadata == '' then return nil end
  local ok, sexp = pcall(sexpr.read, split.metadata)
  return ok and sexp or nil
end

---The parsed metadata of the headline at point; errors if na.
---@return any
function M.current_headline_metadata_sexp ()
  if not M.at_headline_p() then error('Not on a headline') end
  local sexp = M.metadata_sexp_at_line_or_nil(nil)
  if not sexp then error('Headline has no skg metadata') end
  return sexp
end

---@param metadata_sexp any
---@return boolean does METADATA_SEXP describe an occurrence?
function M.occurrence_sexp_p (metadata_sexp)
  return metadata_sexp ~= nil
         and compare.subtree_p(metadata_sexp, { SKG, NODE })
end

---The values after LABEL-PATH in SEXP, as in skg-sexp-cdr-at-path.
---@param sexp any
---@param path string[] label names
---@return any[]|nil
function M.sexp_cdr_at_path (sexp, path)
  if not sexpr.is_list(sexp) or #sexp == 0
     or sexpr.is_list(sexp[1])
     or sexpr.atom_text(sexp[1]) ~= path[1] then
    return nil end
  if #path == 1 then
    local rest = {}
    for i = 2, #sexp do table.insert(rest, sexp[i]) end
    return rest end
  local subpath = {}
  for i = 2, #path do table.insert(subpath, path[i]) end
  for i = 2, #sexp do
    local element = sexp[i]
    if sexpr.is_list(element) then
      local found = M.sexp_cdr_at_path(element, subpath)
      if found then return found end
    end
  end
  return nil
end

---The first value after PATH in SEXP (M.sexp_cdr_at_path), as text,
---or nil.
---@param sexp any
---@param path string[]
---@return string|nil
function M.first_value_at_path (sexp, path)
  local values = M.sexp_cdr_at_path(sexp, path)
  if values and values[1] ~= nil then
    return sexpr.atom_text(values[1]) end
  return nil
end

---@param metadata_sexp any
---@return string|nil the node's skgrepo, if METADATA_SEXP is a node's
function M.node_repo (metadata_sexp)
  if not M.occurrence_sexp_p(metadata_sexp) then return nil end
  return M.first_value_at_path(metadata_sexp,
                             { 'skg', 'skgRepos', 'nodeRepo' })
end

---@param metadata_sexp any
---@return string|nil METADATA_SEXP's '(gnid X)' value, whatever the
---headline's kind. Mirrors skg--metadata-gnid.
function M.metadata_gnid (metadata_sexp)
  return M.first_value_at_path(metadata_sexp, { 'skg', 'gnid' })
end

---@param metadata_sexp any
---@return string|nil METADATA_SEXP's '(vnid X)' value, its viewnode
---ID. Mirrors skg--metadata-vnid.
function M.metadata_vnid (metadata_sexp)
  return M.first_value_at_path(metadata_sexp, { 'skg', 'vnid' })
end

---@param metadata_sexp any
---@return string|nil the node's id, if METADATA_SEXP is a node's
function M.node_id (metadata_sexp)
  if not M.occurrence_sexp_p(metadata_sexp) then return nil end
  return M.metadata_gnid(metadata_sexp)
end

---@return string|nil the ID of the unknown graphnode whose unknown
---viewnode is the headline at point. Acts at the cursor, as its twin
---skg--unknown-node-id-at-point acts at point.
function M.unknown_node_id_at_point ()
  local metadata_sexp = M.metadata_sexp_at_line_or_nil(nil)
  if not (metadata_sexp and compare.subtree_p(
            metadata_sexp, { SKG, sexpr.symbol('unknown') })) then
    return nil end
  return M.metadata_gnid(metadata_sexp)
end

---@param metadata_sexp any
---@return boolean does it lack the bare noClaim atom: does the headline
---claim membership of its parent's list, where its parent reads claims
---(M.reads_claims_p)?
function M.claims_membership_p (metadata_sexp)
  return not compare.subtree_p(metadata_sexp,
    { SKG, sexpr.symbol('noClaim') })
end

---@param metadata_sexp any
---@return boolean does it request an editable view?
function M.node_requests_editable_view_p (metadata_sexp)
  return compare.subtree_p(metadata_sexp,
    { SKG, { sexpr.symbol('viewRequests'),
             sexpr.symbol('editableView') } })
end

---@param metadata_sexp any
---@return boolean does it carry the bare write-protected atom?
function M.node_write_protected_p (metadata_sexp)
  return compare.subtree_p(metadata_sexp,
    { SKG, sexpr.symbol('writeProtected') })
end

---@return boolean has the headline at point no skg metadata?
function M.headline_metadata_empty_p ()
  local split = M.split_as_stars_metadata_title(
    M.get_current_headline_text())
  return split == nil or split.metadata == ''
end

-- ── the central edit primitive ─────────────────────────────────────

---Use EDITS (a sexp for the edit DSL) to edit the metadata of the
---headline at point: merged into existing metadata, a part the merge
---adds landing where the server writes it
---(M.metadata_with_new_parts_at_their_ranks), or created fresh when
---there is none. No effect off a headline.
---@param edits any
function M.edit_metadata_at_point (edits)
  M.edit_metadata_at_line(M.current_line_number(), edits)
end

---@param line_number integer
---@param edits any
function M.edit_metadata_at_line (line_number, edits)
  if not M.at_headline_p(line_number) then return end
  local headline_text = M.line_text(line_number)
  local split = M.split_as_stars_metadata_title(headline_text)
  if split and split.metadata ~= '' then
    local host_sexp = sexpr.read(split.metadata)
    local merged = M.metadata_with_new_parts_at_their_ranks(
      host_sexp, edit_dsl.edit_nested_sexp(host_sexp, edits))
    M.replace_line(line_number,
      M.format_headline(split.stars, sexpr.to_string(merged),
                        split.title))
  else
    local stars, title = headline_text:match('^(%*+%s+)(.*)$')
    if stars then
      M.replace_line(line_number,
        M.format_headline(stars, sexpr.to_string(edits), title))
    end
  end
end

---Rewrite the metadata of the headline at LINE_NUMBER as REWRITE, a
---function of the metadata sexp, gives it. KIND, when given, is the
---kind atom a headline without metadata gets. Off a headline, does
---nothing. Mirrors skg--rewrite-metadata-at-point.
---@param line_number integer
---@param rewrite fun(metadata_sexp: any): any
---@param kind string|nil
function M.rewrite_metadata_at_line (line_number, rewrite, kind)
  if not M.at_headline_p(line_number) then return end
  local split = M.split_as_stars_metadata_title(M.line_text(line_number))
  if not split then return end
  local metadata_sexp = { SKG }
  if split.metadata ~= '' then metadata_sexp = sexpr.read(split.metadata)
  elseif kind then table.insert(metadata_sexp, sexpr.symbol(kind)) end
  M.replace_line(line_number, M.format_headline(
    split.stars, sexpr.to_string(rewrite(metadata_sexp)), split.title))
end

---The position in 'shared/metadata-fields.json''s 'parts' list of the
---name it gives PART, a part of '(skg ...)': a list's head, or an
---atom's name, a kind's atom standing at 'KIND''s
---(metadata_fields.part_rank). Nil when the list does not name it.
---Mirrors skg--metadata-part-rank.
---@param part any
---@return integer|nil
function M.metadata_part_rank (part)
  local name
  if sexpr.is_list(part) then
    name = part[1] ~= nil and not sexpr.is_list(part[1])
           and sexpr.atom_text(part[1]) or nil
  else
    name = sexpr.atom_text(part)
  end
  if not name then return nil end
  local rank = metadata_fields.part_rank(name)
  -- that function ranks an unlisted name after every listed one
  return rank <= #shared.metadata_parts and rank or nil
end

---METADATA_SEXP with PART, a part of '(skg ...)' it lacks, where the
---server writes it ('parts' in 'shared/metadata-fields.json'): before
---the first part the server writes after it, else last. The one
---placement every writer of a part uses. Mirrors
---skg--metadata-with-part-at-its-rank.
---@param metadata_sexp any
---@param part any
---@return any
function M.metadata_with_part_at_its_rank (metadata_sexp, part)
  local part_rank = M.metadata_part_rank(part)
  local result = { metadata_sexp[1] }
  for i = 2, #metadata_sexp do
    local element = metadata_sexp[i]
    local element_rank = M.metadata_part_rank(element)
    if part and part_rank and element_rank and element_rank > part_rank then
      table.insert(result, part)
      part = nil end
    table.insert(result, element)
  end
  if part then table.insert(result, part) end
  return result
end

---EDITED_SEXP, an edit of HOST_SEXP, with each part HOST_SEXP lacked
---moved to where the server writes it
---(M.metadata_with_part_at_its_rank), since the edit appends a part it
---adds. A part HOST_SEXP had keeps its place, and so does an element
---'shared/metadata-fields.json' does not name. Mirrors
---skg--metadata-with-new-parts-at-their-ranks.
---@param host_sexp any
---@param edited_sexp any
---@return any
function M.metadata_with_new_parts_at_their_ranks (host_sexp, edited_sexp)
  if not sexpr.is_list(edited_sexp) then return edited_sexp end
  local host_ranks = {}
  for i = 2, #host_sexp do
    local rank = M.metadata_part_rank(host_sexp[i])
    if rank then host_ranks[rank] = true end
  end
  local function new_part_p (element)
    local rank = M.metadata_part_rank(element)
    return rank ~= nil and not host_ranks[rank]
  end
  local result = { edited_sexp[1] }
  local new_parts = {}
  for i = 2, #edited_sexp do
    table.insert(new_part_p(edited_sexp[i]) and new_parts or result,
                 edited_sexp[i]) end
  for _, part in ipairs(new_parts) do
    result = M.metadata_with_part_at_its_rank(result, part) end
  return result
end

---@param request any a part of an '(editRequest ...)'
---@return boolean is it a processed request, '(processed OBJECT...)' or
---'(unprocessed OBJECT...)'?
function M.processed_request_p (request)
  local head = M.head_name(request)
  return head == 'processed' or head == 'unprocessed'
end

---METADATA_SEXP with its '(editRequest ...)' holding the requests
---REQUESTS_FROM returns when given those it holds (none when the
---headline has no such form). An '(editRequest ...)' left empty goes. A
---headline without the form gets one, where the server writes it:
---before its '(viewRequests ...)', '(view ...)' and '(diff ...)'.
---@param metadata_sexp any
---@param requests_from fun(requests: any[]): any[]
---@return any
local function metadata_with_editRequest_parts (metadata_sexp, requests_from)
  local old_form = M.child_with_head(metadata_sexp, 'editRequest')
  local new_form = { sexpr.symbol('editRequest') }
  for _, request in ipairs(requests_from(
                             old_form and { unpack(old_form, 2) } or {})) do
    table.insert(new_form, request) end
  if #new_form == 1 then new_form = nil end
  if not old_form then
    return new_form
           and M.metadata_with_part_at_its_rank(metadata_sexp, new_form)
           or metadata_sexp end
  local result = { metadata_sexp[1] }
  for i = 2, #metadata_sexp do
    local element = metadata_sexp[i]
    if element ~= old_form then table.insert(result, element)
    elseif new_form then table.insert(result, new_form) end
  end
  return result
end

---METADATA_SEXP with PROCESSED_REQUEST, '(processed OBJECT...)' or
---'(unprocessed OBJECT...)', in its '(editRequest ...)' in place of any
---processed request there, after the headline's other request if it
---has one. A headline without the form gets one, where the server
---writes it: before its '(viewRequests ...)', '(view ...)' and '(diff
---...)'.
---@param metadata_sexp any
---@param processed_request any
---@return any
function M.metadata_with_processed_request (metadata_sexp, processed_request)
  return metadata_with_editRequest_parts(metadata_sexp, function (requests)
    local kept = vim.tbl_filter(
      function (request) return not M.processed_request_p(request) end,
      requests)
    table.insert(kept, processed_request)
    return kept end)
end

---METADATA_SEXP with EDIT_REQUEST as the edit request of its
---'(editRequest ...)': any edit request there replaced, its processed
---request kept after it. A nil EDIT_REQUEST removes the edit request,
---and with it an '(editRequest ...)' left empty. A headline without the
---form gets one, where M.metadata_with_processed_request puts it. The
---counterpart of M.metadata_with_processed_request.
---@param metadata_sexp any
---@param edit_request any|nil
---@return any
function M.metadata_with_edit_request (metadata_sexp, edit_request)
  return metadata_with_editRequest_parts(metadata_sexp, function (requests)
    local kept = vim.tbl_filter(M.processed_request_p, requests)
    if edit_request ~= nil then table.insert(kept, 1, edit_request) end
    return kept end)
end

---Make REQUEST the edit request of the headline at LINE_NUMBER,
---replacing any it holds and keeping the processed request its
---'(editRequest ...)' holds (M.metadata_with_edit_request). A nil
---REQUEST removes the edit request. KIND, when given, is the kind atom
---a headline without metadata gets. Mirrors skg--write-edit-request.
---@param line_number integer
---@param request any|nil
---@param kind string|nil
function M.write_edit_request_at_line (line_number, request, kind)
  M.rewrite_metadata_at_line(line_number, function (metadata_sexp)
    return M.metadata_with_edit_request(metadata_sexp, request) end,
    kind)
end

---M.write_edit_request_at_line at the cursor's headline.
---@param request any|nil
---@param kind string|nil
function M.write_edit_request (request, kind)
  M.write_edit_request_at_line(M.current_line_number(), request, kind)
end

---Whether METADATA_SEXP is an occurrence's that already holds an edit
---request: a part of its '(editRequest ...)' that is neither a
---relRepo request nor a processed request. Mirrors
---skg--node-edit-request-p.
---@param metadata_sexp any|nil
---@return boolean
function M.node_edit_request_p (metadata_sexp)
  if not M.occurrence_sexp_p(metadata_sexp) then return false end
  for _, part in ipairs(M.sexp_cdr_at_path(
                          metadata_sexp, { 'skg', 'editRequest' }) or {}) do
    local head = sexpr.is_list(part) and sexpr.atom_text(part[1]) or nil
    if not (head == 'relRepo'
            or M.processed_request_p(part)) then
      return true end
  end
  return false
end

-- ── user commands ──────────────────────────────────────────────────

---Mark the headline at point for deletion: 'delete' becomes its edit
---request (M.write_edit_request). Then moves the cursor to the next
---line, as skg-delete does. Does not save. Its recursive form is its own
---command, M.delete_recursive, where Emacs takes a prefix argument.
function M.delete ()
  M.write_edit_request(sexpr.symbol('delete'), 'node')
  local line = M.current_line_number()
  if line < vim.api.nvim_buf_line_count(0) then
    vim.api.nvim_win_set_cursor(0, { line + 1, 0 }) end
  vim.notify(
    'This change will only be applied when you save the buffer.')
end

---Ask the next save to remove the attribution of the node at point:
---'stripAttribution' becomes its edit request (M.write_edit_request).
---Does not save. Mirrors skg-strip-attribution.
function M.strip_attribution ()
  M.write_edit_request(sexpr.symbol('stripAttribution'), 'node')
  vim.notify('The attribution goes when you save the buffer.')
end

-- ── detach and attach ──────────────────────────────────────────────
-- Under a parent that reads claims, to detach a child is to write
-- 'noClaim' on it, so that the next save takes it out of the parent's
-- list; to attach it is to remove the atom, so that the next save puts
-- it in ('docs/glossary.org', "claims membership").

---Write 'noClaim' on the headline at point, so that the next save
---takes it out of its parent's list. Refuses a headline whose parent
---reads no claims (M.reads_claims_p), and one
---that claims nothing already. Does not save. Mirrors skg-detach.
function M.detach ()
  vim.notify(M.set_claim(false))
end

---Remove 'noClaim' from the headline at point, so that the next save
---puts it in its parent's list. Refuses a headline whose parent reads
---no claims, and one that claims membership already. Does not save.
---Mirrors skg-attach.
function M.attach ()
  vim.notify(M.set_claim(true))
end

---Make the headline at point claim membership of its parent's list
---when CLAIMS, else claim nothing, by removing or writing 'noClaim'.
---Errors off a headline, on a headline whose kind cannot carry
---'noClaim', and under a parent that reads no claims.
---@param claims boolean
---@return string what the next save will do, or that nothing changed
function M.set_claim (claims)
  if not M.at_headline_p() then error('Not on a headline', 0) end
  local fields = require('skg.sexpr.metadata_fields')
  local verb = claims and 'attach' or 'detach'
  local metadata_sexp = M.metadata_sexp_at_line_or_nil(nil)
  local kind = fields.kind_of_sexp(metadata_sexp or { SKG })
  if not vim.tbl_contains(fields.field_names(kind), 'noClaim') then
    error(string.format(
      'Cannot %s: a headline of this kind (%s) claims no membership',
      verb, kind.kind), 0) end
  local ancestors = M.ancestor_metadata_sexps_in_lines(
    vim.api.nvim_buf_get_lines(0, 0, -1, false),
    M.current_line_number(), 2)
  if not M.reads_claims_p(ancestors[1], ancestors[2]) then
    error(string.format(
      'Cannot %s: the parent reads no claims (it is write-protected,'
      .. ' or no editable node), so no child claims membership of it',
      verb), 0) end
  if claims == M.claims_membership_p(metadata_sexp) then
    if claims then
      return 'This headline claims membership already; nothing to attach.'
    end
    return 'This headline claims nothing already; nothing to detach.'
  end
  if claims then
    local pending = M.pending_detachment_p(
      metadata_sexp, ancestors[1], ancestors[2])
    M.edit_metadata_at_point(sexpr.read('(skg (DELETE noClaim))'))
    if pending then
      return 'Attached: the detachment is cancelled; the next save keeps'
        .. " this headline in its parent's list."
    end
    return "Attached: the next save puts this headline in its parent's list."
  end
  M.edit_metadata_at_point(sexpr.read(
    metadata_sexp and '(skg noClaim)' or '(skg node noClaim)'))
  return "Detached: the next save takes this headline out of its parent's list."
end

---Mark the headline at point, and every occurrence viewdescendant,
---for deletion. Non-occurrence descendants (ghosts, folders, ...) are
---skipped. Does not save.
function M.delete_recursive ()
  if not M.at_headline_p() then error('Not on a headline') end
  local start_line = M.current_line_number()
  local start_level = M.outline_level(start_line)
  M.write_edit_request_at_line(start_line, sexpr.symbol('delete'), 'node')
  local line = M.next_headline_line(start_line)
  while line and (M.outline_level(line) or 0) > start_level do
    local meta = M.metadata_sexp_at_line_or_nil(line)
    if meta and M.occurrence_sexp_p(meta) then
      M.write_edit_request_at_line(line, sexpr.symbol('delete'), 'node') end
    line = M.next_headline_line(line)
  end
  vim.notify(
    'This change will only be applied when you save the buffer.')
end

---Copy the visually-selected region to a new org buffer, stripping
---skg metadata. Without a selection, does nothing.
function M.view_without_metadata ()
  local start_line = vim.fn.line('v')
  local end_line = vim.fn.line('.')
  local mode = vim.fn.mode()
  if not (mode == 'v' or mode == 'V' or mode == '\022') then
    -- Also accept a just-left visual selection via the marks.
    start_line = vim.fn.line("'<")
    end_line = vim.fn.line("'>")
    if start_line == 0 or end_line == 0 then return end
  end
  if start_line > end_line then
    start_line, end_line = end_line, start_line end
  local text = table.concat(
    vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false),
    '\n')
  local stripped = M.strip_metadata_from_org_text(text)
  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(stripped, '\n'))
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
end

---ORG_TEXT with all skg headline metadata removed.
---@param org_text string
---@return string
function M.strip_metadata_from_org_text (org_text)
  local lines = {}
  for _, line in ipairs(vim.split(org_text, '\n')) do
    local split = M.split_as_stars_metadata_title(line)
    if split and split.metadata ~= '' then
      table.insert(lines, split.stars .. split.title)
    else
      table.insert(lines, line) end
  end
  return table.concat(lines, '\n')
end

---Write minimal occurrence metadata onto the metadata-less headline
---at point, prompting for an owned skgrepo (no prompt when only one).
---@return string|nil the chosen skgrepo
function M.populate_minimal_node_metadata ()
  local skgrepo = picker.prompt_for_owned_repo()
  if not skgrepo then return nil end
  M.edit_metadata_at_point(
    sexpr.read(string.format('(skg node (skgRepos (nodeRepo %s)))',
                             skgrepo)))
  return skgrepo
end

---Prompt for and change the skgrepo of the node at point (S-arrows
---cycle owned skgrepos; typed names accepted). With RECURSIVE, also
---changes every true content descendant whose skgrepo matches. On
---a metadata-less headline, populates it minimally instead.
---Write-protected occurrences are not changed -- the save would
---refuse their skgrepo edits -- and produce a loud warning, with the
---full ID list in :messages. Does not save.
---@param recursive boolean|nil
function M.set_repo (recursive)
  if M.headline_metadata_empty_p() then
    M.populate_minimal_node_metadata()
    return end
  local metadata = M.current_headline_metadata_sexp()
  local current_skgrepo = M.node_repo(metadata)
  if not current_skgrepo then error('Node has no repo') end
  local new_skgrepo =
    picker.prompt_for_repo_change(current_skgrepo)
  if not new_skgrepo then return end
  new_skgrepo = vim.trim(new_skgrepo)
  if new_skgrepo == '' then return end
  if new_skgrepo:match('%s') then
    error('Repo names cannot contain whitespace') end
  if new_skgrepo == current_skgrepo then
    vim.notify('Repo unchanged: ' .. current_skgrepo)
    return end
  M.change_repo_and_report(current_skgrepo, new_skgrepo, recursive)
end

function M.set_repo_recursive ()
  M.set_repo(true)
end

---The body of set_repo once a real move is requested: retarget the
---skgrepos (skipping write-protected occurrences) and report --
---loudly, when write-protected occurrences were skipped: one
---message per skipped ID, then the summary.
---@param old_skgrepo string
---@param new_skgrepo string
---@param recursive boolean|nil
function M.change_repo_and_report (old_skgrepo, new_skgrepo, recursive)
  local changed_count, writeProtected_gnids
  if recursive then
    changed_count, writeProtected_gnids =
      M.change_repo_recursive(old_skgrepo, new_skgrepo)
  else
    changed_count, writeProtected_gnids =
      M.change_repo_at_line_unless_write_protected(
        M.current_line_number(), new_skgrepo) end
  for _, gnid in ipairs(writeProtected_gnids) do
    vim.notify(
      'SkgSetRepo: write-protected occurrence not changed '
        .. '(the save would refuse its edit): ' .. gnid,
      vim.log.levels.WARN) end
  local summary = string.format(
    'Repo changed from %s to %s on %d node%s. Save to apply.',
    old_skgrepo, new_skgrepo, changed_count,
    changed_count == 1 and '' or 's')
  if #writeProtected_gnids == 0 then
    vim.notify(summary)
  else
    vim.notify(summary .. string.format(
      '  WARNING: %d write-protected node%s not changed -- the save '
        .. 'would refuse their edits. See :messages for the ID list.',
      #writeProtected_gnids,
      #writeProtected_gnids == 1 and '' or 's'),
      vim.log.levels.WARN) end
end

---Set the skgrepo at LINE_NUMBER to NEW_SKGREPO, unless the occurrence
---is write-protected -- the save would refuse that edit.
---@param line_number integer
---@param new_skgrepo string
---@return integer changed node count
---@return string[] the skipped write-protected IDs
function M.change_repo_at_line_unless_write_protected (line_number,
                                                       new_skgrepo)
  local metadata = M.metadata_sexp_at_line_or_nil(line_number)
  if M.node_write_protected_p(metadata) then
    return 0, { M.node_id(metadata) or '(no id)' } end
  return M.change_repo_at_line(line_number, new_skgrepo), {}
end

---Change OLD_SKGREPO to NEW_SKGREPO in this content subtree (root
---inclusive; only member descendants (no noClaim atom) are
---traversed).
---A write-protected occurrence is not edited -- the save would
---refuse its skgrepo edit -- and its ID is returned instead, for the
---caller to warn about, unless the walk also changed that ID's
---editable occurrence: that one carries the move, and the rerender
---after the save shows it everywhere. Its viewdescendants are still
---traversed: they are self-writers, so their skgrepo edits take
---effect even under a write-protected parent.
---@param old_skgrepo string
---@param new_skgrepo string
---@return integer changed node count
---@return string[] the skipped write-protected IDs
function M.change_repo_recursive (old_skgrepo, new_skgrepo)
  local changed_count = 0
  local changed_gnids = {}
  local writeProtected_gnids = {}
  local function change_or_skip (line)
    local metadata = M.metadata_sexp_at_line_or_nil(line)
    if M.node_write_protected_p(metadata) then
      table.insert(writeProtected_gnids,
                   M.node_id(metadata) or '(no id)')
    else
      local gnid = M.node_id(metadata)
      if gnid then changed_gnids[gnid] = true end
      changed_count = changed_count
        + M.change_repo_at_line(line, new_skgrepo) end
  end
  local start_line = M.current_line_number()
  local start_level = M.outline_level(start_line)
  change_or_skip(start_line)
  local line = M.next_headline_line(start_line)
  while line and (M.outline_level(line) or 0) > start_level do
    local metadata = M.metadata_sexp_at_line_or_nil(line)
    if not (M.occurrence_sexp_p(metadata)
            and M.claims_membership_p(metadata)) then
      line = M.next_headline_after_subtree(line)
    else
      if M.node_repo(metadata) == old_skgrepo then
        change_or_skip(line) end
      line = M.next_headline_line(line) end
  end
  return changed_count,
         vim.tbl_filter(function (gnid)
                          return not changed_gnids[gnid] end,
                        writeProtected_gnids)
end

---Set the skgrepo at LINE_NUMBER to NEW_SKGREPO, and mark the headline a
---home skgrepo boundary ('(view repoBoundary)'), so that until the save
---its herald shows the pending move as "⌂:NEW". The server skips
---'(view ...)' when it reads a buffer, and the save redraws the
---headline with the boundary it computes.
---@param line_number integer
---@param new_skgrepo string
---@return integer 1
function M.change_repo_at_line (line_number, new_skgrepo)
  M.edit_metadata_at_line(line_number, sexpr.read(string.format(
    '(skg (skgRepos (ENSURE (nodeRepo %s))) (view) (view (ENSURE repoBoundary)))',
    new_skgrepo)))
  return 1
end

---Mark the node at point to merge ACQUIREE_ID_OR_LINK (a bare id or
---an [[id:ID][label]] link), prompting when not given. Run from the
---acquirer. Does not save.
---@param acquiree_id_or_link string|nil
function M.set_merge_request (acquiree_id_or_link)
  local answer = acquiree_id_or_link
                 or vim.fn.input('Acquiree ID or link: ')
  if answer == nil or answer == '' then
    error('Acquiree ID cannot be empty') end
  local acquiree_gnid = metadata_fields.extract_id_from_text(answer)
  if acquiree_gnid == '' then
    error('Acquiree ID cannot be empty') end
  M.write_edit_request(
    { sexpr.symbol('merge'), sexpr.symbol(acquiree_gnid) }, 'node')
  vim.notify(string.format(
    'Merge request set for acquiree %s. Save to apply.', acquiree_gnid))
end

---@param metadata_sexp any
---@return boolean does its '(diff ...)' say its relationship was
---removed, by a '(rel removed)'?
function M.relationship_removed_p (metadata_sexp)
  local rel = M.sexp_cdr_at_path(metadata_sexp,
                                 { 'skg', 'diff', 'rel' })
  return rel ~= nil and #rel == 1
         and sexpr.atom_text(rel[1]) == 'removed'
end

---@param metadata_sexp any
---@return string|nil the folder's atom if METADATA_SEXP is a folder's
---(a kind 'folder' in 'shared/metadata-fields.json'). A folder is
---always its bare atom.
function M.folder_atom (metadata_sexp)
  if not sexpr.is_list(metadata_sexp) then return nil end
  for i = 2, #metadata_sexp do
    local item = metadata_sexp[i]
    if sexpr.is_symbol(item) then
      local atom = sexpr.atom_text(item)
      local kind = shared.metadata_kind(atom)
      if kind and kind.kind == 'folder' then return atom end
    end
  end
  return nil
end

---The atoms of the property folders, whose members are a node's
---aliases, IDs and flags. Every other folder is a partner folder.
M.property_folder_atoms = {
  aliasFolder = true, idFolder = true, flagsFolder = true }

---@param metadata_sexp any
---@return string|nil the partner folder's atom if METADATA_SEXP is a
---partner folder's (M.folder_atom, but for the property folders)
function M.partner_folder_atom (metadata_sexp)
  local folder = M.folder_atom(metadata_sexp)
  if folder and not M.property_folder_atoms[folder] then
    return folder end
  return nil
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
---editable? In a forks folder, where a forker the viewparent does not
---fork claims nothing too, it must also be a note the viewparent still
---forks (M.forked_by_its_folders_viewparent_p): a detached forkee or
---mutual.
function M.pending_detachment_p (metadata_sexp, parent_sexp,
                                 grandparent_sexp)
  return sexpr.is_list(metadata_sexp)
         and not M.claims_membership_p(metadata_sexp)
         and M.warranted_by_the_graph_p(metadata_sexp)
         and M.reads_claims_p(parent_sexp, grandparent_sexp)
         and (M.folder_atom(parent_sexp) ~= 'forksFolder'
              or M.forked_by_its_folders_viewparent_p(metadata_sexp))
end

---The form at PATH (heads after the metadata's own) in METADATA_SEXP,
---or nil.
---@param metadata_sexp any
---@param path string[]
---@return any|nil
local function form_at (metadata_sexp, path)
  local form = metadata_sexp
  for _, key in ipairs(path) do
    if not sexpr.is_list(form) then return nil end
    form = M.child_with_head(form, key)
  end
  return form
end

---@param metadata_sexp any
---@return boolean does METADATA_SEXP, a member of a folder, name its
---folder's viewparent (ancestor generation 2) among the notes forking
---it: '(view (rels (forks (in N (ancestors ... 2 ...)))))'? Mirrors
---skg--forked-by-its-folders-viewparent-p.
function M.forked_by_its_folders_viewparent_p (metadata_sexp)
  return vim.tbl_contains(
    M.arguments(form_at(metadata_sexp,
                      { 'view', 'rels', 'forks', 'in', 'ancestors' })),
    '2')
end

---@param metadata_sexp any
---@return boolean do METADATA_SEXP's birth facts name '(forks out 2)'
---and no '(forks in 2)': is it a member of a forks folder that forks
---the folder's viewparent, which does not fork it? Mirrors
---skg--born-only-forking-its-folders-viewparent-p.
function M.born_only_forking_its_folders_viewparent_p (metadata_sexp)
  local birth = form_at(metadata_sexp, { 'view', 'birth' })
  local function born_of (side)
    if not birth then return false end
    for i = 2, #birth do
      local fact = birth[i]
      if M.head_name(fact) == 'forks'
         and vim.deep_equal(M.arguments(fact), { side, '2' }) then
        return true end
    end
    return false
  end
  return born_of('out') and not born_of('in')
end

---@param metadata_sexp any
---@return boolean is it warranted by the graph: does its '(view ...)'
---carry neither 'noWarrant' nor 'ghostWarrant', and its '(diff ...)'
---no '(rel removed)'? What HEAD alone warrants is no detachment.
function M.warranted_by_the_graph_p (metadata_sexp)
  local view_atoms = M.arguments(M.child_with_head(metadata_sexp, 'view'))
  return not (vim.tbl_contains(view_atoms, 'noWarrant')
              or vim.tbl_contains(view_atoms, 'ghostWarrant')
              or M.relationship_removed_p(metadata_sexp))
end

---@param sexp any|nil
---@return boolean is it a node that is not write-protected?
local function editable_node_p (sexp)
  return M.occurrence_sexp_p(sexp)
         and not M.node_write_protected_p(sexp)
end

---@param parent_sexp any|nil
---@param grandparent_sexp any|nil
---@return boolean does a parent with PARENT_SEXP, whose own parent has
---GRANDPARENT_SEXP, read its children's claims: a node that is not
---write-protected, or a folder 'shared/herald-styles.json' does not
---list as write-protected whose viewparent is such a node?
function M.reads_claims_p (parent_sexp, grandparent_sexp)
  local folder = M.folder_atom(parent_sexp)
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
  local metadata_sexp = M.metadata_on_line(lines[index])
  if not (metadata_sexp and not M.claims_membership_p(metadata_sexp)) then
    return false end
  local ancestors = M.ancestor_metadata_sexps_in_lines(lines, index, 2)
  return M.pending_detachment_p(metadata_sexp, ancestors[1], ancestors[2])
end

---The metadata of the COUNT nearest ancestor headlines of the
---headline LINES[INDEX], nearest first: element N is nil where the
---Nth ancestor has no metadata or there is none.
---@param lines string[]
---@param index integer
---@param count integer
---@return table
function M.ancestor_metadata_sexps_in_lines (lines, index, count)
  local ancestors = {}
  for generation, ancestor_index
      in ipairs(M.ancestor_headline_indices_in_lines(lines, index, count)) do
    ancestors[generation] =
      M.metadata_on_line(lines[ancestor_index]) end
  return ancestors
end

---The indices in LINES of the COUNT nearest ancestor headlines of the
---headline LINES[INDEX], nearest first; fewer when it has fewer.
---LINES are a buffer's lines, 1-based.
---@param lines string[]
---@param index integer
---@param count integer
---@return integer[]
function M.ancestor_headline_indices_in_lines (lines, index, count)
  local ancestor_indices = {}
  local level = M.headline_level_on_line(lines[index])
  local ancestor_index = index
  for _ = 1, count do
    if not (level and level > 1) then break end
    repeat ancestor_index = ancestor_index - 1
    until ancestor_index < 1
          or (M.headline_level_on_line(lines[ancestor_index]) or level) < level
    if ancestor_index < 1 then break end
    level = M.headline_level_on_line(lines[ancestor_index])
    table.insert(ancestor_indices, ancestor_index)
  end
  return ancestor_indices
end

---@param metadata_sexp any
---@return boolean is it a gnid-only vognode's, one that shows only
---its gnid: an unknown viewnode or a restricted viewnode?
---Either takes a relRepo request, as a relationship member. Mirrors
---skg--gnid-only-vognode-sexp-p.
function M.gnid_only_vognode_sexp_p (metadata_sexp)
  if not sexpr.is_list(metadata_sexp) then return false end
  for _, head in ipairs({ 'unknown', 'restricted' }) do
    if compare.subtree_p(metadata_sexp,
                         { SKG, sexpr.symbol(head) }) then
      return true end
  end
  return false
end

---@param metadata_sexp any
---@return boolean is it a vognode's: an occurrence, a gnid-only
---vognode or a ghost?
function M.vognode_sexp_p (metadata_sexp)
  if not sexpr.is_list(metadata_sexp) then return false end
  return M.occurrence_sexp_p(metadata_sexp)
    or M.gnid_only_vognode_sexp_p(metadata_sexp)
    or compare.subtree_p(metadata_sexp, { SKG, sexpr.symbol('ghost') })
end

---@param line_number integer
---@return integer|nil the line of the headline's viewparent
function M.parent_line (line_number)
  local level = M.outline_level(line_number)
  if not level then return nil end
  return M.parent_headline_line(line_number, level)
end

---HOST_SEXP with the 'fork' symbol removed from its
---(viewRequests ...) form, dropping that form entirely if it becomes
---empty. The result equals HOST_SEXP when there was no fork request.
---@param host_sexp any
---@return any
function M.remove_fork_from_viewrequests (host_sexp)
  local FORK = sexpr.symbol('fork')
  local VIEWREQUESTS = sexpr.symbol('viewRequests')
  local result = { host_sexp[1] }
  for i = 2, #host_sexp do
    local element = host_sexp[i]
    if sexpr.is_list(element) and element[1] == VIEWREQUESTS then
      local kept = { VIEWREQUESTS }
      for k = 2, #element do
        if element[k] ~= FORK then
          table.insert(kept, element[k]) end
      end
      if #kept > 1 then table.insert(result, kept) end
    else
      table.insert(result, element) end
  end
  return result
end

---Remove every fork viewRequest from the buffer's headlines. Used on
---fork-decline so a lingering explicit-fork atom does not silently
---re-fork on the next save.
function M.strip_fork_requests_in_buffer ()
  local last = vim.api.nvim_buf_line_count(0)
  for line_number = 1, last do
    if M.at_headline_p(line_number) then
      local split = M.split_as_stars_metadata_title(
        M.line_text(line_number))
      if split and split.metadata ~= '' then
        local ok, sexp = pcall(sexpr.read, split.metadata)
        if ok then
          local stripped = M.remove_fork_from_viewrequests(sexp)
          if not compare.values_equal(stripped, sexp) then
            M.replace_line(line_number, M.format_headline(
              split.stars, sexpr.to_string(stripped), split.title))
          end
        end
      end
    end
  end
end

---Toggle the cursor between the start of the line and the start of
---the title (the text after stars and metadata). On a non-headline,
---just move to the beginning of the line.
function M.beginning_of_line ()
  local split = M.at_headline_p()
    and M.split_as_stars_metadata_title(M.get_current_headline_text())
    or nil
  if split then
    local line = M.line_text()
    local prefix_length = #split.stars + #split.metadata
    local rest = line:sub(prefix_length + 1)
    local spaces = #(rest:match('^[ \t]*') or '')
    local title_col = prefix_length + spaces -- 0-based byte column
    local current_col = vim.api.nvim_win_get_cursor(0)[2]
    if current_col ~= title_col then
      vim.api.nvim_win_set_cursor(0,
        { M.current_line_number(), title_col })
      return end
  end
  vim.api.nvim_win_set_cursor(0, { M.current_line_number(), 0 })
end

---Cycle the TODO keyword of the headline at point without corrupting
---its metadata: strip the (skg ...) metadata so the org plugin sees a
---plain headline, cycle, then re-insert the metadata after the stars
---(before the new keyword). The analog of the elisp org-todo advice.
---@param direction integer 1 or -1
function M.todo_cycle (direction)
  if not M.at_headline_p() then return end
  local line_number = M.current_line_number()
  local split = M.split_as_stars_metadata_title(
    M.line_text(line_number))
  local has_metadata = split and split.metadata ~= ''
  if has_metadata then
    M.replace_line(line_number, split.stars .. split.title)
  end
  local ok, err = pcall(function ()
    local orgmode = require('orgmode')
    orgmode.action(direction >= 0 and 'org_mappings.todo_next_state'
                   or 'org_mappings.todo_prev_state')
  end)
  if has_metadata then
    local new_line = M.line_text(line_number)
    local stars, rest = new_line:match('^(%*+%s+)(.*)$')
    if stars then
      M.replace_line(line_number,
        stars .. split.metadata .. ' ' .. rest)
    end
  end
  if not ok then
    vim.notify('skg: TODO cycling unavailable: ' .. tostring(err))
  end
end

return M
