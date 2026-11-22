-- PURPOSE: Choose the skgrepo that records the relationship at point
-- (its relRepo). 'set_relRepo' (':SkgSetRelRepo', <localleader>sr)
-- asks the server's 'relRepo info' endpoint
-- ('docs/api-and-formats.org', "relRepo info") for the relationship's
-- default and current relRepo, then prompts over the skgrepos at
-- least as private as the default, writing
-- '(editRequest (relRepo REPO))' on the headline.
-- 'set_relRepo_recursive' (':SkgSetRelRepoRecursive', sR) asks for a
-- relationship kind and a skgrepo and writes that request on each
-- relationship of the kind in the subtree. Neither saves. Below them
-- stand the helpers both drive. Writing the request into the
-- metadata is skg.metadata's ('write_edit_request_at_line').
-- The Lua port of elisp/skg-request-relRepo-info.el and of the
-- relRepo section of elisp/skg-metadata.el.

local client = require('skg.client')
local config = require('skg.config')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local shared = require('skg.shared')
local state = require('skg.state')

local M = {}

local namespace = vim.api.nvim_create_namespace('skg-relRepo-request')

---Set the relRepo of the relationship or alias at point. With
---RECURSIVE, run M.set_relRepo_recursive instead, which asks
---for a relationship kind and a repo and applies the repo throughout
---the subtree at point.
---
---The headline at point represents one relationship: 'contains' for a
---content child, the folder's relation for a writable partner folder's
---member. This asks the server for the relationship's default and
---current repos (sending its recorder, member and relation), then
---prompts, with TAB completion and S-Left/S-Right cycling, over the
---repos at least as private as the default (more public ones could
---leak an endpoint's ID and would be refused at save), plus a
---no-override choice. The prompt starts with a pending request when
---one is offered, else the current relRepo or the default.
---
---Choosing a repo writes '(editRequest (relRepo REPO))'; the
---no-override choice removes that request, so that on save the
---relationship keeps its saved repo (sticky). Refuses on a
---write-protected folder's member and on a root headline. Edits only
---the buffer. Mirrors skg-set-relRepo.
---@param recursive boolean|nil
function M.set_relRepo (recursive)
  if recursive then return M.set_relRepo_recursive() end
  M.set_relRepo_at_line(metadata.current_line_number())
end

---The single-relationship path of M.set_relRepo: classify the
---relationship at LINE_NUMBER, ask the server for its default and
---current repos, and prompt from the reply. An alias's default is its
---owning node's home, read from the buffer with no request. Mirrors
---skg--set-relRepo-at-point.
---@param line_number integer
function M.set_relRepo_at_line (line_number)
  local buf = vim.api.nvim_get_current_buf()
  if M.alias_headline_p(line_number) then
    local folder = metadata.parent_line(line_number)
    local owner = folder and metadata.parent_line(folder)
    if not owner then error('Alias has no owning node headline', 0) end
    local default = metadata.node_repo(
      metadata.metadata_sexp_at_line_or_nil(owner))
    if not default then error('Node has no repo', 0) end
    local current = M.relRepo_current_value(line_number)
    local mark = vim.api.nvim_buf_set_extmark(
      buf, namespace, line_number - 1, 0, {})
    M.set_relRepo_from_info(buf, mark, sexpr.read(string.format(
      '((response-type relRepo-info) (default %q)%s)', default,
      current and string.format(' (current %q)', current) or '')))
    return end
  local relationship = M.rel_at_line(line_number)
  local mark = vim.api.nvim_buf_set_extmark(
    buf, namespace, line_number - 1, 0, {})
  state.register_response_handler('relRepo-info',
    function (_payload_text, response)
      M.set_relRepo_from_info(buf, mark, response) end, true)
  state.lp_reset()
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'relRepo info'),
    sexpr.pair(sexpr.symbol('recorder'), relationship.recorder),
    sexpr.pair(sexpr.symbol('member'), relationship.member),
    sexpr.pair(sexpr.symbol('relation'), relationship.relation) })
    .. '\n')
end

---Handle RESPONSE, the parsed relRepo-info reply, for M.set_relRepo:
---prompt and apply the choice at the headline MARK (an extmark) holds
---in BUF. An error reply offers every repo, the save validating.
---Mirrors skg--set-relRepo-from-info.
---@param buf integer
---@param mark integer
---@param response any
function M.set_relRepo_from_info (buf, mark, response)
  local err = payload.field_text(response, 'error')
  local default = payload.field_text(response, 'default')
  local current = payload.field_text(response, 'current')
  if not vim.api.nvim_buf_is_valid(buf) then
    vim.notify('skg: buffer vanished before the relRepo prompt')
    return end
  local position = vim.api.nvim_buf_get_extmark_by_id(
    buf, namespace, mark, {})
  pcall(vim.api.nvim_buf_del_extmark, buf, namespace, mark)
  if #position == 0 then
    vim.notify('skg: headline vanished before the relRepo prompt')
    return end
  vim.api.nvim_buf_call(buf, function ()
    local line = position[1] + 1
    if err then
      vim.notify(string.format(
        'relRepo info: %s -- offering every repo; the save will validate.',
        err)) end
    local requested = M.relRepo_requested_value(line)
    local choices = M.relRepo_choices(config.repo_names() or {},
                                             default)
    table.insert(choices, M.relRepo_no_override)
    local prompt = 'relRepo (S-left/right cycle'
      .. (default and ('; default ' .. default) or '')
      .. (current and ('; currently ' .. current) or '') .. '): '
    -- Start from the status quo, so that RET keeps it. A current
    -- relRepo more public than the default is not among the choices;
    -- then the default, then nothing.
    local prefill = nil
    for _, candidate in ipairs({ requested or false, current or false,
                                 default or false }) do
      if candidate and vim.tbl_contains(choices, candidate) then
        prefill = candidate
        break end
    end
    local choice = picker.completing_read_with_cycle(prompt, choices, {
      initial_input = prefill, cycle_values = choices,
      require_match = true })
    if not choice then return end
    local ok, message = pcall(M.apply_relRepo_choice, line, choice)
    vim.notify(tostring(message),
               ok and vim.log.levels.INFO or vim.log.levels.ERROR)
  end)
end

-- ── the relRepo gesture ────────────────────────────────────────────
-- The helpers of 'relRepo_info.set_relRepo' (Emacs 'skg-set-relRepo',
-- C-c s r) and the recursive gesture 'set_relRepo_recursive' (Emacs
-- 'skg-set-relRepo-recursive', C-c s R). Each reads the headline at a
-- line where its Emacs twin reads the headline at point.

---The partner folder atoms where an explicit relRepo request is
---unsupported from this side: the relationship belongs to the other
---end. The membership of hiddenOutsideOfForkPartnersFolder, a compound
---partner folder, is editable, but hide repos are still derived and
---cannot carry this request. Mirrors
---skg--relRepo-unsupported-folder-atoms.
M.relRepo_unsupported_folder_atoms = {
  'hiderFolder', 'hiddenFolder',
  'containerFolder', 'mentionerFolder', 'acquiredByFolder',
  'acquiredFolder', 'illegalEditFolder', 'integratedElsewhereFolder',
  'hiddenInForkPartnerFolder', 'hiddenByForkerFolder',
  'hiddenOutsideOfForkPartnersFolder' }

---The partner folder atoms whose members' relationships are writable
---from this side, each with its relation's wire name. The folder's
---viewparent records the relationship to each member that claims; a
---member of the forks folder claiming nothing either forks the
---viewparent, recording that in its own file, or is a mirror of it
---only through others, recorded nowhere between them, so no relRepo is
---set on it from here. Mirrors skg--writable-folder-relations.
M.writable_folder_relations = {
  { atom = 'forksFolder', relation = 'forks' } }

---The menu entry that removes the pending '(editRequest (relRepo
---...))' request instead of setting one. For a relationship already on
---disk this means the saved repo survives (sticky); it does not mean
---"reset to the default". Mirrors skg--relRepo-no-override.
M.relRepo_no_override = '(no override: follow sticky-else-default)'

---@param metadata_sexp any|nil
---@param atom string
---@return boolean is METADATA_SEXP a '(skg ...)' carrying the bare ATOM?
function M.non_vognode_atom_present_p (metadata_sexp, atom)
  if not sexpr.is_list(metadata_sexp) then return false end
  for i = 2, #metadata_sexp do
    local item = metadata_sexp[i]
    if not sexpr.is_list(item) and sexpr.is_symbol(item)
       and sexpr.atom_text(item) == atom then
      return true end
  end
  return false
end

---The entry of M.writable_folder_relations whose atom METADATA_SEXP
---carries, or nil.
---@param metadata_sexp any|nil
---@return table|nil
local function writable_folder_relation (metadata_sexp)
  for _, entry in ipairs(M.writable_folder_relations) do
    if M.non_vognode_atom_present_p(metadata_sexp, entry.atom) then
      return entry end
  end
  return nil
end

---@param metadata_sexp any|nil
---@return boolean is it a writable folder's metadata? Mirrors
---skg--writable-folder-sexp-p.
function M.writable_folder_sexp_p (metadata_sexp)
  return writable_folder_relation(metadata_sexp) ~= nil
end

---@param metadata_sexp any|nil
---@return string|nil the raw member ID of an occurrence, an unknown
---viewnode or a restricted viewnode. Mirrors
---skg--relationship-member-id.
function M.relationship_member_id (metadata_sexp)
  if not (metadata.occurrence_sexp_p(metadata_sexp)
          or metadata.gnid_only_vognode_sexp_p(metadata_sexp)) then
    return nil end
  return metadata.metadata_gnid(metadata_sexp)
end

---Classify the relationship the headline at LINE_NUMBER represents:
---{ recorder, member, relation }. For a content child, the viewparent
---contains the headline's node; for a writable folder's member, the
---folder's viewparent records the folder's relation toward
---it. Errors where the headline represents no writable relationship:
---off an occurrence, an unknown viewnode or a restricted viewnode,
---on a root headline, under a write-protected folder,
---parent or folder's viewparent, or with an ID missing. Mirrors
---skg--rel-at-point.
---@param line_number integer
---@return table
function M.rel_at_line (line_number)
  if not metadata.at_headline_p(line_number) then
    error('Not on a headline', 0) end
  local member_sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not (metadata.occurrence_sexp_p(member_sexp)
          or metadata.gnid_only_vognode_sexp_p(member_sexp)) then
    error('Not on an occurrence, an unknown viewnode or a restricted'
          .. ' viewnode', 0) end
  local member_gnid = M.relationship_member_id(member_sexp)
  local parent = metadata.parent_line(line_number)
  local parent_sexp = parent and metadata.metadata_sexp_at_line_or_nil(parent)
  if not member_gnid then
    error("No id in this headline's metadata", 0) end
  if not parent_sexp then
    error('Root headline: there is no relationship here to set', 0) end
  for _, atom in ipairs(M.relRepo_unsupported_folder_atoms) do
    if M.non_vognode_atom_present_p(parent_sexp, atom) then
      error(string.format(
        "Cannot set the relationship's repo from this write-protected"
          .. ' %s position', atom), 0) end
  end
  local writable_folder = writable_folder_relation(parent_sexp)
  if writable_folder and not metadata.claims_membership_p(member_sexp) then
    error(string.format(
      'This member of the %s claims nothing: either it forks the'
        .. ' view-parent and records that in its own file, or it is a'
        .. ' mirror of the view-parent only through others; attach it'
        .. ' first to make the view-parent fork it', writable_folder.atom), 0) end
  if writable_folder then
    local folder_viewparent = metadata.parent_line(parent)
    local folder_viewparent_sexp = folder_viewparent
      and metadata.metadata_sexp_at_line_or_nil(folder_viewparent)
    local folder_viewparent_gnid = metadata.node_id(folder_viewparent_sexp)
    if not folder_viewparent_gnid then
      error("Could not find the folder's viewparent", 0) end
    if metadata.node_write_protected_p(folder_viewparent_sexp) then
      error("The folder's viewparent is write-protected here, so the save"
            .. ' would not write this relationship; set it from a view'
            .. " where the folder's viewparent is editable", 0) end
    return { recorder = folder_viewparent_gnid, member = member_gnid,
             relation = writable_folder.relation }
  end
  if metadata.occurrence_sexp_p(parent_sexp) then
    local parent_gnid = metadata.node_id(parent_sexp)
    if not parent_gnid then
      error("No id in the parent headline's metadata", 0) end
    if metadata.node_write_protected_p(parent_sexp) then
      error('The parent is write-protected here, so the save would not'
            .. ' write this relationship; set it from a view where the'
            .. ' parent is editable', 0) end
    return { recorder = parent_gnid, member = member_gnid,
             relation = 'contains' }
  end
  error('The parent headline is neither a node nor a writable folder', 0)
end

---The first value at PATH in the metadata of the headline at
---LINE_NUMBER, as text, or nil.
---@param line_number integer
---@param path string[]
---@return string|nil
local function metadata_value_at_line (line_number, path)
  return metadata.first_value_at_path(
    metadata.metadata_sexp_at_line_or_nil(line_number) or { sexpr.symbol('skg') }, path)
end

---@param line_number integer
---@return string|nil the displayed '(skgRepos (relRepo NAME))' fact.
---Mirrors skg--relRepo-current-value.
function M.relRepo_current_value (line_number)
  return metadata_value_at_line(line_number,
                                { 'skg', 'skgRepos', 'relRepo' })
end

---@param line_number integer
---@return string|nil the pending '(editRequest (relRepo NAME))' value.
---Mirrors skg--relRepo-requested-value.
function M.relRepo_requested_value (line_number)
  return metadata_value_at_line(line_number,
                                { 'skg', 'editRequest', 'relRepo' })
end

---@param line_number integer
---@return boolean does the headline hold an edit request
---(metadata.node_edit_request_p)? Mirrors skg--node-edit-request-at-point-p.
function M.node_edit_request_at_line_p (line_number)
  return metadata.node_edit_request_p(metadata.metadata_sexp_at_line_or_nil(line_number))
end

---@param line_number integer
---@return boolean is the headline an alias? Mirrors skg--alias-headline-p.
function M.alias_headline_p (line_number)
  return metadata.at_headline_p(line_number)
         and M.non_vognode_atom_present_p(
               metadata.metadata_sexp_at_line_or_nil(line_number), 'alias')
end

---The repo-name menu of the relRepo gesture: the tail of LADDER (the
---configured repos, most public first) starting at DEFAULT, exactly
---the repos the save's default floor can accept; the whole LADDER when
---DEFAULT is nil or not in it (the save's floor check backstops a stale
---offer). Mirrors skg--relRepo-choices.
---@param ladder string[]
---@param default string|nil
---@return string[]
function M.relRepo_choices (ladder, default)
  for index, name in ipairs(ladder) do
    if default and name == default then
      return { unpack(ladder, index) } end
  end
  return { unpack(ladder) }
end

---Apply CHOICE, a repo name or M.relRepo_no_override, to the headline
---at LINE_NUMBER. Edits only the buffer. Errors where a delete or a
---nodeMerge request is pending. Mirrors skg--apply-relRepo-choice.
---@param line_number integer
---@param choice string
---@return string what the next save will do with the relationship
function M.apply_relRepo_choice (line_number, choice)
  if M.node_edit_request_at_line_p(line_number) then
    error('Cannot request a relRepo where a delete, merge or flag request is pending', 0) end
  if choice == M.relRepo_no_override then
    if not M.relRepo_requested_value(line_number) then
      return 'No override present; nothing to remove.' end
    -- The displayed relRepo stays under skgRepos; only the request goes.
    metadata.write_edit_request_at_line(line_number, nil)
    return 'Override removed: on save the membership keeps its saved'
           .. ' (sticky) relRepo, or its default if new. Save to apply.'
  end
  local request = { sexpr.symbol('relRepo'), sexpr.symbol(choice) }
  if M.alias_headline_p(line_number)
     or metadata.gnid_only_vognode_sexp_p(
          metadata.metadata_sexp_at_line_or_nil(line_number)) then
    metadata.write_edit_request_at_line(line_number, request)
  else
    metadata.write_edit_request_at_line(line_number, request, 'node') end
  return string.format("relRepo set to '%s'. Save to apply.", choice)
end

---For each relation role in 'shared/relations.json', as the
---relationship-kind menu of M.set_relRepo_recursive shows it: the role
---the view-child would play toward its view-parent; the kind the walk
---dispatches on ('content' or 'forkee') for the two roles whose
---relationship is writable from the child's position, else
---false; and a description, which says why when it is not. Mirrors
---skg--relationship-role-menu-prose.
M.relationship_role_menu_prose = {
  container = { kind = false, description =
    "The node would contain its view-parent -- the shape of a node of the containerward chain. The relationship belongs to that node's own contains list, wherever that list is shown editable; it cannot be set from the chain node's position." },
  content = { kind = 'content', description =
    'The view-parent contains the node: ordinary content. Sets the repo of each parent-contains-child relationship.' },
  mentioner = { kind = false, description =
    'Links are inferred from body text; they carry no false relRepo, so there is nothing to set.' },
  mentioned = { kind = false, description =
    'Links are inferred from body text; they carry no false relRepo, so there is nothing to set.' },
  forker = { kind = false, description =
    "A member of the view-parent's forksFolder that claims nothing: either it forks the view-parent, and the forks relationship belongs to the member (the forker), recorded in its own file, or it is a mirror of the view-parent only through others. Refused from here." },
  forkee = { kind = 'forkee', description =
    "A member of the view-parent's forksFolder that claims: the view-parent forks it. Sets the repo of each forks relationship from the view-parent." },
  hider = { kind = false, description =
    'Hide repos are derived at save, floored at the most public forks relationship explaining them; the hiderFolder is write-protected.' },
  hidden = { kind = false, description =
    'Hide repos are derived at save, floored at the most public forks relationship explaining them; the hiddenFolder is write-protected.' } }

---The relationship-kind menu: one entry { relation, roles } per
---relation in 'shared/relations.json', each role { role, kind,
---description } (M.relationship_role_menu_prose). Mirrors
---skg--relationship-kind-menu-tree.
---@return table[]
function M.relationship_kind_menu_tree ()
  local tree = {}
  for _, relation in ipairs(shared.relations) do
    local roles = {}
    for _, role in ipairs(relation.roles) do
      local prose = M.relationship_role_menu_prose[role]
      if not prose then
        error('No menu prose for relation role ' .. role) end
      table.insert(roles, { role = role, kind = prose.kind,
                            description = prose.description })
    end
    table.insert(tree, { relation = relation.name, roles = roles })
  end
  return tree
end

local relRepo_namespace =
  vim.api.nvim_create_namespace('skg-relRepo-recursive')

---Set the relRepo of every matching relationship in the subtree at
---point. First a menu of the relation roles (M.select_relationship_kind)
---asks which role the view-children should play toward their
---view-parents; only 'content' and 'forkee' are
---settable from the child's position. Then a prompt over the whole
---ladder, plus the no-override choice, which removes the pending
---requests instead. No per-relationship default is fetched: the
---subtree's relationships have different defaults, so the save's
---floor check validates each one. The walk is
---M.set_relRepo_recursive_walk. Does not save. Mirrors
---skg-set-relRepo-recursive.
function M.set_relRepo_recursive ()
  if not metadata.at_headline_p() then error('Not on a headline', 0) end
  local buf = vim.api.nvim_get_current_buf()
  local mark = vim.api.nvim_buf_set_extmark(
    buf, relRepo_namespace, metadata.current_line_number() - 1, 0, {})
  M.select_relationship_kind(function (kind)
    if not vim.api.nvim_buf_is_valid(buf) then
      vim.notify('skg: buffer vanished before the relRepo prompt')
      return end
    local position = vim.api.nvim_buf_get_extmark_by_id(
      buf, relRepo_namespace, mark, {})
    pcall(vim.api.nvim_buf_del_extmark, buf, relRepo_namespace, mark)
    vim.api.nvim_buf_call(buf, function ()
      local choices = vim.list_extend(
        { unpack(require('skg.config').repo_names() or {}) },
        { M.relRepo_no_override })
      local choice = picker.completing_read_with_cycle(string.format(
        "Repo for every '%s' relationship in the subtree (S-left/right"
          .. " cycle; the save validates each relationship's floor): ",
        kind), choices, { require_match = true, cycle_values = choices })
      if not choice then return end
      local ok, count = pcall(M.set_relRepo_recursive_walk,
                              position[1] + 1, kind, choice)
      if not ok then
        vim.notify(tostring(count), vim.log.levels.ERROR)
        return end
      local plural = count == 1 and '' or 's'
      if choice == M.relRepo_no_override then
        vim.notify(string.format(
          "Override removed on %d '%s' relationship%s: on save each"
            .. ' keeps its saved (sticky) repo, or its default if new.'
            .. ' Save to apply.', count, kind, plural))
      else
        vim.notify(string.format(
          "relRepo set to '%s' on %d '%s' relationship%s. Save to apply.",
          choice, count, kind, plural)) end
    end)
  end)
end

---Ask, with vim.ui.select, which relation role the view-children
---should play toward their view-parents; call CONTINUATION with the
---chosen role's kind (M.relationship_kind_menu_choose). Mirrors
---skg--select-relationship-kind, whose menu is an org buffer.
---@param continuation fun(kind: string)
function M.select_relationship_kind (continuation)
  local items = {}
  for _, relation in ipairs(M.relationship_kind_menu_tree()) do
    for _, role in ipairs(relation.roles) do
      table.insert(items, { relation = relation.relation, role = role.role,
                            kind = role.kind,
                            description = role.description }) end
  end
  vim.ui.select(items, {
    prompt = 'What kind of relationships to the view-parent should'
             .. ' qualify? (the role the view-child would play)',
    format_item = function (item)
      return string.format('%s: %s%s', item.relation, item.role,
                           item.kind and '' or ' (not settable here)')
    end },
    function (item) M.relationship_kind_menu_choose(item, continuation) end)
end

---Act on ITEM, the menu entry chosen (nil when the menu was left):
---call CONTINUATION with a settable role's kind, or explain why the
---role cannot be set from the child's position. Mirrors
---skg--relationship-kind-menu-choose.
---@param item table|nil
---@param continuation fun(kind: string)
function M.relationship_kind_menu_choose (item, continuation)
  if not item then return end
  if not item.kind then
    vim.notify("Cannot set that kind of relationship from the child's"
               .. ' position: ' .. item.description, vim.log.levels.ERROR)
    return end
  continuation(item.kind)
end

---Apply CHOICE (a repo name, or M.relRepo_no_override) to every
---headline of the subtree at LINE_NUMBER, that headline included, whose
---relationship to its view-parent is of KIND
---(M.relationship_kind_matches_p), as M.relRepo_recursive_targets finds
---them. Errors before any write when a target holds a delete or a
---nodeMerge request. Mirrors skg--set-relRepo-recursive-walk.
---@param line_number integer
---@param kind string
---@param choice string
---@return integer the number of relationships set
function M.set_relRepo_recursive_walk (line_number, kind, choice)
  local targets = M.relRepo_recursive_targets(line_number, kind)
  -- Do not let a late conflict leave earlier targets edited.
  for _, target in ipairs(targets) do
    if M.node_edit_request_at_line_p(target) then
      error('Cannot request a relRepo where a delete, merge or flag request is pending', 0)
    end
  end
  for _, target in ipairs(targets) do
    M.apply_relRepo_choice(target, choice) end
  return #targets
end

---The lines of the relationships of KIND in the subtree at
---LINE_NUMBER, that headline included. Recurses only where edits still
---affect the graph: below an occurrence that claims membership and is
---neither write-protected nor a member as such of a forks folder, and below a
---writable folder; a noClaim occurrence, a write-protected folder and
---every other non-vognode are pruned with their subtrees. Mirrors
---skg--relRepo-recursive-targets.
---@param line_number integer
---@param kind string
---@return integer[]
function M.relRepo_recursive_targets (line_number, kind)
  local targets = {}
  local start_level = metadata.outline_level(line_number)
  local root_meta = metadata.metadata_sexp_at_line_or_nil(line_number)
  if M.relationship_kind_matches_p(line_number, kind) then
    table.insert(targets, line_number) end
  if not ((metadata.occurrence_sexp_p(root_meta)
           and not M.relRepo_prune_below_p(line_number, root_meta))
          or M.writable_folder_sexp_p(root_meta)) then
    return targets end
  local line = metadata.next_headline_line(line_number)
  while line and (metadata.outline_level(line) or 0) > start_level do
    local meta = metadata.metadata_sexp_at_line_or_nil(line)
    if metadata.occurrence_sexp_p(meta) then
      if not metadata.claims_membership_p(meta) then
        line = metadata.next_headline_after_subtree(line)
      else
        if M.relationship_kind_matches_p(line, kind) then
          table.insert(targets, line) end
        if M.relRepo_prune_below_p(line, meta) then
          line = metadata.next_headline_after_subtree(line)
        else
          line = metadata.next_headline_line(line) end
      end
    elseif metadata.gnid_only_vognode_sexp_p(meta) then
      if M.relationship_kind_matches_p(line, kind) then
        table.insert(targets, line) end
      line = metadata.next_headline_line(line)
    elseif M.writable_folder_sexp_p(meta) then
      line = metadata.next_headline_line(line)
    else -- write-protected folders, alias and ID folders, ghosts
      line = metadata.next_headline_after_subtree(line) end
  end
  return targets
end

---Is the headline at LINE_NUMBER an occurrence claiming membership,
---an unknown viewnode or a restricted viewnode, whose
---relationship to its view-parent is of KIND and is collected at save from this position?
---For 'content', the view-parent is an editable occurrence that is no
---member as such of a forks folder; for 'forkee', it is the writable
---forksFolder with an editable viewparent, and the headline claims (a
---member claiming nothing forks the viewparent, its record its own,
---or is a mirror of it only through others). Mirrors
---skg--relationship-kind-matches-p.
---@param line_number integer
---@param kind string
---@return boolean
function M.relationship_kind_matches_p (line_number, kind)
  local meta = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not ((metadata.occurrence_sexp_p(meta) and metadata.claims_membership_p(meta))
          or metadata.gnid_only_vognode_sexp_p(meta)) then
    return false end
  local parent = metadata.parent_line(line_number)
  if not parent then return false end
  local parent_sexp = metadata.metadata_sexp_at_line_or_nil(parent)
  if metadata.occurrence_sexp_p(parent_sexp) then
    return kind == 'content'
           and not metadata.node_write_protected_p(parent_sexp)
           and not M.member_as_such_at_line_p(parent) end
  if M.non_vognode_atom_present_p(parent_sexp, 'forksFolder') then
    return kind == 'forkee' and M.folder_viewparent_editable_p(parent) end
  return false
end

---Should the walk not descend below the occurrence at LINE_NUMBER,
---whose metadata is METADATA_SEXP? A write-protected node's contains
---is not collected at save, and a member as such's
---viewchildren are hide and unhide signals, not writable relationships.
---Mirrors skg--relRepo-prune-below-p.
---@param line_number integer
---@param metadata_sexp any
---@return boolean
function M.relRepo_prune_below_p (line_number, metadata_sexp)
  return metadata.node_write_protected_p(metadata_sexp)
         or M.member_as_such_at_line_p(line_number)
end

---@param line_number integer
---@return boolean is the headline a member as such of a forksFolder:
---an occurrence member of one, a forkee as such if it claims, a forker
---as such if it claims nothing? Either shows its content in part.
---Mirrors skg--member-as-such-at-point-p.
function M.member_as_such_at_line_p (line_number)
  local meta = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not metadata.occurrence_sexp_p(meta) then
    return false end
  local parent = metadata.parent_line(line_number)
  return parent ~= nil and M.non_vognode_atom_present_p(
    metadata.metadata_sexp_at_line_or_nil(parent), 'forksFolder')
end

---@param line_number integer
---@return boolean has the folder headline at LINE_NUMBER an editable
---occurrence for its viewparent? A write-protected viewparent's
---writable folders are not collected at save. Mirrors
---skg--folder-viewparent-editable-p.
function M.folder_viewparent_editable_p (line_number)
  local viewparent = metadata.parent_line(line_number)
  local viewparent_sexp = viewparent
    and metadata.metadata_sexp_at_line_or_nil(viewparent)
  return metadata.occurrence_sexp_p(viewparent_sexp)
         and not metadata.node_write_protected_p(viewparent_sexp)
end

return M
