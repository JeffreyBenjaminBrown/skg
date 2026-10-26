-- PURPOSE: Edit a headline's metadata sexp as an org tree in a
-- temporary buffer: ':w' (or <localleader>cc) applies the edited tree
-- back into the source headline; killing the buffer cancels;
-- S-left/S-right cycle field values, as 'shared/metadata-fields.json'
-- says for the headline's kind (skg.sexpr.metadata_fields). Also asks
-- for a skgrepo after org's insert-heading actions when the new
-- headline cannot inherit one. The Lua port of elisp/skg-sexpr-edit.el
-- and skg-sexpr-edit/skg-sexpr-cycling.el.
--
-- DEVIATION: the elisp made the display-only title group read-only
-- with text properties; nvim has no per-range read-only text, so
-- title edits are simply discarded at apply (the strip step drops
-- the title group whatever it holds).

local bijection = require('skg.sexpr.org_bijection')
local config = require('skg.config')
local fields = require('skg.sexpr.metadata_fields')
local metadata = require('skg.metadata')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local shared = require('skg.shared')

local M = {}

M.help_text =
  ':w (or <localleader>cc) saves, :bd! cancels,'
  .. ' S-left/S-right cycle values.'

---Edit the metadata sexp on the current headline. With no metadata,
---populates a minimal (skg node (skgRepos (nodeRepo X))) in place and
---opens the
---empty-node view over it: skgrepo pre-filled, the other editable
---fields childless, so an untouched apply yields just the skgrepo.
function M.edit_metadata ()
  if not metadata.at_headline_p() then error('Not on a headline') end
  local source_buf = vim.api.nvim_get_current_buf()
  local line_number = metadata.current_line_number()
  local split = metadata.split_as_stars_metadata_title(
    metadata.get_current_headline_text())
  if not split then error('Not on a headline') end
  if split.metadata == '' then
    M.open_empty_node_view(source_buf, line_number, split)
    return end
  local sexp = sexpr.read(split.metadata)
  local kind = fields.kind_of_sexp(sexp)
  local org_text = bijection.sexp_to_org(sexp)
  if kind then
    org_text = fields.fillIn_defaults_in_org(
      org_text, kind, nil,
      metadata.occurrence_sexp_p(sexp) and split.title or nil) end
  M.open_edit_buffer(org_text, source_buf, line_number,
                     #split.stars, #split.metadata, kind)
  M.goto_field_value('nodeRepo')
end

---Populate minimal metadata on the metadata-less headline, then open
---its metadata view with the chosen skgrepo pre-filled and every other
---editable field childless.
---@param source_buf integer
---@param line_number integer
---@param split table
function M.open_empty_node_view (source_buf, line_number, split)
  local skgrepo = metadata.populate_minimal_node_metadata()
  if not skgrepo then return end
  local new_split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line_number))
  M.open_edit_buffer(
    M.empty_node_org_text(skgrepo, split.title),
    source_buf, line_number,
    #new_split.stars, #new_split.metadata,
    shared.metadata_kind('node'))
  M.goto_field_value('nodeRepo')
end

---Org text for the empty-node metadata view: REPO pre-filled;
---TITLE, if non-blank, under a display-only title group; every other
---editable field childless, so the strip step drops the ones the user
---never populates.
---@param skgrepo string
---@param title string
---@return string
function M.empty_node_org_text (skgrepo, title)
  local headlines = {}
  table.insert(headlines, { level = 1, text = 'title' })
  if title and not title:match('^%s*$') then
    table.insert(headlines, { level = 2, text = title }) end
  table.insert(headlines, { level = 1, text = 'skg' })
  table.insert(headlines, { level = 2, text = 'node' })
  table.insert(headlines, { level = 2, text = 'skgRepos' })
  table.insert(headlines, { level = 3, text = 'nodeRepo' })
  table.insert(headlines, { level = 4, text = skgrepo })
  for _, name in ipairs(fields.field_names(shared.metadata_kind('node'))) do
    table.insert(headlines, { level = 2, text = name })
  end
  return fields.headlines_to_org(headlines)
end

---Open the edit buffer over ORG_TEXT, remembering where the metadata
---lives in the source buffer (line + byte range) so apply can splice it.
---@param org_text string
---@param source_buf integer
---@param line_number integer
---@param metadata_start integer 0-based byte column
---@param metadata_length integer
---@param kind table|nil the headline's kind, from 'shared/metadata-fields.json'
function M.open_edit_buffer (org_text, source_buf, line_number,
                             metadata_start, metadata_length, kind)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(
    buf, 'skg://metadata-edit/' .. tostring(buf))
  local lines = { M.help_text, '' }
  for _, line in ipairs(vim.split(org_text, '\n')) do
    table.insert(lines, line) end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  vim.b[buf].skg_edit_source_buf = source_buf
  vim.b[buf].skg_edit_line = line_number
  vim.b[buf].skg_edit_start = metadata_start
  vim.b[buf].skg_edit_length = metadata_length
  -- The kind is kept by its first atom, a string, since a buffer
  -- variable holds a copy of what it is given.
  vim.b[buf].skg_edit_kind_atom = kind and kind.atoms[1] or nil
  vim.api.nvim_create_autocmd('BufWriteCmd', {
    buffer = buf,
    callback = function () M.apply(buf) end })
  vim.keymap.set('n', '<localleader>cc',
                 function () M.apply(buf) end,
                 { buffer = buf,
                   desc = 'Apply the edited metadata' })
  vim.keymap.set('n', '<S-Left>', function () M.cycle(-1) end,
                 { buffer = buf, desc = 'Cycle this value left' })
  vim.keymap.set('n', '<S-Right>', function () M.cycle(1) end,
                 { buffer = buf, desc = 'Cycle this value right' })
  vim.api.nvim_set_current_buf(buf)
end

---Move the cursor to the first child headline under FIELD_NAME.
---@param field_name string
function M.goto_field_value (field_name)
  for line = 1, vim.api.nvim_buf_line_count(0) do
    local text = metadata.line_text(line)
    if text:match('^%*+ ' .. field_name .. '$') then
      local child = metadata.next_headline_line(line)
      vim.api.nvim_win_set_cursor(0, { child or line, 0 })
      return
    end
  end
end

---Apply the edit buffer back into the source headline's metadata,
---then kill the edit buffer and return to the source buffer.
---@param buf integer
function M.apply (buf)
  local source_buf = vim.b[buf].skg_edit_source_buf
  local line_number = vim.b[buf].skg_edit_line
  local start_col = vim.b[buf].skg_edit_start
  local length = vim.b[buf].skg_edit_length
  if not source_buf or not vim.api.nvim_buf_is_valid(source_buf) then
    error('The buffer this metadata came from is no longer open') end
  local org_text = table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
  local kind_atom = vim.b[buf].skg_edit_kind_atom
  if kind_atom then
    org_text = fields.strip_defaults_from_org(
      org_text, shared.metadata_kind(kind_atom)) end
  local new_sexp = bijection.org_to_sexp(org_text)
  local new_text = sexpr.to_string(new_sexp)
  vim.bo[buf].modified = false
  vim.api.nvim_buf_delete(buf, { force = true })
  vim.api.nvim_set_current_buf(source_buf)
  vim.api.nvim_buf_set_text(
    source_buf, line_number - 1, start_col,
    line_number - 1, start_col + length, { new_text })
  vim.api.nvim_win_set_cursor(0, { line_number, start_col })
end

-- ── value cycling ──────────────────────────────────────────────────

---The values to cycle through for FIELD_NAME, or nil when the field
---is not cycleable. FIELD_VALUE matters for skgrepo defaulting. Every
---field but the skgrepo cycles as 'shared/metadata-fields.json' says
---for the kind of the headline being edited: a value offered only to
---an editable headline is left out when the edit buffer sets
---writeProtected to true, and one offered only to a removed member when
---the headline's metadata lacks '(diff (rel removed))'. The diff view's
---restore requests are described in 'docs/diff-view.org', "Restoring
---what a change removed". Mirrors skg-sexp-edit--cycle-values-for-field.
---@param field_name string
---@param field_value string
---@return string[]|nil
function M.cycle_values_for_field (field_name, field_value)
  if field_name == 'nodeRepo' then
    return M.repo_cycle_values(field_value) end
  return fields.cycle_values(M.edit_buffer_kind(), field_name,
                             M.editable_p(),
                             metadata.removed_membership_p(M.source_sexp()))
end

---The kind of the headline the current edit buffer edits, from
---'shared/metadata-fields.json', or nil.
---@return table|nil
function M.edit_buffer_kind ()
  local kind_atom = vim.b.skg_edit_kind_atom
  return kind_atom and shared.metadata_kind(kind_atom) or nil
end

---Is the headline being edited editable: does the current edit buffer
---not set its writeProtected field to true?
---@return boolean
function M.editable_p ()
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for i, line in ipairs(lines) do
    local stars = line:match('^(%*+) writeProtected$')
    if stars then
      return lines[i + 1] ~= stars .. '* true' end
  end
  return true
end

---Prompt for ARGUMENT of the field value VALUE, as
---'shared/metadata-fields.json' describes it: an id (a pasted link is
---accepted), a skgrepo, or one of a fixed list.
---@param value string
---@param argument table
---@return string|nil
function M.read_argument (value, argument)
  if argument.of == 'id' then
    return vim.fn.input(value .. ': ID (or paste a link): ') end
  if argument.of == 'skgrepo' then
    local names = config.repo_names() or {}
    return picker.completing_read_with_cycle(
      value .. ': skgrepo: ', names, { cycle_values = names }) end
  return picker.completing_read_with_cycle(
    value .. ': ', argument.one_of,
    { require_match = true, cycle_values = argument.one_of })
end

---The metadata sexp the current edit buffer was opened on, as its
---source buffer still holds it, or nil.
---@return any
function M.source_sexp ()
  local source_buf = vim.b.skg_edit_source_buf
  if not source_buf or not vim.api.nvim_buf_is_valid(source_buf) then
    return nil end
  local line = vim.api.nvim_buf_get_lines(
    source_buf, vim.b.skg_edit_line - 1, vim.b.skg_edit_line, false)[1]
  if not line then return nil end
  local ok, parsed = pcall(sexpr.read,
    line:sub(vim.b.skg_edit_start + 1,
             vim.b.skg_edit_start + vim.b.skg_edit_length))
  if ok then return parsed end
  return nil
end

---Owned skgrepo names as a cycle list; a ' (default)'-suffixed current
---value leads, so cycling starts there.
---@param field_value string
---@return string[]|nil
function M.repo_cycle_values (field_value)
  local skgrepos = config.owned_repos()
  if not skgrepos or #skgrepos == 0 then return nil end
  if field_value:match(' %(default%)$') then
    local bare = field_value:gsub(' %(default%)$', '')
    local values = { field_value }
    for _, name in ipairs(skgrepos) do
      if name ~= bare then table.insert(values, name) end
    end
    return values
  end
  return skgrepos
end

---Cycle the headline value at point by DIRECTION (1 or -1); dispatch
---is based on the parent headline text. Choosing a value that takes
---arguments prompts for each. FIELD_VALUE is found among the values
---whole, or by its first word when it carries arguments ('merge ID').
---@param direction integer
function M.cycle (direction)
  if not metadata.at_headline_p() then error('Not on a headline') end
  local line = metadata.current_line_number()
  local split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line))
  local field_value = vim.trim(split.title ~= '' and split.title
                               or split.metadata)
  local level = metadata.outline_level(line)
  local parent_line = metadata.parent_headline_line(line, level)
  if not parent_line then error('Cannot determine parent field') end
  local parent = vim.trim(metadata.split_as_stars_metadata_title(
    metadata.line_text(parent_line)).title)
  local values = M.cycle_values_for_field(parent, field_value)
  if values then
    local index = nil
    for i, value in ipairs(values) do
      if value == field_value then index = i break end
    end
    if not index then
      local first_word = field_value:match('^(%S+)')
      for i, value in ipairs(values) do
        if value == first_word then index = i break end
      end
    end
    local new_value =
      values[((index or 1) - 1 + direction) % #values + 1]
    metadata.replace_line(line,
      string.rep('*', level) .. ' ' .. new_value)
    local arguments = fields.value_arguments(
      M.edit_buffer_kind(), parent, new_value)
    if arguments then
      local texts = { new_value }
      for _, argument in ipairs(arguments) do
        local answer = M.read_argument(new_value, argument)
        if not answer or vim.trim(answer) == '' then return end
        table.insert(texts, vim.trim(answer))
      end
      metadata.replace_line(line,
        string.rep('*', level) .. ' ' .. table.concat(texts, ' '))
    end
  elseif parent == 'nodeRepo' then
    local answer = vim.fn.input('Repo: ', field_value)
    if answer ~= '' then
      metadata.replace_line(line,
        string.rep('*', level) .. ' ' .. answer)
    end
  else
    error(string.format("Field '%s' is not cycleable", parent))
  end
end

-- ── a skgrepo for a new headline ───────────────────────────────────

---The kind of the headline at LINE_NUMBER, as its kind's atom among
---the top-level elements of its metadata, bare or heading a list
---(node, ghost, tombstone, aliasFolder, alias, flag ...), or nil for a
---headline without metadata or without a kind 'shared/metadata-fields.json'
---names. Mirrors skg--headline-kind-at-point.
---@param line_number integer
---@return string|nil
function M.headline_kind_atom (line_number)
  local sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if not sexpr.is_list(sexp) then return nil end
  for i = 2, #sexp do
    local form = sexp[i]
    local head = sexpr.is_list(form) and form[1] or form
    if sexpr.is_symbol(head) then
      local atom = sexpr.atom_text(head)
      if shared.metadata_kind(atom) then return atom end
    end
  end
  return nil
end

---Whether the save can give the headline at LINE_NUMBER a skgrepo by
---inheritance, as the server's walk rootward decides
---('skgrepo_of_nearest_occurrence_ancestor'): it passes through
---unknowns, ghosts and folders, and through headlines without
---metadata, which inherit the same way; the first other ancestor must
---be an occurrence. A root cannot inherit, nor can a
---headline whose walk ends at a tombstone or a property.
---Mirrors skg--headline-can-inherit-skgrepo-p.
---@param line_number integer
---@return boolean
function M.headline_can_inherit_skgrepo_p (line_number)
  local line = line_number
  while true do
    local level = metadata.outline_level(line)
    line = level and metadata.parent_headline_line(line, level)
    if not line then return false end
    local kind = M.headline_kind_atom(line)
    if kind == 'node' then return true end
    if not (kind == nil
            or kind == 'unknown' or kind == 'ghost'
            or kind:match('Folder$')) then
      return false end
  end
end

---After one of org's insert-heading actions, give the new headline a
---skgrepo if it cannot inherit one. Acts only in a content view, on a
---new, empty headline, when M.headline_can_inherit_skgrepo_p says no.
---Routes through metadata.set_repo, which on a metadata-less headline
---prompts for an owned skgrepo (picker.prompt_for_owned_repo, no
---prompt when only one is owned) and writes minimal node metadata
---(metadata.populate_minimal_node_metadata) -- the quick path, no edit
---buffer. (The full metadata view is reserved for an explicit
---<localleader>vm.) Mirrors skg-sexp-edit--after-insert-headline.
function M.after_insert_headline ()
  if vim.b.skg_content_view ~= true then return end
  local line = metadata.current_line_number()
  if not metadata.line_text(line):match('^%*+%s*$') then return end
  if M.headline_can_inherit_skgrepo_p(line) then return end
  metadata.set_repo()
end

---Wrap nvim-orgmode's insert-heading actions,
---'org_insert_heading_respect_content' and 'org_meta_return', so that
---each runs M.after_insert_headline after it: the analog of the elisp
---advice on org-insert-heading-respect-content. The wrap is on the
---class, so it reaches every buffer. Idempotent; a no-op without
---nvim-orgmode.
function M.wrap_org_insert_heading_actions ()
  local ok, org_mappings = pcall(require, 'orgmode.org.mappings')
  if not ok or org_mappings.skg_wraps_insert_heading then return end
  org_mappings.skg_wraps_insert_heading = true
  for _, method in ipairs({ 'insert_heading_respect_content',
                            'meta_return' }) do
    local original = org_mappings[method]
    org_mappings[method] = function (...)
      local result = original(...)
      M.after_insert_headline()
      return result
    end
  end
end

return M
