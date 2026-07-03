-- PURPOSE: Edit a headline's metadata sexp as an org tree in a
-- temporary buffer: ':w' (or <localleader>cc) commits the edited tree
-- back into the source headline; killing the buffer cancels;
-- S-left/S-right cycle field values. The Lua port of
-- elisp/skg-sexpr-edit.el and skg-sexpr-edit/skg-sexpr-cycling.el.
--
-- DEVIATION: the elisp made the display-only title group read-only
-- with text properties; nvim has no per-range read-only text, so
-- title edits are simply DISCARDED at commit (the strip step drops
-- the title group whatever it holds).
--
-- DEVIATION: the elisp advised org-insert-heading-respect-content so
-- a fresh level-1 heading prompted for a source. vim users type
-- headlines rather than calling an insert-heading command, so there
-- is no equivalent seam; the quick path is <localleader>ss on the new
-- headline (which populates minimal metadata), or <localleader>vm for
-- the full metadata view.

local bijection = require('skg.sexpr.org_bijection')
local config = require('skg.config')
local defaults = require('skg.sexpr.activenode_defaults')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

M.help_text =
  ':w (or <localleader>cc) saves, :bd! cancels,'
  .. ' S-left/S-right cycle values.'

---Edit the metadata sexp on the current headline. With no metadata,
---populates a minimal (skg (node (source X))) in place and opens the
---empty-node view over it: source pre-filled, the other editable
---fields childless, so an untouched commit yields just the source.
function M.edit_metadata ()
  if not metadata.at_heading_p() then error('Not on a headline') end
  local source_buf = vim.api.nvim_get_current_buf()
  local line_number = metadata.current_line_number()
  local split = metadata.split_as_stars_metadata_title(
    metadata.get_current_headline_text())
  if not split then error('Not on a headline') end
  if split.metadata == '' then
    M.open_empty_node_view(source_buf, line_number, split)
    return end
  local sexp = sexpr.read(split.metadata)
  local is_activeNode = defaults.activeNode_sexp_p(sexp)
  local org_text = bijection.sexp_to_org(sexp)
  if is_activeNode then
    org_text = defaults.expand_defaults_in_org(
      org_text, nil, split.title) end
  M.open_edit_buffer(org_text, source_buf, line_number,
                     #split.stars, #split.metadata, is_activeNode)
  M.goto_field_value('source')
end

---Populate minimal metadata on the metadata-less headline, then open
---its metadata view with the chosen source pre-filled and every other
---editable field childless.
---@param source_buf integer
---@param line_number integer
---@param split table
function M.open_empty_node_view (source_buf, line_number, split)
  local source = metadata.populate_minimal_node_metadata()
  if not source then return end
  local new_split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line_number))
  M.open_edit_buffer(
    M.empty_node_org_text(source, split.title),
    source_buf, line_number,
    #new_split.stars, #new_split.metadata, true)
  M.goto_field_value('source')
end

---Org text for the empty-node metadata view: SOURCE pre-filled;
---TITLE, if non-blank, under a display-only title group; every other
---editable field childless, so the strip step drops the ones the user
---never populates.
---@param source string
---@param title string
---@return string
function M.empty_node_org_text (source, title)
  local headlines = {}
  table.insert(headlines, { level = 1, text = 'title' })
  if title and not title:match('^%s*$') then
    table.insert(headlines, { level = 2, text = title }) end
  table.insert(headlines, { level = 1, text = 'skg' })
  table.insert(headlines, { level = 2, text = 'node' })
  table.insert(headlines, { level = 3, text = 'source' })
  table.insert(headlines, { level = 4, text = source })
  for _, entry in ipairs(defaults.editable_defaults) do
    table.insert(headlines, { level = 3, text = entry.name })
  end
  return defaults.headlines_to_org(headlines)
end

---Open the edit buffer over ORG_TEXT, remembering where the metadata
---lives in the source (line + byte range) so commit can splice it.
---@param org_text string
---@param source_buf integer
---@param line_number integer
---@param metadata_start integer 0-based byte column
---@param metadata_length integer
---@param is_activeNode boolean
function M.open_edit_buffer (org_text, source_buf, line_number,
                             metadata_start, metadata_length,
                             is_activeNode)
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
  vim.b[buf].skg_edit_is_activeNode = is_activeNode
  vim.api.nvim_create_autocmd('BufWriteCmd', {
    buffer = buf,
    callback = function () M.commit(buf) end })
  vim.keymap.set('n', '<localleader>cc',
                 function () M.commit(buf) end,
                 { buffer = buf,
                   desc = 'Commit the edited metadata' })
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
      local child = metadata.next_heading_line(line)
      vim.api.nvim_win_set_cursor(0, { child or line, 0 })
      return
    end
  end
end

---Commit the edit buffer back into the source headline's metadata,
---then kill the edit buffer and return to the source.
---@param buf integer
function M.commit (buf)
  local source_buf = vim.b[buf].skg_edit_source_buf
  local line_number = vim.b[buf].skg_edit_line
  local start_col = vim.b[buf].skg_edit_start
  local length = vim.b[buf].skg_edit_length
  if not source_buf or not vim.api.nvim_buf_is_valid(source_buf) then
    error('The buffer this metadata came from is no longer open') end
  local org_text = table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
  if vim.b[buf].skg_edit_is_activeNode then
    org_text = defaults.strip_defaults_from_org(org_text) end
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
---is not cycleable. FIELD_VALUE matters for source defaulting.
---@param field_name string
---@param field_value string
---@return string[]|nil
function M.cycle_values_for_field (field_name, field_value)
  if field_name == 'indef' then
    return { 'false (default)', 'true' } end
  if field_name == 'parentIs' then
    return { 'affected (default)', 'independent', 'absent' } end
  if field_name == 'editRequest' then
    return { 'none (default)', 'delete', 'merge' } end
  if field_name == 'source' then
    return M.source_cycle_values(field_value) end
  if field_name == 'viewRequests' then
    -- Only the bare-atom request is cycleable; (col X)/(path X) are
    -- structured forms inserted by their dedicated commands.
    return { 'none (default)', 'definitiveView' } end
  return nil
end

---Owned source names as a cycle list; a ' (default)'-suffixed current
---value leads, so cycling starts there.
---@param field_value string
---@return string[]|nil
function M.source_cycle_values (field_value)
  local sources = config.owned_sources()
  if not sources or #sources == 0 then return nil end
  if field_value:match(' %(default%)$') then
    local bare = field_value:gsub(' %(default%)$', '')
    local values = { field_value }
    for _, name in ipairs(sources) do
      if name ~= bare then table.insert(values, name) end
    end
    return values
  end
  return sources
end

---Cycle the headline value at point by DIRECTION (1 or -1); dispatch
---is based on the parent headline text. Choosing 'merge' prompts for
---the target id.
---@param direction integer
function M.cycle (direction)
  if not metadata.at_heading_p() then error('Not on a headline') end
  local line = metadata.current_line_number()
  local split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line))
  local field_value = vim.trim(split.title ~= '' and split.title
                               or split.metadata)
  local level = metadata.outline_level(line)
  local parent_line = metadata.parent_heading_line(line, level)
  if not parent_line then error('Cannot determine parent field') end
  local parent = vim.trim(metadata.split_as_stars_metadata_title(
    metadata.line_text(parent_line)).title)
  local values = M.cycle_values_for_field(parent, field_value)
  if values then
    local index = 1
    for i, value in ipairs(values) do
      if value == field_value then index = i break end
    end
    local new_value =
      values[(index - 1 + direction) % #values + 1]
    metadata.replace_line(line,
      string.rep('*', level) .. ' ' .. new_value)
    if new_value == 'merge' then
      local id = vim.fn.input(
        'Enter merge target ID (or paste a link): ')
      if id ~= '' then
        metadata.replace_line(line,
          string.rep('*', level) .. ' merge ' .. id)
      end
    end
  elseif parent == 'source' then
    local answer = vim.fn.input('Source: ', field_value)
    if answer ~= '' then
      metadata.replace_line(line,
        string.rep('*', level) .. ' ' .. answer)
    end
  else
    error(string.format("Field '%s' is not cycleable", parent))
  end
end

return M
