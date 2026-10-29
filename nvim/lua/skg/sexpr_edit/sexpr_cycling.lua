-- PURPOSE: S-left / S-right cycling for the metadata edit buffer
-- (skg.sexpr_edit). Dispatch is based on the parent headline text;
-- what a field's cycle runs through comes from
-- 'shared/metadata-fields.json' (skg.sexpr.metadata_fields). The Lua
-- port of elisp/skg-sexpr-edit/skg-sexpr-cycling.el.
--
-- ENTRY POINTS: M.cycle_left, M.cycle_right.

local config = require('skg.config')
local fields = require('skg.sexpr.metadata_fields')
local metadata = require('skg.metadata')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local shared = require('skg.shared')

local M = {}

---The values to cycle through for FIELD_NAME, or nil when the field
---is not cycleable. The nodeRepo cycles through the owned skgrepos;
---every other field cycles as 'shared/metadata-fields.json' says
---for the kind of the headline being edited: a value offered only to
---an editable headline is left out when the edit buffer sets
---writeProtected to true, and one offered only to a removed member when
---the headline's metadata lacks '(diff (rel removed))'. The diff view's
---restore requests are described in 'docs/diff-view.org', "Restoring
---what a change removed". Mirrors skg-sexp-edit--cycle-values-for-field.
---@param field_name string
---@return string[]|nil
function M.cycle_values_for_field (field_name)
  if field_name == 'nodeRepo' then -- from the config
    local skgrepos = config.owned_repos()
    if skgrepos and #skgrepos > 0 then return skgrepos end
    return nil end
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
  local values = M.cycle_values_for_field(parent)
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

---Cycle the value at point to the previous one. Mirrors
---skg-sexp-edit-cycle-left.
function M.cycle_left ()
  M.cycle(-1)
end

---Cycle the value at point to the next one. Mirrors
---skg-sexp-edit-cycle-right.
function M.cycle_right ()
  M.cycle(1)
end

return M
