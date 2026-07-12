-- PURPOSE: The interactive source/source-set pickers.
-- The Lua port of the prompt half of elisp/skg-config.el
-- ('skg--completing-read-with-cycle' and friends). The elisp built
-- them on completing-read with S-left/S-right cycling in the
-- minibuffer; here they ride vim.fn.input with TAB completion plus
-- temporary command-line mappings that cycle via setcmdline. One
-- deviation: the C-? list-all-sources helper key inside the prompt is
-- dropped (few terminals deliver C-?); :SkgViewSourceList shows the
-- same list.

local config = require('skg.config')
local messages = require('skg.messages')

local M = {}

---The collection the temporary completion function completes over.
---@type string[]
M.current_completion_collection = {}

---Completion hook for vim.fn.input; must be reachable from vimscript.
---@param arg_lead string
---@return string[]
function _G.__skg_picker_complete (arg_lead)
  local matches = {}
  for _, candidate in ipairs(M.current_completion_collection) do
    if candidate:sub(1, #arg_lead) == arg_lead then
      table.insert(matches, candidate) end
  end
  return matches
end

---Read a string with TAB completion over COLLECTION and
---S-left/S-right cycling through CYCLE_VALUES (replacing the whole
---prompt content, as the elisp cycler did).
---@param prompt string
---@param collection string[]
---@param opts table|nil {initial_input, cycle_values, require_match}
---@return string|nil nil when the user aborts
function M.completing_read_with_cycle (prompt, collection, opts)
  opts = opts or {}
  M.current_completion_collection = collection
  local cycle_values = opts.cycle_values or {}
  local made_cycle_maps = false
  if #cycle_values > 0 then
    made_cycle_maps = true
    local function cycle (direction)
      local current = vim.fn.getcmdline()
      local index = nil
      for i, value in ipairs(cycle_values) do
        if value == current then index = i break end
      end
      if not index and opts.initial_input then
        for i, value in ipairs(cycle_values) do
          if value == opts.initial_input then index = i break end
        end
      end
      index = ((index or 1) - 1 + direction) % #cycle_values + 1
      vim.fn.setcmdline(cycle_values[index])
      return ''
    end
    vim.keymap.set('c', '<S-Left>', function () return cycle(-1) end,
                   { expr = true })
    vim.keymap.set('c', '<S-Right>', function () return cycle(1) end,
                   { expr = true })
  end
  local ok, answer = pcall(vim.fn.input, {
    prompt = prompt,
    default = opts.initial_input or '',
    completion = 'customlist,v:lua.__skg_picker_complete',
    cancelreturn = '\127' })
  if made_cycle_maps then
    pcall(vim.keymap.del, 'c', '<S-Left>')
    pcall(vim.keymap.del, 'c', '<S-Right>')
  end
  if not ok or answer == '\127' then return nil end
  if opts.require_match then
    local found = false
    for _, candidate in ipairs(collection) do
      if candidate == answer then found = true break end
    end
    if not found then
      vim.notify(string.format('Not a valid choice: %s', answer))
      return nil end
  end
  return answer
end

---Choose an owned source, cycling with S-arrows; no prompt when only
---one source is owned. Nil when none are configured or user aborts.
---@return string|nil
function M.prompt_for_owned_source ()
  local owned = config.owned_sources()
  if not owned or #owned == 0 then
    vim.notify('No owned skg sources found')
    return nil end
  if #owned == 1 then return owned[1] end
  return M.completing_read_with_cycle('Source: ', owned, {
    require_match = true, cycle_values = owned })
end

---Choose a source to replace CURRENT_SOURCE: free-typed names are
---accepted, S-arrows cycle owned sources, TAB completes all names.
---@param current_source string
---@return string|nil
function M.prompt_for_source_change (current_source)
  local owned = config.owned_sources() or {}
  local names = config.source_names() or {}
  return M.completing_read_with_cycle(
    'Source (S-left/right cycle, TAB completes): ', names, {
      initial_input = current_source, cycle_values = owned })
end

---Choose a source-set name, with completion and cycling.
---@return string|nil
function M.prompt_for_source_set ()
  local source_sets = config.source_set_names()
  if not source_sets then
    vim.notify('No skg source-sets found')
    return nil end
  return M.completing_read_with_cycle(
    'Most private source to make available (S-left/right cycle): ', source_sets, {
      require_match = true, cycle_values = source_sets,
      initial_input = 'all' })
end

---Display an org buffer listing configured sources and their paths.
function M.view_source_list ()
  local source_paths = config.source_paths()
  if not source_paths or #source_paths == 0 then
    vim.notify('No skg sources found')
    return end
  local lines = {}
  for _, entry in ipairs(source_paths) do
    table.insert(lines,
      string.format('* %s\n%s', entry.name, entry.path))
  end
  local buf = messages.scratch_org_buffer('skg://sources',
                                          table.concat(lines, '\n'))
  vim.api.nvim_set_current_buf(buf)
end

return M
