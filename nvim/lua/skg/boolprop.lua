-- Read and stage the user-mutable noSearchMatching file property.

local client = require('skg.client')
local config = require('skg.config')
local focus = require('skg.focus')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}
local choices = { 'search matching', 'no search matching' }
local request_namespace = vim.api.nvim_create_namespace('skg-boolprop-request')

local function edit_request_p (meta)
  local values = metadata.sexp_cdr_at_path(meta,
    { 'skg', 'node', 'editRequest' })
  return values ~= nil and #values > 0
end

local function eligible_root (line)
  local meta = metadata.metadata_sexp_at_line_or_nil(line)
  if not metadata.activeNode_sexp_p(meta) then
    error('Search matching can be set only on an active node') end
  if metadata.node_write_protected_p(meta) then
    error('Cannot set search matching on a write-protected node') end
  if edit_request_p(meta) then error('This node already has an editRequest') end
  local id = metadata.node_id(meta)
  if not id then error('Save the node first; it has no graph ID') end
  return id
end

local function stamp (line, desired)
  metadata.edit_metadata_at_line(line, sexpr.read(string.format(
    '(skg (node (editRequest (property noSearchMatching %s))))',
    desired and 'true' or 'false')))
end

local function recursive_targets (root_line)
  local result = { root_line }
  local root_level = metadata.outline_level(root_line)
  local line = metadata.next_heading_line(root_line)
  while line and (metadata.outline_level(line) or 0) > root_level do
    local meta = metadata.metadata_sexp_at_line_or_nil(line)
    if metadata.activeNode_sexp_p(meta)
       and metadata.node_affectsParent_content_of_p(meta) then
      table.insert(result, line)
      line = metadata.next_heading_line(line)
    else
      line = metadata.next_heading_after_subtree(line)
    end
  end
  return result
end

local function apply (root_line, desired, recursive)
  if not recursive then
    stamp(root_line, desired)
    vim.notify('Search matching staged for 1 node. Save to apply.')
    return
  end
  local owned = {}
  for _, name in ipairs(config.owned_sources() or {}) do owned[name] = true end
  local seen, skipped, changed = {}, {}, 0
  for _, line in ipairs(recursive_targets(root_line)) do
    local meta = metadata.metadata_sexp_at_line_or_nil(line)
    local id = metadata.node_id(meta)
    local reason
    if not id then reason = 'no saved ID'
    elseif seen[id] then reason = 'duplicate occurrence'
    elseif metadata.node_write_protected_p(meta) then reason = 'write-protected'
    elseif edit_request_p(meta) then reason = 'already has an editRequest'
    elseif not owned[metadata.node_source(meta)] then reason = 'foreign source' end
    if reason == 'duplicate occurrence' then
      -- A PID already stamped is intentionally silent.
    elseif reason then
      table.insert(skipped, string.format('%s (%s)', id or 'no-id', reason))
    else
      seen[id] = true
      stamp(line, desired)
      changed = changed + 1
    end
  end
  vim.notify(string.format(
    'Search matching staged for %d node%s; skipped: %s. Save to apply.',
    changed, changed == 1 and '' or 's',
    #skipped > 0 and table.concat(skipped, ', ') or 'none'))
end

local function request (recursive)
  local buf = vim.api.nvim_get_current_buf()
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  local expected_id = eligible_root(line)
  local mark = vim.api.nvim_buf_set_extmark(
    buf, request_namespace, line - 1, 0, { right_gravity = true })
  state.register_response_handler('property-state',
    function (_payload_text, response)
      if not vim.api.nvim_buf_is_valid(buf) then
        vim.notify('skg: buffer vanished before the search-matching prompt')
        return end
      local position = vim.api.nvim_buf_get_extmark_by_id(
        buf, request_namespace, mark, {})
      pcall(vim.api.nvim_buf_del_extmark, buf, request_namespace, mark)
      if #position == 0 then
        vim.notify('skg: headline vanished before the search-matching prompt')
        return
      end
      local err = payload.field_text(response, 'error')
      if err then vim.notify('property state: ' .. err) return end
      local canonical_id = payload.field_text(response, 'id')
      local value = payload.field_text(response, 'value')
      local user_owned = payload.field_text(response, 'user-owned')
      if user_owned ~= 'true' then
        vim.notify('Cannot set search matching on a foreign node') return end
      if not vim.api.nvim_buf_is_loaded(buf) then return end
      vim.api.nvim_buf_call(buf, function ()
        local current_line = position[1] + 1
        local current_meta = metadata.metadata_sexp_at_line_or_nil(current_line)
        local current_id = current_meta and metadata.node_id(current_meta)
        if current_id ~= expected_id and current_id ~= canonical_id then
          vim.notify('The headline changed while property state was loading') return end
        local ok = pcall(eligible_root, current_line)
        if not ok then
          vim.notify('The headline is no longer eligible for a property edit') return end
        local initial = value == 'true'
          and 'no search matching' or 'search matching'
        local choice = picker.completing_read_with_cycle(
          'Search behavior (S-left/right cycle): ', choices, {
            initial_input = initial, cycle_values = choices,
            require_match = true })
        if not choice then return end
        apply(current_line, choice == 'no search matching', recursive)
      end)
    end, true)
  state.lp_reset()
  client.send_string(string.format(
    '((request . "property state") (id . %q) (property . "noSearchMatching"))\n',
    expected_id))
end

function M.set_search_matching () request(false) end
function M.set_search_matching_recursive () request(true) end

M._recursive_targets = recursive_targets
M._apply = apply
M._request = request

return M
