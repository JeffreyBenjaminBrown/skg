-- PURPOSE: 'set_relRepo' -- request the relRepo of one relationship,
-- informed by the server's 'relRepo info' endpoint
-- ('docs/api-and-formats.org', "relRepo info"). The helpers it drives
-- live in skg.metadata. The Lua port of
-- elisp/skg-request-relRepo-info.el.

local client = require('skg.client')
local config = require('skg.config')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

local namespace = vim.api.nvim_create_namespace('skg-relRepo-request')

---Set the relRepo of the relationship or alias at point. With
---RECURSIVE, run metadata.set_relRepo_recursive instead, which asks
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
  if recursive then return metadata.set_relRepo_recursive() end
  M.set_relRepo_at_line(metadata.current_line_number())
end

---@param line_number integer
---@return integer|nil the line of the headline's viewparent
local function parent_line (line_number)
  local level = metadata.outline_level(line_number)
  return level and metadata.parent_headline_line(line_number, level)
end

---The single-relationship path of M.set_relRepo: classify the
---relationship at LINE_NUMBER, ask the server for its default and
---current repos, and prompt from the reply. An alias's default is its
---owning node's home, read from the buffer with no request. Mirrors
---skg--set-relRepo-at-point.
---@param line_number integer
function M.set_relRepo_at_line (line_number)
  local buf = vim.api.nvim_get_current_buf()
  if metadata.alias_headline_p(line_number) then
    local folder = parent_line(line_number)
    local owner = folder and parent_line(folder)
    if not owner then error('Alias has no owning node headline', 0) end
    local default = metadata.node_repo(
      metadata.metadata_sexp_at_line_or_nil(owner))
    if not default then error('Node has no repo', 0) end
    local current = metadata.relRepo_current_value(line_number)
    local mark = vim.api.nvim_buf_set_extmark(
      buf, namespace, line_number - 1, 0, {})
    M.set_relRepo_from_info(buf, mark, sexpr.read(string.format(
      '((response-type relRepo-info) (default %q)%s)', default,
      current and string.format(' (current %q)', current) or '')))
    return end
  local relationship = metadata.rel_at_line(line_number)
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
    local requested = metadata.relRepo_requested_value(line)
    local choices = metadata.relRepo_choices(config.repo_names() or {},
                                             default)
    table.insert(choices, metadata.relRepo_no_override)
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
    local ok, message = pcall(metadata.apply_relRepo_choice, line, choice)
    vim.notify(tostring(message),
               ok and vim.log.levels.INFO or vim.log.levels.ERROR)
  end)
end

return M
