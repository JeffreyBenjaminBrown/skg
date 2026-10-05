-- PURPOSE: Export Skg data to plain .org files. All the logic is in
-- Rust; this command picks a skgrepo-set (client-side, with the
-- cycling selector) and an output dir, sends the request, and shows
-- the server's report. The Lua port of elisp/skg-request-export-org.el.

local client = require('skg.client')
local messages = require('skg.messages')
local payload = require('skg.payload')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Export REPO_SET (prompted when absent; choose a public-facing set
---so private notes stay private) to OUTPUT_DIR, which the SERVER
---resolves against its own project root -- possibly inside a
---container whose filesystem differs from this editor's, hence no
---path completion on the prompt. Defaults to 'org-exports'.
---@param repo_set string|nil
---@param output_dir string|nil
function M.export_some_to_org (repo_set, output_dir, approved_pids)
  repo_set = repo_set or picker.prompt_for_repo_set()
  if not repo_set then return end
  if not output_dir then
    output_dir = vim.fn.input(
      "Export to dir (relative to the server's project root)"
      .. ' [org-exports]: ')
    if output_dir == '' then output_dir = 'org-exports' end
  end
  if repo_set == 'all' then
    vim.notify("Exporting repo-set 'all'; consider a public-facing"
               .. ' repo-set so private notes are not exported.')
  end
  state.register_response_handler('export-to-org',
    function (_payload_text, response)
      state.response_handler_map['overPrivateText-telescope-confirmation'] = nil
      M.export_to_org_handler(response)
    end, true)
  state.register_response_handler('overPrivateText-telescope-confirmation',
    function (_payload_text, response)
      state.response_handler_map['overPrivateText-telescope-confirmation'] = nil
      if state.response_handler_map['export-to-org'] then
        state.response_handler_map['export-to-org'] = nil
        state.lp_pending_count = math.max(0, state.lp_pending_count - 1)
      end
      local prompt = payload.field_text(response, 'prompt') or
        'This export includes text selected below home. Include it?'
      local pids = payload.string_list(payload.field(response, 'pids'))
      if vim.fn.confirm(prompt, '&Include\n&Decline', 2) == 1 then
        M.export_some_to_org(repo_set, output_dir, pids) end
    end, false)
  state.lp_reset()
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'export to org'),
    sexpr.pair(sexpr.symbol('repo-set'), repo_set),
    sexpr.pair(sexpr.symbol('output-dir'), output_dir) }
  if approved_pids and #approved_pids > 0 then
    local approval = { sexpr.symbol('approved-overPrivateText-pids') }
    for _, pid in ipairs(approved_pids) do table.insert(approval, pid) end
    table.insert(request, approval) end
  client.send_string(sexpr.to_string(request) .. '\n')
end

---@param response any
function M.export_to_org_handler (response)
  local content = payload.field_text(response, 'content')
  local errors = payload.string_list(payload.field(response, 'errors'))
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  local headline
  if #errors > 0 then
    headline = 'Export to org completed with errors'
  elseif #warnings > 0 then
    headline = 'Export to org completed with warnings'
  else
    headline = 'Export to org complete' end
  messages.big_nonfatal_message(
    'skg://messages/export', headline,
    content or 'Export produced no output.')
  if #errors > 0 or #warnings > 0 then
    messages.big_nonfatal_message(
      'skg://messages/export-details', 'Export to org messages',
      messages.errors_and_warnings_to_org_string(errors, warnings))
  end
end

return M
