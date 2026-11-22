-- PURPOSE: ':SkgExportToOrg' ('export_some_to_org') exports skg data
-- to plain .org files. All the logic is in Rust; this command picks a
-- skgrepo restriction (client-side, with the cycling selector of
-- skg.picker) and an output dir, sends the request, asks before
-- including overPrivateText telescopes when the server asks, and
-- shows the server's report ('export_to_org_handler'). The Lua port
-- of elisp/skg-request-export-org.el.

local client = require('skg.client')
local config = require('skg.config')
local length_prefix = require('skg.length_prefix')
local messages = require('skg.messages')
local payload = require('skg.payload')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Export REPO_SET, the most private owned repo to export, every more
---public owned repo and the foreign repos (prompted when absent;
---choose a public-facing repo so private notes stay private) to OUTPUT_DIR, which the server
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
  local names = config.owned_repos()
  if names and names[#names] == repo_set then
    vim.notify('Exporting with no skgrepo restriction; consider a more'
               .. ' public repo so private notes are not exported.')
  end
  state.register_response_handler('export-to-org',
    function (_payload_text, response)
      state.unregister_response_handlers(
        { 'overPrivateText-telescope-confirmation' })
      M.export_to_org_handler(response)
    end, true)
  length_prefix.register_overPrivateText_confirmation_instead_of(
    'export-to-org',
    'This export includes text selected below home. Include it?',
    function (pids) M.export_some_to_org(repo_set, output_dir, pids) end)
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
