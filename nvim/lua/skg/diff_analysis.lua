-- PURPOSE: Request an org report of semantic graph changes (the
-- git-diff analysis) and show it in a report buffer with the
-- navigation keymap subset. The Lua port of
-- elisp/skg-request-diff-analysis.el.

local client = require('skg.client')
local messages = require('skg.messages')
local payload = require('skg.payload')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Request the report. Refuses while any skg buffer has unsaved
---edits (the report reads git/index/worktree state). Asks which
---stages to include: staged?, and if so unstaged?; declining staged
---implies unstaged only.
---@param include_staged boolean|nil for tests; prompted when nil
---@param include_unstaged boolean|nil
function M.diff_report (include_staged, include_unstaged)
  local unsaved = save.other_unsaved_skg_buffers(-1)
  if #unsaved > 0 then
    local names = {}
    for _, buf in ipairs(unsaved) do
      table.insert(names, vim.api.nvim_buf_get_name(buf)) end
    error('Cannot analyze diff: unsaved skg buffer(s): '
          .. table.concat(names, ', ')) end
  if include_staged == nil then
    include_staged = vim.fn.confirm('Include staged changes?',
                                    '&Yes\n&No', 1) == 1
    if include_staged then
      include_unstaged = vim.fn.confirm('Include unstaged changes?',
                                        '&Yes\n&No', 1) == 1
    else
      include_unstaged = true end
  end
  state.register_response_handler('diff-analysis',
    function (_payload_text, response)
      M.diff_analysis_handler(response)
    end, true)
  state.lp_reset()
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'diff analysis'),
    sexpr.pair(sexpr.symbol('include-staged'),
               include_staged and 'true' or 'false'),
    sexpr.pair(sexpr.symbol('include-unstaged'),
               include_unstaged and 'true' or 'false') }) .. '\n')
end

---@param response any
function M.diff_analysis_handler (response)
  local content = payload.field_text(response, 'content')
  local errors = payload.string_list(payload.field(response, 'errors'))
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  local headline
  if #errors > 0 and #warnings > 0 then
    headline = 'Diff analysis completed with errors and warnings'
  elseif #errors > 0 then
    headline = 'Diff analysis completed with errors'
  elseif #warnings > 0 then
    headline = 'Diff analysis completed with warnings'
  else
    headline = 'Diff analysis complete' end
  messages.big_nonfatal_message(
    'skg://diff-analysis', headline,
    content or '* diff analysis failed\n** Empty response\n')
  if #errors > 0 or #warnings > 0 then
    messages.big_nonfatal_message(
      'skg://messages/diff-analysis', 'Diff analysis messages',
      messages.errors_and_warnings_to_org_string(errors, warnings))
  end
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == 'skg://diff-analysis' then
      require('skg.keymaps').attach_diff_analysis(buf)
    end
  end
end

return M
