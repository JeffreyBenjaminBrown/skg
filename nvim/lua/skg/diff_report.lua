-- PURPOSE: Request an org report of semantic graph changes (the
-- git-diff report) and show it in a report buffer with the
-- navigation keymap subset. The Lua port of
-- elisp/skg-request-diff-report.el.

local buffer = require('skg.buffer')
local client = require('skg.client')
local messages = require('skg.messages')
local payload = require('skg.payload')
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
  local unsaved = buffer.other_unsaved_skg_buffers(-1)
  if #unsaved > 0 then
    local names = {}
    for _, buf in ipairs(unsaved) do
      table.insert(names, vim.api.nvim_buf_get_name(buf)) end
    error('Cannot make diff report: unsaved skg buffer(s): '
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
  state.register_response_handler('diff-report',
    function (_payload_text, response)
      M.diff_report_handler(response)
    end, true)
  state.lp_reset()
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'diff report'),
    sexpr.pair(sexpr.symbol('include-staged'),
               include_staged and 'true' or 'false'),
    sexpr.pair(sexpr.symbol('include-unstaged'),
               include_unstaged and 'true' or 'false') }) .. '\n')
end

---@param response any
function M.diff_report_handler (response)
  local content = payload.field_text(response, 'content')
  local errors = payload.string_list(payload.field(response, 'errors'))
  local warnings =
    payload.string_list(payload.field(response, 'warnings'))
  local headline
  if #errors > 0 and #warnings > 0 then
    headline = 'Diff report completed with errors and warnings'
  elseif #errors > 0 then
    headline = 'Diff report completed with errors'
  elseif #warnings > 0 then
    headline = 'Diff report completed with warnings'
  else
    headline = 'Diff report complete' end
  messages.big_nonfatal_message(
    'skg://diff-report', headline,
    content or '* diff report failed\n** Empty response\n')
  if #errors > 0 or #warnings > 0 then
    messages.big_nonfatal_message(
      'skg://messages/diff-report', 'Diff report messages',
      messages.errors_and_warnings_to_org_string(errors, warnings))
  end
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == 'skg://diff-report' then
      require('skg.keymaps').attach_diff_report(buf)
    end
  end
end

return M
