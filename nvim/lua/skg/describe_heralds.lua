-- PURPOSE: 'describe_at_point' (<localleader>v?) -- say in plain
-- sentences what the headline at point is, when it is a folder, and
-- what each of its heralds means, with this headline's own numbers and
-- letters read out. The sentences come from the server's "describe
-- heralds" endpoint ('docs/api-and-formats.org', "Describe heralds";
-- 'server/herald_descriptions.rs'). The Lua port of
-- elisp/skg-request-describe-heralds.el.

local client = require('skg.client')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---The "describe heralds" request for the headline at LINE_NUMBER of
---the current buffer, or the one whose body it is in: its metadata as
---the buffer holds it, with 'pendingDetachment' when it is a pending
---detachment, and, for a folder of the diff view whose members'
---relationships changed, whether every such change is processed.
---Mirrors skg--describe-request-at-point.
---@param line_number integer
---@return string
function M.request_at_line (line_number)
  local headline = metadata.at_headline_p(line_number) and line_number
    or metadata.previous_headline_line(line_number)
  if not headline then error('Not on or under a headline', 0) end
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local split = metadata.split_as_stars_metadata_title(lines[headline])
  if not split or split.metadata == '' then
    error('This headline carries no skg metadata', 0) end
  local text = split.metadata
  if metadata.pending_detachment_in_lines(lines, headline) then
    text = text:sub(1, -2) .. ' pendingDetachment)' end
  local request = {
    sexpr.pair(sexpr.symbol('request'), 'describe heralds'),
    sexpr.pair(sexpr.symbol('metadata'), text) }
  local folder_change = metadata.folder_change_processed_in_lines(
    lines, headline)
  if folder_change then
    table.insert(request, sexpr.pair(sexpr.symbol('folder-change'),
      folder_change.processed and 'processed' or 'unprocessed')) end
  return sexpr.to_string(request) .. '\n'
end

---Describe the headline at point; shows the sentences in the message
---area when they fit in two lines, else in a scratch buffer. Changes
---nothing. Mirrors skg-describe-at-point.
function M.describe_at_point ()
  local request = M.request_at_line(metadata.current_line_number())
  state.register_response_handler('describe-heralds',
    function (_payload_text, response)
      vim.schedule(function () M.show(M.lines_of_response(response)) end)
    end, true)
  state.lp_reset()
  client.send_string(request)
end

---The sentences of RESPONSE, a parsed describe-heralds response, or
---{ error = MESSAGE }.
---@param response any
---@return table
function M.lines_of_response (response)
  local lines = {}
  for _, element in ipairs(sexpr.is_list(response) and response or {}) do
    if sexpr.is_list(element) and #element >= 1
       and not sexpr.is_list(element[1]) then
      local key = sexpr.atom_text(element[1])
      if key == 'error' then return { error = sexpr.atom_text(element[2]) } end
      if key == 'lines' then
        for i = 2, #element do
          table.insert(lines, sexpr.atom_text(element[i])) end
      end
    end
  end
  return lines
end

---Show LINES (or its error): in the message area when, one to a line,
---they take at most two lines of the screen, else in a scratch buffer
---below, a sentence to a paragraph, which 'q' closes.
---@param lines table
function M.show (lines)
  if lines.error then
    vim.notify('skg describe: ' .. lines.error, vim.log.levels.ERROR)
    return end
  local width, screen_lines = math.max(1, vim.o.columns - 1), 0
  for _, line in ipairs(lines) do
    screen_lines = screen_lines
      + math.max(1, math.ceil(vim.fn.strdisplaywidth(line) / width)) end
  if screen_lines <= 2 then
    vim.notify(table.concat(lines, '\n'))
    return end
  local buf = vim.api.nvim_create_buf(false, true)
  local paragraphs = {}
  for _, line in ipairs(lines) do
    vim.list_extend(paragraphs, { line, '' }) end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, paragraphs)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = 'wipe'
  vim.cmd('botright split')
  vim.api.nvim_win_set_buf(0, buf)
  vim.wo.wrap, vim.wo.linebreak = true, true
  vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = buf })
end

return M
