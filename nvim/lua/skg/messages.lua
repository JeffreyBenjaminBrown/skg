-- PURPOSE: Display helpers for server-reported errors and warnings.
-- The Lua port of the message-formatting half of
-- elisp/skg-request-save.el (skg-big-nonfatal-message and friends),
-- which content views, saves, and the stream consumers all share.

local M = {}

---@param message_list string[]|nil
---@return boolean true when MESSAGE_LIST has at least one message
function M.message_list_nonempty_p (message_list)
  return type(message_list) == 'table' and #message_list > 0
end

---MESSAGES as org text, one headline each.
---@param messages string[]|string
---@return string
function M.messages_to_org_string (messages)
  if type(messages) == 'table' then
    local lines = {}
    for _, message in ipairs(messages) do
      table.insert(lines, '* ' .. message) end
    return table.concat(lines, '\n')
  end
  return '* ' .. tostring(messages)
end

---ERRORS and WARNINGS as one org document with two sections.
---@param errors string[]|nil
---@param warnings string[]|nil
---@return string
function M.errors_and_warnings_to_org_string (errors, warnings)
  local sections = {}
  if M.message_list_nonempty_p(errors) then
    local lines = { '* errors' }
    for _, message in ipairs(errors) do
      table.insert(lines, '** ' .. message) end
    table.insert(sections, table.concat(lines, '\n'))
  end
  if M.message_list_nonempty_p(warnings) then
    local lines = { '* warnings' }
    for _, message in ipairs(warnings) do
      table.insert(lines, '** ' .. message) end
    table.insert(sections, table.concat(lines, '\n'))
  end
  return table.concat(sections, '\n')
end

---Display CONTENT in an org scratch buffer named BUFFER_NAME (shown
---in a split without stealing focus) and echo MESSAGE_TEXT.
---@param buffer_name string
---@param message_text string
---@param content string
function M.big_nonfatal_message (buffer_name, message_text, content)
  local buf = M.scratch_org_buffer(buffer_name, content)
  local already_visible = false
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if vim.api.nvim_win_get_buf(win) == buf then
      already_visible = true break end
  end
  if not already_visible then
    local current_win = vim.api.nvim_get_current_win()
    vim.cmd('split')
    vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
    vim.api.nvim_set_current_win(current_win)
  end
  vim.notify(message_text)
end

---Find-or-create a scratch org buffer named NAME holding CONTENT.
---@param name string
---@param content string
---@return integer bufnr
function M.scratch_org_buffer (name, content)
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(existing) == name then
      buf = existing break end
  end
  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(buf, name)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(content, '\n'))
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  return buf
end

return M
