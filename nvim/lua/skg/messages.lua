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

---The Emacs chords that server messages name, each with the Neovim
---binding that plays it (skg.keymaps).
M.neovim_bindings_of_emacs_chords = {
  ['C-c s d'] = '<localleader>sd',
  ['C-c s w'] = '<localleader>sw',
  ['C-c s s'] = '<localleader>ss',
  ['C-c g RET'] = '<localleader>gg',
  ['C-c g v'] = '<localleader>gv',
}

---MESSAGE with each Emacs chord it names replaced by its Neovim
---binding.
---@param message string
---@return string
function M.with_neovim_bindings (message)
  for chord, binding in pairs(M.neovim_bindings_of_emacs_chords) do
    message = message:gsub(vim.pesc(chord),
                           function () return binding end)
  end
  return message
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
      table.insert(lines, '** ' .. M.with_neovim_bindings(message)) end
    table.insert(sections, table.concat(lines, '\n'))
  end
  if M.message_list_nonempty_p(warnings) then
    local lines = { '* warnings' }
    for _, message in ipairs(warnings) do
      table.insert(lines, '** ' .. M.with_neovim_bindings(message)) end
    table.insert(sections, table.concat(lines, '\n'))
  end
  return table.concat(sections, '\n')
end

---Display CONTENT in an org scratch buffer named BUFFER_NAME (shown
---in a split without stealing focus) and echo MESSAGE_TEXT.
---@param buffer_name string
---@param message_text string
---@param content string
---@return integer bufnr
function M.big_nonfatal_message (buffer_name, message_text, content)
  local buf = M.scratch_org_buffer(buffer_name, content)
  M.show_without_stealing_focus_and_notify(buf, message_text)
  return buf
end

---Like big_nonfatal_message, but CONTENT goes after whatever the
---buffer named BUFFER_NAME already holds, erasing none of it.
---@param buffer_name string
---@param message_text string
---@param content string
---@return integer bufnr
function M.big_nonfatal_message_appended (buffer_name, message_text,
                                          content)
  local buf = M.scratch_org_buffer_appended(buffer_name, content)
  M.show_without_stealing_focus_and_notify(buf, message_text)
  return buf
end

---Show BUF in a split, unless a window already shows it, keeping
---the current window current; then echo MESSAGE_TEXT.
---@param buf integer
---@param message_text string
function M.show_without_stealing_focus_and_notify (buf, message_text)
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
  local buf = M.find_or_create_scratch_buffer(name)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(content, '\n'))
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  return buf
end

---Find-or-create a scratch org buffer named NAME, and append CONTENT
---after its last line. A buffer holding only an empty line (as a new
---one does) is replaced rather than appended to, so CONTENT starts
---on line 1.
---@param name string
---@param content string
---@return integer bufnr
function M.scratch_org_buffer_appended (name, content)
  local buf = M.find_or_create_scratch_buffer(name)
  vim.bo[buf].modifiable = true
  local existing_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local first_line_to_replace =
    (#existing_lines == 1 and existing_lines[1] == '')
    and 0 or #existing_lines
  vim.api.nvim_buf_set_lines(buf, first_line_to_replace, -1, false,
                             vim.split(content, '\n'))
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  return buf
end

---The scratch buffer named NAME, created (listed, unbacked by any
---file) if there is none.
---@param name string
---@return integer bufnr
function M.find_or_create_scratch_buffer (name)
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(existing) == name then
      buf = existing break end
  end
  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(buf, name)
  end
  return buf
end

return M
