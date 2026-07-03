-- PURPOSE: The linkstack -- a stack of {id, title} pairs -- and its
-- push / paste / pop commands plus the editable stack buffer.
-- The Lua port of the stack half of elisp/skg-id-search.el.
--
-- Paste/pop work in ordinary buffers (inserting at the cursor) and in
-- command-line prompts (inserting into the command line), the analog
-- of the elisp minibuffer bindings. In a prompt, the link label is
-- taken from the stack without asking, as elisp did under minibufferp.

local id_search = require('skg.id_search')
local metadata = require('skg.metadata')
local state = require('skg.state')

local M = {}

---@return table|nil the {id, title} top of the stack, messaging when
---empty
function M.stack_top_or_message ()
  local top = state.id_stack[1]
  if not top then
    vim.notify('ID stack is empty')
    return nil end
  return top
end

---Push an ID and its title/label onto the stack: an inline link's id
---and label when point is on one, else the headline's metadata id and
---title.
function M.id_push ()
  local link_hit = id_search.point_in_link_p()
  if link_hit then
    table.insert(state.id_stack, 1, { link_hit.id, link_hit.label })
    vim.notify('pushed to stack: ' .. link_hit.label)
    return end
  local split = metadata.split_as_stars_metadata_title(
    metadata.get_current_headline_text())
  if not split or split.metadata == '' then
    vim.notify('No metadata on this line')
    return end
  local sexp = metadata.metadata_sexp_at_line_or_nil(nil)
  local id = sexp and id_search.extract_id_from_metadata_sexp(sexp)
  if not id then
    vim.notify('No ID in metadata on this line')
    return end
  table.insert(state.id_stack, 1, { id, split.title })
  vim.notify('pushed to stack: ' .. split.title)
end

---@return boolean is a command-line prompt active?
function M.in_cmdline_p ()
  return vim.fn.getcmdtype() ~= ''
end

---Insert TEXT at the cursor, or into the active command line.
---A cursor sitting on the line's last character inserts at the line's
---END (normal mode has no past-the-end position, but that is what a
---line-end paste means); anywhere else inserts before the cursor's
---character, as Emacs 'insert' did at point.
---@param text string
function M.insert_text (text)
  if M.in_cmdline_p() then
    local line = vim.fn.getcmdline()
    local pos = vim.fn.getcmdpos()
    vim.fn.setcmdline(
      line:sub(1, pos - 1) .. text .. line:sub(pos),
      pos + #text)
    return
  end
  local cursor = vim.api.nvim_win_get_cursor(0)
  local row = cursor[1] - 1
  local line = vim.api.nvim_buf_get_lines(0, row, row + 1, false)[1]
                or ''
  local offset = cursor[2]
  if #line > 0 and offset >= #line - 1 then offset = #line end
  local lines = vim.split(text, '\n')
  vim.api.nvim_buf_set_text(0, row, offset, row, offset, lines)
  if #lines == 1 then
    vim.api.nvim_win_set_cursor(0,
      { cursor[1], offset + #lines[1] })
  else
    vim.api.nvim_win_set_cursor(0,
      { cursor[1] + #lines - 1, #lines[#lines] })
  end
end

---Insert an org link from ENTRY ({id, title}); in a buffer, prompts
---for the label (defaulting to the title); in a prompt, uses the
---title directly.
---@param entry table
function M.insert_link_from_entry (entry)
  local label = entry[2]
  if not M.in_cmdline_p() then
    local answer = vim.fn.input('Link label: ', entry[2])
    if answer ~= '' then label = answer end
  end
  M.insert_text(string.format('[[id:%s][%s]]', entry[1], label))
end

---Headline stars for inserting a node at point: the current
---headline's level, else one star.
---@return string
function M.org_stars_for_node_insertion ()
  local line = require('skg.focus').owning_headline_line()
  if line then
    return string.rep('*', metadata.outline_level(line) or 1) end
  return '*'
end

---Insert an indefinitive ActiveNode headline from ENTRY. If point is
---already just after headline stars at the start of a line, insert
---only the metadata and title; otherwise insert a full same-level
---headline.
---@param entry table
function M.insert_node_from_entry (entry)
  local node_text = string.format('(skg (node (id %s) indef)) %s',
                                  entry[1], entry[2])
  local line = metadata.line_text()
  local col = vim.api.nvim_win_get_cursor(0)[2]
  local stars = line:match('^(%*+[ \t]*)')
  if stars and col >= #line:match('^%*+')
     and vim.trim(line:sub(#line:match('^%*+') + 1, col)) == '' then
    M.insert_text(node_text)
  else
    M.insert_text(string.format(
      '%s %s\n', M.org_stars_for_node_insertion(), node_text))
  end
end

function M.paste_id ()
  local entry = M.stack_top_or_message()
  if entry then M.insert_text(entry[1]) end
end

function M.paste_link ()
  local entry = M.stack_top_or_message()
  if entry then M.insert_link_from_entry(entry) end
end

function M.paste_node ()
  local entry = M.stack_top_or_message()
  if entry then M.insert_node_from_entry(entry) end
end

function M.pop_id ()
  local entry = M.stack_top_or_message()
  if entry then
    table.remove(state.id_stack, 1)
    M.insert_text(entry[1]) end
end

function M.pop_link ()
  local entry = M.stack_top_or_message()
  if entry then
    table.remove(state.id_stack, 1)
    M.insert_link_from_entry(entry) end
end

function M.pop_node ()
  local entry = M.stack_top_or_message()
  if entry then
    table.remove(state.id_stack, 1)
    M.insert_node_from_entry(entry) end
end

-- ── the editable stack buffer ──────────────────────────────────────

---The stack as org text: each {id, label} becomes a headline (label)
---with the id as its body; the head of the stack is at the top.
---@return string
function M.format_id_stack_as_org ()
  local blocks = {}
  for _, entry in ipairs(state.id_stack) do
    table.insert(blocks,
                 string.format('* %s\n%s', entry[2], entry[1]))
  end
  return table.concat(blocks, '\n')
end

---Validate BUF as an id-stack buffer. Each headline must have a
---non-empty title and exactly one non-blank body line (the id); no
---content may precede the first headline. Returns ok, result-or-error
---where result lists {id, label} pairs in buffer order (first
---headline = head of stack).
---@param buf integer
---@return boolean ok
---@return any
function M.validate_id_stack_buffer (buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local result = {}
  local index = 1
  while index <= #lines and vim.trim(lines[index]) == '' do
    index = index + 1 end
  if index <= #lines and not lines[index]:match('^%*') then
    return false, 'content before first headline' end
  while index <= #lines do
    local line = lines[index]
    local label = line:match('^%*+ +([^%s].*)$')
    if label then
      local body_lines = {}
      index = index + 1
      while index <= #lines and not lines[index]:match('^%*') do
        if vim.trim(lines[index]) ~= '' then
          table.insert(body_lines, vim.trim(lines[index])) end
        index = index + 1
      end
      if #body_lines ~= 1 then
        return false, string.format(
          "headline '%s' must have exactly one body line", label) end
      table.insert(result, { body_lines[1], label })
    elseif line:match('^%*') then
      return false, 'headline with empty title'
    else
      index = index + 1 end
  end
  return true, result
end

---Replace the stack from BUF's contents, or report why not.
---@param buf integer
function M.save_id_stack_buffer (buf)
  local ok, result = M.validate_id_stack_buffer(buf)
  if ok then
    state.id_stack = result
    vim.bo[buf].modified = false
    vim.notify(string.format('ID stack updated (%d items)', #result))
  else
    vim.notify('Invalid ID buffer: ' .. tostring(result))
  end
end

---Open a buffer to view and edit the stack; ':w' validates and
---updates it without touching disk.
function M.view_id_stack ()
  local name = 'skg://id-stack'
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(existing)
       and vim.api.nvim_buf_get_name(existing) == name then
      buf = existing break end
  end
  if not buf then
    buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, name)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
    vim.split(M.format_id_stack_as_org(), '\n'))
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  if not vim.b[buf].skg_id_stack_autocmd then
    vim.b[buf].skg_id_stack_autocmd = true
    vim.api.nvim_create_autocmd('BufWriteCmd', {
      buffer = buf,
      callback = function () M.save_id_stack_buffer(buf) end })
  end
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  vim.notify('Edit ID stack. :w to save changes.')
end

return M
