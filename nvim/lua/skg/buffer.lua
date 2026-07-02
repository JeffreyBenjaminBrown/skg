-- PURPOSE: skg view buffers: creation from server text, naming, the
-- view-uri registry, close-view notification, and the
-- unsaved-elsewhere warning. The Lua port of elisp/skg-buffer.el.
--
-- Where Emacs used a derived major mode plus a permanent-local
-- 'skg-view-uri', an skg view buffer here is: filetype org (so the
-- baked-in orgmode plugin provides highlighting/folding/cycling),
-- buftype acwrite (so ':w' fires our BufWriteCmd instead of writing a
-- file -- there is no file), 'vim.b.skg_view_uri' as the registry
-- key, and 'vim.b.skg_content_view' marking the buffer as ours. The
-- permanent-local hazard (mode switches wiping the uri) does not
-- exist: vim.b survives filetype changes.

local heralds = require('skg.heralds')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

-- ── naming ─────────────────────────────────────────────────────────

---Buffer name for a content view of ORG_TEXT; errors if ORG_TEXT has
---no headline. (Elisp wrapped names in asterisks; here the skg://
---scheme plays that role and keeps nvim from treating the name as a
---file path.)
---@param org_text string
---@return string
function M.content_view_buffer_name (org_text)
  local title = M.extract_top_headline_title(org_text)
  if not title then
    error(string.format(
      'skg: content view has no headline (first 200 chars: %s)',
      (org_text or ''):sub(1, 200))) end
  return 'skg://'
         .. M.sanitize_buffer_name(M.normalize_buffer_name_links(title))
end

---@param search_terms string
---@return string buffer name for a title search
function M.search_buffer_name (search_terms)
  return 'skg://?' .. M.sanitize_buffer_name(search_terms)
end

---NAME with org id links shortened for display: every
---[[id:ID][LABEL]] becomes [[LABEL]].
---@param name string
---@return string
function M.normalize_buffer_name_links (name)
  return (name:gsub('%[%[id:[^%]]+%]%[([^%]]+)%]%]', '[[%1]]'))
end

---The title of the first headline in ORG_TEXT, with any leading
---(skg ...) metadata stripped; nil if there is no headline.
---@param org_text string|nil
---@return string|nil
function M.extract_top_headline_title (org_text)
  if not org_text then return nil end
  local after_stars = org_text:match('^%*+ +(.-)[\r]?\n')
                      or org_text:match('^%*+ +(.+)$')
  if not after_stars then
    after_stars = org_text:match('\n%*+ +(.-)[\r]?\n')
                  or org_text:match('\n%*+ +(.+)$') end
  if not after_stars then return nil end
  if after_stars:sub(1, 4) == '(skg' then
    local sexp_end = sexpr.find_sexp_end(after_stars)
    if sexp_end then
      return vim.trim(after_stars:sub(sexp_end + 1)) end
  end
  return after_stars
end

---NAME sanitized for use as a buffer name: no nulls or newlines,
---trimmed, truncated to a reasonable length.
---@param name string
---@return string
function M.sanitize_buffer_name (name)
  local cleaned = name:gsub('%z', ''):gsub('[\n\r]', ' ')
  cleaned = vim.trim(cleaned)
  local max_len = 80
  if #cleaned > max_len then
    return cleaned:sub(1, max_len - 3) .. '...' end
  return cleaned
end

-- ── the registry ───────────────────────────────────────────────────

---The buffer whose skg_view_uri is URI, or nil.
---@param uri string
---@return integer|nil bufnr
function M.find_buffer_by_uri (uri)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.b[buf].skg_view_uri == uri then
      return buf end
  end
  return nil
end

---Is BUF an skg view buffer? It qualifies if it carries a view uri or
---the content-view marker, both set solely by skg's own view code --
---so a real .skg file the user merely opened never matches (and is
---never reaped by close_all_skg_buffers).
---@param buf integer
---@return boolean
function M.buffer_p (buf)
  return vim.api.nvim_buf_is_valid(buf)
         and (vim.b[buf].skg_view_uri ~= nil
              or vim.b[buf].skg_content_view == true)
end

---@return integer[] all skg view buffers
function M.all_skg_buffers ()
  local result = {}
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if M.buffer_p(buf) then table.insert(result, buf) end
  end
  return result
end

-- ── lifecycle ──────────────────────────────────────────────────────

---Find-or-create the buffer named BUFFER_NAME, fill it with ORG_TEXT,
---mark it as an skg view (uri = VIEW_URI or a fresh UUID), enable
---heralds, arm the lifecycle autocmds, clear the modified flag, and
---switch to it. The port of 'skg-open-org-buffer-from-text'.
---@param org_text string
---@param buffer_name string
---@param view_uri string|nil
---@return integer bufnr
function M.open_org_buffer_from_text (org_text, buffer_name, view_uri)
  local uri = view_uri or M.generate_uuid()
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(existing)
       and vim.api.nvim_buf_get_name(existing) == buffer_name then
      buf = existing break end
  end
  if not buf then
    buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, buffer_name)
  end
  vim.bo[buf].modifiable = true
  M.disarm_first_change_warning(buf) -- this rewrite is not a user edit
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(org_text, '\n'))
  M.configure_view_buffer(buf, uri)
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  return buf
end

---Buffer-local setup shared by every server-fed view.
---@param buf integer
---@param uri string
function M.configure_view_buffer (buf, uri)
  vim.b[buf].skg_view_uri = uri
  vim.b[buf].skg_content_view = true
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = 'hide'
  vim.bo[buf].filetype = 'org'
  -- The server owns this text's shape; never let an indent expression
  -- reflow it (the analog of setting org-adapt-indentation to nil).
  vim.bo[buf].indentexpr = ''
  vim.bo[buf].autoindent = false
  heralds.enable(buf)
  require('skg.keymaps').attach_content_view(buf)
  M.arm_first_change_warning(buf)
  do -- The skg fold model, on every window that shows this buffer.
    local folds = require('skg.folds')
    for _, win in ipairs(vim.api.nvim_list_wins()) do
      if vim.api.nvim_win_get_buf(win) == buf then
        folds.set_up_window(win) end
    end
  end
  if not vim.b[buf].skg_lifecycle_autocmds then
    vim.b[buf].skg_lifecycle_autocmds = true
    vim.api.nvim_create_autocmd('BufWinEnter', {
      buffer = buf,
      callback = function ()
        require('skg.folds').set_up_window(
          vim.api.nvim_get_current_win())
      end })
    vim.api.nvim_create_autocmd({ 'BufDelete', 'BufWipeout' }, {
      buffer = buf,
      callback = function () M.send_close_view(buf) end })
    vim.api.nvim_create_autocmd('BufWriteCmd', {
      buffer = buf,
      callback = function ()
        local ok, save = pcall(require, 'skg.save')
        if ok then save.request_save_buffer()
        else vim.notify('skg: save is not available: '
                        .. tostring(save)) end
      end })
  end
end

---Warn (once per redraw) if another skg buffer has unsaved edits when
---this one is first modified -- saving is ill-defined when multiple
---buffers have unsaved edits. The analog of first-change-hook, built
---on nvim_buf_attach because the TextChanged/BufModifiedSet autocmds
---do not fire for API-driven edits. Server-driven redraws disarm
---first (see disarm_first_change_warning) so only user edits warn.
---@param buf integer
function M.arm_first_change_warning (buf)
  vim.b[buf].skg_first_change_armed = true
  if vim.b[buf].skg_first_change_attached then return end
  vim.b[buf].skg_first_change_attached = true
  vim.api.nvim_buf_attach(buf, false, {
    on_lines = function ()
      if not vim.api.nvim_buf_is_valid(buf) then return true end
      if vim.b[buf].skg_first_change_armed then
        vim.b[buf].skg_first_change_armed = false
        vim.schedule(function ()
          M.warn_if_other_buffer_modified(buf) end)
      end
    end })
end

---Suppress the first-change warning while skg itself rewrites BUF.
---Callers re-arm via arm_first_change_warning after the rewrite.
---@param buf integer
function M.disarm_first_change_warning (buf)
  vim.b[buf].skg_first_change_armed = false
end

---@param buf integer the buffer being edited
function M.warn_if_other_buffer_modified (buf)
  for _, other in ipairs(vim.api.nvim_list_bufs()) do
    if other ~= buf and vim.api.nvim_buf_is_valid(other)
       and vim.b[other].skg_view_uri ~= nil
       and vim.bo[other].modified then
      vim.notify(
        'WARNING: Another skg buffer has unsaved modifications.'
        .. ' Saving is ill-defined when multiple buffers have'
        .. ' unsaved edits.')
      return end
  end
end

---Tell the server to drop BUF's view. Fire-and-forget; a dead
---connection makes it a no-op.
---@param buf integer
function M.send_close_view (buf)
  local uri = vim.api.nvim_buf_is_valid(buf)
              and vim.b[buf].skg_view_uri or nil
  if uri and state.tcp and not state.tcp:is_closing() then
    local request = sexpr.to_string({
      sexpr.pair(sexpr.symbol('request'), 'close view'),
      sexpr.pair(sexpr.symbol('view-uri'), uri) }) .. '\n'
    pcall(function () state.tcp:write(request) end)
  end
end

---Kill all skg view buffers (see buffer_p for what counts), clearing
---each one's modified flag first -- stale views are not worth saving.
---Each kill triggers send_close_view via the lifecycle autocmd.
function M.close_all_skg_buffers ()
  local bufs = M.all_skg_buffers()
  for _, buf in ipairs(bufs) do
    vim.bo[buf].modified = false
    vim.api.nvim_buf_delete(buf, { force = true })
  end
  vim.notify(string.format('Closed %d skg buffer(s).', #bufs))
end

---A v4-format UUID, the analog of org-id-uuid. Built from OS-level
---randomness (an unseeded math.random would repeat across editor
---processes, and view uris must not collide between clients).
---@return string
function M.generate_uuid ()
  local bytes = { vim.uv.random(16):byte(1, 16) }
  bytes[7] = bytes[7] % 16 + 64  -- version 4
  bytes[9] = bytes[9] % 64 + 128 -- variant 10
  local hex = {}
  for _, byte in ipairs(bytes) do
    table.insert(hex, string.format('%02x', byte)) end
  local s = table.concat(hex)
  return string.format('%s-%s-%s-%s-%s',
    s:sub(1, 8), s:sub(9, 12), s:sub(13, 16), s:sub(17, 20),
    s:sub(21, 32))
end

return M
