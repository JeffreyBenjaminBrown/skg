-- PURPOSE: Say what a forks-folder member restates of its note, in
-- the member's title, b being the note's ancestor letter as counted
-- from the member (its grandparent, past the folder). Where the
-- member's title and body both equal the note's, extmarks show
-- "<b unchanged>" in place of the title and conceal the lines under
-- it, up to the member's next headline; where the title alone does,
-- "<b's title unchanged>"; where the body alone does, the title is
-- shown and "<b's body unchanged>" stands in place of the body. Two
-- empty bodies are equal; a body the view does not draw (a
-- write-protected node's 'omittedBody') equals none. The buffer keeps
-- the real text, which a save sends as it is.
-- 'vim.b.skg_fork_dedup_overlay' (true unless set false) turns the
-- covers on or off per buffer; 'toggle' (':SkgToggleForkDedupOverlay',
-- '<localleader>vu') flips it. This module draws only when called:
-- skg.heralds calls 'refresh_buffer' when it draws a buffer and
-- 'refresh_region' from its edit watcher, and skg.redraw_delta calls
-- 'refresh_rows', so an edit that makes the two differ removes the
-- cover; turning heralds off ('skg.heralds.disable') clears the
-- covers too. The Lua port of elisp/skg-fork-dedup.el.

local M = {}

local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

M.namespace = vim.api.nvim_create_namespace('skg_fork_dedup')

---The ancestor letter of a forks-folder member's note: its
---grandparent, the folder its parent.
M.note_letter = 'b'

---@param buf integer
---@return boolean
function M.enabled_p (buf)
  return vim.b[buf].skg_fork_dedup_overlay ~= false
end

---Toggle the covers in BUF.
---@param buf integer
function M.toggle (buf)
  buf = (buf and buf ~= 0) and buf or vim.api.nvim_get_current_buf()
  vim.b[buf].skg_fork_dedup_overlay = not M.enabled_p(buf)
  M.refresh_buffer(buf)
  vim.notify('Restated titles and bodies are '
             .. (M.enabled_p(buf) and 'covered.' or 'shown.'))
end

---@param line string
---@return boolean is LINE a forks folder's headline?
local function forks_folder_p (line)
  if not (line:find('forksFolder', 1, true) and line:match('^%*+%s')) then
    return false end
  return metadata.folder_atom(metadata.metadata_on_line(line))
         == 'forksFolder'
end

---@param line string|nil
---@return boolean is LINE a headline?
local function headline_p (line)
  return line ~= nil and line:match('^%*+%s') ~= nil
end

---For the node headline LINES[INDEX]: its title and where it starts
---(0-based column); its body, trailing blanks dropped, '' when blank,
---and the indices of its first and last lines, nil when blank;
---'under_last', the index of the last line before the next headline;
---and 'body_known', false when the view omits a body the node has
---('omittedBody'). Nil when the headline is no node's.
---@return table|nil
local function entry (lines, index)
  local line = lines[index]
  local split = metadata.split_as_stars_metadata_title(line)
  if not split or split.metadata == '' then return nil end
  local ok, sexp = pcall(sexpr.read, split.metadata)
  if not (ok and metadata.occurrence_sexp_p(sexp)) then return nil end
  local under_last = index
  while lines[under_last + 1] and not headline_p(lines[under_last + 1]) do
    under_last = under_last + 1 end
  local last = under_last
  while last > index and lines[last]:match('^%s*$') do last = last - 1 end
  local result = { title = split.title, title_col = #line - #split.title,
                   body = '', under_last = under_last,
                   body_known = not metadata.omitted_body_p(sexp) }
  if last > index then
    result.body_first = index + 1
    result.body_last = last
    result.body = table.concat(lines, '\n', index + 1, last)
  end
  return result
end

---Clear the covers on LINES[FIRST..LAST] (1-based, inclusive).
local function clear_rows (buf, first, last)
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, first - 1, last)
end

---Show MESSAGE in place of the title of MEMBER, the headline LINES[INDEX].
local function cover_title (buf, index, member, message)
  vim.api.nvim_buf_set_extmark(buf, M.namespace, index - 1, member.title_col, {
    end_col = member.title_col + #member.title,
    conceal = '',
    virt_text = { { message, 'SkgHeraldMessage' } },
    virt_text_pos = 'inline' })
end

---Conceal LINES[FIRST..LAST] (1-based, inclusive), when there are any.
local function conceal_lines (buf, first, last)
  if last < first then return end
  -- 'conceal_lines' hides every row from the mark's to 'end_row',
  -- both included.
  vim.api.nvim_buf_set_extmark(buf, M.namespace, first - 1, 0, {
    end_row = last - 1, end_col = 0,
    conceal_lines = '' })
end

local function cover_body (buf, lines, member)
  local first = member.body_first
  vim.api.nvim_buf_set_extmark(buf, M.namespace, first - 1, 0, {
    end_col = #lines[first],
    conceal = '',
    virt_text = { { string.format("<%s's body unchanged>", M.note_letter),
                    'SkgHeraldMessage' } },
    virt_text_pos = 'inline' })
  conceal_lines(buf, first + 1, member.body_last)
end

---Draw what MEMBER, the headline LINES[INDEX], restates of NOTE, each
---an 'entry': with its title and body, "<b unchanged>" in place of its
---title, the lines under it concealed; with its title alone, "<b's
---title unchanged>"; with its nonblank body alone, "<b's body
---unchanged>" in place of the body. A blank title restates nothing.
local function cover_member (buf, lines, index, member, note)
  local same_title = member.title:match('%S') ~= nil
                     and member.title == note.title
  local same_body = member.body_known and note.body_known
                    and member.body == note.body
  if same_title and same_body then
    cover_title(buf, index, member,
                string.format('<%s unchanged>', M.note_letter))
    conceal_lines(buf, index + 1, member.under_last)
  elseif same_title then
    cover_title(buf, index, member,
                string.format("<%s's title unchanged>", M.note_letter))
  elseif same_body and member.body_first then
    cover_body(buf, lines, member)
  end
end

---Redraw the covers of the members of the forks folder LINES[FOLDER].
local function draw_folder (buf, lines, folder)
  local level = metadata.headline_level_on_line(lines[folder])
  local note_index =
    metadata.ancestor_headline_indices_in_lines(lines, folder, 1)[1]
  local note = note_index and entry(lines, note_index)
  local index = folder + 1
  while lines[index] do
    local member_level = metadata.headline_level_on_line(lines[index])
    if member_level and member_level <= level then break end
    if member_level == level + 1 then
      local next_headline = index + 1
      while lines[next_headline] and not headline_p(lines[next_headline]) do
        next_headline = next_headline + 1 end
      clear_rows(buf, index, next_headline - 1)
      local member = note and entry(lines, index)
      if member then cover_member(buf, lines, index, member, note) end
    end
    index = index + 1
  end
end

---Remove every cover in BUF, then, when enabled, draw those of each
---forks folder.
---@param buf integer
---@param lines string[]|nil BUF's lines
function M.refresh_buffer (buf, lines)
  vim.api.nvim_buf_clear_namespace(buf, M.namespace, 0, -1)
  if not M.enabled_p(buf) then return end
  lines = lines or vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if forks_folder_p(line) then draw_folder(buf, lines, index) end
  end
end

---Redraw the covers an edit of the 0-based rows FIRST..LAST of BUF can
---change. Within one headline's lines, those are the forks folders
---around that headline ('refresh_rows'). Rows holding more than one
---headline, which a paste or a redraw of the whole view writes, can
---hold any part of any forks folder, so all of them are redrawn
---('refresh_buffer').
---@param buf integer
---@param first integer
---@param last integer
---@param lines string[]|nil BUF's lines
function M.refresh_region (buf, first, last, lines)
  if not M.enabled_p(buf) then return end
  lines = lines or vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index = first + 2, math.min(last + 1, #lines) do
    if headline_p(lines[index]) then
      return M.refresh_buffer(buf, lines) end
  end
  M.refresh_rows(buf, { first }, lines)
end

---The forks folder among the children of the headline LINES[INDEX].
local function forks_folder_child (lines, index)
  local level = metadata.headline_level_on_line(lines[index])
  if not level then return nil end
  for child = index + 1, #lines do
    local child_level = lines[child]:byte(1) == 42 -- '*'
                        and metadata.headline_level_on_line(lines[child])
    if child_level then
      if child_level <= level then return nil end
      if child_level == level + 1 and forks_folder_p(lines[child]) then
        return child end
    end
  end
  return nil
end

---Redraw the covers an edit on each of ROWS (0-based) can change: those
---of the forks folder holding the headline the row falls in, and of
---the forks folder under that headline, whose note it is.
---@param buf integer
---@param rows integer[]
---@param lines string[]|nil BUF's lines
function M.refresh_rows (buf, rows, lines)
  if not M.enabled_p(buf) then return end
  lines = lines or vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local folders = {}
  for _, row in ipairs(rows) do
    local index = math.min(row + 1, #lines)
    while index >= 1 and not headline_p(lines[index]) do
      index = index - 1 end
    if index >= 1 then
      if forks_folder_p(lines[index]) then folders[index] = true end
      local parent =
        metadata.ancestor_headline_indices_in_lines(lines, index, 1)[1]
      if parent and forks_folder_p(lines[parent]) then
        folders[parent] = true end
      local child = forks_folder_child(lines, index)
      if child then folders[child] = true end
    end
  end
  for folder in pairs(folders) do draw_folder(buf, lines, folder) end
end

return M
