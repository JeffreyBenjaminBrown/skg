-- PURPOSE: Extract, search for, and visit (in a new view) IDs.
-- The Lua port of the navigation half of elisp/skg-id-search.el (the
-- linkstack half lives in skg.linkstack).
--
-- Positions are (1-based line, 0-based byte column) cursor pairs;
-- matches are byte ranges from Lua patterns.

local content_view = require('skg.content_view')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Lua pattern matching [[id:X][label]] links, with captures.
M.link_pattern = '%[%[id:([^%]]+)%]%[([^%]]+)%]%]'

---TEXT with each [[id:X][LABEL]] replaced by LABEL. Mirrors the
---Rust-side 'replace_each_link_with_its_label'.
---@param text string
---@return string
function M.replace_links_with_labels (text)
  return (text:gsub(M.link_pattern, '%2'))
end

---UUID v4 (any-case hex), as a Lua pattern.
M.uuid_v4_pattern =
  '%x%x%x%x%x%x%x%x%-%x%x%x%x%-4%x%x%x%-[89abAB]%x%x%x%-'
  .. '%x%x%x%x%x%x%x%x%x%x%x%x'

---A .skg filename: valid ID characters ending in '.skg'.
M.skg_filename_pattern = '([%w_%-]+)%.skg'

-- ── what is point on? ──────────────────────────────────────────────

---Scan LINE for PATTERN matches; return the first whose byte range
---contains POS (1-based, inclusive of the end when INCLUSIVE_END).
---@param line string
---@param pattern string
---@param pos integer
---@param inclusive_end boolean|nil
---@return integer|nil start
---@return integer|nil finish
---@return any captures...
function M.match_containing (line, pattern, pos, inclusive_end)
  local search_from = 1
  while true do
    local results = { line:find(pattern, search_from) }
    local start, finish = results[1], results[2]
    if not start then return nil end
    local past_end = inclusive_end and (finish + 1) or finish
    if pos >= start and pos <= past_end then
      return unpack(results) end
    search_from = start + 1
  end
end

---@return table|nil {id, label} when point is in an [[id:..]] link
function M.point_in_link_p ()
  local line = metadata.line_text()
  local pos = vim.api.nvim_win_get_cursor(0)[2] + 1
  local start, _, id, label =
    M.match_containing(line, M.link_pattern, pos, false)
  if start then return { id = id, label = label } end
  return nil
end

---@return table|nil {id, label} when point is on a bare UUID v4
function M.point_on_uuid_p ()
  local line = metadata.line_text()
  local pos = vim.api.nvim_win_get_cursor(0)[2] + 1
  local start, finish =
    M.match_containing(line, M.uuid_v4_pattern, pos, true)
  if start then
    local uuid = line:sub(start, finish)
    return { id = uuid, label = uuid } end
  return nil
end

---@return table|nil {id, label} when point is on a .skg filename
---(the basename is the id)
function M.point_on_skg_filename_p ()
  local line = metadata.line_text()
  local pos = vim.api.nvim_win_get_cursor(0)[2] + 1
  local search_from = 1
  while true do
    local start, finish, basename =
      line:find(M.skg_filename_pattern, search_from)
    if not start then return nil end
    -- Word-boundary check on the right, as the elisp \b demanded.
    local following = line:sub(finish + 1, finish + 1)
    if (following == '' or not following:match('[%w_%-]'))
       and pos >= start and pos <= finish + 1 then
      return { id = basename, label = line:sub(start, finish) } end
    search_from = start + 1
  end
end

---@param sexp any
---@return boolean does SEXP contain an id under a known shape?
function M.metadata_sexp_contains_id_p (sexp)
  return M.extract_id_from_metadata_sexp(sexp) ~= nil
end

---The id from SEXP, accepting the ActiveNode, diff-phantom, and
---Deleted-phantom shapes. Nil when absent.
---@param sexp any
---@return string|nil
function M.extract_id_from_metadata_sexp (sexp)
  for _, holder in ipairs({ 'node', 'diffPhantom', 'deleted' }) do
    local values = metadata.sexp_cdr_at_path(sexp,
                                             { 'skg', holder, 'id' })
    if values and values[1] ~= nil then
      return sexpr.atom_text(values[1]) end
  end
  return nil
end

---The source from SEXP, same shapes as the id extractor.
---@param sexp any
---@return string|nil
function M.extract_source_from_metadata_sexp (sexp)
  for _, holder in ipairs({ 'node', 'diffPhantom', 'deleted' }) do
    local values = metadata.sexp_cdr_at_path(
      sexp, { 'skg', holder, 'source' })
    if values and values[1] ~= nil then
      return sexpr.atom_text(values[1]) end
  end
  return nil
end

---If LINE_NUMBER's line has metadata bearing an id, the 0-based byte
---column of its opening paren; else nil.
---@param line_number integer
---@return integer|nil
function M.metadata_start_on_line (line_number)
  local line = metadata.line_text(line_number)
  local stars = line:match('^(%*+ )%(skg')
  if not stars then return nil end
  local sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
  if sexp and M.metadata_sexp_contains_id_p(sexp) then
    return #stars end
  return nil
end

---@return table|nil {id, label} when point is within id-bearing
---metadata (the label is the headline title)
function M.point_in_metadata_p ()
  local line_number = metadata.current_line_number()
  local start_col = M.metadata_start_on_line(line_number)
  if not start_col then return nil end
  local split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line_number))
  if not split then return nil end
  local pos = vim.api.nvim_win_get_cursor(0)[2] -- 0-based
  local metadata_end = start_col + #split.metadata
  if pos >= start_col and pos < metadata_end then
    local sexp = metadata.metadata_sexp_at_line_or_nil(line_number)
    local id = sexp and M.extract_id_from_metadata_sexp(sexp)
    if id then return { id = id, label = split.title } end
  end
  return nil
end

---@return table|nil {id, label} when point is on any recognized id
function M.id_at_point ()
  return M.point_on_uuid_p()
         or M.point_in_metadata_p()
         or M.point_in_link_p()
         or M.point_on_skg_filename_p()
end

---{id, label} for the nearest id on the current line, or nil: point
---itself first, then the next id (if it stays on this line), then the
---previous. The cursor is always restored.
---@return table|nil
function M.nearest_id ()
  local result = M.id_at_point()
  if result then return result end
  local saved = vim.api.nvim_win_get_cursor(0)
  local line = saved[1]
  M.id_next()
  local after = vim.api.nvim_win_get_cursor(0)
  if (after[1] ~= saved[1] or after[2] ~= saved[2])
     and after[1] == line then
    result = M.id_at_point() end
  vim.api.nvim_win_set_cursor(0, saved)
  if not result then
    M.id_prev()
    if vim.api.nvim_win_get_cursor(0)[1] == line then
      result = M.id_at_point() end
    vim.api.nvim_win_set_cursor(0, saved)
  end
  return result
end

-- ── motion ─────────────────────────────────────────────────────────

---Move point to the next ID occurrence: the opening paren of
---id-bearing (skg ...) metadata, or the opening bracket of an
---[[id:...]] link, whichever comes first.
function M.id_next ()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local last = vim.api.nvim_buf_line_count(0)
  local candidates = {}
  do -- the next link
    for line_number = cursor[1], last do
      local line = metadata.line_text(line_number)
      local from = line_number == cursor[1] and cursor[2] + 2 or 1
      local start = line:find(M.link_pattern, from)
      if start then
        table.insert(candidates, { line_number, start - 1 })
        break end
    end
  end
  do -- the next metadata
    for line_number = cursor[1], last do
      local col = M.metadata_start_on_line(line_number)
      if col and (line_number > cursor[1] or col > cursor[2]) then
        table.insert(candidates, { line_number, col })
        break end
    end
  end
  local best = M.first_position(candidates)
  if best then vim.api.nvim_win_set_cursor(0, best) end
end

---Move point to the previous ID occurrence.
function M.id_prev ()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local candidates = {}
  do -- the previous link
    for line_number = cursor[1], 1, -1 do
      local line = metadata.line_text(line_number)
      local limit = line_number == cursor[1] and cursor[2] or #line
      local found = nil
      local from = 1
      while true do
        local start = line:find(M.link_pattern, from)
        if not start or start - 1 >= limit then break end
        found = start - 1
        from = start + 1
      end
      if found then
        table.insert(candidates, { line_number, found })
        break end
    end
  end
  do -- the previous metadata
    for line_number = cursor[1], 1, -1 do
      local col = M.metadata_start_on_line(line_number)
      if col and (line_number < cursor[1] or col < cursor[2]) then
        table.insert(candidates, { line_number, col })
        break end
    end
  end
  local best = M.last_position(candidates)
  if best then vim.api.nvim_win_set_cursor(0, best) end
end

---@param positions table[] {line, col} pairs
---@return table|nil the earliest
function M.first_position (positions)
  local best = nil
  for _, position in ipairs(positions) do
    if not best or position[1] < best[1]
       or (position[1] == best[1] and position[2] < best[2]) then
      best = position end
  end
  return best
end

---@param positions table[] {line, col} pairs
---@return table|nil the latest
function M.last_position (positions)
  local best = nil
  for _, position in ipairs(positions) do
    if not best or position[1] > best[1]
       or (position[1] == best[1] and position[2] > best[2]) then
      best = position end
  end
  return best
end

-- ── goto ───────────────────────────────────────────────────────────

---Should a goto from the current buffer bypass the override-choice
---menu? True in git-status buffers (a readable-ID jump from git
---should land on the original raw node). Detected by filetype, the
---analog of elisp's major-mode name check for magit.
---@return boolean
function M.bypass_override_here_p ()
  local ft = vim.bo.filetype
  return ft:sub(1, 6) == 'Neogit' or ft:sub(1, 6) == 'neogit'
         or ft == 'git' or ft == 'diff'
end

---Open a content view for the nearest ID on the current line. From a
---git buffer this bypasses the override-choice menu; elsewhere,
---visiting an overridden node offers the menu.
function M.goto_id_near_point ()
  local result = M.nearest_id()
  if result then
    vim.notify('Visiting node: ' .. result.id)
    content_view.request_single_root_content_view_from_id(
      result.id, M.bypass_override_here_p())
  else
    vim.notify('No ID found on this line')
  end
end

---Like goto, but always bypass the override-choice menu: the escape
---hatch to the original node.
function M.goto_bypass_override ()
  local result = M.nearest_id()
  if result then
    vim.notify('Visiting node (bypassing overriders): ' .. result.id)
    content_view.request_single_root_content_view_from_id(
      result.id, true)
  else
    vim.notify('No ID found on this line')
  end
end

---Open a content view for ID, prompting when not given.
---@param id string|nil
function M.goto_by_id (id)
  id = id or vim.fn.input('Node ID: ')
  if id == nil or id == '' then return end
  vim.notify('Visiting node: ' .. id)
  content_view.request_single_root_content_view_from_id(id)
end

---If point is on an [[id:..][label]] link, visit that id.
function M.visit_link ()
  local hit = M.point_in_link_p()
  if hit then
    vim.notify('Visiting node: ' .. hit.id)
    content_view.request_single_root_content_view_from_id(hit.id)
  else
    vim.notify('Point not on a link')
  end
end

---@param goto_fn fun() the goto to run before closing this buffer
local function goto_then_close (goto_fn)
  local buf = vim.api.nvim_get_current_buf()
  goto_fn()
  if vim.api.nvim_buf_is_valid(buf) then
    vim.bo[buf].modified = false
    vim.api.nvim_buf_delete(buf, { force = true })
  end
end

function M.goto_and_close_this ()
  goto_then_close(M.goto_id_near_point)
end

---@param id string|nil
function M.goto_by_id_and_close_this (id)
  goto_then_close(function () M.goto_by_id(id) end)
end

return M
