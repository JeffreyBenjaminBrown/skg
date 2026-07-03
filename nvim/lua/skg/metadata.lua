-- PURPOSE: Utilities to parse and edit skg headline metadata, plus
-- the user commands that reduce to metadata edits (delete,
-- set-indefinitive, set-source, merge requests, ...).
-- The Lua port of elisp/skg-metadata.el.
--
-- The elisp file's biggest hazard -- org-fold's fragility check
-- crashing on programmatic edits to folded heading lines, worked
-- around by 'skg-replace-current-line' -- does not exist here:
-- replacing a line with nvim_buf_set_lines is always safe, and vim
-- folds recompute from the foldexpr. The helper survives as
-- replace_current_line for shape parity.
--
-- Cursor-and-line conventions: like the elisp, the commands operate
-- on the current buffer around the cursor. Lines are 1-based.

local compare = require('skg.sexpr.compare')
local edit_dsl = require('skg.sexpr.edit_dsl')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')

local M = {}

local SKG = sexpr.symbol('skg')
local NODE = sexpr.symbol('node')

-- ── line-level primitives ──────────────────────────────────────────

---@return integer the cursor's 1-based line number
function M.current_line_number ()
  return vim.api.nvim_win_get_cursor(0)[1]
end

---@param line_number integer|nil defaults to the cursor line
---@return string that line's text
function M.line_text (line_number)
  local row = (line_number or M.current_line_number()) - 1
  return vim.api.nvim_buf_get_lines(0, row, row + 1, false)[1] or ''
end

---@param line_number integer|nil defaults to the cursor line
---@return boolean is that line an org headline?
function M.at_heading_p (line_number)
  return M.line_text(line_number):match('^%*+%s') ~= nil
end

---@param line_number integer|nil
---@return integer|nil the headline's level (star count), or nil
function M.outline_level (line_number)
  local stars = M.line_text(line_number):match('^(%*+)%s')
  return stars and #stars or nil
end

---The current headline in its entirety, including asterisks and
---metadata, without the trailing newline. Assumes point is on a
---headline; does not move point.
---@return string
function M.get_current_headline_text ()
  return M.line_text()
end

---Replace the current line with NEW_CONTENT.
---@param new_content string
function M.replace_current_line (new_content)
  M.replace_line(M.current_line_number(), new_content)
end

---@param line_number integer
---@param new_content string
function M.replace_line (line_number, new_content)
  vim.api.nvim_buf_set_lines(0, line_number - 1, line_number, false,
                             { new_content })
end

---The 1-based line of the next headline after LINE_NUMBER, or nil.
---The stand-in for outline-next-heading.
---@param line_number integer
---@return integer|nil
function M.next_heading_line (line_number)
  local last = vim.api.nvim_buf_line_count(0)
  for candidate = line_number + 1, last do
    if M.at_heading_p(candidate) then return candidate end
  end
  return nil
end

---The 1-based line of the nearest headline at or above LINE_NUMBER
---whose level is below LEVEL, or nil. The stand-in for
---org-up-heading-safe.
---@param line_number integer
---@param level integer
---@return integer|nil
function M.parent_heading_line (line_number, level)
  for candidate = line_number - 1, 1, -1 do
    local candidate_level = M.outline_level(candidate)
    if candidate_level and candidate_level < level then
      return candidate end
  end
  return nil
end

---The line after the subtree rooted at LINE_NUMBER (exclusive scan by
---level), or nil at buffer end. The stand-in for
---skg--goto-next-heading-after-subtree.
---@param line_number integer
---@return integer|nil
function M.next_heading_after_subtree (line_number)
  local prune_level = M.outline_level(line_number)
  local candidate = M.next_heading_line(line_number)
  while candidate
        and (M.outline_level(candidate) or 0) > prune_level do
    candidate = M.next_heading_line(candidate)
  end
  return candidate
end

-- ── headline splitting and reconstruction ──────────────────────────

---Split HEADLINE_TEXT into { stars, metadata, title }, where metadata
---is the complete (skg ...) sexp text or '' when absent. Returns nil
---when HEADLINE_TEXT is not a headline. Handles nested parens.
---@param headline_text string
---@return table|nil
function M.split_as_stars_metadata_title (headline_text)
  local trimmed = headline_text:gsub('^%s+', '')
  local stars = trimmed:match('^(%*+%s+)')
  if not stars then return nil end
  local after_stars = trimmed:sub(#stars + 1)
  if after_stars:sub(1, 4) == '(skg' then
    local sexp_end = sexpr.find_sexp_end(after_stars)
    if not sexp_end then return nil end
    return { stars = stars,
             metadata = after_stars:sub(1, sexp_end),
             title = vim.trim(after_stars:sub(sexp_end + 1)) }
  end
  return { stars = stars, metadata = '', title = after_stars }
end

---Format a headline from STARS, METADATA (a complete (skg ...) sexp
---text, or ''), and TITLE.
---@param stars string
---@param metadata string
---@param title string
---@return string
function M.format_headline (stars, metadata, title)
  if metadata == '' then
    return string.format('%s(skg) %s', stars, title) end
  return string.format('%s%s %s', stars, metadata, title)
end

---Parse METADATA_TEXT into { alist = {{key, value}...},
---bare_values = {...} }, all rendered as strings. graphStats and
---viewStats keep their whole sub-sexp as the value. Tolerant: parse
---failures give empty results, like the elisp condition-case.
---@param metadata_text string
---@return table
function M.parse_metadata_sexp (metadata_text)
  local alist = {}
  local bare_values = {}
  if metadata_text ~= '' then
    local ok, sexp = pcall(sexpr.read, metadata_text)
    if ok and sexpr.is_list(sexp) then
      for i = 2, #sexp do
        local element = sexp[i]
        if sexpr.is_list(element) and #element == 2 then
          table.insert(alist,
            { key = sexpr.atom_text(element[1]),
              value = sexpr.is_list(element[2])
                      and sexpr.to_string(element[2])
                      or sexpr.atom_text(element[2]) })
        elseif sexpr.is_list(element) and #element > 1
               and (element[1] == sexpr.symbol('graphStats')
                    or element[1] == sexpr.symbol('viewStats')) then
          table.insert(alist,
            { key = sexpr.atom_text(element[1]),
              value = sexpr.to_string(element) })
        elseif not sexpr.is_list(element) then
          table.insert(bare_values, sexpr.atom_text(element))
        else
          table.insert(alist,
            { key = sexpr.atom_text(element[1]),
              value = sexpr.to_string(element) })
        end
      end
    end
  end
  return { alist = alist, bare_values = bare_values }
end

---Reconstruct complete (skg ...) metadata text from PARSED (the
---shape parse_metadata_sexp returns). graphStats/viewStats values are
---already complete sexps and are spliced whole.
---@param parsed table
---@return string
function M.reconstruct_metadata_sexp (parsed)
  local parts = {}
  for _, kv in ipairs(parsed.alist) do
    if kv.key == 'graphStats' or kv.key == 'viewStats'
       or kv.value:sub(1, 1) == '(' and kv.value:sub(1, 1) == '('
          and kv.value:match('^%(' .. kv.key) then
      table.insert(parts, kv.value)
    else
      table.insert(parts, string.format('(%s %s)', kv.key, kv.value))
    end
  end
  for _, value in ipairs(parsed.bare_values) do
    table.insert(parts, value)
  end
  if #parts == 0 then return '(skg)' end
  return '(skg ' .. table.concat(parts, ' ') .. ')'
end

-- ── metadata reading helpers ───────────────────────────────────────

---The parsed metadata sexp of the headline at LINE_NUMBER, or nil.
---@param line_number integer|nil
---@return any|nil
function M.metadata_sexp_at_line_or_nil (line_number)
  if not M.at_heading_p(line_number) then return nil end
  local split = M.split_as_stars_metadata_title(
    M.line_text(line_number))
  if not split or split.metadata == '' then return nil end
  local ok, sexp = pcall(sexpr.read, split.metadata)
  return ok and sexp or nil
end

---The parsed metadata of the headline at point; errors if absent.
---@return any
function M.current_headline_metadata_sexp ()
  if not M.at_heading_p() then error('Not on a headline') end
  local sexp = M.metadata_sexp_at_line_or_nil(nil)
  if not sexp then error('Headline has no skg metadata') end
  return sexp
end

---@param metadata_sexp any
---@return boolean does METADATA_SEXP describe an ActiveNode?
function M.activeNode_sexp_p (metadata_sexp)
  return metadata_sexp ~= nil
         and compare.subtree_p(metadata_sexp, { SKG, { NODE } })
end

---The values after LABEL-PATH in SEXP, as in skg-sexp-cdr-at-path.
---@param sexp any
---@param path string[] label names
---@return any[]|nil
function M.sexp_cdr_at_path (sexp, path)
  if not sexpr.is_list(sexp) or #sexp == 0
     or sexpr.is_list(sexp[1])
     or sexpr.atom_text(sexp[1]) ~= path[1] then
    return nil end
  if #path == 1 then
    local rest = {}
    for i = 2, #sexp do table.insert(rest, sexp[i]) end
    return rest end
  local subpath = {}
  for i = 2, #path do table.insert(subpath, path[i]) end
  for i = 2, #sexp do
    local element = sexp[i]
    if sexpr.is_list(element) then
      local found = M.sexp_cdr_at_path(element, subpath)
      if found then return found end
    end
  end
  return nil
end

---@param metadata_sexp any
---@return string|nil the node's source
function M.node_source (metadata_sexp)
  local values = M.sexp_cdr_at_path(metadata_sexp,
                                    { 'skg', 'node', 'source' })
  if values and values[1] ~= nil then
    return sexpr.atom_text(values[1]) end
  return nil
end

---@param metadata_sexp any
---@return string|nil the node's id
function M.node_id (metadata_sexp)
  local values = M.sexp_cdr_at_path(metadata_sexp,
                                    { 'skg', 'node', 'id' })
  if values and values[1] ~= nil then
    return sexpr.atom_text(values[1]) end
  return nil
end

---@param metadata_sexp any
---@return boolean implicit or explicit parentIs=affected?
function M.node_parentIs_content_of_p (metadata_sexp)
  local values = M.sexp_cdr_at_path(metadata_sexp,
                                    { 'skg', 'node', 'parentIs' })
  return values == nil
         or values[1] == sexpr.symbol('affected')
end

---@param metadata_sexp any
---@return boolean does it carry the bare indef marker?
function M.node_indefinitive_p (metadata_sexp)
  return compare.subtree_p(metadata_sexp,
    { SKG, { NODE, sexpr.symbol('indef') } })
end

---@return boolean has the headline at point no skg metadata?
function M.headline_metadata_empty_p ()
  local split = M.split_as_stars_metadata_title(
    M.get_current_headline_text())
  return split == nil or split.metadata == ''
end

-- ── the central edit primitive ─────────────────────────────────────

---Use EDITS (a sexp for the edit DSL) to edit the metadata of the
---headline at point: merged into existing metadata, or created fresh
---when there is none. No effect off a headline.
---@param edits any
function M.edit_metadata_at_point (edits)
  M.edit_metadata_at_line(M.current_line_number(), edits)
end

---@param line_number integer
---@param edits any
function M.edit_metadata_at_line (line_number, edits)
  if not M.at_heading_p(line_number) then return end
  local headline_text = M.line_text(line_number)
  local split = M.split_as_stars_metadata_title(headline_text)
  if split and split.metadata ~= '' then
    local host_sexp = sexpr.read(split.metadata)
    local merged = edit_dsl.edit_nested_sexp(host_sexp, edits)
    M.replace_line(line_number,
      M.format_headline(split.stars, sexpr.to_string(merged),
                        split.title))
  else
    local stars, title = headline_text:match('^(%*+%s+)(.*)$')
    if stars then
      M.replace_line(line_number,
        M.format_headline(stars, sexpr.to_string(edits), title))
    end
  end
end

-- ── user commands ──────────────────────────────────────────────────

---Mark the headline at point for deletion (metadata gains
---(editRequest delete)). Does NOT save.
function M.delete ()
  M.edit_metadata_at_point(
    sexpr.read('(skg (node (editRequest delete)))'))
  vim.notify(
    'This change will only be applied when you save the buffer.')
end

---Mark the headline at point, and every activeNode org-descendant,
---for deletion. Non-activeNode descendants (phantoms, cols, ...) are
---skipped. Does NOT save.
function M.delete_recursive ()
  if not M.at_heading_p() then error('Not on a headline') end
  local edits = sexpr.read('(skg (node (editRequest delete)))')
  local start_line = M.current_line_number()
  local start_level = M.outline_level(start_line)
  M.edit_metadata_at_line(start_line, edits)
  local line = M.next_heading_line(start_line)
  while line and (M.outline_level(line) or 0) > start_level do
    local meta = M.metadata_sexp_at_line_or_nil(line)
    if meta and M.activeNode_sexp_p(meta) then
      M.edit_metadata_at_line(line, edits) end
    line = M.next_heading_line(line)
  end
  vim.notify(
    'This change will only be applied when you save the buffer.')
end

---Mark the headline at point as indefinitive. Does NOT save.
function M.set_indefinitive ()
  M.edit_metadata_at_point(sexpr.read('(skg (node indef))'))
end

---Copy the visually-selected region to a new org buffer, stripping
---skg metadata. Without a selection, does nothing.
function M.view_without_metadata ()
  local start_line = vim.fn.line('v')
  local end_line = vim.fn.line('.')
  local mode = vim.fn.mode()
  if not (mode == 'v' or mode == 'V' or mode == '\022') then
    -- Also accept a just-left visual selection via the marks.
    start_line = vim.fn.line("'<")
    end_line = vim.fn.line("'>")
    if start_line == 0 or end_line == 0 then return end
  end
  if start_line > end_line then
    start_line, end_line = end_line, start_line end
  local text = table.concat(
    vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false),
    '\n')
  local stripped = M.strip_metadata_from_org_text(text)
  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                             vim.split(stripped, '\n'))
  vim.bo[buf].filetype = 'org'
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
end

---ORG_TEXT with all skg headline metadata removed.
---@param org_text string
---@return string
function M.strip_metadata_from_org_text (org_text)
  local lines = {}
  for _, line in ipairs(vim.split(org_text, '\n')) do
    local split = M.split_as_stars_metadata_title(line)
    if split and split.metadata ~= '' then
      table.insert(lines, split.stars .. split.title)
    else
      table.insert(lines, line) end
  end
  return table.concat(lines, '\n')
end

---Write minimal ActiveNode metadata onto the metadata-less headline
---at point, prompting for an owned source (no prompt when only one).
---@return string|nil the chosen source
function M.populate_minimal_node_metadata ()
  local source = picker.prompt_for_owned_source()
  if not source then return nil end
  M.edit_metadata_at_point(
    sexpr.read(string.format('(skg (node (source %s)))', source)))
  return source
end

---Prompt for and change the source of the node at point (S-arrows
---cycle owned sources; typed names accepted). With RECURSIVE, also
---changes every affected content descendant whose source matches. On
---a metadata-less headline, populates it minimally instead. Does NOT
---save.
---@param recursive boolean|nil
function M.set_source (recursive)
  if M.headline_metadata_empty_p() then
    M.populate_minimal_node_metadata()
    return end
  local metadata = M.current_headline_metadata_sexp()
  local current_source = M.node_source(metadata)
  if not current_source then error('Node has no source') end
  local new_source =
    picker.prompt_for_source_change(current_source)
  if not new_source then return end
  new_source = vim.trim(new_source)
  if new_source == '' then return end
  if new_source:match('%s') then
    error('Source names cannot contain whitespace') end
  if new_source == current_source then
    vim.notify('Source unchanged: ' .. current_source)
    return end
  local changed_count
  if recursive then
    changed_count =
      M.change_source_recursive(current_source, new_source)
  else
    changed_count = M.change_source_at_line(
      M.current_line_number(), new_source) end
  vim.notify(string.format(
    'Source changed from %s to %s on %d node%s. Save to apply.',
    current_source, new_source, changed_count,
    changed_count == 1 and '' or 's'))
end

function M.set_source_recursive ()
  M.set_source(true)
end

---Change OLD_SOURCE to NEW_SOURCE in this content subtree (root
---inclusive; only parentIs=affected descendants are traversed).
---@param old_source string
---@param new_source string
---@return integer changed node count
function M.change_source_recursive (old_source, new_source)
  local start_line = M.current_line_number()
  local start_level = M.outline_level(start_line)
  local changed_count =
    M.change_source_at_line(start_line, new_source)
  local line = M.next_heading_line(start_line)
  while line and (M.outline_level(line) or 0) > start_level do
    local metadata = M.metadata_sexp_at_line_or_nil(line)
    if not (M.activeNode_sexp_p(metadata)
            and M.node_parentIs_content_of_p(metadata)) then
      line = M.next_heading_after_subtree(line)
    else
      if M.node_source(metadata) == old_source then
        changed_count = changed_count
          + M.change_source_at_line(line, new_source) end
      line = M.next_heading_line(line) end
  end
  return changed_count
end

---Set the source at LINE_NUMBER to NEW_SOURCE (and refresh the
---sourceHerald so the display keeps pace).
---@param line_number integer
---@param new_source string
---@return integer 1
function M.change_source_at_line (line_number, new_source)
  M.edit_metadata_at_line(line_number, sexpr.read(string.format(
    '(skg (node (ENSURE (source %s)) (viewStats)))', new_source)))
  M.edit_metadata_at_line(line_number, sexpr.read(string.format(
    '(skg (node (viewStats (ENSURE (sourceHerald ⌂:%s)))))',
    new_source)))
  return 1
end

---Mark the node at point to merge ACQUIREE_ID_OR_LINK (a bare id or
---an [[id:ID][label]] link), prompting when not given. Run from the
---acquirer. Does NOT save.
---@param acquiree_id_or_link string|nil
function M.set_merge_request (acquiree_id_or_link)
  local answer = acquiree_id_or_link
                 or vim.fn.input('Acquiree ID or link: ')
  if answer == nil or answer == '' then
    error('Acquiree ID cannot be empty') end
  local acquiree_id = M.id_from_link_or_text(answer)
  if acquiree_id == '' then
    error('Acquiree ID cannot be empty') end
  M.edit_metadata_at_point(sexpr.read(string.format(
    '(skg (node (DELETE (editRequest)) (editRequest (merge %s))))',
    acquiree_id)))
  vim.notify(string.format(
    'Merge request set for acquiree %s. Save to apply.', acquiree_id))
end

---An ID from TEXT, accepting org id links or bare IDs.
---@param text string
---@return string
function M.id_from_link_or_text (text)
  local trimmed = vim.trim(text)
  local id = trimmed:match('%[%[id:([^%]]+)%]%[')
  return id or trimmed
end

---Delete all kv-pairs keyed KEY from the metadata of the headline at
---point. No effect off a headline or without metadata.
---@param key string
function M.delete_kv_pair_from_metadata_by_key (key)
  if not M.at_heading_p() then return end
  local split = M.split_as_stars_metadata_title(
    M.get_current_headline_text())
  if not split or split.metadata == '' then return end
  local parsed = M.parse_metadata_sexp(split.metadata)
  local kept = {}
  for _, kv in ipairs(parsed.alist) do
    if kv.key ~= key then table.insert(kept, kv) end
  end
  parsed.alist = kept
  M.replace_current_line(M.format_headline(
    split.stars, M.reconstruct_metadata_sexp(parsed), split.title))
end

---Delete all instances of the bare VALUE from the metadata of the
---headline at point.
---@param value string
function M.delete_value_from_metadata (value)
  if not M.at_heading_p() then return end
  local split = M.split_as_stars_metadata_title(
    M.get_current_headline_text())
  if not split or split.metadata == '' then return end
  local parsed = M.parse_metadata_sexp(split.metadata)
  local kept = {}
  for _, bare in ipairs(parsed.bare_values) do
    if bare ~= value then table.insert(kept, bare) end
  end
  parsed.bare_values = kept
  M.replace_current_line(M.format_headline(
    split.stars, M.reconstruct_metadata_sexp(parsed), split.title))
end

---HOST_SEXP with the 'fork' symbol removed from its node's
---(viewRequests ...) form, dropping that form entirely if it becomes
---empty. The result equals HOST_SEXP when there was no fork request.
---@param host_sexp any
---@return any
function M.remove_fork_from_viewrequests (host_sexp)
  local FORK = sexpr.symbol('fork')
  local VIEWREQUESTS = sexpr.symbol('viewRequests')
  local result = { host_sexp[1] }
  for i = 2, #host_sexp do
    local element = host_sexp[i]
    if sexpr.is_list(element) and element[1] == NODE then
      local node = { NODE }
      for j = 2, #element do
        local node_element = element[j]
        if sexpr.is_list(node_element)
           and node_element[1] == VIEWREQUESTS then
          local kept = { VIEWREQUESTS }
          for k = 2, #node_element do
            if node_element[k] ~= FORK then
              table.insert(kept, node_element[k]) end
          end
          if #kept > 1 then table.insert(node, kept) end
        else
          table.insert(node, node_element) end
      end
      table.insert(result, node)
    else
      table.insert(result, element) end
  end
  return result
end

---Remove every fork viewRequest from the buffer's headlines. Used on
---fork-decline so a lingering explicit-fork atom does not silently
---re-fork on the next save.
function M.strip_fork_requests_in_buffer ()
  local last = vim.api.nvim_buf_line_count(0)
  for line_number = 1, last do
    if M.at_heading_p(line_number) then
      local split = M.split_as_stars_metadata_title(
        M.line_text(line_number))
      if split and split.metadata ~= '' then
        local ok, sexp = pcall(sexpr.read, split.metadata)
        if ok then
          local stripped = M.remove_fork_from_viewrequests(sexp)
          if not compare.values_equal(stripped, sexp) then
            M.replace_line(line_number, M.format_headline(
              split.stars, sexpr.to_string(stripped), split.title))
          end
        end
      end
    end
  end
end

---Toggle the cursor between the start of the line and the start of
---the title (the text after stars and metadata). On a non-headline,
---just move to the beginning of the line.
function M.beginning_of_line ()
  local split = M.at_heading_p()
    and M.split_as_stars_metadata_title(M.get_current_headline_text())
    or nil
  if split then
    local line = M.line_text()
    local prefix_length = #split.stars + #split.metadata
    local rest = line:sub(prefix_length + 1)
    local spaces = #(rest:match('^[ \t]*') or '')
    local title_col = prefix_length + spaces -- 0-based byte column
    local current_col = vim.api.nvim_win_get_cursor(0)[2]
    if current_col ~= title_col then
      vim.api.nvim_win_set_cursor(0,
        { M.current_line_number(), title_col })
      return end
  end
  vim.api.nvim_win_set_cursor(0, { M.current_line_number(), 0 })
end

---Cycle the TODO keyword of the headline at point without corrupting
---its metadata: strip the (skg ...) metadata so the org plugin sees a
---plain heading, cycle, then re-insert the metadata after the stars
---(before the new keyword). The analog of the elisp org-todo advice.
---@param direction integer 1 or -1
function M.todo_cycle (direction)
  if not M.at_heading_p() then return end
  local line_number = M.current_line_number()
  local split = M.split_as_stars_metadata_title(
    M.line_text(line_number))
  local has_metadata = split and split.metadata ~= ''
  if has_metadata then
    M.replace_line(line_number, split.stars .. split.title)
  end
  local ok, err = pcall(function ()
    local orgmode = require('orgmode')
    orgmode.action(direction >= 0 and 'org_mappings.todo_next_state'
                   or 'org_mappings.todo_prev_state')
  end)
  if has_metadata then
    local new_line = M.line_text(line_number)
    local stars, rest = new_line:match('^(%*+%s+)(.*)$')
    if stars then
      M.replace_line(line_number,
        stars .. split.metadata .. ' ' .. rest)
    end
  end
  if not ok then
    vim.notify('skg: TODO cycling unavailable: ' .. tostring(err))
  end
end

return M
