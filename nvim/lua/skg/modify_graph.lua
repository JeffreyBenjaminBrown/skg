-- PURPOSE: User-facing commands that modify graph structure:
-- goto-biggest-branch, replace-content-with-link, and
-- replace-link-with-content. The Lua port of elisp/skg-modify-graph.el.

local config = require('skg.config')
local focus = require('skg.focus')
local id_search = require('skg.id_search')
local metadata = require('skg.metadata')

local M = {}

---Org bracket link in a headline title, target and optional label
---captured. (The general form, not only id links.)
M.headline_org_link_pattern = '%[%[([^%]]+)%]%[?([^%]]*)%]?%]'

-- ── goto-biggest-branch ────────────────────────────────────────────

---Go to the sibling, or child, with the most org-descendants: if the
---node at point has siblings, consider the node and its siblings;
---otherwise its immediate children. Intentionally an org outline
---heuristic, not a precise graph-content query.
function M.goto_biggest_branch ()
  local candidates = M.biggest_branch_candidates()
  local best_line, best_count = nil, -1
  for _, line in ipairs(candidates) do
    local count = M.org_descendant_count(line)
    if count > best_count then
      best_line, best_count = line, count end
  end
  vim.api.nvim_win_set_cursor(0, { best_line, 0 })
  vim.notify(string.format('Biggest branch has %d org-descendent(s).',
                           best_count))
end

---@return integer[] candidate headline lines
function M.biggest_branch_candidates ()
  local line = focus.owning_headline_line()
  if not line then error('Not in an org headline or body') end
  local level = metadata.outline_level(line)
  local siblings = M.same_level_headlines_in_parent_subtree(line, level)
  if #siblings > 1 then return siblings end
  local children = M.headlines_with_level_between(
    level + 1, line, M.subtree_end(line))
  if #children == 0 then
    error('This branch has no siblings or children') end
  return children
end

---The last line of the subtree rooted at LINE (inclusive).
---@param line integer
---@return integer
function M.subtree_end (line)
  local after = metadata.next_heading_after_subtree(line)
  if after then return after - 1 end
  return vim.api.nvim_buf_line_count(0)
end

---@param line integer a headline
---@param level integer its level
---@return integer[] headlines of LEVEL within the parent's subtree
---(or the whole buffer when there is no parent)
function M.same_level_headlines_in_parent_subtree (line, level)
  local parent = metadata.parent_heading_line(line, level)
  local first, last
  if parent then
    first = parent
    last = M.subtree_end(parent)
  else
    first = 1
    last = vim.api.nvim_buf_line_count(0) end
  return M.headlines_with_level_between(level, first, last)
end

---@param level integer
---@param first integer
---@param last integer
---@return integer[]
function M.headlines_with_level_between (level, first, last)
  local lines = {}
  for line = first, last do
    if metadata.outline_level(line) == level then
      table.insert(lines, line) end
  end
  return lines
end

---@param line integer a headline
---@return integer how many org-descendant headlines sit under it
function M.org_descendant_count (line)
  local count = 0
  for candidate = line + 1, M.subtree_end(line) do
    if metadata.at_heading_p(candidate) then count = count + 1 end
  end
  return count
end

-- ── shared guards for the two replacements ─────────────────────────

---Metadata of the org-parent of the headline at LINE; errors when
---there is none or it is not an activeNode.
---@param line integer
---@return any
function M.container_data (line)
  local level = metadata.outline_level(line)
  local parent = metadata.parent_heading_line(line, level)
  if not parent then
    error('Cannot replace this branch with a link:'
          .. ' node has no container') end
  local sexp = metadata.metadata_sexp_at_line_or_nil(parent)
  if not metadata.activeNode_sexp_p(sexp) then
    error('Cannot replace this branch with a link:'
          .. ' container is not an activeNode') end
  return sexp
end

---Error unless CONTAINER_SEXP is an editable container: sourced,
---owned, and definitive.
---@param container_sexp any
function M.check_container (container_sexp)
  local source = metadata.node_source(container_sexp)
  if not source then
    error('Cannot replace this branch with a link:'
          .. ' container has no source') end
  local owned = config.owned_sources() or {}
  local is_owned = false
  for _, name in ipairs(owned) do
    if name == source then is_owned = true break end
  end
  if not is_owned then
    error('Cannot replace this branch with a link:'
          .. ' container source is not owned: ' .. source) end
  if metadata.node_indefinitive_p(container_sexp) then
    error('Cannot replace this branch with a link:'
          .. ' container is indefinitive') end
end

-- ── replace content with link ──────────────────────────────────────

---Replace the branch at point with a link to its former root, then
---save. The root must be an existing ActiveNode with an ID; its
---org-parent must be a definitive, owned ActiveNode. Asks before
---proceeding when the title already contains a link.
function M.replace_content_with_link ()
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  vim.api.nvim_win_set_cursor(0, { line, 0 })
  local sexp = metadata.metadata_sexp_at_line_or_nil(line)
  if not metadata.activeNode_sexp_p(sexp) then
    error('Cannot replace this branch with a link:'
          .. ' it is not an activeNode') end
  local id = metadata.node_id(sexp)
  if not id then
    error('Cannot replace this branch with a link:'
          .. ' node has no ID') end
  M.check_container(M.container_data(line))
  local split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line))
  if split.title:find(M.headline_org_link_pattern) then
    if #vim.api.nvim_list_uis() > 0
       and vim.fn.confirm('Are you sure?', '&Yes\n&No', 2) ~= 1 then
      error('Canceled') end
  end
  local label = M.read_link_label(
    M.replace_headline_links_with_labels(split.title))
  local replacement = string.format('%s[[id:%s][%s]]',
                                    split.stars, id, label)
  vim.api.nvim_buf_set_lines(0, line - 1, M.subtree_end(line), false,
                             { replacement })
  require('skg.save').request_save_buffer()
end

---TITLE with each org bracket link replaced by its label (or target
---when label-less).
---@param title string
---@return string
function M.replace_headline_links_with_labels (title)
  return (title:gsub('%[%[([^%]]+)%]%[([^%]]+)%]%]', '%2')
              :gsub('%[%[([^%]]+)%]%]', '%1'))
end

---Prompt for a link label, defaulting to DEFAULT_LABEL; headless runs
---take the default.
---@param default_label string
---@return string
function M.read_link_label (default_label)
  if #vim.api.nvim_list_uis() == 0 then return default_label end
  local answer = vim.fn.input('Link label: ', default_label)
  if answer == '' then return default_label end
  return answer
end

-- ── replace link with content ──────────────────────────────────────

---Replace the leaf at point with content linked from that leaf, then
---save. The leaf must have exactly one org bracket link in its title
---plus body, no org-descendants, and a definitive, owned ActiveNode
---org-parent; the link must be an id link. Warns when the leaf
---already had an ID (the old node may become an orphan).
function M.replace_link_with_content ()
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  vim.api.nvim_win_set_cursor(0, { line, 0 })
  M.check_container(M.container_data(line))
  local node_id = metadata.node_id(
    metadata.metadata_sexp_at_line_or_nil(line) or {})
  local link = M.single_link_in_leaf(line)
  if node_id then
    vim.notify(string.format(
      'Warning: replacing existing node %s may have created an orphan',
      node_id))
  end
  local split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line))
  local replacement = string.format(
    '%s(skg (node (id %s) indef (viewRequests definitiveView))) %s',
    split.stars, link.id, link.label or link.id)
  vim.api.nvim_buf_set_lines(0, line - 1, M.subtree_end(line), false,
                             { replacement })
  require('skg.save').request_save_buffer()
end

---The single org bracket link in the leaf at LINE; errors when the
---node has descendants, when there is not exactly one link, or when
---it is not an id link.
---@param line integer
---@return table {id, label}
function M.single_link_in_leaf (line)
  local level = metadata.outline_level(line)
  local next_heading = metadata.next_heading_line(line)
  if next_heading and next_heading <= M.subtree_end(line)
     and (metadata.outline_level(next_heading) or 0) > level then
    error('Cannot replace link with content:'
          .. ' node has org-descendents') end
  local split = metadata.split_as_stars_metadata_title(
    metadata.line_text(line))
  local body_lines = vim.api.nvim_buf_get_lines(
    0, line, M.subtree_end(line), false)
  local text = split.title .. '\n' .. table.concat(body_lines, '\n')
  local links = {}
  for target, label in text:gmatch('%[%[([^%]]+)%]%[([^%]]+)%]%]') do
    table.insert(links, { target = target, label = label })
  end
  for target in text:gmatch('%[%[([^%]]+)%]%]') do
    if not target:find('%]%[') then
      -- A label-less link (the two-capture gmatch above missed it).
      local already = false
      for _, link in ipairs(links) do
        if link.target == target then already = true end
      end
      if not already then
        table.insert(links, { target = target, label = nil }) end
    end
  end
  if #links ~= 1 then
    error(string.format(
      'Cannot replace link with content:'
      .. ' expected exactly one link, found %d', #links)) end
  local link = links[1]
  if link.target:sub(1, 3) ~= 'id:' then
    error('Cannot replace link with content:'
          .. ' the link is not an id link') end
  return { id = link.target:sub(4), label = link.label }
end

return M
