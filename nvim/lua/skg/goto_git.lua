-- PURPOSE: From a node in a view, jump to git's picture of its .skg
-- file: ask the server for the file's path, open a git UI on that
-- source's repo, and land on the file (or, for the parent variant, on
-- the first changed line containing this node's id). The Lua port of
-- elisp/skg-request-file-path.el, re-targeted per the settled plan:
-- neogit (baked into the docker image) plays magit for the status
-- jump; the parent variant's land-on-the-id-line has no neogit API,
-- so it opens a plain git-diff buffer for the parent's file and
-- searches there -- same information, different chrome (a documented
-- deviation).

local client = require('skg.client')
local config = require('skg.config')
local id_search = require('skg.id_search')
local metadata = require('skg.metadata')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---{id, source} for the node on the current line, or nil with a
---message.
---@return table|nil
function M.node_info_at_point ()
  local sexp = metadata.metadata_sexp_at_line_or_nil(nil)
  if not sexp then
    vim.notify('No metadata sexp found on this line.')
    return nil end
  local id = id_search.extract_id_from_metadata_sexp(sexp)
  if not id then
    vim.notify("No id in this line's metadata.")
    return nil end
  local source = id_search.extract_source_from_metadata_sexp(sexp)
  if not source then
    vim.notify('Could not extract id or source from metadata.')
    return nil end
  return { id = id, source = source }
end

---{id, source} for the parent heading's node, or nil with a message.
---@return table|nil
function M.parent_info_at_point ()
  local line = metadata.current_line_number()
  local level = metadata.at_heading_p(line)
                and metadata.outline_level(line) or 999
  local parent = metadata.parent_heading_line(line, level)
  if not parent then
    vim.notify('No parent heading found.')
    return nil end
  local sexp = metadata.metadata_sexp_at_line_or_nil(parent)
  if not sexp then
    vim.notify('Parent heading has no node metadata.')
    return nil end
  local id = id_search.extract_id_from_metadata_sexp(sexp)
  local source = id_search.extract_source_from_metadata_sexp(sexp)
  if not (id and source) then
    vim.notify('Could not extract id or source from parent.')
    return nil end
  return { id = id, source = source }
end

---Request the on-disk path for ID within SOURCE; HANDLER receives the
---resolved absolute path (or is not called, with a message shown).
---@param id string
---@param source string
---@param handler fun(resolved_path: string)
function M.request_file_path (id, source, handler)
  state.register_response_handler('get-file-path',
    function (_payload_text, response)
      local content = payload.field_text(response, 'content')
      local path = content and vim.trim(content) or nil
      if not path then
        vim.notify('No path received')
        return end
      if path:sub(1, 5) == 'Error' or path:sub(1, 5) == 'File ' then
        vim.notify(path)
        return end
      local config_dir =
        vim.fn.fnamemodify(config.config_file_path or '', ':h')
      local resolved = path
      if not path:match('^/') then
        resolved = config_dir .. '/' .. path end
      handler(resolved)
    end, true)
  state.lp_reset()
  client.send_string(sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'get file path'),
    sexpr.pair(sexpr.symbol('id'), id),
    sexpr.pair(sexpr.symbol('source'), source) }) .. '\n')
end

---Open a git status view (neogit) for the repo holding RESOLVED_PATH
---and land on the file's entry; warn when it appears several times
---(unstaged, staged, untracked all list it).
---@param resolved_path string
function M.open_git_status_at_file (resolved_path)
  local dir = vim.fn.fnamemodify(resolved_path, ':h')
  local file = vim.fn.fnamemodify(resolved_path, ':t')
  local ok, neogit = pcall(require, 'neogit')
  if not ok then
    vim.notify('neogit is not available; showing a plain diff instead')
    M.open_plain_diff_at(resolved_path, nil)
    return end
  neogit.open({ cwd = dir, kind = 'tab' })
  -- Neogit populates asynchronously; search for the file once it has.
  local deadline = vim.uv.now() + 5000
  local function try_focus ()
    if vim.bo.filetype:find('Neogit')
       or vim.bo.filetype:find('neogit') then
      local matches = M.search_all(file)
      if #matches > 0 then
        vim.api.nvim_win_set_cursor(0, matches[1])
        if #matches > 1 then
          vim.notify(string.format(
            'Warning: file appears %d times (sections); point is on'
            .. ' the first.', #matches))
        end
        require('skg.readable_ids').enable(
          vim.api.nvim_get_current_buf())
        return true
      end
    end
    if vim.uv.now() < deadline then
      vim.defer_fn(try_focus, 100)
    else
      vim.notify(string.format('git status: %s has not changed.',
                               file))
      require('skg.readable_ids').enable(
        vim.api.nvim_get_current_buf())
    end
  end
  vim.defer_fn(try_focus, 100)
end

---Every {line, col} where NEEDLE occurs in the current buffer.
---@param needle string
---@return table[]
function M.search_all (needle)
  local matches = {}
  for row, line in ipairs(
      vim.api.nvim_buf_get_lines(0, 0, -1, false)) do
    local start = line:find(needle, 1, true)
    if start then table.insert(matches, { row, start - 1 }) end
  end
  return matches
end

---Show the combined staged+unstaged diff (plus untracked content) of
---RESOLVED_PATH in a scratch diff buffer with readable ids; when
---SEARCH_ID is given, land on its first changed line, warning about
---further occurrences.
---@param resolved_path string
---@param search_id string|nil
function M.open_plain_diff_at (resolved_path, search_id)
  local dir = vim.fn.fnamemodify(resolved_path, ':h')
  local file = vim.fn.fnamemodify(resolved_path, ':t')
  local diff = vim.fn.system({ 'git', '-C', dir, 'diff', 'HEAD',
                               '--', file })
  if vim.v.shell_error ~= 0 or vim.trim(diff) == '' then
    -- HEAD may not know the file (untracked) or there is no change;
    -- fall back to the untracked file's own content as an
    -- all-additions diff would show it.
    local staged = vim.fn.system({ 'git', '-C', dir, 'diff',
                                   '--cached', '--', file })
    if vim.trim(staged) ~= '' then
      diff = staged
    else
      diff = '' end
  end
  if vim.trim(diff) == '' then
    vim.notify(string.format(
      'git: %s has no unstaged or staged changes.', file))
    return end
  local name = 'skg://git-diff/' .. file
  local buf = nil
  for _, existing in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(existing) == name then
      buf = existing break end
  end
  if not buf then
    buf = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(buf, name)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(diff, '\n'))
  vim.bo[buf].filetype = 'diff'
  vim.bo[buf].modifiable = false
  vim.bo[buf].modified = false
  vim.api.nvim_set_current_buf(buf)
  require('skg.readable_ids').enable(buf)
  if search_id then
    local matches = M.search_all(search_id)
    if #matches == 0 then
      vim.api.nvim_win_set_cursor(0, { 1, 0 })
      vim.notify(string.format(
        "%s not found in the parent's diff; showing the whole diff.",
        search_id))
    else
      vim.api.nvim_win_set_cursor(0, matches[1])
      if #matches > 1 then
        vim.notify(string.format(
          'Point on first appearance. Warning: ID appears %d times.',
          #matches))
      else
        vim.notify(string.format(
          "Point is on the first appearance of %s in the parent's"
          .. ' diff.', search_id))
      end
    end
  else
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
  end
end

---Open the git status view for the node at point's file.
function M.goto_in_git ()
  local info = M.node_info_at_point()
  if not info then return end
  M.request_file_path(info.id, info.source,
                      M.open_git_status_at_file)
end

---Open the PARENT's file's diff at the first line containing this
---node's id.
function M.goto_in_git_parent ()
  local node = M.node_info_at_point()
  if not node then return end
  local parent = M.parent_info_at_point()
  if not parent then return end
  M.request_file_path(parent.id, parent.source, function (path)
    M.open_plain_diff_at(path, node.id)
  end)
end

---@param goto_fn fun()
local function goto_then_close (goto_fn)
  local buf = vim.api.nvim_get_current_buf()
  goto_fn()
  if vim.api.nvim_buf_is_valid(buf) then
    vim.bo[buf].modified = false
    vim.api.nvim_buf_delete(buf, { force = true })
  end
end

function M.goto_in_git_and_close_this ()
  goto_then_close(M.goto_in_git)
end

function M.goto_in_git_parent_and_close_this ()
  goto_then_close(M.goto_in_git_parent)
end

return M
