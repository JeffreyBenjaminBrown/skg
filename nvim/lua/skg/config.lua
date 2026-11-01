-- PURPOSE: Read configuration from skgconfig.toml.
-- The Lua port of elisp/skg-config.el's parsing half. The interactive
-- skgrepo pickers that elisp kept in the same file (built on
-- completing-read with S-arrow cycling) live in skg.picker instead,
-- since they are UI, not parsing. Like the elisp, this is a
-- hand-rolled line scan of the narrow TOML subset skgconfig.toml
-- uses, not a general TOML parser.

local M = {}

---The absolute path of the unrestricted skgconfig.toml, set by
---require('skg').init. (The analog of 'skg-config-dir', which lived
---in skg-state; here the config module owns it.)
---@type string|nil
M.config_file_path = nil

---@return string|nil the active config path, if it exists on disk
function M.config_file ()
  if M.config_file_path
     and vim.fn.filereadable(M.config_file_path) == 1 then
    return M.config_file_path end
  return nil
end

---@param file string
---@return string[] the lines of FILE, trimmed
local function trimmed_lines (file)
  local lines = {}
  for _, line in ipairs(vim.fn.readfile(file)) do
    table.insert(lines, vim.trim(line)) end
  return lines
end

---The array-table name from LINE ('[[repos]]' -> 'repos'), or nil.
---@param line string
---@return string|nil
function M.toml_array_table_line_name (line)
  return line:match('^%[%[([%w_%-]+)%]%]')
end

---The integer value of 'port = ...' from FILE; errors if absent.
---@param file string
---@return integer
function M.port_from_toml (file)
  for _, line in ipairs(trimmed_lines(file)) do
    local port = line:match('^port[ \t]*=[ \t]*(%d+)')
    if port then return tonumber(port) end
  end
  error('No port setting found in ' .. file)
end

---Names of the owned skgrepos in FILE, in declaration order, which is
---the privacy ladder wherever foreign entries sit among them. A
---skgrepo is owned unless its entry says 'foreign = true', mirroring
---the server. A skgrepo with no 'name' key defaults its name to its
---path, also mirroring the server.
---@param file string
---@return string[]
function M.owned_repos_from_toml (file)
  return M.repo_names_by_ownership_from_toml(file, true)
end

---Names of the foreign skgrepos in FILE, in declaration order, named
---as 'owned_repos_from_toml' names them.
---@param file string
---@return string[]
function M.foreign_repos_from_toml (file)
  return M.repo_names_by_ownership_from_toml(file, false)
end

---Names of the owned skgrepos in FILE if OWNED, else the foreign
---ones, in declaration order; see 'owned_repos_from_toml'.
---@param file string
---@param owned boolean
---@return string[]
function M.repo_names_by_ownership_from_toml (file, owned)
  local skgrepos = {}
  local in_skgrepos = false
  local current_name, current_path, current_foreign = nil, nil, false
  local function flush ()
    if in_skgrepos and current_path and (owned ~= current_foreign) then
      table.insert(skgrepos, current_name or current_path) end
    current_name, current_path, current_foreign = nil, nil, false
  end
  for _, line in ipairs(trimmed_lines(file)) do
    if line:match('^%[%[repos%]%]') then
      flush(); in_skgrepos = true
    elseif line:match('^%[%[') then
      flush(); in_skgrepos = false
    elseif in_skgrepos then
      local name = line:match('^name[ \t]*=[ \t]*"([^"]+)"')
      local path = line:match('^path[ \t]*=[ \t]*"([^"]+)"')
      if name then current_name = name end
      if path then current_path = path end
      if line:match('^foreign[ \t]*=[ \t]*true%f[^%w_]') then
        current_foreign = true end
    end
  end
  flush()
  return skgrepos
end

---Name strings from each [[TABLE_NAME]] entry in FILE.
---@param file string
---@param table_name string
---@return string[]
function M.table_names_from_toml (file, table_name)
  local names = {}
  local current_table = nil
  for _, line in ipairs(trimmed_lines(file)) do
    local new_table = M.toml_array_table_line_name(line)
    if new_table then
      current_table = new_table
    elseif current_table == table_name then
      local name = line:match('^name[ \t]*=[ \t]*"([^"]+)"')
      if name then table.insert(names, name) end
    end
  end
  return names
end

---@param file string
---@return string[] configured skgrepo names
function M.repo_names_from_toml (file)
  return M.table_names_from_toml(file, 'repos')
end

---Pairs of {name, absolute dir} for each [[repos]] entry in FILE.
---Relative skgrepo paths are resolved against the directory of FILE,
---matching what the server's 'make_paths_absolute' does at
---config-load time.
---@param file string
---@return table[] each {name = ..., path = ...}
function M.repo_paths_from_toml (file)
  local config_dir = vim.fn.fnamemodify(file, ':h')
  local result = {}
  local current_table = nil
  local current_name = nil
  local current_path = nil
  local function flush ()
    if current_name and current_path then
      local absolute = current_path
      if not absolute:match('^/') then
        absolute = config_dir .. '/' .. absolute end
      table.insert(result, { name = current_name,
                             path = vim.fn.fnamemodify(absolute, ':p')
                                    :gsub('/$', '') })
      current_name = nil
      current_path = nil end
  end
  for _, line in ipairs(trimmed_lines(file)) do
    local new_table = M.toml_array_table_line_name(line)
    if new_table then
      flush()
      current_table = new_table
      current_name = nil
      current_path = nil
    elseif current_table == 'repos' then
      local path = line:match('^path[ \t]*=[ \t]*"([^"]+)"')
      local name = line:match('^name[ \t]*=[ \t]*"([^"]+)"')
      if path then current_path = path end
      if name then current_name = name end
    end
  end
  flush()
  return result
end


-- ── wrappers over the active config ────────────────────────────────

---@return table[]|nil (name, absolute-path) pairs, or nil without config
function M.repo_paths ()
  local file = M.config_file()
  return file and M.repo_paths_from_toml(file) or nil
end

---@return string[]|nil
function M.repo_names ()
  local file = M.config_file()
  return file and M.repo_names_from_toml(file) or nil
end

---The skgrepo restriction's rungs: the owned repo names, in privacy
---order. Each names the most private owned repo to show, that repo
---and every more public owned one (a prefix of the config's privacy
---ladder); the last, with foreign repos shown, is no skgrepo
---restriction. Foreign repos are on no rung, wherever the config
---lists them.
---@return string[]|nil the rungs, or nil without config
function M.repo_set_names ()
  return M.owned_repos()
end

---How the clients show the skgrepo restriction's rung NAME, a repo
---name: the config's most private owned repo, which with foreign
---repos shown shows every repo, gets ' (no restriction)' appended;
---the words never reach the server.
---@param name string
---@return string
function M.skgrepo_restriction_label (name)
  local names = M.owned_repos()
  if names and #names > 0 and names[#names] == name then
    return name .. ' (no restriction)' end
  return name
end

---@return string[]|nil foreign skgrepo names, or nil without config
function M.foreign_repos ()
  local file = M.config_file()
  return file and M.foreign_repos_from_toml(file) or nil
end

---@return string[]|nil owned skgrepo names, or nil without config
function M.owned_repos ()
  local file = M.config_file()
  return file and M.owned_repos_from_toml(file) or nil
end

---@param repo_name string
---@return string|nil absolute directory for REPO_NAME, or nil
function M.repo_dir (repo_name)
  for _, entry in ipairs(M.repo_paths() or {}) do
    if entry.name == repo_name then return entry.path end
  end
  return nil
end

---The absolute path of ID.skg within REPO's directory, or nil if
---REPO is not declared in the config.
---@param gnid string
---@param skgrepo string
---@return string|nil
function M.abs_path_for_id_and_repo (gnid, skgrepo)
  local dir = M.repo_dir(skgrepo)
  if dir then return dir .. '/' .. gnid .. '.skg' end
  return nil
end

return M
