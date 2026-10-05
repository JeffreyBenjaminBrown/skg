-- PURPOSE: Read configuration from skgconfig.toml.
-- The Lua port of elisp/skg-config.el's parsing half. The interactive
-- skgrepo pickers that elisp kept in the same file (built on
-- completing-read with S-arrow cycling) live in skg.picker instead,
-- since they are UI, not parsing. Like the elisp, this is a
-- hand-rolled line scan of the narrow TOML subset skgconfig.toml
-- uses, not a general TOML parser.

local M = {}

---The absolute path of the active skgconfig.toml, set by
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

---The `owned_folder' setting from FILE, defaulting to 'owned'.
---Repos whose path sits under this folder are owned; all others
---are foreign. Replaces the retired per-repo 'user_owns_it' key.
---@param file string
---@return string
function M.owned_folder_from_toml (file)
  for _, line in ipairs(trimmed_lines(file)) do
    local folder = line:match('^owned_folder[ \t]*=[ \t]*"([^"]+)"')
    if folder then return folder end
  end
  return 'owned'
end

---Names of the OWNED skgrepos in FILE, in declaration order. A skgrepo
---is owned iff its path (resolved against FILE's directory, the data
---root) sits under the data root's owned_folder (default 'owned') --
---the author-folder layout, mirroring the server's rule. A skgrepo
---with no 'name' key defaults its name to its path, also mirroring
---the server.
---@param file string
---@return string[]
function M.owned_repos_from_toml (file)
  local owned_folder = M.owned_folder_from_toml(file)
  local data_root = file:match('^(.*/)') or './'
  local owned_root = data_root .. owned_folder .. '/'
  local skgrepos = {}
  local in_skgrepos = false
  local current_name, current_path = nil, nil
  local function flush ()
    if in_skgrepos and current_path then
      local abs = current_path
      if not abs:match('^/') then abs = data_root .. abs end
      if not abs:match('/$') then abs = abs .. '/' end
      if abs == owned_root
         or abs:sub(1, #owned_root) == owned_root
      then
        table.insert(skgrepos, current_name or current_path) end
    end
    current_name, current_path = nil, nil
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

---The skgrepo-set choices, in privacy order, ending with 'all'.
---A skgrepo-set is a prefix of the config's privacy order: each
---repo names the set of itself and everything more public;
---'all' means every skgrepo.
---@return string[]|nil skgrepo-set choices, 'all' last, or nil
function M.repo_set_names ()
  local file = M.config_file()
  if not file then return nil end
  local names = M.repo_names_from_toml(file)
  table.insert(names, 'all')
  return names
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
---@param skgid string
---@param skgrepo string
---@return string|nil
function M.abs_path_for_id_and_repo (skgid, skgrepo)
  local dir = M.repo_dir(skgrepo)
  if dir then return dir .. '/' .. skgid .. '.skg' end
  return nil
end

return M
