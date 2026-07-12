-- PURPOSE: Read configuration from skgconfig.toml.
-- The Lua port of elisp/skg-config.el's parsing half. The interactive
-- source pickers that elisp kept in the same file (built on
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

---The array-table name from LINE ('[[sources]]' -> 'sources'), or nil.
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

---Name strings for sources with user_owns_it = true in FILE.
---@param file string
---@return string[]
function M.owned_sources_from_toml (file)
  local sources = {}
  local current_name = nil
  for _, line in ipairs(trimmed_lines(file)) do
    if line:match('^%[%[sources%]%]') then
      current_name = nil
    else
      local name = line:match('^name[ \t]*=[ \t]*"([^"]+)"')
      if name then
        current_name = name
      elseif current_name
             and line:match('^user_owns_it[ \t]*=[ \t]*true') then
        table.insert(sources, current_name)
        current_name = nil end
    end
  end
  return sources
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
---@return string[] configured source names
function M.source_names_from_toml (file)
  return M.table_names_from_toml(file, 'sources')
end

---Pairs of {name, absolute dir} for each [[sources]] entry in FILE.
---Relative source paths are resolved against the directory of FILE,
---matching what the server's 'make_paths_absolute' does at
---config-load time.
---@param file string
---@return table[] each {name = ..., path = ...}
function M.source_paths_from_toml (file)
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
    elseif current_table == 'sources' then
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
function M.source_paths ()
  local file = M.config_file()
  return file and M.source_paths_from_toml(file) or nil
end

---@return string[]|nil
function M.source_names ()
  local file = M.config_file()
  return file and M.source_names_from_toml(file) or nil
end

---The source-set choices, in privacy order, ending with 'all'.
---A source-set is a prefix of the config's privacy order: each
---source names the set of itself and everything more public;
---'all' means every source.
---@return string[]|nil source-set choices, 'all' last, or nil
function M.source_set_names ()
  local file = M.config_file()
  if not file then return nil end
  local names = M.source_names_from_toml(file)
  table.insert(names, 'all')
  return names
end

---@return string[]|nil owned source names, or nil without config
function M.owned_sources ()
  local file = M.config_file()
  return file and M.owned_sources_from_toml(file) or nil
end

---@param source_name string
---@return string|nil absolute directory for SOURCE_NAME, or nil
function M.source_dir (source_name)
  for _, entry in ipairs(M.source_paths() or {}) do
    if entry.name == source_name then return entry.path end
  end
  return nil
end

---The absolute path of ID.skg within SOURCE's directory, or nil if
---SOURCE is not declared in the config.
---@param id string
---@param source string
---@return string|nil
function M.abs_path_for_id_and_source (id, source)
  local dir = M.source_dir(source)
  if dir then return dir .. '/' .. id .. '.skg' end
  return nil
end

return M
