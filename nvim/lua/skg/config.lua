-- PURPOSE: Read configuration from skgconfig.toml.
-- The Lua port of elisp/skg-config.el's parsing half. The interactive
-- source pickers that elisp kept in the same file (built on
-- completing-read with S-arrow cycling) live in skg.picker instead,
-- since they are UI, not parsing. Like the elisp, this is a
-- hand-rolled line scan of the narrow TOML subset skgconfig.toml
-- uses, not a general TOML parser.

local M = {}
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')

---The absolute path of the active skgconfig.toml, set by
---require('skg').init. (The analog of 'skg-config-dir', which lived
---in skg-state; here the config module owns it.)
---@type string|nil
M.config_file_path = nil

---Normalized source entries supplied by the connected server. Nil before
---connection, when the TOML readers below remain the startup fallback.
---@type table[]|nil
M.source_inventory = nil

---Authoritative selected path generations and derived-store health from the
---connection handshake.
---@type table|nil
M.store_state = nil

---Advance the selected graph baseline from an authoritative response.
---Responses from older operations must not regress a newer observation.
---@param generation integer|nil
function M.observe_graph_generation (generation)
  if type(generation) ~= 'number' or generation < 0
     or generation ~= math.floor(generation) then return end
  M.store_state = M.store_state or {}
  local current = M.store_state.graph_generation
  if type(current) ~= 'number' or generation > current then
    M.store_state.graph_generation = generation end
end

local function optional_atom_text (value)
  if value == nil or sexpr.is_nil(value) then return nil end
  return sexpr.atom_text(value)
end

---Install the source-inventory value from a verification response.
---@param wire_entries any
function M.install_source_inventory (wire_entries)
  if wire_entries == nil or not sexpr.is_list(wire_entries) then return end
  local inventory = {}
  for _, entry in ipairs(wire_entries) do
    local owned = payload.field(entry, 'owned')
    table.insert(inventory, {
      name = optional_atom_text(payload.field(entry, 'name')),
      abbreviation = optional_atom_text(
        payload.field(entry, 'abbreviation')),
      owned = owned ~= nil and not sexpr.is_nil(owned),
      position = payload.field(entry, 'position'),
      configured_path = optional_atom_text(
        payload.field(entry, 'configured-path')),
      path = optional_atom_text(payload.field(entry, 'directory')),
      directory_identity = optional_atom_text(
        payload.field(entry, 'directory-identity')),
    })
  end
  M.source_inventory = inventory
  local raw_file = package.loaded['skg.raw_file']
  if raw_file and raw_file.enroll_open_files then
    raw_file.enroll_open_files() end
end

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

---The `owned_folder' setting from FILE, defaulting to 'owned'.
---Sources whose path sits under this folder are owned; all others
---are foreign. Replaces the retired per-source 'user_owns_it' key.
---@param file string
---@return string
function M.owned_folder_from_toml (file)
  for _, line in ipairs(trimmed_lines(file)) do
    local folder = line:match('^owned_folder[ \t]*=[ \t]*"([^"]+)"')
    if folder then return folder end
  end
  return 'owned'
end

---Names of the OWNED sources in FILE, in declaration order. A source
---is owned iff its path (resolved against FILE's directory, the data
---root) sits under the data root's owned_folder (default 'owned') --
---the author-folder layout, mirroring the server's rule. A source
---with no 'name' key defaults its name to its path, also mirroring
---the server.
---@param file string
---@return string[]
function M.owned_sources_from_toml (file)
  local owned_folder = M.owned_folder_from_toml(file)
  local data_root = file:match('^(.*/)') or './'
  local owned_root = data_root .. owned_folder .. '/'
  local sources = {}
  local in_sources = false
  local current_name, current_path = nil, nil
  local function flush ()
    if in_sources and current_path then
      local abs = current_path
      if not abs:match('^/') then abs = data_root .. abs end
      if not abs:match('/$') then abs = abs .. '/' end
      if abs == owned_root
         or abs:sub(1, #owned_root) == owned_root
      then
        table.insert(sources, current_name or current_path) end
    end
    current_name, current_path = nil, nil
  end
  for _, line in ipairs(trimmed_lines(file)) do
    if line:match('^%[%[sources%]%]') then
      flush(); in_sources = true
    elseif line:match('^%[%[') then
      flush(); in_sources = false
    elseif in_sources then
      local name = line:match('^name[ \t]*=[ \t]*"([^"]+)"')
      local path = line:match('^path[ \t]*=[ \t]*"([^"]+)"')
      if name then current_name = name end
      if path then current_path = path end
    end
  end
  flush()
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
  local names = {}
  for _, source in ipairs(M.source_paths_from_toml(file)) do
    table.insert(names, source.name) end
  return names
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
    if current_path then
      local absolute = current_path
      if not absolute:match('^/') then
        absolute = config_dir .. '/' .. absolute end
      table.insert(result, { name = current_name or current_path,
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
  if M.source_inventory then
    local result = {}
    for _, source in ipairs(M.source_inventory) do
      table.insert(result, { name = source.name, path = source.path }) end
    return result end
  local file = M.config_file()
  return file and M.source_paths_from_toml(file) or nil
end

---@return string[]|nil
function M.source_names ()
  if M.source_inventory then
    local result = {}
    for _, source in ipairs(M.source_inventory) do
      table.insert(result, source.name) end
    return result end
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
  if M.source_inventory then
    local result = {}
    for _, source in ipairs(M.source_inventory) do
      if source.owned then table.insert(result, source.name) end end
    return result end
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
