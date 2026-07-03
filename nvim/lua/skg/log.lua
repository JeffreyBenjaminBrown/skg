-- PURPOSE: Structured logging for the skg Neovim client.
-- The Lua port of elisp/skg-log.el.
--
-- Output: JSON lines to M.log_file, one object per entry.
-- Each entry has: timestamp, level, caller, category, message.
--
-- Toggle: set M.log_file to nil to disable all logging.
-- Filter: set M.log_categories to a list of strings to restrict
--         which categories are logged. Empty list = log everything.
-- Level:  set M.log_min_level to 'debug', 'info', 'warn', or 'error'.
--
-- USAGE:
--   log.log('info', 'save', 'sending %d bytes', content_length)
--   log.log('debug', 'tcp', 'received %s', payload)
--   log.log('warn', 'uri', 'nil view uri in %s', buffer_name)
--   log.log('error', 'parse', 'failed: %s', vim.inspect(err))

local M = {}

---Path to the skg client log file. Nil disables logging.
---@type string|nil
M.log_file = nil

---Enabled log categories. Empty list means log everything.
---@type string[]
M.log_categories = {}

---Minimum log level: 'debug', 'info', 'warn', or 'error'.
---@type string
M.log_min_level = 'info'

local level_order = { debug = 0, info = 1, warn = 2, error = 3 }

---Log a structured JSON entry if LEVEL and CATEGORY pass the filters.
---@param level string 'debug'|'info'|'warn'|'error'
---@param category string e.g. 'save', 'tcp', 'search', 'uri'
---@param format_string string passed to string.format
---@param ... any format arguments
function M.log (level, category, format_string, ...)
  if not M.log_file then return end
  if (level_order[level] or 1)
     < (level_order[M.log_min_level] or 1) then return end
  if #M.log_categories > 0
     and not vim.tbl_contains(M.log_categories, category) then return end
  local message = string.format(format_string, ...)
  local entry = string.format(
    '{"ts":"%s","level":"%s","cat":"%s","fn":"%s","msg":%s}\n',
    M.timestamp_with_milliseconds(),
    level, category,
    M.calling_function_name(),
    vim.json.encode(message))
  local handle = io.open(M.log_file, 'a')
  if handle then
    handle:write(entry)
    handle:close() end
end

---An ISO-8601-ish timestamp with millisecond precision,
---matching the elisp client's '%Y-%m-%dT%H:%M:%S.%3N'.
---@return string
function M.timestamp_with_milliseconds ()
  local seconds, microseconds = vim.uv.gettimeofday()
  return os.date('%Y-%m-%dT%H:%M:%S', seconds)
         .. string.format('.%03d', math.floor(microseconds / 1000))
end

---The name of the function that called M.log.
---Walks the stack to find the first frame outside this module,
---the analog of the elisp backtrace walk in 'skg-log--caller'.
---@return string
function M.calling_function_name ()
  for depth = 2, 20 do
    local frame = debug.getinfo(depth, 'nS')
    if not frame then break end
    if frame.source ~= debug.getinfo(1, 'S').source then
      return frame.name
             or (frame.short_src .. ':' .. tostring(frame.currentline)) end
  end
  return '?'
end

return M
