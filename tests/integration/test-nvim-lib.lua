-- PURPOSE: Shared bootstrap and polling utilities for the nvim-client
-- integration tests, the Lua analog of loading elisp/skg-init.el plus
-- ../test-wait.el. Load from a test directory with
--   local T = dofile('../test-nvim-lib.lua')
-- It puts the repo's nvim/ plugin on the runtimepath, points the
-- client at $SKG_TEST_PORT, and arms a global timeout.

local M = {}

do -- runtimepath and editor settings
  local this_file = vim.fn.fnamemodify(
    debug.getinfo(1, 'S').source:sub(2), ':p')
  local repo_root = vim.fn.fnamemodify(this_file, ':h:h:h')
  vim.opt.runtimepath:prepend(repo_root .. '/nvim')
  vim.o.swapfile = false
  M.repo_root = repo_root
end

M.state = require('skg.state')
M.client = require('skg.client')

do -- the test server's port
  local port = os.getenv('SKG_TEST_PORT')
  if port then
    M.client.port = tonumber(port)
    print('Using test port: ' .. port)
  end
end

---Fail the test after TIMEOUT_SECS (default 20) if it has not passed.
---@param timeout_secs number|nil
function M.arm_timeout (timeout_secs)
  vim.defer_fn(function ()
    print('TIMEOUT: Integration test timed out!')
    os.exit(1)
  end, (timeout_secs or 20) * 1000)
end

---Poll PREDICATE every 0.1s, processing I/O between polls. Returns
---its first truthy value, or nil after TIMEOUT_SECS (default 15).
---@param predicate fun(): any
---@param timeout_secs number|nil
---@return any
function M.wait_for (predicate, timeout_secs)
  local result = nil
  vim.wait((timeout_secs or 15) * 1000, function ()
    result = predicate()
    return result and true or false
  end, 100)
  return result
end

---Wait for a buffer named BUFFER_NAME; returns its bufnr or nil.
---@param buffer_name string
---@param timeout_secs number|nil
---@return integer|nil
function M.wait_for_buffer (buffer_name, timeout_secs)
  return M.wait_for(function ()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf)
         and vim.api.nvim_buf_get_name(buf) == buffer_name then
        return buf end
    end
    return nil
  end, timeout_secs)
end

---Wait until no one-shot handlers are pending and the LP machine is
---idle. Returns true on success, nil on timeout.
---@param timeout_secs number|nil
---@return boolean|nil
function M.wait_for_response (timeout_secs)
  return M.wait_for(function ()
    return M.state.lp_pending_count == 0
           and M.state.lp_bytes_left == nil
           and #M.state.lp_buffer == 0
  end, timeout_secs)
end

---@param buf integer
---@return string BUF's full text
function M.buffer_text (buf)
  return table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
end

---Write TEXT to PATH (the tests log intermediate buffer states).
---@param path string
---@param text string
function M.write_file (path, text)
  local handle = assert(io.open(path, 'w'))
  handle:write(text)
  handle:close()
end

---Report success and exit 0.
---@param message string|nil
function M.pass (message)
  print(message or 'PASS: Integration test successful!')
  os.exit(0)
end

---Report failure and exit 1.
---@param message string
function M.fail (message)
  print('FAIL: ' .. message)
  os.exit(1)
end

---FAIL unless CONDITION; the message names what was expected.
---@param condition any
---@param message string
function M.check (condition, message)
  if not condition then M.fail(message) end
  print('ok: ' .. message)
end

return M
