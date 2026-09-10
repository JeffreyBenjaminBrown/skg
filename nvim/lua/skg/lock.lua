-- PURPOSE: Lock skg buffers against edits during a save-in-progress,
-- and the single-flight stream guard. The Lua port of
-- elisp/skg-request-save/skg-lock-buffers.el.
--
-- The elisp locked with whole-buffer overlays carrying
-- modification-hooks, chosen there for a custom error message and for
-- interplay with inhibit-read-only. Neither concern maps here; the
-- lock is the 'modifiable' flag plus a buffer-local tag. DEVIATION:
-- an edit attempt in a locked buffer reports vim's generic "E21:
-- Cannot make changes, 'modifiable' is off" rather than "skg: buffer
-- locked -- save in progress".

local state = require('skg.state')

local M = {}

---Non-nil while a streaming request (save, rerender, diff-toggle) is
---in flight: a label string describing the operation. The server
---handles requests sequentially, so responses never interleave -- but
---the client could send a second request before the first finishes,
---and overlapping operations would corrupt each other's lock/unlock
---state. Cleared by terminal handlers, the sentinel, and the
---busy-initializing handler (via the connection-reset hook below).
---@type string|nil
M.stream_in_progress = nil
M.stream_owner = nil

---Mark a streaming operation as in progress; errors if another
---stream is already in flight.
---@param label string
function M.begin_stream (label, owner)
  if M.stream_in_progress then
    error(string.format('skg: %s blocked -- %s already in progress',
                        label, M.stream_in_progress)) end
  M.stream_in_progress = label
  M.stream_owner = owner
end

function M.end_stream (owner)
  if owner == nil or M.stream_owner == owner then
    M.stream_in_progress = nil
    M.stream_owner = nil
  end
end

function M.register_stream_request_cleanup (label, owner)
  state.set_request_failure_handler(function (reason)
    vim.notify(string.format('skg: %s interrupted: %s', label, reason),
               vim.log.levels.ERROR)
  end)
  state.set_request_finalizer(function ()
    M.end_stream(owner)
    M.unlock_all_save_locked(owner)
  end)
end

---@param buf integer
function M.lock_for_save (buf, owner)
  if not vim.b[buf].skg_save_locked then
    vim.b[buf].skg_pre_save_modifiable = vim.bo[buf].modifiable
    vim.b[buf].skg_save_locked = true
    vim.b[buf].skg_save_lock_owner = owner
    vim.bo[buf].modifiable = false
  end
end

---@param buf integer
function M.unlock_after_save (buf, owner)
  if vim.api.nvim_buf_is_valid(buf)
     and vim.b[buf].skg_save_locked
     and (owner == nil or vim.b[buf].skg_save_lock_owner == owner) then
    local prior = vim.b[buf].skg_pre_save_modifiable == true
    vim.b[buf].skg_save_locked = false
    vim.b[buf].skg_pre_save_modifiable = nil
    vim.b[buf].skg_save_lock_owner = nil
    if not vim.b[buf].skg_maintenance_epoch then
      vim.bo[buf].modifiable = prior end
  end
end

function M.lock_all_skg_buffers (owner)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.b[buf].skg_view_uri ~= nil then
      M.lock_for_save(buf, owner) end
  end
end

function M.unlock_all_save_locked (owner)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    M.unlock_after_save(buf, owner)
  end
end

---Unlock skg buffers whose uri is NOT in URI_LIST.
---@param uri_list string[]
function M.unlock_buffers_not_in_uri_list (uri_list, owner)
  local keep = {}
  for _, uri in ipairs(uri_list) do keep[uri] = true end
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.b[buf].skg_save_locked then
      local uri = vim.b[buf].skg_view_uri
      if uri and not keep[uri] then M.unlock_after_save(buf, owner) end
    end
  end
end

---Unlock skg buffers that are neither SAVED_URI nor in
---COLLATERAL_URIS (the keep-locked set is the collateral views plus
---the saved view itself).
---@param saved_uri string
---@param collateral_uris string[]|nil
function M.unlock_non_collateral_buffers (saved_uri, collateral_uris, owner)
  local keep = { saved_uri }
  for _, uri in ipairs(collateral_uris or {}) do
    table.insert(keep, uri) end
  M.unlock_buffers_not_in_uri_list(keep, owner)
end

return M
