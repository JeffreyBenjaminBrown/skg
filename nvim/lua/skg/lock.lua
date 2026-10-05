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

---Mark a streaming operation as in progress; errors if another
---stream is already in flight.
---@param label string
function M.begin_stream (label)
  if M.stream_in_progress then
    error(string.format('skg: %s blocked -- %s already in progress',
                        label, M.stream_in_progress)) end
  M.stream_in_progress = label
end

function M.end_stream ()
  M.stream_in_progress = nil
end

---@param buf integer
function M.lock_for_save (buf)
  if not vim.b[buf].skg_save_locked then
    vim.b[buf].skg_save_locked = true
    vim.bo[buf].modifiable = false
  end
end

---@param buf integer
function M.unlock_after_save (buf)
  if vim.api.nvim_buf_is_valid(buf)
     and vim.b[buf].skg_save_locked then
    vim.b[buf].skg_save_locked = false
    vim.bo[buf].modifiable = true
  end
end

function M.lock_all_skg_buffers ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.b[buf].skg_view_id ~= nil then
      M.lock_for_save(buf) end
  end
end

function M.unlock_all_save_locked ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    M.unlock_after_save(buf)
  end
end

---Unlock skg buffers whose view_id is NOT in VIEW_ID_LIST.
---@param view_id_list string[]
function M.unlock_buffers_not_in_view_id_list (view_id_list)
  local keep = {}
  for _, view_id in ipairs(view_id_list) do keep[view_id] = true end
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.b[buf].skg_save_locked then
      local view_id = vim.b[buf].skg_view_id
      if view_id and not keep[view_id] then M.unlock_after_save(buf) end
    end
  end
end

---Unlock skg buffers outside SAVED_VIEW_ID and the narrowed keep-set.
---COLLATERAL_VIEW_IDS is historical naming: it can also contain dirty views whose
---buffer snapshots were inputs to the conflict check.
---@param saved_view_id string
---@param collateral_view_ids string[]|nil
function M.unlock_non_collateral_buffers (saved_view_id, collateral_view_ids)
  local keep = { saved_view_id }
  for _, view_id in ipairs(collateral_view_ids or {}) do
    table.insert(keep, view_id) end
  M.unlock_buffers_not_in_view_id_list(keep)
end

-- A server crash or busy-initializing teardown mid-save must not
-- leave buffers permanently locked: join the connection-reset hooks
-- the client runs from its sentinel.
table.insert(state.connection_reset_hooks, function ()
  M.end_stream()
  M.unlock_all_save_locked()
end)

return M
