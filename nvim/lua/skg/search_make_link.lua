-- PURPOSE: A workflow to search for text and insert a link to one of
-- the results: run from any buffer, it records the buffer and cursor,
-- searches, and in the results buffer a pick (<CR> or
-- <localleader>cc) kills the search, returns to the source position,
-- and inserts [[id:X][LABEL]] there. The Lua port of
-- elisp/skg-search-make-link.el.
--
-- PITFALL (inherited): do NOT save the source buffer eagerly before
-- the search. The server's rerender can collapse a freshly-typed
-- blank line, leaving the recorded position dangling on the next
-- headline, so the inserted link glues itself to the wrong spot.

local id_search = require('skg.id_search')
local search = require('skg.search')

local M = {}

---When set, the next search buffer that opens should become a
---link-creation buffer: {buf, cursor}. Cleared once consumed.
M.pending = nil

---Search; then pick a result to insert a link at the position this
---command was called from.
---@param search_terms string|nil
function M.search_make_link (search_terms)
  search_terms = search_terms
    or vim.fn.input('Search terms (for link insertion): ')
  if search_terms == nil or search_terms == '' then return end
  -- Track the buffer (not a file path: view buffers are not
  -- file-visiting) and the cursor position.
  M.pending = { buf = vim.api.nvim_get_current_buf(),
                cursor = vim.api.nvim_win_get_cursor(0) }
  search.search(search_terms)
end

---If a search-make-link was pending, configure the current (freshly
---populated search) buffer for it: record the target buffer-locally,
---bind the pick keys, and clear the pending state so a later plain
---search does not inherit it.
function M.maybe_activate_link_from_search_mode ()
  if not M.pending then return end
  local buf = vim.api.nvim_get_current_buf()
  vim.b[buf].skg_link_target_buf = M.pending.buf
  vim.b[buf].skg_link_target_cursor = M.pending.cursor
  M.pending = nil
  vim.keymap.set('n', '<localleader>cc', M.finish,
                 { buffer = buf,
                   desc = 'Pick this result; insert a link to it' })
  vim.keymap.set('n', '<CR>', M.finish,
                 { buffer = buf,
                   desc = 'Pick this result; insert a link to it' })
  vim.notify('Pick a result with <CR> (or <localleader>cc)'
             .. ' to insert a link')
end

---Pick the result at point: kill the search buffer, return to the
---source position, and insert the link, its label normalized so it
---never contains nested textlinks.
function M.finish ()
  local search_buf = vim.api.nvim_get_current_buf()
  local target_buf = vim.b[search_buf].skg_link_target_buf
  local target_cursor = vim.b[search_buf].skg_link_target_cursor
  if not target_buf then
    error('Not in a link-creation search buffer') end
  local found = id_search.nearest_id()
  if not found then error('No ID near point') end
  if not vim.api.nvim_buf_is_valid(target_buf) then
    error('Cannot insert link: source buffer no longer exists') end
  local label = id_search.replace_links_with_labels(found.label)
  vim.bo[search_buf].modified = false
  vim.api.nvim_buf_delete(search_buf, { force = true })
  vim.api.nvim_set_current_buf(target_buf)
  local line_count = vim.api.nvim_buf_line_count(target_buf)
  local line = math.min(target_cursor[1], line_count)
  local column = math.min(target_cursor[2],
                          #vim.api.nvim_buf_get_lines(
                            target_buf, line - 1, line, false)[1])
  vim.api.nvim_win_set_cursor(0, { line, column })
  vim.api.nvim_put(
    { string.format('[[id:%s][%s]]', found.id, label) },
    'c', false, true)
  vim.notify('Inserted link to ' .. found.id)
end

table.insert(search.search_buffer_setup_hooks,
             M.maybe_activate_link_from_search_mode)

return M
