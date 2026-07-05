-- PURPOSE: Commands that request additional views by adding a
-- (viewRequests ...) atom to the headline at point and saving,
-- letting the server fulfill the request during completion. Two
-- families, both auto-saving: COLLECTIONS (col RELNAME), building
-- both cols of the relation, and PATHS (path ROLENAME), the backpath
-- for one partner role. Also the definitive-view request (no
-- auto-save) and the explicit fork. The Lua port of
-- elisp/skg-request-views.el.

local focus = require('skg.focus')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Request VIEW_REQUEST_TEXT (e.g. '(col aliases)', '(path container)',
---'fork') for the headline owning point, then save.
---@param view_request_text string
function M.request_view_and_save (view_request_text)
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  metadata.edit_metadata_at_line(line, sexpr.read(string.format(
    '(skg (node (viewRequests %s)))', view_request_text)))
  require('skg.save').request_save_buffer()
end

---The two command families, generated like the elisp macro did.
local command_rows = {
  { 'show_collection_aliases', '(col aliases)' },
  { 'show_collection_overrides', '(col overrides)' },
  { 'show_collection_hides', '(col hides)' },
  { 'show_collection_subscribes', '(col subscribes)' },
  { 'show_paths_through_containers', '(path container)' },
  { 'show_paths_through_link_sources', '(path linkSource)' },
  { 'show_paths_through_link_dests', '(path linkDest)' },
  { 'show_paths_through_overriders', '(path overrider)' },
  { 'show_paths_through_overridden', '(path overridden)' },
  { 'show_paths_through_hiders', '(path hider)' },
  { 'show_paths_through_hidden', '(path hidden)' },
  { 'show_paths_through_subscribers', '(path subscriber)' },
  { 'show_paths_through_subscribees', '(path subscribee)' },
}
for _, row in ipairs(command_rows) do
  local name, form = row[1], row[2]
  M[name] = function () M.request_view_and_save(form) end
end

---Request a definitive view for the headline at point (the node must
---be indefinitive and childless). Does NOT auto-save.
function M.set_definitive ()
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  metadata.edit_metadata_at_line(line,
    sexpr.read('(skg (node (viewRequests definitiveView)))'))
end

---Fork the (owned) node at point: create a private clone that
---overrides it. Refuses on a modified buffer, so the clone's saved
---snapshot matches what is on screen; otherwise stamps (viewRequests
---fork) and auto-saves. The server answers with the usual
---fork-confirmation buffer.
function M.fork_node ()
  if vim.bo.modified then
    vim.notify('Save the buffer before forking.')
    return end
  M.request_view_and_save('fork')
end

return M
