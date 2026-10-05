-- PURPOSE: Commands that request additional views by adding a
-- (viewRequests ...) atom to the headline at point and saving,
-- letting the server fulfill the request during completion. Two
-- families, both auto-saving: FOLDERS (folder RELNAME), building
-- both folders of the relation, and ROLE TREES (roleTree ROLENAME), the role tree
-- for one partner role. Also the definitive-view request (no
-- auto-save) and the explicit fork. The Lua port of
-- elisp/skg-request-views.el.

local focus = require('skg.focus')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

---Request VIEW_REQUEST_TEXT (e.g. '(folder aliases)', '(roleTree container)',
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
  { 'show_folderOf_aliases', '(folder aliases)' },
  { 'show_folderOf_overridesViewOf', '(folder overrides_view_of)' },
  { 'show_folderOf_hidesFromItsSubscriptions', '(folder hides_from_its_subscriptions)' },
  { 'show_folderOf_subscribesTo', '(folder subscribes_to)' },
  { 'show_folderOf_flags', 'flags' },
  { 'show_containerward_tree', '(roleTree container)' },
  { 'show_mentionerward_tree', '(roleTree mentioner)' },
  { 'show_mentionedward_tree', '(roleTree mentioned)' },
  { 'show_overriderward_tree', '(roleTree overrider)' },
  { 'show_overriddenward_tree', '(roleTree overridden)' },
  { 'show_hiderward_tree', '(roleTree hider)' },
  { 'show_hiddenward_tree', '(roleTree hidden)' },
  { 'show_subscriberward_tree', '(roleTree subscriber)' },
  { 'show_subscribeeward_tree', '(roleTree subscribee)' },
}
for _, row in ipairs(command_rows) do
  local name, form = row[1], row[2]
  M[name] = function () M.request_view_and_save(form) end
end

---Request a definitive view for the headline at point (the node must
---be write-protected and childless). Does NOT auto-save.
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
