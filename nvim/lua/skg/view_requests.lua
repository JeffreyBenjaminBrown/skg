-- PURPOSE: Commands that request viewbranches by adding a
-- (viewRequests ...) atom to the headline at point and saving,
-- letting the server fulfill the request during completion. Two
-- families, both auto-saving: folders (folder RELNAME), building
-- both folders of the relation, and role trees (roleTree ROLENAME), the role tree
-- for one partner role. Also the editable-view and write-protect
-- requests (no auto-save) and the explicit fork. The Lua port of
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
  { 'show_folderOf_overrides', '(folder overrides)' },
  { 'show_folderOf_hidesFromSubs', '(folder hidesFromSubs)' },
  { 'show_folderOf_subscribesTo', '(folder subscribesTo)' },
  { 'show_folderOf_flags', 'flags' },
  { 'show_folderOf_containers', '(folder containers)' },
  { 'show_folderOf_mentioners', '(folder mentioners)' },
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

---Request an editable view for the headline at point. On a
---write-protected headline, the save makes it the node's editable
---occurrence and write-protects the one that was, which keeps what it
---shows. On an editable headline, the save extends its content
---editable, up to the server's initial_node_limit extensions. A buffer
---may carry one such request per node. Does not auto-save.
function M.set_editable ()
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  metadata.edit_metadata_at_line(line,
    sexpr.read('(skg (node (viewRequests editableView)))'))
end

---Request write-protection for the headline at point. At the next
---save, the occurrence's own edits are saved first; then the server
---makes it write-protected, resyncing its title from the graph, and
---makes each of its direct children not affect it, keeping their own
---editability and subtrees. A no-op on a write-protected headline.
---Requesting an editable view (set_editable) of another occurrence of
---the node in the same save moves editability there. Does not
---auto-save.
function M.request_write_protected ()
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  metadata.edit_metadata_at_line(line,
    sexpr.read('(skg (node (viewRequests writeProtectedView)))'))
end

---Request write-protection for the headline at point and its whole
---content subtree. At the next save, after the subtree's own edits
---are saved, the server makes the occurrence and every node of its
---content subtree write-protected, each child under them not
---affecting its parent. Does not auto-save.
function M.request_write_protected_recursive ()
  local line = focus.owning_headline_line()
  if not line then error('Not on a headline') end
  metadata.edit_metadata_at_line(line,
    sexpr.read('(skg (node (viewRequests writeProtectedSubtreeView)))'))
end

---Fork the (owned) node at point: create a private clone that
---overrides it. Refuses on a modified buffer, so the clone's saved
---saved state matches what is on screen; otherwise stamps (viewRequests
---fork) and auto-saves. The server answers with the usual
---fork-confirmation buffer.
function M.fork_node ()
  if vim.bo.modified then
    vim.notify('Save the buffer before forking.')
    return end
  M.request_view_and_save('fork')
end

return M
