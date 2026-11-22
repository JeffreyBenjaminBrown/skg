-- PURPOSE: In the diff view, ask the next save to bring back what a
-- change removed ('docs/diff-view.org', "Restoring what a change
-- removed"): 'restore_deleted_node' (':SkgRestoreDeletedNode',
-- <localleader>sn) writes back the deleted node at point (a ghost),
-- and 'restore_removed_membership' (':SkgRestoreRemovedMembership',
-- <localleader>sl) re-adds the removed relationship at point; neither
-- saves. 'restoreRel_position_at_line_p' says where a restoreRel may
-- stand, as the save checks it, and 'removed_membership_p' whether a
-- headline carries a removed membership, so that the metadata
-- editor's cycle (skg.sexpr_edit.sexpr_cycling) offers restoreRel.
-- Writing the request into the metadata is skg.metadata's
-- ('write_edit_request'). The Lua port of
-- elisp/skg-restore-requests.el.
--
-- The server refuses the requests on the wrong kind of headline with
-- the same texts as the refusals below, except that its refusal of a
-- restoreRel without a removed membership goes on to say where a
-- restoreRel belongs; Neovim shows them naming its own commands
-- (messages.with_neovim_bindings).

local compare = require('skg.sexpr.compare')
local messages = require('skg.messages')
local metadata = require('skg.metadata')
local sexpr = require('skg.sexpr.parse')

local M = {}

local SKG = sexpr.symbol('skg')

M.restore_rel_on_a_deleted_node = messages.with_neovim_bindings(
  'This node is deleted; restore it first (skg-restore-deleted-node)')
M.restore_node_on_a_node_not_deleted = messages.with_neovim_bindings(
  'Only a node deleted since HEAD can be restored;'
  .. ' restoreNode belongs on a ghost (skg-restore-deleted-node)')
M.restore_rel_without_a_removed_membership =
  'Only a removed membership can be restored'

---@param metadata_sexp any
---@return boolean is it a ghost: a node HEAD has and the worktree
---lacks, drawn only in the diff view?
function M.ghost_sexp_p (metadata_sexp)
  return metadata_sexp ~= nil
         and compare.subtree_p(metadata_sexp,
                               { SKG, sexpr.symbol('ghost') })
end

---@param metadata_sexp any
---@return boolean is it an occurrence whose '(diff ...)' says
---its relationship was removed? Whether a restoreRel may sit on it
---depends also on where it stands (M.restoreRel_position_at_line_p).
function M.removed_membership_p (metadata_sexp)
  return metadata.occurrence_sexp_p(metadata_sexp)
         and metadata.relationship_removed_p(metadata_sexp)
end

---Is the headline at LINE_NUMBER a node of the containerward chain as
---the diff view, whose request restoreRel is, draws it: a vognode
---claiming nothing under a member of a partner folder whose members
---carry the chain there (every one but the two acquisition folders), or under
---another such node ('stands_in_containerward_chain',
---server/types/tree/forest.rs)?
---@param line_number integer
---@return boolean
function M.containerward_chain_node_at_line_p (line_number)
  local line = line_number
  while true do
    local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(line)
    if not (metadata.vognode_sexp_p(metadata_sexp)
            and (not metadata.occurrence_sexp_p(metadata_sexp)
                 or not metadata.claims_membership_p(metadata_sexp))) then
      return false end
    local parent = metadata.parent_line(line)
    if not (parent
            and metadata.vognode_sexp_p(
                  metadata.metadata_sexp_at_line_or_nil(parent)))
    then return false end
    local grandparent = metadata.parent_line(parent)
    local grandparent_folder = grandparent
      and metadata.partner_folder_atom(
            metadata.metadata_sexp_at_line_or_nil(grandparent))
    if grandparent_folder
       and grandparent_folder ~= 'acquiredByFolder'
       and grandparent_folder ~= 'acquiredFolder'
       and grandparent_folder ~= 'illegalEditFolder' then
      return true end
    line = parent
  end
end

---Does the headline at LINE_NUMBER (default: point) stand where a
---restoreRel may: under a partner folder (whose removed members
---claim nothing), or as a removed member, a child claiming nothing under a vognode
---that is no node of the containerward chain? The save checks the
---same ('restoreRel_stands_where_it_may',
---server/from_text/buffer_to_viewnodes/local.rs).
---@param line_number integer|nil
---@return boolean
function M.restoreRel_position_at_line_p (line_number)
  local line = line_number or metadata.current_line_number()
  local parent = metadata.parent_line(line)
  if not parent then return false end
  local parent_sexp = metadata.metadata_sexp_at_line_or_nil(parent)
  if metadata.partner_folder_atom(parent_sexp) then return true end
  if metadata.vognode_sexp_p(parent_sexp) then
    return not metadata.claims_membership_p(
             metadata.metadata_sexp_at_line_or_nil(line))
           and not M.containerward_chain_node_at_line_p(line)
  end
  return false
end

---Ask the next save to write back the deleted node at point, a
---ghost: its file as HEAD had it, and its place in the
---lists of the nodes that recorded it at HEAD. Does not save.
function M.restore_deleted_node ()
  if not M.ghost_sexp_p(metadata.metadata_sexp_at_line_or_nil(nil)) then
    error(M.restore_node_on_a_node_not_deleted, 0) end
  metadata.write_edit_request(sexpr.symbol('restoreNode'), 'ghost')
  vim.notify('Restore request set. Save to apply.')
end

---Ask the next save to re-add the removed relationship at point: a
---removed member's, or a folder member's carrying '(rel removed)', as
---HEAD recorded it; which of the two it is, and whether it is either,
---depends on where it stands (M.restoreRel_position_at_line_p). Does not save.
function M.restore_removed_membership ()
  local metadata_sexp = metadata.metadata_sexp_at_line_or_nil(nil)
  if M.ghost_sexp_p(metadata_sexp) then
    error(M.restore_rel_on_a_deleted_node, 0) end
  if not (M.removed_membership_p(metadata_sexp)
          and M.restoreRel_position_at_line_p(nil)) then
    error(M.restore_rel_without_a_removed_membership, 0) end
  metadata.write_edit_request(sexpr.symbol('restoreRel'), 'node')
  vim.notify('Restore request set. Save to apply.')
end

return M
