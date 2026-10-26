//! The refresh's diff-view mode. Outside the diff view every function
//! here answers None or does nothing; the mode itself is not written
//! yet, so for now it does nothing in the diff view either.

use crate::refresh::Walk;
use crate::refresh::goal::Goal;
use crate::types::misc::ID;
use crate::types::viewnode::{Folder, Viewnode, ViewnodeKind};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;

use ego_tree::{NodeId, Tree};
use std::error::Error;

pub(crate) fn content_goal (
  _pid          : &ID,
  _unintegrated : &dyn Fn (Vec<ID>) -> Vec<ID>,
  _walk         : &Walk,
) -> Option<Goal> { None }

pub(crate) fn chain_goal (
  _pid  : &ID,
  _walk : &Walk,
) -> Option<Goal> { None }

pub(crate) fn ghost_content_goal (
  _pid  : &ID,
  _walk : &Walk,
) -> Option<Goal> { None }

pub(crate) fn folder_goal (
  _kind        : &Folder,
  _anchor      : &ID,
  _member_role : RelationRole,
  _walk        : &Walk,
) -> Option<Goal> { None }

pub(crate) fn property_goal (
  _kind : &Folder,
  _pid  : &ID,
  _walk : &Walk,
) -> Option<Goal> { None }

pub(crate) fn acquisition_goal (
  _tree   : &Tree<Viewnode>,
  _folder : NodeId,
  _kind   : &Folder,
  _walk   : &Walk,
) -> Option<Goal> { None }

/// Whether the change viewnode, or acquisition folder, at 'child' no
/// longer stands for a change: in a content view, always.
pub(crate) fn change_is_gone (
  tree  : &Tree<Viewnode>,
  child : NodeId,
  _walk : &Walk,
) -> bool {
  matches! ( &tree . get (child) . unwrap () . value () . kind,
             ViewnodeKind::ChangeViewnode (_)
             | ViewnodeKind::Folder (Folder::Acquired | Folder::AcquiredBy) ) }

pub(crate) fn draw_change_viewnodes (
  _tree   : &mut Tree<Viewnode>,
  _parent : NodeId,
  _walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> { Ok (( )) }

pub(crate) fn tag_children (
  _tree   : &mut Tree<Viewnode>,
  _parent : NodeId,
  _goal   : Option<&Goal>,
  _walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> { Ok (( )) }

pub(crate) fn fold_body_of_drawn_occurrence (
  _viewnode : &mut Viewnode,
  _pid      : &ID,
  _walk     : &Walk,
) {}
