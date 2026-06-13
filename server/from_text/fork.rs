//! Fork support. Editing a foreign node N (one in a source the user
//! does not own) is read as a request to CLONE it: the clone C lives in
//! an owned source, copies N's edited title/body/contains, subscribes
//! to N and overrides N. N itself is left untouched.
//!
//! Detection happens in 'apply_foreign_policy' (validate.rs): a foreign
//! SaveNode whose buffer content differs from disk is a fork candidate
//! rather than a 'ModifiedForeignNode' error. This module resolves the
//! clone's owned source (from N's nearest owned ancestor in the view)
//! and builds C's SaveNode. The confirmation-gating and the commit live
//! in the save handler.

use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{ForkSpec, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewNodeKind, Vognode};

use std::collections::HashMap;

/// For every FOREIGN vognode in the view, the source of its nearest
/// OWNED vognode ancestor, if any. A fork's clone C must live in an
/// owned source; the foreign node N's own source is read-only, so C
/// inherits from N's nearest owned ancestor in the view (the same
/// "inherit a source from the parent" idea 'add_missing_info' uses, but
/// skipping foreign ancestors). A foreign node drawn at several
/// positions can have different owned ancestors; the first reached in
/// preorder wins -- monogamy means at most one clone anyway, and the
/// user can override the choice in the confirmation buffer.
pub fn owned_ancestor_sources_for_foreign_vognodes (
  viewforest : &ViewForest,
  config     : &SkgConfig,
) -> HashMap<ID, SourceName> {
  let mut map : HashMap<ID, SourceName> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewNodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if config . user_owns_source (& t . source) { continue; } // not foreign
    let mut current = node;
    while let Some (parent) = current . parent () {
      if let ViewNodeKind::Vognode (Vognode::Active (pt))
        = & parent . value () . kind
      { if config . user_owns_source (& pt . source) {
          map . entry ( t . id . clone () )
            . or_insert_with ( || pt . source . clone () );
          break; }}
      current = parent; }}
  map }

/// Build the ForkSpec for a fork of the foreign buffer node N: resolve
/// the clone's owned source from the nearest-owned-ancestor map, then
/// construct C. Errors with 'ForkSourceUnresolved' if N has no owned
/// ancestor (the confirmation buffer is where the user then sets one).
pub fn fork_spec_from_buffer_node (
  buffer_node           : &NodeComplete,
  owned_ancestor_source : &HashMap<ID, SourceName>,
) -> Result<ForkSpec, BufferValidationError> {
  let clone_source : SourceName =
    owned_ancestor_source . get (& buffer_node . pid) . cloned ()
    . ok_or_else ( || BufferValidationError::ForkSourceUnresolved (
        buffer_node . pid . clone () )) ?;
  Ok ( build_fork_clone (buffer_node, clone_source) ) }

/// Construct the clone C from the edited foreign buffer node N and a
/// resolved owned source. C copies N's title/body/contains (the
/// edited buffer values -- N is already disk-supplemented, so no disk
/// fetch is needed, unlike nodeMerge whose acquiree is only an ID
/// reference), subscribes_to = [N] and overrides_view_of = [N], no
/// hides (the prerequisite display rule excludes C's own contains from
/// its subscribee-as-such view), a fresh pid, and the owned source.
/// contains is stored RAW (the child IDs as the buffer collected them);
/// override substitution applies at render time.
pub fn build_fork_clone (
  buffer_node  : &NodeComplete,
  clone_source : SourceName,
) -> ForkSpec {
  let clone : NodeComplete = NodeComplete {
    title         : buffer_node . title . clone (),
    aliases       : MSV::Unspecified,
    source        : clone_source,
    pid           : ID ( uuid::Uuid::new_v4 () . to_string () ),
    extra_ids     : Vec::new (),
    body          : buffer_node . body . clone (),
    contains      : buffer_node . contains . clone (),
    subscribes_to : MSV::Specified ( vec! [ buffer_node . pid . clone () ] ),
    hides_from_its_subscriptions : MSV::Specified ( Vec::new () ),
    overrides_view_of : MSV::Specified ( vec! [ buffer_node . pid . clone () ] ),
    misc          : Vec::new (),
  };
  ForkSpec {
    clone           : SaveNode (clone),
    original_id     : buffer_node . pid . clone (),
    original_title  : buffer_node . title . clone (),
    original_source : buffer_node . source . clone (),
  }}
