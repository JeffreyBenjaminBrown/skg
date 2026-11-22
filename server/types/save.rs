//! What a save plans and then does. 'SavePlan' is a validated buffer's
//! plan: one 'NodeInstruction' per node to write ('SaveNode') or delete
//! ('DeleteNode'), plus the nodeMerges ('NodeMerge'), skgrepo moves
//! ('SkgrepoMove') and forks ('ForkSpec') the buffer asks for. Also
//! stale graphnodes ('UneditedStaleGraphnode'). Building a plan is
//! 'from_text.rs'; carrying it out is 'save.rs'; what a save that
//! fails says is 'types/errors.rs'.

use super::misc::{ID, SkgrepoChange, SkgrepoName};
use super::nodes::complete::Graphnode;


/////////////////
/// Types
/////////////////

/// When a user changes a node's skgrepo,
/// one of these is generated
/// (in addition to the usual NodeInstruction).
#[derive(Debug)]
pub struct SkgrepoMove {
  pub pid            : ID,
  pub skgrepo_change : SkgrepoChange,
}

/// Defines what to do with a single node: save it or delete it.
/// PITFALL: Don't merge the 'NodeMerge' type into this one.
/// It might seem natural, but there are places where you expect
/// a save or a delete and do not expect a nodeMerge. I tried it anyway.
/// The resulting pattern-matching and error-guarding was ugly.
/// (Maybe especially because a NodeMerge naturally consists of
/// two Saves and a Delete, neither of which it is reasonable
/// to represent with a NodeMerge.)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NodeInstruction {
  // PITFALL: Save(SaveNode) might smell funny, but consider that
  // some functions and type fields require specifically a SaveNode,
  // not a NodeInstruction.
  Save (SaveNode),
  Delete (DeleteNode),
}

/// A Save nodeInstruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SaveNode(pub Graphnode);

/// A Delete nodeInstruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DeleteNode {
  pub gnid: ID,
  pub home_skgrepo: SkgrepoName,
}

/// The whole plan a save applies to the graph, with no view attached: the
/// graph-mutation step consumes only this. (Named 'SavePlan', not e.g.
/// 'SaveInstructions', because the plan is whole -- a bare list of nodeInstructions
/// might not be.) The rerender step consumes the ViewForest -- returned
/// alongside this as the other half of 'buffer_to_validated_saveplan's pair,
/// not stored here -- plus this plan's PIDs, for collateral selection.
#[derive(Debug)]
pub struct SavePlan {
  pub node_instructions      : Vec<NodeInstruction>,
  /// Each merge request as (acquirer pid, acquiree pid). The NodeMerge
  /// triples are built from these only once the save's ordinary
  /// nodeInstructions have been prepared ('prepare_save_under_mutation_gate'),
  /// so that a nodeMerge carries the save's own edits to both graphnodes.
  pub nodeMerge_pairs        : Vec<(ID, ID)>,
  pub skgrepo_moves          : Vec<SkgrepoMove>,
  /// Forks detected this save: editing a foreign node N is read as a
  /// request to clone it. Held separate from 'node_instructions' because a
  /// save carrying forks is gated on the user's confirmation -- the
  /// clones are skgsave-committed only on approval (see ForkSpec, the save handler's
  /// fork-confirmation stage). Empty for an ordinary save.
  pub fork_specs         : Vec<ForkSpec>,
  /// Facts collected while interpreting the hidden-outside compound
  /// partner folder.  They are deliberately not warnings yet: the save
  /// handler turns them into user messages only after filesystem and
  /// graph mutation succeeds.
  pub post_skgsave_commit_notice_candidates : Vec<PostSkgsaveCommitNoticeCandidate>,
  /// The graphnodes the saved buffer shows stale but did not edit,
  /// whose Saves the plan left out ('without_unedited_stale_saves'
  /// in 'server/serve/handlers/save_staleness.rs'). The save handler
  /// warns of each once the save succeeds.
  pub unedited_stale_graphnodes : Vec<UneditedStaleGraphnode>,
  /// What the plan hides and stops hiding, and each removal from a
  /// forker that hides nothing, worded for the user
  /// ('hide_report_lines' in 'server/from_text/hide_report.rs'). The
  /// save's answer carries them as '(hides ...)', and so does a
  /// preview's, which writes nothing.
  pub hide_report : Vec<String>,
}

/// A graphnode a save left as the graph has it: stale and unedited
/// in the saved buffer ('docs/glossary.org', "baseline, stale,
/// edited, unedited").
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UneditedStaleGraphnode {
  pub pid                    : ID,
  pub title                  : String, // as the buffer shows it
  pub deleted_from_the_graph : bool, // else changed in it
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PostSkgsaveCommitNoticeCandidate {
  HiddenOutsideAdded { forker : ID, member : ID },
}

/// One fork: the user made a foreign node N (write-protected, in a skgrepo
/// they do not own) editable and edited it; that edit is read as a
/// request to clone N. 'clone' is the new owned node C, built from the
/// edited buffer node -- a fresh pid, an owned skgrepo, the edited
/// title/body/contains, 'forks = [N]', no hides. N itself is left untouched on
/// disk (its foreign SaveNode is dropped). The 'original_*' fields name
/// N, for the already-forked check and the confirmation buffer's display.
#[derive(Debug, Clone)]
pub struct ForkSpec {
  pub clone            : SaveNode,
  pub original_gnid   : ID,
  pub original_title   : String,
  pub original_skgrepo : SkgrepoName,
  /// True iff the clone's skgrepo was specified by the user (in the
  /// confirmation buffer, or via explicit skgrepos on N's new children
  /// in the saved metadata), as opposed to inferred or defaulted. A
  /// confirmed skgrepo renders as settled in the confirmation buffer;
  /// an unconfirmed one renders as the PICK-A-REPO placeholder plus
  /// a suggestion, and the client asks before approving.
  pub skgrepo_confirmed : bool,
}

/// When an 'acquiree' merges into an 'acquirer',
/// we need two SaveNodes and a DeleteNode.
#[derive(Debug, Clone)]
pub struct NodeMerge {
  pub acquiree_text_preserver : SaveNode, // new node with acquiree's title and body
  pub updated_acquirer        : SaveNode, // acquirer with acquiree's IDs, contents, and relationships merged in. (This is complex; see 'acquirer_after_nodeMerge'.)
  pub acquiree_to_delete      : DeleteNode,
}


/////////////////
/// Functions
/////////////////

impl NodeInstruction {
  pub fn is_delete (&self) -> bool {
    matches!(self, NodeInstruction::Delete (_))
  }

  pub fn is_save (&self) -> bool {
    matches!(self, NodeInstruction::Save (_))
  }

  /// The gnid this instruction saves or deletes: a Save's pid, a
  /// Delete's gnid.
  pub fn gnid (
    &self
  ) -> &ID {
    match self {
      NodeInstruction::Save (SaveNode (node))           => &node . pid,
      NodeInstruction::Delete (DeleteNode { gnid, .. }) => gnid, } }

  /// The graphnode a Save writes; None for a Delete.
  pub fn saved_graphnode (
    &self
  ) -> Option<&Graphnode> {
    match self {
      NodeInstruction::Save (SaveNode (node)) => Some (node),
      NodeInstruction::Delete (_)             => None, } }

  /// The gnid a Delete removes; None for a Save.
  pub fn deleted_gnid (
    &self
  ) -> Option<&ID> {
    match self {
      NodeInstruction::Delete (DeleteNode { gnid, .. }) => Some (gnid),
      NodeInstruction::Save (_)                         => None, } }

  /// Split a slice of NodeInstructions into (deletes, saves),
  /// cloning each item.
  pub fn partition_save_and_delete (
    node_defs : &[NodeInstruction]
  ) -> ( Vec<DeleteNode>, Vec<SaveNode> ) {
    use itertools::{Itertools, Either};
    node_defs . iter () . cloned () . partition_map (
      |instr| match instr {
        NodeInstruction::Delete (d) => Either::Left (d),
        NodeInstruction::Save (s)   => Either::Right (s) } )
  }
}

impl From<SaveNode> for NodeInstruction {
  fn from(save: SaveNode) -> Self {
    NodeInstruction::Save (save)
  }
}

impl From<DeleteNode> for NodeInstruction {
  fn from(del: DeleteNode) -> Self {
    NodeInstruction::Delete (del)
  }
}

impl NodeMerge {
  pub fn to_vec (
    &self
  ) -> Vec<NodeInstruction> {
    vec![
      self . acquiree_text_preserver . clone() . into(),
      self . updated_acquirer . clone() . into(),
      self . acquiree_to_delete . clone() . into(),
    ] }

  pub fn acquirer_gnid (
    &self
  ) -> &ID {
    &self . updated_acquirer . 0 . pid
  }

  pub fn acquiree_gnid (
    &self
  ) -> &ID {
    &self . acquiree_to_delete . gnid
  }

  /// Extracts the three targets from a NodeMerge:
  /// - acquiree_text_preserver -> &Graphnode
  /// - updated_acquirer -> &Graphnode
  /// - acquiree_to_delete -> (&ID, &RepoName)
  pub fn targets_from_nodeMerge (
    &self
  ) -> (&Graphnode, &Graphnode, (&ID, &SkgrepoName)) {
    ( &self . acquiree_text_preserver . 0,
      &self . updated_acquirer . 0,
      (&self . acquiree_to_delete . gnid, &self . acquiree_to_delete . home_skgrepo) )
  }
}
