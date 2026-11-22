//! Which changed nodes a commit takes ('CommitChoice',
//! 'docs/diff-view.org', "Stage and commit"): every changed node, or
//! every processed one less those held back because they need a node
//! that does not go in ('hold_back_waiting_nodes'). The changed nodes
//! and whether each is processed come from the change-forest
//! ('change_leaves.rs').

use crate::dbs::in_rust_graph::InRustGraph;
use crate::diff_view::change_leaves::{
  ChangeLeaves, reresolved_extra_ids_of, title_in_straddle};
use crate::diff_view::processed_store::ProcessedStore;
use crate::graph_straddle::types::GraphStraddle;
use crate::graph_straddle::union_walk::ROLES_OF_LISTED_MEMBERS;
use crate::types::config::SkgConfig;
use crate::types::misc::ID;

use std::collections::{BTreeSet, HashMap};

/// Why a processed node waits: it needs a node that does not go in.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum WaitReason {
  /// Its worktree names, in one of its four lists, a new node.
  NamesNew,
  /// It is deleted, and another node names it at HEAD, in one of its
  /// four lists, but not in the worktree.
  StillNamed,
  /// It is a part of a merge ('merge_needs').
  Merge,
}

impl WaitReason {
  /// The reason as the answer to a commit names it.
  pub fn wire_name (
    self,
  ) -> &'static str {
    match self {
      WaitReason::NamesNew   => "names-new",
      WaitReason::StillNamed => "still-named",
      WaitReason::Merge      => "merge", } }
}

/// A processed node held back, why, and the node that does not go in
/// which it waits for.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WaitingNode {
  pub pid       : ID,
  pub reason    : WaitReason,
  pub waits_for : ID,
}

/// Which changed nodes a commit takes.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CommitChoice {
  /// 'ChangeLeaves::changed_nodes'.
  pub changed  : BTreeSet<ID>,
  /// Those of 'changed' that are processed ('node_is_processed').
  pub processed : BTreeSet<ID>,
  /// Those that go in.
  pub going_in : BTreeSet<ID>,
  /// The processed nodes held back, in pid order.
  pub waiting  : Vec<WaitingNode>,
  /// Those of 'changed' whose every leaf is marked but one of whose
  /// files is not what skg writes for it
  /// ('files_hold_more_than_their_node'), in pid order: a change no
  /// mark covers, so not processed, and not going in.
  pub unmarkable : Vec<ID>,
}

impl CommitChoice {
  /// The changed nodes that do not go in.
  pub fn left_out (
    &self,
  ) -> BTreeSet<ID> {
    self . changed . difference (&self . going_in) . cloned () . collect () }
}

/// The commit of the processed nodes: every processed changed node,
/// less those that wait ('hold_back_waiting_nodes'). A node among
/// 'files_not_as_skg_writes_them' ('files_hold_more_than_their_node')
/// is not processed, whatever its marks: its files may hold a change
/// no mark covers.
pub fn processed_commit_choice (
  config                      : &SkgConfig,
  straddle                    : &GraphStraddle,
  store                       : &ProcessedStore,
  files_not_as_skg_writes_them : &BTreeSet<ID>,
) -> CommitChoice {
  let leaves : &ChangeLeaves = straddle . change_leaves ();
  let changed : BTreeSet<ID> = leaves . changed_nodes (config);
  let (unmarkable, processed) : (Vec<ID>, Vec<ID>) = changed . iter ()
    . filter ( |pid| leaves . node_is_processed (store, pid) )
    . cloned ()
    . partition ( |pid| files_not_as_skg_writes_them . contains (pid) );
  let processed : BTreeSet<ID> = processed . into_iter () . collect ();
  let (going_in, waiting) : (BTreeSet<ID>, Vec<WaitingNode>) =
    hold_back_waiting_nodes (straddle, &changed, processed . clone ());
  CommitChoice { changed, processed, going_in, waiting, unmarkable } }

/// The commit of every changed node, processed or not. Nothing waits:
/// whatever a node could need goes in with it.
pub fn whole_commit_choice (
  config   : &SkgConfig,
  straddle : &GraphStraddle,
  store    : &ProcessedStore,
) -> CommitChoice {
  let leaves : &ChangeLeaves = straddle . change_leaves ();
  let changed : BTreeSet<ID> = leaves . changed_nodes (config);
  CommitChoice {
    processed : changed . iter ()
      . filter ( |pid| leaves . node_is_processed (store, pid) )
      . cloned () . collect (),
    going_in  : changed . clone (),
    changed,
    waiting   : Vec::new (),
    unmarkable : Vec::new (), } }

/// Of 'candidates', those that go in, and those held back because they
/// need a node that does not: one of 'changed' outside the set going
/// in. The rule is applied in rounds until a round holds nothing
/// back; each round only removes, so it ends, and a node that needs a
/// held-back node is held back too. Each held-back node names the
/// first, by title, of the nodes it needs that were not going in when
/// its round began; for a merge, an unprocessed one (outside
/// 'candidates') if there is one. A node that 'changed' lacks is
/// unchanged in the owned skgrepos, so its HEAD there is its worktree,
/// and nothing waits for it.
pub fn hold_back_waiting_nodes (
  straddle   : &GraphStraddle,
  changed    : &BTreeSet<ID>,
  candidates : BTreeSet<ID>,
) -> (BTreeSet<ID>, Vec<WaitingNode>) {
  let needs : HashMap<ID, BTreeSet<ID>> = merge_needs (straddle, changed);
  let processed : BTreeSet<ID> = candidates . clone ();
  let mut going_in : BTreeSet<ID> = candidates;
  let mut waiting : Vec<WaitingNode> = Vec::new ();
  loop {
    let held_back : Vec<WaitingNode> = going_in . iter ()
      . filter_map ( |pid| why_it_waits (
        straddle, changed, &processed, &going_in, needs . get (pid), pid ))
      . collect ();
    if held_back . is_empty () { break; }
    for node in &held_back { going_in . remove (&node . pid); }
    waiting . extend (held_back); }
  waiting . sort_by ( |a, b| a . pid . cmp (&b . pid) );
  (going_in, waiting) }

/// Why 'pid' cannot go in while only 'going_in' does, if it cannot:
/// the first of the three reasons that applies. 'merge_needed' are the
/// nodes it needs as a part of a merge ('merge_needs').
fn why_it_waits (
  straddle     : &GraphStraddle,
  changed      : &BTreeSet<ID>,
  processed    : &BTreeSet<ID>,
  going_in     : &BTreeSet<ID>,
  merge_needed : Option<&BTreeSet<ID>>,
  pid          : &ID,
) -> Option<WaitingNode> {
  let left_behind = |needed : &mut dyn Iterator<Item = &ID>| -> Vec<ID> {
    needed . filter ( |needed| *needed != pid
                       && changed . contains (*needed)
                       && ! going_in . contains (*needed) )
      . cloned () . collect () };
  let waits = |reason : WaitReason, waits_for : ID| -> WaitingNode {
    WaitingNode { pid : pid . clone (), reason, waits_for } };
  first_by_title ( straddle,
                   left_behind ( &mut new_nodes_named_by (straddle, pid) . iter () )
                   . into_iter () )
    . map ( |needed| waits (WaitReason::NamesNew, needed) )
    . or_else ( || first_by_title ( straddle,
                  left_behind ( &mut nodes_still_naming (straddle, pid) . iter () )
                  . into_iter () )
                . map ( |needed| waits (WaitReason::StillNamed, needed) ))
    . or_else ( || {
      let behind : Vec<ID> =
        left_behind ( &mut merge_needed . into_iter () . flatten () );
      let unprocessed : Vec<ID> = behind . iter ()
        . filter ( |needed| ! processed . contains (*needed) )
        . cloned () . collect ();
      first_by_title ( straddle,
                       if unprocessed . is_empty () { behind } else { unprocessed }
                       . into_iter () )
        . map ( |needed| waits (WaitReason::Merge, needed) ) } ) }

/// The new nodes (in the after-graph only) that 'pid' names in the
/// after-graph, in any of its four lists.
fn new_nodes_named_by (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<ID> {
  ROLES_OF_LISTED_MEMBERS . iter ()
    . flat_map ( |role|
      straddle . after . outbound_gnids_for_relation (pid, role . relation) )
    . filter_map ( |gnid| straddle . after . pid_of (&gnid) )
    . filter ( |member| ! straddle . before . nodes . contains_key (member) )
    . collect () }

/// When 'pid' is deleted, the nodes that name it at HEAD, in any of
/// their four lists, by an ID their worktree's list lacks. Committing
/// the deletion without them would leave HEAD naming a node it lacks.
/// One that names it in the worktree too names a node the worktree
/// lacks already, so committing the deletion changes nothing there.
fn nodes_still_naming (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<ID> {
  if straddle . after . nodes . contains_key (pid)
    || ! straddle . before . nodes . contains_key (pid) {
    return Vec::new (); }
  let mut naming : Vec<ID> = Vec::new ();
  for role in ROLES_OF_LISTED_MEMBERS {
    for recorder in straddle . before . inbound_pids_for_relation (pid, role . relation) {
      let in_worktree : Vec<ID> =
        straddle . after . outbound_gnids_for_relation (&recorder, role . relation);
      if straddle . before . outbound_gnids_for_relation (&recorder, role . relation)
        . iter ()
        . any ( |gnid| straddle . before . pid_of (gnid) . as_ref () == Some (pid)
                       && ! in_worktree . contains (gnid) ) {
        naming . push (recorder); }} }
  naming }

/// What each part of each merge since HEAD needs, among changed nodes
/// only. A merge is found by an ID that the two graphs resolve to
/// different nodes and that is an extra ID, in one graph only, of a
/// changed node ('reresolved_extra_ids_of'). Its core is that node,
/// the node whose pid is the ID, and the node each graph resolves the
/// ID to (the acquirer and the acquiree, or the two holders of an
/// extra ID moved from one node to another):
/// the acquirer alone would hold one ID as both a primary and an
/// extra ID, and the acquiree's deletion alone would leave the ID
/// dangling, so each needs the other. The nodes listing the ID itself,
/// in one of their four lists ('recorders_listing'), resolve it
/// through whichever of the core is committed:
/// - one listing it in both graphs needs the core, and the core needs it;
/// - one listing it at HEAD only needs nothing, but the core needs it,
///   since HEAD's list would resolve the ID anew;
/// - one listing it in the worktree only needs the core, which does not
///   need it: left out, it is absent from the commit.
fn merge_needs (
  straddle : &GraphStraddle,
  changed  : &BTreeSet<ID>,
) -> HashMap<ID, BTreeSet<ID>> {
  let mut needs : HashMap<ID, BTreeSet<ID>> = HashMap::new ();
  let mut add = |needer : &ID, needed : &BTreeSet<ID>| {
    needs . entry ( needer . clone () ) . or_default () . extend (
      needed . iter () . filter ( |pid| *pid != needer ) . cloned () ); };
  for pid in changed {
    for gnid in reresolved_extra_ids_of (straddle, pid) {
      let core : BTreeSet<ID> =
        [ Some ( pid . clone () ), Some ( gnid . clone () ),
          straddle . before . pid_of (&gnid), straddle . after . pid_of (&gnid) ]
        . into_iter () . flatten ()
        . filter ( |member| changed . contains (member) ) . collect ();
      let listing_in = |graph : &InRustGraph| -> BTreeSet<ID> {
        recorders_listing (graph, &gnid) . into_iter ()
          . filter ( |recorder| changed . contains (recorder) ) . collect () };
      let at_head  : BTreeSet<ID> = listing_in (&straddle . before);
      let in_worktree : BTreeSet<ID> = listing_in (&straddle . after);
      let core_needs : BTreeSet<ID> =
        core . iter () . chain ( at_head . iter () ) . cloned () . collect ();
      for member in &core { add (member, &core_needs); }
      for lister in &in_worktree { add (lister, &core); }}}
  needs }

/// The nodes of 'graph' that list the ID 'gnid' itself in one of
/// their four lists.
fn recorders_listing (
  graph : &InRustGraph,
  gnid  : &ID,
) -> BTreeSet<ID> {
  let member : ID = graph . relationship_member_key (gnid) . into_id ();
  ROLES_OF_LISTED_MEMBERS . iter ()
    . flat_map ( |role|
      graph . inbound_pids_for_relation (&member, role . relation)
        . into_iter ()
        . filter ( move |recorder|
          graph . outbound_gnids_for_relation (recorder, role . relation)
            . contains (gnid) ) )
    . collect () }

/// Of 'pids', the first by title (in the after-graph, else the
/// before-graph), then by pid.
fn first_by_title (
  straddle : &GraphStraddle,
  pids     : impl Iterator<Item = ID>,
) -> Option<ID> {
  pids . min_by ( |a, b|
    ( title_in_straddle (straddle, a), a )
      . cmp (&( title_in_straddle (straddle, b), b )) ) }
