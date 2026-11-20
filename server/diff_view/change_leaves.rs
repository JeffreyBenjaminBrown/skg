//! The change-forest: the tree of everything whose being processed can be
//! asked about, built from the graph straddle, independent of any view
//! ('docs/diff-view.org', "Change-forest leaves"). Its roots
//! are the changed graphnodes that record a change; under each come its
//! changed aspects; its leaves are the individual changes a user
//! marks. A relationship's leaves live under its recorder.
//!
//! Whether a processed aggregate (a set of leaves, a
//! node, the whole changeset) is processed is derived here from a 'ProcessedStore'.
//!
//! Here too are the changed nodes, the unit a commit of the processed
//! takes ('docs/diff-view.org', "Stage and commit"), and the rule that
//! holds back a processed node which needs one that does not go in
//! ('CommitChoice'). The processed summary counts those nodes, so that
//! it predicts what the commit takes.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole, RelationshipKey};
use crate::graph_straddle::types::{GraphStraddle, NodeChanges, Partner};
use crate::graph_straddle::union_walk::ROLES_OF_LISTED_MEMBERS;
use crate::diff_view::processed_store::ProcessedStore;
use crate::diff_view::unmarkable_changes::{
  UnmarkableChange, unmarkable_changes_between};
use crate::serve::util::format_string_list_sexp;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;

use serde::{Deserialize, Serialize};
use sexp::{Atom, Sexp};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// One change a user can mark processed.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum LeafKey {
  Node {
    pid    : ID,
    aspect : NodeAspect, },
  /// A relationship. Seen from either end, it is the same leaf.
  /// Flattened, so that a marks file spells it as one object of
  /// 'relation', 'recorder', 'member' and 'aspect'.
  Relationship {
    #[serde(flatten)]
    relationship : RelationshipKey,
    aspect       : RelationshipAspect, },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum NodeAspect {
  Existence,
  Text,
  #[serde(rename = "Home")] // the spelling marks files already on disk use
  HomeSkgrepo,
  Alias   (String),
  ExtraId (ID),
  Flag    (Flag),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum RelationshipAspect {
  Existence,
  RelRepo,
}

/// A change-forest leaf: its key, the fingerprint of the change it
/// stands for, and the files that record it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ChangeLeaf {
  pub key         : LeafKey,
  pub fingerprint : u64,
  pub files       : Vec<ChangedFile>,
}

/// The '.skg' file of node 'pid' in skgrepo 'skgrepo', as a file that
/// records a change.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ChangedFile {
  pub skgrepo : SkgrepoName,
  pub pid     : ID,
}

/// What the client shows after a save of a diff-view buffer, counted
/// in changed nodes ('ChangeLeaves::changed_nodes'), the unit a commit
/// takes. Order-only nodes count as changed and processed. The
/// changed nodes of the foreign skgrepos are counted apart, and never
/// processed. 'waiting_nodes' are processed nodes that a commit of the
/// processed would hold back ('CommitChoice').
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ProcessedSummary {
  pub processed_nodes       : usize,
  pub changed_nodes         : usize,
  pub foreign_changed_nodes : usize,
  pub order_only_nodes      : usize,
  pub waiting_nodes         : usize,
}

/// The processed summary a save of a diff-view buffer reports: that of the
/// whole changeset, and how many of its leaves are not editable in
/// the buffer (their recorder is not among the nodes
/// 'nodes_editable_in_diffView' returns for it).
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct BufferProcessedSummary {
  pub summary                   : ProcessedSummary,
  pub changes_not_editable_here : usize,
}

impl BufferProcessedSummary {
  /// '(processed-summary (processed N) (changed M) (foreign F)
  /// (order-only O) (waiting W) (notEditable K))'.
  pub fn to_sexp (
    &self,
  ) -> Sexp {
    let entry = |key : &str, count : usize| -> Sexp {
      Sexp::List ( vec! [ Sexp::Atom ( Atom::S ( key . to_string () )),
                          Sexp::Atom ( Atom::I ( count as i64 )) ] ) };
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "processed-summary" . to_string () )),
      entry ( "processed",   self . summary . processed_nodes ),
      entry ( "changed",     self . summary . changed_nodes ),
      entry ( "foreign",     self . summary . foreign_changed_nodes ),
      entry ( "order-only",  self . summary . order_only_nodes ),
      entry ( "waiting",     self . summary . waiting_nodes ),
      entry ( "notEditable", self . changes_not_editable_here ), ] ) }
}

/// The response of a request that can report a processed summary:
///   (LEADING... (processed-summary ...) (errors (...)) (warnings (...)))
/// 'leading' are the request's own entries; the summary is there when
/// one is given.
pub fn processed_response_sexp (
  leading  : Vec<Sexp>,
  summary  : Option<&BufferProcessedSummary>,
  errors   : &[String],
  warnings : &[String],
) -> Sexp {
  Sexp::List (
    leading . into_iter ()
      . chain ( processed_response_trailing_entries (
        summary, errors, warnings ))
      . collect () ) }

/// The entries of 'processed_response_sexp' after the leading ones,
/// for a response whose leading entries are printed by hand.
pub fn processed_response_trailing_entries (
  summary  : Option<&BufferProcessedSummary>,
  errors   : &[String],
  warnings : &[String],
) -> Vec<Sexp> {
  summary . map ( |summary| summary . to_sexp () ) . into_iter ()
    . chain ( [ format_string_list_sexp ("errors", errors),
                format_string_list_sexp ("warnings", warnings) ] )
    . collect () }

/// Which relationships a walk through new nodes follows, from recorder
/// to member, and so which relationship leaves of the new nodes it
/// reaches it names ('ChangeLeaves::leaves_of_new_nodes_reached_from').
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum NewNodeWalk {
  /// Content only ('newContent').
  Contentward,
  /// Every relationship a node records ('newOutward').
  Outward,
}

impl NewNodeWalk {
  pub fn follows (
    self,
    relation : NodeRelation,
  ) -> bool {
    match self {
      NewNodeWalk::Contentward => relation == NodeRelation::Contains,
      NewNodeWalk::Outward     => true, } }
}

/// See the module comment.
#[derive(Clone, Debug, Default)]
pub struct ChangeLeaves {
  /// Each node that records a leaf, with its leaves.
  leaves_by_recorder    : BTreeMap<ID, Vec<ChangeLeaf>>,
  /// Where each leaf is in 'leaves_by_recorder': (recorder, position).
  index                 : HashMap<LeafKey, (ID, usize)>,
  /// Where the relationship leaves naming each member are.
  index_by_member       : HashMap<ID, Vec<(ID, usize)>>,
  order_only_pids       : BTreeSet<ID>,
  /// The new nodes: those the after-graph holds and the before-graph
  /// lacks.
  new_pids              : BTreeSet<ID>,
  foreign_changed_nodes : usize,
  /// The nodes whose set of mentioners changed: the mentioned nodes,
  /// in one graph only, of the recorders. A mention is no leaf: it
  /// changes with its mentioner's text, which is the mentioner's
  /// leaf. C reads it ('GraphStraddle::changed_pids').
  mentioner_sets_changed : BTreeSet<ID>,
  /// The nodes in both graphs whose files changed in a way no leaf
  /// records, with those changes ('unmarkable_changes_between').
  unmarkable_by_node    : BTreeMap<ID, Vec<UnmarkableChange>>,
}

impl ChangeLeaves {
  /// Only the nodes that differ between the two graphs, and those
  /// that record a member the two graphs resolve differently, are
  /// compared ('recorders_of_reresolved_members'): no other node can
  /// record a change.
  pub fn from_graph_straddle (
    straddle : &GraphStraddle,
  ) -> ChangeLeaves {
    let differing : BTreeSet<ID> =
      nodes_differing_between_the_two_graphs (straddle);
    let compared : BTreeSet<ID> =
      differing . iter () . cloned ()
      . chain ( recorders_of_reresolved_members (straddle, &differing) )
      . collect ();
    let mut leaves_by_recorder : BTreeMap<ID, Vec<ChangeLeaf>> =
      BTreeMap::new ();
    for pid in &compared {
      let leaves : Vec<ChangeLeaf> = leaves_recorded_by (straddle, pid);
      if ! leaves . is_empty () {
        leaves_by_recorder . insert (pid . clone (), leaves); }}
    let index : HashMap<LeafKey, (ID, usize)> =
      leaves_by_recorder . iter ()
      . flat_map ( |(recorder, leaves)|
        leaves . iter () . enumerate () . map (
          move |(position, leaf)|
          ( leaf . key . clone (), (recorder . clone (), position) )) )
      . collect ();
    let index_by_member : HashMap<ID, Vec<(ID, usize)>> = {
      let mut index_by_member : HashMap<ID, Vec<(ID, usize)>> =
        HashMap::new ();
      for (recorder, leaves) in &leaves_by_recorder {
        for (position, leaf) in leaves . iter () . enumerate () {
          if let LeafKey::Relationship { relationship, .. } = &leaf . key {
            index_by_member . entry ( relationship . member . clone () )
              . or_default () . push (( recorder . clone (), position )); }} }
      index_by_member };
    let unmarkable_by_node : BTreeMap<ID, Vec<UnmarkableChange>> =
      differing . iter ()
      . filter_map ( |pid| {
        let (before, after) = ( straddle . before . get (pid) ?, straddle . after . get (pid) ? );
        let changes : Vec<UnmarkableChange> = unmarkable_changes_between (
          &straddle . before, &straddle . after, before, after );
        ( ! changes . is_empty () ) . then ( || (pid . clone (), changes) ) } )
      . collect ();
    let order_only_pids : BTreeSet<ID> =
      differing . into_iter ()
      . filter ( |pid|
        ! leaves_by_recorder . contains_key (pid)
        && straddle . node_in_both_graphs (pid) )
      . collect ();
    let new_pids : BTreeSet<ID> =
      leaves_by_recorder . keys ()
      . filter ( |pid|
        straddle . after . nodes . contains_key (*pid)
        && ! straddle . before . nodes . contains_key (*pid) )
      . cloned ()
      . collect ();
    let mentioner_sets_changed : BTreeSet<ID> =
      leaves_by_recorder . keys ()
      . flat_map ( |recorder|
        straddle . partners (recorder, RelationRole::MENTIONED) )
      . filter ( |partner| partner . existence . is_some () )
      . map ( |partner| partner . id )
      . collect ();
    ChangeLeaves { leaves_by_recorder, index, index_by_member, order_only_pids,
                   new_pids,
                   foreign_changed_nodes : straddle . foreign_changed_nodes,
                   mentioner_sets_changed, unmarkable_by_node } }

  /// The nodes that record a leaf (the forest's roots).
  pub fn recorders (
    &self,
  ) -> impl Iterator<Item = &ID> {
    self . leaves_by_recorder . keys () }

  pub fn leaf (
    &self,
    key : &LeafKey,
  ) -> Option<&ChangeLeaf> {
    let (recorder, position) : &(ID, usize) = self . index . get (key) ?;
    self . leaves_by_recorder . get (recorder) ? . get (*position) }

  /// None when 'key' is not a leaf of this forest: that change does
  /// not exist (any more).
  pub fn leaf_fingerprint (
    &self,
    key : &LeafKey,
  ) -> Option<u64> {
    self . leaf (key) . map ( |leaf| leaf . fingerprint ) }

  /// The leaves a node records: its own aspects, and the relationships
  /// it records.
  pub fn leaves_under (
    &self,
    pid : &ID,
  ) -> &[ChangeLeaf] {
    self . leaves_by_recorder . get (pid)
      . map ( |leaves| leaves . as_slice () )
      . unwrap_or (&[]) }

  /// The relationship leaves whose member is 'pid'.
  pub fn leaves_naming_member (
    &self,
    pid : &ID,
  ) -> impl Iterator<Item = &ChangeLeaf> {
    self . index_by_member . get (pid) . into_iter () . flatten ()
      . filter_map ( |(recorder, position)|
        self . leaves_by_recorder . get (recorder) ? . get (*position) ) }

  /// The leaves of the new nodes reached from 'pid' by 'walk': of each,
  /// its existence (which covers its text) and the relationships it
  /// records that 'walk' follows ('NewNodeWalk::follows').
  pub fn leaves_of_new_nodes_reached_from (
    &self,
    pid  : &ID,
    walk : NewNodeWalk,
  ) -> Vec<&ChangeLeaf> {
    self . new_nodes_reached_from (pid, walk) . iter ()
      . flat_map ( |new_pid| self . leaves_under (new_pid) )
      . filter ( |leaf| match &leaf . key {
        LeafKey::Relationship { relationship, .. } =>
          walk . follows (relationship . relation),
        LeafKey::Node { .. } => true, } )
      . collect () }

  /// The new nodes reached from 'pid' through new nodes only, along the
  /// relationships 'walk' follows, in their recorded direction,
  /// breadth-first, each once: 'pid' itself when it is new, and every
  /// new member of such a relationship that a node reached (or 'pid')
  /// records. A new node's relationships are all leaves, so the walk
  /// reads the after-graph's lists from the forest alone.
  pub fn new_nodes_reached_from (
    &self,
    pid  : &ID,
    walk : NewNodeWalk,
  ) -> Vec<ID> {
    let mut reached : HashSet<ID> = HashSet::from ([ pid . clone () ]);
    let mut queue : VecDeque<ID> = VecDeque::from ([ pid . clone () ]);
    let mut new_nodes : Vec<ID> = Vec::new ();
    while let Some (current) = queue . pop_front () {
      if self . new_pids . contains (&current) {
        new_nodes . push ( current . clone () ); }
      for leaf in self . leaves_under (&current) {
        if let LeafKey::Relationship {
          relationship : RelationshipKey { relation, recorder, member },
          aspect : RelationshipAspect::Existence } = &leaf . key {
          if walk . follows (*relation)
            && recorder == &current
            && self . new_pids . contains (member)
            && reached . insert ( member . clone () ) {
            queue . push_back ( member . clone () ); }} }}
    new_nodes }

  /// The members of the relationship leaves whose relationship
  /// exists in one graph only.
  pub fn members_of_relationships_in_one_graph (
    &self,
  ) -> impl Iterator<Item = &ID> {
    self . leaves_by_recorder . values () . flatten ()
      . filter_map ( |leaf| match &leaf . key {
        LeafKey::Relationship {
          relationship, aspect : RelationshipAspect::Existence } =>
          Some (&relationship . member),
        _ => None, } ) }

  pub fn mentioner_sets_changed (
    &self,
  ) -> &BTreeSet<ID> {
    &self . mentioner_sets_changed }

  /// The changed nodes, the unit a commit takes: every node that
  /// records a leaf in an owned skgrepo ('records_in_owned_skgrepo'),
  /// and every order-only node. A container of a merge's acquiree
  /// whose file is unchanged is one, since it records a member
  /// re-resolved ('recorders_of_reresolved_members'). A foreign node
  /// that lists such a member records a leaf too, but in a foreign
  /// skgrepo, which no commit takes, so it is not one.
  pub fn changed_nodes (
    &self,
    config : &SkgConfig,
  ) -> BTreeSet<ID> {
    self . leaves_by_recorder . keys ()
      . filter ( |pid| self . records_in_owned_skgrepo (config, pid) )
      . chain ( self . order_only_pids . iter () )
      . cloned ()
      . collect () }

  /// Whether some leaf 'pid' records has a file in an owned skgrepo.
  pub fn records_in_owned_skgrepo (
    &self,
    config : &SkgConfig,
    pid    : &ID,
  ) -> bool {
    self . leaves_under (pid) . iter ()
      . flat_map ( |leaf| leaf . files . iter () )
      . any ( |file| config . skgrepo_is_owned (&file . skgrepo) ) }

  /// Whether every leaf 'pid' records is marked at its current
  /// fingerprint, and its files changed in no way a leaf does not
  /// record ('unmarkable_changes'). An order-only node records no leaf
  /// but changed in such a way, so it is never processed.
  pub fn node_is_processed (
    &self,
    store : &ProcessedStore,
    pid   : &ID,
  ) -> bool {
    self . leaves_all_marked (store, pid)
      && self . unmarkable_changes (pid) . is_empty () }

  /// Whether every leaf 'pid' records is marked at its current
  /// fingerprint; true when it records none.
  pub fn leaves_all_marked (
    &self,
    store : &ProcessedStore,
    pid   : &ID,
  ) -> bool {
    self . leaves_under (pid) . iter ()
      . all ( |leaf| store . is_processed (&leaf . key, leaf . fingerprint) ) }

  /// How the files of 'pid', a node in both graphs, changed in ways no
  /// leaf records, as the two graphs show it; empty when in none.
  pub fn unmarkable_changes (
    &self,
    pid : &ID,
  ) -> &[UnmarkableChange] {
    self . unmarkable_by_node . get (pid)
      . map ( |changes| changes . as_slice () )
      . unwrap_or (&[]) }

  /// The leaves the nodes 'pids' record, each once.
  pub fn leaves_recorded_by_nodes<'a> (
    &'a self,
    pids : impl IntoIterator<Item = &'a ID>,
  ) -> Vec<&'a ChangeLeaf> {
    pids . into_iter () . collect::<BTreeSet<&ID>> () . into_iter ()
      . flat_map ( |pid| self . leaves_under (pid) )
      . collect () }

  pub fn order_only_pids (
    &self,
  ) -> &BTreeSet<ID> {
    &self . order_only_pids }

  /// Whether every key among 'keys' that is a leaf of this forest is
  /// marked processed with its current fingerprint. Keys that are not
  /// leaves (unchanged things) are never asked.
  pub fn processed_of_leaves<'a> (
    &self,
    store         : &ProcessedStore,
    keys  : impl IntoIterator<Item = &'a LeafKey>,
  ) -> bool {
    keys . into_iter ()
      . filter_map ( |key| self . leaf (key) )
      . all ( |leaf| store . is_processed (&leaf . key, leaf . fingerprint) ) }

  /// How many leaves are recorded by a node not among 'editable_here'.
  pub fn leaves_not_recorded_by (
    &self,
    editable_here : &HashSet<ID>,
  ) -> usize {
    self . leaves_by_recorder . iter ()
      . filter ( |(recorder, _)| ! editable_here . contains (*recorder) )
      . map ( |(_, leaves)| leaves . len () )
      . sum () }
}

/// The processed summary of the straddle's changes, in nodes. Its
/// 'waiting_nodes' come from the same rule a commit applies
/// ('processed_commit_choice').
pub fn processed_summary (
  config   : &SkgConfig,
  straddle : &GraphStraddle,
  store    : &ProcessedStore,
) -> ProcessedSummary {
  let choice : CommitChoice =
    processed_commit_choice (config, straddle, store, &BTreeSet::new ());
  ProcessedSummary {
    processed_nodes       : choice . processed . len (),
    changed_nodes         : choice . changed . len (),
    foreign_changed_nodes : straddle . change_leaves () . foreign_changed_nodes,
    order_only_nodes      : straddle . change_leaves () . order_only_pids . len (),
    waiting_nodes         : choice . waiting . len (), } }

/// The processed summary, with the count of leaves whose recorder
/// is not among 'editable_here' (the nodes a buffer makes editable,
/// 'nodes_editable_in_diffView').
pub fn buffer_processed_summary (
  config        : &SkgConfig,
  straddle      : &GraphStraddle,
  store         : &ProcessedStore,
  editable_here : &HashSet<ID>,
) -> BufferProcessedSummary {
  BufferProcessedSummary {
    summary                   : processed_summary (config, straddle, store),
    changes_not_editable_here :
      straddle . change_leaves () . leaves_not_recorded_by (editable_here), } }

impl ProcessedSummary {
  /// Every changed node of the owned skgrepos is processed.
  pub fn is_complete (
    &self,
  ) -> bool {
    self . processed_nodes == self . changed_nodes }
}

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
  /// Those of 'changed' whose every leaf is marked but whose files
  /// changed in a way no leaf records, with those changes, in pid
  /// order: not processed, and so not going in.
  pub unmarkable : Vec<(ID, Vec<UnmarkableChange>)>,
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
/// is not processed either, as if it changed in a way no leaf records.
pub fn processed_commit_choice (
  config                      : &SkgConfig,
  straddle                    : &GraphStraddle,
  store                       : &ProcessedStore,
  files_not_as_skg_writes_them : &BTreeSet<ID>,
) -> CommitChoice {
  let leaves : &ChangeLeaves = straddle . change_leaves ();
  let changed : BTreeSet<ID> = leaves . changed_nodes (config);
  let unmarkable : Vec<(ID, Vec<UnmarkableChange>)> = changed . iter ()
    . filter ( |pid| leaves . leaves_all_marked (store, pid) )
    . filter_map ( |pid| {
      let changes : Vec<UnmarkableChange> =
        unmarkable_changes_of ( leaves, files_not_as_skg_writes_them, pid );
      ( ! changes . is_empty () ) . then ( || (pid . clone (), changes) ) } )
    . collect ();
  let processed : BTreeSet<ID> = changed . iter ()
    . filter ( |pid| leaves . leaves_all_marked (store, pid)
                     && ! unmarkable . iter () . any ( |(other, _)| other == *pid ) )
    . cloned () . collect ();
  let (going_in, waiting) : (BTreeSet<ID>, Vec<WaitingNode>) =
    hold_back_waiting_nodes (straddle, &changed, processed . clone ());
  CommitChoice { changed, processed, going_in, waiting, unmarkable } }

/// How the files of 'pid' changed in ways no leaf records: as the two
/// graphs show it, else, when its files are not what skg writes for it
/// ('files_not_as_skg_writes_them'), that.
pub fn unmarkable_changes_of (
  leaves                       : &ChangeLeaves,
  files_not_as_skg_writes_them : &BTreeSet<ID>,
  pid                          : &ID,
) -> Vec<UnmarkableChange> {
  let in_graph : &[UnmarkableChange] = leaves . unmarkable_changes (pid);
  if ! in_graph . is_empty () { in_graph . to_vec () }
  else if files_not_as_skg_writes_them . contains (pid) {
    vec! [ UnmarkableChange::FileNotAsSkgWritesIt ] }
  else { Vec::new () }}

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

/// The IDs among the extra IDs 'pid' has in one graph only that the
/// two graphs resolve to different nodes.
fn reresolved_extra_ids_of (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<ID> {
  let extra_ids_in = |graph : &InRustGraph| -> BTreeSet<ID> {
    graph . get (pid)
      . map ( |node| node . extra_ids . iter () . cloned () . collect () )
      . unwrap_or_default () };
  let member_key_in = |graph : &InRustGraph, gnid : &ID| -> ID {
    graph . relationship_member_key (gnid) . into_id () };
  extra_ids_in (&straddle . before)
    . symmetric_difference ( &extra_ids_in (&straddle . after) )
    . filter ( |gnid| member_key_in (&straddle . before, gnid)
                      != member_key_in (&straddle . after, gnid) )
    . cloned ()
    . collect () }

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

/// The node's title, in the after-graph, else the before-graph; its
/// pid when neither holds it.
pub fn title_in_straddle (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> String {
  straddle . after . get (pid)
    . or ( straddle . before . get (pid) )
    . map ( |node| node . title . clone () )
    . unwrap_or_else ( || pid . 0 . clone () ) }

/// The nodes of either graph that the other lacks or holds unequal.
fn nodes_differing_between_the_two_graphs (
  straddle : &GraphStraddle,
) -> BTreeSet<ID> {
  let in_before : Vec<&ID> = // unequal or only before
    straddle . before . nodes . iter ()
    . filter ( |(pid, before)|
      straddle . after . nodes . get (*pid) != Some (*before) )
    . map ( |(pid, _)| pid )
    . collect ();
  let only_after : Vec<&ID> =
    straddle . after . nodes . keys ()
    . filter ( |pid| ! straddle . before . nodes . contains_key (*pid) )
    . collect ();
  in_before . into_iter () . chain (only_after) . cloned () . collect () }

/// The nodes that list a member whose ID the two graphs resolve to
/// different pids. After a nodeMerge since HEAD, a container of the
/// acquiree that still lists the acquiree's ID is equal in both graphs,
/// but the after-graph resolves that ID to the acquirer, so the
/// container records a changed relationship. An ID resolves
/// differently only when it is an extra ID of some node in one graph
/// and not of the same node in the other, so such IDs are found among
/// the extra-ID differences of the nodes in 'differing' (those that
/// differ between the graphs), and their recorders through each
/// graph's recorderward relmaps.
fn recorders_of_reresolved_members (
  straddle  : &GraphStraddle,
  differing : &BTreeSet<ID>,
) -> BTreeSet<ID> {
  let extra_ids_in = |graph : &InRustGraph, pid : &ID| -> BTreeSet<ID> {
    graph . get (pid)
      . map ( |node| node . extra_ids . iter () . cloned () . collect () )
      . unwrap_or_default () };
  let member_key_in = |graph : &InRustGraph, gnid : &ID| -> ID {
    graph . relationship_member_key (gnid) . into_id () };
  let reresolved : BTreeSet<ID> =
    differing . iter ()
    . flat_map ( |pid|
      extra_ids_in (&straddle . before, pid)
      . symmetric_difference ( &extra_ids_in (&straddle . after, pid) )
      . cloned () . collect::<Vec<ID>> () )
    . filter ( |gnid| member_key_in (&straddle . before, gnid)
                       != member_key_in (&straddle . after, gnid) )
    . collect ();
  let mut recorders : BTreeSet<ID> = BTreeSet::new ();
  for gnid in &reresolved {
    for graph in [ &straddle . before, &straddle . after ] {
      let member : ID = member_key_in (graph, gnid);
      for role in ROLES_OF_LISTED_MEMBERS {
        recorders . extend (
          graph . inbound_pids_for_relation (&member, role . relation) ); }} }
  recorders }

/// The leaves recorded by 'pid': its existence when it is new or
/// deleted, else its changed aspects; then the relationships it
/// records whose existence or relRepo changed.
fn leaves_recorded_by (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<ChangeLeaf> {
  let before : Option<&GraphnodeInRust> = straddle . before . get (pid);
  let after  : Option<&GraphnodeInRust> = straddle . after  . get (pid);
  let file_in = |skgrepo : &SkgrepoName| -> ChangedFile {
    ChangedFile { skgrepo : skgrepo . clone (), pid : pid . clone () } };
  let home_file = |node : &GraphnodeInRust| -> Vec<ChangedFile> {
    vec! [ file_in (&node . home_skgrepo) ] };
  let node_leaf = |aspect : NodeAspect, fingerprint : u64,
                   files : Vec<ChangedFile>| -> ChangeLeaf {
    ChangeLeaf { key : LeafKey::Node { pid : pid . clone (), aspect },
                 fingerprint, files } };
  let mut leaves : Vec<ChangeLeaf> = Vec::new ();
  match (before, after) {
    (Some (before), Some (after)) => {
      let changes : NodeChanges = straddle . node_changes (pid);
      if changes . text_changed () {
        leaves . push ( node_leaf (
          NodeAspect::Text,
          fingerprint_of (&( &before . title, &before . body,
                             &after  . title, &after  . body )),
          home_file (after) )); }
      if let Some (home) = &changes . home {
        leaves . push ( node_leaf (
          NodeAspect::HomeSkgrepo,
          fingerprint_of (&( &home . old, &home . new )),
          vec! [ file_in (&home . old), file_in (&home . new) ] )); }
      for (gained, alias) in
        changes . aliases_gained . iter () . map ( |a| (true, a) )
        . chain ( changes . aliases_lost . iter () . map ( |a| (false, a) ))
      { leaves . push ( node_leaf (
          NodeAspect::Alias (alias . clone ()),
          fingerprint_of (&( ! gained, gained,
                             alias_relRepos ( if gained { after } else { before }, alias ) )),
          home_file (after) )); }
      for (gained, extra_id) in
        changes . extra_ids_gained . iter () . map ( |i| (true, i) )
        . chain ( changes . extra_ids_lost . iter () . map ( |i| (false, i) ))
      { leaves . push ( node_leaf (
          NodeAspect::ExtraId (extra_id . clone ()),
          fingerprint_of (&( ! gained, gained )),
          home_file (after) )); }
      for (gained, flag) in
        changes . flags_gained . iter () . map ( |f| (true, f) )
        . chain ( changes . flags_lost . iter () . map ( |f| (false, f) ))
      { leaves . push ( node_leaf (
          NodeAspect::Flag (*flag),
          fingerprint_of (&( ! gained, gained )),
          home_file (after) )); }},
    (before, after) => {
      // A new or deleted node: its existence is one leaf, whose
      // fingerprint covers everything its files hold, the order and
      // skgrepos of its lists among it.
      let existing : &GraphnodeInRust =
        after . or (before)
        . expect ("a changed node is in one graph or the other");
      leaves . push ( node_leaf (
        NodeAspect::Existence,
        fingerprint_of (&( before, after )),
        home_file (existing) )); }}
  for role in ROLES_OF_LISTED_MEMBERS {
    for partner in straddle . partners (pid, role) {
      leaves . extend ( relationship_leaves (pid, role, &partner) ); }}
  leaves }

/// The leaves of the relationship by which 'recorder' lists
/// 'partner' (in the partner's 'role').
fn relationship_leaves (
  recorder : &ID,
  role     : RelationRole,
  partner  : &Partner,
) -> Vec<ChangeLeaf> {
  let key = |aspect : RelationshipAspect| -> LeafKey {
    LeafKey::Relationship {
      relationship : RelationshipKey {
        relation : role . relation,
        recorder : recorder . clone (),
        member   : partner . id . clone () },
      aspect } };
  let file_in = |skgrepo : &SkgrepoName| -> ChangedFile {
    ChangedFile { skgrepo : skgrepo . clone (), pid : recorder . clone () } };
  if partner . existence . is_some () {
    let relRepo : &SkgrepoName =
      partner . relRepo_after . as_ref ()
      . or (partner . relRepo_before . as_ref ())
      . expect ("a partner is in one graph or the other");
    vec! [ ChangeLeaf {
      key         : key (RelationshipAspect::Existence),
      fingerprint : fingerprint_of (&( &partner . relRepo_before,
                                       &partner . relRepo_after )),
      files       : vec! [ file_in (relRepo) ], } ]
  } else if partner . relRepo_moved () {
    vec! [ ChangeLeaf {
      key         : key (RelationshipAspect::RelRepo),
      fingerprint : fingerprint_of (&( &partner . relRepo_before,
                                       &partner . relRepo_after )),
      files       : [ &partner . relRepo_before, &partner . relRepo_after ]
                    . into_iter () . flatten () . map (file_in) . collect (), } ]
  } else { Vec::new () }}

/// The skgrepos 'node' records the alias 'text' in.
fn alias_relRepos<'a> (
  node : &'a GraphnodeInRust,
  text : &str,
) -> Vec<&'a SkgrepoName> {
  node . aliases . or_default () . iter ()
    . filter ( |alias| alias . member == text )
    . map ( |alias| &alias . relRepo )
    . collect () }

/// The fingerprint of a leaf's before and after values.
fn fingerprint_of<T : Hash> (
  values : &T,
) -> u64 {
  let mut hasher : DefaultHasher = DefaultHasher::new ();
  values . hash (&mut hasher);
  hasher . finish () }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/change_leaves.rs"]
mod tests;
