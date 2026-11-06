//! The save's refusal of a stale buffer, decided by content.
//!
//! A save's write of a graphnode has one of two sources: the buffer's
//! copy of it (the Save instruction the saved text yields), or the
//! graph as it stands at save time. Only the buffer's copy can be
//! older than the graph, and only a write from it, or a delete, can
//! discard graph state the user never saw. What the buffer showed
//! before any edit is its baseline: the text the server last drew for
//! it, which the client keeps and sends with the save. The baseline is
//! parsed and extracted as the saved text is, up to the noop filter;
//! the graphnodes its Save instructions still write are those whose
//! baseline's copy no longer matches the graph (or that the graph no
//! longer has). A graphnode the buffer writes or deletes among them
//! is stale, and the whole save is refused, before anything is asked
//! or written, with one error per stale graphnode saying what changed.
//!
//! A graphnode among the baseline's writes is outdated. The buffer
//! may still show it exactly as the baseline does: the user did not
//! edit it, and the buffer's copy of it differs from the graph only
//! because the graph changed. Such an untouched outdated graphnode is
//! left out of the save before any validator judges its write
//! ('without_untouched_outdated_saves', called by the plan builder),
//! with its home skgrepo move, and so is never forked, moved back or
//! stale; the save warns of it, and the redraw shows it as the graph
//! has it. The baseline is read once, before the plan is built
//! ('what_the_baseline_shows'), since the plan builder needs its
//! writes for this and its occurrences' titles and home skgrepos for
//! the write-protected check ('errors_for_writeProtected_occurrences').
//!
//! What the buffer writes is the nodeInstructions its text yields once
//! the noop filter has dropped the ones identical to the graph and the
//! untouched outdated ones have been left out, plus the originals of
//! its fork clones. So a graphnode the buffer shows unchanged, or as
//! the baseline has it, is never stale. A nodeMerge is built from the graph at
//! save time, and the writes a save derives from the current graph
//! (delete propagation, strandeds resets, Hoist repairs, restore
//! requests) undo nothing, so none of those is checked. A strip of
//! whitespace changes nothing a parsed baseline shows, and a restart
//! or a rebuild makes nothing stale by itself: only a graphnode whose
//! baseline's copy no longer matches the graph is stale. A graphnode
//! the buffer writes without a baseline copy of it (pasted in by ID,
//! or typed into a new empty view, whose baseline is the text the
//! client created it with) has nothing to compare, and is not checked.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::from_text::buffer_to_viewnodes::add_missing_info::{
  IdResolution, WhatTheParseSupplied};
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use crate::from_text::local_fieldintent_collection::{
  NonmergeSavePlan, extract_nonmergeSavePlan_locally_in_graph};
use crate::from_text::validate::{buffernode_differs_from_disknode, flatten_ms};
use crate::graph_straddle::types::NodeChanges;
use crate::graph_straddle::union_walk::node_changes_between;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::{
  BufferValidationError, StaleGraphnodeReport, StaleAspect, HeadlineName};
use crate::types::list::{Diff_Item, interleaved_diff_without_reorders};
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::save::{
  DeleteNode, ForkSpec, NodeInstruction, SaveNode, SkgrepoMove,
  UntouchedOutdatedGraphnode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewnodeKind, Vognode};
use crate::serve::handlers::strip_body_whitespace::strip_trailing_whitespace_from_body;

use std::collections::{BTreeMap, BTreeSet};

/// One error per graphnode the buffer would write or delete that is
/// among the baseline's writes ('baseline_writes'), naming the
/// buffer's occurrences of it and what changed.
pub fn stale_write_errors (
  graph            : &InRustGraph,
  baseline_writes  : &BTreeMap<ID, Graphnode>,
  viewforest       : &ViewForest,
  nodeInstructions : &[NodeInstruction],
  fork_specs       : &[ForkSpec],
) -> Vec<BufferValidationError> {
  if baseline_writes . is_empty () { return Vec::new (); }
  buffer_written_pids (graph, nodeInstructions, fork_specs)
    . into_iter ()
    . filter_map ( |pid| {
      let then : &Graphnode = baseline_writes . get (&pid) ?;
      Some ( BufferValidationError::ChangedSinceRendered {
        change    : change_since_rendered (graph, &pid, then),
        headlines : occurrence_headlines_of (graph, viewforest, &pid),
        gnid      : pid, } ) } )
    . collect () }

/// What a save reads from the saved buffer's baseline, which it parses
/// once ('what_the_baseline_shows').
#[derive(Debug, Default)]
pub struct WhatTheBaselineShows {
  /// The baseline's writes, by pid: the graphnodes saving the baseline
  /// itself would write, each as the baseline has it once supplemented,
  /// and as a view reads it back ('as_a_view_reads_it_back').
  pub writes : BTreeMap<ID, Graphnode>,
  /// The title and home skgrepo of each occurrence in the baseline,
  /// editable or write-protected, by its graphnode's pid. A
  /// write-protected occurrence writes nothing, so a graphnode the
  /// baseline shows only write-protected is among these alone.
  pub occurrences : BTreeMap<ID, Vec<OccurrenceInTheBaseline>>,
}

/// An occurrence's title and home skgrepo as the baseline shows them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OccurrenceInTheBaseline {
  pub title        : String,
  pub home_skgrepo : SkgrepoName,
}

/// The baseline, parsed and extracted as the saved text is
/// ('buffer_to_validated_saveplan_with_fork_skgrepos_in_graph'), minus
/// the validators, which judge edits, and a baseline has none. A
/// baseline that cannot be parsed or extracted yields one error saying
/// so, which refuses the save.
pub fn what_the_baseline_shows (
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  baseline            : &str,
) -> Result<WhatTheBaselineShows, BufferValidationError> {
  what_the_baseline_shows_else_why_not (graph, config, skgrepo_restriction, baseline)
    . map_err ( |message| BufferValidationError::Other ( format! (
      "The saved buffer's baseline, the text the server last drew for it, could not be read: {}. Nothing was saved. Archive your edits (M-x skg-show-unsaved-changes in Emacs, :SkgShowUnsavedChanges in Neovim), reopen a fresh view, and reapply them there.",
      message )) ) }

fn what_the_baseline_shows_else_why_not (
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  baseline            : &str,
) -> Result<WhatTheBaselineShows, String> {
  let (viewforest, parsing_errors, _warnings, _supplied)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, WhatTheParseSupplied) =
    org_to_viewforest (baseline, IdResolution::ThroughGraph (graph)) ?;
  if ! parsing_errors . is_empty () {
    return Err ( parsing_errors . iter () . map (ToString::to_string)
                 . collect::<Vec<String>> () . join ("; ") ); }
  let (nonmerge_plan, _nodeMerge_pairs) : (NonmergeSavePlan, Vec<(ID, ID)>) =
    extract_nonmergeSavePlan_locally_in_graph (
      &viewforest, graph, config, skgrepo_restriction )
    . map_err ( |error| error . to_string () ) ?;
  let writes : BTreeMap<ID, Graphnode> =
    nonmerge_plan . node_instructions . into_iter ()
    . filter_map ( |nodeInstruction| match nodeInstruction {
      NodeInstruction::Save (SaveNode (node)) =>
        Some (( graph . pid_else_raw_id (&node . pid),
                as_a_view_reads_it_back (graph, node) )),
      NodeInstruction::Delete (_) => None, } )
    . filter ( |(pid, node)| match graphnode_as_read_back_from_a_view (graph, pid) {
      // The noop filter compares with the graph as stored. What a
      // view of a graphnode reads back can differ from that without
      // anything having changed: the parser trims the trailing
      // whitespace a body is drawn with, and a view draws a
      // relationship member once per pid. So the baseline's copy is
      // compared with the graph as a view reads it back.
      Some (as_read_back) => buffernode_differs_from_disknode (node, &as_read_back),
      None => true, } )
    . collect ();
  Ok ( WhatTheBaselineShows {
    writes,
    occurrences : occurrences_in_the_baseline (graph, &viewforest), } ) }

/// The title and home skgrepo of every occurrence in 'viewforest', by
/// its graphnode's pid.
fn occurrences_in_the_baseline (
  graph      : &InRustGraph,
  viewforest : &ViewForest,
) -> BTreeMap<ID, Vec<OccurrenceInTheBaseline>> {
  let mut occurrences : BTreeMap<ID, Vec<OccurrenceInTheBaseline>> =
    BTreeMap::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &node . value () . kind else { continue; };
    occurrences . entry ( graph . pid_else_raw_id (&occurrence . gnid) )
      . or_default ()
      . push ( OccurrenceInTheBaseline {
        title        : occurrence . title . clone (),
        home_skgrepo : occurrence . home_skgrepo . clone (), } ); }
  occurrences }

/// The buffer's Saves of untouched outdated graphnodes removed, with
/// their home skgrepo moves, and each such graphnode named. A
/// graphnode is outdated when it is among 'baseline_writes', and
/// untouched when the buffer's copy of it (its Save here, supplemented
/// and noop-filtered) equals the baseline's copy, compared as a view
/// reads a graphnode back. Writing it would only undo what changed in
/// the graph; left out, it stays as the graph has it. A Delete is an
/// edit, and is kept.
pub fn without_untouched_outdated_saves (
  graph            : &InRustGraph,
  baseline_writes  : &BTreeMap<ID, Graphnode>,
  nodeInstructions : Vec<NodeInstruction>,
  skgrepo_moves    : Vec<SkgrepoMove>,
) -> ( Vec<NodeInstruction>, Vec<SkgrepoMove>, Vec<UntouchedOutdatedGraphnode> ) {
  if baseline_writes . is_empty () {
    return (nodeInstructions, skgrepo_moves, Vec::new ()); }
  let mut kept      : Vec<NodeInstruction> = Vec::new ();
  let mut untouched : Vec<UntouchedOutdatedGraphnode> = Vec::new ();
  for nodeInstruction in nodeInstructions {
    let NodeInstruction::Save (SaveNode (node)) = &nodeInstruction
      else { kept . push (nodeInstruction); continue; };
    let pid : ID = graph . pid_else_raw_id (&node . pid);
    let is_untouched : bool =
      baseline_writes . get (&pid) . is_some_and ( |baseline_copy|
        ! buffernode_differs_from_disknode (
          &as_a_view_reads_it_back (graph, node . clone ()), baseline_copy ));
    if is_untouched {
      untouched . push ( UntouchedOutdatedGraphnode {
        deleted_from_the_graph : graph_lacks (graph, &pid),
        title                  : node . title . clone (),
        pid, } );
    } else { kept . push (nodeInstruction); }}
  let skgrepo_moves : Vec<SkgrepoMove> = skgrepo_moves . into_iter ()
    . filter ( |skgrepo_move| ! untouched . iter () . any ( |node|
      node . pid == graph . pid_else_raw_id (&skgrepo_move . pid) ))
    . collect ();
  (kept, skgrepo_moves, untouched) }

/// The save's warning that it left 'node' as the graph has it.
pub fn left_as_the_graph_has_it_warning (
  node : &UntouchedOutdatedGraphnode,
) -> String {
  format! ( "Left as the graph has it: {} ({}), which {} after this buffer was rendered and which this buffer did not edit.",
            node . title, node . pid . 0,
            if node . deleted_from_the_graph { "was deleted" } else { "changed" } ) }

/// Whether the graph no longer has the outdated graphnode 'pid': what
/// tells a deleted one from a changed one, in the warning as in the
/// stale error ('change_since_rendered', which reads the graphnode
/// back where the graph has it).
fn graph_lacks (
  graph : &InRustGraph,
  pid   : &ID,
) -> bool {
  graphnode_from_graph (graph, pid) . is_none () }

/// The graph's graphnode 'pid' as a view drawn from it would read it
/// back ('as_a_view_reads_it_back').
fn graphnode_as_read_back_from_a_view (
  graph : &InRustGraph,
  pid   : &ID,
) -> Option<Graphnode> {
  graphnode_from_graph (graph, pid)
    . map ( |node| as_a_view_reads_it_back (graph, node) ) }

/// 'node' as a view drawn from it would read it back: its body without
/// the trailing whitespace the parser trims (as
/// 'strip_trailing_whitespace_from_body' strips it on disk), and its
/// relationship lists by distinct pid ('with_relationships_by_distinct_pid').
/// Every comparison of copies here, the baseline's with the graph's
/// and the buffer's with the baseline's, compares two graphnodes so
/// read back.
fn as_a_view_reads_it_back (
  graph : &InRustGraph,
  node  : Graphnode,
) -> Graphnode {
  with_relationships_by_distinct_pid ( graph, Graphnode {
    body : node . body . as_deref ()
           . map (strip_trailing_whitespace_from_body)
           . filter ( |body| ! body . is_empty () ),
    .. node } ) }

/// 'node' with each member of its relationship lists ('contains',
/// 'subscribesTo', 'hidesFromSubs', 'overrides') replaced by its pid,
/// and every later member with the same pid dropped, keeping order. A
/// nodeMerge of two members of one list leaves both IDs in the file,
/// naming one graphnode, which a view draws once.
/// (Aliases are text, not IDs. The extra IDs are not compared, and
/// each would resolve to 'node' itself.)
fn with_relationships_by_distinct_pid (
  graph : &InRustGraph,
  node  : Graphnode,
) -> Graphnode {
  let by_distinct_pid = |members : Vec<RelPartner<ID>>| -> Vec<RelPartner<ID>> {
    let mut seen : BTreeSet<ID> = BTreeSet::new ();
    members . into_iter ()
      . map ( |member| RelPartner {
        member : graph . pid_else_raw_id (&member . member),
        .. member } )
      . filter ( |member| seen . insert ( member . member . clone () ))
      . collect () };
  let msv_by_distinct_pid = |members : MSV<RelPartner<ID>>| -> MSV<RelPartner<ID>> {
    match members {
      MSV::Specified (members) => MSV::Specified ( by_distinct_pid (members) ),
      MSV::Unspecified => MSV::Unspecified, }};
  Graphnode {
    contains      : by_distinct_pid     (node . contains),
    subscribesTo  : msv_by_distinct_pid (node . subscribesTo),
    hidesFromSubs : msv_by_distinct_pid (node . hidesFromSubs),
    overrides     : msv_by_distinct_pid (node . overrides),
    .. node } }

/// The pids of the graphnodes the buffer writes or deletes: its
/// nodeInstructions, and the originals of its fork clones.
/// (A BTreeSet, so the errors come in a stable order.)
fn buffer_written_pids (
  graph            : &InRustGraph,
  nodeInstructions : &[NodeInstruction],
  fork_specs       : &[ForkSpec],
) -> BTreeSet<ID> {
  nodeInstructions . iter ()
    . map ( |nodeInstruction| match nodeInstruction {
      NodeInstruction::Save (SaveNode (node)) => &node . pid,
      NodeInstruction::Delete (DeleteNode { gnid, .. }) => gnid, } )
    . chain ( fork_specs . iter () . map ( |fork_spec| &fork_spec . original_gnid ))
    . map ( |gnid| graph . pid_else_raw_id (gnid) )
    . collect () }

/// How the graph's graphnode 'pid' differs from 'then', the
/// baseline's copy of it, compared as the diff view compares two
/// graphnodes ('node_changes_between').
fn change_since_rendered (
  graph : &InRustGraph,
  pid   : &ID,
  then  : &Graphnode,
) -> StaleGraphnodeReport {
  let Some (now) : Option<Graphnode> = graphnode_as_read_back_from_a_view (graph, pid)
    else { return StaleGraphnodeReport::Deleted; }; // 'graph_lacks' it
  let (then, now) : (GraphnodeInRust, GraphnodeInRust) =
    ( GraphnodeInRust::from (then), GraphnodeInRust::from (&now) );
  let changes : NodeChanges = node_changes_between (&then, &now);
  let mut aspects : Vec<StaleAspect> = Vec::new ();
  if changes . title_changed {
    aspects . push ( StaleAspect {
      name : "title",
      detail : Some ( format! ( "then \"{}\", now \"{}\"",
                                then . title, now . title )) } ); }
  if changes . body_changed {
    aspects . push ( StaleAspect { name : "body", detail : None } ); }
  if then . contains != now . contains {
    aspects . push ( StaleAspect {
      name : "content",
      detail : Some ( content_change (&then . contains, &now . contains) ) } ); }
  for (name, changed) in [
    ( "aliases",
      ! changes . aliases_gained . is_empty ()
      || ! changes . aliases_lost . is_empty ()
      || relationships_changed (&then . aliases, &now . aliases) ),
    ( "extra IDs",
      ! changes . extra_ids_gained . is_empty ()
      || ! changes . extra_ids_lost . is_empty () ),
    ( "flags", // the noop filter compares them in order
      then . flags != now . flags ),
    ( "subscriptions",
      relationships_changed (&then . subscribesTo, &now . subscribesTo) ),
    ( "hides",
      relationships_changed (&then . hidesFromSubs, &now . hidesFromSubs) ),
    ( "overrides",
      relationships_changed (&then . overrides, &now . overrides) ), ] {
    if changed { aspects . push ( StaleAspect { name, detail : None } ); }}
  if let Some (home) = &changes . home {
    aspects . push ( StaleAspect {
      name : "home skgrepo",
      detail : Some ( format! ( "then {}, now {}", home . old, home . new )) } ); }
  StaleGraphnodeReport::Changed {
    title      : now . title . clone (),
    aspects,
    body_lines : if changes . body_changed {
                   body_line_diff ( then . body . as_deref (), now . body . as_deref () )
                 } else { Vec::new () }, } }

/// Whether a relationship list the baseline specifies differs from the
/// graph's, as the noop filter judges it: an unspecified list matches
/// anything, and an empty one matches an unspecified one.
fn relationships_changed<T : Clone + PartialEq> (
  then : &MSV<T>,
  now  : &MSV<T>,
) -> bool {
  ! then . is_unspecified () && flatten_ms (then) != flatten_ms (now) }

/// "gained X, Y, lost Z", by ID, from the diff view's comparison of
/// two lists; a change in only the order or the relationships'
/// skgrepos says so.
fn content_change (
  then : &[RelPartner<ID>],
  now  : &[RelPartner<ID>],
) -> String {
  let ids = |partners : &[RelPartner<ID>]| -> Vec<ID> {
    partners . iter () . map ( |partner| partner . member . clone () ) . collect () };
  let (then_ids, now_ids) : (Vec<ID>, Vec<ID>) = (ids (then), ids (now));
  let (gained, lost) : (Vec<String>, Vec<String>) = {
    let mut gained : Vec<String> = Vec::new ();
    let mut lost   : Vec<String> = Vec::new ();
    for item in interleaved_diff_without_reorders (&then_ids, &now_ids) {
      match item {
        Diff_Item::New (id)     => gained . push (id . 0),
        Diff_Item::Removed (id) => lost   . push (id . 0),
        Diff_Item::Unchanged (_) => {}, }}
    (gained, lost) };
  let none_else = |ids : Vec<String>| -> String {
    if ids . is_empty () { "none" . to_string () } else { ids . join (", ") } };
  if gained . is_empty () && lost . is_empty () {
    if then_ids == now_ids { "the skgrepos of its relationships" . to_string () }
    else { "reordered" . to_string () }
  } else { format! ( "gained {}, lost {}", none_else (gained), none_else (lost) ) }}

/// The body's changed lines, then against now, from the diff view's
/// comparison of two lists: each lost line prefixed "- ", each gained
/// one "+ ", unchanged ones omitted, at most
/// 'MAX_BODY_DIFF_LINES_EACH_WAY' lost and as many gained, then a line
/// counting the rest.
fn body_line_diff (
  then : Option<&str>,
  now  : Option<&str>,
) -> Vec<String> {
  let lines = |body : Option<&str>| -> Vec<String> {
    body . unwrap_or ("") . lines () . map (str::to_string) . collect () };
  let mut shown : Vec<String> = Vec::new ();
  let (mut lost_shown, mut gained_shown, mut omitted)
    : (usize, usize, usize) = (0, 0, 0);
  for item in interleaved_diff_without_reorders ( &lines (then), &lines (now) ) {
    match item {
      Diff_Item::Removed (line) if lost_shown < MAX_BODY_DIFF_LINES_EACH_WAY => {
        lost_shown += 1;
        shown . push ( format! ("- {}", line) ); }
      Diff_Item::New (line) if gained_shown < MAX_BODY_DIFF_LINES_EACH_WAY => {
        gained_shown += 1;
        shown . push ( format! ("+ {}", line) ); }
      Diff_Item::Removed (_) | Diff_Item::New (_) => omitted += 1,
      Diff_Item::Unchanged (_) => {}, }}
  if omitted > 0 {
    shown . push ( format! ("... and {} more", omitted) ); }
  shown }

const MAX_BODY_DIFF_LINES_EACH_WAY : usize = 40;

/// The headline names of the occurrences of 'pid' in 'viewforest'.
fn occurrence_headlines_of (
  graph      : &InRustGraph,
  viewforest : &ViewForest,
  pid        : &ID,
) -> Vec<HeadlineName> {
  viewforest . nodes ()
    . filter ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        graph . pid_else_raw_id (&occurrence . gnid) == *pid,
      _ => false, } )
    . map ( |node| node . value () . headline_name () )
    . collect () }
