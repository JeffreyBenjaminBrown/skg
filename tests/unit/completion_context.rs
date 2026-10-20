// The three constructors of 'CompletionContext' give the settings
// that the literal constructions in 'render_initial_view',
// 'complete_viewforest_with_graph_straddle' and 'rerender_view'
// wrote, field by field.

use super::*;
use crate::dbs::init::empty_in_ram_tantivy_index;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::misc::{SkgConfig, SkgrepoSetName};
use crate::types::views_state::DiffViewBufferKind;

use std::collections::BTreeSet;

fn runtime () -> RuntimeGeneration {
  RuntimeGeneration {
    config        : Arc::new ( SkgConfig::dummyFromSkgrepos (HashMap::new ()) ),
    graph         : Arc::new ( InRustGraph::from_graphnodes (&[]) ),
    tantivy_index : empty_in_ram_tantivy_index () . unwrap (),
    generation    : 0, } }

fn straddle () -> GraphStraddle {
  GraphStraddle::new (
    Arc::new ( InRustGraph::from_graphnodes (&[]) ),
    Arc::new ( InRustGraph::from_graphnodes (&[]) ),
    String::new (), 0 ) }

fn restriction () -> SkgrepoRestriction {
  SkgrepoRestriction {
    name     : SkgrepoSetName ("main" . to_string ()),
    skgrepos : BTreeSet::from ([ SkgrepoName::from ("main") ]), } }

/// The settings of a context that do not borrow mutable state.
#[derive(Debug, PartialEq)]
struct Settings {
  deleted_by_this_save_pids      : HashSet<ID>,
  deleted_by_this_save_extra_ids : HashMap<ID, HashSet<ID>>,
  skgrepo_restriction            : Option<SkgrepoRestriction>,
  node_budget                    : usize,
  create_partnerFolders_for_fresh_nodes : bool,
  substitute_existing_content_overrides : bool,
  has_warning_sink               : bool,
  diffView_buffer_kind           : Option<DiffViewBufferKind>,
  nodes_before_completion        : HashSet<NodeId>,
  editable_before_completion     : HashSet<NodeId>,
}

fn settings_of (
  context : &CompletionContext,
) -> Settings {
  Settings {
    deleted_by_this_save_pids      : context . deleted_by_this_save_pids . clone (),
    deleted_by_this_save_extra_ids : context . deleted_by_this_save_extra_ids . clone (),
    skgrepo_restriction            : context . skgrepo_restriction . cloned (),
    node_budget                    : context . node_budget,
    create_partnerFolders_for_fresh_nodes :
      context . create_partnerFolders_for_fresh_nodes,
    substitute_existing_content_overrides :
      context . substitute_existing_content_overrides,
    has_warning_sink               : context . warning_sink . is_some (),
    diffView_buffer_kind           : context . diffView_source
      . map ( |drawing| drawing . diffView_buffer_kind ),
    nodes_before_completion        : context . nodes_before_completion . clone (),
    editable_before_completion     : context . editable_before_completion . clone (), } }

#[test]
fn for_content_view_gives_render_initial_views_settings () {
  let runtime : RuntimeGeneration = runtime ();
  let graph_snap : Arc<InRustGraph> = runtime . graph . clone ();
  let restriction : SkgrepoRestriction = restriction ();
  let mut editable_map : EditableMap = EditableMap::new ();
  let mut errors : Vec<String> = Vec::new ();
  let mut sink : Vec<CompletionWarning> = Vec::new ();
  let context : CompletionContext = CompletionContext::for_content_view (
    &runtime, &graph_snap, &mut editable_map, &mut errors,
    Some (&restriction), &mut sink );
  assert_eq! ( settings_of (&context), Settings {
    deleted_by_this_save_pids      : HashSet::new (),
    deleted_by_this_save_extra_ids : HashMap::new (),
    skgrepo_restriction            : Some (restriction . clone ()),
    node_budget                    : runtime . config . initial_node_limit,
    create_partnerFolders_for_fresh_nodes : true,
    substitute_existing_content_overrides : false,
    has_warning_sink               : true,
    diffView_buffer_kind           : None,
    nodes_before_completion        : HashSet::new (),
    editable_before_completion     : HashSet::new (), } ); }

#[test]
fn for_diffView_gives_the_first_drawings_settings () {
  let runtime : RuntimeGeneration = runtime ();
  let graph_snap : Arc<InRustGraph> = runtime . graph . clone ();
  let straddle : GraphStraddle = straddle ();
  let mut editable_map : EditableMap = EditableMap::new ();
  let mut errors : Vec<String> = Vec::new ();
  let context : CompletionContext = CompletionContext::for_diffView (
    &runtime, &graph_snap, &mut editable_map, &mut errors,
    DiffViewSource { straddle : &straddle,
                     diffView_buffer_kind : DiffViewBufferKind::Merges } );
  assert_eq! ( settings_of (&context), Settings {
    deleted_by_this_save_pids      : HashSet::new (),
    deleted_by_this_save_extra_ids : HashMap::new (),
    skgrepo_restriction            : None,
    node_budget                    : runtime . config . initial_node_limit,
    create_partnerFolders_for_fresh_nodes : false,
    substitute_existing_content_overrides : false,
    has_warning_sink               : false,
    diffView_buffer_kind           : Some (DiffViewBufferKind::Merges),
    nodes_before_completion        : HashSet::new (),
    editable_before_completion     : HashSet::new (), } ); }

/// Outside the diff view a rerender takes the restriction and the two
/// flags it is given; in a buffer of the diff view it takes the first
/// drawing's instead. Either way it keeps what the save deleted, the
/// sink, and what the view held before.
#[test]
fn for_rerender_gives_rerender_views_settings_in_and_out_of_the_diff_view () {
  let runtime : RuntimeGeneration = runtime ();
  let graph_snap : Arc<InRustGraph> = runtime . graph . clone ();
  let straddle : GraphStraddle = straddle ();
  let restriction : SkgrepoRestriction = restriction ();
  let deleted_pids : HashSet<ID> = HashSet::from ([ ID::from ("gone") ]);
  let deleted_extra_ids : HashMap<ID, HashSet<ID>> =
    HashMap::from ([ ( ID::from ("gone"),
                       HashSet::from ([ ID::from ("gone2") ]) ) ]);
  let tree : Tree<Viewnode> =
    Tree::new ( crate::types::viewnode::viewforest_root_viewnode () );
  let some_node : NodeId = tree . root () . id ();
  let nodes_before : HashSet<NodeId> = HashSet::from ([ some_node ]);
  for in_diffView in [false, true] {
    let mut editable_map : EditableMap = EditableMap::new ();
    let mut errors : Vec<String> = Vec::new ();
    let mut sink : Vec<CompletionWarning> = Vec::new ();
    let drawing : Option<DiffViewSource> = in_diffView . then ( ||
      DiffViewSource { straddle : &straddle,
                       diffView_buffer_kind : DiffViewBufferKind::AllChanges } );
    let context : CompletionContext = CompletionContext::for_rerender (
      &runtime, &graph_snap, &mut editable_map, &mut errors,
      &deleted_pids, &deleted_extra_ids, Some (&restriction),
      true, true, Some (&mut sink), drawing,
      &nodes_before, &nodes_before );
    assert_eq! ( settings_of (&context), Settings {
      deleted_by_this_save_pids      : deleted_pids . clone (),
      deleted_by_this_save_extra_ids : deleted_extra_ids . clone (),
      skgrepo_restriction            :
        ( ! in_diffView ) . then ( || restriction . clone () ),
      node_budget                    : runtime . config . initial_node_limit,
      create_partnerFolders_for_fresh_nodes : ! in_diffView,
      substitute_existing_content_overrides : ! in_diffView,
      has_warning_sink               : true,
      diffView_buffer_kind           :
        in_diffView . then_some (DiffViewBufferKind::AllChanges),
      nodes_before_completion        : nodes_before . clone (),
      editable_before_completion     : nodes_before . clone (), },
      "in the diff view: {}", in_diffView ); }}
