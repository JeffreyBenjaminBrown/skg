// The two constructors of 'CompletionContext' give the settings
// that the literal constructions in
// 'complete_viewforest_with_graph_straddle' and 'rerender_view'
// wrote, field by field.

use super::*;
use crate::dbs::init::empty_in_ram_tantivy_index;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::misc::SkgConfig;
use crate::types::views_state::DiffViewBufferKind;

use std::collections::HashMap;

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

/// The settings of a context that do not borrow mutable state.
#[derive(Debug, PartialEq)]
struct Settings {
  skgrepo_restriction            : Option<SkgrepoRestriction>,
  node_budget                    : usize,
  diffView_mode                  : Option<(*const GraphStraddle, DiffViewBufferKind)>, // None for a content view
  nodes_before_completion        : HashSet<NodeId>,
  editable_before_completion     : HashSet<NodeId>,
}

fn settings_of (
  context : &CompletionContext,
) -> Settings {
  Settings {
    skgrepo_restriction            : context . skgrepo_restriction . cloned (),
    node_budget                    : context . node_budget,
    diffView_mode                  : match context . mode {
      CompletionMode::ContentView => None,
      CompletionMode::DiffView { straddle, buffer_kind } =>
        Some (( straddle as *const GraphStraddle, buffer_kind )), },
    nodes_before_completion        : context . nodes_before_completion . clone (),
    editable_before_completion     : context . editable_before_completion . clone (), } }

#[test]
fn for_diffView_gives_the_first_renderings_settings () {
  let runtime : RuntimeGeneration = runtime ();
  let graph_snap : Arc<InRustGraph> = runtime . graph . clone ();
  let straddle : GraphStraddle = straddle ();
  let mut editable_map : EditableMap = EditableMap::new ();
  let mut errors : Vec<String> = Vec::new ();
  let context : CompletionContext = CompletionContext::for_diffView (
    &runtime, &graph_snap, &mut editable_map, &mut errors,
    &straddle, DiffViewBufferKind::Merges );
  assert_eq! ( settings_of (&context), Settings {
    skgrepo_restriction            : None,
    node_budget                    : runtime . config . initial_node_limit,
    diffView_mode                  :
      Some (( &straddle as *const GraphStraddle, DiffViewBufferKind::Merges )),
    nodes_before_completion        : HashSet::new (),
    editable_before_completion     : HashSet::new (), } ); }

/// A rerender of a buffer of the diff view takes the first
/// rendering's settings, and keeps what the view held before.
#[test]
fn for_rerender_gives_the_first_renderings_settings_and_keeps_the_rest () {
  let runtime : RuntimeGeneration = runtime ();
  let graph_snap : Arc<InRustGraph> = runtime . graph . clone ();
  let straddle : GraphStraddle = straddle ();
  let tree : Tree<Viewnode> =
    Tree::new ( crate::types::viewnode::viewforest_root_viewnode () );
  let some_node : NodeId = tree . root () . id ();
  let nodes_before : HashSet<NodeId> = HashSet::from ([ some_node ]);
  let mut editable_map : EditableMap = EditableMap::new ();
  let mut errors : Vec<String> = Vec::new ();
  let mode : CompletionMode =
    CompletionMode::DiffView { straddle    : &straddle,
                               buffer_kind : DiffViewBufferKind::AllChanges };
  let context : CompletionContext = CompletionContext::for_rerender (
    &runtime, &graph_snap, &mut editable_map, &mut errors,
    mode, &nodes_before, &nodes_before );
  assert_eq! ( settings_of (&context), Settings {
    skgrepo_restriction            : None,
    node_budget                    : runtime . config . initial_node_limit,
    diffView_mode                  : Some ((
      &straddle as *const GraphStraddle, DiffViewBufferKind::AllChanges )),
    nodes_before_completion        : nodes_before . clone (),
    editable_before_completion     : nodes_before . clone (), } ); }
