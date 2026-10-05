/// PURPOSE:
/// Add missing information to nodes in the viewforest. Namely:
/// - when treatment should be Alias, make it so
/// - add missing IDs where treatment is Content

use crate::types::git::RelationshipAxes;
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind};
use crate::types::maybe_placed_viewnode::MpVognode;
use crate::types::viewnode::AffectsParent;
use crate::types::viewnode::{Editability, PropertyFolder, Property};
use crate::types::misc::{ID, SkgRepoName};
use crate::types::tree::forest::MpViewForest;
use crate::types::tree::generic::do_everywhere_in_tree_dfs;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::id_resolution::replace_ids_with_pids;
use crate::types::misc::SkgConfig;
use ego_tree::{NodeId, NodeMut, NodeRef};
use std::boxed::Box;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use uuid::Uuid;

/// Which nodes enrichment INVENTED data for, as opposed to reading it
/// from the buffer: 'new_nodes' had no id (so they got fresh UUIDs);
/// 'inherited_repo_nodes' had no skgrepo (so they inherited their
/// parent's). Downstream policy needs the distinction the enriched
/// tree itself has erased -- e.g. a NEW node whose INHERITED skgrepo is
/// foreign is not a foreign-creation error but a rider on its
/// parent's fork, whereas an explicitly foreign new node is the error.
pub struct EnrichmentProvenance {
  pub new_nodes               : HashSet<ID>,
  pub inherited_skgrepo_nodes : HashSet<ID>,
}

/// PURPOSE:
/// Just read this function definition;
/// it's clearer than a restatement in English.
/// .
/// PITFALL:
/// Does not add *all* missing info.
/// 'supplement_unspecified_fields_from_disk' does some of that, too,
/// although it operates on NodeInstructions, downstream.
pub fn add_missing_info_to_viewforest(
  viewforest  : &mut MpViewForest,
  config      : &SkgConfig,
) -> Result<EnrichmentProvenance, Box<dyn Error>> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)?;
  let graph = InRustGraph::from_graphnodes (&nodes);
  add_missing_info_to_viewforest_in_graph (viewforest, &graph)
}

/// Production enrichment from the operation's immutable graph snapshot.
pub fn add_missing_info_to_viewforest_in_graph(
  viewforest : &mut MpViewForest,
  graph      : &InRustGraph,
) -> Result<EnrichmentProvenance, Box<dyn Error>> {
  let root_skgid : NodeId = viewforest . internal_root_skgid ();
  replace_ids_with_pids (viewforest, root_skgid, graph);
  let skgrepo_of_skgid : HashMap<ID, SkgRepoName> =
    skgrepos_for_repoless_ided_nodes_from_graph (viewforest, graph);
  finish_missing_info_enrichment (viewforest, root_skgid, &skgrepo_of_skgid)
}

fn finish_missing_info_enrichment(
  viewforest       : &mut MpViewForest,
  root_skgid       : NodeId,
  skgrepo_of_skgid : &HashMap<ID, SkgRepoName>,
) -> Result<EnrichmentProvenance, Box<dyn Error>> {
  let mut provenance : EnrichmentProvenance =
    EnrichmentProvenance {
      new_nodes               : HashSet::new (),
      inherited_skgrepo_nodes : HashSet::new (), };
  do_everywhere_in_tree_dfs(
    viewforest,
    root_skgid,
    true,
    &mut |mut node| {
      make_alias_if_appropriate (&mut node)?;
      fill_skgrepo_from_graph_map (&mut node, skgrepo_of_skgid);
      let skgrepo_inherited : bool =
        inherit_parent_skgrepo_if_possible (&mut node)?;
      let id_assigned : bool =
        assign_new_skgid_if_absent (&mut node)?; // Do this *after* PID replacement, so fresh UUIDs do not trigger a pointless graph lookup.
      if skgrepo_inherited || id_assigned {
        if let MpViewnodeKind::Vognode (MpVognode::Active (t))
          = & node . value () . kind
        { if let Some (skgid) = & t . skgid {
            if id_assigned {
              provenance . new_nodes . insert (skgid . clone ()); }
            if skgrepo_inherited {
              provenance . inherited_skgrepo_nodes
                . insert (skgid . clone ()); }} }}
      Ok (( )) } )?;
  Ok (provenance) }

pub fn na_affectsParent_under_visible_parent_becomes_isContainer (
  viewforest : &mut MpViewForest,
) {
  let root_skgid : NodeId =
    viewforest . internal_root_skgid ();
  let mut need_changing : Vec<NodeId> = Vec::new();
  for node_ref in viewforest . nodes() {
    // collect immuatable references to what needs changing
    let affects_parent_visible : bool =
      node_ref . parent()
      . map ( |p| p . id() != root_skgid )
      . unwrap_or (false);
    if ! affects_parent_visible { continue; }
    if let MpViewnodeKind::Vognode (MpVognode::Active (t))
      = &node_ref . value() . kind
      { if t . affectsParent == AffectsParent::NA
        { need_changing . push (node_ref . id()); }}}
  for treeid in need_changing { // change them
    let mut node_mut : NodeMut<MpViewnode> =
      viewforest . get_mut (treeid) . unwrap();
    if let MpViewnodeKind::Vognode (MpVognode::Active (t))
      = &mut node_mut . value() . kind
      { t . affectsParent = AffectsParent::True; }}}

/// Make it a Property::Alias if both:
/// - it is an ActiveVognode
/// - its parent is an AliasFolder
fn make_alias_if_appropriate(
  node: &mut NodeMut<MpViewnode>
) -> Result<(), String> {
  if let MpViewnodeKind::Vognode (MpVognode::Active (_))
    = &node . value() . kind
  { // It is a real, normal vognode.
    let affects_parent_aliasFolder : bool =
      node . parent()
      . map(|mut p|
            matches!(&p . value() . kind,
                     MpViewnodeKind::PropertyFolder (PropertyFolder::Alias)))
      . unwrap_or (false);
    if affects_parent_aliasFolder { // Make it an Alias.
      let org : &mut MpViewnode = node . value();
      let MpViewnodeKind::Vognode (MpVognode::Active (t))
        : &MpViewnodeKind
        = &org . kind
      else { unreachable!() };
      org . kind = MpViewnodeKind::Property (
        Property::Alias { text: t . title . clone(),
                      relRepo: t . viewStats . relRepo . clone (),
                      relRepo_request: t . relRepo_request . clone (),
                      relationship_axes: RelationshipAxes::default () } ); }}
  Ok (( )) }

/// Inherit parent's skgrepo if both:
/// - this is a repoless ActiveVognode
/// - its parent is an ActiveVognode with a skgrepo
/// Returns whether it inherited one.
fn inherit_parent_skgrepo_if_possible(
  node: &mut NodeMut<MpViewnode>
) -> Result<bool, String> {
  let needs_skgrepo : bool =
    match &node . value() . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . home_skgrepo . is_none(),
      _ => false, };
  if needs_skgrepo {
    let parent_skgrepo : Option<SkgRepoName> =
      node . parent() . and_then(|mut p| {
        match &p . value() . kind {
          MpViewnodeKind::Vognode (MpVognode::Active (pt))
            => pt . home_skgrepo . clone(),
          _ => None, }} );
    if let Some (skgrepo) = parent_skgrepo {
      if let MpViewnodeKind::Vognode (MpVognode::Active (t))
        = &mut node . value() . kind
      { t . home_skgrepo = Some (skgrepo);
        return Ok (true); }}}
  Ok (false) }

/// Look up, from the graph, the skgrepo of every repoless,
/// write-protected ActiveVognode that already carries an id (ids are pids
/// here). Ids the graph does not know resolve to nothing and are
/// omitted from the map, so those nodes fall through to
/// parent-inheritance in the DFS.
fn skgrepos_for_repoless_ided_nodes_from_graph (
  viewforest : &MpViewForest,
  graph      : &InRustGraph,
) -> HashMap<ID, SkgRepoName> {
  let mut skgids : HashSet<ID> = HashSet::new ();
  collect_repoless_active_skgids (viewforest . root (), &mut skgids);
  skgids . into_iter ()
    . filter_map (|skgid| graph . pid_and_skgrepo (&skgid)
      . map (|(_pid, skgrepo)| (skgid, skgrepo)))
    . collect ()
}

/// Collect the ids of repoless, WRITE_PROTECTED ActiveVognodes that
/// already carry an id. Editable nodes are excluded on purpose (see
/// 'resolve_repos_for_repoless_ided_nodes').
fn collect_repoless_active_skgids (
  node_ref : NodeRef<MpViewnode>,
  skgids      : &mut HashSet<ID>,
) {
  if let MpViewnodeKind::Vognode (MpVognode::Active (t))
    = &node_ref . value () . kind
    { if t . home_skgrepo . is_none ()
         && matches! ( t . editability, Editability::WriteProtected )
      { if let Some (skgid) = &t . skgid {
          skgids . insert ( skgid . clone () ); }}}
  for child in node_ref . children () {
    collect_repoless_active_skgids ( child, skgids ); }}

/// If the node is a repoless, WRITE_PROTECTED ActiveVognode whose id the
/// graph resolved, set its skgrepo from 'repo_of_id' (built before the
/// DFS). This is how a bare folder-member reference acquires the skgrepo of
/// the existing node it names -- something
/// 'inherit_parent_repo_if_possible' cannot do, since the viewparent
/// is a non-vognode rather than an ActiveVognode with a skgrepo. The
/// write-protected gate matches the folder above, so an editable node
/// sharing an id with a write-protected one is never filled.
fn fill_skgrepo_from_graph_map (
  node             : &mut NodeMut<MpViewnode>,
  skgrepo_of_skgid : &HashMap<ID, SkgRepoName>,
) {
  if let MpViewnodeKind::Vognode (MpVognode::Active (t))
    = &mut node . value () . kind
  { if t . home_skgrepo . is_some ()
       || ! matches! ( t . editability, Editability::WriteProtected )
    { return; }
    let resolved : Option<SkgRepoName> =
      t . skgid . as_ref ()
      . and_then ( |skgid| skgrepo_of_skgid . get (skgid) . cloned () );
    if let Some (skgrepo) = resolved {
      t . home_skgrepo = Some (skgrepo); }}}

/// Assign a new UUID to an ActiveVognode if it doesn't have an ID.
/// Returns whether it assigned one.
fn assign_new_skgid_if_absent(
  node: &mut NodeMut<MpViewnode>
) -> Result<bool, String> {
  if let MpViewnodeKind::Vognode (MpVognode::Active (t))
    = &mut node . value() . kind {
    if t . skgid . is_none() {
      let new_skgid : String = Uuid::new_v4() . to_string();
      t . skgid = Some(ID (new_skgid));
      return Ok (true); }}
  Ok (false) }
