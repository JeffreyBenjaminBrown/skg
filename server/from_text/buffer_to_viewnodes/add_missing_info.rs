/// PURPOSE:
/// What the parse ('org_to_viewforest') supplies, as it builds the
/// tree, that a headline may leave out:
/// - an occurrence's gnid becomes its pid;
/// - an occurrence typed under an alias folder becomes an alias;
/// - an occurrence without a skgrepo gets one: the graph's, or that
///   of its nearest occurrence ancestor;
/// and the record of what the parse invented ('ParseProvenance').
/// An occurrence without a gnid gets a fresh one from
/// 'viewnode_from_metadata'.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::serve::parse_metadata_sexp::ViewnodeMetadata;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::viewnode::{RelationshipEditRequest, Folder, PropertyMember, Viewnode, ViewnodeKind, Vognode};
use ego_tree::NodeRef;
use std::collections::{HashMap, HashSet};
use std::iter::once;

/// How the parse reads the gnids a buffer writes.
/// 'ThroughGraph' resolves each through the graph: an occurrence's
/// gnid becomes its pid, and where the parse supplies a node's home
/// skgrepo it reads the graph's. 'AsWritten' keeps every gnid as the
/// text writes it and consults no graph, for a caller that reads
/// only what the text says (the dirty-view dependency scan, tests of
/// fully written text).
#[derive(Clone, Copy)]
pub enum IdResolution<'a> {
  ThroughGraph (&'a InRustGraph),
  AsWritten,
}

impl IdResolution<'_> {
  /// The pid of the node 'gnid' names, if resolving through a graph
  /// that knows it.
  pub fn pid_of (
    &self,
    gnid : &ID,
  ) -> Option<ID> {
    match self {
      IdResolution::ThroughGraph (graph) => graph . pid_of (gnid),
      IdResolution::AsWritten            => None, }}

  /// The home skgrepo of the node 'gnid' names, if resolving through
  /// a graph that knows it.
  pub fn home_skgrepo_of (
    &self,
    gnid : &ID,
  ) -> Option<SkgrepoName> {
    match self {
      IdResolution::ThroughGraph (graph) =>
        graph . pid_and_skgrepo (gnid) . map ( |(_pid, skgrepo)| skgrepo ),
      IdResolution::AsWritten => None, }}
}

/// Which nodes the parse invented data for, as opposed to reading it
/// from the buffer: 'new_nodes' had no id (so they got fresh UUIDs);
/// 'inherited_skgrepo_nodes' had no skgrepo (so they inherited that of
/// their nearest occurrence ancestor). Downstream policy needs the
/// distinction the parsed tree itself has erased -- e.g. a new node
/// whose inherited skgrepo is foreign is not a foreign-creation error
/// but a rider on its ancestor's fork, whereas an explicitly foreign
/// new node is the error.
/// It also needs a fact the tree does not show:
/// 'writeProtected_skgrepos_after_save' (see
/// 'skgrepos_after_save_of_writeProtected_occurrences').
pub struct ParseProvenance {
  pub new_nodes               : HashSet<ID>,
  pub inherited_skgrepo_nodes : HashSet<ID>,
  pub writeProtected_skgrepos_after_save : HashMap<ID, SkgrepoName>,
}

impl ParseProvenance {
  /// Nothing invented yet.
  pub fn new (
    writeProtected_skgrepos_after_save : HashMap<ID, SkgrepoName>,
  ) -> ParseProvenance {
    ParseProvenance {
      new_nodes               : HashSet::new (),
      inherited_skgrepo_nodes : HashSet::new (),
      writeProtected_skgrepos_after_save, }}

  /// Record what the parse invented for 'viewnode', an occurrence.
  pub fn record (
    &mut self,
    viewnode          : &Viewnode,
    gnid_minted       : bool,
    skgrepo_inherited : bool,
  ) {
    let Some (gnid) : Option<&ID> = viewnode . occurrence_gnid ()
      else { return; };
    if gnid_minted {
      self . new_nodes . insert (gnid . clone ()); }
    if skgrepo_inherited {
      self . inherited_skgrepo_nodes . insert (gnid . clone ()); }}
}

/// Replace an occurrence's gnid with its pid, if resolving through a
/// graph that knows it.
/// (Otherwise two headlines might refer to the same node, yet appear
/// not to.) Precedes everything else the parse supplies.
pub fn replace_gnid_with_pid (
  metadata      : &mut ViewnodeMetadata,
  id_resolution : IdResolution,
) {
  if ! metadata . names_an_occurrence () { return; }
  let pid : Option<ID> =
    metadata . gnid . as_ref ()
    . and_then ( |gnid| id_resolution . pid_of (gnid) );
  if let Some (pid) = pid {
    metadata . gnid = Some (pid); }}

/// The skgrepo each ID with a write-protected occurrence will have once
/// this save lands: that of its editable occurrence in this buffer, if
/// one carries a skgrepo (it may be moving the node), else the graph's.
/// A write-protected occurrence's own skgrepo is not an instruction, and
/// it is stale whenever the buffer moved the node's editable occurrence
/// (C-c s s edits only that one). Whatever reads an ancestor's skgrepo
/// must therefore read a write-protected ancestor's from here.
/// Reads the headlines as written (gnids already pids), before the
/// parse supplies any skgrepo.
pub fn skgrepos_after_save_of_writeProtected_occurrences<'a> (
  headlines     : impl Iterator<Item = &'a ViewnodeMetadata>,
  id_resolution : IdResolution,
) -> HashMap<ID, SkgrepoName> {
  let mut editable_skgrepos : HashMap<ID, SkgrepoName> = HashMap::new ();
  let mut writeProtected_gnids : HashSet<ID> = HashSet::new ();
  for metadata in headlines {
    if ! metadata . names_an_occurrence () { continue; }
    let Some (gnid) = &metadata . gnid else { continue; };
    if metadata . writeProtected {
      writeProtected_gnids . insert (gnid . clone ());
    } else if let Some (skgrepo) = &metadata . home_skgrepo {
      editable_skgrepos . insert (gnid . clone (), skgrepo . clone ()); }}
  writeProtected_gnids . into_iter ()
    . filter_map ( |gnid| {
      let skgrepo : Option<SkgrepoName> =
        editable_skgrepos . get (&gnid) . cloned ()
        . or_else ( || id_resolution . home_skgrepo_of (&gnid) );
      skgrepo . map ( |skgrepo| (gnid, skgrepo) ) } )
    . collect () }

/// Give an occurrence headline that names no skgrepo one, if one can
/// be found:
/// - a write-protected one with a gnid the graph knows gets the
///   graph's. This is how a bare folder-member reference acquires the
///   skgrepo of the existing node it names, rather than the one of the
///   occurrence above its folder. (An editable one is never filled
///   this way, so an editable node sharing an id with a write-protected
///   one is not.)
/// - otherwise it inherits that of its nearest occurrence ancestor
///   ('skgrepo_of_nearest_occurrence_ancestor').
/// What finds none leaves the headline without one, for validation to
/// refuse. Returns whether it inherited one.
pub fn complete_occurrence_skgrepo (
  metadata                           : &mut ViewnodeMetadata,
  viewparent                         : NodeRef<Viewnode>,
  id_resolution                      : IdResolution,
  writeProtected_skgrepos_after_save : &HashMap<ID, SkgrepoName>,
) -> bool {
  if ! metadata . names_an_occurrence ()
     || metadata . home_skgrepo . is_some () { return false; }
  let from_graph : Option<SkgrepoName> =
    if metadata . writeProtected {
      metadata . gnid . as_ref ()
      . and_then ( |gnid| id_resolution . home_skgrepo_of (gnid) )
    } else { None };
  if from_graph . is_some () {
    metadata . home_skgrepo = from_graph;
    return false; }
  let inherited : Option<SkgrepoName> =
    skgrepo_of_nearest_occurrence_ancestor (
      viewparent, writeProtected_skgrepos_after_save );
  let did_inherit : bool = inherited . is_some ();
  metadata . home_skgrepo = inherited;
  did_inherit }

/// The skgrepo of the nearest occurrence at or above 'node'.
/// The walk rootward passes through ghosts, gnid-only vognodes,
/// tombstones and folders,
/// which carry no home skgrepo of their own (and never show the home
/// herald), so a new headline below a ghost, a tombstone or in a
/// partner folder inherits from the occurrence above them (and, its
/// parent reading no claims, is saved as an orphan graphnode). Any
/// other kind ends the walk and supplies none: the buffer root (so a
/// bare headline at top level, or one with no occurrence above it, is
/// refused at validation), a property, a change viewnode.
/// A write-protected occurrence's skgrepo is the one its ID will have
/// after this save (WRITEPROTECTED_SKGREPOS_AFTER_SAVE), not the one it
/// shows.
fn skgrepo_of_nearest_occurrence_ancestor (
  node                               : NodeRef<Viewnode>,
  writeProtected_skgrepos_after_save : &HashMap<ID, SkgrepoName>,
) -> Option<SkgrepoName> {
  for ancestor in once (node) . chain (node . ancestors ()) {
    match &ancestor . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (at)) =>
        return Some ( if at . is_writeProtected () {
          writeProtected_skgrepos_after_save . get (&at . gnid) . cloned ()
          . unwrap_or_else ( || at . home_skgrepo . clone () )
        } else { at . home_skgrepo . clone () } ),
      ViewnodeKind::Vognode (
        Vognode::Ghost (_) | Vognode::GnidOnly (_))
        | ViewnodeKind::Tombstone
        | ViewnodeKind::Folder (_) => continue,
      ViewnodeKind::PropertyMember (_)
        | ViewnodeKind::ChangeViewnode (_)
        | ViewnodeKind::BufferRoot => return None, }}
  None }

/// Whether an occurrence headline typed under 'viewparent' is an
/// alias: whether 'viewparent' is an alias folder.
pub fn viewparent_makes_aliases (
  viewparent : NodeRef<Viewnode>,
) -> bool {
  matches! ( &viewparent . value () . kind,
             ViewnodeKind::Folder (Folder::Alias) ) }

/// The alias an occurrence typed under an alias folder stands for:
/// its title is the alias, and it keeps its relRepo and any relRepo
/// request (an alias takes no restoreRel).
pub fn make_occurrence_an_alias (
  viewnode : &mut Viewnode,
) {
  let ViewnodeKind::Vognode (Vognode::Occurrence (t)) = &viewnode . kind
    else { return; };
  viewnode . kind = ViewnodeKind::PropertyMember (
    PropertyMember::alias (
      t . title . clone(),
      t . viewStats . relRepo . clone (),
      t . relationshipEditRequest . clone () . filter ( |request|
        matches! (request, RelationshipEditRequest::RelRepo (_)) ) )); }
