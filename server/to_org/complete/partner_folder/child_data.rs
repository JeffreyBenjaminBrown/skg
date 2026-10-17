/// Shared per-child information for PartnerFolder reconciliation.
///
/// Used by the rerender-time completers for SubscribeeFolder,
/// HiddenInSubscribeeFolder, and HiddenOutsideOfSubscribeeFolder.
///
/// Vocabulary for this module:
///
/// - A `goal_list` is the ordered list of node IDs that a folder
///   should present after completion.  The list is computed from the
///   graph.
/// - A goal child is a child Viewnode whose UnrestrictedVognode ID appears in
///   that `goal_list`, whether it already existed in the buffer or
///   was created during reconciliation.
/// - A relevant child is one this reconciliation pass is allowed to
///   manage: for PartnerFolders, an UnrestrictedVognode marked affectsParent=true.
///   Relevant children whose IDs are not in the goal list are removed
///   or otherwise demoted by the caller-specific cleanup step.
/// - `ChildData` is the pre-fetched title/repo metadata
///   needed to create any missing goal child without querying while
///   the tree is being mutated.

use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::{ID, SkgrepoName};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::viewnode::{ChangeFacts, DiffPresence, Phantom, Viewnode, ViewnodeKind, Vognode, AffectsParent, PartnerFolder, mk_writeProtected_viewnode, mk_unknown_viewnode};
use crate::update_buffer::util::{complete_relevant_children_in_viewforest, RepairSummary};
use crate::update_buffer::util::treat_certain_children;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io;

/// Per-child information needed to build a viewnode for a sharing
/// folder's child (subscribee, hidden-in-subscribee, or
/// hidden-outside-of-subscribees).
pub struct ChildData {
  pub home_skgrepo : SkgrepoName,
  pub title        : String,
  /// True when the exact stored relationship member has no current
  /// node.  It is rendered as an Unknown, never as a title-less unrestricted
  /// fallback.
  pub unknown : bool,
  pub relRepo : Option<SkgrepoName>,
}

/// Build a map from child ID to ChildData for the create-child
/// closure of `complete_relevant_children_in_viewforest`.
/// The sharing completers build this before mutation so their flow
/// stays explicit: read tree and graph facts, compute the goal list,
/// prepare child data, then reconcile folder children.
pub fn build_child_data (
  tree                           : &Tree<Viewnode>,
  folder_node                       : NodeId,
  goal_list                      : &[ID],
  relRepos                       : &HashMap<ID, SkgrepoName>,
  runtime                        : &RuntimeGeneration,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let existing_children : HashMap<ID, (SkgrepoName, String)> = {
    let node_ref : NodeRef<Viewnode> =
      tree . get (folder_node)
        . ok_or ("build_child_data: node not found") ?;
    let mut m : HashMap<ID, (SkgrepoName, String)> = HashMap::new ();
    for child_ref in node_ref . children () {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        = & child_ref . value () . kind
        { m . insert ( t . skgid . clone (),
                       ( t . home_skgrepo . clone (),
                         t . title . clone () )); }}
    m };
  let mut result : HashMap<ID, ChildData> = HashMap::new ();
  for child_skgid in goal_list {
    if result . contains_key (child_skgid) { continue; }
    match SkgEnv::find_skgrepo_in_generation (runtime, child_skgid) {
      None => { result . insert ( child_skgid . clone (),
        ChildData { home_skgrepo: SkgrepoName::not_found (), title: String::new (),
                    unknown: true,
                    relRepo: relRepos . get (child_skgid) . cloned () } ); },
      Some (child_src) => {
        // `find_repo` deliberately falls back through Tantivy.  During a
        // same-save rerender the search index can still name a just-deleted node;
        // do not let that stale hint turn a retained raw relationship member
        // into a failed disk read.  An unreadable, formerly indexed file
        // means precisely an Unknown relationship member.
        match graphnode_graphFirst_by_pid_and_skgrepo (
          &runtime . graph, &runtime . config, child_skgid, &child_src ) {
          Ok (skg) => if let Some ( (s, t) ) = existing_children . get (child_skgid) {
            result . insert ( child_skgid . clone (),
                              ChildData { home_skgrepo  : s . clone (),
                                          title   : t . clone (),
                                          unknown : false,
                                          relRepo : None } );
          } else {
            result . insert ( child_skgid . clone (),
                              ChildData { home_skgrepo  : skg . home_skgrepo . clone (),
                                          title   : skg . title . clone (),
                                          unknown : false,
                                          relRepo : None } ); },
          Err (e) if e . downcast_ref::<io::Error> ()
            . is_some_and (|io_error| io_error . kind () == io::ErrorKind::NotFound) => {
            result . insert ( child_skgid . clone (),
              ChildData { home_skgrepo: SkgrepoName::not_found (), title: String::new (),
                          unknown: true,
                          relRepo: relRepos . get (child_skgid) . cloned () } ); },
          Err (e) => return Err (e),
        }
      }
    }
  }
  Ok (result) }

/// Reconcile a PartnerFolder's children against a goal list.
///
/// The rerender-time PartnerFolder completers
/// (SubscribeeFolder, HiddenInSubscribeeFolder,
/// HiddenOutsideOfSubscribeeFolder) share the same call shape: build
/// per-child data before mutation, then call
/// `complete_relevant_children_in_viewforest` with identical
/// relevance/key/create closures. An unknown ChildData entry produces
/// an unknown phantom; any other, a write-protected viewnode marked
/// AffectsParent::True.
pub fn reconcile_partnerFolder_children_against_goal_list (
  tree          : &mut Tree<Viewnode>,
  folder_node      : NodeId,
  kind          : PartnerFolder,
  goal_list     : &[ID],
  child_data    : &HashMap<ID, ChildData>,
) -> Result<RepairSummary<ID>, Box<dyn Error>> {
  reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
    tree, folder_node, kind, goal_list, child_data, &HashMap::new () )
}

/// As above, with the raw aliases captured for nodes deleted by the current
/// save.  Only the immediate post-save rerender has this information.
pub fn reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
  tree          : &mut Tree<Viewnode>,
  folder_node      : NodeId,
  kind          : PartnerFolder,
  goal_list     : &[ID],
  child_data    : &HashMap<ID, ChildData>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
) -> Result<RepairSummary<ID>, Box<dyn Error>> {
  let label : &'static str = kind . caller_label ();
  normalize_relationship_backed_partner_unknowns (
    tree, folder_node, child_data, deleted_by_this_save_extra_ids ) ?;
  let summary : RepairSummary<ID> =
    complete_relevant_children_in_viewforest (
    tree, folder_node,
    // A RestrictedVognode child is IRRELEVANT
    // (TODO/DONE/full-schema/DONE/9-2_source-set-safety.org): the goal omits
    // every restricted member, and a retained placeholder already in the
    // folder survives as an irrelevant child (preserved as-is, not
    // goal-matched), so it needs no id.
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        => t . affectsParent == AffectsParent::True,
      ViewnodeKind::Vognode (Vognode::Phantom (crate::types::viewnode::Phantom::Unknown (_)))
        => true,
      _ => false },
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        => Ok ( t . skgid . clone () ),
      ViewnodeKind::Vognode (Vognode::Phantom (crate::types::viewnode::Phantom::Unknown (u)))
        => Ok ( u . skgid . clone () ),
      _ => Err ( format! (
        "{}: relevant child not a normal graphnode", label )) },
    goal_list,
    |skgid : &ID| {
      let d : &ChildData =
        child_data . get (skgid) . ok_or_else (
          || format! ( "{}: child data not pre-fetched for {}",
                       label, skgid . 0 )) ?;
      Ok (
        if d . unknown {
          let mut unknown : Viewnode = mk_unknown_viewnode (skgid . clone ());
          if let ViewnodeKind::Vognode (Vognode::Phantom (
            crate::types::viewnode::Phantom::Unknown (u))) = &mut unknown . kind
          { u . relRepo = d . relRepo . clone (); }
          unknown
        } else { mk_writeProtected_viewnode ( skgid . clone (),
                                              d . home_skgrepo . clone (),
                                              d . title . clone (),
                                              AffectsParent::True ) } ) },
  ) ?;
  mark_goal_children_as_folder_members (
    tree, folder_node, goal_list) ?;
  Ok (summary) }

/// An already-open PartnerFolder can still hold an Unrestricted occurrence after its
/// graphnode was deleted.  When the rebuilt raw goal retains that membership,
/// turn it into Unknown before the generic deletion pass.  Replacing only the
/// kind keeps focus/folding state but removes title, body, home, and node edits.
fn normalize_relationship_backed_partner_unknowns (
  tree       : &mut Tree<Viewnode>,
  folder_node   : NodeId,
  child_data : &HashMap<ID, ChildData>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
) -> Result<(), Box<dyn Error>> {
  treat_certain_children (
    tree, folder_node,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =>
        restriction . affectsParent == AffectsParent::True
        && (child_data . get (&restriction . skgid)
            . is_some_and (|data| data . unknown)
            || deleted_by_this_save_extra_ids . get (&restriction . skgid)
               . is_some_and (|extra_ids| extra_ids . iter () . any (
                 |raw_skgid| child_data . get (raw_skgid)
                   . is_some_and (|data| data . unknown)))),
      _ => false },
    |vn : &mut Viewnode| {
      let unrestricted_skgid : ID = match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) => restriction . skgid . clone (),
        _ => unreachable! (), };
      let skgid : ID = if child_data . get (&unrestricted_skgid)
        . is_some_and (|data| data . unknown) { unrestricted_skgid . clone () }
      else { deleted_by_this_save_extra_ids . get (&unrestricted_skgid)
        . and_then (|extra_ids| extra_ids . iter () . find (
          |raw_skgid| child_data . get (*raw_skgid)
            . is_some_and (|data| data . unknown)))
        . expect ("normalization predicate found an unknown raw member")
        . clone () };
      let relRepo : Option<SkgrepoName> = child_data . get (&skgid)
        . and_then (|data| data . relRepo . clone ());
      vn . kind = ViewnodeKind::Vognode (Vognode::Phantom (
        crate::types::viewnode::Phantom::Unknown (
          crate::types::viewnode::PhantomUnknown {
            skgid, relRepo, relRepo_request: None,
            change_facts: ChangeFacts::default (),
            readiness_request: None }))); })
    . map_err ( |e| -> Box<dyn Error> { e . into () } )
}

/// Stamp each member of a folder with the presence of its node and of
/// its relationship to the folder's anchor, from 'presence_by_skgid'.
/// Members are unrestricted vognodes or, for a node only in the
/// before-graph, unknown phantoms.
pub fn apply_presence_to_folder_members (
  tree              : &mut Tree<Viewnode>,
  folder_node       : NodeId,
  presence_by_skgid : &HashMap<ID, DiffPresence>,
) -> Result<(), Box<dyn Error>> {
  treat_certain_children (
    tree, folder_node,
    |vn : &Viewnode| matches! (
      &vn . kind,
      ViewnodeKind::Vognode (
        Vognode::Unrestricted (_) | Vognode::Phantom (Phantom::Unknown (_)) )),
    |vn : &mut Viewnode| {
      let (skgid, change_facts) : (&ID, &mut ChangeFacts) = match &mut vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          (&t . skgid, &mut t . change_facts),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
          (&u . skgid, &mut u . change_facts),
        _ => return, };
      change_facts . presence =
        presence_by_skgid . get (skgid) . copied () . unwrap_or_default (); } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::viewnode::mk_writeProtected_viewnode;

  fn skgid (text : &str) -> ID { ID::from (text) }
  fn skgrepo (text : &str) -> SkgrepoName { SkgrepoName::from (text) }

  #[test]
  fn deleted_primary_with_surviving_extra_member_becomes_unknown () {
    let primary : ID = skgid ("deleted-primary");
    let raw_extra : ID = skgid ("surviving-extra");
    let mut tree : Tree<Viewnode> = Tree::new (
      Viewnode { focused: false, folded: false, body_folded: false,
                 kind: ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee) });
    let folder : NodeId = tree . root () . id ();
    let mut child : Viewnode = mk_writeProtected_viewnode (
      primary . clone (), skgrepo ("main"), "last seen" . to_string (),
      AffectsParent::True );
    child . focused = true;
    let child_treeid   : NodeId = tree . root_mut () . append (child) . id ();
    let mut child_data : HashMap<ID, ChildData> = HashMap::new ();
    child_data . insert ( raw_extra . clone (), ChildData {
      home_skgrepo: SkgrepoName::not_found (), title: String::new (),
      unknown: true, relRepo: Some (skgrepo ("foreign")) } );
    let mut deleted_extra_ids : HashMap<ID, HashSet<ID>> = HashMap::new ();
    deleted_extra_ids . insert (
      primary, [raw_extra . clone ()] . into_iter () . collect ());

    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      &mut tree, folder, PartnerFolder::Subscribee, &[raw_extra . clone ()],
      &child_data, &deleted_extra_ids ) . unwrap ();

    let rendered = tree . get (child_treeid) . unwrap () . value ();
    assert! (rendered . focused,
      "normalization must preserve the existing view wrapper state");
    match &rendered . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) => {
        assert_eq! (unknown . skgid, raw_extra);
        assert_eq! (unknown . relRepo, Some (skgrepo ("foreign"))); },
      other => panic! ("expected raw extra member as Unknown, got {other:?}"), }
  }
}

/// See this module's header for definition of "goal child".
///
/// This function repairs surviving or newly matched goal children whose
/// membership sign is stale, so the folder continues to own them
/// as generated folder members.
fn mark_goal_children_as_folder_members (
  tree          : &mut Tree<Viewnode>,
  folder_node      : NodeId,
  goal_list     : &[ID],
) -> Result<(), Box<dyn Error>> {
  let goal_set : HashSet<ID> =
    goal_list . iter () . cloned () . collect ();
  treat_certain_children (
    tree, folder_node,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        goal_set . contains (&t . skgid),
      _ => false },
    |vn : &mut Viewnode| {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        = &mut vn . kind
        { t . affectsParent = AffectsParent::True; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }
