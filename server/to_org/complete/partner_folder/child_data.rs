/// Shared per-child information for PartnerFolder reconciliation.
///
/// Used by the rerender-time completers for SubscribeeFolder,
/// HiddenInSubscribeeFolder, and HiddenOutsideOfSubscribeeFolder.
///
/// Vocabulary for this module:
///
/// - A `goal_list` is the ordered list of node IDs that a folder
///   should present after completion.  The list is computed from the
///   graph and, in diff views, from git-diff state.
/// - A goal child is a child Viewnode whose UnrestrictedVognode ID appears in
///   that `goal_list`, whether it already existed in the buffer or
///   was created during reconciliation.
/// - A relevant child is one this reconciliation pass is allowed to
///   manage: for PartnerFolders, an UnrestrictedVognode marked affectsParent=true.
///   Relevant children whose IDs are not in the goal list are removed
///   or otherwise demoted by the caller-specific cleanup step.
/// - `ChildData` is the pre-fetched title/repo/phantom metadata
///   needed to create any missing goal child without querying while
///   the tree is being mutated.

use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::git::{NodeAxes, RelationshipAxes, Sign, SkgRepoDiff};
use crate::types::misc::{ID, SkgRepoName};
use crate::types::phantom::title_for_phantom;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode, AffectsParent, PartnerFolder, mk_writeProtected_viewnode, mk_phantom_viewnode, mk_unknown_viewnode};
use crate::update_buffer::util::{complete_relevant_children_in_viewforest, RepairSummary};
use crate::update_buffer::util::treat_certain_children;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io;

/// Per-child information needed to build a viewnode for a sharing
/// folder's child (subscribee, hidden-in-subscribee, or
/// hidden-outside-of-subscribees).
///
/// `phantom: None` => normal write-protected child marked AffectsParent::True.
/// `phantom: Some(axes)` => diff-view phantom marking removal.
pub struct ChildData {
  pub home_skgrepo : SkgRepoName,
  pub title        : String,
  pub phantom      : Option<(NodeAxes, RelationshipAxes)>,
  /// True when the exact stored relationship member has no current
  /// node.  It is rendered as an Unknown, never as a title-less unrestricted
  /// fallback.
  pub unknown : bool,
  pub relRepo : Option<SkgRepoName>,
}

/// Build a map from child ID to ChildData for the create-child
/// closure of `complete_relevant_children_in_viewforest`.
/// The sharing completers build this before mutation so their flow
/// stays explicit: read tree and graph facts, compute the goal list,
/// prepare child data, then reconcile folder children.
///
/// `axes_for_removed` supplies each removed member's per-stage diff
/// axes.  The caller must name the relation its folder represents when
/// building that closure (outbound folders call `phantom_axes` with
/// their relation; inbound folders read the inverse scan; filter folders
/// compare derived membership), so an axis can never silently come
/// from a different relation involving the same ID.
pub fn build_child_data (
  tree                           : &Tree<Viewnode>,
  folder_node                       : NodeId,
  goal_list                      : &[ID],
  removed_skgids                 : &HashSet<ID>,
  axes_for_removed               : &dyn Fn (&ID, &SkgRepoName)
                                     -> (NodeAxes, RelationshipAxes),
  skgrepo_diffs                  : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
  deleted_since_head_pid_src_map : &HashMap<ID, SkgRepoName>,
  relRepos                       : &HashMap<ID, SkgRepoName>,
  runtime                        : &RuntimeGeneration,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let existing_children : HashMap<ID, (SkgRepoName, String)> = {
    let node_ref : NodeRef<Viewnode> =
      tree . get (folder_node)
        . ok_or ("build_child_data: node not found") ?;
    let mut m : HashMap<ID, (SkgRepoName, String)> = HashMap::new ();
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
    if removed_skgids . contains (child_skgid) {
      // A removed-member diff-phantom is a *non-Unrestricted* viewnode. If its
      // skgrepo can't be determined, fall back to the NOT_FOUND sentinel
      // rather than aborting the whole render (TODO/DONE/local-view-update/plan_v2.org §7.6).
      let child_src : SkgRepoName =
        SkgEnv::find_skgrepo_in_generation (
          runtime, child_skgid, deleted_since_head_pid_src_map)
        . unwrap_or_else ( SkgRepoName::not_found );
      let axes : (NodeAxes, RelationshipAxes) =
        axes_for_removed ( child_skgid, &child_src );
      let child_title : String =
        title_for_phantom ( &runtime . graph, child_skgid, &child_src,
                            skgrepo_diffs . as_ref (), &runtime . config );
      result . insert ( child_skgid . clone (),
                        ChildData { home_skgrepo  : child_src,
                                    title   : child_title,
                                    phantom : Some (axes),
                                    unknown : false,
                                    relRepo : None } );
    } else {
      match SkgEnv::find_skgrepo_in_generation (
        runtime, child_skgid, deleted_since_head_pid_src_map) {
        None => { result . insert ( child_skgid . clone (),
          ChildData { home_skgrepo: SkgRepoName::not_found (), title: String::new (),
                      phantom: None, unknown: true,
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
                                            phantom : None,
                                            unknown : false,
                                            relRepo : None } );
            } else {
              result . insert ( child_skgid . clone (),
                                ChildData { home_skgrepo  : skg . home_skgrepo . clone (),
                                            title   : skg . title . clone (),
                                            phantom : None,
                                            unknown : false,
                                            relRepo : None } ); },
            Err (e) if e . downcast_ref::<io::Error> ()
              . is_some_and (|io_error| io_error . kind () == io::ErrorKind::NotFound) => {
              result . insert ( child_skgid . clone (),
                ChildData { home_skgrepo: SkgRepoName::not_found (), title: String::new (),
                            phantom: None, unknown: true,
                            relRepo: relRepos . get (child_skgid) . cloned () } ); },
            Err (e) => return Err (e),
          }
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
/// relevance/key/create closures. Phantom-flagged ChildData entries
/// produce phantom viewnodes; non-phantom entries produce
/// write-protected viewnodes marked AffectsParent::True.
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
        } else { match d . phantom {
          None => mk_writeProtected_viewnode ( skgid . clone (),
                                             d . home_skgrepo . clone (),
                                             d . title . clone (),
                                             AffectsParent::True ),
          Some ((ex, mem)) =>
            mk_phantom_viewnode (
              skgid . clone (), d . home_skgrepo . clone (),
              d . title . clone (), ex, mem ) } } ) },
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
      let relRepo : Option<SkgRepoName> = child_data . get (&skgid)
        . and_then (|data| data . relRepo . clone ());
      vn . kind = ViewnodeKind::Vognode (Vognode::Phantom (
        crate::types::viewnode::Phantom::Unknown (
          crate::types::viewnode::PhantomUnknown {
            skgid, relRepo, relRepo_request: None }))); })
    . map_err ( |e| -> Box<dyn Error> { e . into () } )
}

/// Stamp per-stage membership signs onto a folder's existing Unrestricted
/// members, from a per-member axes map (an outbound folder reads the
/// recorder's relation diff via 'outbound_member_axes'; an inbound folder
/// reads the inverse scan):
/// - a member PRESENT after both stages gets only its Plus signs
///   ('addedR'), mirroring 'mark_relationship_axes_on_existing_children's
///   rule for content children (Minus positions are phantoms,
///   handled by the goal list);
/// - an Unrestricted child whose net result is REMOVED -- reachable only
///   when a stale saved buffer still holds, as an Unrestricted member, a
///   write-protected-folder member whose relationship is gone -- gets the full axes
///   and flips to a phantom, so the rendered buffer cannot show a
///   removed relationship as a live member.
pub fn apply_relationship_axes_to_folder_members (
  tree       : &mut Tree<Viewnode>,
  folder_node   : NodeId,
  axes_by_skgid : &HashMap<ID, RelationshipAxes>,
) -> Result<(), Box<dyn Error>> {
  if axes_by_skgid . is_empty () { return Ok (( )); }
  treat_certain_children (
    tree, folder_node,
    |vn : &Viewnode| matches! (
      &vn . kind, ViewnodeKind::Vognode (Vognode::Unrestricted (_)) ),
    |vn : &mut Viewnode| {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        = &mut vn . kind
      { if let Some (m) = axes_by_skgid . get (&t . skgid) {
          if m . net_is_present () {
            if m . staged   == Some (Sign::Plus)
              { t . relationship_axes . staged   = Some (Sign::Plus); }
            if m . unstaged == Some (Sign::Plus)
              { t . relationship_axes . unstaged = Some (Sign::Plus); }
          } else {
            t . relationship_axes = *m; }}}
      vn . normal_to_phantom (); // flips only when the axes require
    } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::viewnode::{mk_writeProtected_viewnode, Phantom};

  fn skgid (text : &str) -> ID { ID::from (text) }
  fn skgrepo (text : &str) -> SkgRepoName { SkgRepoName::from (text) }

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
      home_skgrepo: SkgRepoName::not_found (), title: String::new (), phantom: None,
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
/// membership marker is stale, so the folder continues to own them
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
        goal_set . contains (&t . skgid)
        && ! t . should_be_diffPhantom (),
      _ => false },
    |vn : &mut Viewnode| {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        = &mut vn . kind
        { t . affectsParent = AffectsParent::True; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }
