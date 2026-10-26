use crate::to_org::complete::contents::clobberWriteProtectedViewnode;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::viewnode::mk_unknown_viewnode;
use crate::to_org::util::{EditableMap, make_writeProtected_if_repeat_then_extend_editable_map};
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::env::find_skgrepo_with_optional_tantivy;
use crate::types::phantom::home_from_disk;
use crate::types::nodes::complete::Graphnode;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::util::setlike_vector_subtraction;
use crate::types::viewnode::{
    Viewnode, ViewnodeKind, Editability,
    ViewRequest, mk_editable_viewnode};
use crate::types::viewnode::{Vognode, Folder, Unknown};
use crate::types::tree::moves::move_to_last_child;
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_skgrepo_from_ancestor, read_at_ancestor_in_tree, read_at_node_in_tree};
use crate::types::tree::viewnode_graphnode::{
    pid_and_skgrepo_from_viewnode_at,
    write_at_unrestrictedVognode_in_tree};
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::util::{
    KeptChildOrder,
    complete_managed_children_in_viewforest,
    partition_children,
    treat_certain_children};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

enum ContentReality {
  Real, // Real content: Exists in worktree, and parent contains it at this position.
  Unknown, // A content id that resolves to nothing (a dangling reference) -> an Unknown phantom, rather than aborting the whole view.
}

struct ChildData {
  title        : String,
  home_skgrepo : SkgrepoName,
  body         : Option<String>,
  kind         : ContentReality,
  /// The exact stored relRepo of an unresolved relationship member.
  /// Known children derive their display facts from their graphnode;
  /// an Unknown has no home, so only this retained relationship fact can show
  /// its optional relRepo herald.
  relRepo : Option<SkgrepoName>,
}

/// UnrestrictedVognode content reconcile + content-child creation, for one node, in the
/// level-order BFS visit. View completion (dispatch_node_update)
/// settles the node's
/// Finalizable state *before* calling this (via 'apply_editability_rule')
/// and passes:
/// - `settled`: the editability rule (see 'to_org/extend/editable.rs') already ran for this node (it carried a
///   ViewRequest::Editable), so its map entry and write-protected/def are already
///   correct -- skip 'make_write-protected_if_repeat_then_extend_defmap', which would
///   otherwise write-protect a just-made-Final node against its own entry.
/// - `cascade`: this node is Final (EVR-made); by the cascade it hands a
///   ViewRequest::Editable to each of its affected content children so the
///   BFS makes each Final (clobbering competing Tentative occurrences).
/// - `node_budget`: the remaining node budget (see 'to_org/extend/editable.rs'); this
///   extension costs one unit of it. None when this extension is not
///   charged (in a diff view, all but a request's).
pub fn extend_true_content_at_unrestrictedVognode (
  node               : NodeId,
  tree               : &mut Tree<Viewnode>,
  editable_map                   : &mut EditableMap,
  config             : &SkgConfig,
  graph_snap                     : &Arc<InRustGraph>,
  skgrepo_restriction            : Option<&SkgrepoRestriction>,
  settled                        : bool,
  cascade                        : bool,
  node_budget                    : Option<&mut usize>,
) -> Result<(), Box<dyn Error>> {
  // Phantoms are diff placeholders; this pass mutates and extends
  // real content nodes.
  error_unless_node_satisfies(
    tree, node,
    |vn : &Viewnode| matches!( &vn . kind,
                                ViewnodeKind::Vognode (Vognode::Unrestricted (_))),
    "extend_true_content_at_unrestrictedVognode: expected Unrestricted vognode" ) ?;
  if ! settled {
    // A EVR node was already resolved by apply_editability_rule; running
    // the dedup here would write-protect it against its own (just-inserted)
    // map entry. Ordinary nodes still dedup first-wins (Tentative),
    // but that a member takes the claim from a non-member.
    make_writeProtected_if_repeat_then_extend_editable_map(
      tree, node, editable_map, graph_snap, config ) ?; }
  let (pid, initial_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_viewnode_at( tree, node,
                                  "extend_true_content_at_unrestrictedVognode" ) ?;
  { let is_writeProtected : bool =
      read_at_node_in_tree( tree, node,
        |vn : &Viewnode| match &vn . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (t))
            => t . is_writeProtected (),
          _ => false } ) ?;
    if is_writeProtected {
      clobberWriteProtectedViewnode( tree, node, graph_snap, config ) ?;
      return Ok (( )); }}
  // The node budget: this vognode is editable and about to extend -- extend its whole
  // content group, and (via the BFS) its folders. Each extension costs one budget
  // unit; a write-protected node (returned above) costs nothing, and a folder fills
  // for free. visit_normal_node already forced this node write-protected if the
  // budget was 0, so here it is > 0; saturating_sub is defensive.
  if let Some (node_budget) = node_budget {
    *node_budget = node_budget . saturating_sub (1); }
  let graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      graph_snap, config, &pid, &initial_skgrepo ) ?;
  // Every editable node re-syncs title/body/repo from the snapshot,
  // saved and collateral alike. (After extraction the snapshot already reflects
  // the saved buffer's text, so re-syncing the saved node yields the same
  // content it just defined -- a no-op.)
  sync_unrestrictedVognode_from_disk (tree, node, &graphnode) ?;
  reconcile_content_children (
    tree, node, &graphnode, config, graph_snap,
    skgrepo_restriction ) ?;
  if cascade {
    pass_editableViewRequest_to_content_children( tree, node ) ?; }
  order_children_as_non_vognodes_then_ignored_then_content(
    tree, node ) ?;
  Ok(( )) }

/// The cascade (see 'to_org/extend/editable.rs'): hand a ViewRequest::Editable to each affected,
/// non-phantom Unrestricted content child of a Final node -- new and existing
/// alike -- so the main BFS makes each Final (and it in turn cascades to its
/// own content). The cascade does *not* flow through non-vognodes, so only
/// member content children receive it.
fn pass_editableViewRequest_to_content_children (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let child_skgids : Vec<NodeId> =
    tree . get (node) . unwrap ()
    . children ()
    . filter ( |c| matches!( &c . value () . kind,
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . claimsMembership ) )
    . map ( |c| c . id () )
    . collect ();
  for cid in child_skgids {
    write_at_unrestrictedVognode_in_tree (
      tree, cid,
      |t| { t . view_requests . insert ( ViewRequest::Editable ); } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  Ok (( )) }

/// Overwrite the viewnode's title, skgrepo, and body with the fresh values from
/// the snapshot. Every editable node re-syncs, saved and collateral
/// alike -- after extraction the snapshot already holds the saved buffer's text,
/// so the saved node re-syncs to the same content it just defined.
fn sync_unrestrictedVognode_from_disk (
  tree         : &mut Tree<Viewnode>,
  node         : NodeId,
  graphnode : &Graphnode,
) -> Result<(), Box<dyn Error>> {
  let disk_title   : String = graphnode . title . clone ();
  let disk_body    : Option<String> = graphnode . body . clone ();
  let disk_skgrepo : SkgrepoName = graphnode . home_skgrepo . clone ();
  write_at_unrestrictedVognode_in_tree (
    tree, node,
    |t| { t . title = disk_title;
          t . home_skgrepo = disk_skgrepo;
          if let Editability::Editable { body, .. }
            = &mut t . editability
            { *body = disk_body; }} ) ?;
  Ok (( )) }

/// Compute the content goal list (diff-aware), reconcile
/// children against it, and mark any surviving non-goal children
/// as not claiming membership.
fn reconcile_content_children (
  tree                           : &mut Tree<Viewnode>,
  node                           : NodeId,
  graphnode                   : &Graphnode,
  config                         : &SkgConfig,
  graph_snap                     : &Arc<InRustGraph>,
  skgrepo_restriction            : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  // Resolve each id through the in-Rust-graph extra_id map so that an
  // id appearing in a node's 'contains' after merging into another
  // node is redirected to the acquirer's pid. Without this, the
  // rerender would carry the raw (now deleted) acquiree through to
  // the goal list and entombstone it in its parent's
  // subtree, even though the parent's containment logically points
  // at the acquirer. (Fresh views already got this for free from
  // 'pid_and_repo_from_id'; this makes the rerender consistent.)
  let content_members : Vec<RelPartner<RelationshipMemberKey>> =
    // relRepo gating ('docs/telescopes.org', "What lives where"): a
    // membership whose repo is restricted is invisible here even
    // when the member node itself is unrestricted -- the private
    // reading-list case. Node-repo omission still happens
    // below, in omit_restricted_members.
    graph_snap . outbound_member_keys_for_relation_gated (
      &graphnode . pid, NodeRelation::Contains, skgrepo_restriction );
  let content_skgids : Vec<ID> = content_members . iter ()
    . map ( |m| m . member . id () . clone () )
    . collect ();
  let unresolved_relRepos : HashMap<ID, SkgrepoName> =
    // The members the graph does not resolve are shown as unknown
    // phantoms, with their relationships' relRepos.
    content_members . iter ()
    . filter ( |m| m . member . is_unresolved () )
    . map ( |m| ( m . member . id () . clone (), m . relRepo . clone () ))
    . collect ();
  let is_sub : bool = is_subscribee (tree, node) ?;
  // An editable subscribee-as-such (the glossary's "subscribee as such") regenerates its content as
  // contains-minus-hides, saved and collateral views alike. (View-update is
  // strictly post-extraction, so the hide edits are already in the graph and
  // regenerating contains-minus-hides is correct. The cascade extends
  // subscribee content through this same path.)
  let apparent_content_skgids : Vec<ID> =
    content_goal_list( tree, node, &content_skgids, is_sub, config,
                       graph_snap, skgrepo_restriction ) ?;
  let apparent_content_skgids : Vec<ID> =
    // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): rendering omits every
    // restricted member from the goal (the weave preserves them at save).
    // A tombstone where a now-hidden node stood survives anyway --
    // it is unmanaged by this reconciler, not goal-matched.
    omit_restricted_members (
      apparent_content_skgids, skgrepo_restriction,
      |skgid : &ID| graph_snap . pid_and_skgrepo (skgid)
                 . map ( |(_pid, src)| src )
                 . or_else ( || home_from_disk (skgid, config) ));
  // The content group is extended whole -- never truncated mid-group. The
  // budget is spent once per extending vognode (in extend_true_content_at_unrestrictedVognode),
  // not per child, so a node either fully extends or is left write-protected; we
  // never create a silent partial sibling set.
  // A content child whose node vanished stays here; at its own BFS visit it
  // becomes a tombstone (or, in a diff view, a ghost) whose folders
  // generalized-orphan and entombstone -- so no folder reconciles against a
  // missing Graphnode -- while any user subtree under it is preserved (made a
  // non-member), and a now-childless tombstone is removed by the prune sweep
  // ('prune_childless_tombstones').
  adopt_nonMember_children_that_are_content (
    tree, node, &apparent_content_skgids ) ?;
  complete_content_children(
    tree, node, &apparent_content_skgids, &unresolved_relRepos,
    &graphnode . home_skgrepo, config, graph_snap,
    skgrepo_restriction ) ?;
  mark_erroneous_content_children_as_indep(
    tree, node, &apparent_content_skgids ) ?;
  convert_nonmember_unknown_children_to_tombstones(
    tree, node, &apparent_content_skgids ) ?;
  Ok (( )) }

/// For each entry of the goal list with no member child, the first
/// non-member child standing for the entry's node becomes the member,
/// keeping its viewnode and fold state, so that the reconciler, which
/// matches members only, does not create a second one beside it. (Such
/// a non-member is typically left from when the node was
/// write-protected.) The reconciler then moves it into goal order.
fn adopt_nonMember_children_that_are_content (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
  goal : &[ID],
) -> Result<(), Box<dyn Error>> {
  let adoptees : Vec<NodeId> = {
    let node_ref : NodeRef<Viewnode> = tree . get (node)
      . ok_or ("adopt_nonMember_children_that_are_content: node not found") ?;
    let unrestricted_children : Vec<(NodeId, &ID, bool)> =
      node_ref . children ()
      . filter_map ( |child| match &child . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          Some (( child . id (), &t . skgid, t . claimsMembership )),
        _ => None, } )
      . collect ();
    goal . iter ()
      . filter ( |entry| ! unrestricted_children . iter () . any (
        |(_, skgid, claimsMembership)| *skgid == *entry
                                    && *claimsMembership ))
      . filter_map ( |entry| unrestricted_children . iter () . find (
        |(_, skgid, claimsMembership)| *skgid == entry
                                    && ! *claimsMembership )
        . map ( |(treeid, _, _)| *treeid ))
      . collect () };
  for adoptee in adoptees {
    write_at_unrestrictedVognode_in_tree (
      tree, adoptee,
      |t| { t . claimsMembership = true; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  Ok (( )) }

/// An Unknown content placeholder (a dangling reference the parent kept
/// rather than failing the whole view) that is *no longer a member* of the
/// parent's contains converts to a tombstone; the postorder prune sweep
/// then removes it. An Unknown still in contains is retained (a present-but-
/// unresolvable reference). Self-deletion is the sweep's job -- this only
/// decides member-vs-convert, so a Tombstone -> Unknown -> Tombstone chain collapses in
/// one sweep.
fn convert_nonmember_unknown_children_to_tombstones (
  tree          : &mut Tree<Viewnode>,
  node          : NodeId,
  member_skgids : &[ID],
) -> Result<(), Box<dyn Error>> {
  let member_set : HashSet<ID> =
    member_skgids . iter () . cloned () . collect ();
  treat_certain_children(
    tree, node,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
        ! member_set . contains (&u . skgid),
      _ => false },
    |vn : &mut Viewnode| { vn . replace_kind (ViewnodeKind::Tombstone); },
  ) . map_err( |e| -> Box<dyn Error> { e . into() } ) }
/// Whether this node claims membership
/// and is a child of SubscribeeFolder (and not a phantom).
fn is_subscribee (
  tree : &Tree<Viewnode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let is_member_of_parent : bool =
    read_at_node_in_tree( tree, node,
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => t . claimsMembership,
        _ => false } ) ?;
  let parent_is_subscribeeFolder : bool =
    read_at_ancestor_in_tree( tree, node, 1,
      |vn : &Viewnode| matches!( &vn . kind,
        ViewnodeKind::Folder (Folder::Subscribee)))
    . unwrap_or (false);
  Ok( is_member_of_parent && parent_is_subscribeeFolder ) }

/// The worktree content goal list for a node: the (extra-id-resolved) contains,
/// in order. For a subscribee-as-such (the glossary's "subscribee as such") it is contains minus what the
/// subscriber neither hides nor contains: the hidden remainder shows in the
/// HiddenInSubscribeeFolder, and content the subscriber has integrated into its
/// own graph (any node it contains) has dropped out of "unintegrated subscribed
/// content" entirely. Hiding is the explicit integration signal; containing is
/// the implicit one (a node moved into the subscriber is not also hidden --
/// "hiding your own content is silly", resolve_visibility -- so without the
/// contains subtraction it would double-show, once as integrated content and
/// once as unintegrated; 'docs/forks.org', "Unintegrated content: the
/// subscribeeFolder").
fn content_goal_list (
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  content_skgids     : &[ID],
  is_subscribee      : bool,
  config             : &SkgConfig,
  graph_snap         : &Arc<InRustGraph>,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<Vec<ID>, Box<dyn Error>> {
  if !is_subscribee {
    Ok ( content_skgids . to_vec () )
  } else {
    let (grandparent_pid, grandparent_skgrepo) : (ID, SkgrepoName) =
      pid_and_skgrepo_from_ancestor( tree, node, 2,
                                    "content_goal_list" ) ?;
    let grandparent_graphnode : Graphnode =
      graphnode_graphFirst_by_pid_and_skgrepo (
        graph_snap, config, &grandparent_pid, &grandparent_skgrepo ) ?;
    // Resolve the subtrahends through extra_id -> pid the same way
    // 'content_ids' (the minuend) was resolved by the caller. Without
    // this, a child the subscriber has integrated under a now-merged id
    // (its contains holds the pre-merge acquiree alias, while the
    // subscribee's contains holds the acquirer's primary) would not
    // cancel, and would double-show as unintegrated subscribed content.
    // relRepo gating ('docs/telescopes.org', "What lives where"): a member
    // the grandparent-subscriber hides or contains only in a skgrepo
    // outside the skgrepo restriction must not subtract the subscribee's
    // content here -- else a privately-contained/-hidden member
    // would vanish from a public view even though no unrestricted relationship
    // explains its absence (leak by omission). Mirrors the
    // 'reconcile_content_children' gate on 'graphnode.contains'
    // just above.
    let skgrepo_unrestricted = |skgrepo : &SkgrepoName| match skgrepo_restriction {
      None      => true,
      Some (a)  => a . is_all () || a . contains_skgrepo (skgrepo) };
    let worktree_hidden : Vec<ID> =
        grandparent_graphnode . hidesFromSubs
        . or_default () . iter ()
        . filter ( |m| skgrepo_unrestricted (& m . relRepo) )
        . map ( |m| m . member . clone () )
        . collect ();
    let subscriber_contains : Vec<ID> =
        grandparent_graphnode . contains . iter ()
        . filter ( |m| skgrepo_unrestricted (& m . relRepo) )
        . map ( |m| m . member . clone () )
        . collect ();
    Ok ( unintegrated_content_skgids (
      graph_snap, content_skgids, &worktree_hidden, &subscriber_contains ) )
  } }

/// The membership subtraction shared by subscribee reconciliation and
/// occurrence-level herald facts. Inputs preserve raw IDs; extra IDs are
/// resolved before comparing membership, without replacing overridden nodes.
pub fn unintegrated_content_skgids (
  graph               : &InRustGraph,
  content_skgids      : &[ID],
  subscriber_hidden   : &[ID],
  subscriber_contains : &[ID],
) -> Vec<ID> {
  let resolve_pids = |skgids : &[ID]| -> Vec<ID> {
    skgids . iter ()
      . map (|skgid| graph . pid_of (skgid) . unwrap_or_else (|| skgid . clone ()))
      . collect () };
  let contents : Vec<ID> = resolve_pids (content_skgids);
  let hidden : Vec<ID> = resolve_pids (subscriber_hidden);
  let contained : Vec<ID> = resolve_pids (subscriber_contains);
  setlike_vector_subtraction (
    setlike_vector_subtraction (contents, &hidden), &contained) }

/// Reconcile the node's non-parentIgnored UnrestrictedVognode children
/// against the goal list of content IDs. Missing children are created
/// as editable or unknown viewnodes as appropriate.
fn complete_content_children (
  tree               : &mut Tree<Viewnode>,
  node               : NodeId,
  goal_list          : &[ID],
  unresolved_relRepos : &HashMap<ID, SkgrepoName>, // of the members the graph does not resolve
  recorder_home      : &SkgrepoName,
  config             : &SkgConfig,
  graph_snap         : &Arc<InRustGraph>,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  let child_data : HashMap<ID, ChildData> =
    build_child_creation_data(
      tree, node, goal_list, unresolved_relRepos, config, graph_snap,
      skgrepo_restriction ) ?;
  normalize_relationship_backed_content_unknowns (
    tree, node, goal_list, unresolved_relRepos, recorder_home, graph_snap ) ?;
  // The RepairSummary is dropped: content is not a generated
  // folder, so its reconciliation is not a "repair" to warn about.
  complete_managed_children_in_viewforest(
    tree, node, KeptChildOrder::FollowsGoal,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        => t . claimsMembership,
      ViewnodeKind::Vognode (Vognode::Unknown (_))
        // An Unknown is a real raw relationship member.  Match it by
        // its raw ID so a rerender retains one placeholder rather than
        // appending another one for the same dangling relationship.
        => true,
      // Everything else is unmanaged: never matched against the goal
      // list, so it is preserved as-is. A tombstone standing for a
      // hidden node is one such; the goal omits every hidden member
      // (omit_restricted_members), so none is created here either.
      _ => false },
    |vn : &Viewnode| match &vn . kind {
      // Unrestricted and Unknown children participate. Collected
      // ids (the overridesHere original when present), so goal lists
      // stay in original IDs, an existing shown substitute satisfies
      // its original goal member, and only genuinely missing members
      // are created -- which keeps post-save rerendering stable.
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        => Ok ( t . collected_skgid () ),
      ViewnodeKind::Vognode (Vognode::Unknown (u))
        => Ok ( u . skgid . clone() ),
      _ => Err(
        "complete_content_children: managed child had no content ID"
        . to_string() ) },
    goal_list,
    |skgid : &ID| {
      let d : &ChildData = child_data . get (skgid) . ok_or_else( || format!(
        "complete_content_children: child data not pre-fetched for {}",
        skgid . 0 )) ?;
      Ok ( match d . kind {
        ContentReality::Real =>
          mk_editable_viewnode(
            skgid . clone(), d . home_skgrepo . clone(),
            d . title . clone(), d . body . clone() ),
        ContentReality::Unknown => {
          let mut unknown : Viewnode = mk_unknown_viewnode ( skgid . clone() );
          if let ViewnodeKind::Vognode (Vognode::Unknown (u)) =
            &mut unknown . kind
          { u . relRepo = d . relRepo . clone ()
              . filter ( |skgrepo| skgrepo != recorder_home ); }
          unknown } } ) },
  ) . map ( |_summary| () ) ?;
  Ok (())
}

/// An open view can still hold an Unrestricted occurrence when a save deletes its
/// graphnode but leaves the parent's exact raw membership in place.  Replace
/// that occurrence before its later BFS visit entombstones it:
/// the surviving relationship is the authoritative fact, so it is Unknown.
/// Assigning only `kind` deliberately preserves
/// the wrapper's focus and fold state.
fn normalize_relationship_backed_content_unknowns (
  tree                 : &mut Tree<Viewnode>,
  node                 : NodeId,
  goal_list            : &[ID],
  unresolved_relRepos  : &HashMap<ID, SkgrepoName>,
  recorder_home        : &SkgrepoName,
  graph_snap           : &Arc<InRustGraph>,
) -> Result<(), Box<dyn Error>> {
  treat_certain_children (
    tree, node,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =>
        restriction . claimsMembership
        && graph_snap . pid_of (&restriction . collected_skgid ()) . is_none ()
        && goal_list . contains (&restriction . collected_skgid ()),
      _ => false },
    |vn : &mut Viewnode| {
      let skgid : ID = match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =>
          restriction . collected_skgid (),
        _ => unreachable! (), };
      vn . replace_kind ( ViewnodeKind::Vognode (Vognode::Unknown ( Unknown {
          relRepo: unresolved_relRepos . get (&skgid) . cloned ()
            . filter (|skgrepo| skgrepo != recorder_home),
          relationshipEditRequest: None,
          claimsMembership: true,
          skgid }))); })
    . map_err ( |e| -> Box<dyn Error> { e . into () } )
}

/// 'erroneous content children' are children that look like content,
/// but are not actually content.
/// This marks them noClaim.
///
/// `content_ids` describes the parent's current, saved/worktree
/// `contains` list.
fn mark_erroneous_content_children_as_indep (
  tree           : &mut Tree<Viewnode>,
  node           : NodeId,
  content_skgids : &[ID],
) -> Result<(), Box<dyn Error>> {
  let content_id_set : HashSet<ID> =
    content_skgids . iter() . cloned() . collect();
  treat_certain_children(
    tree, node,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        t . claimsMembership
        // collected_id: a shown substitute is a member via its
        // original, and must not be made a non-member.
        && !content_id_set . contains( &t . collected_skgid () ),
      _ => false },
    |vn : &mut Viewnode| {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
        = vn . kind
        { t . claimsMembership = false; }},
  ) . map_err( |e| -> Box<dyn Error> { e . into() } ) }

/// Reorder children into three groups:
/// - non-vognodes first
/// - parentIgnored UnrestrictedVognodes
/// - non-ignored UnrestrictedVognodes last
/// Preserves relative order within each group.
fn order_children_as_non_vognodes_then_ignored_then_content (
  tree    : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let groups : HashMap<i32, Vec<NodeId>> =
    partition_children( tree, node,
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Folder (_)
          | ViewnodeKind::PropertyMember (_)
          | ViewnodeKind::ChangeViewnode (_)
          | ViewnodeKind::BufferRoot
          | ViewnodeKind::Tombstone                => 0,
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if ! t . claimsMembership                  => 1,
        ViewnodeKind::Vognode (Vognode::Unrestricted (_)) => 2,
        // A ghost is a former member, standing where the member was.
        ViewnodeKind::Vognode (Vognode::Ghost (_))  => 2,
        // Unknown is a content-position placeholder: order it
        // alongside the true content children rather than as
        // a non-vognode.
        ViewnodeKind::Vognode (Vognode::Unknown (_))  => 2,
      } ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  let empty : Vec<NodeId> = Vec::new();
  for &cid in groups . get( &0 ) . unwrap_or (&empty) . iter()
    . chain( groups . get( &1 ) . unwrap_or (&empty) . iter() )
    . chain( groups . get( &2 ) . unwrap_or (&empty) . iter() )
  { move_to_last_child( tree, node, cid ) ?; }
  Ok(( )) }


/// WHAT IT DOES: Maps each child of 'node' to a ChildData:
/// determines title, skgrepo, and whether it is real or unknown.
///
/// MOTIVATION: We pre-extract what will be needed
/// by the last argument to complete_content_children,
/// that is, by the closure that creates children,
/// This way the closure captures only owned/pre-computed data
/// and does not conflict with the &mut tree borrow
/// in complete_managed_children_in_viewforest.
fn build_child_creation_data (
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  goal_list          : &[ID],
  unresolved_relRepos : &HashMap<ID, SkgrepoName>, // of the members the graph does not resolve
  config             : &SkgConfig,
  graph_snap         : &Arc<InRustGraph>,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let child_skgrepos : HashMap<ID, SkgrepoName> =
    { let node_ref : NodeRef<Viewnode> =
        tree . get (node)
          . ok_or ("build_child_creation_data: node not found") ?;
      let mut m : HashMap<ID, SkgrepoName> = HashMap::new();
      for child_ref in node_ref . children() {
        match &child_ref . value() . kind {
          // Only a member Unrestricted child counts as "already present" -- the
          // same predicate complete_content_children's reconcile applies. An
          // non-member same-id child is a distinct occurrence (e.g. a
          // containerward ancestor, or a stale branch made a non-member), so its goal id
          // must still be pre-fetched here; otherwise the reconcile sends it to
          // the create closure and child_data.get(id).expect(..) panics.
          // Keys are collected ids, matching the reconcile's orderkey:
          // an existing shown substitute registers under its original.
          ViewnodeKind::Vognode (Vognode::Unrestricted (t))
            if t . claimsMembership
            => { m . insert( t . collected_skgid (),
                             t . home_skgrepo . clone()); },
          _ => {}, }}
      m };
  let mut result : HashMap<ID, ChildData> = HashMap::new();
  for skgid in goal_list {
    if result . contains_key (skgid) { continue; }
    // Skip IDs already present as children in the viewnode tree.
    // 'complete_managed_children_in_viewforest' only needs
    // ChildData for children it creates from scratch; existing
    // children keep their in-tree state. Eagerly reading disk for
    // every goal_list ID would fail (ENOENT) for children that
    // this save just deleted, since their .skg file is gone.
    if child_skgrepos . contains_key (skgid) { continue; }
    let found_skgrepo : Option<SkgrepoName> =
      if unresolved_relRepos . contains_key (skgid) { None }
      else { find_skgrepo_with_optional_tantivy (
               graph_snap, skgid, None, config ) };
    let child_skgrepo : SkgrepoName =
      match found_skgrepo
      { Some (s) => s,
        None => {
          // The id resolves to nothing (a dangling reference). Render an
          // Unknown phantom rather than aborting the whole view. This is
          // what lets the one view completion serve de-novo (which must tolerate
          // dangling refs) and makes post-save robust to them too.
          result . insert ( skgid . clone (),
            ChildData { title  : String::new (),
                        home_skgrepo : SkgrepoName::not_found (),
                        body         : None,
                        kind         : ContentReality::Unknown,
                        relRepo:
                          unresolved_relRepos . get (skgid) . cloned () } );
          continue; } };
    if skgrepo_restriction
      . is_some_and ( |restriction| !restriction . contains_skgrepo (&child_skgrepo) )
    {
      // The goal omits every hidden member ('omit_restricted_members'),
      // so this is a broken invariant, not something to draw.
      return Err ( format! (
        "build_child_creation_data: the goal names {}, which the skgrepo restriction hides",
        skgid . 0 ) . into () ); }
    let skg : Graphnode =
      graphnode_graphFirst_by_pid_and_skgrepo (
        graph_snap, config, skgid, &child_skgrepo ) ?;
    result . insert( skgid . clone(),
                   ChildData { title: skg . title . clone(),
                               home_skgrepo: skg . home_skgrepo . clone(),
                               body: skg . body . clone(),
                               kind: ContentReality::Real,
                               relRepo: None } ); }
  Ok (result) }

#[cfg(test)]
#[path = "../../../tests/unit/reconcile_content.rs"]
mod tests;
