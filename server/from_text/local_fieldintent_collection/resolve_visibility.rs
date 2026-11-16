//! This file defines visibility resolution, which consumes the
//! 'ForkeeVisibility' signals that collection put in the map.
//! It is async because it consults disk.
//! .
//! The policy of the forkee-visibility step, for forker R
//! viewing forkee E, is:
//! .
//! - If N is graph-content of forkee E,
//!   and E has a viewchild with ID N and claimsMembership,
//!   then N is intended to be visible through this fork,
//!   so it is removed from the hides of forker R.
//! - If N is graph-content of E, E has no viewchild with ID N and
//!   claimsMembership, and N is not graph-content of R after the
//!   save, then N is hidden from R.
//! - If N is a claimsMembership viewchild of E but is not
//!   graph-content of E, then it is not a hiderel edit. If the graph
//!   lacks N (the user typed it there) it joins E's content, when the
//!   user owns E, and the save is refused otherwise
//!   ('add_children_typed_under_member_as_such'). Any other such N is not
//!   touched: the refresh of the save's rerender makes it vanish.
//! - A foreign R records no hides: a save that would hide or unhide
//!   anything in R is refused ('cut_under_a_foreign_note').
//! - An E the graph lacks (typed there) has no list to lose: what it
//!   shows is its contains ('set_contains_of_new_member_as_such').
//! .
//! A forker F shown as such in the forks folder of a note N gives a
//! signal of its own ('ForkerVisibility'), read by the
//! forker-visibility step ('resolve_forker_visibility'): F shows its
//! content less what N contains or hides, so a child absent there is
//! never removed from F. A child cut from F makes N fork F, and N
//! hides it unless N contains it after the save.
//! .
//! The skgrepo restriction limits what can be edited as well as what
//! can be seen ('server/skgrepo_sets.rs'), so both E's content and R's
//! hides are read by the records the
//! skgrepo restriction shows, as the view drew them: a hide whose
//! record is restricted is neither removed nor moved, and no hide is
//! inferred of a member that only a restricted record of E's
//! explains.
//! .
//! A forks folder adds one more signal ('ForksFolderMembers'): a note
//! that forks the forker, that the forker does not fork after
//! the save, and that its forks folder no longer shows, was deleted
//! there, and is hidden from it ('resolve_forks_folder_removals').
//! .
//! Resolution cannot run during collection, even in principle: it
//! needs disk (the forkee's contains, the forker's hides)
//! and the forker's own post-save contains -- a finished entry
//! elsewhere in the same map.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::{Mirrors, mirrors_of};
use crate::dbs::node_lookup::opt_graphnode_by_gnid;
use crate::refresh::goal::fork_partners_shown;
use crate::from_text::local_fieldintent_collection::lower::LoweredNodeIntents;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::from_text::local_fieldintent_collection::types::{
  ForksFolderMembers, HiddenOutsideEdit, ForkeeVisibility, ForkerVisibility };
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::PostSkgsaveCommitNoticeCandidate;

use std::collections::{HashMap, HashSet};
use std::error::Error;

/// The 'visibility' pairs are (forker, signal), and the
/// 'forker_visibility' pairs (the note whose forks folder shows the
/// forker, signal), as 'lower_collected_fieldIntents' extracted them
/// from the map.
pub fn resolve_visibility (
  mut lowered : LoweredNodeIntents,
  visibility  : &[(ID, ForkeeVisibility)],
  forker_visibility : &[(ID, ForkerVisibility)],
  hidden_outside : &[(ID, HiddenOutsideEdit)],
  forks_folder_members : &[(ID, ForksFolderMembers)],
  graph       : &InRustGraph,
  config      : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(LoweredNodeIntents, Vec<PostSkgsaveCommitNoticeCandidate>), Box<dyn Error>> {
  validate_no_overlapping_forkee_hiderel_conflicts (
    visibility, forker_visibility, graph, config, skgrepo_restriction ) ?;
  infer_hides_from_contains_removals (
    // Before the signal loops below, so that an explicit
    // member-as-such gesture about the same child wins.
    &mut lowered, visibility, forker_visibility, graph, config,
    skgrepo_restriction ) ?;
  for (forker, signal) in visibility {
    let Some (forkee_from_disk) =
      opt_graphnode_by_gnid (
        graph, config, &signal . forkee ) ?
    else { lowered . set_contains_of_new_member_as_such (
             &signal . forkee, &signal . visible );
           continue; };
    let Some (forker_from_disk) =
      opt_graphnode_by_gnid (
        graph, config, forker ) ?
    else { continue; };
    add_children_typed_under_member_as_such (
      &mut lowered, &forkee_from_disk, MemberAsSuch::Forkee, &signal . visible,
      graph, config, skgrepo_restriction ) ?;
    let forker_contains : HashSet<ID> =
      lowered . forker_contains_after_save (
        &forker_from_disk );
    let visible_content : HashSet<ID> =
      signal . visible . iter() . cloned() . collect();
    let forkee_contains : Vec<ID> = // what the buffer could show of the forkee's content
      members_with_visible_records (
        &forkee_from_disk . contains, skgrepo_restriction );
    let forker_hides : Vec<ID> = // a restricted hide the buffer could not show, so no save removes it
      members_with_visible_records (
        forker_from_disk . hides . or_default (),
        skgrepo_restriction );
    let inferred_hides : Vec<ID> =
      forkee_contains . iter()
      . filter ( |gnid| ! visible_content . contains (*gnid) )
      . filter ( |gnid| ! forker_contains . contains (*gnid) )
      . cloned()
      . collect();
    let inferred_unhides : Vec<ID> =
      forkee_contains . iter()
      . filter ( |gnid| visible_content . contains (*gnid) )
      . filter ( |gnid| forker_hides . contains (*gnid) )
      . cloned() . collect();
    if inferred_hides . is_empty() && inferred_unhides . is_empty()
      { continue; }
    if ! config . skgrepo_is_owned (&forker_from_disk . home_skgrepo) {
      return Err ( Box::new ( BufferValidationError::Other (
        cut_under_a_foreign_note (
          &forker_from_disk, &forkee_from_disk, MemberAsSuch::Forkee ) ))); }
    lowered . apply_hiderel_delta_to_forker (
      forker_from_disk,
      &inferred_hides,
      &inferred_unhides ); }
  resolve_forker_visibility (
    &mut lowered, forker_visibility, graph, config,
    skgrepo_restriction ) ?;
  resolve_forks_folder_removals (
    &mut lowered, forks_folder_members, graph, config,
    skgrepo_restriction ) ?;
  let post_skgsave_commit_notice_candidates = apply_hiddenoutside_edits (
    &mut lowered, hidden_outside, graph, config,
    skgrepo_restriction ) ?;
  Ok ((lowered, post_skgsave_commit_notice_candidates)) }

/// The refusal of a save that cuts from, or shows again, the content
/// of 'member', a member as such of the forks folder of 'note', which
/// the user does not own: a cut there is for the note to hide what it
/// cut (and, from a forker, to fork that forker), and only a note of
/// one's own can record either.
fn cut_under_a_foreign_note (
  note           : &Graphnode,
  member         : &Graphnode,
  member_as_such : MemberAsSuch,
) -> String {
  format! (
    "Cannot change what {} ('{}') shows of {} ('{}'), its {}: {} is foreign, and only a note of your own can fork another and hide what that one contains.",
    note . pid, note . title, member . pid, member . title,
    member_as_such . word (), note . pid ) }

/// Which side of 'forks' a member as such of a note's forks folder
/// stands on: a forkee as such (the note forks it) or a forker as
/// such (it claims nothing there).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MemberAsSuch { Forkee, Forker }

impl MemberAsSuch {
  fn word (
    self,
  ) -> &'static str {
    match self { MemberAsSuch::Forkee => "forkee",
                 MemberAsSuch::Forker => "forker", } }}

/// Each child typed under a member M as such (a claiming child the
/// buffer shows there, of a node the graph lacks, that M's list does
/// not name) joins M's content, when the user owns M: inserted right
/// after the sibling it follows in the buffer that M's list names (or
/// another child typed there), first when none precedes it. M's
/// contains is read by the records the skgrepo restriction shows, as
/// the view drew them; disk supplementation keeps the rest where they
/// stand. Under a foreign M the save is refused: only M's owner can
/// change its content, and making it one's own (forking it) is done
/// from a view of M as itself. A child the graph has, placed under M
/// but not named by its list, is no member of M and joins nothing:
/// the save's rerender leaves it out.
fn add_children_typed_under_member_as_such (
  lowered             : &mut LoweredNodeIntents,
  member_from_disk    : &Graphnode,
  member_as_such      : MemberAsSuch,
  shown               : &[ID],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let key : &dyn Fn (&ID) -> ID =
    &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
  let mut contains : Vec<ID> =
    members_with_visible_records (
      &member_from_disk . contains, skgrepo_restriction );
  let named : HashSet<ID> =
    members_of (&member_from_disk . contains) . iter () . map (key) . collect ();
  let typed : HashSet<&ID> =
    shown . iter ()
    . filter ( |gnid| graph . pid_of (gnid) . is_none ()
                      && ! named . contains (&key (gnid)) )
    . collect ();
  if typed . is_empty () { return Ok (( )); }
  if ! config . skgrepo_is_owned (&member_from_disk . home_skgrepo) {
    return Err ( Box::new ( BufferValidationError::Other ( format! (
      "Cannot add a child under {} ('{}'), a foreign node shown as a {}: only its owner can change its content. To add one, make the node your own first: view it as itself, where an edit forks it.",
      member_from_disk . pid, member_from_disk . title, member_as_such . word () )))); }
  let mut follows : Option<usize> = None; // where the last sibling the list names, or the last typed child, stands
  for gnid in shown {
    if typed . contains (gnid) {
      let at : usize = follows . map_or (0, |position| position + 1);
      contains . insert (at, gnid . clone ());
      follows = Some (at);
    } else if let Some (position) =
      contains . iter () . position ( |member| key (member) == key (gnid) )
    { follows = Some (position); }}
  lowered . set_contains_of_member_as_such (member_from_disk . clone (), contains);
  Ok (( )) }

/// The forker-visibility step, for a forker F shown as such in the
/// forks folder of a note N (it forks N, or N forked it and the user
/// detached it there). Where F and N are fork partners, F shows its
/// content less what N contains or hides
/// ('less_what_the_forker_integrates' in 'server/refresh/goal.rs'),
/// and otherwise all of it; either way by the records the skgrepo
/// restriction shows, as the view drew it. So:
/// - a child typed under F joins F's content, when the user owns F,
///   and the save is refused otherwise
///   ('add_children_typed_under_member_as_such');
/// - a child of F that N hides, shown claiming under F (moved out of
///   F's folder of what N hides in it), is unhidden in N, as under a
///   forkee;
/// - a child F showed that the buffer no longer shows claiming under
///   F was cut from it. A cut from a forker makes N fork F (so F, which
///   forks N, becomes a mutual), and N hides each child cut that it
///   does not contain after the save, as a cut from a forkee does. A
///   forker N already forks (a mutual the user detached there, in the
///   same save) is not forked again: the detachment stands, and the
///   cut only hides. The view the save draws gives F its claim, as
///   every view the refresh draws gives each member of a forks folder
///   the claim the graph gives it ('claim_forks_folder_members_as_the_graph_does'
///   in 'server/refresh/visit.rs').
/// A foreign N can record neither a fork nor a hide, so a save that
/// cuts from F or unhides under it is refused
/// ('cut_under_a_foreign_note'). A child F does not show (one N
/// contains or hides) is absent from under F by design, and is never
/// removed from F's list. A forker the graph lacks (typed there) has
/// no list to lose: what it shows is its contains
/// ('set_contains_of_new_member_as_such'), as for a forkee typed
/// there.
fn resolve_forker_visibility (
  lowered             : &mut LoweredNodeIntents,
  signals             : &[(ID, ForkerVisibility)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  for (note, signal) in signals {
    let Some (forker_from_disk) : Option<Graphnode> =
      opt_graphnode_by_gnid (graph, config, &signal . forker) ?
      else { lowered . set_contains_of_new_member_as_such (
               &signal . forker, &signal . visible );
             continue; };
    let Some (note_from_disk) : Option<Graphnode> =
      opt_graphnode_by_gnid (graph, config, note) ?
      else { continue; };
    add_children_typed_under_member_as_such (
      lowered, &forker_from_disk, MemberAsSuch::Forker, &signal . visible,
      graph, config, skgrepo_restriction ) ?;
    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let visible : HashSet<ID> =
      signal . visible . iter () . map (key) . collect ();
    let forker_contains : Vec<ID> =
      members_with_visible_records (
        &forker_from_disk . contains, skgrepo_restriction );
    let note_hides : Vec<ID> = // a restricted hide the buffer could not show, so no save removes it
      members_with_visible_records (
        note_from_disk . hides . or_default (), skgrepo_restriction );
    let shown_before : Vec<ID> = // what the view drew under the forker
      if are_fork_partners (
           graph, skgrepo_restriction, &note_from_disk . pid, &forker_from_disk . pid )
      { let integrated : HashSet<ID> =
          note_hides . iter ()
          . chain ( members_with_visible_records (
                      &note_from_disk . contains, skgrepo_restriction ) . iter () )
          . map (key) . collect ();
        forker_contains . iter ()
          . filter ( |gnid| ! integrated . contains (&key (gnid)) )
          . cloned () . collect ()
      } else { forker_contains . clone () };
    let cut : Vec<ID> = shown_before . into_iter ()
      . filter ( |gnid| ! visible . contains (&key (gnid)) )
      . collect ();
    let inferred_unhides : Vec<ID> = {
      let forker_contains_keys : HashSet<ID> =
        forker_contains . iter () . map (key) . collect ();
      note_hides . iter ()
        . filter ( |gnid| visible . contains (&key (gnid))
                          && forker_contains_keys . contains (&key (gnid)) )
        . cloned () . collect () };
    if cut . is_empty () && inferred_unhides . is_empty () { continue; }
    if ! config . skgrepo_is_owned (&note_from_disk . home_skgrepo) {
      return Err ( Box::new ( BufferValidationError::Other (
        cut_under_a_foreign_note (
          &note_from_disk, &forker_from_disk, MemberAsSuch::Forker ) ))); }
    let inferred_hides : Vec<ID> =
      if cut . is_empty () { Vec::new () }
      else {
        let note_forks_forker : bool = // by any record, a restricted one included
          members_of ( note_from_disk . forks . or_default () ) . iter ()
          . any ( |forkee| key (forkee) == forker_from_disk . pid );
        if ! note_forks_forker {
          lowered . add_forkee_to_note (
            note_from_disk . clone (), &forker_from_disk . pid ); }
        let note_contains : HashSet<ID> =
          lowered . forker_contains_after_save (&note_from_disk) . iter ()
          . map (key) . collect ();
        cut . into_iter ()
          . filter ( |gnid| ! note_contains . contains (&key (gnid)) )
          . collect () };
    if inferred_hides . is_empty () && inferred_unhides . is_empty () { continue; }
    lowered . apply_hiderel_delta_to_forker (
      note_from_disk, &inferred_hides, &inferred_unhides ); }
  Ok (( )) }

/// Whether 'a' and 'b' are fork partners, either way, by a record the
/// skgrepo restriction shows.
fn are_fork_partners (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  a           : &ID,
  b           : &ID,
) -> bool {
  let b_pid : ID = graph . pid_else_raw_id (b);
  graph . outbound_gnids_for_relation_gated (a, NodeRelation::Forks, restriction)
    . iter () . any ( |forkee| graph . pid_else_raw_id (forkee) == b_pid )
  || graph . inbound_pids_for_relation_gated (a, NodeRelation::Forks, restriction)
    . contains (&b_pid) }

/// A note N's forks folder shows a forker of N claiming nothing (a
/// forker-only member, which N does not fork) and each mutual
/// claiming; N hides neither. Deleting either from the folder hides it
/// in N, so that it leaves the folder (where a forker N hides is not
/// drawn) for the folder of what N hides outside its fork partners,
/// whence removing it unhides it. So each note that forks N by a
/// record the skgrepo restriction shows, that N does not fork after the
/// save (a deleted mutual's fork the same save ends), that the folder
/// no longer shows, and that N does not hide already, joins N's hides.
/// A forker whose record the restriction keeps out was never drawn,
/// and one whose node it keeps out stood as a restricted viewnode,
/// whose line edits no relationship, so neither is hidden here. Only
/// an owned N is written. This runs before the edits of the
/// hidden-outside folder, which may unhide it again.
fn resolve_forks_folder_removals (
  lowered             : &mut LoweredNodeIntents,
  signals             : &[(ID, ForksFolderMembers)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  for (note, signal) in signals {
    let Some (note_from_disk) : Option<Graphnode> =
      opt_graphnode_by_gnid (graph, config, note) ?
      else { continue; };
    if ! config . skgrepo_is_owned (&note_from_disk . home_skgrepo) {
      continue; }
    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let shown : HashSet<ID> = signal . members . iter () . map (key) . collect ();
    let forked_after_save : HashSet<ID> =
      lowered . forker_forks_after_save (&note_from_disk) . iter ()
      . map (key) . collect ();
    let hidden : HashSet<ID> =
      lowered . forker_hides_after_resolution (&note_from_disk) . iter ()
      . map (key) . collect ();
    let deleted_forkers : Vec<ID> =
      graph . inbound_pids_for_relation_gated (
        &note_from_disk . pid, NodeRelation::Forks, skgrepo_restriction )
      . into_iter ()
      . filter ( |forker| ! shown . contains (forker)
                          && ! forked_after_save . contains (forker)
                          && ! hidden . contains (forker)
                          && ! graph . node_is_restricted (forker, skgrepo_restriction) )
      . collect ();
    if ! deleted_forkers . is_empty () {
      lowered . apply_hiderel_delta_to_forker (
        note_from_disk, &deleted_forkers, &[] ); }}
  Ok (( )) }

/// The members of 'list' whose records the skgrepo restriction shows
/// ('SkgrepoRestriction::record_is_visible'), as written.
fn members_with_visible_records (
  list                : &[RelPartner<ID>],
  skgrepo_restriction : &SkgrepoRestriction,
) -> Vec<ID> {
  list . iter ()
    . filter ( |member| SkgrepoRestriction::record_is_visible (
      skgrepo_restriction, &member . relRepo ))
    . map ( |member| member . member . clone () )
    . collect () }

/// Applies the submitted visible-outside subset after all ordinary hide
/// inference.  Only the old *visible outside* hides are replaceable: hides
/// whose records are restricted, and members inside a fork partner, remain owned
/// by the graph and survive an edit of this compound partner folder. A hide is
/// visible, and so replaceable, when the skgrepo restriction holds its relRepo
/// ('SkgrepoRestriction::record_is_visible'): the folder as drawn showed its
/// member, as an occurrence or a gnid-only vognode, so the submitted
/// folder is the whole set of hides with a visible record outside the
/// fork partners, and a folder saved under a skgrepo restriction never unhides a
/// member it could not show. Nor does it unhide a member it showed as a
/// restricted viewnode (its node kept out): deleting that line changes
/// no relationship to the note it stands for. What counts as inside is what the folder's goal
/// subtracts ('goal_list_for_hiddenOutsideOfForkPartners_folder'): the content
/// of each fork partner the forks folder shows, forkee or forker
/// ('fork_partners_shown'), by the records the skgrepo restriction
/// shows. So a hide of something only a forker contains, which the
/// view draws in that forker's folder of what the note hides in it,
/// is not replaced here: a save that moves it out of that folder
/// unhides it ('resolve_forker_visibility') whether or not the buffer
/// keeps this folder.
fn apply_hiddenoutside_edits (
  lowered                : &mut LoweredNodeIntents,
  edits                  : &[(ID, HiddenOutsideEdit)],
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
) -> Result<Vec<PostSkgsaveCommitNoticeCandidate>, Box<dyn Error>> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut candidates : Vec<PostSkgsaveCommitNoticeCandidate> = Vec::new ();
  for (forker, edit) in edits {
    if ! seen . insert (forker . clone ()) {
      return Err (Box::new (BufferValidationError::Other (
        format! ("More than one HiddenOutsideOfForkPartners edit was submitted for forker {}", forker) ))); }
    let Some (forker_from_disk) =
      opt_graphnode_by_gnid (graph, config, forker) ?
    else { continue; };
    if ! config . skgrepo_is_owned (&forker_from_disk . home_skgrepo) {
      continue; }

    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let is_visible : &dyn Fn (&RelPartner<ID>) -> bool =
      &|member : &RelPartner<ID>| -> bool {
      SkgrepoRestriction::record_is_visible (
        skgrepo_restriction, &member . relRepo ) };
    let partner_gnids : Vec<ID> = fork_partners_shown (
      graph, skgrepo_restriction, &forker_from_disk . pid,
      &lowered . forker_forks_after_save (&forker_from_disk) );
    let mut inside : HashSet<ID> = HashSet::new ();
    for partner_gnid in partner_gnids {
      let Some (partner) =
        opt_graphnode_by_gnid (graph, config, &partner_gnid) ?
      else { continue; };
      for member in partner . contains . iter ()
        . filter (|member| is_visible (member)) {
        inside . insert (key (&member . member)); }}

    // The replacement domain is intentionally built from disk, rather than
    // from newly inferred hides: preceding user actions in this save retain
    // their ordinary inference semantics.
    let replaceable_outside : HashSet<ID> =
      forker_from_disk . hides . or_default ()
      . iter ()
      . filter (|member| is_visible (member))
      // A hide of a node the restriction keeps out stood as a
      // restricted viewnode, whose line edits no hide.
      . filter (|member| ! graph . node_is_restricted (&member . member, skgrepo_restriction))
      .map (|member| key (&member . member))
      .filter (|member_key| ! inside . contains (member_key))
      .collect ();
    let submitted : HashSet<ID> =
      edit . members . iter () . map (|gnid| key (gnid)) . collect ();
    let disk_hide_keys : HashSet<ID> =
      forker_from_disk . hides . or_default ()
      . iter () . map (|member| key (&member . member)) . collect ();
    for member in &edit . members {
      if ! disk_hide_keys . contains (&key (member)) {
        candidates . push (
          PostSkgsaveCommitNoticeCandidate::HiddenOutsideAdded {
            forker : forker . clone (), member : member . clone () }); }}
    let current_hides : Vec<ID> =
      lowered . forker_hides_after_resolution (&forker_from_disk);
    let inferred_unhides : Vec<ID> = current_hides . iter ()
      . filter (|gnid| {
        let member_key : ID = key (gnid);
        replaceable_outside . contains (&member_key)
          && ! submitted . contains (&member_key) })
      . cloned () . collect ();
    let current_keys : HashSet<ID> =
      current_hides . iter () . map (|gnid| key (gnid)) . collect ();
    let inferred_hides : Vec<ID> = edit . members . iter ()
      . filter (|gnid| ! current_keys . contains (&key (gnid)))
      . cloned () . collect ();
    if ! inferred_hides . is_empty () || ! inferred_unhides . is_empty () {
      lowered . apply_hiderel_delta_to_forker (
        forker_from_disk, &inferred_hides, &inferred_unhides); }}
  Ok (candidates) }

/// Deleting (or moving away) a child of an owned note F, where
/// that child is disk-content of a fork partner of F (a node F forks,
/// or a forker of F that F does not hide: the members its forks
/// folder shows, 'fork_partners_shown'), hides the child from F's
/// forks -- otherwise the child would reappear under that partner as
/// unintegrated content the user just dismissed (docs/sharing-model.org:
/// branches deleted from a clone become hides). Symmetrically,
/// re-adding such a child to F's
/// contains drops a stale hide of it by F. A removed child counts as
/// the member it stands for ('member_it_stands_for'): removing a
/// mirror of a partner's member hides that member, unless the
/// same save leaves F containing the member. Placing a mirror
/// drops no hide, as containing it does not integrate the member
/// ('unintegrated_content_gnids' in 'server/refresh/goal.rs' compares
/// IDs): an unhidden member would come back as unintegrated. Details:
/// - Members whose records the skgrepo restriction keeps out were
///   omitted from the buffer, not removed, so they never count as
///   removals ('SkgrepoRestriction::record_is_visible'). A member
///   with a visible record had a headline (a restricted viewnode,
///   where the skgrepo restriction keeps out its node), so its absence
///   is a removal.
/// - A child the same save explicitly shows through a member as such
///   of F's forks folder (a forkee or forker as such) is not hidden
///   here: the explicit gesture wins. This runs before the signal
///   loops, so signal deltas also land last.
/// - A node with no disk entry removes nothing (a fork clone's
///   creation-time hides are computed in 'build_fork_clone').
/// - Only what the skgrepo restriction shows explains or blocks a
///   hide: the note's forks, its forkers, each partner's content
///   and the note's hides are read by their visible records. A
///   member only a restricted record explains is not hidden, and
///   re-adding a member drops no restricted hide of it.
fn infer_hides_from_contains_removals (
  lowered                : &mut LoweredNodeIntents,
  visibility             : &[(ID, ForkeeVisibility)],
  forker_visibility      : &[(ID, ForkerVisibility)],
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  for (forker_pid, skgrepo, new_contains, forks_msv)
    in lowered . nodeSaveIntents_with_specified_contains () {
    if ! config . skgrepo_is_owned (&skgrepo) { continue; }
    let Some (forker_from_disk) =
      opt_graphnode_by_gnid (
        graph, config, &forker_pid ) ?
    else { continue; };
    let forker_contains : Vec<ID> =
      members_of (&forker_from_disk . contains);
    let visible_contains : HashSet<&ID> =
      forker_from_disk . contains . iter ()
      . filter ( |member| SkgrepoRestriction::record_is_visible (
          skgrepo_restriction, &member . relRepo ))
      . map ( |member| &member . member )
      . collect ();
    let removed : Vec<ID> = {
      let new_contains_set : HashSet<&ID> =
        new_contains . iter () . collect ();
      let signal_visible : HashSet<&ID> = // children the same save explicitly shows through a member as such of this forker's forks folder
        visibility . iter ()
        . filter ( |(pid, _)| *pid == forker_pid )
        . flat_map ( |(_, signal)| signal . visible . iter () )
        . chain ( forker_visibility . iter ()
                  . filter ( |(pid, _)| *pid == forker_pid )
                  . flat_map ( |(_, signal)| signal . visible . iter () ))
        . collect ();
      forker_contains . iter ()
        . filter ( |gnid| ! new_contains_set . contains (gnid) )
        . filter ( |gnid| ! signal_visible . contains (gnid) )
        . filter ( |gnid| visible_contains . contains (gnid) )
        . cloned () . collect () };
    let inferred_unhides : Vec<ID> = {
      let disk_contains : HashSet<&ID> =
        forker_contains . iter () . collect ();
      let disk_hides : Vec<ID> = // a restricted hide stays, as the buffer could not show it
        members_with_visible_records (
          forker_from_disk . hides . or_default (),
          skgrepo_restriction );
      new_contains . iter ()
        . filter ( |gnid| ! disk_contains . contains (gnid) )
        . filter ( |gnid| disk_hides . contains (gnid) )
        . cloned () . collect () };
    let inferred_hides : Vec<ID> =
      if removed . is_empty () { Vec::new () }
      else {
        let partner_content : HashSet<ID> = {
          let forkees : Vec<ID> = match &forks_msv {
            // The post-save forks: unforking a node in
            // the same save also stops hides being inferred from it.
            MSV::Specified (forkees) => forkees . clone (),
            MSV::Unspecified =>
              members_with_visible_records (
                forker_from_disk . forks . or_default (),
                skgrepo_restriction ), };
          let mut content : HashSet<ID> = HashSet::new ();
          for partner in fork_partners_shown (
            graph, skgrepo_restriction, &forker_pid, &forkees )
          { if let Some (partner_from_disk) =
              opt_graphnode_by_gnid (
                graph, config, &partner ) ?
            { content . extend (
                members_with_visible_records (
                  & partner_from_disk . contains,
                  skgrepo_restriction )); }}
          content };
        let new_contains_set : HashSet<&ID> = new_contains . iter () . collect ();
        removed . iter ()
          . filter_map ( |gnid| member_it_stands_for (
            graph, skgrepo_restriction, gnid, &partner_content ))
          . filter ( |member| ! new_contains_set . contains (member) )
          . collect () };
    if inferred_hides . is_empty () && inferred_unhides . is_empty ()
      { continue; }
    lowered . apply_hiderel_delta_to_forker (
      forker_from_disk, &inferred_hides, &inferred_unhides ); }
  Ok (( )) }

/// The member of 'members' that 'gnid', a child removed from a
/// forker, stands for: 'gnid' itself when it is one; otherwise
/// the member that is a mirror of it under the skgrepo restriction
/// ('server/dbs/in_rust_graph/mirrors.rs'), the nearest along forks
/// first, and among equals the least. Every contains list, a
/// forkee's included, records the original; a mirror is shown in
/// its place only where a perspective ranks it first. So when the
/// forkee's author forks a member of its content (to answer under
/// a note of the forker's, say), the content still names the
/// original, which the forker's own view draws raw and the
/// author's view draws as the fork. Either way the forker met the
/// same member, so dismissing the fork dismisses it. Any mirror
/// counts, not one perspective's: which member a child stands for is
/// a fact about the child, whichever view the user found it in.
pub(crate) fn member_it_stands_for (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  gnid        : &ID,
  members     : &HashSet<ID>,
) -> Option<ID> {
  if members . contains (gnid) { return Some (gnid . clone ()); }
  let mirrors : Mirrors = mirrors_of (graph, restriction, gnid);
  members . iter ()
    . filter_map ( |member| mirrors . members . iter ()
      . find ( |mirror| mirror . pid == graph . pid_else_raw_id (member) )
      . map ( |mirror| ( mirror . distance, member ) ))
    . min ()
    . map ( |(_distance, member)| member . clone () ) }

/// Checks one forker + content node
/// against all forkee branches under the forker.
/// Rejects the save if one forkee says "hide it"
/// and another says "show it". A forkee's content is read by the
/// records the skgrepo restriction shows, as the view drew it. A
/// forker as such under the same note says only "show it", of each
/// child of its own it shows: a child absent there is no hide.
fn validate_no_overlapping_forkee_hiderel_conflicts (
  visibility          : &[(ID, ForkeeVisibility)],
  forker_visibility   : &[(ID, ForkerVisibility)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  // This needs the visibility signals plus disk contains lists,
  // because the conflict is per forker/forkee-content pair,
  // not just per viewchild shown in the buffer.
  let mut seen : HashMap<(ID, ID), bool> = HashMap::new();
  for (note, signal) in forker_visibility {
    let Some (forker_from_disk) : Option<Graphnode> =
      opt_graphnode_by_gnid (graph, config, &signal . forker) ?
      else { continue; };
    for content_gnid in members_with_visible_records (
      &forker_from_disk . contains, skgrepo_restriction ) {
      if signal . visible . contains (&content_gnid) {
        seen . insert ((note . clone (), content_gnid), true); }}}
  for (forker, signal) in visibility {
    let forkee_from_disk : Graphnode =
      match opt_graphnode_by_gnid (
        graph, config, &signal . forkee ) ?
      { Some (forkee_from_disk) => forkee_from_disk,
        None                        => continue, };
    let visible_content : HashSet<ID> =
      signal . visible . iter() . cloned() . collect();
    for content_gnid in members_with_visible_records (
      &forkee_from_disk . contains, skgrepo_restriction ) {
      let content_gnid : ID = content_gnid;
      let key : (ID, ID) =
        (forker . clone(), content_gnid . clone());
      let visible : bool = visible_content . contains (&content_gnid);
      match seen . get (&key) {
        Some (previous_visible) => {
          let previous_visible : &bool = previous_visible;
          if *previous_visible != visible {
            return Err (Box::new (BufferValidationError::Other (
              format!( "Conflicting forkee visibility edits for forker {} and content {}",
                        forker, content_gnid )) )); }},
        None => {}, }
      seen . insert (key, visible); }}
  Ok (( )) }
