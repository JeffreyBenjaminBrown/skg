//! This file defines visibility resolution, which consumes the
//! visibility signals ('MemberAsSuchVisibility') that collection put
//! in the map.
//! It is async because it consults disk.
//! .
//! The policy of the forkee-visibility step, for forker R
//! viewing forkee E, is:
//! .
//! - If N is graph-content of forkee E,
//!   and E has a viewchild with ID N and claimsMembership,
//!   then N is intended to be visible through this fork,
//!   so it is removed from the hides of forker R.
//! - If N is graph-content of E that the view drew under E (the
//!   folder showed E before the save, 'forks_folder_members_drawn'; R
//!   neither hid nor contained it, nor did the user keep it in an
//!   owned note but E, before the save, by the graph:
//!   'docs/glossary.org', "what a member shows"; nor does the buffer
//!   show it in E's folder of what the user keeps elsewhere or of what
//!   R hides in it, which a stale buffer may still do after another
//!   view changed the graph), E has no viewchild
//!   with ID N and claimsMembership, N is not graph-content of R after
//!   the save, and no owned note but E contains N after the save
//!   ('owned_notes_containing_after_save'), then N is hidden from R. A
//!   child moved into an owned note is kept there (integrated), and
//!   needs no hide.
//! - If N is a claimsMembership viewchild of E but is not
//!   graph-content of E, then it is not a hiderel edit: the user
//!   placed it there (typed it, or moved it from elsewhere), and it
//!   joins E's content, when the user owns E, and the save is refused
//!   otherwise ('add_children_placed_under_member_as_such').
//! - A foreign R records no hides: a save that would hide or unhide
//!   anything in R is refused ('refusal_to_change_what_a_foreign_note_hides').
//! - An E the graph lacks (typed there) has no list to lose: what it
//!   shows is its contains ('set_contains_of_new_member_as_such').
//! - An E the folder did not show before the save (typed there in
//!   this save) had nothing drawn under it, so nothing is hidden for
//!   it: R forks it, as its claim says, and a child typed under it
//!   joins its content, as above.
//! .
//! Each step compares children by pid ('InRustGraph::pid_else_raw_id'):
//! a list may name a child by an extra ID (after a nodeMerge), and the
//! buffer shows it by its pid. A hide is written, and an unhide found,
//! as the list it comes from names the child.
//! .
//! A forker shown as such in a forks folder gives a visibility signal
//! too, read by the forker-visibility step ('resolve_forker_visibility').
//! .
//! The skgrepo restriction limits what can be edited as well as what
//! can be seen ('server/skgrepo_sets.rs'), so both E's content and R's
//! hides are read by the records the
//! skgrepo restriction shows, as the view drew them: a hide whose
//! record is restricted is neither removed nor moved, and no hide is
//! inferred of a member that only a restricted record of E's
//! explains.
//! .
//! A forks folder adds one more signal ('ForksFolderMembers'), read by
//! 'resolve_forks_folder_removals'.
//! .
//! Resolution cannot run during collection, even in principle: it
//! needs disk (the forkee's contains, the forker's hides)
//! and the forker's own post-save contains -- a finished entry
//! elsewhere in the same map.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::{Mirrors, mirrors_of};
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::refresh::goal::{
  forks_folder_members_shown, forker_only_members, owned_notes_containing};
use crate::from_text::local_fieldintent_collection::lower::LoweredNodeIntents;
use crate::from_text::local_fieldintent_collection::types::{
  ForksFolderMembers, HiddenOutsideEdit, MemberAsSuch, MemberAsSuchVisibility };
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::PostSkgsaveCommitNoticeCandidate;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::slice::from_ref;

/// The 'member_as_such_visibility' pairs are (the note whose forks
/// folder shows the member, signal), as 'lower_collected_fieldIntents'
/// extracted them from the map. 'is_diff_view' says whether the saved buffer is one
/// of the diff view's, whose forks folders hold a note's direct fork
/// partners only ('resolve_forks_folder_removals').
pub fn resolve_visibility (
  mut lowered : LoweredNodeIntents,
  member_as_such_visibility : &[(ID, MemberAsSuchVisibility)],
  hidden_outside : &[(ID, HiddenOutsideEdit)],
  forks_folder_members : &[(ID, ForksFolderMembers)],
  graph       : &InRustGraph,
  config      : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  is_diff_view : bool,
) -> Result<(LoweredNodeIntents, Vec<PostSkgsaveCommitNoticeCandidate>), Box<dyn Error>> {
  validate_no_overlapping_forkee_hiderel_conflicts (
    member_as_such_visibility, graph, config, skgrepo_restriction,
    is_diff_view ) ?;
  infer_hides_from_contains_removals (
    // Before the signal loops below, so that an explicit
    // member-as-such gesture about the same child wins.
    &mut lowered, member_as_such_visibility, graph, config,
    skgrepo_restriction ) ?;
  add_children_placed_under_members_as_such (
    // Before the signal loops, so that a child moved from one member
    // as such to an owned one in the same save counts as contained
    // there after the save, whichever signal comes first.
    &mut lowered, member_as_such_visibility, graph, config,
    skgrepo_restriction ) ?;
  for (forker, signal) in signals_of_role (
    member_as_such_visibility, MemberAsSuch::Forkee ) {
    let Some (forkee_from_disk) =
      graphnode_from_graph (graph, &signal . member)
    else { lowered . set_contains_of_new_member_as_such (
             &signal . member, &signal . visible );
           continue; };
    let Some (forker_from_disk) =
      graphnode_from_graph (graph, forker)
    else { continue; };
    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let forker_contains : HashSet<ID> =
      lowered . note_contains_after_save (
        &forker_from_disk ) . iter () . map (key) . collect ();
    let visible_content : HashSet<ID> =
      signal . visible . iter () . map (key) . collect ();
    let forkee_contains : Vec<ID> = // what the buffer could show of the forkee's content
      members_with_visible_records (
        &forkee_from_disk . contains, skgrepo_restriction );
    let forker_hides : Vec<ID> = // a restricted hide the buffer could not show, so no save removes it
      members_with_visible_records (
        forker_from_disk . hides . or_default (),
        skgrepo_restriction );
    let shown_before : bool = // false for a member typed into the folder in this save
      forks_folder_members_drawn (
        graph, skgrepo_restriction, &forker_from_disk, is_diff_view )
      . contains (&key (&signal . member));
    let drawn_before : Vec<ID> = // what the view drew under the forkee
      if ! shown_before { Vec::new () } else {
        let hidden_contained_or_set_aside : HashSet<ID> =
          forker_hides . iter ()
          . chain ( members_with_visible_records (
                      &forker_from_disk . contains, skgrepo_restriction ) . iter () )
          . chain ( signal . in_integratedElsewhere_or_hiddenIn_folder . iter () )
          . map (key) . collect ();
        forkee_contains . iter ()
          . filter ( |gnid| ! hidden_contained_or_set_aside . contains (&key (gnid))
                            && ! kept_elsewhere_before_save (
                                   graph, config, skgrepo_restriction, gnid,
                                   &forkee_from_disk . pid ) )
          . cloned () . collect () };
    let inferred_hides : Vec<ID> =
      drawn_before . iter()
      . filter ( |gnid| ! visible_content . contains (&key (gnid)) )
      . filter ( |gnid| ! forker_contains . contains (&key (gnid)) )
      . filter ( |gnid| owned_notes_containing_after_save (
                          &lowered, gnid, &forkee_from_disk . pid,
                          graph, config, skgrepo_restriction ) . is_empty () )
      . cloned()
      . collect();
    let inferred_unhides : Vec<ID> = { // as the forker's hides name them
      let forkee_contains_keys : HashSet<ID> =
        forkee_contains . iter () . map (key) . collect ();
      forker_hides . iter ()
        . filter ( |gnid| visible_content . contains (&key (gnid))
                          && forkee_contains_keys . contains (&key (gnid)) )
        . cloned () . collect () };
    if inferred_hides . is_empty() && inferred_unhides . is_empty()
      { continue; }
    if ! config . skgrepo_is_owned (&forker_from_disk . home_skgrepo) {
      return Err ( Box::new ( BufferValidationError::Other (
        refusal_to_change_what_a_foreign_note_hides (
          &forker_from_disk, &forkee_from_disk, MemberAsSuch::Forkee ) ))); }
    lowered . apply_hiderel_delta_to_hider (
      forker_from_disk,
      &inferred_hides,
      &inferred_unhides ); }
  resolve_forker_visibility (
    &mut lowered,
    signals_of_role ( member_as_such_visibility, MemberAsSuch::Forker ),
    graph, config, skgrepo_restriction, is_diff_view ) ?;
  resolve_forks_folder_removals (
    &mut lowered, forks_folder_members, graph, config,
    skgrepo_restriction, is_diff_view ) ?;
  let post_skgsave_commit_notice_candidates = apply_hiddenoutside_edits (
    &mut lowered, hidden_outside, graph, config,
    skgrepo_restriction ) ?;
  Ok ((lowered, post_skgsave_commit_notice_candidates)) }

/// The refusal of a save that cuts from, or shows again, the content
/// of 'member', a member as such of the forks folder of 'note', which
/// the user does not own: a cut there is for the note to hide what it
/// cut (and, from a forker as such, to fork that forker), showing a
/// child again is for it to stop hiding it, and only a note of one's
/// own can record either.
fn refusal_to_change_what_a_foreign_note_hides (
  note           : &Graphnode,
  member         : &Graphnode,
  member_as_such : MemberAsSuch,
) -> String {
  let what_only_a_note_of_your_own_can_do : &str = match member_as_such {
    MemberAsSuch::Forkee => "change what it hides",
    MemberAsSuch::Forker => "fork another and change what it hides of that one", };
  format! (
    "Cannot change what {} ('{}') shows of {} ('{}'), {}: {} is foreign, and only a note of your own can {}.",
    note . pid, note . title, member . pid, member . title,
    member_as_such . as_named_beside_the_note (), note . pid, what_only_a_note_of_your_own_can_do ) }

/// Each child placed under a member as such, of either role, joins
/// its content ('add_children_placed_under_member_as_such'). A signal
/// whose member, or whose note, the graph lacks places nothing here:
/// a member the graph lacks has no list ('set_contains_of_new_member_as_such').
fn add_children_placed_under_members_as_such (
  lowered             : &mut LoweredNodeIntents,
  member_as_such_visibility : &[(ID, MemberAsSuchVisibility)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let signals : Vec<(&ID, &ID, MemberAsSuch, &[ID])> =
    // the forkees' signals, then the forkers'
    signals_of_role ( member_as_such_visibility, MemberAsSuch::Forkee )
    . chain ( signals_of_role ( member_as_such_visibility, MemberAsSuch::Forker ))
    . map ( |(note, signal)|
            ( note, &signal . member, signal . role, signal . visible . as_slice () ))
    . collect ();
  for (note, member, member_as_such, visible) in signals {
    let Some (member_from_disk) : Option<Graphnode> =
      graphnode_from_graph (graph, member)
      else { continue; };
    if graphnode_from_graph (graph, note) . is_none () { continue; }
    add_children_placed_under_member_as_such (
      lowered, &member_from_disk, member_as_such, visible,
      graph, config, skgrepo_restriction ) ?; }
  Ok (( )) }

/// The notes of the user's graph, but 'excepted', that contain
/// 'gnid' after this save ('owned_notes_containing' in
/// 'server/refresh/goal.rs'): a note the save writes by its contains
/// as the save leaves them ('LoweredNodeIntents::contains_after_save'),
/// any other as the graph has it.
fn owned_notes_containing_after_save (
  lowered             : &LoweredNodeIntents,
  gnid                : &ID,
  excepted            : &ID,
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Vec<ID> {
  owned_notes_containing (
    graph, config, skgrepo_restriction, gnid, from_ref (excepted),
    Some ( &lowered . contains_after_save (graph) )) }

/// Whether, before the save, by the graph, the user kept 'gnid' in an
/// owned note but 'member' ('owned_notes_containing'): a member as
/// such then drew it in its folder of what it contains that the user
/// keeps elsewhere, not among its content.
fn kept_elsewhere_before_save (
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  gnid                : &ID,
  member              : &ID,
) -> bool {
  ! owned_notes_containing (
      graph, config, skgrepo_restriction, gnid, from_ref (member), None )
    . is_empty () }

/// Each child placed under a member M as such (a claiming child the
/// buffer shows there that M's list does not name) joins M's content
/// when the user owns M, and the save is refused otherwise
/// ('docs/forks.org', "Adding a child under a member as such", which
/// says too why a stale buffer puts back nothing here). It goes right
/// after the sibling it follows in the buffer that M's list names, or
/// another child placed there, first when none precedes it. M's
/// contains is read by the records the skgrepo restriction shows, as
/// the view drew them; disk supplementation keeps the rest where they
/// stand.
fn add_children_placed_under_member_as_such (
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
  let placed : HashSet<&ID> =
    shown . iter ()
    . filter ( |gnid| ! named . contains (&key (gnid)) )
    . collect ();
  if placed . is_empty () { return Ok (( )); }
  if ! config . skgrepo_is_owned (&member_from_disk . home_skgrepo) {
    return Err ( Box::new ( BufferValidationError::Other ( format! (
      "Cannot add a child under {} ('{}'), a foreign node shown as a {}: only its owner can change its content. To add one, make the node your own first: view it as itself, where an edit starts the fork operation.",
      member_from_disk . pid, member_from_disk . title, member_as_such . word () )))); }
  let mut follows : Option<usize> = None; // where the last sibling the list names, or the last child placed there, stands
  for gnid in shown {
    if placed . contains (gnid) {
      let at : usize = follows . map_or (0, |position| position + 1);
      contains . insert (at, gnid . clone ());
      follows = Some (at);
    } else if let Some (position) =
      contains . iter () . position ( |member| key (member) == key (gnid) )
    { follows = Some (position); }}
  lowered . set_contains_of_member_as_such (member_from_disk . clone (), contains);
  Ok (( )) }

/// The forker-visibility step ('docs/forks.org', "Cutting from a
/// forker"), for a forker F shown as such in the forks folder of a
/// note N (it forks N, or it is an indirect member, or N forked it and
/// the user detached it there). Where the folder showed F before the
/// save ('forks_folder_members_drawn'), the view drew F's content as
/// 'what_the_member_shows' ('server/refresh/goal.rs') gives it, by
/// the records the skgrepo restriction shows (the graph before the
/// save). Where it did not, F was typed into the folder in this save,
/// and the view drew nothing under it, so nothing is cut from it: N
/// neither forks F nor hides anything for it. So:
/// - a child of F that N hides, shown claiming under F (moved out of
///   F's folder of what N hides in it), is unhidden in N, as under a
///   forkee;
/// - a child F showed that the buffer no longer shows claiming under
///   F, nor in F's folder of what the user keeps elsewhere or of what
///   N hides in it (where a stale buffer may show a child the graph
///   has since stopped keeping or hiding), was cut from it. A cut
///   makes N fork F, but for a forker N already forks (a mutual the
///   user detached there, in the same save: the detachment stands),
///   and N hides each child cut that neither N nor an owned note but
///   F contains after the save ('owned_notes_containing_after_save').
/// A foreign N can record neither a fork nor a hide, so a save that
/// cuts from F or unhides under it is refused
/// ('refusal_to_change_what_a_foreign_note_hides'). A child F does not
/// show (one N contains or hides) is absent from under F by design,
/// and is never removed from F's list. A forker the graph lacks
/// (typed there) has no list to lose: what it shows is its contains
/// ('set_contains_of_new_member_as_such').
fn resolve_forker_visibility<'a> (
  lowered             : &mut LoweredNodeIntents,
  signals             : impl Iterator<Item = &'a (ID, MemberAsSuchVisibility)>,
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  is_diff_view        : bool,
) -> Result<(), Box<dyn Error>> {
  for (note, signal) in signals {
    let Some (forker_from_disk) : Option<Graphnode> =
      graphnode_from_graph (graph, &signal . member)
      else { lowered . set_contains_of_new_member_as_such (
               &signal . member, &signal . visible );
             continue; };
    let Some (note_from_disk) : Option<Graphnode> =
      graphnode_from_graph (graph, note)
      else { continue; };
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
    let shown_before : bool = // false for a member typed into the folder in this save
      forks_folder_members_drawn (
        graph, skgrepo_restriction, &note_from_disk, is_diff_view )
      . contains (&forker_from_disk . pid);
    let drawn_before : Vec<ID> = // what the view drew under the forker
      if ! shown_before { Vec::new () } else {
        let hidden_or_contained_by_the_note : HashSet<ID> =
          note_hides . iter ()
          . chain ( members_with_visible_records (
                      &note_from_disk . contains, skgrepo_restriction ) . iter () )
          . map (key) . collect ();
        forker_contains . iter ()
          . filter ( |gnid| ! hidden_or_contained_by_the_note . contains (&key (gnid))
                            && ! kept_elsewhere_before_save (
                                   graph, config, skgrepo_restriction, gnid,
                                   &forker_from_disk . pid ) )
          . cloned () . collect () };
    let cut : Vec<ID> = {
      let set_aside : HashSet<ID> = // shown in F's folder of what the user keeps elsewhere, or of what N hides in it
        signal . in_integratedElsewhere_or_hiddenIn_folder . iter () . map (key) . collect ();
      drawn_before . into_iter ()
        . filter ( |gnid| ! visible . contains (&key (gnid))
                          && ! set_aside . contains (&key (gnid)) )
        . collect () };
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
        refusal_to_change_what_a_foreign_note_hides (
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
          lowered . note_contains_after_save (&note_from_disk) . iter ()
          . map (key) . collect ();
        cut . into_iter ()
          . filter ( |gnid| ! note_contains . contains (&key (gnid))
                            && owned_notes_containing_after_save (
                                 lowered, gnid, &forker_from_disk . pid,
                                 graph, config, skgrepo_restriction ) . is_empty () )
          . collect () };
    if inferred_hides . is_empty () && inferred_unhides . is_empty () { continue; }
    lowered . apply_hiderel_delta_to_hider (
      note_from_disk, &inferred_hides, &inferred_unhides ); }
  Ok (( )) }

/// A note N's forks folder shows each member N does not fork claiming
/// nothing (a forker-only or an indirect member) and each mutual
/// claiming; N hides none of them. Deleting one from the folder hides
/// it in N, so that it leaves the folder (where a member N hides is
/// not drawn) for the folder of what N hides outside its fork
/// partners, whence removing it unhides it. So each member of the
/// folder as the view drew it ('forks_folder_members_drawn') that
/// would stand in it claiming nothing
/// after the save (with N's forkees as the save leaves them: a deleted
/// mutual's fork the same save ends), that the folder no longer
/// shows, and that N does not hide already, joins N's hides. A forkee
/// the deletion unforks is no member after the save, unless another
/// mirror still joins it to N; nor is what only it joined to N. In
/// the diff view an indirect member was never drawn, so none is
/// hidden. A member whose record the restriction keeps out was never drawn, and
/// one whose node it keeps out stood as a restricted viewnode, whose
/// line edits no relationship, so neither is hidden here. Only an
/// owned N is written. This runs before the edits of the
/// hidden-outside folder, which may unhide it again.
fn resolve_forks_folder_removals (
  lowered             : &mut LoweredNodeIntents,
  signals             : &[(ID, ForksFolderMembers)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  is_diff_view        : bool,
) -> Result<(), Box<dyn Error>> {
  for (note, signal) in signals {
    let Some (note_from_disk) : Option<Graphnode> =
      graphnode_from_graph (graph, note)
      else { continue; };
    if ! config . skgrepo_is_owned (&note_from_disk . home_skgrepo) {
      continue; }
    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let shown : HashSet<ID> = signal . members . iter () . map (key) . collect ();
    let forked_after_save : HashSet<ID> =
      lowered . note_forks_after_save (&note_from_disk) . iter ()
      . map (key) . collect ();
    let hidden : HashSet<ID> =
      lowered . note_hides_after_resolution (&note_from_disk) . iter ()
      . map (key) . collect ();
    let drawn : HashSet<ID> =
      forks_folder_members_drawn (
        graph, skgrepo_restriction, &note_from_disk, is_diff_view );
    let deleted : Vec<ID> =
      forks_folder_members_shown (
        graph, skgrepo_restriction, &note_from_disk . pid,
        &lowered . note_forks_after_save (&note_from_disk) )
      . into_iter ()
      . filter ( |member| drawn . contains (member)
                          && ! shown . contains (member)
                          && ! forked_after_save . contains (member)
                          && ! hidden . contains (member)
                          && ! graph . node_is_restricted (member, skgrepo_restriction) )
      . collect ();
    if ! deleted . is_empty () {
      lowered . apply_hiderel_delta_to_hider (
        note_from_disk, &deleted, &[] ); }}
  Ok (( )) }

/// The members of the forks folder of 'note' as the view drew them,
/// each by its pid: by the graph before the save, with the note's
/// forkees by the records the skgrepo restriction shows
/// ('forks_folder_members_shown'). A forks folder of the diff view
/// holds the note's direct fork partners only
/// ('diff_view::forks_folder_goal'). The graph, not the buffer's
/// baseline, which a save need not carry: a member absent from it was
/// typed into the folder in this save.
fn forks_folder_members_drawn (
  graph               : &InRustGraph,
  skgrepo_restriction : &SkgrepoRestriction,
  note_from_disk      : &Graphnode,
  is_diff_view        : bool,
) -> HashSet<ID> {
  let forkees : Vec<ID> = members_with_visible_records (
    note_from_disk . forks . or_default (), skgrepo_restriction );
  if is_diff_view {
    forkees . iter () . map ( |forkee| graph . pid_else_raw_id (forkee) )
      . chain ( forker_only_members (
        graph, skgrepo_restriction, &note_from_disk . pid, &forkees ))
      . collect ()
  } else {
    forks_folder_members_shown (
      graph, skgrepo_restriction, &note_from_disk . pid, &forkees )
      . into_iter () . collect () }}

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
/// whose records are restricted, and members inside a member of the forks folder, remain owned
/// by the graph and survive an edit of this compound partner folder. A hide is
/// visible, and so replaceable, when the skgrepo restriction holds its relRepo
/// ('SkgrepoRestriction::record_is_visible'): the folder as drawn showed its
/// member, as an occurrence or a gnid-only vognode, so the submitted
/// folder is the whole set of hides with a visible record outside the
/// forks folder's members, and a folder saved under a skgrepo restriction never unhides a
/// member it could not show. Nor does it unhide a member it showed as a
/// restricted viewnode (its node kept out): deleting that line changes
/// no relationship to the note it stands for. What counts as inside is what the folder's goal
/// subtracts ('hidden_outside_of_fork_partners'): the content
/// of each member the forks folder shows, forkee, forker or indirect
/// ('forks_folder_members_shown'), by the records the skgrepo restriction
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
  for (hider, edit) in edits {
    if ! seen . insert (hider . clone ()) {
      return Err (Box::new (BufferValidationError::Other (
        format! ("More than one HiddenOutsideOfForkPartners edit was submitted for hider {}", hider) ))); }
    let Some (hider_from_disk) =
      graphnode_from_graph (graph, hider)
    else { continue; };
    if ! config . skgrepo_is_owned (&hider_from_disk . home_skgrepo) {
      continue; }

    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let is_visible : &dyn Fn (&RelPartner<ID>) -> bool =
      &|member : &RelPartner<ID>| -> bool {
      SkgrepoRestriction::record_is_visible (
        skgrepo_restriction, &member . relRepo ) };
    let partner_gnids : Vec<ID> = forks_folder_members_shown (
      graph, skgrepo_restriction, &hider_from_disk . pid,
      &lowered . note_forks_after_save (&hider_from_disk) );
    let mut inside : HashSet<ID> = HashSet::new ();
    for partner_gnid in partner_gnids {
      let Some (partner) =
        graphnode_from_graph (graph, &partner_gnid)
      else { continue; };
      for member in partner . contains . iter ()
        . filter (|member| is_visible (member)) {
        inside . insert (key (&member . member)); }}

    // The replacement domain is intentionally built from disk, rather than
    // from newly inferred hides: preceding user actions in this save retain
    // their ordinary inference semantics.
    let replaceable_outside : HashSet<ID> =
      hider_from_disk . hides . or_default ()
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
      hider_from_disk . hides . or_default ()
      . iter () . map (|member| key (&member . member)) . collect ();
    for member in &edit . members {
      if ! disk_hide_keys . contains (&key (member)) {
        candidates . push (
          PostSkgsaveCommitNoticeCandidate::HiddenOutsideAdded {
            forker : hider . clone (), member : member . clone () }); }}
    let current_hides : Vec<ID> =
      lowered . note_hides_after_resolution (&hider_from_disk);
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
      lowered . apply_hiderel_delta_to_hider (
        hider_from_disk, &inferred_hides, &inferred_unhides); }}
  Ok (candidates) }

/// Deleting (or moving away) a child of an owned note F, where
/// that child is disk-content of a member of F's forks folder (a node
/// F forks, a forker of F that F does not hide, or an indirect member:
/// 'forks_folder_members_shown'), hides the child from F's forks
/// folder -- otherwise the child would reappear under that member as
/// unintegrated content the user just dismissed (docs/sharing-model.org:
/// branches deleted from a clone become hides). Symmetrically,
/// re-adding such a child to F's
/// contains drops a stale hide of it by F. A removed child counts as
/// the member it stands for ('member_it_stands_for'): removing a
/// mirror of a partner's member hides that member, unless the
/// same save leaves F containing the member. Placing a mirror
/// drops no hide, as containing it does not integrate the member
/// ('what_the_member_shows' in 'server/refresh/goal.rs' compares
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
/// - A member the user keeps elsewhere after the save is not hidden,
///   since no partner shows it ('what_the_member_shows'): only where
///   some partner containing it would show it
///   ('owned_notes_containing_after_save') does its removal hide it.
///   Moving a child from F into another owned note records no hide.
fn infer_hides_from_contains_removals (
  lowered                : &mut LoweredNodeIntents,
  member_as_such_visibility : &[(ID, MemberAsSuchVisibility)],
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  for (hider_pid, skgrepo, new_contains, forks_msv)
    in lowered . nodeSaveIntents_with_specified_contains () {
    if ! config . skgrepo_is_owned (&skgrepo) { continue; }
    let Some (hider_from_disk) =
      graphnode_from_graph (graph, &hider_pid)
    else { continue; };
    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let hider_contains : Vec<ID> =
      members_of (&hider_from_disk . contains);
    let visible_contains : HashSet<&ID> =
      hider_from_disk . contains . iter ()
      . filter ( |member| SkgrepoRestriction::record_is_visible (
          skgrepo_restriction, &member . relRepo ))
      . map ( |member| &member . member )
      . collect ();
    let new_contains_keys : HashSet<ID> =
      new_contains . iter () . map (key) . collect ();
    let removed : Vec<ID> = {
      let signal_visible : HashSet<ID> = // children the same save explicitly shows through a member as such of this forker's forks folder
        member_as_such_visibility . iter ()
        . filter ( |(pid, _)| *pid == hider_pid )
        . flat_map ( |(_, signal)| signal . visible . iter () . map (key) )
        . collect ();
      hider_contains . iter ()
        . filter ( |gnid| ! new_contains_keys . contains (&key (gnid)) )
        . filter ( |gnid| ! signal_visible . contains (&key (gnid)) )
        . filter ( |gnid| visible_contains . contains (gnid) )
        . cloned () . collect () };
    let inferred_unhides : Vec<ID> = { // as the hider's hides name them
      let disk_contains_keys : HashSet<ID> =
        hider_contains . iter () . map (key) . collect ();
      let disk_hides : Vec<ID> = // a restricted hide stays, as the buffer could not show it
        members_with_visible_records (
          hider_from_disk . hides . or_default (),
          skgrepo_restriction );
      disk_hides . into_iter ()
        . filter ( |gnid| new_contains_keys . contains (&key (gnid))
                          && ! disk_contains_keys . contains (&key (gnid)) )
        . collect () };
    let inferred_hides : Vec<ID> =
      if removed . is_empty () { Vec::new () }
      else {
        let partner_contents : Vec<(ID, Vec<ID>)> = { // each partner's content, as written
          let forkees : Vec<ID> = match &forks_msv {
            // The post-save forks: unforking a node in
            // the same save also stops hides being inferred from it.
            MSV::Specified (forkees) => forkees . clone (),
            MSV::Unspecified =>
              members_with_visible_records (
                hider_from_disk . forks . or_default (),
                skgrepo_restriction ), };
          let mut contents : Vec<(ID, Vec<ID>)> = Vec::new ();
          for partner in forks_folder_members_shown (
            graph, skgrepo_restriction, &hider_pid, &forkees )
          { if let Some (partner_from_disk) =
              graphnode_from_graph (graph, &partner)
            { contents . push ((
                partner_from_disk . pid . clone (),
                members_with_visible_records (
                  & partner_from_disk . contains,
                  skgrepo_restriction ))); }}
          contents };
        let partner_content : HashSet<ID> =
          partner_contents . iter ()
          . flat_map ( |(_, content)| content . iter () . cloned () )
          . collect ();
        let would_show_under_a_partner : &dyn Fn (&ID) -> bool =
          // Some partner containing it would show it after the save:
          // no owned note but that partner keeps it then.
          &|member : &ID| -> bool {
            partner_contents . iter ()
            . filter ( |(_, content)| content . iter ()
                                      . any ( |gnid| key (gnid) == key (member) ))
            . any ( |(partner, _)| owned_notes_containing_after_save (
                      lowered, member, partner,
                      graph, config, skgrepo_restriction ) . is_empty () ) };
        removed . iter ()
          . filter_map ( |gnid| member_it_stands_for (
            graph, skgrepo_restriction, gnid, &partner_content ))
          . filter ( |member| ! new_contains_keys . contains (&key (member)) )
          . filter ( |member| would_show_under_a_partner (member) )
          . collect () };
    if inferred_hides . is_empty () && inferred_unhides . is_empty ()
      { continue; }
    lowered . apply_hiderel_delta_to_hider (
      hider_from_disk, &inferred_hides, &inferred_unhides ); }
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
  let mirrors : Mirrors = mirrors_of (graph, restriction, gnid, None);
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
/// records the skgrepo restriction shows, as the view drew it: a
/// child the user kept in an owned note but the forkee before the
/// save stood in its folder of what the user keeps elsewhere, and
/// says nothing ('kept_elsewhere_before_save'), as does a child the
/// buffer shows in that folder or in the folder of what the forker
/// hides in it. A
/// forker as such under the same note says only "show it", of each
/// child of its own it shows: a child absent there is no hide. So
/// does a forkee typed into the folder in this save, under which the
/// view drew nothing ('forks_folder_members_drawn').
/// Children are compared by pid, whichever ID a list names them by.
fn validate_no_overlapping_forkee_hiderel_conflicts (
  member_as_such_visibility : &[(ID, MemberAsSuchVisibility)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  is_diff_view        : bool,
) -> Result<(), Box<dyn Error>> {
  // This needs the visibility signals plus disk contains lists,
  // because the conflict is per forker/forkee-content pair,
  // not just per viewchild shown in the buffer.
  let pid_key : &dyn Fn (&ID) -> ID =
    &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
  let mut seen : HashMap<(ID, ID), bool> = HashMap::new();
  for (note, signal) in signals_of_role (
    member_as_such_visibility, MemberAsSuch::Forker ) {
    let Some (forker_from_disk) : Option<Graphnode> =
      graphnode_from_graph (graph, &signal . member)
      else { continue; };
    let visible_content : HashSet<ID> =
      signal . visible . iter () . map (pid_key) . collect ();
    for content_gnid in members_with_visible_records (
      &forker_from_disk . contains, skgrepo_restriction ) {
      if visible_content . contains (&pid_key (&content_gnid)) {
        seen . insert ((note . clone (), pid_key (&content_gnid)), true); }}}
  for (forker, signal) in signals_of_role (
    member_as_such_visibility, MemberAsSuch::Forkee ) {
    let forkee_from_disk : Graphnode =
      match graphnode_from_graph (graph, &signal . member)
      { Some (forkee_from_disk) => forkee_from_disk,
        None                        => continue, };
    let visible_content : HashSet<ID> =
      signal . visible . iter () . map (pid_key) . collect ();
    let set_aside : HashSet<ID> =
      signal . in_integratedElsewhere_or_hiddenIn_folder . iter ()
      . map (pid_key) . collect ();
    let shown_before : bool =
      match graphnode_from_graph (graph, forker) {
        Some (forker_from_disk) =>
          forks_folder_members_drawn (
            graph, skgrepo_restriction, &forker_from_disk, is_diff_view )
          . contains (&forkee_from_disk . pid),
        None => false, };
    for content_gnid in members_with_visible_records (
      &forkee_from_disk . contains, skgrepo_restriction ) {
      let content_gnid : ID = pid_key (&content_gnid);
      if kept_elsewhere_before_save (
           graph, config, skgrepo_restriction, &content_gnid, &forkee_from_disk . pid )
         || set_aside . contains (&content_gnid)
         || ( ! shown_before && ! visible_content . contains (&content_gnid) )
      { continue; }
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

/// The visibility signals whose member plays 'role', in their order.
fn signals_of_role (
  signals : &[(ID, MemberAsSuchVisibility)],
  role    : MemberAsSuch,
) -> impl Iterator<Item = &(ID, MemberAsSuchVisibility)> {
  signals . iter () . filter ( move |(_, signal)| signal . role == role ) }
