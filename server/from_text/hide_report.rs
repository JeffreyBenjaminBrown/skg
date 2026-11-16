//! What a save hides and stops hiding, in words a user can read
//! ('docs/api-and-formats.org', "Save buffer": the '(hides ...)'
//! entry). A hide is never typed: the server infers it from what left
//! a forker's content ('resolve_visibility.rs'), or from an edit
//! of a hidden folder. So a gesture that hides nothing looks, in the
//! buffer, just like one that works; these lines tell them apart.
//! .
//! The lines compare the plan with the graph, not the inference's own
//! steps: for each forker the plan writes (a fork's clone
//! included), its hides as the plan leaves them against its hides on
//! disk. That catches every source of a hide, and reports what the
//! save will in fact write. Then each child that left an owned
//! note with a member its forks folder shows (a note it forks, or a
//! forker or an indirect member it does not hide), and whose departure hid
//! nothing, earns a "Hides nothing:" line naming the rule that held:
//! among them, that an owned note keeps it after the save
//! ('owned_notes_containing'), so that no member of the forks folder
//! shows it.
//! .
//! Only what the skgrepo restriction shows counts, as in the
//! inference: a hide or a member whose record is kept out of sight is
//! neither reported nor counted as removed. A node the same save
//! deletes is left out: deleting it strips its hides anyway.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::from_text::local_fieldintent_collection::resolve_visibility::member_it_stands_for;
use crate::refresh::goal::{ContainsAfterSave, forks_folder_members_shown, owned_notes_containing};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::{ForkSpec, NodeInstruction, SaveNode};

use std::collections::{HashMap, HashSet};

/// One line per hide and per unhide the plan writes, then one per
/// removal from a forker that hides nothing, saying why.
pub fn hide_report_lines (
  graph             : &InRustGraph,
  config            : &SkgConfig,
  restriction       : &SkgrepoRestriction,
  node_instructions : &[NodeInstruction],
  fork_specs        : &[ForkSpec],
) -> Vec<String> {
  let saved : Vec<&Graphnode> =
    node_instructions . iter ()
    . filter_map ( |instruction| match instruction {
        NodeInstruction::Save (SaveNode (node)) => Some (node),
        NodeInstruction::Delete (_) => None, } )
    . chain ( fork_specs . iter () . map ( |spec| & spec . clone . 0 ))
    . collect ();
  let deleted : HashSet<ID> =
    node_instructions . iter ()
    . filter_map ( |instruction| match instruction {
        NodeInstruction::Delete (delete) =>
          Some ( graph . pid_else_raw_id (& delete . gnid) ),
        NodeInstruction::Save (_) => None, } )
    . collect ();
  let titles : HashMap<ID, String> = // the plan's titles, which a new clone has only here
    saved . iter ()
    . map ( |node| ( node . pid . clone (), node . title . clone () ))
    . collect ();
  let label = |gnid : &ID| -> String {
    let pid : ID = graph . pid_else_raw_id (gnid);
    let title : String = titles . get (&pid) . cloned ()
      . or_else ( || graphnode_from_graph (graph, &pid)
                     . map ( |node| node . title ))
      . unwrap_or_else ( || gnid . to_string () );
    format! ("'{}'", title) };
  let clones : HashSet<ID> =
    fork_specs . iter () . map ( |spec| spec . clone . 0 . pid . clone () ) . collect ();
  let forker_label = |pid : &ID| -> String { // a clone keeps its original's title, so it is named as the fork
    if clones . contains (pid) { format! ("the fork {}", label (pid)) }
    else { label (pid) } };
  let key = |gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
  let visible_members = |list : &[RelPartner<ID>]| -> Vec<ID> {
    list . iter ()
      . filter ( |member| SkgrepoRestriction::record_is_visible (
        restriction, &member . relRepo ))
      . map ( |member| key (&member . member) )
      . collect () };
  let contains_after_save : HashMap<ID, Vec<ID>> = // each saved note's content as the plan leaves it
    saved . iter ()
    . map ( |node| ( node . pid . clone (), visible_members (&node . contains) ))
    . collect ();
  let after_save : ContainsAfterSave = ContainsAfterSave {
    saved : saved . iter ()
      . map ( |node| ( node . pid . clone (), node . home_skgrepo . clone (),
                       visible_members (&node . contains) . into_iter () . collect () ))
      . collect (),
    deleted : deleted . clone () };
  let mut changes : Vec<String> = Vec::new ();
  let mut nothings : Vec<String> = Vec::new ();
  for node in &saved {
    if ! config . skgrepo_is_owned (&node . home_skgrepo) { continue; }
    let on_disk : Option<Graphnode> = graphnode_from_graph (graph, &node . pid);
    let disk_hides : Vec<ID> = on_disk . as_ref ()
      . map ( |disk| visible_members (disk . hides . or_default ()) )
      . unwrap_or_default ();
    let hides_after : Vec<ID> = match &node . hides {
      MSV::Specified (hides) => visible_members (hides),
      MSV::Unspecified => disk_hides . clone (), };
    let forkees : Vec<ID> = match &node . forks {
      MSV::Specified (forkees) => visible_members (forkees),
      MSV::Unspecified => on_disk . as_ref ()
        . map ( |disk| visible_members (disk . forks . or_default ()) )
        . unwrap_or_default (), };
    let partner_contents : Vec<(ID, Vec<ID>)> =
      forks_folder_members_shown (graph, restriction, &node . pid, &forkees) . iter ()
      . map ( |partner| (
        partner . clone (),
        graphnode_from_graph (graph, partner)
          . map ( |partner| visible_members (&partner . contains) )
          . unwrap_or_default () ))
      . collect ();
    let containers_of = |member : &ID| -> String { // the forks folder's members whose content holds 'member', named
      partner_contents . iter ()
        . filter ( |(_, content)| content . contains (member) )
        . map ( |(partner, _)| label (partner) )
        . collect::<Vec<String>> () . join (" and ") };
    let as_content_of = |member : &ID| -> String {
      let containers : String = containers_of (member);
      if containers . is_empty () { String::new () }
      else { format! (", as content of {}", containers) } };
    let hidden : Vec<&ID> = hides_after . iter ()
      . filter ( |member| ! disk_hides . contains (member) )
      . filter ( |member| ! deleted . contains (*member) )
      . collect ();
    let unhidden : Vec<&ID> = disk_hides . iter ()
      . filter ( |member| ! hides_after . contains (member) )
      . filter ( |member| ! deleted . contains (*member) )
      . collect ();
    for member in &hidden {
      changes . push ( format! ( "Hides {} from {}{}.",
        label (member), forker_label (&node . pid), as_content_of (member) )); }
    for member in &unhidden {
      changes . push ( format! ( "Stops hiding {} from {}{}.",
        label (member), forker_label (&node . pid), as_content_of (member) )); }
    let Some (disk) = &on_disk else { continue; };
    if partner_contents . is_empty () { continue; }
    let contains_after : Vec<ID> = visible_members (&node . contains);
    let removed : Vec<ID> = visible_members (&disk . contains) . into_iter ()
      . filter ( |child| ! contains_after . contains (child) )
      . filter ( |child| ! deleted . contains (child) )
      . collect ();
    let all_content : HashSet<ID> = partner_contents . iter ()
      . flat_map ( |(_, content)| content . iter () . cloned () )
      . collect ();
    for child in &removed {
      let stands_for : Option<ID> = member_it_stands_for (
        graph, restriction, child, &all_content ) . map ( |member| key (&member) );
      let it_or = |member : &ID| -> String {
        if member == child { "it" . to_string () } else { label (member) } };
      let keepers : Vec<String> = // the owned notes keeping what it stands for after the save
        stands_for . as_ref ()
        . map ( |member| owned_notes_containing (
                  graph, config, restriction, member,
                  &[ node . pid . clone () ], Some (&after_save) ))
        . unwrap_or_default ()
        . iter () . map ( |keeper| label (keeper) ) . collect ();
      let placed_under : Vec<String> = // the forks folder's members this save places the child under
        partner_contents . iter ()
        . filter ( |(partner, content_before)|
                   ! content_before . contains (child)
                   && contains_after_save . get (partner)
                      . is_some_and ( |content_after| content_after . contains (child) ))
        . map ( |(partner, _)| label (partner) )
        . collect ();
      let why : String = match stands_for {
        _ if ! placed_under . is_empty () => format! (
          "this save places it under {}", placed_under . join (" and ") ),
        None => format! (
          "it is no content of {}, nor a mirror of any",
          partner_contents . iter ()
            . map ( |(partner, _)| label (partner) )
            . collect::<Vec<String>> () . join (" or ") ),
        Some (member) if hidden . contains (&&member)
                         || deleted . contains (&member) => continue,
        Some (member) if hides_after . contains (&member) =>
          if member == *child { "it was hidden already" . to_string () }
          else { format! ("{}, which it stands for, was hidden already",
                         label (&member)) },
        Some (member) if contains_after . contains (&member) => format! (
          "{} still contains {}, which it stands for",
          label (&node . pid), label (&member) ),
        Some (member) if ! keepers . is_empty () => format! (
          "{} {} {}{}",
          keepers . join (" and "),
          if keepers . len () == 1 { "keeps" } else { "keep" },
          it_or (&member),
          if member == *child { "" } else { ", which it stands for" } ),
        Some (member) => format! (
          "this save shows {} under {}",
          it_or (&member), containers_of (&member) ), };
      nothings . push ( format! ( "Hides nothing: {} left {}, but {}.",
        label (child), label (&node . pid), why )); }}
  changes . extend (nothings);
  changes }
