//! A read receipt ('docs/forks.org', "Read receipts"): one gesture
//! that writes a note under the reader's fork of an item someone
//! mailed them, forking the item first if the reader holds no fork of
//! it, and hides the item from the reader's fork of the mailbox it
//! came in. This module plans the writes from the graph alone; the
//! "receipt" request ('server/serve/handlers/receipt.rs') writes them
//! as one batch, so that no half of the gesture lands without the
//! other.
//! .
//! A mailbox of an item is a note of another owner whose content
//! holds the item, or a mirror of it, and that a note the reader owns
//! forks: the reader's fork of the mailbox. The hide lands there, on
//! the member the mailbox's list names, as a save's would
//! ('member_it_stands_for').

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::mirrors_of;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::from_text::fork::{
  build_fork_clone, existing_owned_fork_of, validate_fork_specs_in_graph};
use crate::from_text::hide_report::hide_report_lines;
use crate::from_text::local_fieldintent_collection::resolve_visibility::member_it_stands_for;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::{Graphnode, empty_graphnode};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::save::{ForkSpec, NodeInstruction, SaveNode};

use std::collections::{BTreeSet, HashSet};

/// The receipt's title when the request names none.
pub const DEFAULT_RECEIPT_TITLE : &str = "Read.";

/// What the reader asks for.
#[derive(Clone, Debug)]
pub struct ReceiptRequest {
  pub item      : ID,
  pub text      : String, // the receipt's body, unless it is empty or equals the title
  pub title     : String,
  pub mailbox   : Option<ID>, // the mailbox, or the reader's fork of it; inferred when absent
  pub fork_into : Option<SkgrepoName>, // the skgrepo of a new fork of the item
  pub hide      : bool,
}

/// One mailbox of the item.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mailbox {
  pub note   : ID, // the other's note, by pid
  pub fork   : ID, // the reader's fork of it, by pid
  pub member : ID, // the member of its content the item stands for, as its list names it
}

/// The writes a receipt makes, and what it says of them.
pub struct ReceiptPlan {
  pub receipt      : Graphnode,
  pub holder       : Graphnode, // the note the receipt is the first child of, as written
  pub new_fork     : Option<ForkSpec>, // when the holder is a new fork of the item
  pub mailbox_fork : Option<Graphnode>, // the reader's fork of the mailbox, as written, when it changes
  pub lines        : Vec<String>,
}

impl ReceiptPlan {
  /// The writes, in the order the filesystem gets them: the receipt,
  /// its holder, the fork of the mailbox.
  pub fn node_instructions (
    &self,
  ) -> Vec<NodeInstruction> {
    [ Some (&self . receipt), Some (&self . holder),
      self . mailbox_fork . as_ref () ]
      . into_iter () . flatten ()
      . map ( |node| NodeInstruction::Save ( SaveNode ( node . clone () )) )
      . collect () }

  /// The pids the receipt writes, in the same order.
  pub fn written_pids (
    &self,
  ) -> Vec<ID> {
    [ Some (&self . receipt), Some (&self . holder),
      self . mailbox_fork . as_ref () ]
      . into_iter () . flatten ()
      . map ( |node| node . pid . clone () )
      . collect () }
}

/// Why a receipt writes nothing. The last two say what the reader
/// must choose; a client can offer the choices and ask again.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReceiptRefusal {
  Plain (String),
  ChooseSkgrepo { message : String,
                  choices : Vec<SkgrepoName> },
  ChooseMailbox { message : String,
                  choices : Vec<(ID, String)> }, // each mailbox's pid and title
}

impl ReceiptRefusal {
  pub fn message (
    &self,
  ) -> &str {
    match self {
      ReceiptRefusal::Plain (message)
        | ReceiptRefusal::ChooseSkgrepo { message, .. }
        | ReceiptRefusal::ChooseMailbox { message, .. } => message, } }
}

/// Plan the receipt 'request' asks for, against 'graph'.
pub fn plan_receipt (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  request     : &ReceiptRequest,
) -> Result<ReceiptPlan, ReceiptRefusal> {
  let item : &GraphnodeInRust = graph . pid_of (&request . item)
    . filter ( |pid| graph . node_is_shown (pid, restriction) )
    . and_then ( |pid| graph . nodes . get (&pid) )
    . ok_or_else ( || ReceiptRefusal::Plain ( format! (
      "No note the skgrepo restriction shows has the ID {}.", request . item ))) ?;
  let title : String = request . title . trim () . to_string ();
  if title . is_empty () {
    return Err ( ReceiptRefusal::Plain (
      "A receipt needs a title." . to_string () )); }
  let mailbox : Option<Mailbox> =
    chosen_mailbox (graph, config, restriction, item, request) ?;
  let receipt_pid : ID = ID ( uuid::Uuid::new_v4 () . to_string () );
  let (holder, new_fork, holder_line) : (Graphnode, Option<ForkSpec>, String) =
    receipt_holder (graph, config, restriction, item, mailbox . as_ref (),
                    request, &receipt_pid) ?;
  let receipt : Graphnode = {
    let body : Option<String> = // a body restating the title says nothing
      Some ( request . text . trim_end () . to_string () )
      . filter ( |text| ! text . trim () . is_empty () && text . trim () != title );
    Graphnode { pid          : receipt_pid,
                home_skgrepo : holder . home_skgrepo . clone (),
                title        : title . clone (),
                body,
                ..empty_graphnode () }};
  let (mailbox_fork, hide_lines) : (Option<Graphnode>, Vec<String>) =
    if ! request . hide {
      ( None, vec! [ "Hides nothing: the request asks for no hide." . to_string () ] )
    } else {
      match &mailbox {
        None => ( None, vec! [ format! (
          "Hides nothing: no note of another's that holds '{}' is forked by \
           a note of yours, so it is in no mailbox you read.", item . title ) ] ),
        Some (mailbox) =>
          mailbox_fork_hiding (graph, config, restriction, item, mailbox) ?, }};
  let mut lines : Vec<String> = vec! [ holder_line ];
  lines . extend (hide_lines);
  Ok ( ReceiptPlan { receipt, holder, new_fork, mailbox_fork, lines } ) }

/// The mailbox the receipt hides the item from: the one the request
/// names (the other's note, or the reader's fork of it), else the
/// only one. With several and none named, a receipt that hides is
/// refused, listing them; one that does not hide uses none.
fn chosen_mailbox (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  item        : &GraphnodeInRust,
  request     : &ReceiptRequest,
) -> Result<Option<Mailbox>, ReceiptRefusal> {
  let candidates : Vec<Mailbox> =
    mailboxes_of (graph, config, restriction, &item . pid);
  let title_of = |pid : &ID| -> String {
    graph . nodes . get (pid) . map ( |node| node . title . clone () )
      . unwrap_or_else ( || pid . to_string () ) };
  let listed = || -> String {
    if candidates . is_empty () { "it is in none".to_string () }
    else { format! ( "it is in {}", candidates . iter ()
                     . map ( |c| format! ( "'{}' ({})", title_of (&c . note), c . note ))
                     . collect::<Vec<String>> () . join (", ") ) } };
  if let Some (named) = &request . mailbox {
    let named_pid : ID = graph . pid_else_raw_id (named);
    return candidates . iter ()
      . find ( |c| c . note == named_pid || c . fork == named_pid )
      . cloned () . map (Some)
      . ok_or_else ( || ReceiptRefusal::Plain ( format! (
        "{} is no mailbox of '{}' that you read (a note of another's that \
         holds it, which a note of yours forks), nor your fork of one: {}.",
        named, item . title, listed () ))); }
  match candidates . len () {
    0 => Ok (None),
    1 => Ok ( candidates . into_iter () . next () ),
    _ if ! request . hide => Ok (None),
    _ => Err ( ReceiptRefusal::ChooseMailbox {
      message : format! (
        "'{}' is in several mailboxes you read, so name the one to hide it \
         from (--mailbox): {}.", item . title, listed () ),
      choices : candidates . iter ()
        . map ( |c| ( c . note . clone (), title_of (&c . note) ))
        . collect () } ), }}

/// Every note of another owner whose content holds the item, or a
/// mirror of it, and that a note the reader owns forks, by pid.
pub fn mailboxes_of (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  item        : &ID,
) -> Vec<Mailbox> {
  let containers : BTreeSet<ID> =
    mirrors_of (graph, restriction, item, None) . pids () . iter ()
    . flat_map ( |mirror| graph . inbound_pids_for_relation (
      mirror, NodeRelation::Contains ))
    . collect ();
  containers . into_iter ()
    . filter_map ( |container| {
      let node : &GraphnodeInRust = graph . nodes . get (&container) ?;
      if config . skgrepo_is_owned (&node . home_skgrepo)
         || ! graph . node_is_shown (&container, restriction) {
        return None; }
      let fork : ID = existing_owned_fork_of (config, graph, &container) ?;
      let members : HashSet<ID> = node . contains . iter ()
        . filter ( |record| SkgrepoRestriction::record_is_visible (
          restriction, &record . relRepo ))
        . map ( |record| record . member . clone () )
        . collect ();
      let member : ID = member_it_stands_for (graph, restriction, item, &members) ?;
      Some ( Mailbox { note : container, fork, member } ) } )
    . collect () }

/// The note the receipt goes under, with the receipt as its first
/// child, and the line that says so: the item itself if the reader
/// owns it; else the reader's fork of it; else a new fork of it, in
/// the skgrepo 'fork_skgrepo' gives.
fn receipt_holder (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  item        : &GraphnodeInRust,
  mailbox     : Option<&Mailbox>,
  request     : &ReceiptRequest,
  receipt_pid : &ID,
) -> Result<(Graphnode, Option<ForkSpec>, String), ReceiptRefusal> {
  let existing : Option<(ID, &str)> =
    if config . skgrepo_is_owned (&item . home_skgrepo) {
      Some (( item . pid . clone (), "which is yours" ))
    } else { existing_owned_fork_of (config, graph, &item . pid)
             . map ( |fork| ( fork, "your fork of it" )) };
  if let Some ((pid, which)) = existing {
    let mut holder : Graphnode = Graphnode::from (
      graph . nodes . get (&pid) . expect ("an existing fork is a node") );
    if let Some (named) = &request . fork_into {
      if named != &holder . home_skgrepo {
        return Err ( ReceiptRefusal::Plain ( format! (
          "The receipt goes under '{}' ({}, {}), in '{}', not in '{}' as \
           --fork-into says; leave --fork-into out.",
          holder . title, holder . pid, which, holder . home_skgrepo, named ))); }}
    if ! restriction . contains_skgrepo (&holder . home_skgrepo) {
      return Err ( ReceiptRefusal::Plain ( format! (
        "The receipt goes under '{}' ({}, {}), whose skgrepo '{}' the \
         skgrepo restriction keeps out.",
        holder . title, holder . pid, which, holder . home_skgrepo ))); }
    holder . contains . insert ( 0, RelPartner::at_relRepo (
      holder . home_skgrepo . clone (), receipt_pid . clone () ));
    let line : String = format! (
      "Writes the receipt '{}' as the first child of '{}' ({}), {}, in '{}'.",
      request . title . trim (), holder . title, holder . pid, which,
      holder . home_skgrepo );
    return Ok (( holder, None, line )); }
  let skgrepo : SkgrepoName = match &request . fork_into {
    Some (named) => named . clone (),
    None => default_fork_skgrepo (graph, config, restriction, item, mailbox) ?, };
  let buffer_node : Graphnode = { // the item, with the receipt typed as its first child
    let mut node : Graphnode = Graphnode::from (item);
    node . contains . insert ( 0, RelPartner::at_relRepo (
      skgrepo . clone (), receipt_pid . clone () ));
    node };
  let spec : ForkSpec = build_fork_clone (
    &buffer_node, &item . title, &item . contains, skgrepo . clone (),
    true, config, restriction );
  let errors : Vec<BufferValidationError> = validate_fork_specs_in_graph (
    std::slice::from_ref (&spec), graph, config, restriction );
  if ! errors . is_empty () {
    return Err ( ReceiptRefusal::Plain (
      errors . iter () . map ( |e| e . to_string () )
        . collect::<Vec<String>> () . join (" ") )); }
  let line : String = format! (
    "Forks '{}' ({}, of '{}') into '{}', and writes the receipt '{}' as \
     the first child of the fork.",
    item . title, item . pid, item . home_skgrepo, skgrepo,
    request . title . trim () );
  Ok (( spec . clone . 0 . clone (), Some (spec), line )) }

/// The skgrepo of a new fork of the item when the request names
/// none. With one owned skgrepo the skgrepo restriction shows, that
/// one. Otherwise the home of the reader's fork of the mailbox, as a
/// save's fork of the item placed under that fork would suggest, but
/// only where the item is homed where its mailbox is: an item from
/// another skgrepo of its owner may be more private than the
/// mailbox, and so than the reader's fork of it, which may be public.
/// Otherwise nothing is suggested, and the reader must choose.
fn default_fork_skgrepo (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  item        : &GraphnodeInRust,
  mailbox     : Option<&Mailbox>,
) -> Result<SkgrepoName, ReceiptRefusal> {
  let choices : Vec<SkgrepoName> = config . privacy_ladder () . into_iter ()
    . filter ( |skgrepo| restriction . contains_skgrepo (skgrepo) )
    . collect ();
  match choices . as_slice () {
    [] => return Err ( ReceiptRefusal::Plain (
      "A receipt for a note of another's forks it, but you own no skgrepo \
       the skgrepo restriction shows." . to_string () )),
    [only] => return Ok ( only . clone () ),
    _ => {}, }
  let home_of = |pid : &ID| -> Option<SkgrepoName> {
    graph . nodes . get (pid) . map ( |node| node . home_skgrepo . clone () ) };
  let reason : String = match mailbox {
    None => format! ( "'{}' is in no mailbox you read, so nothing suggests one",
                      item . title ),
    Some (mailbox) => {
      let mailbox_home : Option<SkgrepoName> = home_of (&mailbox . note);
      let fork_home : Option<SkgrepoName> = home_of (&mailbox . fork);
      if let (Some (mailbox_home), Some (fork_home)) = (&mailbox_home, &fork_home) {
        if *mailbox_home == item . home_skgrepo
           && choices . contains (fork_home) {
          return Ok ( fork_home . clone () ); }}
      format! (
        "'{}' is homed in '{}', not in its mailbox's skgrepo{}, so it may be \
         more private than your fork of the mailbox{}",
        item . title, item . home_skgrepo,
        mailbox_home . map ( |h| format! (" '{}'", h) ) . unwrap_or_default (),
        fork_home . map ( |h| format! (", in '{}'", h) ) . unwrap_or_default () ) }, };
  Err ( ReceiptRefusal::ChooseSkgrepo {
    message : format! (
      "Name the skgrepo for your fork of '{}' (--fork-into): {}. Yours: {}.",
      item . title, reason,
      choices . iter () . map ( |s| s . to_string () )
        . collect::<Vec<String>> () . join (", ") ),
    choices } ) }

/// The reader's fork of the mailbox, hiding the member the item
/// stands for, and the lines that say so. Where it already hides the
/// member, nothing changes. Where it contains a mirror of the member
/// (a reply typed under it there files it), the receipt takes that out
/// too, as removing its headline would.
fn mailbox_fork_hiding (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  item        : &GraphnodeInRust,
  mailbox     : &Mailbox,
) -> Result<(Option<Graphnode>, Vec<String>), ReceiptRefusal> {
  let mut fork : Graphnode = Graphnode::from (
    graph . nodes . get (&mailbox . fork)
      . expect ("a mailbox's fork is a node") );
  let member_pid : ID = graph . pid_else_raw_id (&mailbox . member);
  let already_hidden : bool = fork . hides . or_default () . iter ()
    . any ( |record| graph . pid_else_raw_id (&record . member) == member_pid );
  if already_hidden {
    return Ok (( None, vec! [ format! (
      "Hides nothing: '{}' already hides '{}'.", fork . title, item . title ) ] )); }
  if ! restriction . contains_skgrepo (&fork . home_skgrepo) {
    return Err ( ReceiptRefusal::Plain ( format! (
      "The hide goes in '{}' ({}), your fork of the mailbox, whose skgrepo \
       '{}' the skgrepo restriction keeps out.",
      fork . title, fork . pid, fork . home_skgrepo ))); }
  let mirrors_of_member : HashSet<ID> =
    mirrors_of (graph, restriction, &member_pid, None) . pids ()
    . into_iter () . collect ();
  fork . contains . retain ( |record| ! mirrors_of_member . contains (
    &graph . pid_else_raw_id (&record . member) ));
  let member_home : SkgrepoName = graph . nodes . get (&member_pid)
    . map ( |node| node . home_skgrepo . clone () )
    . unwrap_or_else ( || fork . home_skgrepo . clone () );
  let record : RelPartner<ID> = RelPartner::at_relRepo (
    config . default_relRepo (&fork . home_skgrepo, &member_home),
    mailbox . member . clone () );
  fork . hides = MSV::Specified (
    fork . hides . or_default () . iter () . cloned ()
      . chain ( std::iter::once (record) ) . collect () );
  let lines : Vec<String> = hide_report_lines (
    graph, config, restriction,
    &[ NodeInstruction::Save ( SaveNode ( fork . clone () )) ], &[] );
  Ok (( Some (fork), lines )) }
