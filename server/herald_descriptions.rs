//! PURPOSE: Say in plain sentences what the heralds of one headline
//! mean, with this headline's own numbers, letters and skgrepos read
//! out: first what the headline is, when it is a folder; then one
//! sentence per herald, most informative first. Both clients and
//! skg-for-ai ask for them over the "describe heralds" endpoint
//! ('docs/api-and-formats.org', "Describe heralds"), sending the
//! headline's metadata as their buffer holds it, so the sentences and
//! the logic that fills them in live here alone.
//!
//! A sentence is keyed on the path, in the headline's metadata, of the
//! element that makes a herald appear, as 'text_emitting_rule_paths'
//! (server/heralds.rs) names it ("view/noWarrant",
//! "editRequest/delete"). Walking the metadata, the first path along
//! the way that has a sentence is described, values and all. A path
//! the rule table renders but no sentence covers reads "No description
//! yet for PATH."; the unit tests require that there be none. The
//! relationship heralds, which the clients render themselves from
//! '(view (rels ...) (props ...) (birth ...))', are described here
//! from those facts, by the rules 'docs/heralds.org' states.
//!
//! Order: by the herald's style, a style that asks more attention
//! first ('STYLES_MOST_INFORMATIVE_FIRST'); the folder's sentence
//! before them all.

use crate::herald_conformance::emittable_kind_atoms;
use crate::heralds::{herald_styles, text_emitting_rule_paths};
use crate::types::config::SkgConfig;
use crate::types::misc::SkgrepoName;
use crate::types::nodes::complete::Flag;
use crate::types::viewnode::{ChangesetMarkerKind, Folder, OtherChange};

use serde_json::Value;
use sexp::{Atom, Sexp};
use std::collections::BTreeMap;
use std::sync::OnceLock;

/// 'shared/relations.json', for the relations' letters and display
/// order.
const RELATIONS_JSON : &str =
  include_str! ("../shared/relations.json");

/// The herald styles ('shared/herald-styles.json'), one that asks the
/// reader more attention first. A sentence sorts by its herald's style.
pub const STYLES_MOST_INFORMATIVE_FIRST : [&str; 12] = [
  "crucial", "ghost", "stop", "high", "nonstandard", "message",
  "go", "medium", "ugly", "low", "normal", "boring" ];

/// What a sentence is said for a headline with no heralds.
pub const NO_HERALDS_SENTENCE : &str =
  "No heralds are shown for this headline.";

/// One sentence, and the style of the herald it describes, if that
/// differs from the style the rule table gives its path.
type Sentence = (Option<&'static str>, String);

/// What the sentences need from the whole headline.
struct HeadlineContext<'a> {
  kind            : String,         // its kind atom; "node" when it names none
  folder          : Option<Folder>, // its folder, when it is one
  node_repo       : Option<String>,
  attribution     : Option<(String, bool)>, // (NAME, unverified)
  write_protected : bool,
  birth           : Vec<(String, String, Option<usize>)>, // (RELATION, SIDE, GEN)
  config          : &'a SkgConfig,
}

/// The sentences describing the headline whose metadata is
/// METADATA_TEXT, '(skg ...)' as a client's buffer holds it (with the
/// atom 'pendingDetachment' added by a client that sees the headline
/// is one). FOLDER_CHANGE is what the client draws on a folder of the
/// diff view: None, or whether every change of its members'
/// relationship to the note above it is processed.
pub fn describe_heralds (
  metadata_text : &str,
  folder_change : Option<bool>,
  config        : &SkgConfig,
) -> Result<Vec<String>, String> {
  let metadata : Sexp = sexp::parse (metadata_text)
    . map_err ( |e| format! ("the metadata does not read: {}", e) ) ?;
  let parts : &[Sexp] = match &metadata {
    Sexp::List (items)
      if items . first () . and_then (atom_text) . as_deref () == Some ("skg") =>
      &items [1..],
    _ => return Err ( "the metadata is no '(skg ...)' form" . to_string () ), };
  let context : HeadlineContext = HeadlineContext::of (parts, config);
  let rule_paths : BTreeMap<String, Option<&'static str>> =
    text_emitting_rule_paths ();
  let mut described : Vec<(usize, String)> = Vec::new ();
  if let Some (folder) = &context . folder {
    described . push (( 0, folder_sentence (folder) ));
    if let Some (processed) = folder_change {
      described . push (( 0, changed_folder_sentence (processed) )); }}
  for part in parts {
    describe_element ( part, &[], &context, &rule_paths, &mut described ); }
  described . sort_by_key ( |(rank, _)| *rank );
  Ok ( if described . is_empty () {
         vec! [ NO_HERALDS_SENTENCE . to_string () ]
       } else {
         described . into_iter () . map ( |(_, sentence)| sentence ) . collect () } ) }

/// Describe ELEMENT, a part of the metadata under PARENT_PATH: by the
/// sentences of its path when it has them; else, when the rule table
/// renders the path, by saying it has none yet; else by describing
/// each element inside it.
fn describe_element (
  element     : &Sexp,
  parent_path : &[String],
  context     : &HeadlineContext,
  rule_paths  : &BTreeMap<String, Option<&'static str>>,
  out         : &mut Vec<(usize, String)>,
) {
  let (head, values) : (String, &[Sexp]) = match element {
    Sexp::Atom (_) => match atom_text (element) {
      Some (text) => (text, &[]),
      None => return, },
    Sexp::List (items) => match items . split_first () {
      Some ((first, rest)) => match atom_text (first) {
        Some (text) => (text, rest),
        None => return, },
      None => return, }, };
  let path : Vec<String> = [ parent_path, &[head] ] . concat ();
  let path_text : String = path . join ("/");
  let path_style : Option<&'static str> = style_of_path (&path_text, rule_paths);
  if let Some (sentences) = sentences_for_path (&path_text, values, context) {
    for (style, sentence) in sentences {
      out . push (( rank_of_style ( style . or (path_style) ), sentence )); }
  } else if rule_paths . contains_key (&path_text) {
    out . push (( rank_of_style (path_style),
                  format! ("No description yet for {}.", path_text) ));
  } else {
    for value in values {
      describe_element ( value, &path, context, rule_paths, out ); }} }

/// The sentences of the herald(s) that the element at PATH, with
/// VALUES after its head, makes appear; or None when PATH has none.
/// An empty list says the element shows nothing a sentence elsewhere
/// does not already say.
fn sentences_for_path (
  path    : &str,
  values  : &[Sexp],
  context : &HeadlineContext,
) -> Option<Vec<Sentence>> {
  let words : Vec<String> = values . iter () . filter_map (atom_text) . collect ();
  let first : String = words . first () . cloned () . unwrap_or_default ();
  let one = |sentence : String| -> Option<Vec<Sentence>> {
    Some ( vec! [ (None, sentence) ] ) };
  match path {
    "ghost" => one ( "ᗣ☮: a ghost: a note the worktree lacks, shown with the \
                      text HEAD (the last git-commit) has for it; it is \
                      write-protected." . to_string () ),
    "view/ghostWarrant" => one ( "ᗣ: only HEAD (the last git-commit) puts it \
                                  here; the worktree no longer does (a removed \
                                  alias, say)." . to_string () ),
    "view/noWarrant" => one ( "⊥: the graph does not relate this note to the \
                               headline above it as its position implies \
                               (content its parent does not contain, say, or a \
                               folder member the folder's relation does not \
                               hold for)." . to_string () ),
    "writeProtected" => one ( "☮: this occurrence is write-protected: a save \
                               refuses an edit to it here, though the note may \
                               be yours and editable elsewhere in the view."
                               . to_string () ),
    "view/omittedBody" => one ( "B: it has a body, which this write-protected \
                                 occurrence leaves out; visit the note to read \
                                 it." . to_string () ),
    "view/cycle" => one ( "⟳: it repeats a note above it in this view, so its \
                           content is not drawn again here." . to_string () ),
    "view/repoBoundary" => one ( home_skgrepo_sentence (context) ),
    // Read by the home skgrepo and relationship sentences.
    "view/attribution" | "view/birth" => Some ( vec! [] ),
    "view/rels" => Some ( relationship_sentences (values, context) ),
    "view/props" => Some ( property_count_sentences (values) ),
    "inPlaceOf" => Some ( vec! [ ( Some ( relationship_style ("in_place_of") ),
      "ĥ (after K): it stands in the place of one of its mirrors, a note \
       joined to it by forks; its parent contains that note, not this one."
       . to_string () ) ] ),
    "skgRepos/relRepo" => one ( format! (
      "~{0}: its relationship to the headline above it is stored in the \
       skgrepo '{0}', more private than that relationship's default.", first )),
    "editRequest/relRepo" => one ( format! (
      "request:~{0}: a pending request: the next save stores its \
       relationship to the headline above it in the skgrepo '{0}'.", first )),
    "pendingDetachment" => one ( "detach: the next save takes this headline \
      out of its parent's list (in a forks folder, ends the fork), leaving the \
      note itself; attaching it again (C-c s a; :SkgAttach) first cancels \
      that." . to_string () ),
    "alias" => one ( "alias: an alias of the note two levels up: another \
                      name for it." . to_string () ),
    "id" => one ( "ID: an extra ID of the note two levels up, which \
                   resolves to that note too." . to_string () ),
    "flag" => one ( flag_sentence (&first) ),
    "oldText" => one ( old_text_sentence (&first) ),
    "bodyDiff" => one ( "body diff: how this note's body changed since HEAD \
                         (the last git-commit), as a diff." . to_string () ),
    "homeSkgrepoMove" | "diff/homeSkgrepoMove" => {
      let (old, new) : (String, String) =
        ( form_value (values, "old"), form_value (values, "new") );
      one ( format! (
        "{}-{} +{}: its home moved from the skgrepo '{}' to '{}' since HEAD \
         (the last git-commit).",
        if path == "homeSkgrepoMove" { "⌂ " } else { "" },
        old, new, old, new )) },
    "relRepoMove" => {
      let (old, new) : (String, String) =
        ( form_value (values, "old"), form_value (values, "new") );
      one ( format! (
        "relationship moved from {0} to {1}: its relationship to the note \
         above it moved from the skgrepo '{0}' to '{1}' since HEAD (the last \
         git-commit).", old, new )) },
    "changesetMarker" => one ( changeset_marker_sentence (&first) ),
    "otherChanges" => one ( format! (
      "{}: the note's other changes since HEAD (the last git-commit).",
      words . iter ()
        . filter_map ( |word| OtherChange::from_wire_name (word) )
        . map (OtherChange::in_words)
        . collect::<Vec<&str>> () . join ("; ") )),
    "diff/node" | "diff/rel" | "diff/properties" =>
      one ( change_fact_sentence (path, &first, context) ),
    "diff/processed" => Some ( processed_sentences (&words) ),
    "diff/movedLikeAncestor" => one ( format! (
      "Δ⌂={0}: its home moved the same way as that of {0}, the nearest \
       moved note above it.", first )),
    "editRequest/processed" | "editRequest/unprocessed" =>
      one ( processed_request_sentence (path, &words) ),
    "tombstone" => one ( "something stood here: a tombstone, left where a \
      headline stood that can stand here for nothing (a note a save deleted, \
      say, or one the skgrepo restriction now keeps out); it reveals nothing \
      of what it was." . to_string () ),
    "unknown" => one ( "Reference to unknown node.: the graph has no note \
      with this ID (it is missing, not merely outside the skgrepo \
      restriction)." . to_string () ),
    "restricted" => one ( "restricted: a note the skgrepo restriction keeps \
      out, which a record you can see names; nothing more of it shows."
      . to_string () ),
    "editRequest/delete" => one ( "delete: a pending request: the next save \
      deletes this note." . to_string () ),
    "editRequest/merge" => one ( format! (
      "merge:{0}: a pending request: the next save merges the note {0} into \
       this one, which acquires it.", first )),
    "editRequest/restoreNode" => one ( "restore node: a pending request: the \
      next save restores this note as HEAD (the last git-commit) has it."
      . to_string () ),
    "editRequest/restoreRel" => one ( "restore membership: a pending request: \
      the next save restores its relationship to the headline above it as HEAD \
      (the last git-commit) has it." . to_string () ),
    "editRequest/stripAttribution" => one ( "request:strip attribution: a \
      pending request: the next save removes its record of the note it was \
      forked from." . to_string () ),
    "editRequest/flag" => one ( match words . last () . map (String::as_str) {
      Some ("true") => "request:no search matching: a pending request: after \
        the next save, text searches no longer match it directly.",
      _ => "request:search matching: a pending request: after the next save, \
        text searches match it again.", } . to_string () ),
    "viewRequests" => Some ( values . iter ()
      . map ( |request| (None, view_request_sentence (request)) )
      . collect () ),
    // A folder is described before every herald ('folder_sentence').
    _ if folder_of_atom (path) . is_some () => Some ( vec! [] ),
    _ => None, }}

//
// The folders
//

/// What FOLDER is and how its members relate to the note above it,
/// after the label the folder shows.
pub fn folder_sentence (
  folder : &Folder,
) -> String {
  match folder {
    Folder::Forks =>
      "Its mirrors are these.: its mirrors: the notes this one forks or is \
       forked by, and the notes joined to those by forks. On a member, 'bK' \
       says this note (b, two levels up) forks it, 'Kb' that it forks this \
       note, 'bKb' both, and no 'b' that it is joined to this note only \
       through other forks. Each member shows only what it contains that this \
       note neither contains nor hides and you keep in no other note.",
    Folder::Hider =>
      "☮ These hide it from their forks.: the notes that hide the note above \
       from their forks: each keeps it out of what it shows of the members of \
       its own forks folder. Write-protected here; a hide is edited in the \
       hider's view.",
    Folder::Hidden =>
      "☮ It hides these from its forks.: the notes the note above hides from \
       its forks: it keeps them out of what it shows of every member of its \
       forks folder. Write-protected here; edit those hides in its forks \
       folder.",
    Folder::IntegratedElsewhere =>
      "☮ It contains these, and you keep them elsewhere.: what the forks-folder \
       member above contains that you already keep in another note of yours, \
       so the member does not show it as new.",
    Folder::HiddenInForkPartner =>
      "☮ It contains these, but c hides them.: what the forks-folder member \
       above contains that c, the note whose forks folder holds it (three \
       levels up), hides.",
    Folder::HiddenByForker =>
      "☮ a hides these, which c contains.: what a, the forker above, hides of \
       the content of c, the note whose forks folder holds it (three levels \
       up). Write-protected: a's hides are edited in a's own view.",
    Folder::HiddenOutsideOfForkPartners =>
      "b hides these, but no fork partner shown contains them.: what b, the \
       note whose forks folder this stands in, hides that no member of that \
       folder contains. Editable: removing a headline from here unhides it.",
    Folder::Container =>
      "☮ These also contain it.: the other notes that contain the note above, \
       each with its own containers under it, so nesting here reads upward.",
    Folder::Mentioner =>
      "☮ These link to it.: the notes whose text links to the note above; a \
       link lives in the linking note's text.",
    Folder::AcquiredBy =>
      "☮ it was acquired by: in the diff view, under a ghost: the note that \
       acquired it by a nodeMerge. Nothing in it is saved.",
    Folder::Acquired =>
      "☮ it acquired: in the diff view, under a note: the ghosts of the notes \
       it acquired by a nodeMerge. Nothing in it is saved.",
    Folder::Alias =>
      "aliases: the aliases of the note above, other names for it, one \
       headline each. Editable: adding, rewording or removing a headline here \
       changes them.",
    Folder::ID =>
      "IDs: the extra IDs of the note above, other IDs that resolve to it, \
       one headline each.",
    Folder::Flags { .. } =>
      "☮ flags: the flags of the note above that are true, one row each.",
    Folder::IllegalEdit { .. } =>
      "☮ ILLEGAL EDIT TO FOREIGN NODE: a section of the foreign note above \
       held in one of your skgrepos, which no save of yours could have made \
       and which is no part of that note; nothing in it is saved (see \
       \"Illegal edits\" in docs/telescopes.org).",
  } . to_string () }

/// What a folder of the diff view shows when a member's relationship
/// to the note above it changed ('folder_heralds' in
/// 'shared/herald-styles.json').
fn changed_folder_sentence (
  processed : bool,
) -> String {
  format! ( "In the diff view, the label says that the relationship of some \
             member to the note above it changed since HEAD (the last \
             git-commit){}.",
            if processed { "; ✓: every such change is marked processed" }
            else { "" } ) }

/// The folder whose atom is ATOM, or None.
fn folder_of_atom (
  atom : &str,
) -> Option<Folder> {
  Folder::all () . into_iter ()
    . find ( |folder| folder . repr_in_client () == atom ) }

//
// Home skgrepo
//

/// The sentence of '⌂' ('docs/heralds.org', "Other heralds").
fn home_skgrepo_sentence (
  context : &HeadlineContext,
) -> String {
  let Some (repo) = &context . node_repo else {
    return "⌂: its home skgrepo is not that of the note above it."
      . to_string () };
  let label : String = skgrepo_label (repo, context . config);
  let lives_in : String =
    if context . config . skgrepo_is_owned ( &SkgrepoName::from (repo . as_str ()) ) {
      format! ("it lives in your skgrepo '{}'", label)
    } else {
      format! ("it lives in the skgrepo '{}', which you do not own", label) };
  match &context . attribution {
    None => format! ("⌂:{}: {}.", label, lives_in),
    Some ((original, unverified)) => {
      let original_label : String = skgrepo_label (original, context . config);
      format! ( "⌂:<{}/{}{}>: {} and was forked from a note of '{}'{}.",
                label, original_label,
                if *unverified { "?" } else { "" },
                lives_in, original_label,
                if *unverified {
                  ", though no loaded skgrepo is known to be the one it records"
                } else { "" } ) }}}

/// The label the heralds show for the skgrepo REPO: its
/// 'herald_label', or REPO when the config lacks it.
fn skgrepo_label (
  repo   : &str,
  config : &SkgConfig,
) -> String {
  config . skgrepos . get ( &SkgrepoName::from (repo) )
    . map ( |skgrepo| skgrepo . herald_label () . to_string () )
    . unwrap_or_else ( || repo . to_string () ) }

//
// Relationship heralds
//

/// One side of a relation in '(view (rels ...))'.
struct SideFacts {
  count       : usize,
  gens        : Vec<usize>,                  // distinct and sorted
  subset      : Option<(usize, Vec<usize>)>, // the fraction's numerator, if the side has one
  users_graph : bool, // 'usersGraph': a note of the user's graph is among its members
}

/// What the clients draw after an in-side that carries 'usersGraph'
/// ('heralds--rel-side-text').
const USERS_GRAPH_MARKER : &str = "•";

/// One sentence per relation token, in display order.
fn relationship_sentences (
  relation_forms : &[Sexp],
  context        : &HeadlineContext,
) -> Vec<Sentence> {
  relations_in_display_order () . into_iter ()
    . filter_map ( |(relation, letter)| {
      let form : &[Sexp] = form_tail (relation_forms, &relation) ?;
      relation_sentence (&relation, &letter, form, context) } )
    . collect () }

/// "TOKEN: what each side says." with the relation's tier, the
/// greatest of its sides'.
fn relation_sentence (
  relation : &str,
  letter   : &str,
  form     : &[Sexp],
  context  : &HeadlineContext,
) -> Option<Sentence> {
  let sides : Vec<(&str, SideFacts)> = [ "in", "out" ] . into_iter ()
    . filter_map ( |side| side_facts (relation, side, form)
                   . map ( |facts| (side, facts) ))
    // A side with no members shows nothing, as in 'side_text'.
    . filter ( |(_, facts)| facts . count > 0 || ! facts . gens . is_empty () )
    . collect ();
  if sides . is_empty () { return None; }
  let token_side = |wanted : &str| -> String {
    sides . iter () . find ( |(side, _)| *side == wanted )
      . map ( |(_, facts)| side_token_text (facts) )
      . unwrap_or_default () };
  let token : String = format! ( "{}{}{}", token_side ("in"), letter, token_side ("out") );
  let clauses : Vec<String> = sides . iter ()
    . map ( |(side, facts)| side_clause (relation, side, facts) )
    . collect ();
  let tier : &'static str = sides . iter ()
    . map ( |(side, facts)| side_tier (relation, side, facts, context) )
    . chain ( sides . iter () . any ( |(_, facts)| facts . users_graph )
              . then ( || relationship_style ("users_graph") ) )
    . fold ( "normal", greater_tier );
  let born_here : bool = context . birth . iter ()
    . any ( |(born_of, _, _)| born_of == relation );
  Some (( Some (tier),
          format! ( "{}: {}.{}", token, clauses . join ("; "),
                    if born_here { " This relationship is why it is drawn here." }
                    else { "" } ) )) }

/// The facts of SIDE of RELATION in FORM, '(RELATION (in ...) (out ...))'
/// less its head, or None when the side is absent.
fn side_facts (
  relation : &str,
  side     : &str,
  form     : &[Sexp],
) -> Option<SideFacts> {
  let facts : &[Sexp] = form_tail (form, side) ?;
  let subset_key : Option<&str> = match (relation, side) {
    ("contains", "out") => Some ("unintegrated"),
    ("linksTo",  "in")  => Some ("substantive"),
    _ => None, };
  Some ( SideFacts {
    count  : first_number (facts),
    gens   : ancestor_generations (facts),
    subset : subset_key . and_then ( |key| form_tail (facts, key) )
      . map ( |subset| ( first_number (subset), ancestor_generations (subset) )),
    users_graph : facts . iter () . any (
      |value| atom_text (value) . as_deref () == Some ("usersGraph") ), } ) }

/// The side's text in the token, as the clients render it
/// ('heralds--rel-side-text'): its count or fraction, then the marker
/// when a note of the user's graph is among its members.
fn side_token_text (
  facts : &SideFacts,
) -> String {
  format! ( "{}{}", side_count_text (facts),
            if facts . users_graph { USERS_GRAPH_MARKER } else { "" } ) }

/// The side's count, or its fraction, as the clients render it.
fn side_count_text (
  facts : &SideFacts,
) -> String {
  match &facts . subset {
    None => side_text (facts . count, &facts . gens),
    Some ((numerator, numerator_gens)) => {
      if facts . count == 0 { return String::new (); }
      if *numerator == 0 { return side_text (facts . count, &facts . gens); }
      let remaining : Vec<usize> = facts . gens . iter ()
        . filter ( |g| ! numerator_gens . contains (g) ) . copied () . collect ();
      format! ( "{}/{}", part_text (*numerator, numerator_gens, numerator_gens),
                if *numerator == facts . count { String::new () }
                else { part_text (facts . count, &remaining, &facts . gens) } ) }}}

fn side_text (
  count : usize,
  gens  : &[usize],
) -> String {
  if count == 0 && gens . is_empty () { String::new () }
  else { part_text (count, gens, gens) }}

/// COUNT, unless the ancestors MEMBER_GENS account for every member,
/// then the ancestor flags of GENS.
fn part_text (
  count       : usize,
  gens        : &[usize],
  member_gens : &[usize],
) -> String {
  format! ( "{}{}",
            if count == member_gens . len () { String::new () }
            else { count . to_string () },
            gens . iter () . map ( |g| ancestor_flag (*g) ) . collect::<String> () ) }

/// "5 notes link to this one; 2 of them are substantive (...)", for
/// one side.
fn side_clause (
  relation : &str,
  side     : &str,
  facts    : &SideFacts,
) -> String {
  let (verb_one, verb_many) : (&str, &str) = match relation {
    "contains" => ("contains", "contain"),
    "linksTo"  => ("links to", "link to"),
    "forks"    => ("forks", "fork"),
    _          => ("hides", "hide"), };
  let hides : bool = relation == "hides";
  let (members, among) : (String, Option<String>) =
    members_phrase (facts . count, &facts . gens);
  let many : bool = facts . count != 1;
  let main : String = if side == "in" {
    format! ( "{}{} {} this one{}{}",
              members,
              among . map ( |names| format! (", among them {},", names) )
                . unwrap_or_default (),
              if many { verb_many } else { verb_one },
              if ! hides { "" } else if many { " from their forks" }
              else { " from its forks" },
              if ! facts . users_graph { "" }
              else if hides { "; a note of yours is among them (•)" }
              else { "; a note of yours, not the one it stands under, is \
                      among them: you keep it elsewhere (•)" } )
  } else {
    format! ( "this note {} {}{}{}",
              verb_one, members,
              if hides { " from its forks" } else { "" },
              among . map ( |names| format! (", among them {}", names) )
                . unwrap_or_default () ) };
  match ( relation, &facts . subset ) {
    ( "linksTo", Some ((numerator, gens)) ) => format! (
      "{}; {}", main, subset_clause (
        facts . count, *numerator, gens,
        "substantive (it has a body, content, or links to other notes too)",
        "substantive (they have a body, content, or link to other notes too)" )),
    ( _, Some ((numerator, gens)) ) => format! (
      "{}; {}", main, subset_clause (
        facts . count, *numerator, gens,
        "new to you (neither the folder's note nor another note of yours \
         holds or hides it)",
        "new to you (neither the folder's note nor another note of yours \
         holds or hides them)" )),
    _ => main, }}

/// What the numerator NUMERATOR of TOTAL members says, WHAT_ONE or
/// WHAT_MANY being what it is of each.
fn subset_clause (
  total     : usize,
  numerator : usize,
  gens      : &[usize],
  what_one  : &str,
  what_many : &str,
) -> String {
  let among : String = if gens . is_empty () || numerator == 0 { String::new () }
    else { format! ( ", among them {}", names_of_ancestors (gens) ) };
  if total == 1 {
    if numerator == 1 { format! ("it is {}", what_one) }
    else { format! ("it is not {}", what_one . split (" (") . next () . unwrap_or (what_one)) }
  } else if numerator == 0 {
    format! ( "none of them is {}", what_one . split (" (") . next () . unwrap_or (what_one) )
  } else if numerator == total {
    format! ("all of them are {}", what_many)
  } else {
    format! ( "{} of them {} {}{}", numerator,
              if numerator == 1 { "is" } else { "are" },
              if numerator == 1 { what_one } else { what_many },
              among ) }}

/// The members of a side: (WHO, AMONG). WHO names the ancestors when
/// they are every member, else counts the members; AMONG then names
/// the ancestors among them, if any.
fn members_phrase (
  count : usize,
  gens  : &[usize],
) -> (String, Option<String>) {
  if ! gens . is_empty () && count == gens . len () {
    ( names_of_ancestors (gens), None )
  } else {
    ( format! ( "{} {}", count, if count == 1 { "note" } else { "notes" } ),
      ( ! gens . is_empty () ) . then ( || names_of_ancestors (gens) )) }}

/// "the headline above it (a) and the headline two levels up (b)".
fn names_of_ancestors (
  gens : &[usize],
) -> String {
  let names : Vec<String> = gens . iter () . map ( |g| ancestor_name (*g) ) . collect ();
  match names . split_last () {
    None => String::new (),
    Some ((last, [])) => last . clone (),
    Some ((last, rest)) => format! ( "{} and {}", rest . join (", "), last ), }}

fn ancestor_name (
  generation : usize,
) -> String {
  const WORDS : [&str; 11] = [ "", "one", "two", "three", "four", "five",
                               "six", "seven", "eight", "nine", "ten" ];
  if generation == 1 {
    format! ( "the headline above it ({})", ancestor_flag (generation) )
  } else {
    format! ( "the headline {} levels up ({})",
              WORDS . get (generation) . map ( |w| w . to_string () )
                . unwrap_or_else ( || generation . to_string () ),
              ancestor_flag (generation) ) }}

/// 'a' for the viewparent, 'b' the grandparent, ..., then '{27}'.
fn ancestor_flag (
  generation : usize,
) -> String {
  if ( 1 ..= 26 ) . contains (&generation) {
    char::from ( b'a' + (generation - 1) as u8 ) . to_string ()
  } else { format! ("{{{}}}", generation) }}

/// The tier a side shows ('docs/heralds.org', "Tiers" and "Birth"):
/// low when the birth relation accounts for its only members, which
/// are ancestors; the write-protected tier on a write-protected
/// occurrence's contents; else its relation's tier for that side; and
/// a fraction's subset tier when its numerator counts someone.
fn side_tier (
  relation : &str,
  side     : &str,
  facts    : &SideFacts,
  context  : &HeadlineContext,
) -> &'static str {
  let birth_explained : bool =
    ! facts . gens . is_empty () && facts . count == facts . gens . len ()
    && facts . gens . iter () . all ( |g| context . birth . iter () . any (
      |(born_of, born_side, born_gen)|
      born_of == relation && born_side == side && *born_gen == Some (*g) ));
  let base : &'static str =
    if birth_explained { relationship_style ("birth_explained_side") }
    else if relation == "contains" && side == "out" && context . write_protected {
      side_tier_named (relation, "out_write_protected") }
    else { side_tier_named (relation, side) };
  match &facts . subset {
    Some ((numerator, _)) if *numerator > 0 => greater_tier (
      base, side_tier_named ( relation,
                              if side == "out" { "out_unintegrated" }
                              else { "in_substantive" } )),
    _ => base, }}

/// The tier 'side_tiers' in 'shared/herald-styles.json' gives KEY of
/// RELATION.
fn side_tier_named (
  relation : &str,
  key      : &str,
) -> &'static str {
  herald_styles () ["relationship_heralds"] ["side_tiers"] [relation] [key]
    . as_str () . unwrap_or ("normal") }

/// The style 'relationship_heralds' in 'shared/herald-styles.json'
/// names KEY.
fn relationship_style (
  key : &str,
) -> &'static str {
  herald_styles () ["relationship_heralds"] [key] . as_str () . unwrap_or ("normal") }

/// The greater of the tiers A and B, by 'tier_order'.
fn greater_tier (
  a : &'static str,
  b : &'static str,
) -> &'static str {
  let position = |tier : &str| -> usize {
    herald_styles () ["tier_order"] . as_array ()
      . and_then ( |order| order . iter () . position ( |t| t . as_str () == Some (tier) ))
      . unwrap_or (0) };
  if position (b) > position (a) { b } else { a }}

/// "A2", "I1", "F1": one sentence per property count.
fn property_count_sentences (
  count_forms : &[Sexp],
) -> Vec<Sentence> {
  let style : &'static str = relationship_style ("property_counts");
  [ ("aliases", "A"), ("extraIds", "I"), ("flags", "F") ] . into_iter ()
    . filter_map ( |(key, letter)| {
      let count : usize = first_number ( form_tail (count_forms, key) ? );
      let plural : bool = count != 1;
      Some (( Some (style), match key {
        "aliases" => format! ( "{}{}: it has {} alias{}, other names for it.",
                               letter, count, count, if plural { "es" } else { "" } ),
        "extraIds" => format! ( "{}{}: it has {} extra ID{}, which resolve{} to \
                                 it too.", letter, count, count,
                                if plural { "s" } else { "" },
                                if plural { "" } else { "s" } ),
        _ => format! ( "{}{}: {} of its flags {} true (such as 'no search \
                        matching').", letter, count, count,
                       if plural { "are" } else { "is" } ), } )) } )
    . collect () }

/// (NAME, LETTER) of each relation of 'shared/relations.json', in
/// display order.
fn relations_in_display_order (
) -> Vec<(String, String)> {
  static RELATIONS : OnceLock<Vec<(String, String)>> = OnceLock::new ();
  RELATIONS . get_or_init ( || {
    let file : Value = serde_json::from_str (RELATIONS_JSON)
      . expect ("shared/relations.json should parse as JSON");
    let mut relations : Vec<&Value> = file ["relations"] . as_array ()
      . expect ("shared/relations.json should list its relations")
      . iter () . collect ();
    relations . sort_by_key ( |r| r ["display_position"] . as_u64 () );
    relations . into_iter ()
      . map ( |r| ( r ["name"] . as_str () . unwrap_or_default () . to_string (),
                    r ["letter"] . as_str () . unwrap_or_default () . to_string () ))
      . collect () } ) . clone () }

//
// The other heralds
//

fn flag_sentence (
  wire_name : &str,
) -> String {
  match Flag::from_wire_name (wire_name) {
    None if wire_name . is_empty () =>
      "☮: a true flag of the note two levels up." . to_string (),
    None => format! ("No description yet for flag/{}.", wire_name),
    Some (flag) => format! (
      "☮ {}: a true flag of the note two levels up: {}.",
      flag . herald_text (),
      match flag {
        Flag::Had_ID_Before_Import => "it had an ID before it was imported",
        Flag::Was_Overloaded => "it was overloaded during the org-roam import",
        Flag::NoSearchMatching => "text searches do not match it directly", } ), }}

fn old_text_sentence (
  which : &str,
) -> String {
  let what : &str = match which {
    "titleAndBody" => "title and body",
    "title"        => "title",
    _              => "body", };
  format! ( "old {0}: the {0} HEAD (the last git-commit) has for this note, \
             which changed since.", what ) }

fn changeset_marker_sentence (
  kind : &str,
) -> String {
  match ChangesetMarkerKind::from_wire_name (kind) {
    Some (ChangesetMarkerKind::Content) =>
      "its content changed: it stands for changes to what the note above \
       contains, drawn beside it." . to_string (),
    Some (ChangesetMarkerKind::RelRepo) =>
      "some content relationships changed skgrepo: some of the note's content \
       relationships moved to another skgrepo since HEAD (the last \
       git-commit)." . to_string (),
    None => format! ("No description yet for changesetMarker/{}.", kind), }}

/// '∃+ ∃- M ~+ ~-', and '+ -' on a headline with one diff object
/// ('docs/heralds.org', "Diff heralds").
fn change_fact_sentence (
  path    : &str,
  change  : &str,
  context : &HeadlineContext,
) -> String {
  let added : bool = change == "added";
  let two_objects : bool = context . kind == "node" || context . kind == "ghost";
  match path {
    "diff/properties" =>
      "M: HEAD (the last git-commit) and the worktree both have this note, \
       and its file records a change (to its title, body or another \
       property)." . to_string (),
    "diff/rel" => format! (
      "~{}: {} lacks its relationship to the headline above it: the \
       relationship {} since the last git-commit.",
      if added { "+" } else { "-" },
      if added { "HEAD" } else { "the worktree" },
      if added { "is new" } else { "was removed" } ),
    _ if two_objects => format! (
      "∃{}: {} lacks this note: it {} since the last git-commit.",
      if added { "+" } else { "-" },
      if added { "HEAD" } else { "the worktree" },
      if added { "is new" } else { "was deleted" } ),
    _ => format! (
      "{}: {} lacks this {}: it {} since the last git-commit.",
      if added { "+" } else { "-" },
      if added { "HEAD" } else { "the worktree" },
      match context . kind . as_str () {
        "alias" => "alias", "id" => "ID", "flag" => "flag", _ => "change", },
      if added { "is new" } else { "was removed" } ), }}

/// One '✓' per diff object '(processed ...)' names.
fn processed_sentences (
  objects : &[String],
) -> Vec<Sentence> {
  if objects . is_empty () {
    return vec! [ (None, "✓: its change is marked processed." . to_string ()) ]; }
  objects . iter () . map ( |object| ( None, format! (
    "✓: the changes of {} are marked processed.", diff_object_words (object) )))
    . collect () }

fn processed_request_sentence (
  path    : &str,
  objects : &[String],
) -> String {
  let mark : &str = if path . ends_with ("unprocessed") { "unprocessed" }
                    else { "processed" };
  format! ( "request:{}{}: a pending request: the next save marks {} {}.",
            mark,
            objects . iter () . map ( |o| format! (":{}", o) ) . collect::<String> (),
            if objects . is_empty () { "its changes" . to_string () }
            else { objects . iter () . map ( |o| diff_object_words (o) )
                   . collect::<Vec<&str>> () . join (" and ") },
            mark ) }

fn diff_object_words (
  object : &str,
) -> &'static str {
  match object {
    "node"       => "the note itself",
    "rel"        => "its relationship to the headline above it",
    "newContent" => "the new notes reached through its content",
    "newOutward" => "the new notes reached through its other relationships",
    _            => "an object this client does not know", }}

/// "req:...": a view request the next save carries out.
fn view_request_sentence (
  request : &Sexp,
) -> String {
  let (name, argument) : (String, String) = match request {
    Sexp::List (items) => (
      items . first () . and_then (atom_text) . unwrap_or_default (),
      items . get (1) . and_then (atom_text) . unwrap_or_default () ),
    _ => ( atom_text (request) . unwrap_or_default (), String::new () ), };
  match name . as_str () {
    "folder" => format! (
      "req:folder:{0}: a pending view request: the next save draws its \
       folders of {0}.", argument ),
    "flags" => "req:flags: a pending view request: the next save draws its \
                flags folder." . to_string (),
    "editableView" => "req:editable: a pending view request: the next save \
                       makes it editable here." . to_string (),
    "writeProtectedView" => "req:write-protected: a pending view request: \
      the next save makes it write-protected here, so that its children \
      claim nothing." . to_string (),
    "writeProtectedSubtreeView" => "req:write-protected-subtree: a pending \
      view request: the next save makes it and its whole content subtree \
      write-protected here." . to_string (),
    "fork" => "req:fork: a pending view request: the next save makes a \
               private copy of it that forks it." . to_string (),
    other => format! ("No description yet for viewRequests/{}.", other), }}

//
// Reading the metadata
//

impl<'a> HeadlineContext<'a> {
  fn of (
    parts  : &[Sexp],
    config : &'a SkgConfig,
  ) -> HeadlineContext<'a> {
    let kinds = emittable_kind_atoms ();
    let kind : String = parts . iter ()
      . filter_map ( |part| match part {
        Sexp::List (items) => items . first () . and_then (atom_text),
        _ => atom_text (part), } )
      . find ( |head| kinds . contains (head . as_str ()) )
      . unwrap_or_else ( || "node" . to_string () );
    let view : &[Sexp] = form_tail (parts, "view") . unwrap_or (&[]);
    HeadlineContext {
      folder          : folder_of_atom (&kind),
      node_repo       : form_tail (parts, "skgRepos")
        . and_then ( |repos| form_tail (repos, "nodeRepo") )
        . and_then ( |values| values . first () ) . and_then (atom_text),
      attribution     : form_tail (view, "attribution") . map ( |values| (
        values . first () . and_then (atom_text) . unwrap_or_default (),
        values . iter () . skip (1) . any ( |v| atom_text (v) . as_deref () == Some ("unverified") ) )),
      write_protected : parts . iter () . any ( |part| atom_text (part) . as_deref () == Some ("writeProtected") ),
      birth           : form_tail (view, "birth") . unwrap_or (&[]) . iter ()
        . filter_map ( |fact| match fact {
          Sexp::List (items) => Some ((
            items . first () . and_then (atom_text) ?,
            items . get (1) . and_then (atom_text) ?,
            items . get (2) . and_then ( |g| match g {
              Sexp::Atom (Atom::I (n)) => usize::try_from (*n) . ok (),
              _ => None, } ) )),
          _ => None, } )
        . collect (),
      kind,
      config, } }}

/// The values after the head of the first list in FORMS headed by
/// HEAD, or None.
fn form_tail<'s> (
  forms : &'s [Sexp],
  head  : &str,
) -> Option<&'s [Sexp]> {
  forms . iter () . find_map ( |form| match form {
    Sexp::List (items)
      if items . first () . and_then (atom_text) . as_deref () == Some (head) =>
      Some (&items [1..]),
    _ => None, } ) }

/// The first value of '(HEAD VALUE)' among VALUES, or "?".
fn form_value (
  values : &[Sexp],
  head   : &str,
) -> String {
  form_tail (values, head)
    . and_then ( |tail| tail . first () ) . and_then (atom_text)
    . unwrap_or_else ( || "?" . to_string () ) }

fn first_number (
  values : &[Sexp],
) -> usize {
  values . iter () . find_map ( |v| match v {
    Sexp::Atom (Atom::I (n)) => usize::try_from (*n) . ok (),
    _ => None, } ) . unwrap_or (0) }

/// The distinct ancestor generations of '(ancestors GEN...)' among
/// VALUES, sorted.
fn ancestor_generations (
  values : &[Sexp],
) -> Vec<usize> {
  let mut gens : Vec<usize> = form_tail (values, "ancestors") . unwrap_or (&[])
    . iter () . filter_map ( |v| match v {
      Sexp::Atom (Atom::I (n)) => usize::try_from (*n) . ok (),
      _ => None, } )
    . collect ();
  gens . sort ();
  gens . dedup ();
  gens }

fn atom_text (
  sexp : &Sexp,
) -> Option<String> {
  match sexp {
    Sexp::Atom (Atom::S (s)) => Some ( s . clone () ),
    Sexp::Atom (Atom::I (i)) => Some ( i . to_string () ),
    Sexp::Atom (Atom::F (f)) => Some ( f . to_string () ),
    Sexp::List (_) => None, }}

/// The style of the rule at PATH, or else of the first rule below it
/// that has one.
fn style_of_path (
  path       : &str,
  rule_paths : &BTreeMap<String, Option<&'static str>>,
) -> Option<&'static str> {
  let below : String = format! ("{}/", path);
  rule_paths . get (path) . copied () . flatten ()
    . or_else ( || rule_paths . iter ()
                . filter ( |(p, _)| p . starts_with (&below) )
                . find_map ( |(_, style)| *style )) }

/// Where a sentence of a herald styled STYLE sorts: after the folder's
/// sentence (rank 0), by 'STYLES_MOST_INFORMATIVE_FIRST'.
fn rank_of_style (
  style : Option<&str>,
) -> usize {
  1 + style . and_then ( |s| STYLES_MOST_INFORMATIVE_FIRST . iter ()
                         . position ( |t| *t == s ))
    . unwrap_or ( STYLES_MOST_INFORMATIVE_FIRST . len () ) }

/// The sentences of the first path from the top down to PATH that
/// has them, given the rest of PATH as its values, or None when none
/// does. For the tests, which require that every path the rule table
/// renders have them.
pub fn sentences_covering (
  path : &str,
) -> Option<Vec<String>> {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos ( Default::default () );
  let context : HeadlineContext = HeadlineContext::of (&[], &config);
  let steps : Vec<&str> = path . split ('/') . collect ();
  ( 1 ..= steps . len () ) . find_map ( |n| {
    let values : Vec<Sexp> = steps [n..] . iter ()
      . map ( |step| Sexp::Atom ( Atom::S ( step . to_string () )) )
      . collect ();
    sentences_for_path ( &steps [..n] . join ("/"), &values, &context ) } )
    . map ( |sentences| sentences . into_iter ()
            . map ( |(_, sentence)| sentence ) . collect () ) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../tests/unit/herald_descriptions.rs"]
mod tests;
