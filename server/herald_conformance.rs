//! PURPOSE: The checks that the herald rule table ('heralds.rs'), the
//! metadata vocabulary the server emits, and 'shared/herald-styles.json'
//! agree. 'herald_styles_inconsistencies' is asserted at startup
//! ('main.rs'); the conformance tests ('tests/unit/heralds.rs') demand
//! that every atom 'emittable_metadata_atoms' lists has a rule and
//! every rule a live atom. 'herald_descriptions.rs' reads the kind
//! atoms ('emittable_kind_atoms').

use crate::heralds::{
  herald_styles, node_heralds_keys_in_rule_table,
  property_and_change_kind_atoms, rules_in_rule_table};
use crate::types::nodes::complete::Flag;
use crate::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, DiffChange, Folder, GraphnodeStats,
  OtherChange, DiffObject, ViewRequest, ViewnodeStats};

use serde_json::Value;
use std::collections::HashSet;

//
// Consistency of shared/herald-styles.json
//

/// Every way 'shared/herald-styles.json' disagrees with itself or with
/// the rule table. Run as a unit test, and asserted at startup.
pub fn herald_styles_inconsistencies () -> Vec<String> {
  herald_styles_inconsistencies_of ( herald_styles () ) }

/// Every tier file names (in 'tier_order', 'node_heralds' or
/// 'relationship_heralds') that is not a key of its 'styles', every
/// styled atom of the rule table that has no entry in its
/// 'node_heralds', and every way its 'folder_heralds' disagrees with
/// the rule table ('folder_heralds_inconsistencies').
pub fn herald_styles_inconsistencies_of (
  file : &Value,
) -> Vec<String> {
  let styles : HashSet<&str> =
    file ["styles"] . as_object ()
    . map ( |styles| styles . keys () . map (String::as_str) . collect () )
    . unwrap_or_default ();
  let undefined_tiers : Vec<String> =
    tiers_named_in_herald_styles (file) . into_iter ()
    . filter ( |(_, tier)| ! styles . contains (tier) )
    . map ( |(place, tier)| format! (
      "{} names {}, which is not a key of 'styles'", place, tier ))
    . collect ();
  let unstyled_atoms : Vec<String> =
    node_heralds_keys_in_rule_table () . into_iter ()
    . filter ( |key| file ["node_heralds"] [*key] . as_str () . is_none () )
    . map ( |key| format! (
      "the rule table styles {}, which has no entry in 'node_heralds'",
      key ))
    . collect ();
  [ undefined_tiers, unstyled_atoms,
    folder_heralds_inconsistencies (file) ] . concat () }

/// 'folder_heralds.write_protected' must name exactly the folders whose
/// label the rule table gives a ☮, and 'folder_heralds.changed_labels'
/// must give a string for folders only.
fn folder_heralds_inconsistencies (
  file : &Value,
) -> Vec<String> {
  let folder_atoms : HashSet<&str> =
    Folder::all () . iter () . map ( |folder| folder . repr_in_client () ) . collect ();
  let listed : HashSet<&str> =
    file ["folder_heralds"] ["write_protected"] . as_array ()
    . map ( |atoms| atoms . iter () . filter_map (Value::as_str) . collect () )
    . unwrap_or_default ();
  let protected_in_table : HashSet<&str> =
    rules_in_rule_table () . into_iter ()
    . filter ( |rule| rule . node_heralds_key == Some ("writeProtected") )
    . filter_map ( |rule| rule . label )
    . filter ( |label| folder_atoms . contains (label) )
    . collect ();
  let mut problems : Vec<String> = Vec::new ();
  for atom in protected_in_table . difference (&listed) {
    problems . push ( format! (
      "the rule table gives {} a ☮, which 'folder_heralds.write_protected' omits",
      atom )); }
  for atom in listed . difference (&protected_in_table) {
    problems . push ( format! (
      "'folder_heralds.write_protected' names {}, whose label the rule table gives no ☮",
      atom )); }
  if let Some (labels) = file ["folder_heralds"] ["changed_labels"] . as_object () {
    for (atom, label) in labels {
      if atom . ends_with ("comment") { continue; }
      if ! folder_atoms . contains (atom . as_str ()) {
        problems . push ( format! (
          "'folder_heralds.changed_labels' names {}, which is no folder", atom )); }
      if ! label . is_string () {
        problems . push ( format! (
          "'folder_heralds.changed_labels' gives {} no string", atom )); }}}
  problems . sort ();
  problems }

/// Each tier file names, with the path where it does so.
fn tiers_named_in_herald_styles (
  file : &Value,
) -> Vec<(String, &str)> {
  fn collect<'a> (
    place : String,
    value : &'a Value,
    out   : &mut Vec<(String, &'a str)>,
  ) {
    match value {
      Value::String (tier) => out . push (( place, tier . as_str () )),
      Value::Array (tiers) =>
        for (i, tier) in tiers . iter () . enumerate () {
          collect ( format! ("{}[{}]", place, i), tier, out ); },
      Value::Object (entries) =>
        for (key, entry) in entries {
          if ! key . ends_with ("comment") {
            collect ( format! ("{}.{}", place, key), entry, out ); }},
      _ => () }}
  let mut out : Vec<(String, &str)> = Vec::new ();
  for field in [ "tier_order", "node_heralds", "relationship_heralds" ] {
    collect ( field . to_string (), &file [field], &mut out ); }
  out }

//
// Vocabulary enumeration, for the conformance test
//

/// Every match atom in the rule table (labels of rules, recursively,
/// and the kinds of WHEN rules), excluding the engine directives (ANY;
/// IT is not a label) and the root label "skg".
pub fn atoms_in_rule_table () -> std::collections::HashSet<&'static str> {
  rules_in_rule_table () . into_iter ()
    . flat_map ( |rule| rule . label . into_iter () . chain ( rule . when ))
    . filter ( |label| *label != "ANY" && *label != "skg" )
    . collect () }

/// Every metadata atom the server can emit in a match (label)
/// position. Value-position data (counts, IDs, skgrepo names)
/// is consumed by ANY/IT
/// rules and so is deliberately absent.
///
/// Each component is derived from the type that owns it; the
/// exhaustiveness guards below make adding an enum variant or struct
/// field a compile error here until this list learns the new atom.
pub fn emittable_metadata_atoms () -> std::collections::HashSet<&'static str> {
  let mut atoms : Vec<&'static str> = vec! [
    // Bare buffer-position atoms, from org_to_text.rs:
    "focused", "folded", "bodyFolded",
    // The viewnode ID that starts every wrapper; its value is value position.
    "vnid",
    // Kind atoms, from org_to_text.rs:
    "node", "unknown", "restricted",
    "tombstone", "ghost",
    // The parts beside them:
    "gnid", "skgRepos", "nodeRepo",
    "writeProtected",
    // The two forms of derived facts.
    "view", "diff",
    // The assembled relationship-herald forms inside '(view ...)'
    // (server/herald_tokens.rs); their sub-forms are value position,
    // consumed by the client's renderer, so they are not match atoms.
    "rels", "props", "birth",
    "editRequest", "viewRequests",
    // The processed requests inside '(editRequest ...)'.
    "processed", "unprocessed",
    // NodeEditRequest atoms. The flag form's name and desired value
    // are matched literally so its herald can describe the complete state
    // change rather than echoing two context-free arguments.
    "delete", "merge", "flag", "true", "false", "stripAttribution",
    // The diff view's restore requests, on a ghost and on
    // a removed membership.
    "restoreNode", "restoreRel",
  ];
  // Flag-viewnode heralds match every public wire name.  Keeping this derived
  // from the registry makes adding a flag a conformance-checked change.
  atoms . extend (Flag::ALL . map (Flag::wire_name));
  atoms . extend ( graphstats_atoms () );
  atoms . extend ( viewstats_atoms () );
  atoms . extend ( claim_emitted_atoms () );
  atoms . extend ( property_and_folder_atoms () );
  atoms . extend ( ViewRequest::EMITTABLE_MATCH_ATOMS );
  atoms . extend ( change_fact_atoms () );
  atoms . extend ( client_computed_atoms () );
  atoms . into_iter () . collect () }

/// Atoms no wire carries, which the clients write into a headline's
/// metadata before they hand it to the lens: 'pendingDetachment'
/// ('pending_detachment_rule').
fn client_computed_atoms (
) -> Vec<&'static str> {
  vec! [ "pendingDetachment" ] }

/// Every kind atom the server writes: the kinds a WHEN may name.
pub fn emittable_kind_atoms (
) -> HashSet<&'static str> {
  [ "node", "ghost", "unknown", "restricted", "tombstone" ] . into_iter ()
    . chain ( property_and_change_kind_atoms () )
    . chain ( Folder::all () . iter () . map ( |folder| folder . repr_in_client () ))
    . collect () }

/// The kinds every WHEN rule in the table names.
pub fn when_kinds_in_rule_table (
) -> HashSet<&'static str> {
  rules_in_rule_table () . into_iter ()
    . flat_map ( |rule| rule . when )
    . collect () }

/// The diff view's form, from org_to_text.rs's 'change_fact_forms':
/// its head, the heads of its parts and their atoms, and the diff
/// objects.
fn change_fact_atoms () -> Vec<&'static str> {
  fn guard ( facts : ChangeFacts, object : DiffObject ) {
    let ChangeFacts {
      change : _,   // -> node, rel, properties, with DiffChange::EMITTABLE_ATOMS
      processed : _, // -> processed, with the objects
      leaves : _,   // -> leaves, with the objects
      home_skgrepo_move : _,   // -> the homeSkgrepoMove atom, below
      moved_like_ancestor : _, // -> the movedLikeAncestor atom, below
    } = facts;
    match object { // compile error here = update DiffObject::ALL
      DiffObject::Node | DiffObject::Rel
        | DiffObject::NewContent | DiffObject::NewOutward => () }}
  let _ = guard;
  [ vec! [ "diff", "processed", "leaves",
           "homeSkgrepoMove", "movedLikeAncestor" ],
    DiffChange::EMITTABLE_ATOMS . to_vec (),
    DiffObject::ALL . into_iter () . map (DiffObject::wire_name)
      . collect () ] . concat () }

/// GraphnodeStats emits no match atoms: its counts feed semantic
/// '(rels ...)' facts. The
/// destructuring pattern is the exhaustiveness guard -- a new field
/// fails to compile here until it is accounted for.
fn graphstats_atoms () -> Vec<&'static str> {
  fn guard ( g : GraphnodeStats ) {
    let GraphnodeStats {
      aliases : _,    // -> Ak, inside semantic rels metadata
      extra_ids : _,  // -> Ik, inside semantic rels metadata
      flags : _, // -> Fk, inside semantic rels metadata
      rels : _,       // -> relationship facts inside semantic rels metadata
    } = g; }
  let _ = guard;
  vec! [] }

/// The match atoms of ViewnodeStats, from
/// occurrence_metadata_to_string (org_to_text.rs): the
/// facts of '(view ...)', the relRepo of '(skgRepos ...)', and the
/// occurrence's 'inPlaceOf'; and the property members' warrant
/// atoms. The relationship facts are the forms named in
/// 'emittable_metadata_atoms'.
fn viewstats_atoms () -> Vec<&'static str> {
  fn guard ( v : ViewnodeStats ) {
    let ViewnodeStats {
      cycle : _,
      repoBoundary : _,     // -> the repoBoundary atom
      warrant : _,      // -> the noWarrant atom
      rel_heralds : _,      // -> the rels, props and birth forms
      omitted_body : _,     // -> the omittedBody atom
      relRepo : _,          // -> the relRepo atom and herald
      attribution : _,      // -> the attribution form, folded into repoBoundary's herald
    } = v; }
  let _ = guard;
  vec! [ "cycle", "repoBoundary", "noWarrant", "omittedBody",
         "inPlaceOf", "relRepo",
         // The attribution form; its 'unverified' rides inside it,
         // which the form's rule consumes whole.
         "attribution",
         // A ghost warrant ('Warrant::ByHead').
         "ghostWarrant" ] }

/// The claim atom the serializer can emit: 'noClaim' on an occurrence,
/// an alias, an id, an unknown viewnode or a restricted viewnode that
/// does not claim membership (one that claims carries nothing).
fn claim_emitted_atoms () -> Vec<&'static str> {
  vec! [ "noClaim" ] }

/// Folder and Property atoms, via the same repr_in_client constants the
/// serializer uses.
fn property_and_folder_atoms () -> Vec<&'static str> {
  // The arguments the diff view's properties take in match
  // position (org_to_text.rs's 'property_metadata_to_string').
  let property_argument_atoms : Vec<&'static str> =
    [ vec! [ "titleAndBody", "title", "body", "old", "new" ],
      ChangesetMarkerKind::ALL . into_iter ()
        . map (ChangesetMarkerKind::wire_name) . collect (),
      OtherChange::ALL . into_iter ()
        . map (OtherChange::wire_name) . collect () ] . concat ();
  let mut out : Vec<&'static str> = Vec::new ();
  out . extend ( Folder::all () . iter ()
                 . map ( |c| c . repr_in_client () ) );
  out . extend ( property_and_change_kind_atoms () );
  out . extend ( property_argument_atoms );
  out }
