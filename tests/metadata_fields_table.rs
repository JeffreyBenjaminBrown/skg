// cargo nextest run --test grouped_unit -E 'test(metadata_fields_table::)'
//
// 'shared/metadata-fields.json', which both clients' metadata editors
// read, against the parser ('server/serve/parse_metadata_sexp.rs'), as
// 'relations_match_the_shared_relations_file' checks
// 'shared/relations.json' against 'NodeRelation': the table names
// every kind the parser knows; each value it offers parses where it
// offers it, under the conditions it states and no others; nothing it
// does not offer parses; and its values name every edit request and
// view request the parser knows.

use skg::serve::parse_metadata_sexp::{
  ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use skg::types::misc::{ID, SkgrepoChange, SkgrepoName};
use skg::types::nodes::complete::Flag;
use skg::types::viewnode::{
  ChangesetMarkerKind, FolderRelation, NodeEditRequest,
  Folder, PropertyMember, ChangeViewnode, ViewRequest};

use serde_json::Value;
use std::collections::BTreeSet;

/// The parsed table.
fn metadata_fields_table (
) -> Value {
  let path : std::path::PathBuf =
    std::path::Path::new ( env! ("CARGO_MANIFEST_DIR") )
    . join ("shared/metadata-fields.json");
  serde_json::from_str ( &std::fs::read_to_string (&path) . unwrap () )
    . unwrap () }

fn kinds (
  table : &Value,
) -> &Vec<Value> {
  table ["kinds"] . as_array () . unwrap () }

fn strings_of (
  value : &Value,
) -> Vec<String> {
  value . as_array () . unwrap () . iter ()
    . map ( |item| item . as_str () . unwrap () . to_string () )
    . collect () }

/// The head atom of every kind the parser reads. The matches make a
/// new variant a compile error here until it is listed.
fn every_kind_atom (
) -> BTreeSet<String> {
  let change : SkgrepoChange =
    SkgrepoChange { old : SkgrepoName::from ("a"),
                    new : SkgrepoName::from ("b") };
  let members : Vec<PropertyMember> = vec! [
    PropertyMember::Alias { text : String::new (), relRepo : None,
                      relationshipEditRequest : None },
    PropertyMember::ID { skgid : ID::default () },
    PropertyMember::Flag { flag : Flag::NoSearchMatching,
                     title : String::new (), body : None } ];
  for member in &members {
    match member {
      PropertyMember::Alias { .. } | PropertyMember::ID { .. }
        | PropertyMember::Flag { .. } => (), } }
  let changes : Vec<ChangeViewnode> = vec! [
    ChangeViewnode::OldText { title : None, body : None },
    ChangeViewnode::BodyDiff { diff : String::new () },
    ChangeViewnode::HomeSkgrepoMove { skgrepo_change : change . clone () },
    ChangeViewnode::RelRepoMove { skgrepo_change : change },
    ChangeViewnode::ChangesetMarker { kind : ChangesetMarkerKind::Content } ];
  for change in &changes {
    match change {
      ChangeViewnode::OldText { .. }
        | ChangeViewnode::BodyDiff { .. } | ChangeViewnode::HomeSkgrepoMove { .. }
        | ChangeViewnode::RelRepoMove { .. }
        | ChangeViewnode::ChangesetMarker { .. } => (), } }
  let folders : Vec<Folder> = vec! [
    Folder::Alias, Folder::ID, Folder::flags (),
    Folder::Subscribee, Folder::Subscriber,
    Folder::Overridden, Folder::Overrider,
    Folder::Hider, Folder::Hidden,
    Folder::HiddenInSubscribee,
    Folder::HiddenOutsideOfSubscribee,
    Folder::Container, Folder::Mentioner,
    Folder::AcquiredBy, Folder::Acquired ];
  for folder in &folders {
    match folder {
      Folder::Alias | Folder::ID | Folder::Flags { .. }
        | Folder::Subscribee | Folder::Subscriber
        | Folder::Overridden | Folder::Overrider
        | Folder::Hider | Folder::Hidden
        | Folder::HiddenInSubscribee
        | Folder::HiddenOutsideOfSubscribee
        | Folder::Container | Folder::Mentioner
        | Folder::AcquiredBy | Folder::Acquired => (), } }
  [ "node", "ghost", "unknown",
    "tombstone", "forestRoot" ]
    . iter () . map ( |atom| atom . to_string () )
    . chain ( members . iter ()
              . map ( |m| m . repr_in_client () . to_string () ))
    . chain ( changes . iter ()
              . map ( |c| c . repr_in_client () . to_string () ))
    . chain ( folders . iter ()
              . map ( |f| f . repr_in_client () . to_string () ))
    . collect () }

/// The wire name of each edit request. The match makes a new variant
/// a compile error here until it is named.
fn edit_request_wire_name (
  request : &NodeEditRequest,
) -> &'static str {
  match request {
    NodeEditRequest::NodeMerge (_)   => "merge",
    NodeEditRequest::Delete          => "delete",
    NodeEditRequest::SetFlag { .. }  => "flag",
    NodeEditRequest::RestoreNode     => "restoreNode",
    NodeEditRequest::RestoreRel      => "restoreRel", } }

fn every_edit_request (
) -> Vec<NodeEditRequest> {
  vec! [ NodeEditRequest::NodeMerge ( ID::from ("b") ),
         NodeEditRequest::Delete,
         NodeEditRequest::SetFlag { flag : Flag::NoSearchMatching,
                                    value : true },
         NodeEditRequest::RestoreNode,
         NodeEditRequest::RestoreRel ] }

/// The wire name of each view request, as the parser reads it. The
/// match makes a new variant a compile error here until it is named.
fn view_request_wire_name (
  request : &ViewRequest,
) -> &'static str {
  match request {
    ViewRequest::Folder (_)             => "folder",
    ViewRequest::Flags                  => "flags",
    ViewRequest::Editable               => "editableView",
    ViewRequest::WriteProtected         => "writeProtectedView",
    ViewRequest::WriteProtectedSubtree  => "writeProtectedSubtreeView",
    ViewRequest::Fork                   => "fork",
    ViewRequest::Tombstone              => "tombstone", } }

fn every_view_request (
) -> Vec<ViewRequest> {
  vec! [ ViewRequest::Folder (FolderRelation::Aliases),
         ViewRequest::Flags, ViewRequest::Editable,
         ViewRequest::WriteProtected, ViewRequest::WriteProtectedSubtree,
         ViewRequest::Fork, ViewRequest::Tombstone ] }

/// A minimal headline metadata of the kind whose head atom is 'atom'.
fn example_of_kind (
  atom : &str,
) -> String {
  match atom {
    "node"                 => "(skg node (gnid a) (skgRepos (nodeRepo r)))" . to_string (),
    "ghost"                => "(skg ghost (gnid a) (skgRepos (nodeRepo r)))" . to_string (),
    "unknown"              => "(skg unknown (gnid a))" . to_string (),
    "flag"                 => "(skg (flag noSearchMatching))" . to_string (),
    "oldText"              => "(skg (oldText title))" . to_string (),
    "homeSkgrepoMove" | "relRepoMove" =>
      format! ( "(skg ({} (old a) (new b)))", atom ),
    "changesetMarker"      => "(skg (changesetMarker content))" . to_string (),
    _                      => format! ( "(skg {})", atom ), } }

/// 'example' with 'field' inserted beside the kind, at the top level
/// of its '(skg ...)', where every part stands.
fn with_field (
  example   : &str,
  field     : &str,
) -> String {
  let (opening, closing) : (&str, &str) =
    example . split_at ( example . len () - 1 );
  format! ( "{} {}{}", opening, field, closing ) }

/// The metadata 'sexp' parses, and becomes a viewnode without error.
fn parse_cleanly (
  sexp : &str,
) -> Result<ViewnodeMetadata, String> {
  let metadata : ViewnodeMetadata = parse_metadata_to_viewnodemd (sexp) ?;
  let (_, error, _) = viewnode_from_metadata (&metadata, String::new (), None);
  match error {
    Some (error) => Err ( format! ( "{:?}", error )),
    None => Ok (metadata), } }

/// One way to write a value of a field: its text in the metadata, and
/// what the table says of it.
#[derive(Debug, Clone)]
struct WrittenValue {
  field_name : String,
  value      : String,
  when       : Option<String>,
  text       : String,
}

/// The sample argument for an argument of the table.
fn argument_samples (
  argument : &Value,
) -> Vec<String> {
  match ( argument . get ("of") . and_then (Value::as_str),
          argument . get ("one_of") ) {
    ( Some ("id"), _ )      => vec! [ "b" . to_string () ],
    ( Some ("skgrepo"), _ ) => vec! [ "r" . to_string () ],
    ( None, Some (choices) ) => strings_of (choices),
    other => panic! ( "unknown argument {:?}", other ), } }

/// Every way to write each value of 'field': one per combination of
/// its arguments' samples.
fn written_values (
  field : &Value,
) -> Vec<WrittenValue> {
  let name : &str = field ["name"] . as_str () . unwrap ();
  let shape : &str = field ["shape"] . as_str () . unwrap ();
  field ["values"] . as_array () . unwrap () . iter ()
    . flat_map ( |value| {
      let word : String = value ["value"] . as_str () . unwrap () . to_string ();
      let when : Option<String> =
        value . get ("when") . and_then (Value::as_str) . map (String::from);
      let mut combinations : Vec<Vec<String>> = vec! [ vec! [] ];
      for argument in value . get ("arguments") . and_then (Value::as_array)
                      . cloned () . unwrap_or_default () {
        combinations = combinations . into_iter ()
          . flat_map ( |prefix| argument_samples (&argument) . into_iter ()
                       . map ( move |sample| {
                         let mut longer : Vec<String> = prefix . clone ();
                         longer . push (sample);
                         longer } ))
          . collect (); }
      combinations . into_iter () . map ( move |arguments| {
        let written_value : String =
          if arguments . is_empty () { word . clone () }
          else { format! ( "({} {})", word, arguments . join (" ") ) };
        WrittenValue {
          field_name : name . to_string (),
          value      : word . clone (),
          when       : when . clone (),
          text       : if shape == "atom" { name . to_string () }
                       else { format! ( "({} {})", name, written_value ) }, } } )
        . collect::<Vec<WrittenValue>> () } )
    . collect () }

fn fields_of (
  kind : &Value,
) -> &Vec<Value> {
  kind ["fields"] . as_array () . unwrap () }

#[test]
fn the_table_names_every_kind_the_parser_knows () {
  let table : Value = metadata_fields_table ();
  let mut from_table : Vec<String> =
    kinds (&table) . iter ()
    . flat_map ( |kind| strings_of (&kind ["atoms"]) ) . collect ();
  let count : usize = from_table . len ();
  from_table . sort ();
  from_table . dedup ();
  assert_eq! ( count, from_table . len (), "an atom is listed twice" );
  assert_eq! ( from_table . into_iter () . collect::<BTreeSet<String>> (),
               every_kind_atom () );
  for atom in every_kind_atom () {
    let example : String = example_of_kind (&atom);
    assert! ( parse_cleanly (&example) . is_ok (),
              "{}: {:?}", example, parse_cleanly (&example) ); } }

#[test]
fn each_offered_value_parses_under_its_conditions_and_no_others () {
  let table : Value = metadata_fields_table ();
  for kind in kinds (&table) {
    let has_writeProtected : bool = fields_of (kind) . iter ()
      . any ( |field| field ["name"] == "writeProtected" );
    for atom in strings_of (&kind ["atoms"]) {
      let example : String = example_of_kind (&atom);
      for field in fields_of (kind) {
        for written in written_values (field) {
          let removed_member : bool =
            written . when . as_deref () == Some ("removedMember");
          let base : String =
            if removed_member {
              with_field (&example, "(diff (rel removed))") }
            else { example . clone () };
          let sexp : String = with_field (&base, &written . text);
          assert! ( parse_cleanly (&sexp) . is_ok (),
                    "{}: {:?}", sexp, parse_cleanly (&sexp) );
          if removed_member {
            let elsewhere : String =
              with_field (&example, &written . text);
            assert! ( parse_cleanly (&elsewhere) . is_err (),
                      "{} parsed without '(rel removed)'", elsewhere ); }
          if has_writeProtected && written . field_name != "writeProtected" {
            let write_protected : String =
              with_field (&sexp, "writeProtected");
            assert_eq! (
              parse_cleanly (&write_protected) . is_err (),
              written . when . as_deref () == Some ("editable"),
              "{}: {:?}", write_protected,
              parse_cleanly (&write_protected) ); } } } } } }

#[test]
fn nothing_the_table_does_not_offer_parses () {
  let table : Value = metadata_fields_table ();
  let every_written_value : Vec<WrittenValue> =
    kinds (&table) . iter ()
    . flat_map ( |kind| fields_of (kind) . iter () . flat_map (written_values) )
    . collect ();
  for kind in kinds (&table) {
    let offered : BTreeSet<(String, String)> =
      fields_of (kind) . iter () . flat_map (written_values)
      . map ( |written| ( written . field_name, written . value ))
      . collect ();
    for atom in strings_of (&kind ["atoms"]) {
      let example : String = example_of_kind (&atom);
      let bases : Vec<String> =
        vec! [ example . clone (),
               with_field (&example, "(diff (rel removed))") ];
      for written in &every_written_value {
        if offered . contains ( &( written . field_name . clone (),
                                   written . value . clone () )) {
          continue; }
        for base in &bases {
          let sexp : String = with_field (base, &written . text);
          assert! ( parse_cleanly (&sexp) . is_err (),
                    "{} parsed, but the table does not offer {} {} there",
                    sexp, written . field_name, written . value ); } } } } }

#[test]
fn the_values_name_every_request_the_parser_knows () {
  let table : Value = metadata_fields_table ();
  let node : &Value = kinds (&table) . iter ()
    . find ( |kind| strings_of (&kind ["atoms"]) == vec! [ "node" ] )
    . unwrap ();
  let mut edit_request_values : BTreeSet<String> = BTreeSet::new ();
  let mut view_request_values : BTreeSet<String> = BTreeSet::new ();
  for kind in kinds (&table) {
    for field in fields_of (kind) {
      let name : &str = field ["name"] . as_str () . unwrap ();
      let shape : &str = field ["shape"] . as_str () . unwrap ();
      let default : &str = field ["default"] . as_str () . unwrap ();
      assert_eq! ( default, if shape == "atom" { "false" } else { "none" } );
      for value in field ["values"] . as_array () . unwrap () {
        let word : String = value ["value"] . as_str () . unwrap () . to_string ();
        match name {
          "editRequest"  => { edit_request_values . insert (word); },
          "viewRequests" => { view_request_values . insert (word); },
          "writeProtected" | "noClaim" => assert_eq! ( word, "true" ),
          other => panic! ( "unknown field {}", other ), } } } }
  let mut expected_edit_request_values : BTreeSet<String> =
    every_edit_request () . iter ()
    . map ( |request| edit_request_wire_name (request) . to_string () )
    . collect ();
  expected_edit_request_values . insert ( "relRepo" . to_string () );
  assert_eq! ( edit_request_values, expected_edit_request_values );
  assert_eq! ( view_request_values,
               every_view_request () . iter ()
               . map ( |request| view_request_wire_name (request) . to_string () )
               . collect::<BTreeSet<String>> () );
  for field in fields_of (node) {
    for value in field ["values"] . as_array () . unwrap () {
      let arguments : Vec<Value> = value . get ("arguments")
        . and_then (Value::as_array) . cloned () . unwrap_or_default ();
      let one_ofs : Vec<Vec<String>> = arguments . iter ()
        . filter_map ( |argument| argument . get ("one_of") . map (strings_of) )
        . collect ();
      match value ["value"] . as_str () . unwrap () {
        "flag" => assert_eq! ( one_ofs, vec! [
          Flag::ALL . iter () . filter ( |flag| flag . is_mutable () )
            . map ( |flag| flag . wire_name () . to_string () ) . collect (),
          vec! [ "true" . to_string (), "false" . to_string () ] ] ),
        "folder" => assert_eq! ( one_ofs, vec! [
          FolderRelation::ALL . iter ()
            . map ( |relation| relation . relname () . to_string () )
            . collect::<Vec<String>> () ] ),
        _ => assert! ( one_ofs . is_empty () ), } } } }

#[test]
fn each_node_value_parses_to_what_it_names () {
  let table : Value = metadata_fields_table ();
  let node : &Value = kinds (&table) . iter ()
    . find ( |kind| strings_of (&kind ["atoms"]) == vec! [ "node" ] )
    . unwrap ();
  let example : String = example_of_kind ("node");
  let absent : ViewnodeMetadata = parse_cleanly (&example) . unwrap ();
  assert! ( ! absent . writeProtected );
  assert_eq! ( absent . claimsMembership, true );
  assert! ( absent . edit_request . is_none ()
            && absent . relRepo_request . is_none () );
  assert! ( absent . view_requests . is_empty () );
  for field in fields_of (node) {
    for written in written_values (field) {
      let base : String = with_field (&example, "(diff (rel removed))");
      let parsed : ViewnodeMetadata =
        parse_cleanly ( &with_field (&base, &written . text) )
        . unwrap ();
      match written . field_name . as_str () {
        "writeProtected" => assert! ( parsed . writeProtected ),
        "noClaim" => assert_eq! ( parsed . claimsMembership, false ),
        "editRequest" if written . value == "relRepo" =>
          assert! ( parsed . relRepo_request . is_some () ),
        "editRequest" => assert_eq! (
          parsed . edit_request . as_ref () . map (edit_request_wire_name),
          Some ( written . value . as_str () )),
        "viewRequests" => assert_eq! (
          parsed . view_requests . iter () . map (view_request_wire_name)
            . collect::<Vec<&str>> (),
          vec! [ written . value . as_str () ] ),
        other => panic! ( "unknown field {}", other ), } } } }
