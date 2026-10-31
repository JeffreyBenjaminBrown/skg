/// PURPOSE: 'shared/metadata-fields.json', the metadata fields a user
/// may set on each kind of headline, as the server's parser reads it.
/// Both clients' metadata editors read the same file, so the parser
/// and the editors agree on which kind takes which field, and which
/// values, by construction. The file's own comments describe it;
/// 'tests/metadata_fields_table.rs' checks it against the parser and
/// the printer.
///
/// A kind is known by its atom. A field the table does not list for
/// a kind is one the kind does not take, and a value it does not list
/// for a field is one the field does not take there. The table lists
/// only what a user sets; the parts the server writes (the gnid, the
/// skgrepos, 'overridesHere', the derived facts) and the readiness
/// request are the parser's to place ('parse_metadata_sexp.rs').

use serde::Deserialize;
use std::sync::LazyLock;

/// The file, embedded at compile time.
const METADATA_FIELDS_JSON : &str =
  include_str! ("../../shared/metadata-fields.json");

static METADATA_FIELDS_TABLE : LazyLock<MetadataFieldsTable> =
  LazyLock::new ( || serde_json::from_str (METADATA_FIELDS_JSON)
                  . expect ( "shared/metadata-fields.json should parse as the table of metadata fields" ));

/// The part of the file the parser reads: its kinds.
#[derive(Deserialize)]
struct MetadataFieldsTable {
  kinds : Vec<KindOfTable>,
}

/// One kind of headline: the atoms it is known by, and its fields.
#[derive(Deserialize)]
struct KindOfTable {
  atoms  : Vec<String>,
  fields : Vec<FieldOfTable>,
}

/// One field of a kind, and the values it takes there.
#[derive(Deserialize)]
struct FieldOfTable {
  name   : String,
  values : Vec<ValueOfTable>,
}

#[derive(Deserialize)]
struct ValueOfTable {
  value     : String,
  #[serde(default)]
  arguments : Vec<ArgumentOfTable>,
}

/// An argument of a value: 'of' an id or a skgrepo, or 'one_of' a
/// fixed list.
#[derive(Deserialize)]
struct ArgumentOfTable {
  of     : Option<String>,
  one_of : Option<Vec<String>>,
}

/// The field 'field_name' of the kind known by 'kind_atom', if the
/// table lists it there.
fn field_of_kind (
  kind_atom  : &str,
  field_name : &str,
) -> Option<&'static FieldOfTable> {
  METADATA_FIELDS_TABLE . kinds . iter ()
    . find ( |kind| kind . atoms . iter () . any ( |atom| atom == kind_atom ))
    . and_then ( |kind| kind . fields . iter ()
                 . find ( |field| field . name == field_name )) }

/// Whether the kind known by 'kind_atom' takes the field
/// 'field_name'.
pub fn kind_takes_field (
  kind_atom  : &str,
  field_name : &str,
) -> bool {
  field_of_kind (kind_atom, field_name) . is_some () }

/// Whether the field 'field_name' of the kind known by 'kind_atom'
/// takes the value 'value'.
pub fn kind_takes_value (
  kind_atom  : &str,
  field_name : &str,
  value      : &str,
) -> bool {
  field_of_kind (kind_atom, field_name) . is_some_and (
    |field| field . values . iter () . any ( |offered| offered . value == value )) }

/// The values the field 'field_name' of the kind known by 'kind_atom'
/// takes, as a message writes them: a bare value as it is, one with
/// arguments as a form with a placeholder for each ('(relRepo R)').
pub fn values_written_for_a_message (
  kind_atom  : &str,
  field_name : &str,
) -> Vec<String> {
  field_of_kind (kind_atom, field_name) . map_or_else (
    Vec::new,
    |field| field . values . iter ()
      . map ( |value|
              if value . arguments . is_empty () { value . value . clone () }
              else { format! ( "({} {})",
                               value . value,
                               value . arguments . iter ()
                                 . map (argument_placeholder)
                                 . collect::<Vec<String>> () . join (" ") ) } )
      . collect () ) }

/// What a message writes for an argument: 'R' for a skgrepo, 'ID' for
/// an id, the choices of a fixed list joined by '|'.
fn argument_placeholder (
  argument : &ArgumentOfTable,
) -> String {
  match ( argument . of . as_deref (), &argument . one_of ) {
    ( Some ("skgrepo"), _ ) => "R" . to_string (),
    ( Some ("id"), _ )      => "ID" . to_string (),
    ( _, Some (choices) )   => choices . join ("|"),
    ( Some (other), None )  => other . to_uppercase (),
    ( None, None )          => "X" . to_string (), } }
