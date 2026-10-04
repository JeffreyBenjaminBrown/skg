use super::{
  FileProperty, NodeComplete, empty_node_complete,
  file_property_is_true, set_file_property};
use crate::types::misc::ID;

fn node_with_ids (
  pid : &str,
  extra_ids : &[&str],
) -> NodeComplete {
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = ID::from (pid);
  node . extra_ids = extra_ids . iter ()
    . map ( |id| ID::from (*id) )
    . collect ();
  node
}

#[test]
fn normalize_ids_treats_the_pid_as_first_and_keeps_first_extra_order () {
  let mut node : NodeComplete =
    node_with_ids ("P", &["B", "P", "A", "B", "C", "A"]);
  node . normalize_ids ();
  assert_eq! (
    node . extra_ids,
    vec![ID::from ("B"), ID::from ("A"), ID::from ("C")]);
}

#[test]
fn normalize_ids_is_idempotent () {
  let mut node : NodeComplete =
    node_with_ids ("P", &["A", "A", "P", "B"]);
  node . normalize_ids ();
  let once : Vec<ID> = node . extra_ids . clone ();
  node . normalize_ids ();
  assert_eq! (node . extra_ids, once);
}

#[test]
fn normalize_ids_leaves_an_already_normal_list_alone () {
  let mut node : NodeComplete =
    node_with_ids ("P", &["A", "B", "C"]);
  let before : Vec<ID> = node . extra_ids . clone ();
  node . normalize_ids ();
  assert_eq! (node . extra_ids, before);
}

#[test]
fn file_property_registry_has_stable_public_names_and_order () {
  assert_eq! (
    FileProperty::ALL . map (FileProperty::wire_name),
    ["hadId", "wasOverloaded", "noSearchMatching"] );
  assert_eq! (
    FileProperty::ALL . map (FileProperty::herald_text),
    ["☮ had ID before import",
     "☮ was overloaded during org-roam import",
     "☮ no search matching"] );
  for property in FileProperty::ALL {
    assert_eq! (
      FileProperty::from_wire_name (property . wire_name ()),
      Some (property)); }
  assert_eq! (FileProperty::from_wire_name ("NoIndex"), None);
  assert! (! FileProperty::Had_ID_Before_Import . is_mutable ());
  assert! (! FileProperty::Was_Overloaded . is_mutable ());
  assert! (FileProperty::NoSearchMatching . is_mutable ());
}

#[test]
fn setting_one_file_property_preserves_the_others_and_repairs_duplicates () {
  let mut misc : Vec<FileProperty> = vec! [
    FileProperty::Was_Overloaded,
    FileProperty::NoSearchMatching,
    FileProperty::Had_ID_Before_Import,
    FileProperty::NoSearchMatching,
  ];
  set_file_property (
    &mut misc, FileProperty::NoSearchMatching, false );
  assert_eq! (misc, vec! [
    FileProperty::Was_Overloaded,
    FileProperty::Had_ID_Before_Import]);
  assert! (! file_property_is_true (
    &misc, FileProperty::NoSearchMatching ));

  set_file_property (
    &mut misc, FileProperty::NoSearchMatching, true );
  set_file_property (
    &mut misc, FileProperty::NoSearchMatching, true );
  assert_eq! (misc, vec! [
    FileProperty::Was_Overloaded,
    FileProperty::Had_ID_Before_Import,
    FileProperty::NoSearchMatching]);
}

#[test]
fn no_search_matching_has_the_exact_persisted_yaml_spelling () {
  assert_eq! (serde_yaml::to_string (&FileProperty::NoSearchMatching)
              . unwrap (), "NoSearchMatching\n");
  assert_eq! (serde_yaml::from_str::<FileProperty> ("NoSearchMatching")
              . unwrap (), FileProperty::NoSearchMatching);
}
