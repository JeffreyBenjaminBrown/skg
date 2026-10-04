use super::{
  Flag, Graphnode, empty_node_complete,
  flag_is_true, set_flag};
use crate::types::misc::ID;

fn node_with_ids (
  pid : &str,
  extra_ids : &[&str],
) -> Graphnode {
  let mut node : Graphnode = empty_node_complete ();
  node . pid = ID::from (pid);
  node . extra_ids = extra_ids . iter ()
    . map ( |id| ID::from (*id) )
    . collect ();
  node
}

#[test]
fn normalize_ids_treats_the_pid_as_first_and_keeps_first_extra_order () {
  let mut node : Graphnode =
    node_with_ids ("P", &["B", "P", "A", "B", "C", "A"]);
  node . normalize_ids ();
  assert_eq! (
    node . extra_ids,
    vec![ID::from ("B"), ID::from ("A"), ID::from ("C")]);
}

#[test]
fn normalize_ids_is_idempotent () {
  let mut node : Graphnode =
    node_with_ids ("P", &["A", "A", "P", "B"]);
  node . normalize_ids ();
  let once : Vec<ID> = node . extra_ids . clone ();
  node . normalize_ids ();
  assert_eq! (node . extra_ids, once);
}

#[test]
fn normalize_ids_leaves_an_already_normal_list_alone () {
  let mut node : Graphnode =
    node_with_ids ("P", &["A", "B", "C"]);
  let before : Vec<ID> = node . extra_ids . clone ();
  node . normalize_ids ();
  assert_eq! (node . extra_ids, before);
}

#[test]
fn flag_registry_has_stable_public_names_and_order () {
  assert_eq! (
    Flag::ALL . map (Flag::wire_name),
    ["hadId", "wasOverloaded", "noSearchMatching"] );
  assert_eq! (
    Flag::ALL . map (Flag::herald_text),
    ["☮ had ID before import",
     "☮ was overloaded during org-roam import",
     "☮ no search matching"] );
  for flag in Flag::ALL {
    assert_eq! (
      Flag::from_wire_name (flag . wire_name ()),
      Some (flag)); }
  assert_eq! (Flag::from_wire_name ("NoIndex"), None);
  assert! (! Flag::Had_ID_Before_Import . is_mutable ());
  assert! (! Flag::Was_Overloaded . is_mutable ());
  assert! (Flag::NoSearchMatching . is_mutable ());
}

#[test]
fn setting_one_flag_preserves_the_others_and_repairs_duplicates () {
  let mut misc : Vec<Flag> = vec! [
    Flag::Was_Overloaded,
    Flag::NoSearchMatching,
    Flag::Had_ID_Before_Import,
    Flag::NoSearchMatching,
  ];
  set_flag (
    &mut misc, Flag::NoSearchMatching, false );
  assert_eq! (misc, vec! [
    Flag::Was_Overloaded,
    Flag::Had_ID_Before_Import]);
  assert! (! flag_is_true (
    &misc, Flag::NoSearchMatching ));

  set_flag (
    &mut misc, Flag::NoSearchMatching, true );
  set_flag (
    &mut misc, Flag::NoSearchMatching, true );
  assert_eq! (misc, vec! [
    Flag::Was_Overloaded,
    Flag::Had_ID_Before_Import,
    Flag::NoSearchMatching]);
}

#[test]
fn no_search_matching_has_the_exact_persisted_yaml_spelling () {
  assert_eq! (serde_yaml::to_string (&Flag::NoSearchMatching)
              . unwrap (), "NoSearchMatching\n");
  assert_eq! (serde_yaml::from_str::<Flag> ("NoSearchMatching")
              . unwrap (), Flag::NoSearchMatching);
}
