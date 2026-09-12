use super::{NodeComplete, empty_node_complete};
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
