use super::*;
use crate::source_sets::SourceSetName;
use crate::git_ops::diff::compute_diff_for_source;
use crate::types::git::NodeChanges;
use crate::types::misc::MSV;
use crate::types::nodes::complete::empty_node_complete;
use crate::types::nodes::fs::NodeFS;
use git2::{Config, Index, Oid, Repository, Signature, Tree};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

fn id (s : &str) -> ID { ID ( s . to_string () ) }
fn src (s : &str) -> SourceName { SourceName ( s . to_string () ) }

fn nodecomplete (
  pid       : &str,
  overrides : Vec<&str>,
) -> NodeComplete {
  let node_fs : NodeFS =
    serde_yaml::from_str ( & format! (
      "pid: '{}'\ntitle: \"{}\"\n{}",
      pid, pid,
      if overrides . is_empty () { String::new () }
      else { format! (
        "overrides_view_of:\n{}",
        overrides . iter ()
          . map ( |o| format! ("  - \"{}\"\n", o) )
          . collect::<String> () ) } )) . unwrap ();
  node_fs . into_complete_as_single_section ( src ("main") ) }

fn modified_entry (
  overrides_diff : Vec<Diff_Item<ID>>,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status : GitDiffStatus::Modified,
    node_changes : Some ( NodeChanges {
      overrides_view_of_diff : overrides_diff,
      .. NodeChanges::default () } ),
    before_node : None,
    after_node : None } }

fn deleted_entry (
  before : NodeComplete,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status : GitDiffStatus::Deleted,
    node_changes : None,
    before_node : Some (before),
    after_node : None } }

fn added_entry (
  after : NodeComplete,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status : GitDiffStatus::Added,
    node_changes : None,
    before_node : None,
    after_node : Some (after) } }

fn empty_source_diff () -> SourceDiff {
  SourceDiff {
    is_git_repo   : true,
    staged        : HashMap::new (),
    unstaged      : HashMap::new (),
    added_nodes   : HashMap::new (),
    deleted_nodes : HashMap::new () } }

#[test]
fn signs_come_from_modified_deleted_and_added_files_per_stage () {
  let owner : ID = id ("N");
  let mut sd : SourceDiff = empty_source_diff ();
  // edge-r's Modified file removed its edge to N, STAGED.
  sd . staged . insert (
    PathBuf::from ("edge-r.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( owner . clone () ) ] ));
  // del-r's file was Deleted UNSTAGED; its before_node named N.
  sd . unstaged . insert (
    PathBuf::from ("del-r.skg"),
    deleted_entry ( nodecomplete ("del-r", vec! ["N"]) ));
  // newfile-r's file was Added UNSTAGED; its after_node names N.
  sd . unstaged . insert (
    PathBuf::from ("newfile-r.skg"),
    added_entry ( nodecomplete ("newfile-r", vec! ["N"]) ));
  // bystander: a Deleted file that never named N.
  sd . unstaged . insert (
    PathBuf::from ("bystander.skg"),
    deleted_entry ( nodecomplete ("bystander", vec! []) ));
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs, None );
  assert_eq! ( scan . len (), 3, "{:?}", scan );
  assert_eq! ( scan [ &id ("edge-r") ],
    MembershipAxes { staged : Some (Sign::Minus), unstaged : None } );
  assert_eq! ( scan [ &id ("del-r") ],
    MembershipAxes { staged : None, unstaged : Some (Sign::Minus) } );
  assert_eq! ( scan [ &id ("newfile-r") ],
    MembershipAxes { staged : None, unstaged : Some (Sign::Plus) } );
  assert! ( ! scan [ &id ("del-r") ] . net_is_present () );
  assert! ( scan [ &id ("newfile-r") ] . net_is_present () );
}

#[test]
fn owner_absent_from_every_diff_yields_nothing () {
  let mut sd : SourceDiff = empty_source_diff ();
  sd . unstaged . insert (
    PathBuf::from ("other.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( id ("SOMEONE-ELSE") ) ] ));
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &id ("N"), NodeRelation::OverridesViewOf, &diffs, None );
  assert! ( scan . is_empty (), "{:?}", scan );
}

#[test]
fn each_relation_is_read_separately () {
  // One Modified member file removed its SUBSCRIPTION to N while its
  // override of N is untouched: the overriderCol's scan must see
  // nothing, the subscriberCol's scan the Minus.
  let owner : ID = id ("N");
  let mut sd : SourceDiff = empty_source_diff ();
  sd . unstaged . insert (
    PathBuf::from ("m.skg"),
    NodeCompleteDiff {
      status : GitDiffStatus::Modified,
      node_changes : Some ( NodeChanges {
        subscribes_to_diff : vec! [
          Diff_Item::Removed ( owner . clone () ) ],
        .. NodeChanges::default () } ),
      before_node : None,
      after_node : None } );
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  assert! ( inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs, None )
    . is_empty () );
  assert_eq! ( inverse_scan_for_inbound_col (
      &owner, NodeRelation::Subscribes, &diffs, None ) [ &id ("m") ],
    MembershipAxes { staged : None, unstaged : Some (Sign::Minus) } );
}

#[test]
fn cross_source_move_yields_no_membership_change () {
  // mover's file leaves source A and lands in source B within the
  // same stage, asserting the same edge to N before and after: the
  // Minus and Plus must cancel, leaving no membership sign at all.
  let owner : ID = id ("N");
  let mut sd_a : SourceDiff = empty_source_diff ();
  sd_a . unstaged . insert (
    PathBuf::from ("mover.skg"),
    deleted_entry ( nodecomplete ("mover", vec! ["N"]) ));
  let mut sd_b : SourceDiff = empty_source_diff ();
  sd_b . unstaged . insert (
    PathBuf::from ("mover.skg"),
    added_entry ( nodecomplete ("mover", vec! ["N"]) ));
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([
      ( src ("a"), sd_a ),
      ( src ("b"), sd_b ) ]) );
  let scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs, None );
  assert! ( scan . is_empty (),
    "a move must not fabricate a membership change: {:?}", scan );
}

#[test]
fn edge_source_gates_deleted_stage_signs () {
  // del-r's file was Deleted; its before_node's override of N was
  // recorded in the PRIVATE source (a MemberAtSource whose source
  // differs from del-r's own -- public -- home). A public-only
  // active set must not see the resulting phantom sign; ungated
  // (None) still does.
  let owner : ID = id ("N");
  let mut before : NodeComplete = empty_node_complete ();
  before . pid = id ("del-r");
  before . title = "del-r" . to_string ();
  before . source = src ("public");
  before . overrides_view_of = MSV::Specified ( vec! [
    MemberAtSource::at_source ( src ("private"), owner . clone () ) ] );
  let mut sd : SourceDiff = empty_source_diff ();
  sd . unstaged . insert (
    PathBuf::from ("del-r.skg"),
    deleted_entry ( before ) );
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([ ( src ("public"), sd ) ]) );
  let public_only : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName::from ("public"),
    sources : BTreeSet::from ([ src ("public") ]) };
  let gated : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs,
      Some (&public_only) );
  assert! ( gated . is_empty (),
    "a Deleted-stage sign recorded at an inactive source must not \
     surface: {:?}", gated );
  let ungated : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs, None );
  assert! ( ! ungated . is_empty (),
    "ungated (None) scan should still see the Deleted-stage sign: {:?}",
    ungated );
}

fn real_git_relation_diff (
  source_name : &str,
) -> (TempDir, SourceDiff) {
  let directory : TempDir = TempDir::new () . unwrap ();
  let repository : Repository =
    Repository::init (directory . path ()) . unwrap ();
  { let mut config : Config =
      repository . config () . unwrap ();
    config . set_str ("user.email", "test@test.com") . unwrap ();
    config . set_str ("user.name", "Test") . unwrap (); }
  let member_path : PathBuf =
    directory . path () . join ("member.skg");
  fs::write (
    &member_path,
    "pid: member\ntitle: member\n" ) . unwrap ();
  { let mut index : Index =
      repository . index () . unwrap ();
    index . add_path (Path::new ("member.skg")) . unwrap ();
    index . write () . unwrap ();
    let tree_id : Oid = index . write_tree () . unwrap ();
    let tree : Tree =
      repository . find_tree (tree_id) . unwrap ();
    let signature : Signature =
      Signature::now ("skg test", "skg@example.com") . unwrap ();
    repository . commit (
      Some ("HEAD"), &signature, &signature,
      "initial", &tree, &[] ) . unwrap (); }
  fs::write (
    &member_path,
    "pid: member\ntitle: member\noverrides_view_of:\n  - owner\n" )
    . unwrap ();
  let source : SourceName = SourceName::from (source_name);
  let diff : SourceDiff =
    compute_diff_for_source (directory . path (), &source) . unwrap ();
  (directory, diff) }

fn real_git_sibling_source_diffs () -> (TempDir, SourceDiff, SourceDiff) {
  let directory : TempDir = TempDir::new () . unwrap ();
  let repository : Repository =
    Repository::init (directory . path ()) . unwrap ();
  { let mut config : Config =
      repository . config () . unwrap ();
    config . set_str ("user.email", "test@test.com") . unwrap ();
    config . set_str ("user.name", "Test") . unwrap (); }
  let public_dir : PathBuf = directory . path () . join ("public");
  let private_dir : PathBuf = directory . path () . join ("private");
  fs::create_dir_all (&public_dir) . unwrap ();
  fs::create_dir_all (&private_dir) . unwrap ();
  let public_path : PathBuf = public_dir . join ("member.skg");
  let private_path : PathBuf = private_dir . join ("member.skg");
  let initial : &str = "pid: member\ntitle: member\n";
  fs::write (&public_path, initial) . unwrap ();
  fs::write (&private_path, initial) . unwrap ();
  { let mut index : Index =
      repository . index () . unwrap ();
    index . add_path (Path::new ("public/member.skg")) . unwrap ();
    index . add_path (Path::new ("private/member.skg")) . unwrap ();
    index . write () . unwrap ();
    let tree_id : Oid = index . write_tree () . unwrap ();
    let tree : Tree =
      repository . find_tree (tree_id) . unwrap ();
    let signature : Signature =
      Signature::now ("skg test", "skg@example.com") . unwrap ();
    repository . commit (
      Some ("HEAD"), &signature, &signature,
      "initial", &tree, &[] ) . unwrap (); }
  let relation : &str =
    "pid: member\ntitle: member\noverrides_view_of:\n  - owner\n";
  fs::write (&public_path, relation) . unwrap ();
  { let mut index : Index =
      repository . index () . unwrap ();
    index . add_path (Path::new ("public/member.skg")) . unwrap ();
    index . write () . unwrap (); }
  fs::write (&private_path, relation) . unwrap ();
  let public_source : SourceName = SourceName::from ("public");
  let private_source : SourceName = SourceName::from ("private");
  let public_diff : SourceDiff =
    compute_diff_for_source (&public_dir, &public_source) . unwrap ();
  let private_diff : SourceDiff =
    compute_diff_for_source (&private_dir, &private_source) . unwrap ();
  fs::write (
    &public_path,
    "pid: member\ntitle: member\noverrides_view_of:\n  - other\n" )
    . unwrap ();
  (directory, public_diff, private_diff) }

#[test]
fn modified_git_relation_sign_uses_captured_source_provenance () {
  let (_private_dir, private_diff) : (TempDir, SourceDiff) =
    real_git_relation_diff ("private");
  let private_diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([(src ("private"), private_diff)]) );
  let public_only : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName::from ("public"),
    sources : BTreeSet::from ([src ("public")]) };
  let private_entry : &NodeCompleteDiff =
    private_diffs . as_ref () . unwrap () [ &src ("private") ]
      . unstaged . get (Path::new ("member.skg")) . unwrap ();
  assert_eq! (
    private_entry . before_node . as_ref () . unwrap () . source,
    src ("private") );
  assert_eq! (
    private_entry . after_node . as_ref () . unwrap () . source,
    src ("private") );
  let private_scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &id ("owner"), NodeRelation::OverridesViewOf,
      &private_diffs, Some (&public_only) );
  assert! (
    private_scan . is_empty (),
    "an inactive relation edit must not create a public inbound member: {:?}",
    private_scan );

  let (_public_dir, public_diff) : (TempDir, SourceDiff) =
    real_git_relation_diff ("public");
  let public_diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([(src ("public"), public_diff)]) );
  let public_scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &id ("owner"), NodeRelation::OverridesViewOf,
      &public_diffs, Some (&public_only) );
  assert_eq! (
    public_scan [&id ("member")],
    MembershipAxes { staged : None, unstaged : Some (Sign::Plus) } ); }

#[test]
fn shared_repo_diff_is_scoped_to_each_source_before_provenance_capture () {
  let (_directory, public_diff, private_diff) : (TempDir, SourceDiff, SourceDiff) =
    real_git_sibling_source_diffs ();
  assert_eq! (public_diff . staged . len (), 1);
  assert! (public_diff . staged . contains_key (Path::new ("member.skg")));
  assert! (public_diff . unstaged . is_empty ());
  assert! (private_diff . staged . is_empty ());
  assert_eq! (private_diff . unstaged . len (), 1);
  assert! (private_diff . unstaged
    . contains_key (Path::new ("member.skg")));
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([
      (src ("public"), public_diff),
      (src ("private"), private_diff) ]) );
  let public_only : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName::from ("public"),
    sources : BTreeSet::from ([src ("public")]) };
  let scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &id ("owner"), NodeRelation::OverridesViewOf,
      &diffs, Some (&public_only) );
  assert_eq! (
    scan [&id ("member")],
    MembershipAxes { staged : Some (Sign::Plus), unstaged : None } ); }
