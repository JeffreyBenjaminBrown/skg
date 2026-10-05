use super::*;
use super::super::git::{GitDiffStatus, NodeChanges};
use super::super::misc::Skgrepo;

fn skgrepo_name (s: &str) -> SkgrepoName { SkgrepoName ( s . to_string () ) }
fn skgid        (s: &str) -> ID         { ID ( s . to_string () ) }

fn make_parent_diff (
  contains_diff : Vec<Diff_Item<ID>>,
) -> GraphnodeDiff {
  GraphnodeDiff {
    status: GitDiffStatus::Modified,
    node_changes: Some ( NodeChanges {
      contains_diff,
      .. NodeChanges::default () } ),
    before_node: None,
    after_node: None, } }

fn skgrepo_diff_with_parent_contains (
  parent_pid : &ID,
  staged_ops   : Vec<Diff_Item<ID>>,
  unstaged_ops : Vec<Diff_Item<ID>>,
) -> SkgrepoDiff {
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_pid . 0 ) );
  let mut staged   : HashMap<PathBuf, GraphnodeDiff> = HashMap::new ();
  let mut unstaged : HashMap<PathBuf, GraphnodeDiff> = HashMap::new ();
  if !staged_ops . is_empty () {
    staged . insert ( parent_file . clone (),
                      make_parent_diff (staged_ops) ); }
  if !unstaged_ops . is_empty () {
    unstaged . insert ( parent_file,
                        make_parent_diff (unstaged_ops) ); }
  SkgrepoDiff {
    is_gitrepo: true,
    staged, unstaged,
    added_nodes: HashMap::new (),
    deleted_nodes: HashMap::new (), } }

#[test]
fn staged_removal_is_attributed_to_staged_side () {
  let parent    : ID = skgid ("parent");
  let child     : ID = skgid ("child");
  let src       : SkgrepoName = skgrepo_name ("public");
  let mut diffs : HashMap<SkgrepoName, SkgrepoDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   skgrepo_diff_with_parent_contains (
                     &parent,
                     vec! [ Diff_Item::Removed (child . clone ()) ],
                     vec! [] ) );
  let (ex, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, RelationshipAxes {
    staged: Some (Sign::Minus), unstaged: None } );
  assert_eq! ( ex, NodeAxes::default () );
}

#[test]
fn unstaged_removal_is_attributed_to_unstaged_side () {
  let parent    : ID = skgid ("parent");
  let child     : ID = skgid ("child");
  let src       : SkgrepoName = skgrepo_name ("public");
  let mut diffs : HashMap<SkgrepoName, SkgrepoDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   skgrepo_diff_with_parent_contains (
                     &parent,
                     vec! [],
                     vec! [ Diff_Item::Removed (child . clone ()) ] ) );
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, RelationshipAxes {
    staged: None, unstaged: Some (Sign::Minus) } );
}

#[test]
fn staged_add_then_unstaged_remove () {
  let parent    : ID = skgid ("parent");
  let child     : ID = skgid ("child");
  let src       : SkgrepoName = skgrepo_name ("public");
  let mut diffs : HashMap<SkgrepoName, SkgrepoDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   skgrepo_diff_with_parent_contains (
                     &parent,
                     vec! [ Diff_Item::New     (child . clone ()) ],
                     vec! [ Diff_Item::Removed (child . clone ()) ] ) );
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, RelationshipAxes {
    staged: Some (Sign::Plus), unstaged: Some (Sign::Minus) } );
}

#[test]
fn no_parent_contains_diff_falls_back_to_unstaged_minus () {
  // Fallback preserves legacy behavior for callers (e.g. subscription
  // phantoms) where there's no contains_diff entry for this child.
  let parent : ID = skgid ("parent");
  let child  : ID = skgid ("child");
  let src    : SkgrepoName = skgrepo_name ("public");
  let diffs  : HashMap<SkgrepoName, SkgrepoDiff> = HashMap::new ();
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, RelationshipAxes {
    staged: None, unstaged: Some (Sign::Minus) } );
}

#[test]
fn each_relation_reads_its_own_diff_when_one_recorder_bears_both () {
  // The mislabeling case that motivated relation-true attribution:
  // one recorder both contains and overrides the same child, each relationship
  // removed in a DIFFERENT stage. The contains-phantom must carry
  // the contains stage and the overriddenFolder-phantom the overrides
  // stage; a first-hit scan across relations would label both from
  // whichever relation it checked first.
  let parent : ID = skgid ("parent");
  let child  : ID = skgid ("child");
  let src    : SkgrepoName = skgrepo_name ("public");
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent . 0 ) );
  let diff_for = | contains_ops : Vec<Diff_Item<ID>>,
                   overrides_ops : Vec<Diff_Item<ID>> |
    -> GraphnodeDiff {
    GraphnodeDiff {
      status: GitDiffStatus::Modified,
      node_changes: Some ( NodeChanges {
        contains_diff          : contains_ops,
        overrides_diff         : overrides_ops,
        .. NodeChanges::default () } ),
      before_node: None,
      after_node: None, } };
  let mut diffs : HashMap<SkgrepoName, SkgrepoDiff> = HashMap::new ();
  diffs . insert ( src . clone (), SkgrepoDiff {
    is_gitrepo: true,
    staged: HashMap::from ([ // contains relationship removed STAGED
      ( parent_file . clone (),
        diff_for ( vec! [ Diff_Item::Removed (child . clone ()) ],
                   vec! [] )) ]),
    unstaged: HashMap::from ([ // overrides relationship removed UNSTAGED
      ( parent_file,
        diff_for ( vec! [],
                   vec! [ Diff_Item::Removed (child . clone ()) ] )) ]),
    added_nodes: HashMap::new (),
    deleted_nodes: HashMap::new (), } );
  let (_, contains_mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( contains_mem, RelationshipAxes {
    staged: Some (Sign::Minus), unstaged: None },
    "the content phantom is labeled from contains_diff only" );
  let (_, overrides_mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Overrides, Some (&diffs) );
  assert_eq! ( overrides_mem, RelationshipAxes {
    staged: None, unstaged: Some (Sign::Minus) },
    "the overriddenFolder phantom is labeled from \
     overrides_diff only" );
}

/// A node with sections in several skgrepos has exactly one home: the
/// most public RETAINED section. The privacy order here is declaration
/// order (foreign, zed, alpha), deliberately NOT alphabetical. The
/// foreign section is discarded by the owned-pid collision rule, so
/// "zed" wins over the retained "alpha" section. Rebuilding the config
/// for every assertion also gives its HashMap a fresh randomized state,
/// catching any accidental return to raw map iteration.
#[test]
fn home_from_disk_is_the_most_public_section () {
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let foreign_path : PathBuf = dir . path () . join ("foreign");
  let public_path  : PathBuf = dir . path () . join ("zed");
  let private_path : PathBuf = dir . path () . join ("alpha");
  std::fs::create_dir_all (&foreign_path) . unwrap ();
  std::fs::create_dir_all (&public_path)  . unwrap ();
  std::fs::create_dir_all (&private_path) . unwrap ();
  let make_config = || -> SkgConfig {
    let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
      HashMap::new ();
    for (name, path, owned) in
      [ ("foreign", foreign_path . clone (), false),
        ("zed",     public_path  . clone (), true),
        ("alpha",   private_path . clone (), true) ] {
      skgrepos . insert ( skgrepo_name (name), Skgrepo {
        name         : skgrepo_name (name),
        abbreviation : None,
        path,
        owned, } ); }
    let mut config : SkgConfig =
      SkgConfig::dummyFromSkgrepos (skgrepos);
    config . skgrepo_order = // most public first
      vec! [ skgrepo_name ("foreign"),
             skgrepo_name ("zed"),
             skgrepo_name ("alpha") ];
    config };
  { // The foreign collision is ignored; most-public retained wins.
    std::fs::write ( foreign_path . join ("N.skg"),
                     "pid: N\ntitle: unrelated foreign N\n" ) . unwrap ();
    std::fs::write ( public_path  . join ("N.skg"),
                     "pid: N\ntitle: N\n" ) . unwrap ();
    std::fs::write ( private_path . join ("N.skg"),
                     "pid: N\ncontains:\n- C\n" ) . unwrap ();
    for _ in 0 .. 20 {
      let config : SkgConfig = make_config ();
      assert_eq! ( home_from_disk ( &skgid ("N"), &config ),
                   Some ( skgrepo_name ("zed") ) ); }}
  { // A section in only the more private skgrepo: that is the home.
    std::fs::write ( private_path . join ("P.skg"),
                     "pid: P\ntitle: P\n" ) . unwrap ();
    let config : SkgConfig = make_config ();
    assert_eq! ( home_from_disk ( &skgid ("P"), &config ),
                 Some ( skgrepo_name ("alpha") ) ); }
  let config : SkgConfig = make_config ();
  assert_eq! ( home_from_disk ( &skgid ("absent"), &config ), None );
}
