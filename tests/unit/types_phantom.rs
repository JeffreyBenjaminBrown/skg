use super::*;
use super::super::git::{GitDiffStatus, NodeChanges};
use super::super::misc::SkgfileSource;

fn source_name (s: &str) -> SourceName { SourceName ( s . to_string () ) }
fn id        (s: &str) -> ID         { ID ( s . to_string () ) }

fn make_parent_diff (
  contains_diff : Vec<Diff_Item<ID>>,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status: GitDiffStatus::Modified,
    node_changes: Some ( NodeChanges {
      contains_diff,
      .. NodeChanges::default () } ),
    before_node: None,
    after_node: None, } }

fn source_diff_with_parent_contains (
  parent_pid : &ID,
  staged_ops   : Vec<Diff_Item<ID>>,
  unstaged_ops : Vec<Diff_Item<ID>>,
) -> SourceDiff {
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_pid . 0 ) );
  let mut staged   : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
  let mut unstaged : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
  if !staged_ops . is_empty () {
    staged . insert ( parent_file . clone (),
                      make_parent_diff (staged_ops) ); }
  if !unstaged_ops . is_empty () {
    unstaged . insert ( parent_file,
                        make_parent_diff (unstaged_ops) ); }
  SourceDiff {
    is_git_repo: true,
    staged, unstaged,
    added_nodes: HashMap::new (),
    deleted_nodes: HashMap::new (), } }

#[test]
fn staged_removal_is_attributed_to_staged_side () {
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   source_diff_with_parent_contains (
                     &parent,
                     vec! [ Diff_Item::Removed (child . clone ()) ],
                     vec! [] ) );
  let (ex, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: Some (Sign::Minus), unstaged: None } );
  assert_eq! ( ex, ExistenceAxes::default () );
}

#[test]
fn unstaged_removal_is_attributed_to_unstaged_side () {
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   source_diff_with_parent_contains (
                     &parent,
                     vec! [],
                     vec! [ Diff_Item::Removed (child . clone ()) ] ) );
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: None, unstaged: Some (Sign::Minus) } );
}

#[test]
fn staged_add_then_unstaged_remove () {
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   source_diff_with_parent_contains (
                     &parent,
                     vec! [ Diff_Item::New     (child . clone ()) ],
                     vec! [ Diff_Item::Removed (child . clone ()) ] ) );
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: Some (Sign::Plus), unstaged: Some (Sign::Minus) } );
}

#[test]
fn no_parent_contains_diff_falls_back_to_unstaged_minus () {
  // Fallback preserves legacy behavior for callers (e.g. subscription
  // phantoms) where there's no contains_diff entry for this child.
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let diffs  : HashMap<SourceName, SourceDiff> = HashMap::new ();
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: None, unstaged: Some (Sign::Minus) } );
}

#[test]
fn each_relation_reads_its_own_diff_when_one_owner_bears_both () {
  // The mislabeling case that motivated relation-true attribution:
  // one owner both contains and overrides the same child, each edge
  // removed in a DIFFERENT stage. The contains-phantom must carry
  // the contains stage and the overriddenCol-phantom the overrides
  // stage; a first-hit scan across relations would label both from
  // whichever relation it checked first.
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent . 0 ) );
  let diff_for = | contains_ops : Vec<Diff_Item<ID>>,
                   overrides_ops : Vec<Diff_Item<ID>> |
    -> NodeCompleteDiff {
    NodeCompleteDiff {
      status: GitDiffStatus::Modified,
      node_changes: Some ( NodeChanges {
        contains_diff          : contains_ops,
        overrides_view_of_diff : overrides_ops,
        .. NodeChanges::default () } ),
      before_node: None,
      after_node: None, } };
  let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
  diffs . insert ( src . clone (), SourceDiff {
    is_git_repo: true,
    staged: HashMap::from ([ // contains edge removed STAGED
      ( parent_file . clone (),
        diff_for ( vec! [ Diff_Item::Removed (child . clone ()) ],
                   vec! [] )) ]),
    unstaged: HashMap::from ([ // overrides edge removed UNSTAGED
      ( parent_file,
        diff_for ( vec! [],
                   vec! [ Diff_Item::Removed (child . clone ()) ] )) ]),
    added_nodes: HashMap::new (),
    deleted_nodes: HashMap::new (), } );
  let (_, contains_mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::Contains, Some (&diffs) );
  assert_eq! ( contains_mem, MembershipAxes {
    staged: Some (Sign::Minus), unstaged: None },
    "the content phantom is labeled from contains_diff only" );
  let (_, overrides_mem) = phantom_axes (
    &child, &src, &parent, &src,
    NodeRelation::OverridesViewOf, Some (&diffs) );
  assert_eq! ( overrides_mem, MembershipAxes {
    staged: None, unstaged: Some (Sign::Minus) },
    "the overriddenCol phantom is labeled from \
     overrides_view_of_diff only" );
}

/// A node with sections in several sources has exactly one home: the
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
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new ();
    for (name, path, user_owns_it) in
      [ ("foreign", foreign_path . clone (), false),
        ("zed",     public_path  . clone (), true),
        ("alpha",   private_path . clone (), true) ] {
      sources . insert ( source_name (name), SkgfileSource {
        name         : source_name (name),
        abbreviation : None,
        path,
        user_owns_it, } ); }
    let mut config : SkgConfig =
      SkgConfig::dummyFromSources (sources);
    config . source_order = // most public first
      vec! [ source_name ("foreign"),
             source_name ("zed"),
             source_name ("alpha") ];
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
      assert_eq! ( home_from_disk ( &id ("N"), &config ),
                   Some ( source_name ("zed") ) ); }}
  { // A section in only the more private source: that is the home.
    std::fs::write ( private_path . join ("P.skg"),
                     "pid: P\ntitle: P\n" ) . unwrap ();
    let config : SkgConfig = make_config ();
    assert_eq! ( home_from_disk ( &id ("P"), &config ),
                 Some ( source_name ("alpha") ) ); }
  let config : SkgConfig = make_config ();
  assert_eq! ( home_from_disk ( &id ("absent"), &config ), None );
}
