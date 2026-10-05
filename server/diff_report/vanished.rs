//! TODO/more.org, "The skg diff report should report what vanished
//! nodes used to be": a node id that the worktree still REFERENCES
//! (in some contains / subscribes_to / hides_from_its_subscriptions /
//! overrides_view_of list) but that exists in NO skgrepo -- the kind
//! that renders as "Parent references unknown node." -- is
//! investigated in the git history of every skgrepo. If it was never
//! there, the report says that; otherwise it names the commit at
//! which it vanished and what it was connected to (in every possible
//! way, links included) when last present.

use crate::diff_report::git_snapshot::{
  parse_blob_node, path_is_skgrepo_skg, repo_prefix_in_gitrepo};
use crate::diff_report::types::{
  CommitStamp, GraphSnapshot, VanishedNodeReport, VanishedNodeSighting};
use crate::git_ops::read_gitrepo::open_gitrepo;
use crate::types::misc::{ID, MSV, SkgConfig, SkgRepoName, members_msv, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::links::links_from_node;

use git2::{Commit, ObjectType, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeSet, HashSet};
use std::path::{Path, PathBuf};

/// Every id some node of 'git_snapshot' lists as a relationship member
/// (contains, subscribes_to, hides_from_its_subscriptions,
/// overrides_view_of) that no node of 'git_snapshot' answers to (as
/// primary or extra id). Link targets are NOT collected here:
/// a dangling link degrades to text, not to an unknown-node phantom.
pub fn dangling_skgids_in_git_snapshot (
  git_snapshot : &GraphSnapshot,
) -> BTreeSet<ID> {
  let resolvable : HashSet<&ID> = {
    let mut skgids : HashSet<&ID> = HashSet::new ();
    for node in git_snapshot . nodes . values () {
      skgids . insert (& node . pid);
      skgids . extend ( node . extra_ids . iter () ); }
    skgids };
  let mut dangling : BTreeSet<ID> = BTreeSet::new ();
  for node in git_snapshot . nodes . values () {
    let contains_skgids   : Vec<ID> = members_of (& node . contains);
    let subscribes_skgids : MSV<ID> = members_msv (& node . subscribes_to);
    let hides_skgids       : MSV<ID> = members_msv (
      & node . hides_from_its_subscriptions);
    let overrides_skgids   : MSV<ID> = members_msv (& node . overrides_view_of);
    let referenced =
      contains_skgids . iter ()
      . chain ( subscribes_skgids . or_default () . iter () )
      . chain ( hides_skgids . or_default () . iter () )
      . chain ( overrides_skgids . or_default () . iter () );
    for skgid in referenced {
      if ! resolvable . contains (skgid) {
        dangling . insert ( skgid . clone () ); }} }
  dangling }

/// Investigate each id of 'ids' in the git history of every skgrepo:
/// walk each skgrepo's FIRST-PARENT chain from HEAD looking for the most
/// recent commit whose tree holds '<id>.skg'. A skgrepo that cannot be
/// opened or walked contributes nothing (the ordinary diff-report
/// refusals have already vetted the skgrepos the selection needs).
/// One walk per skgrepo covers all ids.
pub fn investigate_vanished_skgids (
  config : &SkgConfig,
  skgids    : &BTreeSet<ID>,
) -> Vec<VanishedNodeReport> {
  if skgids . is_empty () { return Vec::new (); }
  let mut reports : Vec<VanishedNodeReport> =
    skgids . iter ()
    . map ( |skgid| VanishedNodeReport {
        skgid : skgid . clone (), sightings : Vec::new () } )
    . collect ();
  let skgrepo_names : Vec<&SkgRepoName> = {
    let mut names : Vec<&SkgRepoName> =
      config . skgrepos . keys () . collect ();
    names . sort (); // deterministic report order
    names };
  for skgrepo_name in skgrepo_names {
    let Some (skgrepo) = config . skgrepos . get (skgrepo_name)
      else { continue; };
    let Some (gitrepo) = open_gitrepo ( Path::new (& skgrepo . path) )
      else { continue; };
    let Ok (prefix) =
      repo_prefix_in_gitrepo (&gitrepo, Path::new (& skgrepo . path))
      else { continue; };
    sight_skgids_in_gitrepo (
      &gitrepo, &prefix, skgrepo_name, skgids, &mut reports ); }
  reports }

/// One first-parent walk from HEAD. The first commit (i.e. the most
/// recent) in which an id's file appears yields its sighting: that
/// commit is 'last_present', and the previously visited commit -- its
/// first-parent descendant, which lacks the file -- is 'vanished_at'
/// (None if the file is present at HEAD itself, which cannot happen
/// for a genuinely dangling id).
fn sight_skgids_in_gitrepo (
  gitrepo        : &Repository,
  prefix      : &Path,
  skgrepo_name : &SkgRepoName,
  skgids         : &BTreeSet<ID>,
  reports     : &mut [VanishedNodeReport],
) {
  let mut walk = match gitrepo . revwalk () {
    Ok (w) => w, Err (_) => return, };
  if walk . push_head () . is_err () { return; }
  walk . simplify_first_parent () . ok ();
  let mut remaining : BTreeSet<ID> = skgids . clone ();
  let mut descendant : Option<git2::Oid> = None;
  for oid in walk . flatten () {
    let Ok (commit) = gitrepo . find_commit (oid) else { break; };
    let Ok (tree) = commit . tree () else { break; };
    for skgid in remaining . clone () {
      let file : PathBuf =
        prefix . join ( format! ("{}.skg", skgid . 0) );
      if tree . get_path (&file) . is_ok () {
        remaining . remove (&skgid);
        if let Some (sighting) = sighting_at_commit (
          gitrepo, prefix, skgrepo_name, &skgid, &commit, descendant )
        { if let Some (report) =
            reports . iter_mut () . find ( |r| r . skgid == skgid )
          { report . sightings . push (sighting); }} }}
    if remaining . is_empty () { break; }
    descendant = Some (oid); } }

fn commit_stamp (
  commit : &Commit,
) -> CommitStamp {
  CommitStamp {
    short_sha : commit . id () . to_string () [..8] . to_string (),
    date      : date_from_epoch ( commit . time () . seconds () ),
    summary   : commit . summary () . unwrap_or ("") . to_string (), } }

/// The whole picture of the id at 'commit' (where its file exists):
/// its own title and outbound lists, plus every OTHER node in that
/// tree that referenced it, by relation -- links included.
fn sighting_at_commit (
  gitrepo        : &Repository,
  prefix      : &Path,
  skgrepo_name : &SkgRepoName,
  skgid          : &ID,
  commit      : &Commit,
  descendant  : Option<git2::Oid>,
) -> Option<VanishedNodeSighting> {
  let tree = commit . tree () . ok () ?;
  let own : Graphnode = {
    let file : PathBuf =
      prefix . join ( format! ("{}.skg", skgid . 0) );
    let entry = tree . get_path (&file) . ok () ?;
    let blob = gitrepo . find_blob ( entry . id () ) . ok () ?;
    parse_blob_node ( blob . content (), skgrepo_name, &file ) . ok () ? };
  let outbound : Vec<(&'static str, Vec<ID>)> = {
    let mut outbound : Vec<(&'static str, Vec<ID>)> = Vec::new ();
    let mut keep = |name : &'static str, members : &[ID]| {
      if ! members . is_empty () {
        outbound . push ( (name, members . to_vec ()) ); }};
    let contains_skgids   : Vec<ID> = members_of (& own . contains);
    let subscribes_skgids : MSV<ID> = members_msv (& own . subscribes_to);
    let hides_skgids       : MSV<ID> = members_msv (
      & own . hides_from_its_subscriptions);
    let overrides_skgids   : MSV<ID> = members_msv (& own . overrides_view_of);
    keep ("contains",                     & contains_skgids);
    keep ("subscribes_to",                subscribes_skgids . or_default ());
    keep ("hides_from_its_subscriptions", hides_skgids . or_default ());
    keep ("overrides_view_of",            overrides_skgids . or_default ());
    outbound };
  let inbound : Vec<(ID, &'static str)> =
    inbound_references_in_tree (gitrepo, prefix, skgrepo_name, skgid, &tree);
  Some ( VanishedNodeSighting {
    home_skgrepo : skgrepo_name . clone (),
    last_present : commit_stamp (commit),
    vanished_at  : descendant
      . and_then ( |oid| gitrepo . find_commit (oid) . ok () )
      . map ( |c| commit_stamp (&c) ),
    title        : own . title,
    outbound,
    inbound, } ) }

/// Every node in 'tree' (other than the id's own file) that refers to
/// the id, with the relation(s) it does so by.
fn inbound_references_in_tree (
  gitrepo        : &Repository,
  prefix      : &Path,
  skgrepo_name : &SkgRepoName,
  skgid          : &ID,
  tree        : &git2::Tree,
) -> Vec<(ID, &'static str)> {
  let mut inbound : Vec<(ID, &'static str)> = Vec::new ();
  tree . walk (TreeWalkMode::PreOrder, |root, entry| {
    if entry . kind () != Some (ObjectType::Blob) {
      return TreeWalkResult::Ok; }
    let rel_path : PathBuf =
      PathBuf::from (root) . join ( entry . name () . unwrap_or ("") );
    if ! path_is_skgrepo_skg (&rel_path, prefix) {
      return TreeWalkResult::Ok; }
    let Ok (blob) = gitrepo . find_blob ( entry . id () )
      else { return TreeWalkResult::Ok; };
    let Ok (node) = parse_blob_node (
      blob . content (), skgrepo_name, &rel_path )
      else { return TreeWalkResult::Ok; }; // an unparseable neighbor cannot hide the parseable ones
    if node . pid == *skgid {
      return TreeWalkResult::Ok; }
    let mut note = |name : &'static str, hit : bool| {
      if hit { inbound . push ( (node . pid . clone (), name) ); }};
    note ("contains",
          node . contains . iter () . any ( |m| &m . member == skgid ));
    note ("subscribes_to",
          node . subscribes_to . or_default () . iter ()
            . any ( |m| &m . member == skgid ));
    note ("hides_from_its_subscriptions",
          node . hides_from_its_subscriptions . or_default () . iter ()
            . any ( |m| &m . member == skgid ));
    note ("overrides_view_of",
          node . overrides_view_of . or_default () . iter ()
            . any ( |m| &m . member == skgid ));
    note ("link",
          links_from_node (&node) . iter ()
            . any ( |l| l . skgid == *skgid ));
    TreeWalkResult::Ok
  }) . ok ();
  inbound }

/// Days-to-civil conversion (Howard Hinnant's algorithm), so the
/// report can stamp commits without a date-time dependency.
fn date_from_epoch (
  seconds : i64,
) -> String {
  let days : i64 =
    seconds . div_euclid (86_400);
  let z : i64 = days + 719_468;
  let era : i64 = z . div_euclid (146_097);
  let doe : i64 = z - era * 146_097;
  let yoe : i64 =
    (doe - doe/1460 + doe/36_524 - doe/146_096) / 365;
  let y : i64 = yoe + era * 400;
  let doy : i64 = doe - (365*yoe + yoe/4 - yoe/100);
  let mp : i64 = (5*doy + 2)/153;
  let d : i64 = doy - (153*mp+2)/5 + 1;
  let m : i64 = if mp < 10 { mp + 3 } else { mp - 9 };
  let y : i64 = if m <= 2 { y + 1 } else { y };
  format! ("{:04}-{:02}-{:02}", y, m, d) }
