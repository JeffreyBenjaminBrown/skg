//! TODO/more.org, "The skg diff report should report what vanished
//! nodes used to be": a node id that the worktree still REFERENCES
//! (in some contains / subscribes_to / hides_from_its_subscriptions /
//! overrides_view_of list) but that exists in NO source -- the kind
//! that renders as "Parent references unknown node." -- is
//! investigated in the git history of every source. If it was never
//! there, the report says that; otherwise it names the commit at
//! which it vanished and what it was connected to (in every possible
//! way, textlinks included) when last present.

use crate::diff_analysis::snapshot::{
  parse_blob_node, path_is_source_skg, source_prefix_in_repo};
use crate::diff_analysis::types::{
  CommitStamp, GraphSnapshot, VanishedNodeReport, VanishedNodeSighting};
use crate::git_ops::read_repo::open_repo;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::textlinks::textlinks_from_node;

use git2::{Commit, ObjectType, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeSet, HashSet};
use std::path::{Path, PathBuf};

/// Every id some node of 'snapshot' lists as a relationship member
/// (contains, subscribes_to, hides_from_its_subscriptions,
/// overrides_view_of) that no node of 'snapshot' answers to (as
/// primary or extra id). Textlink targets are NOT collected here:
/// a dead textlink degrades to text, not to an unknown-node phantom.
pub fn dangling_ids_in_snapshot (
  snapshot : &GraphSnapshot,
) -> BTreeSet<ID> {
  let resolvable : HashSet<&ID> = {
    let mut ids : HashSet<&ID> = HashSet::new ();
    for node in snapshot . nodes . values () {
      ids . insert (& node . pid);
      ids . extend ( node . extra_ids . iter () ); }
    ids };
  let mut dangling : BTreeSet<ID> = BTreeSet::new ();
  for node in snapshot . nodes . values () {
    let referenced =
      node . contains . iter ()
      . chain ( node . subscribes_to . or_default () . iter () )
      . chain ( node . hides_from_its_subscriptions
                . or_default () . iter () )
      . chain ( node . overrides_view_of . or_default () . iter () );
    for id in referenced {
      if ! resolvable . contains (id) {
        dangling . insert ( id . clone () ); }} }
  dangling }

/// Investigate each id of 'ids' in the git history of every source:
/// walk each repo's FIRST-PARENT chain from HEAD looking for the most
/// recent commit whose tree holds '<id>.skg'. A source that cannot be
/// opened or walked contributes nothing (the ordinary diff-analysis
/// refusals have already vetted the sources the selection needs).
/// One walk per source covers all ids.
pub fn investigate_vanished_ids (
  config : &SkgConfig,
  ids    : &BTreeSet<ID>,
) -> Vec<VanishedNodeReport> {
  if ids . is_empty () { return Vec::new (); }
  let mut reports : Vec<VanishedNodeReport> =
    ids . iter ()
    . map ( |id| VanishedNodeReport {
        id : id . clone (), sightings : Vec::new () } )
    . collect ();
  let source_names : Vec<&SourceName> = {
    let mut names : Vec<&SourceName> =
      config . sources . keys () . collect ();
    names . sort (); // deterministic report order
    names };
  for source_name in source_names {
    let Some (source) = config . sources . get (source_name)
      else { continue; };
    let Some (repo) = open_repo ( Path::new (& source . path) )
      else { continue; };
    let Ok (prefix) =
      source_prefix_in_repo (&repo, Path::new (& source . path))
      else { continue; };
    sight_ids_in_repo (
      &repo, &prefix, source_name, ids, &mut reports ); }
  reports }

/// One first-parent walk from HEAD. The first commit (i.e. the most
/// recent) in which an id's file appears yields its sighting: that
/// commit is 'last_present', and the previously visited commit -- its
/// first-parent descendant, which lacks the file -- is 'vanished_at'
/// (None if the file is present at HEAD itself, which cannot happen
/// for a genuinely dangling id).
fn sight_ids_in_repo (
  repo        : &Repository,
  prefix      : &Path,
  source_name : &SourceName,
  ids         : &BTreeSet<ID>,
  reports     : &mut [VanishedNodeReport],
) {
  let mut walk = match repo . revwalk () {
    Ok (w) => w, Err (_) => return, };
  if walk . push_head () . is_err () { return; }
  walk . simplify_first_parent () . ok ();
  let mut remaining : BTreeSet<ID> = ids . clone ();
  let mut descendant : Option<git2::Oid> = None;
  for oid in walk . flatten () {
    let Ok (commit) = repo . find_commit (oid) else { break; };
    let Ok (tree) = commit . tree () else { break; };
    for id in remaining . clone () {
      let file : PathBuf =
        prefix . join ( format! ("{}.skg", id . 0) );
      if tree . get_path (&file) . is_ok () {
        remaining . remove (&id);
        if let Some (sighting) = sighting_at_commit (
          repo, prefix, source_name, &id, &commit, descendant )
        { if let Some (report) =
            reports . iter_mut () . find ( |r| r . id == id )
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
/// tree that referenced it, by relation -- textlinks included.
fn sighting_at_commit (
  repo        : &Repository,
  prefix      : &Path,
  source_name : &SourceName,
  id          : &ID,
  commit      : &Commit,
  descendant  : Option<git2::Oid>,
) -> Option<VanishedNodeSighting> {
  let tree = commit . tree () . ok () ?;
  let own : NodeComplete = {
    let file : PathBuf =
      prefix . join ( format! ("{}.skg", id . 0) );
    let entry = tree . get_path (&file) . ok () ?;
    let blob = repo . find_blob ( entry . id () ) . ok () ?;
    parse_blob_node ( blob . content (), source_name, &file ) . ok () ? };
  let outbound : Vec<(&'static str, Vec<ID>)> = {
    let mut outbound : Vec<(&'static str, Vec<ID>)> = Vec::new ();
    let mut keep = |name : &'static str, members : &[ID]| {
      if ! members . is_empty () {
        outbound . push ( (name, members . to_vec ()) ); }};
    keep ("contains",                     & own . contains);
    keep ("subscribes_to",                own . subscribes_to . or_default ());
    keep ("hides_from_its_subscriptions", own . hides_from_its_subscriptions . or_default ());
    keep ("overrides_view_of",            own . overrides_view_of . or_default ());
    outbound };
  let inbound : Vec<(ID, &'static str)> =
    inbound_references_in_tree (repo, prefix, source_name, id, &tree);
  Some ( VanishedNodeSighting {
    source       : source_name . clone (),
    last_present : commit_stamp (commit),
    vanished_at  : descendant
      . and_then ( |oid| repo . find_commit (oid) . ok () )
      . map ( |c| commit_stamp (&c) ),
    title        : own . title,
    outbound,
    inbound, } ) }

/// Every node in 'tree' (other than the id's own file) that refers to
/// the id, with the relation(s) it does so by.
fn inbound_references_in_tree (
  repo        : &Repository,
  prefix      : &Path,
  source_name : &SourceName,
  id          : &ID,
  tree        : &git2::Tree,
) -> Vec<(ID, &'static str)> {
  let mut inbound : Vec<(ID, &'static str)> = Vec::new ();
  tree . walk (TreeWalkMode::PreOrder, |root, entry| {
    if entry . kind () != Some (ObjectType::Blob) {
      return TreeWalkResult::Ok; }
    let rel_path : PathBuf =
      PathBuf::from (root) . join ( entry . name () . unwrap_or ("") );
    if ! path_is_source_skg (&rel_path, prefix) {
      return TreeWalkResult::Ok; }
    let Ok (blob) = repo . find_blob ( entry . id () )
      else { return TreeWalkResult::Ok; };
    let Ok (node) = parse_blob_node (
      blob . content (), source_name, &rel_path )
      else { return TreeWalkResult::Ok; }; // an unparseable neighbor cannot hide the parseable ones
    if node . pid == *id {
      return TreeWalkResult::Ok; }
    let mut note = |name : &'static str, hit : bool| {
      if hit { inbound . push ( (node . pid . clone (), name) ); }};
    note ("contains",
          node . contains . contains (id));
    note ("subscribes_to",
          node . subscribes_to . or_default () . contains (id));
    note ("hides_from_its_subscriptions",
          node . hides_from_its_subscriptions . or_default ()
            . contains (id));
    note ("overrides_view_of",
          node . overrides_view_of . or_default () . contains (id));
    note ("textlink",
          textlinks_from_node (&node) . iter ()
            . any ( |l| l . id == *id ));
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
