//! An unknown node is an ID that some node's relation lists name but
//! that no node answers to ("Parent references unknown node"). The
//! "investigate unknown node" request
//! ('server/serve/handlers/investigate_unknown_node.rs') looks for it
//! in the git history of every skgrepo. If it was never there, the
//! answer says that; otherwise it names the commit at which it
//! vanished and what it was connected to (in every possible way,
//! links included) when last present.

use crate::git_ops::git_snapshot::{
  parse_blob_node, path_is_skgrepo_skg, repo_prefix_in_gitrepo};
use crate::git_ops::read_gitrepo::open_gitrepo;
use crate::types::misc::{ID, MSV, SkgConfig, SkgrepoName, members_msv, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::links::links_from_node;

use git2::{Commit, ObjectType, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct VanishedNodeReport {
  pub skgid        : ID,
  pub sightings : Vec<VanishedNodeSighting>, // empty means: never in any skgrepo's git history
}

#[derive(Clone, Debug)]
pub struct VanishedNodeSighting {
  pub home_skgrepo : SkgrepoName,
  pub last_present : CommitStamp,
  pub vanished_at  : Option<CommitStamp>, // its first-parent descendant, which lacks the file
  pub title        : String,             // the node's title when last present
  pub outbound     : Vec<(&'static str, Vec<ID>)>, // its own nonempty relation lists then
  pub inbound      : Vec<(ID, &'static str)>,      // who referred to it then, and how (links included)
}

#[derive(Clone, Debug)]
pub struct CommitStamp {
  pub short_sha : String,
  pub date      : String, // YYYY-MM-DD, UTC
  pub summary   : String,
}

/// Investigate each id of 'ids' in the git history of every skgrepo:
/// walk each skgrepo's first-parent chain from HEAD looking for the most
/// recent commit whose tree holds '<id>.skg'. A skgrepo that cannot be
/// opened or walked contributes nothing. One walk per skgrepo covers
/// all ids.
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
  let skgrepo_names : Vec<&SkgrepoName> = {
    let mut names : Vec<&SkgrepoName> =
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
  skgrepo_name : &SkgrepoName,
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
    summary   : commit . summary () . ok () . flatten ()
                . unwrap_or ("") . to_string (), } }

/// The whole picture of the id at 'commit' (where its file exists):
/// its own title and outbound lists, plus every other node in that
/// tree that referenced it, by relation -- links included.
fn sighting_at_commit (
  gitrepo        : &Repository,
  prefix      : &Path,
  skgrepo_name : &SkgrepoName,
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
    let subscribes_skgids : MSV<ID> = members_msv (& own . subscribesTo);
    let hides_skgids       : MSV<ID> = members_msv (
      & own . hidesFromSubs);
    let overrides_skgids   : MSV<ID> = members_msv (& own . overrides);
    keep ("contains",                     & contains_skgids);
    keep ("subscribesTo",                subscribes_skgids . or_default ());
    keep ("hidesFromSubs", hides_skgids . or_default ());
    keep ("overrides",            overrides_skgids . or_default ());
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
  skgrepo_name : &SkgrepoName,
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
    note ("subscribesTo",
          node . subscribesTo . or_default () . iter ()
            . any ( |m| &m . member == skgid ));
    note ("hidesFromSubs",
          node . hidesFromSubs . or_default () . iter ()
            . any ( |m| &m . member == skgid ));
    note ("overrides",
          node . overrides . or_default () . iter ()
            . any ( |m| &m . member == skgid ));
    note ("link",
          links_from_node (&node) . iter ()
            . any ( |l| l . skgid == *skgid ));
    TreeWalkResult::Ok
  }) . ok ();
  inbound }

/// Days-to-civil conversion (Howard Hinnant's algorithm), so the
/// answer can stamp commits without a date-time dependency.
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

/// One ID's report as Org text: what git history says it used to be.
pub fn vanished_node_report_as_org (
  report : &VanishedNodeReport,
) -> String {
  let mut out : String = String::new ();
  let stamp = |c : &CommitStamp| -> String {
    format! ("{} ({}, {:?})", c . short_sha, c . date, c . summary) };
  out . push_str (&format! ("* {}\n", report . skgid));
  if report . sightings . is_empty () {
    out . push_str (
      "** never present in the git history of any skgrepo\n");
    return out; }
  for sighting in & report . sightings {
    out . push_str (&format! (
      "** in skgrepo {}\n", sighting . home_skgrepo ));
    out . push_str (&format! (
      "*** title when last present: {}\n", sighting . title ));
    out . push_str (&format! (
      "*** last present at commit {}\n",
      stamp (& sighting . last_present) ));
    match & sighting . vanished_at {
      Some (c) => out . push_str (&format! (
        "*** vanished at commit {}\n", stamp (c) )),
      None => out . push_str (
        "*** still present at HEAD (only the worktree lacks it)\n" ), }
    if sighting . outbound . is_empty () {
      out . push_str ("*** its own relationship lists then: all empty\n");
    } else {
      out . push_str ("*** its own relationship lists then\n");
      for (relation, members) in & sighting . outbound {
        out . push_str (&format! ("**** {}\n", relation));
        for member in members {
          out . push_str (&format! ("***** {}\n", member)); }} }
    if sighting . inbound . is_empty () {
      out . push_str ("*** nothing else referred to it then\n");
    } else {
      out . push_str ("*** referred to then by\n");
      for (referrer, relation) in & sighting . inbound {
        out . push_str (&format! (
          "**** {} (via {})\n", referrer, relation )); }} }
  out }
