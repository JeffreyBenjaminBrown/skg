use crate::diff_report::types::{
  DiffReport, DuplicateIDReport, GraphSnapshot, ListDiffItem, NodeBucket,
  NodeDiffReport, RelationshipDiff, GitSnapshotPair, RepoForReport,
  TextDiffLine, ValueSetDiff};
use crate::types::list::{Diff_Item, compute_interleaved_diff};
use crate::types::misc::{
  ID, MSV, RelPartner, SkgRepoName, members_of, members_msv};
use crate::types::nodes::complete::Graphnode;
use crate::types::links::links_from_node;

use similar::{ChangeTag, TextDiff};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::thread;
use std::time::{Duration, Instant};

pub fn diff_git_snapshots (
  pair : &GitSnapshotPair,
) -> DiffReport {
  diff_git_snapshots_with_pid_filter (pair, None)
}

pub fn diff_git_snapshots_for_pids (
  pair : &GitSnapshotPair,
  pids : &BTreeSet<ID>,
) -> DiffReport {
  diff_git_snapshots_with_pid_filter (pair, Some (pids))
}

fn diff_git_snapshots_with_pid_filter (
  pair      : &GitSnapshotPair,
  only_pids : Option<&BTreeSet<ID>>,
) -> DiffReport {
  let total_start : Instant =
    Instant::now ();
  let duplicate_skgids : Vec<DuplicateIDReport> =
    profile_step ("duplicate_id_reports", || {
      duplicate_skgid_reports (&pair . before, &pair . after) });
  let ambiguous_pids : BTreeSet<ID> =
    profile_step ("ambiguous_pids", || {
      ambiguous_pids (&pair . before, &pair . after, &duplicate_skgids) });
  let (before_facts, after_facts) : (GraphFacts, GraphFacts) =
    graph_facts_for_diff (pair, &ambiguous_pids, only_pids);
  let mut reports : Vec<NodeDiffReport> =
    profile_step ("node_reports", || {
      node_reports (
        pair, &before_facts, &after_facts, &ambiguous_pids, only_pids ) });
  profile_step ("sort reports", || {
    reports . sort_by_key ( |r| r . title . clone () ); });
  let titles : HashMap<ID, String> =
    profile_step ("title_map", || {
      title_map (&pair . before, &pair . after) });
  let buckets : Vec<NodeBucket> =
    profile_step ("bucket_reports", || {
      bucket_reports (reports, &before_facts, &after_facts) });
  profile_log (
    "diff_snapshots total",
    total_start . elapsed ());
  DiffReport {
    duplicate_ids : duplicate_skgids,
    titles,
    buckets,
    vanished : Vec::new () } // filled by the caller (mod.rs), which has the config
}

fn graph_facts_for_diff (
  pair           : &GitSnapshotPair,
  ambiguous_pids : &BTreeSet<ID>,
  only_pids      : Option<&BTreeSet<ID>>,
) -> (GraphFacts, GraphFacts) {
  thread::scope ( |scope| {
    let before_handle : thread::ScopedJoinHandle<'_, GraphFacts> =
      scope . spawn ( || profile_step (
        "before GraphFacts", || {
          GraphFacts::from_git_snapshot_maybe_limited (
            &pair . before, ambiguous_pids, only_pids) }) );
    let after_handle : thread::ScopedJoinHandle<'_, GraphFacts> =
      scope . spawn ( || profile_step (
        "after GraphFacts", || {
          GraphFacts::from_git_snapshot_maybe_limited (
            &pair . after, ambiguous_pids, only_pids) }) );
    ( before_handle . join ()
        . expect ("before GraphFacts panicked"),
      after_handle . join ()
        . expect ("after GraphFacts panicked") ) })
}

fn profile_step<T, F> (
  label : &str,
  f     : F,
) -> T
where
  F : FnOnce () -> T,
{
  let start : Instant =
    Instant::now ();
  let result : T =
    f ();
  profile_log (label, start . elapsed ());
  result
}

fn profile_log (
  label    : &str,
  duration : Duration,
) {
  if std::env::var_os ("SKG_PROFILE_DIFF_REPORT") . is_none () {
    return; }
  eprintln! (
    "diff-report profile: {}: {}.{:03}s",
    label,
    duration . as_secs (),
    duration . subsec_millis ()); }

/// Shape-aware: an id present in several REPOS is the normal
/// telescope shape (one pid, sections at several skgrepos), not a
/// duplicate. The VIOLATION is one id claimed by two distinct pids
/// at either endpoint.
fn duplicate_skgid_reports (
  before : &GraphSnapshot,
  after  : &GraphSnapshot,
) -> Vec<DuplicateIDReport> {
  let skgids : BTreeSet<ID> =
    before . id_claims . keys () . cloned ()
      . chain (after . id_claims . keys () . cloned ())
      . collect ();
  let mut reports : Vec<DuplicateIDReport> =
    Vec::new ();
  for skgid in skgids {
    let claiming_pids = |git_snapshot : &GraphSnapshot| -> usize {
      git_snapshot . id_claims . get (&skgid)
        . map ( |by_pid| by_pid . len () )
        . unwrap_or (0) };
    if claiming_pids (before) <= 1 && claiming_pids (after) <= 1 {
      continue; }
    let title : String =
      title_for_skgid (&skgid, before, after);
    reports . push ( DuplicateIDReport {
      skgid : skgid . clone (),
      before_skgrepos : before . skgrepos_claiming_skgid (&skgid),
      after_skgrepos  : after . skgrepos_claiming_skgid (&skgid),
      title }); }
  reports . sort_by_key ( |r| r . skgid . clone () );
  reports
}

fn ambiguous_pids (
  before     : &GraphSnapshot,
  after      : &GraphSnapshot,
  duplicates : &[DuplicateIDReport],
) -> BTreeSet<ID> {
  let duplicate_skgids : BTreeSet<ID> =
    duplicates . iter () . map ( |d| d . skgid . clone () ) . collect ();
  before . nodes . values ()
    . chain (after . nodes . values ())
    . filter ( |node| node . all_skgids () . any ( |skgid|
      duplicate_skgids . contains (skgid) ) )
    . map ( |node| node . pid . clone () )
    . collect ()
}

fn node_reports (
  pair          : &GitSnapshotPair,
  before_facts  : &GraphFacts,
  after_facts   : &GraphFacts,
  ambiguous_pids : &BTreeSet<ID>,
  only_pids      : Option<&BTreeSet<ID>>,
) -> Vec<NodeDiffReport> {
  let pids : BTreeSet<ID> =
    match only_pids {
      Some (pids) =>
        pids . iter () . cloned ()
          . filter ( |pid| ! ambiguous_pids . contains (pid) )
          . collect (),
      None =>
        pair . before . nodes . keys () . cloned ()
          . chain (pair . after . nodes . keys () . cloned ())
          . filter ( |pid| ! ambiguous_pids . contains (pid) )
          . collect (), };
  let mut reports : Vec<NodeDiffReport> =
    Vec::new ();
  for pid in pids {
    let before_node : Option<&Graphnode> =
      pair . before . nodes . get (&pid);
    let after_node : Option<&Graphnode> =
      pair . after . nodes . get (&pid);
    let title_diff : Option<Vec<TextDiffLine>> =
      text_diff_option (
        before_node . map ( |n| n . title . as_str () ) . unwrap_or (""),
        after_node  . map ( |n| n . title . as_str () ) . unwrap_or (""));
    let body_diff : Option<Vec<TextDiffLine>> =
      text_diff_option (
        before_node . and_then ( |n| n . body . as_deref () ) . unwrap_or (""),
        after_node  . and_then ( |n| n . body . as_deref () ) . unwrap_or (""));
    let skgrepo_change : Option<(SkgRepoName, SkgRepoName)> =
      match (before_node, after_node) {
        (Some (b), Some (a)) if b . home_skgrepo != a . home_skgrepo =>
          Some ((b . home_skgrepo . clone (), a . home_skgrepo . clone ())),
        _ => None, };
    let value_set_diffs : Vec<ValueSetDiff> =
      value_set_diffs (before_node, after_node);
    let relationship_diffs : Vec<RelationshipDiff> =
      relationship_diffs_for_pid (&pid, before_facts, after_facts);
    let content_list_diff : Option<Vec<ListDiffItem>> =
      content_list_diff_for_pid (&pid, before_facts, after_facts);
    let changed : bool =
      before_node . is_none () || after_node . is_none () ||
      title_diff . is_some () ||
      body_diff . is_some () ||
      skgrepo_change . is_some () ||
      value_set_diffs . iter () . any ( |d|
        ! d . lost . is_empty () || ! d . gained . is_empty () ) ||
      relationship_diffs . iter () . any ( |d|
        ! d . lost . is_empty () || ! d . gained . is_empty () ) ||
      content_list_diff . is_some ();
    if ! changed {
      continue; }
    let skgrepo : RepoForReport =
      match (before_node, after_node) {
        (_, Some (a)) => RepoForReport::After (a . home_skgrepo . clone ()),
        (Some (b), None) => RepoForReport::Before (b . home_skgrepo . clone ()),
        (None, None) => continue, };
    let title : String =
      after_node . or (before_node)
        . map ( |n| n . title . clone () )
        . unwrap_or_else ( || "[missing title]" . to_string () );
    reports . push ( NodeDiffReport {
      pid,
      home_skgrepo: skgrepo,
      title,
      title_diff,
      body_diff,
      skgrepo_change,
      value_set_diffs,
      relationship_diffs,
      content_list_diff }); }
  reports
}

fn value_set_diffs (
  before_node : Option<&Graphnode>,
  after_node  : Option<&Graphnode>,
) -> Vec<ValueSetDiff> {
  let mut result : Vec<ValueSetDiff> =
    Vec::new ();
  result . push ( string_set_diff (
    "aliases",
    strings_from_msv (before_node . map ( |n| &n . aliases )),
    strings_from_msv (after_node  . map ( |n| &n . aliases ))));
  result . push ( string_set_diff (
    "extra_id",
    skgids_to_strings (before_node . map ( |n| n . extra_ids . as_slice () )),
    skgids_to_strings (after_node  . map ( |n| n . extra_ids . as_slice () ))));
  result . into_iter () . filter ( |d|
    ! d . lost . is_empty () || ! d . gained . is_empty () )
    . collect ()
}

fn strings_from_msv (
  msv : Option<&MSV<RelPartner<String>>>,
) -> Vec<String> {
  msv . map ( |m| members_msv (m) . or_default () . to_vec () )
    . unwrap_or_default ()
}

fn skgids_to_strings (
  skgids : Option<&[ID]>,
) -> Vec<String> {
  skgids . unwrap_or (&[])
    . iter ()
    . map ( |skgid| skgid . to_string () )
    . collect ()
}

fn string_set_diff (
  name   : &'static str,
  before : Vec<String>,
  after  : Vec<String>,
) -> ValueSetDiff {
  let before_set : BTreeSet<String> =
    before . into_iter () . collect ();
  let after_set : BTreeSet<String> =
    after . into_iter () . collect ();
  ValueSetDiff {
    name,
    lost: before_set . difference (&after_set) . cloned () . collect (),
    gained: after_set . difference (&before_set) . cloned () . collect () }
}

fn relationship_diffs_for_pid (
  pid          : &ID,
  before_facts : &GraphFacts,
  after_facts  : &GraphFacts,
) -> Vec<RelationshipDiff> {
  ROLE_NAMES . iter ()
    . filter_map ( |role| {
      let before : BTreeSet<ID> =
        before_facts . role_sets . get (*role)
          . and_then ( |m| m . get (pid) )
          . cloned () . unwrap_or_default ();
      let after : BTreeSet<ID> =
        after_facts . role_sets . get (*role)
          . and_then ( |m| m . get (pid) )
          . cloned () . unwrap_or_default ();
      let lost : Vec<ID> =
        before . difference (&after) . cloned () . collect ();
      let gained : Vec<ID> =
        after . difference (&before) . cloned () . collect ();
      let unchanged : Vec<ID> =
        if is_backward_relationship_role (role) {
          before . intersection (&after) . cloned () . collect ()
        } else {
          Vec::new () };
      if lost . is_empty () && gained . is_empty () {
        None
      } else {
        Some ( RelationshipDiff { role, lost, gained, unchanged } ) } } )
    . collect ()
}

fn is_backward_relationship_role (
  role : &str,
) -> bool {
  matches! (
    role,
    "container" | "subscribee" | "hidden" | "overridden" | "mentioned" )
}

fn content_list_diff_for_pid (
  pid          : &ID,
  before_facts : &GraphFacts,
  after_facts  : &GraphFacts,
) -> Option<Vec<ListDiffItem>> {
  let before : Vec<ID> =
    before_facts . contained_order . get (pid)
      . cloned () . unwrap_or_default ();
  let after : Vec<ID> =
    after_facts . contained_order . get (pid)
      . cloned () . unwrap_or_default ();
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (&before, &after);
  let changed : bool =
    diff . iter () . any ( |item|
      ! matches! (item, Diff_Item::Unchanged (_)) );
  if ! changed {
    return None; }
  Some ( diff . into_iter () . map ( |item| match item {
    Diff_Item::Unchanged (skgid) => ListDiffItem::Unchanged (skgid),
    Diff_Item::Removed   (skgid) => ListDiffItem::Removed   (skgid),
    Diff_Item::New       (skgid) => ListDiffItem::Added     (skgid), } )
    . collect () )
}

fn text_diff_option (
  before : &str,
  after  : &str,
) -> Option<Vec<TextDiffLine>> {
  if before == after {
    return None; }
  let diff : TextDiff<str> =
    TextDiff::from_lines (before, after);
  let mut lines : Vec<TextDiffLine> =
    Vec::new ();
  for change in diff . iter_all_changes () {
    let value : String =
      change . value () . trim_end_matches ('\n') . to_string ();
    match change . tag () {
      ChangeTag::Equal =>
        lines . push (TextDiffLine::Unchanged (value)),
      ChangeTag::Delete =>
        lines . push (TextDiffLine::Removed (value)),
      ChangeTag::Insert =>
        lines . push (TextDiffLine::Added (value)), } }
  Some (lines)
}

fn bucket_reports (
  reports      : Vec<NodeDiffReport>,
  before_facts : &GraphFacts,
  after_facts  : &GraphFacts,
) -> Vec<NodeBucket> {
  let mut buckets : BTreeMap<&'static str, Vec<NodeDiffReport>> =
    BUCKET_NAMES . iter () . map ( |name| (*name, Vec::new ()) )
      . collect ();
  for report in reports {
    let existed_before : bool =
      before_facts . existing_pids . contains (&report . pid);
    let exists_after : bool =
      after_facts . existing_pids . contains (&report . pid);
    let root_before : bool =
      ! before_facts . role_sets ["container"] . contains_key (&report . pid);
    let root_after : bool =
      ! after_facts . role_sets ["container"] . contains_key (&report . pid);
    let pid_preserved_as_extra_id : bool =
      after_facts . extra_ids . contains (&report . pid);
    let moved_across_skgrepos : bool =
      report . skgrepo_change . is_some ();
    let bucket : &'static str =
      match (existed_before, exists_after, root_before, root_after) {
        (true, true, false, true) => "modified, newly orphaned",
        (true, true, _,     _) if moved_across_skgrepos =>
          "modified, moved across repos",
        (true, true, _,     _)    => "modified, other",
        (false, true, _,    true) => "new roots",
        (true, false, _,     _) if pid_preserved_as_extra_id =>
          "deleted nodes, probably via merger",
        (true, false, true, _)    => "deleted roots",
        (true, false, false, _)   => "deleted nodes, not roots",
        (false, true, _,    false) => "new nodes, not roots",
        (false, false, _, _)      => continue, };
    buckets . get_mut (bucket) . unwrap () . push (report); }
  BUCKET_NAMES . iter () . map ( |name| {
    let mut nodes : Vec<NodeDiffReport> =
      buckets . remove (name) . unwrap_or_default ();
    nodes . sort_by_key ( |r| (r . title . clone (), r . pid . clone ()) );
    NodeBucket { name, nodes } } )
    . collect ()
}

fn title_for_skgid (
  skgid  : &ID,
  before : &GraphSnapshot,
  after  : &GraphSnapshot,
) -> String {
  after . nodes . values ()
    . chain (before . nodes . values ())
    . find ( |node| node . all_skgids () . any ( |x| x == skgid ) )
    . map ( |node| node . title . clone () )
    . unwrap_or_else ( || "[missing title]" . to_string () )
}

fn title_map (
  before : &GraphSnapshot,
  after  : &GraphSnapshot,
) -> HashMap<ID, String> {
  let mut map : HashMap<ID, String> =
    HashMap::new ();
  for node in before . nodes . values () {
    for skgid in node . all_skgids () {
      map . insert (skgid . clone (), node . title . clone ()); }}
  for node in after . nodes . values () {
    for skgid in node . all_skgids () {
      map . insert (skgid . clone (), node . title . clone ()); }}
  map
}

struct GraphFacts {
  existing_pids    : BTreeSet<ID>,
  extra_ids        : BTreeSet<ID>,
  role_sets        : HashMap<&'static str, HashMap<ID, BTreeSet<ID>>>,
  contained_order  : HashMap<ID, Vec<ID>>,
}

impl GraphFacts {
  fn from_git_snapshot_maybe_limited (
    git_snapshot       : &GraphSnapshot,
    ambiguous_pids : &BTreeSet<ID>,
    only_pids      : Option<&BTreeSet<ID>>,
  ) -> Self {
    match only_pids {
      Some (pids) =>
        Self::from_git_snapshot_for_pids (git_snapshot, ambiguous_pids, pids),
      None =>
        Self::from_git_snapshot (git_snapshot, ambiguous_pids), }
  }

  fn from_git_snapshot (
    git_snapshot       : &GraphSnapshot,
    ambiguous_pids : &BTreeSet<ID>,
  ) -> Self {
    let mut facts : GraphFacts =
      GraphFacts {
        existing_pids: git_snapshot . nodes . keys ()
          . filter ( |pid| ! ambiguous_pids . contains (*pid) )
          . cloned () . collect (),
        extra_ids: BTreeSet::new (),
        role_sets: ROLE_NAMES . iter ()
          . map ( |role| (*role, HashMap::new ()) )
          . collect (),
        contained_order: HashMap::new () };
    for node in git_snapshot . nodes . values () {
      if ambiguous_pids . contains (&node . pid) {
        continue; }
      facts . extra_ids . extend (
        node . extra_ids . iter ()
          . filter ( |skgid| ! ambiguous_pids . contains (*skgid) )
          . cloned () );
      let contains_skgids : Vec<ID> =
        members_of (&node . contains);
      facts . contained_order . insert (
        node . pid . clone (),
        contains_skgids . iter ()
          . filter ( |skgid| ! ambiguous_pids . contains (*skgid) )
          . cloned () . collect () );
      for contained in &contains_skgids {
        if ambiguous_pids . contains (contained) {
          continue; }
        facts . add_relationship ("content", &node . pid, contained);
        facts . add_relationship ("container", contained, &node . pid); }
      for subscribee in members_msv (&node . subscribesTo) . or_default () {
        if ambiguous_pids . contains (subscribee) {
          continue; }
        facts . add_relationship ("subscriber", &node . pid, subscribee);
        facts . add_relationship ("subscribee", subscribee, &node . pid); }
      for hidden in members_msv (&node . hidesFromSubs) . or_default () {
        if ambiguous_pids . contains (hidden) {
          continue; }
        facts . add_relationship ("hider", &node . pid, hidden);
        facts . add_relationship ("hidden", hidden, &node . pid); }
      for overridden in members_msv (&node . overrides) . or_default () {
        if ambiguous_pids . contains (overridden) {
          continue; }
        facts . add_relationship ("overrider", &node . pid, overridden);
        facts . add_relationship ("overridden", overridden, &node . pid); }
      for link in links_from_node (node) {
        if ambiguous_pids . contains (&link . skgid) {
          continue; }
        facts . add_relationship ("mentioner", &node . pid, &link . skgid);
        facts . add_relationship ("mentioned", &link . skgid, &node . pid); } }
    facts
  }

  fn from_git_snapshot_for_pids (
    git_snapshot       : &GraphSnapshot,
    ambiguous_pids : &BTreeSet<ID>,
    pids           : &BTreeSet<ID>,
  ) -> Self {
    let tracked_pids : BTreeSet<ID> =
      pids . difference (ambiguous_pids) . cloned () . collect ();
    let mut facts : GraphFacts =
      GraphFacts {
        existing_pids: tracked_pids . iter ()
          . filter ( |pid| git_snapshot . nodes . contains_key (*pid) )
          . cloned () . collect (),
        extra_ids: BTreeSet::new (),
        role_sets: ROLE_NAMES . iter ()
          . map ( |role| (*role, HashMap::new ()) )
          . collect (),
        contained_order: HashMap::new () };
    for node in git_snapshot . nodes . values () {
      if ambiguous_pids . contains (&node . pid) {
        continue; }
      let track_outbound : bool =
        tracked_pids . contains (&node . pid);
      let contains_skgids : Vec<ID> =
        members_of (&node . contains);
      if track_outbound {
        facts . extra_ids . extend (
          node . extra_ids . iter ()
            . filter ( |skgid| ! ambiguous_pids . contains (*skgid) )
            . cloned () );
        facts . contained_order . insert (
          node . pid . clone (),
          contains_skgids . iter ()
            . filter ( |skgid| ! ambiguous_pids . contains (*skgid) )
            . cloned () . collect () ); }
      facts . extra_ids . extend (
        node . extra_ids . iter ()
          . filter ( |skgid| tracked_pids . contains (*skgid) )
          . cloned () );
      for contained in &contains_skgids {
        if ambiguous_pids . contains (contained) {
          continue; }
        if track_outbound {
          facts . add_relationship ("content", &node . pid, contained); }
        if tracked_pids . contains (contained) {
          facts . add_relationship ("container", contained, &node . pid); }}
      for subscribee in members_msv (&node . subscribesTo) . or_default () {
        if ambiguous_pids . contains (subscribee) {
          continue; }
        if track_outbound {
          facts . add_relationship ("subscriber", &node . pid, subscribee); }
        if tracked_pids . contains (subscribee) {
          facts . add_relationship ("subscribee", subscribee, &node . pid); }}
      for hidden in members_msv (&node . hidesFromSubs) . or_default () {
        if ambiguous_pids . contains (hidden) {
          continue; }
        if track_outbound {
          facts . add_relationship ("hider", &node . pid, hidden); }
        if tracked_pids . contains (hidden) {
          facts . add_relationship ("hidden", hidden, &node . pid); }}
      for overridden in members_msv (&node . overrides) . or_default () {
        if ambiguous_pids . contains (overridden) {
          continue; }
        if track_outbound {
          facts . add_relationship ("overrider", &node . pid, overridden); }
        if tracked_pids . contains (overridden) {
          facts . add_relationship ("overridden", overridden, &node . pid); }}
      for link in links_from_node (node) {
        if ambiguous_pids . contains (&link . skgid) {
          continue; }
        if track_outbound {
          facts . add_relationship ("mentioner", &node . pid, &link . skgid); }
        if tracked_pids . contains (&link . skgid) {
          facts . add_relationship ("mentioned", &link . skgid, &node . pid); }}}
    facts
  }

  fn add_relationship (
    &mut self,
    role : &'static str,
    from : &ID,
    to   : &ID,
  ) {
    self . role_sets . get_mut (role) . unwrap ()
      . entry (from . clone ())
      . or_insert_with (BTreeSet::new)
      . insert (to . clone ());
  }
}

const ROLE_NAMES : &[&str] = &[
  "content",
  "container",
  "subscriber",
  "subscribee",
  "hider",
  "hidden",
  "overrider",
  "overridden",
  "mentioner",
  "mentioned",
];

const BUCKET_NAMES : &[&str] = &[
  "modified, newly orphaned",
  "new roots",
  "deleted roots",
  "deleted nodes, not roots",
  "deleted nodes, probably via merger",
  "modified, moved across repos",
  "modified, other",
  "new nodes, not roots",
];
