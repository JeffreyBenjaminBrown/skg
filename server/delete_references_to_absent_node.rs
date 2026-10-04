//! Pure scanning and rewriting for `skg-delete-references-to-absent-node`.
//!
//! Transport deliberately does not live here: an approval is always compared
//! with a freshly scanned `Preview`, and rewrite instructions are rebuilt from
//! that same snapshot rather than from client-supplied text.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::save::nodecomplete_from_noderust;
use crate::types::misc::{ID, RelPartner, MSV, SkgConfig, RepoName};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::links::links_with_ranges_from_text;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum StructuredField {
  Contains,
  SubscribesTo,
  HidesFromItsSubscriptions,
  OverridesViewOf,
}

impl StructuredField {
  pub fn label (self) -> &'static str {
    match self {
      Self::Contains => "contains",
      Self::SubscribesTo => "subscribes_to",
      Self::HidesFromItsSubscriptions => "hides_from_its_subscriptions",
      Self::OverridesViewOf => "overrides_view_of", } }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructuralOccurrence {
  pub owner_pid    : ID,
  pub owner_repo : RepoName,
  pub owner_title  : String,
  pub field        : StructuredField,
  pub raw_id       : ID,
  pub relRepo : RepoName,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum TextField { Title, Body }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LinkOccurrence {
  pub owner_pid    : ID,
  pub owner_repo : RepoName,
  pub field        : TextField,
  pub line         : usize,
  pub label        : String,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Preview {
  pub raw_id       : ID,
  pub structural   : Vec<StructuralOccurrence>,
  pub links   : Vec<LinkOccurrence>,
}

impl Preview {
  pub fn changed_nodes (&self) -> usize {
    self . structural . iter ()
      . map (|o| o . owner_pid . clone ())
      . collect::<std::collections::BTreeSet<ID>> () . len ()
  }

  pub fn opaque_approval (&self) -> String {
    // Length-prefix every string, so this remains lossless even for titles or
    // labels containing tabs/newlines.  It is intentionally opaque to
    // clients: they echo it, never interpret it as rewrite instructions.
    let encode = |s : &str| format! ("{}:{}", s . len (), s);
    let mut rows : Vec<String> = Vec::new ();
    for o in &self . structural {
      rows . push (format! ("S{}{}{}{}{}{}",
        encode (&o . owner_pid . 0), encode (&o . owner_repo . 0),
        encode (&o . owner_title), encode (o . field . label ()),
        encode (&o . raw_id . 0), encode (&o . relRepo . 0))); }
    for o in &self . links {
      rows . push (format! ("T{}{}{:?}:{}{}",
        encode (&o . owner_pid . 0), encode (&o . owner_repo . 0),
        o . field, o . line, encode (&o . label))); }
    // Rows are self-delimiting: their leading kind and length-prefixed fields
    // make a separator unnecessary.  In particular, an actual newline would
    // be escaped by the S-expression transport and would no longer compare
    // equal after a client echoed this opaque token.
    rows . concat ()
  }
}

pub fn preview_warning_org (
  preview : &Preview,
) -> String {
  let mut text : String = format! (
    "* References to absent ID {}\n\n", preview . raw_id);
  if ! preview . structural . is_empty () {
    text . push_str ("** Structured relationships to remove\n");
    for o in &preview . structural {
      text . push_str (&format! ("- {} / {} / {} (source {})\n",
        o . owner_pid, o . field . label (), o . raw_id, o . relRepo)); }}
  if ! preview . links . is_empty () {
    text . push_str ("** Text links left unchanged\n");
    for o in &preview . links {
      text . push_str (&format! ("- {} {:?} line {}: {}\n",
        o . owner_pid, o . field, o . line, o . label)); }}
  text
}

pub fn result_org (
  preview : &Preview,
) -> String {
  let mut result = format! (
    "* Absent-reference cleanup complete\n\nRemoved {} structured membership(s) from {} owned telescope(s). Text links were left unchanged.\n",
    preview . structural . len (), preview . changed_nodes ());
  for occurrence in &preview . structural {
    result . push_str (&format! ("- {}: {} in source {}\n",
      occurrence . owner_pid, occurrence . field . label (),
      occurrence . relRepo)); }
  result
}

/// Snapshot every owned retained telescope which contains `raw_id` exactly.
/// A primary/extra alias is intentionally *not* equivalent for this command.
pub fn preview (
  graph  : &InRustGraph,
  config : &SkgConfig,
  raw_id : &ID,
) -> Result<Preview, String> {
  if graph . pid_of (raw_id) . is_some () {
    return Err (format! (
      "Cannot remove references to {}: it currently resolves to a graph node.", raw_id)); }
  let mut result : Preview = Preview { raw_id : raw_id . clone (), ..Preview::default () };
  for node in graph . nodes . values () {
    if ! config . user_owns_repo (&node . home_repo) { continue; }
    let mut record = |field : StructuredField, members : &[RelPartner<ID>]| {
      for member in members . iter () . filter (|m| &m . member == raw_id) {
        result . structural . push (StructuralOccurrence {
          owner_pid       : node . pid . clone (),
          owner_repo    : node . home_repo . clone (),
          owner_title     : node . title . clone (),
          field,
          raw_id          : raw_id . clone (),
          relRepo : member . relRepo . clone (), }); }};
    record (StructuredField::Contains, &node . contains);
    record (StructuredField::SubscribesTo, node . subscribes_to . or_default ());
    record (StructuredField::HidesFromItsSubscriptions,
            node . hides_from_its_subscriptions . or_default ());
    record (StructuredField::OverridesViewOf,
            node . overrides_view_of . or_default ());
    record_links (&mut result . links, node, raw_id, TextField::Title,
                       &node . title);
    if let Some (body) = &node . body {
      record_links (&mut result . links, node, raw_id, TextField::Body,
                         body); }}
  result . structural . sort_by (|a, b|
    (&a . owner_pid, a . field, &a . relRepo)
      . cmp (&(&b . owner_pid, b . field, &b . relRepo)));
  result . links . sort_by (|a, b|
    (&a . owner_pid, a . field, a . line, &a . label)
      . cmp (&(&b . owner_pid, b . field, b . line, &b . label)));
  Ok (result)
}

fn record_links (
  occurrences : &mut Vec<LinkOccurrence>,
  node        : &crate::types::nodes::rust::NodeRust,
  raw_id      : &ID,
  field       : TextField,
  text        : &str,
) {
  for (range, link) in links_with_ranges_from_text (text) . into_iter ()
    . filter (|(_, link)| &link . id == raw_id) {
    let start : usize = range . start;
    occurrences . push (LinkOccurrence {
      owner_pid    : node . pid . clone (),
      owner_repo : node . home_repo . clone (),
      field,
      line         : text [..start] . bytes () . filter (|b| *b == b'\n') . count () + 1,
      label        : link . label, }); }
}

/// Build one verbatim SaveNode per owned affected telescope.  It removes only
/// exact raw-ID matches and preserves all list order, relRepos, and
/// MSV shapes.  The caller must use the same fresh snapshot it previewed.
pub fn rewrite (
  graph  : &InRustGraph,
  config : &SkgConfig,
  preview : &Preview,
) -> Result<Vec<DefineNode>, String> {
  let fresh : Preview = self::preview (graph, config, &preview . raw_id) ?;
  if &fresh != preview {
    return Err ("Cleanup preview is stale; rescan before rewriting." . to_string ()); }
  let affected : std::collections::BTreeSet<ID> = fresh . structural . iter ()
    . map (|o| o . owner_pid . clone ()) . collect ();
  let mut writes : Vec<DefineNode> = Vec::new ();
  for pid in affected {
    let node = graph . get (&pid)
      .ok_or_else (|| format! ("Cleanup owner disappeared: {}", pid))?;
    let mut rewritten = nodecomplete_from_noderust (node);
    rewritten . contains . retain (|m| m . member != fresh . raw_id);
    rewritten . subscribes_to = remove_exact (
      &rewritten . subscribes_to, &fresh . raw_id);
    rewritten . hides_from_its_subscriptions = remove_exact (
      &rewritten . hides_from_its_subscriptions, &fresh . raw_id);
    rewritten . overrides_view_of = remove_exact (
      &rewritten . overrides_view_of, &fresh . raw_id);
    writes . push (DefineNode::Save (SaveNode (rewritten))); }
  Ok (writes)
}

fn remove_exact (
  members : &MSV<RelPartner<ID>>,
  raw_id  : &ID,
) -> MSV<RelPartner<ID>> {
  match members {
    MSV::Unspecified => MSV::Unspecified,
    MSV::Specified (members) => MSV::Specified (
      members . iter () . filter (|m| &m . member != raw_id)
        . cloned () . collect ()), }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{SkgfileRepo};
  use crate::types::nodes::complete::{empty_node_complete, NodeComplete};
  use std::collections::HashMap;
  use std::path::PathBuf;

  fn id (text : &str) -> ID { ID::from (text) }
  fn member (repo : &str, raw : &str) -> RelPartner<ID> {
    RelPartner::at_relRepo (RepoName::from (repo), id (raw)) }

  fn config () -> SkgConfig {
    let repo = |name : &str, owned : bool| SkgfileRepo {
      name: RepoName::from (name), abbreviation: None,
      path: PathBuf::from (if owned { "owned/main" } else { "foreign/other" }),
      user_owns_it: owned };
    SkgConfig::dummyFromRepos (HashMap::from ([
      (RepoName::from ("main"), repo ("main", true)),
      (RepoName::from ("foreign"), repo ("foreign", false)),
    ]))
  }

  fn node (pid : &str, repo : &str) -> NodeComplete {
    let mut node : NodeComplete = empty_node_complete ();
    node . pid = id (pid);
    node . home_repo = RepoName::from (repo);
    node . title = format! ("{} [[id:gone][title label]]", pid);
    node . body = Some ( // the verbatim example is not a reference
      "line one\n=[[id:gone][body label]]= example\n[[id:gone][body label]]"
      . to_string ());
    node
  }

  #[test]
  fn scans_owned_exact_members_and_leaves_everything_else_verbatim () {
    let mut owned = node ("owned", "main");
    owned . contains = vec! [member ("main", "gone"), member ("main", "keep")];
    owned . subscribes_to = MSV::Specified (vec! [member ("main", "gone")]);
    owned . hides_from_its_subscriptions =
      MSV::Specified (vec! [member ("private", "gone")]);
    owned . overrides_view_of = MSV::Specified (vec! [member ("main", "gone")]);
    let mut foreign = node ("foreign", "foreign");
    foreign . contains = vec! [member ("foreign", "gone")];
    let graph = InRustGraph::from_nodecompletes (&[owned, foreign]);

    let scanned = preview (&graph, &config (), &id ("gone")) . unwrap ();
    assert_eq! (scanned . structural . len (), 4);
    assert_eq! (scanned . links . len (), 2);
    assert! (scanned . structural . iter ()
              . all (|occurrence| occurrence . owner_pid == id ("owned")));
    assert_eq! (scanned . links [1] . line, 3);
    let approval = scanned . opaque_approval ();
    assert_eq! (approval, preview (&graph, &config (), &id ("gone"))
                . unwrap () . opaque_approval ());

    let rewrites = rewrite (&graph, &config (), &scanned) . unwrap ();
    assert_eq! (rewrites . len (), 1, "one SaveNode per affected owner");
    let DefineNode::Save (SaveNode (rewritten)) = &rewrites [0] else {
      panic! ("cleanup must produce a SaveNode"); };
    assert_eq! (rewritten . contains, vec! [member ("main", "keep")]);
    assert_eq! (rewritten . subscribes_to, MSV::Specified (Vec::new ()));
    assert_eq! (rewritten . hides_from_its_subscriptions,
                MSV::Specified (Vec::new ()));
    assert_eq! (rewritten . overrides_view_of, MSV::Specified (Vec::new ()));
    assert_eq! (graph . get (&id ("foreign")) . unwrap () . contains,
                vec! [member ("foreign", "gone")]);
  }

  #[test]
  fn rejects_a_raw_id_that_currently_resolves () {
    let graph = InRustGraph::from_nodecompletes (&[node ("gone", "main")]);
    assert! (preview (&graph, &config (), &id ("gone")) . is_err ());
  }

  #[test]
  fn stale_preview_produces_no_rewrite_instructions () {
    let mut owned = node ("owned", "main");
    owned . contains = vec! [member ("main", "gone")];
    let graph = InRustGraph::from_nodecompletes (&[owned]);
    let scanned = preview (&graph, &config (), &id ("gone")) . unwrap ();
    let mut changed = graph . clone ();
    changed . nodes . get_mut (&id ("owned")) . unwrap () . title =
      "changed after preview" . to_string ();
    assert_eq! (rewrite (&changed, &config (), &scanned),
                Err ("Cleanup preview is stale; rescan before rewriting." . to_string ()));
  }
}
