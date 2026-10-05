//! Pure scanning and rewriting for `skg-delete-references-to-absent-node`.
//!
//! Transport deliberately does not live here: an approval is always compared
//! with a freshly scanned `Preview`, and rewrite instructions are rebuilt from
//! that same graph snapshot rather than from client-supplied text.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::save::graphnode_from_graphnodeInRust;
use crate::types::misc::{ID, RelPartner, MSV, SkgConfig, SkgrepoName};
use crate::types::save::{NodeInstruction, SaveNode};
use crate::types::links::links_with_ranges_from_text;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum StructuredField {
  Contains,
  SubscribesTo,
  HidesFromSubs,
  Overrides,
}

impl StructuredField {
  pub fn label (self) -> &'static str {
    match self {
      Self::Contains => "contains",
      Self::SubscribesTo => "subscribesTo",
      Self::HidesFromSubs => "hidesFromSubs",
      Self::Overrides => "overrides", } }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructuralOccurrence {
  pub recorder_pid     : ID,
  pub recorder_skgrepo : SkgrepoName,
  pub recorder_title   : String,
  pub field            : StructuredField,
  pub raw_skgid        : ID,
  pub relRepo          : SkgrepoName,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum TextField { Title, Body }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LinkOccurrence {
  pub recorder_pid     : ID,
  pub recorder_skgrepo : SkgrepoName,
  pub field            : TextField,
  pub line             : usize,
  pub label            : String,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Preview {
  pub raw_skgid       : ID,
  pub structural   : Vec<StructuralOccurrence>,
  pub links   : Vec<LinkOccurrence>,
}

impl Preview {
  pub fn changed_nodes (&self) -> usize {
    self . structural . iter ()
      . map (|o| o . recorder_pid . clone ())
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
        encode (&o . recorder_pid . 0), encode (&o . recorder_skgrepo . 0),
        encode (&o . recorder_title), encode (o . field . label ()),
        encode (&o . raw_skgid . 0), encode (&o . relRepo . 0))); }
    for o in &self . links {
      rows . push (format! ("T{}{}{:?}:{}{}",
        encode (&o . recorder_pid . 0), encode (&o . recorder_skgrepo . 0),
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
    "* References to absent ID {}\n\n", preview . raw_skgid);
  if ! preview . structural . is_empty () {
    text . push_str ("** Structured relationships to remove\n");
    for o in &preview . structural {
      text . push_str (&format! ("- {} / {} / {} (repo {})\n",
        o . recorder_pid, o . field . label (), o . raw_skgid, o . relRepo)); }}
  if ! preview . links . is_empty () {
    text . push_str ("** Text links left unchanged\n");
    for o in &preview . links {
      text . push_str (&format! ("- {} {:?} line {}: {}\n",
        o . recorder_pid, o . field, o . line, o . label)); }}
  text
}

pub fn result_org (
  preview : &Preview,
) -> String {
  let mut result = format! (
    "* Absent-reference cleanup complete\n\nRemoved {} structured membership(s) from {} owned telescope(s). Text links were left unchanged.\n",
    preview . structural . len (), preview . changed_nodes ());
  for occurrence in &preview . structural {
    result . push_str (&format! ("- {}: {} in repo {}\n",
      occurrence . recorder_pid, occurrence . field . label (),
      occurrence . relRepo)); }
  result
}

/// Snapshot every owned retained telescope which contains `raw_id` exactly.
/// A primary/extra alias is intentionally *not* equivalent for this command.
pub fn preview (
  graph     : &InRustGraph,
  config    : &SkgConfig,
  raw_skgid : &ID,
) -> Result<Preview, String> {
  if graph . pid_of (raw_skgid) . is_some () {
    return Err (format! (
      "Cannot remove references to {}: it currently resolves to a graphnode.", raw_skgid)); }
  let mut result : Preview = Preview { raw_skgid : raw_skgid . clone (), ..Preview::default () };
  for node in graph . nodes . values () {
    if ! config . skgrepo_is_owned (&node . home_skgrepo) { continue; }
    let mut record = |field : StructuredField, members : &[RelPartner<ID>]| {
      for member in members . iter () . filter (|m| &m . member == raw_skgid) {
        result . structural . push (StructuralOccurrence {
          recorder_pid       : node . pid . clone (),
          recorder_skgrepo    : node . home_skgrepo . clone (),
          recorder_title     : node . title . clone (),
          field,
          raw_skgid          : raw_skgid . clone (),
          relRepo : member . relRepo . clone (), }); }};
    record (StructuredField::Contains, &node . contains);
    record (StructuredField::SubscribesTo, node . subscribesTo . or_default ());
    record (StructuredField::HidesFromSubs,
            node . hidesFromSubs . or_default ());
    record (StructuredField::Overrides,
            node . overrides . or_default ());
    record_links (&mut result . links, node, raw_skgid, TextField::Title,
                       &node . title);
    if let Some (body) = &node . body {
      record_links (&mut result . links, node, raw_skgid, TextField::Body,
                         body); }}
  result . structural . sort_by (|a, b|
    (&a . recorder_pid, a . field, &a . relRepo)
      . cmp (&(&b . recorder_pid, b . field, &b . relRepo)));
  result . links . sort_by (|a, b|
    (&a . recorder_pid, a . field, a . line, &a . label)
      . cmp (&(&b . recorder_pid, b . field, b . line, &b . label)));
  Ok (result)
}

fn record_links (
  occurrences : &mut Vec<LinkOccurrence>,
  node        : &crate::types::nodes::rust::GraphnodeInRust,
  raw_skgid   : &ID,
  field       : TextField,
  text        : &str,
) {
  for (range, link) in links_with_ranges_from_text (text) . into_iter ()
    . filter (|(_, link)| &link . skgid == raw_skgid) {
    let start : usize = range . start;
    occurrences . push (LinkOccurrence {
      recorder_pid    : node . pid . clone (),
      recorder_skgrepo : node . home_skgrepo . clone (),
      field,
      line         : text [..start] . bytes () . filter (|b| *b == b'\n') . count () + 1,
      label        : link . label, }); }
}

/// Build one verbatim SaveNode per owned affected telescope.  It removes only
/// exact raw-ID matches and preserves all list order, relRepos, and
/// MSV shapes.  The caller must use the same fresh graph snapshot it previewed.
pub fn rewrite (
  graph  : &InRustGraph,
  config : &SkgConfig,
  preview : &Preview,
) -> Result<Vec<NodeInstruction>, String> {
  let fresh : Preview = self::preview (graph, config, &preview . raw_skgid) ?;
  if &fresh != preview {
    return Err ("Cleanup preview is stale; rescan before rewriting." . to_string ()); }
  let affected : std::collections::BTreeSet<ID> = fresh . structural . iter ()
    . map (|o| o . recorder_pid . clone ()) . collect ();
  let mut writes : Vec<NodeInstruction> = Vec::new ();
  for pid in affected {
    let node = graph . get (&pid)
      .ok_or_else (|| format! ("Cleanup recorder disappeared: {}", pid))?;
    let mut rewritten = graphnode_from_graphnodeInRust (node);
    rewritten . contains . retain (|m| m . member != fresh . raw_skgid);
    rewritten . subscribesTo = remove_exact (
      &rewritten . subscribesTo, &fresh . raw_skgid);
    rewritten . hidesFromSubs = remove_exact (
      &rewritten . hidesFromSubs, &fresh . raw_skgid);
    rewritten . overrides = remove_exact (
      &rewritten . overrides, &fresh . raw_skgid);
    writes . push (NodeInstruction::Save (SaveNode (rewritten))); }
  Ok (writes)
}

fn remove_exact (
  members : &MSV<RelPartner<ID>>,
  raw_skgid  : &ID,
) -> MSV<RelPartner<ID>> {
  match members {
    MSV::Unspecified => MSV::Unspecified,
    MSV::Specified (members) => MSV::Specified (
      members . iter () . filter (|m| &m . member != raw_skgid)
        . cloned () . collect ()), }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{Skgrepo};
  use crate::types::nodes::complete::{empty_graphnode, Graphnode};
  use std::collections::HashMap;
  use std::path::PathBuf;

  fn skgid (text : &str) -> ID { ID::from (text) }
  fn member (skgrepo : &str, raw : &str) -> RelPartner<ID> {
    RelPartner::at_relRepo (SkgrepoName::from (skgrepo), skgid (raw)) }

  fn config () -> SkgConfig {
    let skgrepo = |name : &str, owned : bool| Skgrepo {
      name: SkgrepoName::from (name), abbreviation: None,
      path: PathBuf::from (if owned { "owned/main" } else { "foreign/other" }),
      owned: owned };
    SkgConfig::dummyFromSkgrepos (HashMap::from ([
      (SkgrepoName::from ("main"), skgrepo ("main", true)),
      (SkgrepoName::from ("foreign"), skgrepo ("foreign", false)),
    ]))
  }

  fn node (pid : &str, skgrepo : &str) -> Graphnode {
    let mut node : Graphnode = empty_graphnode ();
    node . pid = skgid (pid);
    node . home_skgrepo = SkgrepoName::from (skgrepo);
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
    owned . subscribesTo = MSV::Specified (vec! [member ("main", "gone")]);
    owned . hidesFromSubs =
      MSV::Specified (vec! [member ("private", "gone")]);
    owned . overrides = MSV::Specified (vec! [member ("main", "gone")]);
    let mut foreign = node ("foreign", "foreign");
    foreign . contains = vec! [member ("foreign", "gone")];
    let graph = InRustGraph::from_graphnodes (&[owned, foreign]);

    let scanned = preview (&graph, &config (), &skgid ("gone")) . unwrap ();
    assert_eq! (scanned . structural . len (), 4);
    assert_eq! (scanned . links . len (), 2);
    assert! (scanned . structural . iter ()
              . all (|occurrence| occurrence . recorder_pid == skgid ("owned")));
    assert_eq! (scanned . links [1] . line, 3);
    let approval = scanned . opaque_approval ();
    assert_eq! (approval, preview (&graph, &config (), &skgid ("gone"))
                . unwrap () . opaque_approval ());

    let rewrites = rewrite (&graph, &config (), &scanned) . unwrap ();
    assert_eq! (rewrites . len (), 1, "one SaveNode per affected recorder");
    let NodeInstruction::Save (SaveNode (rewritten)) = &rewrites [0] else {
      panic! ("cleanup must produce a SaveNode"); };
    assert_eq! (rewritten . contains, vec! [member ("main", "keep")]);
    assert_eq! (rewritten . subscribesTo, MSV::Specified (Vec::new ()));
    assert_eq! (rewritten . hidesFromSubs,
                MSV::Specified (Vec::new ()));
    assert_eq! (rewritten . overrides, MSV::Specified (Vec::new ()));
    assert_eq! (graph . get (&skgid ("foreign")) . unwrap () . contains,
                vec! [member ("foreign", "gone")]);
  }

  #[test]
  fn rejects_a_raw_skgid_that_currently_resolves () {
    let graph = InRustGraph::from_graphnodes (&[node ("gone", "main")]);
    assert! (preview (&graph, &config (), &skgid ("gone")) . is_err ());
  }

  #[test]
  fn stale_preview_produces_no_rewrite_instructions () {
    let mut owned = node ("owned", "main");
    owned . contains = vec! [member ("main", "gone")];
    let graph = InRustGraph::from_graphnodes (&[owned]);
    let scanned = preview (&graph, &config (), &skgid ("gone")) . unwrap ();
    let mut changed = graph . clone ();
    changed . nodes . get_mut (&skgid ("owned")) . unwrap () . title =
      "changed after preview" . to_string ();
    assert_eq! (rewrite (&changed, &config (), &scanned),
                Err ("Cleanup preview is stale; rescan before rewriting." . to_string ()));
  }
}
