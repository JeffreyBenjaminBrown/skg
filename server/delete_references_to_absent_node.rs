//! Pure scanning and rewriting for `skg-delete-references-to-absent-node`.
//!
//! Transport deliberately does not live here: an approval is always compared
//! with a freshly scanned `Preview`, and rewrite instructions are rebuilt from
//! that same snapshot rather than from client-supplied text.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::save::nodecomplete_from_noderust;
use crate::types::misc::{ID, MemberAtSource, MSV, SkgConfig, SourceName};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::textlinks::textlinks_from_text;

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
  pub owner_source : SourceName,
  pub owner_title  : String,
  pub field        : StructuredField,
  pub raw_id       : ID,
  pub relation_source : SourceName,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum TextField { Title, Body }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TextLinkOccurrence {
  pub owner_pid    : ID,
  pub owner_source : SourceName,
  pub field        : TextField,
  pub line         : usize,
  pub label        : String,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Preview {
  pub raw_id       : ID,
  pub structural   : Vec<StructuralOccurrence>,
  pub text_links   : Vec<TextLinkOccurrence>,
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
        encode (&o . owner_pid . 0), encode (&o . owner_source . 0),
        encode (&o . owner_title), encode (o . field . label ()),
        encode (&o . raw_id . 0), encode (&o . relation_source . 0))); }
    for o in &self . text_links {
      rows . push (format! ("T{}{}{:?}:{}{}",
        encode (&o . owner_pid . 0), encode (&o . owner_source . 0),
        o . field, o . line, encode (&o . label))); }
    rows . join ("\n")
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
        o . owner_pid, o . field . label (), o . raw_id, o . relation_source)); }}
  if ! preview . text_links . is_empty () {
    text . push_str ("** Text links left unchanged\n");
    for o in &preview . text_links {
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
      occurrence . relation_source)); }
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
    if ! config . user_owns_source (&node . source) { continue; }
    let mut record = |field : StructuredField, members : &[MemberAtSource<ID>]| {
      for member in members . iter () . filter (|m| &m . member == raw_id) {
        result . structural . push (StructuralOccurrence {
          owner_pid       : node . pid . clone (),
          owner_source    : node . source . clone (),
          owner_title     : node . title . clone (),
          field,
          raw_id          : raw_id . clone (),
          relation_source : member . source . clone (), }); }};
    record (StructuredField::Contains, &node . contains);
    record (StructuredField::SubscribesTo, node . subscribes_to . or_default ());
    record (StructuredField::HidesFromItsSubscriptions,
            node . hides_from_its_subscriptions . or_default ());
    record (StructuredField::OverridesViewOf,
            node . overrides_view_of . or_default ());
    record_text_links (&mut result . text_links, node, raw_id, TextField::Title,
                       &node . title);
    if let Some (body) = &node . body {
      record_text_links (&mut result . text_links, node, raw_id, TextField::Body,
                         body); }}
  result . structural . sort_by (|a, b|
    (&a . owner_pid, a . field, &a . relation_source)
      . cmp (&(&b . owner_pid, b . field, &b . relation_source)));
  result . text_links . sort_by (|a, b|
    (&a . owner_pid, a . field, a . line, &a . label)
      . cmp (&(&b . owner_pid, b . field, b . line, &b . label)));
  Ok (result)
}

fn record_text_links (
  occurrences : &mut Vec<TextLinkOccurrence>,
  node        : &crate::types::nodes::rust::NodeRust,
  raw_id      : &ID,
  field       : TextField,
  text        : &str,
) {
  let mut search_from : usize = 0;
  for link in textlinks_from_text (text) . into_iter ()
    . filter (|link| &link . id == raw_id) {
    let rendered : String = link . to_string ();
    let Some (offset) = text [search_from ..] . find (&rendered) else { continue; };
    let start : usize = search_from + offset;
    occurrences . push (TextLinkOccurrence {
      owner_pid    : node . pid . clone (),
      owner_source : node . source . clone (),
      field,
      line         : text [..start] . bytes () . filter (|b| *b == b'\n') . count () + 1,
      label        : link . label, });
    search_from = start + rendered . len (); }
}

/// Build one verbatim SaveNode per owned affected telescope.  It removes only
/// exact raw-ID matches and preserves all list order, recording sources, and
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
  members : &MSV<MemberAtSource<ID>>,
  raw_id  : &ID,
) -> MSV<MemberAtSource<ID>> {
  match members {
    MSV::Unspecified => MSV::Unspecified,
    MSV::Specified (members) => MSV::Specified (
      members . iter () . filter (|m| &m . member != raw_id)
        . cloned () . collect ()), }
}
