//! DB-free unit tests for server/export_org.rs.
//! Included via `#[path = "../tests/unit/export_org.rs"] mod tests;`,
//! so `super::*` reaches the module's private items.

use super::*;
use crate::source_sets::{ActiveSourceSet, SourceSetName};
use crate::types::misc::{ID, SourceName, privacied_all};
use crate::types::nodes::complete::empty_node_complete;

use std::collections::BTreeSet;
use std::fs;

const MAGIC : &str = "3d9aa9be-d95a-48bc-b362-33f9e7ebdf6f";

fn node (
  pid      : &str,
  title    : &str,
  body     : Option<&str>,
  contains : &[&str],
) -> NodeComplete {
  let mut n : NodeComplete = empty_node_complete ();
  n . pid      = ID::from (pid);
  n . title    = title . to_string ();
  n . body     = body . map ( |s| s . to_string () );
  n . source   = SourceName::from ("main");
  n . contains = privacied_all (
    & n . source,
    contains . iter () . map ( |c| ID::from (*c) ) . collect () );
  n }

fn active_all () -> ActiveSourceSet {
  ActiveSourceSet { name    : SourceSetName::from ("all"),
                    sources : BTreeSet::new () } }

//
// relpath
//

#[test]
fn relpath_cases () {
  assert_eq! (relpath ("a", "broken"),        "./broken.org");
  assert_eq! (relpath ("broken", "a"),        "./a.org");
  assert_eq! (relpath ("a", "sub/n"),         "./sub/n.org");
  assert_eq! (relpath ("sub/n", "a"),         "../a.org");
  assert_eq! (relpath ("docs/setup", "org_export_is_partial"),
              "../org_export_is_partial.org");
  assert_eq! (relpath ("a/b/c", "a/d/e"),     "../d/e.org");
  assert_eq! (relpath ("docs/setup", "docs/commands"),
              "./commands.org");
}

//
// parse_target_filepath
//

#[test]
fn parse_target_filepath_cases () {
  assert_eq! (
    parse_target_filepath (Some ("target_filepath = docs/setup")) . unwrap (),
    Some ("docs/setup" . to_string ()) );
  assert_eq! (
    parse_target_filepath (Some ("target_filepath = org_export_is_partial")) . unwrap (),
    Some ("org_export_is_partial" . to_string ()) );
  assert_eq! ( // quoted value may contain whitespace
    parse_target_filepath (Some ("target_filepath = \"with space\"")) . unwrap (),
    Some ("with space" . to_string ()) );
  assert_eq! ( // a leading ./ is stripped
    parse_target_filepath (Some ("target_filepath = ./docs/x")) . unwrap (),
    Some ("docs/x" . to_string ()) );
  assert_eq! (
    parse_target_filepath (Some ("# comment\ntarget_filepath = x")) . unwrap (),
    Some ("x" . to_string ()) );
  assert_eq! (parse_target_filepath (Some ("nope = 1")) . unwrap (), None);
  assert_eq! (parse_target_filepath (None) . unwrap (), None);
  // refusals
  assert! (parse_target_filepath (Some ("target_filepath = with space")) . is_err ());
  assert! (parse_target_filepath (Some ("target_filepath = ../etc")) . is_err ());
  assert! (parse_target_filepath (Some ("target_filepath = /abs")) . is_err ());
  assert! (parse_target_filepath (Some ("target_filepath = a/../b")) . is_err ());
}

//
// end-to-end export
//

fn sample_nodes () -> Vec<NodeComplete> {
  vec! [
    node ("a", "Root A",
          Some ("see [[id:b][bee]] and [[id:zzz][gone]]"),
          &["ma", "b", "n"]),
    node ("ma", &format! ("[[id:{}][how]]", MAGIC),
          Some ("target_filepath = a"), &[]),
    node ("b", "Bee", Some ("back to [[id:a][rootlink]]"), &["c"]),
    node ("c", "Cee", None, &[]),
    node ("n", "Enn", None, &["mn"]),
    node ("mn", &format! ("[[id:{}][how]]", MAGIC),
          Some ("target_filepath = sub/n"), &[]),
    node (BROKEN_LINK_SINK_ID, "Broken sink",
          Some ("deep [[id:c][cee-link]]"), &["ms"]),
    node ("ms", &format! ("[[id:{}][how]]", MAGIC),
          Some ("target_filepath = broken"), &[]),
  ] }

#[test]
fn export_writes_expected_files_and_links () {
  let nodes : Vec<NodeComplete> = sample_nodes ();
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let report : ExportReport =
    export_to_org (&active_all (), &nodes, dir . path ()) . unwrap ();

  assert_eq! (report . roots_found, 3);
  assert_eq! (report . broken_links, 1); // the [[id:zzz]] link
  assert_eq! (
    report . files_written,
    vec! ["a.org" . to_string (),
          "broken.org" . to_string (),
          "sub/n.org" . to_string ()] );

  let a : String =
    fs::read_to_string (dir . path () . join ("a.org")) . unwrap ();
  assert! (a . contains ("* Root A"), "a.org:\n{}", a);
  assert! (a . contains ("[[*Bee][bee]]"), "a.org:\n{}", a);
  assert! (a . contains ("[[./broken.org][gone]]"), "a.org:\n{}", a);
  assert! (a . contains ("** Bee"), "a.org:\n{}", a);
  assert! (a . contains ("[[*Root A][rootlink]]"), "a.org:\n{}", a);
  assert! (a . contains ("*** Cee"), "a.org:\n{}", a);
  assert! (a . contains ("** [[./sub/n.org][Enn]]"), "a.org:\n{}", a);
  // The nested root's subtree is NOT inlined.
  assert! (! a . contains ("Cee\n* "), "nested inlined? a.org:\n{}", a);

  let n : String =
    fs::read_to_string (dir . path () . join ("sub/n.org")) . unwrap ();
  assert_eq! (n, "* Enn\n");

  let broken : String =
    fs::read_to_string (dir . path () . join ("broken.org")) . unwrap ();
  assert! (broken . contains ("* Broken sink"), "broken.org:\n{}", broken);
  assert! (broken . contains ("[[./a.org::*Cee][cee-link]]"),
           "broken.org:\n{}", broken);
}

#[test]
fn heading_text_matches_deep_link_anchor () {
  // A node whose title contains a link: the rendered heading must
  // be the plain-label form, equal to the anchor used to link to it,
  // so org's `::*` / `*` heading search resolves.
  let nodes : Vec<NodeComplete> = vec! [
    node ("a","Root A", Some ("ref [[id:hl][to headline]]"),
          &["ma","hl","other"]),
    node ("ma", &format! ("[[id:{}][how]]", MAGIC),
          Some ("target_filepath = a"), &[]),
    node ("hl", "Pre [[id:other][mid]] Post", Some ("hl body"), &[]),
    node ("other","Other", None, &[]),
  ];
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  export_to_org (&active_all (), &nodes, dir . path ()) . unwrap ();
  let a : String =
    fs::read_to_string (dir . path () . join ("a.org")) . unwrap ();
  // heading is plain text (no link markup), and the body link to it
  // uses exactly that text.
  assert! (a . contains ("** Pre mid Post"), "a.org:\n{}", a);
  assert! (a . contains ("[[*Pre mid Post][to headline]]"), "a.org:\n{}", a);
  assert! (! a . contains ("** Pre [[*Other"), "heading kept link markup:\n{}", a);

  // A root whose title contains a link: heading is the label form,
  // and a same-file self-link uses that same text.
  let nodes2 : Vec<NodeComplete> = vec! [
    node ("r","Root [[id:x][X]] tail",
          Some ("self [[id:r][back to root]]"), &["mr","x"]),
    node ("mr", &format! ("[[id:{}][how]]", MAGIC),
          Some ("target_filepath = r"), &[]),
    node ("x","Ex", Some ("up [[id:r][to root]]"), &[]),
  ];
  let dir2 : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  export_to_org (&active_all (), &nodes2, dir2 . path ()) . unwrap ();
  let r : String =
    fs::read_to_string (dir2 . path () . join ("r.org")) . unwrap ();
  assert! (r . contains ("* Root X tail"), "r.org:\n{}", r);
  assert! (r . contains ("[[*Root X tail][back to root]]"), "r.org:\n{}", r);
}

#[test]
fn diamond_node_rendered_once_then_linked () {
  // a -> b -> shared -> leaf ; a -> c -> shared. The shared subtree
  // must render exactly once; the second container links to it.
  let nodes : Vec<NodeComplete> = vec! [
    node ("a","Root A", None, &["ma","b","c"]),
    node ("ma", &format! ("[[id:{}][how]]", MAGIC),
          Some ("target_filepath = a"), &[]),
    node ("b","Bee", None, &["shared"]),
    node ("c","Cee", None, &["shared"]),
    node ("shared","Shared", Some ("shared body"), &["leaf"]),
    node ("leaf","Leaf", None, &[]),
  ];
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let report : ExportReport =
    export_to_org (&active_all (), &nodes, dir . path ()) . unwrap ();
  let a : String =
    fs::read_to_string (dir . path () . join ("a.org")) . unwrap ();
  assert_eq! (a . matches ("Shared\nshared body") . count (), 1,
              "shared subtree duplicated:\n{}", a);
  assert_eq! (a . matches ("**** Leaf") . count (), 1,
              "leaf duplicated:\n{}", a);
  // the second occurrence is a link to the first.
  assert! (a . contains ("[[*Shared][Shared]]"),
           "second container should link to shared:\n{}", a);
  // no false ambiguous-heading warning (it is the SAME node, once).
  assert! (! report . warnings . iter ()
           . any ( |w| w . contains ("ambiguous heading")),
           "unexpected ambiguity warning: {:?}", report . warnings);
}

#[test]
fn title_link_to_instruction_without_target_is_content () {
  // A node whose title links to the instruction node but has no
  // target_filepath is ordinary content, not a marker: it must NOT
  // be excluded from the export.
  let nodes : Vec<NodeComplete> = vec! [
    node ("a","Root A", None, &["ma","doc"]),
    node ("ma", &format! ("[[id:{}][how]]", MAGIC),
          Some ("target_filepath = a"), &[]),
    node ("doc", &format! ("see [[id:{}][the instruction node]]", MAGIC),
          Some ("explanatory body"), &[]),
  ];
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  export_to_org (&active_all (), &nodes, dir . path ()) . unwrap ();
  let a : String =
    fs::read_to_string (dir . path () . join ("a.org")) . unwrap ();
  assert! (a . contains ("** see the instruction node"),
           "doc node wrongly excluded:\n{}", a);
  assert! (a . contains ("explanatory body"), "a.org:\n{}", a);
}

#[test]
fn marker_child_is_excluded_from_content () {
  let nodes : Vec<NodeComplete> = sample_nodes ();
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  export_to_org (&active_all (), &nodes, dir . path ()) . unwrap ();
  let a : String =
    fs::read_to_string (dir . path () . join ("a.org")) . unwrap ();
  // "how" is the marker child's label; it must not appear as content.
  assert! (! a . contains ("how"), "marker leaked into a.org:\n{}", a);
}
