use skg::import_org_roam::parse::{
  parse_org_file,
  parse_roam_aliases,
  is_headline,
  headline_level,
  headline_title,
};
use skg::types::misc::ID;
use skg::types::skgnode::{FileProperty, SkgNode};

use std::io::Write;

fn write_temp_org (
  content : &str,
  name    : &str,
) -> tempfile::NamedTempFile {
  let mut f : tempfile::NamedTempFile =
    tempfile::Builder::new()
    . prefix (name)
    . suffix (".org")
    . tempfile()
    . unwrap();
  f . write_all (content . as_bytes() ) . unwrap();
  f }

#[test]
fn test_minimal_file () {
  let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:END:
#+title: My Node
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "minimal");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert_eq! (nodes . len(), 1);
  assert_eq! (nodes[0] . title, "My Node");
  assert_eq! (nodes[0] . pid, ID::new ("abc-123"));
  assert! (nodes[0] . body . is_none());
  assert! (nodes[0] . contains . is_empty()); }

#[test]
fn test_file_with_body () {
  let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:END:
#+title: My Node
Some body text here.
More body text.
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "with-body");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert_eq! (nodes . len(), 1);
  assert_eq! (nodes[0] . body . as_deref(),
              Some ("Some body text here.\nMore body text.") ); }

#[test]
fn test_nested_id_headlines () {
  let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Parent
* child one
  :PROPERTIES:
  :ID:       child-1
  :END:
  Child one body.
* child two
  :PROPERTIES:
  :ID:       child-2
  :END:
  Child two body.
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "nested");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert_eq! (nodes . len(), 3);
  // File-level node
  assert_eq! (nodes[0] . title, "Parent");
  assert_eq! (nodes[0] . pid, ID::new ("file-id"));
  let contained : &[ID] =
    &nodes[0] . contains;
  assert_eq! (contained . len(), 2);
  assert_eq! (contained[0], ID::new ("child-1"));
  assert_eq! (contained[1], ID::new ("child-2"));
  assert! (nodes[0] . body . is_none());
  // Child nodes
  assert_eq! (nodes[1] . title, "child one");
  assert_eq! (nodes[1] . body . as_deref(),
              Some ("  Child one body.") );
  assert_eq! (nodes[2] . title, "child two");
  assert_eq! (nodes[2] . body . as_deref(),
              Some ("  Child two body.") ); }

#[test]
fn test_non_id_headline_becomes_node () {
  let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Parent
* just a regular headline
  Some text under it.
** sub-headline
   More text.
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "non-id-hl");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  // 3 nodes: file + headline + sub-headline
  assert_eq! (nodes . len(), 3);
  assert_eq! (nodes[0] . title, "Parent");
  assert_eq! (nodes[0] . pid, ID::new ("file-id"));
  let contained : &[ID] =
    &nodes[0] . contains;
  assert_eq! (contained . len(), 1);
  assert! (nodes[0] . body . is_none());
  // Headline node (generated ID)
  assert_eq! (nodes[1] . title, "just a regular headline");
  assert_eq! (nodes[1] . body . as_deref(),
              Some ("  Some text under it.") );
  let hl_contained : &[ID] =
    &nodes[1] . contains;
  assert_eq! (hl_contained . len(), 1);
  // Sub-headline node
  assert_eq! (nodes[2] . title, "sub-headline");
  assert_eq! (nodes[2] . body . as_deref(),
              Some ("   More text.") ); }

#[test]
fn test_id_headline_under_non_id_headline () {
  // Every headline is a node. file→intermediary→deep-child.
  let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Root
* intermediary (no ID)
** actual child
   :PROPERTIES:
   :ID:       deep-child
   :END:
   Deep child body.
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "transitive");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  // 3 nodes: file, intermediary, deep-child
  assert_eq! (nodes . len(), 3);
  // Root contains intermediary (generated ID)
  let root_contained : &[ID] =
    &nodes[0] . contains;
  assert_eq! (root_contained . len(), 1);
  assert! (nodes[0] . body . is_none());
  // Intermediary contains deep-child
  assert_eq! (nodes[1] . title, "intermediary (no ID)");
  let mid_contained : &[ID] =
    &nodes[1] . contains;
  assert_eq! (mid_contained . len(), 1);
  assert_eq! (mid_contained[0], ID::new ("deep-child"));
  // Deep child
  assert_eq! (nodes[2] . title, "actual child");
  assert_eq! (nodes[2] . pid, ID::new ("deep-child"));
  assert_eq! (nodes[2] . body . as_deref(),
              Some ("   Deep child body.") ); }

#[test]
fn test_roam_aliases () {
  let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:ROAM_ALIASES: \"machine learning\" energy \"dark matter\"
:END:
#+title: Physics
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "aliases");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert_eq! (nodes . len(), 1);
  let aliases : &[String] =
    nodes[0] . aliases . or_default();
  assert_eq! (aliases, &vec![
    "machine learning", "energy", "dark matter" ]); }

#[test]
fn test_empty_file () {
  let f : tempfile::NamedTempFile =
    write_temp_org ("", "empty");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert! (nodes . is_empty()); }

#[test]
fn test_file_without_id () {
  let content : &str = "\
#+title: No ID here
Some text.
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "no-id");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert! (nodes . is_empty()); }

#[test]
fn test_missing_title_uses_filename () {
  let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:END:
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "my_note");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert_eq! (nodes . len(), 1);
  // Title should be derived from filename
  // (tempfile adds random chars, so just check it's not empty)
  assert! (! nodes[0] . title . is_empty()); }

#[test]
fn test_parse_roam_aliases_mixed () {
  let result : Vec<String> =
    parse_roam_aliases ("\"machine learning\" energy \"dark matter\"");
  assert_eq! (result, vec![
    "machine learning", "energy", "dark matter" ]); }

#[test]
fn test_parse_roam_aliases_unquoted_only () {
  let result : Vec<String> =
    parse_roam_aliases ("energy power force");
  assert_eq! (result, vec![ "energy", "power", "force" ]); }

#[test]
fn test_parse_roam_aliases_empty () {
  let result : Vec<String> = parse_roam_aliases ("");
  assert! (result . is_empty()); }

#[test]
fn test_is_headline () {
  assert! (is_headline ("* foo"));
  assert! (is_headline ("** bar"));
  assert! (is_headline ("*** baz quux"));
  assert! (! is_headline ("not a headline"));
  assert! (! is_headline ("*bold*"));
  assert! (! is_headline ("")); }

#[test]
fn test_headline_level () {
  assert_eq! (headline_level ("* foo"),   1);
  assert_eq! (headline_level ("** bar"),  2);
  assert_eq! (headline_level ("*** baz"), 3); }

#[test]
fn test_headline_title () {
  assert_eq! (headline_title ("* hello world"), "hello world");
  assert_eq! (headline_title ("** sub"), "sub"); }

#[test]
fn test_power_org_structure () {
  // Every headline becomes a node: file + 5 headlines = 6 nodes.
  let content : &str = "\
:PROPERTIES:
:ID:       b9775088-1bd9-490f-a062-c6cfd189b65d
:ROAM_ALIASES: energy power force work strength
:END:
#+title: energy
* see also [[id:80cfe814][constraint]]
* the feeling of forcing it
  :PROPERTIES:
  :ID:       1cd8051b-95ee-4f73-a05e-624200b52c90
  :END:
** It might be entirely avoidable.
** It might be a useful last resort sometimes.
* etymology : sociology, physics
  Power implies choice; energy, only possibility.
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "power");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert_eq! (nodes . len(), 6);
  // File-level node
  assert_eq! (nodes[0] . title, "energy");
  assert_eq! (nodes[0] . aliases . or_default(),
              &vec!["energy", "power", "force", "work", "strength"]);
  let file_contained : &[ID] =
    &nodes[0] . contains;
  assert_eq! (file_contained . len(), 3);
  // The :ID: child's contained ID is known
  assert_eq! (file_contained[1],
              ID::new ("1cd8051b-95ee-4f73-a05e-624200b52c90"));
  assert! (nodes[0] . body . is_none());
  // "see also" headline
  assert_eq! (nodes[1] . title,
              "see also [[id:80cfe814][constraint]]");
  assert! (nodes[1] . body . is_none());
  assert! (nodes[1] . contains . is_empty());
  // "the feeling of forcing it" — has :ID: and 2 sub-headlines
  assert_eq! (nodes[2] . title, "the feeling of forcing it");
  assert_eq! (nodes[2] . pid,
              ID::new ("1cd8051b-95ee-4f73-a05e-624200b52c90"));
  let forcing_contained : &[ID] =
    &nodes[2] . contains;
  assert_eq! (forcing_contained . len(), 2);
  assert! (nodes[2] . body . is_none());
  // Sub-headlines of "the feeling of forcing it"
  assert_eq! (nodes[3] . title, "It might be entirely avoidable.");
  assert_eq! (nodes[4] . title,
              "It might be a useful last resort sometimes.");
  // "etymology" headline
  assert_eq! (nodes[5] . title, "etymology : sociology, physics");
  assert_eq! (nodes[5] . body . as_deref(),
              Some ("  Power implies choice; energy, only possibility.") );
  assert! (nodes[5] . contains . is_empty()); }

#[test]
fn test_music_and_consciousness () {
  let content : &str = "\
:PROPERTIES:
:ID:       01104862
:END:
#+title: music & consciousness
= things about consciousness that music highlights
* [[id:39029f2f][Effort and observation are somewhat disjunctive.]]
* [[id:681da8ea][Music illuminates (the?) infinite nature of want.]]
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "music");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  // 3 nodes: file + 2 headlines
  assert_eq! (nodes . len(), 3);
  // File node
  assert_eq! (nodes[0] . title, "music & consciousness");
  assert_eq! (nodes[0] . pid, ID::new ("01104862"));
  assert_eq! (nodes[0] . body . as_deref(),
              Some ("= things about consciousness that music highlights") );
  let contained : &[ID] =
    &nodes[0] . contains;
  assert_eq! (contained . len(), 2);
  // Headline nodes — titles preserve the [[id:...][...]] links
  assert_eq! (nodes[1] . title,
              "[[id:39029f2f][Effort and observation are somewhat disjunctive.]]");
  assert! (nodes[1] . body . is_none());
  assert! (nodes[1] . contains . is_empty());
  assert_eq! (nodes[2] . title,
              "[[id:681da8ea][Music illuminates (the?) infinite nature of want.]]");
  assert! (nodes[2] . body . is_none());
  assert! (nodes[2] . contains . is_empty()); }

#[test]
fn test_had_id_before_import () {
  // Nodes with :ID: get Had_ID_Before_Import in misc.
  // Nodes without :ID: (assigned UUIDs) get empty misc.
  let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Parent
* child with ID
  :PROPERTIES:
  :ID:       child-id
  :END:
  Body.
* child without ID
  Just text.
";
  let f : tempfile::NamedTempFile =
    write_temp_org (content, "had-id");
  let nodes : Vec<SkgNode> =
    parse_org_file (f . path());
  assert_eq! (nodes . len(), 3);
  // File-level node had :ID: → Had_ID_Before_Import.
  assert_eq! (nodes[0] . misc,
              vec![FileProperty::Had_ID_Before_Import]);
  // Child with :ID: → Had_ID_Before_Import.
  assert_eq! (nodes[1] . misc,
              vec![FileProperty::Had_ID_Before_Import]);
  // Child without :ID: → empty misc.
  assert! (nodes[2] . misc . is_empty()); }
