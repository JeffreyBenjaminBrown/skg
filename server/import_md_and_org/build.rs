//! Convert parsed document outlines into additive graph nodes.

use crate::export_org::EXPORT_MARKER_ID;
use crate::types::misc::{
  ID, MSV, SourceName, rel_partners_at_relSource,
  rel_partners_at_relSource_msv,
};
use crate::types::nodes::complete::{
  FileProperty, NodeComplete, normalize_body,
};
use super::parse::{ParsedDocument, ParsedSection, rendered_range};
use std::path::{Component, Path, PathBuf};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct OutlineNode {
  node : NodeComplete,
  original_level : usize,
  children : Vec<usize>,
}

pub struct BuiltDocument {
  pub nodes : Vec<NodeComplete>,
  pub root_id : ID,
  pub export_target : String,
  pub footnote_node_indices : HashMap<String, usize>,
}

pub fn build_document (
  document : &ParsedDocument,
  source : &SourceName,
  new_id : &mut impl FnMut () -> ID,
) -> Result<BuiltDocument, String> {
  let export_target : String = export_target (&document . path)?;
  let mut outline : Vec<OutlineNode> = document . sections . iter ()
    . map (|section| OutlineNode {
      node : node_for_section (document, section, source, new_id),
      original_level : section . level,
      children : Vec::new (),
    }) . collect ();
  let mut ancestors : Vec<usize> = vec![0];
  for index in 1..outline . len () {
    let level : usize = outline [index] . original_level;
    while ancestors . len () > 1 &&
      outline [*ancestors . last () . unwrap ()] . original_level >= level {
      ancestors . pop (); }
    let parent : usize = *ancestors . last () . unwrap ();
    outline [parent] . children . push (index);
    ancestors . push (index); }
  group_super_indentation (0, &mut outline, source, new_id);
  let marker : NodeComplete = export_marker (&export_target, source, new_id)?;
  let marker_index : usize = outline . len ();
  outline . push (OutlineNode {
    node : marker,
    original_level : 0,
    children : Vec::new (),
  });
  outline [0] . children . push (marker_index);
  let mut footnote_node_indices : HashMap<String, usize> = HashMap::new ();
  let definitions : Vec<_> = document . footnote_definitions . iter ()
    .filter (|definition| ! definition . ambiguous) . collect ();
  if ! definitions . is_empty () {
    let footnotes_index : usize = outline . len ();
    outline . push (OutlineNode {
      node : synthetic_node ("Footnotes", "", source, new_id),
      original_level : 0,
      children : Vec::new (),
    });
    outline [footnotes_index] . node . body = None;
    for definition in definitions {
      let child_index : usize = outline . len ();
      outline . push (OutlineNode {
        node : synthetic_node (&format! ("Footnote {}", definition . name),
          &document . text [definition . range . clone ()], source, new_id),
        original_level : 0,
        children : Vec::new (),
      });
      footnote_node_indices . insert (definition . name . clone (), child_index);
      outline [footnotes_index] . children . push (child_index); }
    outline [0] . children . push (footnotes_index); }
  let ids : Vec<ID> = outline . iter () . map (|entry| entry . node . pid . clone ())
    . collect ();
  for entry in &mut outline {
    entry . node . contains = rel_partners_at_relSource (
      source,
      entry . children . iter () . map (|index| ids [*index] . clone ())
        . collect ()); }
  let root_id : ID = ids [0] . clone ();
  let nodes : Vec<NodeComplete> = outline . into_iter () . map (|entry| entry . node)
    . collect ();
  Ok (BuiltDocument { nodes, root_id, export_target, footnote_node_indices })
}

fn node_for_section (
  document : &ParsedDocument,
  section : &ParsedSection,
  source : &SourceName,
  new_id : &mut impl FnMut () -> ID,
) -> NodeComplete {
  let id : ID = section . explicit_id . as_ref ()
    . map (|value| ID::new (value)) . unwrap_or_else (new_id);
  let body : String = rendered_range (document, section . body . clone ());
  NodeComplete {
    title : section . title . clone (),
    overPrivateText_telescope : false,
    aliases : rel_partners_at_relSource_msv (
      source, MSV::Specified (section . aliases . clone ())),
    source : source . clone (),
    pid : id,
    extra_ids : Vec::new (),
    body : normalize_body (Some (body)),
    contains : Vec::new (),
    subscribes_to : MSV::Unspecified,
    hides_from_its_subscriptions : MSV::Unspecified,
    overrides_view_of : MSV::Unspecified,
    misc : if section . explicit_id . is_some () {
      vec![FileProperty::Had_ID_Before_Import]
    } else { Vec::new () },
  }
}

fn group_super_indentation (
  index : usize,
  outline : &mut Vec<OutlineNode>,
  source : &SourceName,
  new_id : &mut impl FnMut () -> ID,
) {
  let children : Vec<usize> = outline [index] . children . clone ();
  for child in children {
    group_super_indentation (child, outline, source, new_id); }
  let children : Vec<usize> = outline [index] . children . clone ();
  outline [index] . children =
    group_children (children, outline, source, new_id);
}

fn group_children (
  children : Vec<usize>,
  outline : &mut Vec<OutlineNode>,
  source : &SourceName,
  new_id : &mut impl FnMut () -> ID,
) -> Vec<usize> {
  let mut levels : Vec<usize> = children . iter ()
    . map (|index| outline [*index] . original_level) . collect ();
  levels . sort ();
  levels . dedup ();
  if levels . len () <= 1 { return children; }
  let deepest : usize = *levels . last () . unwrap ();
  let (special, normal) : (Vec<usize>, Vec<usize>) = children . into_iter ()
    . partition (|index| outline [*index] . original_level == deepest);
  let special_index : usize = outline . len ();
  outline . push (OutlineNode {
    node : synthetic_node (
      "These are special!",
      "They were super-indented in the original document.",
      source, new_id),
    original_level : 0,
    children : special,
  });
  let normal : Vec<usize> = group_children (normal, outline, source, new_id);
  let normal_index : usize = outline . len ();
  outline . push (OutlineNode {
    node : synthetic_node (
      "These are normal.",
      "They have been buried to encourage reading the nodes that were super-indented in the original document.",
      source, new_id),
    original_level : 0,
    children : normal,
  });
  vec![special_index, normal_index]
}

fn synthetic_node (
  title : &str,
  body : &str,
  source : &SourceName,
  new_id : &mut impl FnMut () -> ID,
) -> NodeComplete {
  let mut node : NodeComplete =
    crate::types::nodes::complete::empty_node_complete ();
  node . pid = new_id ();
  node . title = title . to_string ();
  node . body = Some (body . to_string ());
  node . source = source . clone ();
  node
}

fn export_marker (
  target : &str,
  source : &SourceName,
  new_id : &mut impl FnMut () -> ID,
) -> Result<NodeComplete, String> {
  let quote : char = if ! target . contains ('"') { '"' }
    else if ! target . contains ('\'') { '\'' }
    else { return Err (format! (
      "Export target {:?} contains both quote styles", target)); };
  let mut node : NodeComplete = crate::types::nodes::complete::empty_node_complete ();
  node . pid = new_id ();
  node . title = format! ("[[id:{}][Export to Org]]", EXPORT_MARKER_ID);
  node . body = Some (format! ("target_filepath = {}{}{}", quote, target, quote));
  node . source = source . clone ();
  Ok (node)
}

fn export_target (
  relative_path : &Path,
) -> Result<String, String> {
  let target : PathBuf = relative_path . with_extension ("");
  let components : Vec<String> = target . components () . map (|component| {
    match component {
      Component::Normal (name) => Ok (name . to_string_lossy () . into_owned ()),
      _ => Err (format! ("Unsafe import path {}", relative_path . display ())),
    }
  }) . collect::<Result<_, _>> ()?;
  if components . is_empty () || components . iter () . any (|part|
    part . is_empty () || part . contains ('\n') || part . contains ('\r')) {
    return Err (format! ("Unsafe export target for {}", relative_path . display ())); }
  Ok (components . join ("/"))
}

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::parse::parse_document;

  #[test]
  fn grouping_keeps_parent_body_and_adds_marker () {
    let document : ParsedDocument = parse_document (
      Path::new ("tutorials/start.md"),
      "Introduction\n# A\nA body\n### Deep\nDeep body\n## Normal\nNormal body\n" . to_string ());
    let source : SourceName = SourceName::from ("owned");
    let mut counter : usize = 0;
    let mut next = || { counter += 1; ID::new (&format! ("generated-{}", counter)) };
    let built : BuiltDocument = build_document (&document, &source, &mut next) . unwrap ();
    assert_eq! (built . export_target, "tutorials/start");
    assert_eq! (built . nodes [0] . title, "start");
    assert_eq! (built . nodes [1] . body . as_deref (), Some ("A body"));
    let first_child : &ID = &built . nodes [1] . contains [0] . member;
    let special : &NodeComplete = built . nodes . iter ()
      . find (|node| &node . pid == first_child) . unwrap ();
    assert_eq! (special . title, "These are special!");
    assert! (built . nodes [0] . contains . iter () . any (|child|
      built . nodes . iter () . any (|node|
        node . pid == child . member && node . title . contains ("Export to Org"))));
  }

  #[test]
  fn markdown_hard_breaks_become_org_breaks_outside_literal_blocks () {
    let document : ParsedDocument = parse_document (
      Path::new ("line-breaks.md"),
      "First  \nSecond\\\nThird\n\n```\nLiteral  \n```\n" . to_string ());
    let source : SourceName = SourceName::from ("owned");
    let mut next = || ID::new (&uuid::Uuid::new_v4 () . to_string ());
    let built : BuiltDocument = build_document (&document, &source, &mut next) . unwrap ();
    let body : &str = built . nodes [0] . body . as_deref () . unwrap ();
    assert_eq! (body, "First\\\\\nSecond\\\\\nThird\n\n```\nLiteral  \n```");
  }
}
