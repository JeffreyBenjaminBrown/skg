use crate::types::{OrgNode, MetadataItem};

use std::collections::HashSet;
use std::fmt::Write as _;

pub fn render_org_node (
  node: &OrgNode,
  level: usize,
  metadata: &HashSet<MetadataItem>,
  render_body: bool
) -> String {
  render_org_node_from_text (
    level,
    &node.title,
    if render_body { node.body.as_deref() } else { None },
    metadata ) }

pub fn render_org_node_from_text (
  level: usize,
  title: &str,
  body: Option<&str>,
  metadata: &HashSet<MetadataItem>
) -> String {
  if ( metadata . is_empty() &&
       title    . is_empty() ) {
    panic!("render_org_node_from_text called with both empty metadata and empty title"); }

  let mut result = String::new();
  result.push_str( // Leading bullet is mandatory.
    &org_bullet(level));
  if ! metadata.is_empty() { // Maybe add metadata.
    result.push(' ');
    result.push_str(&render_metadata_header(metadata));
  }
  if ! title.is_empty() { // Maybe add title.
    // PITFALL: Title can be missing, for the right metadata.
    result.push(' ');
    result.push_str (title); }
  result.push('\n');
  if let Some(body_text) = body { // Maybe add body
    if !body_text.is_empty() {
      result.push_str(body_text);
      if !body_text.ends_with('\n') {
        result.push('\n'); }} }
  result }

pub fn render_metadata_header(
  metadata: &HashSet<MetadataItem>
) -> String {
  if metadata.is_empty() {
    panic!("render_metadata_header called with empty metadata - caller should check first"); }
  let mut metadata_vec: Vec<&MetadataItem> =
    metadata.iter().collect();
  metadata_vec.sort_by(|a, b| {
    // Sort metadata: id first, then alphabetical by Display
    match (a, b) {
      (MetadataItem::ID(_), MetadataItem::ID(_)) =>
        a.to_string().cmp(&b.to_string()),
      (MetadataItem::ID(_), _) => std::cmp::Ordering::Less,
      (_, MetadataItem::ID(_)) => std::cmp::Ordering::Greater,
      _ => a.to_string().cmp(&b.to_string()), } });
  let mut result = String::new();
  result.push_str("<skg<");
  for (i, item) in metadata_vec.iter().enumerate() {
    if i > 0 {
      result.push(','); }
    write!(&mut result, "{}", item).unwrap(); }
  result.push_str(">>");
  result }

pub fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }
