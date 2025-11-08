use regex::{Regex, Match};

use crate::types::{TextLink, SkgNode};

pub fn textlinks_from_node (
  node : &SkgNode )
  -> Vec<TextLink> {
  // All textlinks in its title
  // and (if present) its body.

  let combined_text : String =
    format!(
      "{} {}",
      node.title,
      node . body . as_deref () . unwrap_or ("") );
  textlinks_from_text ( &combined_text ) }

pub fn textlinks_from_text (
  text: &str )
  -> Vec <TextLink> {

  let textlink_pattern : Regex = Regex::new (
    // non-greedy .*? pattern avoids capturing too much.
    r"\[\[id:(.*?)\]\[(.*?)\]\]").unwrap();
  let mut textlinks = Vec::new ();
  for capture in textlink_pattern.captures_iter ( text ) {
    if capture.len () >= 3 { // capture group 0 is the entire match
      let id    : String = capture [1] . to_string ();
      let label : String = capture [2] . to_string ();
      textlinks.push (
        TextLink::new ( id, label )); }}
  textlinks }

pub fn replace_each_link_with_its_label (
  text : &str )
  -> String {
  // Replaces each textlink with that textlink's label.
  // Strips some text from each textlink while adding nothing.

  let textlink_re : Regex = Regex::new (
    r"\[\[.*?\]\[(.*?)\]\]") . unwrap (); // capture the label but not the ID
  let mut result = String::from ( text );
  let mut input_offset = 0; // offset in the input string
  for cap in textlink_re.captures_iter ( text ) {
    let whole_match : Match =
      cap . get (0) . unwrap ();
    let textlink_label : Match =
      cap . get (1) . unwrap ();
    let start_pos : usize =
      whole_match . start () - input_offset;
    let end_pos : usize =
      whole_match . end ()   - input_offset;
    result.replace_range ( // the replacement
      start_pos .. end_pos,
      textlink_label.as_str () );
    input_offset += whole_match.len ()
      - textlink_label.len (); }
  result }
