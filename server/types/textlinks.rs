use regex::{Regex, Match};
use std::fmt;
use std::str::FromStr;
use std::sync::LazyLock;

use crate::types::misc::ID;
use crate::types::errors::TextLinkParseError;
use crate::types::skgnode::SkgNode;

static TEXTLINK_PATTERN : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (
    r"\[\[id:(.*?)\]\[(.*?)\]\]") . unwrap () );

static LINK_LABEL_PATTERN : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (
    r"\[\[.*?\]\[(.*?)\]\]") . unwrap () );

//
// Type Definitions
//

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextLink {
  // TextLinks are represented in, and must be parsed from, the raw text fields `title` and `body`.
  pub id: ID,
  pub label: String,
}

//
// Implementations
//

impl TextLink {
  pub fn new ( skgid : impl Into<String>,
               label  : impl Into<String>)
             -> Self {
    TextLink { id    : ID ( skgid . into () ),
               label : label . into (),
    }} }

impl fmt::Display for TextLink {
  // Format: [[id:ID][LABEL]], where allcaps terms are variables.
  // This is the same format org-roam uses.
  fn fmt ( &self,
            f : &mut fmt::Formatter <'_> )
            -> fmt::Result {
    write! ( f, "[[id:{}][{}]]", self . id, self . label ) }}

impl FromStr for TextLink {
  type Err = TextLinkParseError;

  fn from_str ( text: &str )
                -> Result <Self, Self::Err> {
    if ( !text . starts_with ("[[id:") ||
          !text . ends_with ("]]") ) {
      return Err (TextLinkParseError::InvalidFormat); }

    let interior : &str = &text [5 .. text . len () - 2];

    if let Some (idx) = interior . find ("][") {
      let skgid : &str = &interior [0..idx];
      let label  : &str = &interior [idx+2..];
      Ok ( TextLink {
        id    : ID ( skgid . to_string () ),
        label : label . to_string (),
      } )
    } else {
      Err (TextLinkParseError::MissingDivider)
    } } }

//
// Functions
//

pub fn textlinks_from_node (
  node : &SkgNode )
  -> Vec<TextLink> {
  // All textlinks in its title
  // and (if present) its body.

  let combined_text : String =
    format!(
      "{} {}",
      node . title,
      node . body . as_deref () . unwrap_or ("") );
  textlinks_from_text (&combined_text) }

pub fn textlinks_from_text (
  text: &str )
  -> Vec <TextLink> {
  let mut textlinks : Vec<TextLink> = Vec::new ();
  for capture in TEXTLINK_PATTERN . captures_iter (text) {
    if capture . len () >= 3 { // capture group 0 is the entire match
      let skgid : String = capture [1] . to_string ();
      let label  : String = capture [2] . to_string ();
      textlinks . push (
        TextLink::new ( skgid, label )); }}
  textlinks }

pub fn replace_each_link_with_its_label (
  text : &str )
  -> String {
  // Replaces each textlink with that textlink's label.
  // Strips some text from each textlink while adding nothing.
  let mut result : String = String::from (text);
  let mut input_offset : usize = 0; // offset in the input string
  for cap in LINK_LABEL_PATTERN . captures_iter (text) {
    let whole_match : Match =
      cap . get (0) . unwrap ();
    let textlink_label : Match =
      cap . get (1) . unwrap ();
    let start_pos : usize =
      whole_match . start () - input_offset;
    let end_pos : usize =
      whole_match . end ()   - input_offset;
    result . replace_range ( // the replacement
      start_pos .. end_pos,
      textlink_label . as_str () );
    input_offset += whole_match . len ()
      - textlink_label . len (); }
  result }
