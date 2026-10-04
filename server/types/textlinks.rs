pub mod org_literal_ranges;

use regex::{Captures, Regex, Match};
use std::fmt;
use std::ops::Range;
use std::str::FromStr;
use std::sync::LazyLock;

use crate::types::misc::ID;
use crate::types::errors::TextLinkParseError;
use crate::types::nodes::complete::NodeComplete;
use org_literal_ranges::org_literal_ranges;

// LazyLock<Regex> ensures each regex is compiled exactly once, on first use, rather than per call.
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
  node : &NodeComplete )
  -> Vec<TextLink> {
  // All textlinks in its title
  // and (if present) its body.
  // Scanned separately, so the body's first line is still a line start.
  let mut textlinks : Vec<TextLink> = textlinks_from_text (&node . title);
  textlinks . extend (
    textlinks_from_text ( node . body . as_deref () . unwrap_or ("") ));
  textlinks }

pub fn textlinks_from_text (
  text: &str )
  -> Vec <TextLink> {
  textlinks_with_ranges_from_text (text) . into_iter ()
    . map ( |(_, textlink)| textlink )
    . collect () }

/// Each textlink in 'text' that Org treats as a link, with its byte
/// range. Link syntax in text Org shows literally (see
/// 'org_literal_ranges') is an example, not a link.
pub fn textlinks_with_ranges_from_text (
  text: &str )
  -> Vec <(Range<usize>, TextLink)> {
  captures_outside_literals (&TEXTLINK_PATTERN, text) . into_iter ()
    . map ( |capture| (
      capture . get (0) . unwrap () . range (),
      TextLink::new ( capture [1] . to_string (),
                      capture [2] . to_string () )) )
    . collect () }

fn captures_outside_literals <'t> (
  pattern : &Regex,
  text    : &'t str )
  -> Vec <Captures<'t>> {
  let literal : Vec<Range<usize>> = org_literal_ranges (text);
  pattern . captures_iter (text)
    . filter ( |capture| {
      let whole : Match = capture . get (0) . unwrap ();
      // Only a literal region containing the whole link makes it an
      // example; =verbatim= in a link's label is just its formatting.
      ! literal . iter () . any ( |range|
        range . start <= whole . start () && whole . end () <= range . end ) } )
    . collect () }

pub fn replace_each_link_with_its_label (
  text : &str )
  -> String {
  // Replaces each textlink with that textlink's label,
  // except examples in text Org shows literally.
  // Strips some text from each textlink while adding nothing.
  let mut result : String = String::from (text);
  let mut input_offset : usize = 0; // offset in the input string
  for cap in captures_outside_literals (&LINK_LABEL_PATTERN, text) {
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
