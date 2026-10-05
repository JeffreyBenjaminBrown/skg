pub mod org_literal_ranges;

use regex::{Captures, Regex, Match};
use std::fmt;
use std::ops::Range;
use std::str::FromStr;
use std::sync::LazyLock;

use crate::types::misc::ID;
use crate::types::errors::LinkParseError;
use crate::types::nodes::complete::Graphnode;
use org_literal_ranges::org_literal_ranges;

// LazyLock<Regex> ensures each regex is compiled exactly once, on first use, rather than per call.
static LINK_PATTERN : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (
    r"\[\[id:(.*?)\]\[(.*?)\]\]") . unwrap () );
static LINK_LABEL_PATTERN : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (
    r"\[\[.*?\]\[(.*?)\]\]") . unwrap () );

//
// Type Definitions
//

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Link {
  // Links are represented in, and must be parsed from, the raw text fields `title` and `body`.
  pub skgid: ID,
  pub label: String,
}

//
// Implementations
//

impl Link {
  pub fn new ( skgid : impl Into<String>,
               label  : impl Into<String>)
             -> Self {
    Link { skgid    : ID ( skgid . into () ),
               label : label . into (),
    }} }

impl fmt::Display for Link {
  // Format: [[id:ID][LABEL]], where allcaps terms are variables.
  // This is the same format org-roam uses.
  fn fmt ( &self,
            f : &mut fmt::Formatter <'_> )
            -> fmt::Result {
    write! ( f, "[[id:{}][{}]]", self . skgid, self . label ) }}

impl FromStr for Link {
  type Err = LinkParseError;

  fn from_str ( text: &str )
                -> Result <Self, Self::Err> {
    if ( !text . starts_with ("[[id:") ||
          !text . ends_with ("]]") ) {
      return Err (LinkParseError::InvalidFormat); }

    let interior : &str = &text [5 .. text . len () - 2];

    if let Some (idx) = interior . find ("][") {
      let skgid : &str = &interior [0..idx];
      let label  : &str = &interior [idx+2..];
      Ok ( Link {
        skgid    : ID ( skgid . to_string () ),
        label : label . to_string (),
      } )
    } else {
      Err (LinkParseError::MissingDivider)
    } } }

//
// Functions
//

pub fn links_from_node (
  node : &Graphnode )
  -> Vec<Link> {
  // All links in its title
  // and (if present) its body.
  // Scanned separately, so the body's first line is still a line start.
  let mut links : Vec<Link> = links_from_text (&node . title);
  links . extend (
    links_from_text ( node . body . as_deref () . unwrap_or ("") ));
  links }

pub fn links_from_text (
  text: &str )
  -> Vec <Link> {
  links_with_ranges_from_text (text) . into_iter ()
    . map ( |(_, link)| link )
    . collect () }

/// Each link in 'text' that Org treats as a link, with its byte
/// range. Link syntax in text Org shows literally (see
/// 'org_literal_ranges') is an example, not a link.
pub fn links_with_ranges_from_text (
  text: &str )
  -> Vec <(Range<usize>, Link)> {
  captures_outside_literals (&LINK_PATTERN, text) . into_iter ()
    . map ( |capture| (
      capture . get (0) . unwrap () . range (),
      Link::new ( capture [1] . to_string (),
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
  // Replaces each link with that link's label,
  // except examples in text Org shows literally.
  // Strips some text from each link while adding nothing.
  let mut result : String = String::from (text);
  let mut input_offset : usize = 0; // offset in the input string
  for cap in captures_outside_literals (&LINK_LABEL_PATTERN, text) {
    let whole_match : Match =
      cap . get (0) . unwrap ();
    let link_label : Match =
      cap . get (1) . unwrap ();
    let start_pos : usize =
      whole_match . start () - input_offset;
    let end_pos : usize =
      whole_match . end ()   - input_offset;
    result . replace_range ( // the replacement
      start_pos .. end_pos,
      link_label . as_str () );
    input_offset += whole_match . len ()
      - link_label . len (); }
  result }
