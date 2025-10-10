use super::{ID, SkgNode, NodeSaveAction};
use std::fmt;
use std::str::FromStr;

pub type SaveInstruction = (SkgNode, NodeSaveAction);

#[derive(Debug, Clone, PartialEq)]
pub struct OrgNode {
  pub metadata: OrgnodeMetadata,
  pub title: String, // does not re-state the metadata
  pub body: Option<String>,
}

/* Each org headline corresponds to a node.
This is the metadata necessary to interpret the headline. */
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeMetadata {
  pub id: Option<ID>,
  pub relToOrgParent: RelToOrgParent,
  pub cycle: bool,
  pub focused: bool,
  pub folded: bool,
  pub mightContainMore: bool,
  pub repeat: bool,
  pub toDelete: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelToOrgParent {
  Content, // The default relationship.
  Container, // For looking 'backward': The node contains its parent.
  AliasCol, // The node collects aliases for its parent.
  Alias, // The node is an alias for its grandparent.
  SearchResult, // When the user searches for title/alias text, each hit is one of these. If, somehow, Rust finds a SearchResult in a saved org buffer, it ignores it, including all of its recursive content.
  None, // The node bears no relationship to its parent.
}

//
// Implementations
//

impl fmt::Display for RelToOrgParent {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    let s : &str =
      match self {
        RelToOrgParent::Content => "content",
        RelToOrgParent::Container => "container",
        RelToOrgParent::AliasCol => "aliasCol",
        RelToOrgParent::Alias => "alias",
        RelToOrgParent::SearchResult => "searchResult",
        RelToOrgParent::None => "none",
      };
    write! ( f, "{}", s ) } }

impl FromStr for RelToOrgParent {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "content" => Ok ( RelToOrgParent::Content ),
      "container" => Ok ( RelToOrgParent::Container ),
      "aliasCol" => Ok ( RelToOrgParent::AliasCol ),
      "alias" => Ok ( RelToOrgParent::Alias ),
      "searchResult" => Ok ( RelToOrgParent::SearchResult ),
      "none" => Ok ( RelToOrgParent::None ),
      _ => Err ( format! ( "Unknown RelToOrgParent value: {}", s )),
    }} }

pub fn default_metadata () -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : None,
    relToOrgParent : RelToOrgParent::Content,
    cycle : false,
    focused : false,
    folded : false,
    mightContainMore : false,
    repeat : false,
    toDelete : false,
  } }

/// Parse metadata string from org-mode headline into OrgnodeMetadata.
/// Format: "(id xyz) repeated folded (relToOrgParent container)"
pub fn parse_metadata_to_headline_md (
  metadata_str : &str
) -> Result<OrgnodeMetadata, String> {
  let mut result : OrgnodeMetadata =
    default_metadata ();
  let mut chars : std::iter::Peekable<std::str::Chars> =
    metadata_str.chars ().peekable ();

  while let Some ( c ) = chars.peek () {
    // Skip whitespace
    if c.is_whitespace () {
      chars.next ();
      continue;
    }

    if *c == '(' {
      // Parse (key value) pair
      chars.next (); // consume '('
      let mut token : String = String::new ();
      // Read until whitespace or ')'
      while let Some ( &ch ) = chars.peek () {
        if ch.is_whitespace () || ch == ')' {
          break;
        }
        token.push ( ch );
        chars.next ();
      }

      // Skip whitespace between key and value
      while let Some ( &ch ) = chars.peek () {
        if ch.is_whitespace () {
          chars.next ();
        } else {
          break;
        }}

      // Read value
      let mut value : String = String::new ();
      while let Some ( &ch ) = chars.peek () {
        if ch == ')' {
          chars.next (); // consume ')'
          break;
        }
        value.push ( ch );
        chars.next ();
      }

      let key : &str = token.trim ();
      let val : &str = value.trim ();

      match key {
        "id" => { result.id = Some ( ID::from ( val )); },
        "relToOrgParent" => {
          result.relToOrgParent = match val {
            "alias"        => RelToOrgParent::Alias,
            "aliasCol"     => RelToOrgParent::AliasCol,
            "container"    => RelToOrgParent::Container,
            "content"      => RelToOrgParent::Content,
            "none"         => RelToOrgParent::None,
            "searchResult" => RelToOrgParent::SearchResult,
            _ => return Err (
              format! ( "Unknown relToOrgParent value: {}", val )),
          }; },
        _ => {
          return Err ( format! ( "Unknown metadata key: {}", key )); }}
    } else {
      // Parse bare value
      let mut token : String = String::new ();
      while let Some ( &ch ) = chars.peek () {
        if ch.is_whitespace () || ch == '(' {
          break;
        }
        token.push ( ch );
        chars.next ();
      }

      let trimmed : &str = token.trim ();
      if ! trimmed.is_empty () {
        match trimmed {
          "repeated"         => result.repeat = true,
          "folded"           => result.folded = true,
          "focused"          => result.focused = true,
          "cycle"            => result.cycle = true,
          "mightContainMore" => result.mightContainMore = true,
          "toDelete"         => result.toDelete = true,
          _ => {
            return Err ( format! ( "Unknown metadata value: {}", trimmed ));
          }}}}}
  Ok ( result ) }

/// Renders OrgnodeMetadata as a metadata string suitable for org-mode display.
/// This is the inverse of parse_metadata_to_headline_md.
/// Returns string like "(id abc123) repeated focused" etc.
pub fn headlinemd_to_string (
  metadata : &OrgnodeMetadata
) -> String {
  let mut parts : Vec<String> =
    Vec::new ();
  if let Some ( ref id ) = metadata.id {
    parts.push ( format! ( "(id {})", id.0 )); }
  if metadata.relToOrgParent != RelToOrgParent::Content {
    parts.push ( format! ( "(relToOrgParent {})", metadata.relToOrgParent )); }
  if metadata.repeat {
    parts.push ( "repeated".to_string () ); }
  if metadata.folded {
    parts.push ( "folded".to_string () ); }
  if metadata.focused {
    parts.push ( "focused".to_string () ); }
  if metadata.cycle {
    parts.push ( "cycle".to_string () ); }
  if metadata.mightContainMore {
    parts.push ( "mightContainMore".to_string () ); }
  if metadata.toDelete {
    parts.push ( "toDelete".to_string () ); }
  parts.join ( " " ) }
