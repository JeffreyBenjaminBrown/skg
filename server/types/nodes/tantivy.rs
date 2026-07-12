//! NodeTantivy: what Tantivy indexes.
//!
//! Title, aliases, and body for full-text search. No relations.
//! Includes 'misc' because 'Had_ID_Before_Import' feeds the
//! context-ranking score multiplier (see [[../../../server/context.rs][context.rs]]).

use crate::types::misc::{ID, MSV, PrivaciedMember, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeTantivy {
  pub pid     : ID,
  pub source  : SourceName, // the home; each alias doc instead
                            // carries ITS OWN level (see 'aliases')
  pub title   : String,
  // Aliases keep their PRIVACY LEVELS: each alias document's
  // source field is the alias's level, not the node's home, so a
  // restricted search cannot match a private alias of a public
  // node (dbs-and-search, 5_plan.org).
  pub aliases : MSV<PrivaciedMember<String>>,
  pub body    : Option<String>,
  pub misc    : Vec<FileProperty>,
}

impl From<&NodeComplete> for NodeTantivy {
  /// Keep title, aliases (leveled), body, misc (Tantivy indexes
  /// these). Drop relations.
  fn from (c: &NodeComplete) -> Self {
    NodeTantivy {
      pid     : c . pid . clone (),
      source  : c . source . clone (),
      title   : c . title . clone (),
      aliases : c . aliases . clone (),
      body    : c . body . clone (),
      misc    : c . misc . clone (),
    }
  }
}
