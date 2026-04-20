//! NodeTantivy: what Tantivy indexes.
//!
//! Title, aliases, and body for full-text search. No relations.
//! Includes 'misc' because 'Had_ID_Before_Import' feeds the
//! context-ranking score multiplier (see [[../../../server/context.rs][context.rs]]).

use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeTantivy {
  pub pid     : ID,
  pub source  : SourceName,
  pub title   : String,
  pub aliases : MSV<String>,
  pub body    : Option<String>,
  pub misc    : Vec<FileProperty>,
}

impl From<&NodeComplete> for NodeTantivy {
  /// Keep title, aliases, body, misc (Tantivy indexes these).
  /// Drop relations.
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
