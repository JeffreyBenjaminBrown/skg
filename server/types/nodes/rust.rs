//! NodeRust: the Tier A projection held in memory.
//!
//! Wide enough to match everything in-memory NodeComplete consumers
//! currently read (render, save completion, merge), so deploying
//! NodeRust doesn't regress behavior. Narrower than NodeComplete
//! only in omitting 'misc' (which is consumed only by Tantivy
//! indexing and the org-roam importer, not by in-memory readers).
//!
//! Plan C will use 'im::HashMap<ID, NodeRust>' as the in-memory
//! graph's value type. For now this struct just exists so Plan C
//! can build on Plan A.
//!
//! See /home/ubuntu/.claude/plans/many-and-better-node-types.org
//! (Plan A) for the overall design.

use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::NodeComplete;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeRust {
  pub pid                          : ID,
  pub source                       : SourceName,
  pub extra_ids                    : Vec<ID>,
  pub title                        : String,
  pub aliases                      : MSV<String>,
  pub body                         : Option<String>,
  pub contains                     : Vec<ID>,
  pub subscribes_to                : MSV<ID>,
  pub hides_from_its_subscriptions : MSV<ID>,
  pub overrides_view_of            : MSV<ID>,
}

impl From<&NodeComplete> for NodeRust {
  /// Drop only 'misc'. Everything else is kept so in-memory
  /// NodeComplete consumers don't regress.
  fn from (c: &NodeComplete) -> Self {
    NodeRust {
      pid                          : c . pid . clone (),
      source                       : c . source . clone (),
      extra_ids                    : c . extra_ids . clone (),
      title                        : c . title . clone (),
      aliases                      : c . aliases . clone (),
      body                         : c . body . clone (),
      contains                     : c . contains . clone (),
      subscribes_to                : c . subscribes_to . clone (),
      hides_from_its_subscriptions : c . hides_from_its_subscriptions . clone (),
      overrides_view_of            : c . overrides_view_of . clone (),
    }
  }
}
