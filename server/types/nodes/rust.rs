//! NodeRust: the projection held in the in-memory graph.
//!
//! Wide enough to match everything in-memory NodeComplete consumers
//! currently read, plus textlinks_to — derived from body parsing at
//! NodeRust construction time, matching how NodeTypedb is built.
//! Narrower than NodeComplete only in omitting 'misc' (which is
//! consumed only by Tantivy indexing and the org-roam importer, not
//! by in-memory readers).

use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::textlinks::textlinks_from_node;

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
  // PITFALL: derived from the text.
  // Parsed from title+body via 'textlinks_from_node' during
  // construction; never read from disk.
  pub textlinks_to                 : Vec<ID>,
}

impl From<&NodeComplete> for NodeRust {
  /// Drop 'misc'; derive 'textlinks_to' by parsing title+body.
  fn from (c: &NodeComplete) -> Self {
    let textlinks_to : Vec<ID> =
      textlinks_from_node (c)
      . into_iter ()
      . map ( |tl| tl . id )
      . collect ();
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
      textlinks_to,
    }
  }
}
