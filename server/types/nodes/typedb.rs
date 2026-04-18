//! NodeTypedb: what TypeDB stores.
//!
//! A node entity plus its outbound graph relations. No title,
//! aliases, or body (those live in Tantivy / disk).
//!
//! 'textlinks_to' is unusual: it's derived from body parsing, not
//! stored on disk or on NodeComplete. The constructor takes it
//! alongside a &NodeComplete so the caller explicitly supplies
//! the parsed result.
//!
//! See /home/ubuntu/.claude/plans/many-and-better-node-types.org
//! (Plan A) for the overall design.

use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::NodeComplete;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeTypedb {
  pub pid                          : ID,
  pub source                       : SourceName,
  pub extra_ids                    : Vec<ID>,
  pub contains                     : Vec<ID>,
  pub subscribes_to                : MSV<ID>,
  pub hides_from_its_subscriptions : MSV<ID>,
  pub overrides_view_of            : MSV<ID>,
  // PITFALL: derived from the text.
  // Not even NodeComplete has this
  // (explicitly, although it has the text).
  pub textlinks_to                 : Vec<ID>,
}

impl NodeTypedb {
  /// Build a NodeTypedb from a complete node plus parsed textlinks.
  /// 'textlinks' is the caller's responsibility (derived from body
  /// parsing); NodeComplete doesn't carry it.
  pub fn from_complete_and_textlinks (
    complete  : &NodeComplete,
    textlinks : Vec<ID>,
  ) -> Self {
    NodeTypedb {
      pid                          : complete . pid . clone (),
      source                       : complete . source . clone (),
      extra_ids                    : complete . extra_ids . clone (),
      contains                     : complete . contains . clone (),
      subscribes_to                : complete . subscribes_to . clone (),
      hides_from_its_subscriptions : complete . hides_from_its_subscriptions . clone (),
      overrides_view_of            : complete . overrides_view_of . clone (),
      textlinks_to                 : textlinks,
    }
  }
}
