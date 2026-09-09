//! NodeTypedb: what TypeDB stores.
//!
//! A node entity plus its outbound graph relations. No title,
//! aliases, or body (those live in Tantivy / disk).
//!
//! 'textlinks_to' is unusual: it's derived from body parsing, not
//! stored on disk or on NodeComplete. The constructor takes it
//! alongside a &NodeComplete so the caller explicitly supplies
//! the parsed result.

use crate::types::misc::{ID, MSV, SourceName, members_msv, members_of};
use crate::types::nodes::complete::NodeComplete;
use crate::types::textlinks::textlinks_from_node;

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
  pub fn from_nodecomplete_and_textlinks (
    complete  : &NodeComplete,
    textlinks : Vec<ID>,
  ) -> Self {
    NodeTypedb {
      pid                          : complete . pid . clone (),
      source                       : complete . source . clone (),
      extra_ids                    : complete . extra_ids . clone (),
      contains                     : members_of (&complete . contains),
      subscribes_to                : members_msv (&complete . subscribes_to),
      hides_from_its_subscriptions : members_msv (&complete . hides_from_its_subscriptions),
      overrides_view_of            : members_msv (&complete . overrides_view_of),
      textlinks_to                 : textlinks,
    }
  }

  /// Convenience: build a NodeTypedb from a complete node, parsing
  /// textlinks from its title+body in the process. Equivalent to
  /// calling 'from_nodecomplete_and_textlinks' with parsed textlinks.
  pub fn from_complete_parsing_textlinks (
    complete : &NodeComplete,
  ) -> Self {
    let textlinks : Vec<ID> =
      textlinks_from_node (complete)
      . into_iter ()
      . map ( |tl| tl . id )
      . collect ();
    Self::from_nodecomplete_and_textlinks (complete, textlinks)
  }
}
