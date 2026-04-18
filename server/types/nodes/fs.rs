//! NodeFS: the on-disk shape of a node.
//!
//! What the YAML on disk literally holds. Bit-exact with today's
//! SkgNode on-disk serialization — does NOT include 'source'
//! (which is inferred from file location, not stored in YAML).
//!
//! Conversions:
//! - 'NodeFS::into_complete(source)' produces a NodeComplete.
//! - 'From<&NodeComplete> for NodeFS' drops source for writing.
//!
//! See /home/ubuntu/.claude/plans/many-and-better-node-types.org
//! (Plan A) for the overall design.

use serde::{Serialize, Deserialize};

use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct NodeFS {
  // Field order and serde attributes mirror 'SkgNode' exactly, so
  // YAML output is bit-exact. When anything here changes, update
  // the matching field in 'SkgNode' (see server/types/skgnode.rs)
  // and vice versa, until the SkgNode alias is retired.

  pub title: String,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub aliases: MSV<String>,

  pub pid: ID,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub extra_ids: Vec<ID>,

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ID>,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub subscribes_to: MSV<ID>,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub hides_from_its_subscriptions: MSV<ID>,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub overrides_view_of: MSV<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub misc: Vec<FileProperty>,
}

impl NodeFS {
  /// Attach a source to produce a NodeComplete.
  pub fn into_complete (
    self,
    source : SourceName,
  ) -> NodeComplete {
    NodeComplete {
      title                        : self . title,
      aliases                      : self . aliases,
      source,
      pid                          : self . pid,
      extra_ids                    : self . extra_ids,
      body                         : self . body,
      contains                     : self . contains,
      subscribes_to                : self . subscribes_to,
      hides_from_its_subscriptions : self . hides_from_its_subscriptions,
      overrides_view_of            : self . overrides_view_of,
      misc                         : self . misc,
    }
  }
}

impl From<&NodeComplete> for NodeFS {
  /// Drop 'source'. Everything else is cloned.
  fn from (c: &NodeComplete) -> Self {
    NodeFS {
      title                        : c . title . clone (),
      aliases                      : c . aliases . clone (),
      pid                          : c . pid . clone (),
      extra_ids                    : c . extra_ids . clone (),
      body                         : c . body . clone (),
      contains                     : c . contains . clone (),
      subscribes_to                : c . subscribes_to . clone (),
      hides_from_its_subscriptions : c . hides_from_its_subscriptions . clone (),
      overrides_view_of            : c . overrides_view_of . clone (),
      misc                         : c . misc . clone (),
    }
  }
}
