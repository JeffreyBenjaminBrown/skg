//! NodeRust: the projection held in the in-Rust graph.
//!
//! Wide enough to match everything NodeComplete carries (except
//! derived fields), plus links_to — derived from body parsing at
//! NodeRust construction time.

use crate::types::misc::{ID, MSV, RelPartner, RepoName};
use crate::types::nodes::complete::{Flag, NodeComplete};
use crate::types::links::links_from_node;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeRust {
  pub pid                          : ID,
  pub home_repo                       : RepoName,
  pub extra_ids                    : Vec<ID>,
  pub title                        : String,
  pub overPrivateText_telescope               : bool,
  pub aliases                      : MSV<RelPartner<String>>,
  pub body                         : Option<String>,
  pub contains                     : Vec<RelPartner<ID>>,
  pub subscribes_to                : MSV<RelPartner<ID>>,
  pub hides_from_its_subscriptions : MSV<RelPartner<ID>>,
  pub overrides_view_of            : MSV<RelPartner<ID>>,
  pub misc                         : Vec<Flag>,
  // PITFALL: derived from the text.
  // Parsed from title+body via 'links_from_node' during
  // construction; never read from disk.
  pub links_to                 : Vec<ID>,
}

impl From<&NodeComplete> for NodeRust {
  /// Derive 'links_to' by parsing title+body; copy everything else.
  fn from (c: &NodeComplete) -> Self {
    let links_to : Vec<ID> =
      links_from_node (c)
      . into_iter ()
      . map ( |tl| tl . id )
      . collect ();
    NodeRust {
      pid                          : c . pid . clone (),
      home_repo                       : c . home_repo . clone (),
      extra_ids                    : c . normalized_extra_ids (),
      title                        : c . title . clone (),
      overPrivateText_telescope               : c . overPrivateText_telescope,
      aliases                      : c . aliases . clone (),
      body                         : c . body . clone (),
      contains                     : c . contains . clone (),
      subscribes_to                : c . subscribes_to . clone (),
      hides_from_its_subscriptions : c . hides_from_its_subscriptions . clone (),
      overrides_view_of            : c . overrides_view_of . clone (),
      misc                         : c . misc . clone (),
      links_to,
    }
  }
}
