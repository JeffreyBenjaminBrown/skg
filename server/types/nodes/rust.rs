//! GraphnodeInRust: the projection held in the in-Rust graph.
//!
//! Wide enough to match everything Graphnode carries (except
//! derived fields), plus linksTo — derived from body parsing at
//! GraphnodeInRust construction time.

use crate::types::misc::{ID, MSV, RelPartner, SkgrepoName};
use crate::types::nodes::complete::{Flag, Graphnode};
use crate::types::links::links_from_node;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct GraphnodeInRust {
  pub pid                          : ID,
  pub home_skgrepo                 : SkgrepoName,
  pub extra_ids                    : Vec<ID>,
  pub title                        : String,
  pub overPrivateText_telescope               : bool,
  pub aliases                      : MSV<RelPartner<String>>,
  pub body                         : Option<String>,
  pub contains                     : Vec<RelPartner<ID>>,
  pub subscribesTo                 : MSV<RelPartner<ID>>,
  pub hidesFromSubs                : MSV<RelPartner<ID>>,
  pub overrides                    : MSV<RelPartner<ID>>,
  pub flags                        : Vec<Flag>,
  // PITFALL: derived from the text.
  // Parsed from title+body via 'links_from_node' during
  // construction; never read from disk.
  pub linksTo                  : Vec<ID>,
}

impl From<&Graphnode> for GraphnodeInRust {
  /// Derive 'linksTo' by parsing title+body; copy everything else.
  fn from (c: &Graphnode) -> Self {
    let linksTo : Vec<ID> =
      links_from_node (c)
      . into_iter ()
      . map ( |tl| tl . skgid )
      . collect ();
    GraphnodeInRust {
      pid                          : c . pid . clone (),
      home_skgrepo                 : c . home_skgrepo . clone (),
      extra_ids                    : c . normalized_extra_ids (),
      title                        : c . title . clone (),
      overPrivateText_telescope               : c . overPrivateText_telescope,
      aliases                      : c . aliases . clone (),
      body                         : c . body . clone (),
      contains                     : c . contains . clone (),
      subscribesTo                 : c . subscribesTo . clone (),
      hidesFromSubs                : c . hidesFromSubs . clone (),
      overrides                    : c . overrides . clone (),
      flags                        : c . flags . clone (),
      linksTo,
    }
  }
}
