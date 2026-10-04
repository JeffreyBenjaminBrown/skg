//! GraphnodeInTantivy: what Tantivy indexes.
//!
//! Title, aliases, and body for full-text search. No relations.
//! Includes node flags because 'Had_ID_Before_Import' feeds the
//! context-ranking score multiplier and 'NoSearchMatching' feeds Tantivy's
//! mandatory direct-match exclusion.

use crate::types::misc::{ID, MSV, RelPartner, RepoName};
use crate::types::nodes::complete::{Flag, Graphnode};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct GraphnodeInTantivy {
  pub pid     : ID,
  pub home_repo  : RepoName, // the home; each alias doc instead
                            // carries ITS OWN level (see 'aliases')
  pub title   : String,
  pub overPrivateText_telescope : bool,
  // Aliases keep their PRIVACY LEVELS: each alias document's
  // repo field is the alias's relRepo, not the node's home, so a
  // restricted search cannot match a private alias of a public
  // node (dbs-and-search, 5_plan.org).
  pub aliases : MSV<RelPartner<String>>,
  pub body    : Option<String>,
  pub misc    : Vec<Flag>,
}

impl From<&Graphnode> for GraphnodeInTantivy {
  /// Keep title, aliases (with relRepos), body, misc (Tantivy indexes
  /// these). Drop relations.
  fn from (c: &Graphnode) -> Self {
    GraphnodeInTantivy {
      pid     : c . pid . clone (),
      home_repo  : c . home_repo . clone (),
      title   : c . title . clone (),
      overPrivateText_telescope : c . overPrivateText_telescope,
      aliases : c . aliases . clone (),
      body    : c . body . clone (),
      misc    : c . misc . clone (),
    }
  }
}
