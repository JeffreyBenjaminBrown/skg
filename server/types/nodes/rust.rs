//! GraphnodeInRust: the projection held in the in-Rust graph.
//!
//! Wide enough to match everything Graphnode carries (except
//! derived fields), plus linksTo — derived from body parsing at
//! GraphnodeInRust construction time.

use crate::types::misc::{ID, MSV, RelPartner, SkgrepoName, members_msv};
use crate::types::nodes::complete::{Flag, Graphnode};
use crate::types::links::links_from_node;
use crate::skgrepo_sets::SkgrepoRestriction;

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
      . map ( |tl| tl . gnid )
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

impl GraphnodeInRust {
  /// The texts of the node's aliases, without their relRepos; empty
  /// when the aliases are unspecified.
  pub fn alias_texts (
    &self,
  ) -> Vec<String> {
    members_msv (&self . aliases) . or_default () . to_vec () }

  /// Every ID of the node: its pid, then its extra IDs.
  pub fn ids (
    &self,
  ) -> Vec<ID> {
    std::iter::once ( self . pid . clone () )
      . chain ( self . extra_ids . iter () . cloned () )
      . collect () }

  /// Each alias whose record the skgrepo restriction shows (each,
  /// under none), its text, in order, with its relRepo where that
  /// differs from the node's home: what an alias headline names.
  pub fn aliases_with_relRepo_unless_home (
    &self,
    restriction : Option<&SkgrepoRestriction>,
  ) -> Vec<(String, Option<SkgrepoName>)> {
    self . aliases . or_default () . iter ()
      . filter ( |alias| SkgrepoRestriction::record_is_visible (
        restriction, &alias . relRepo ))
      . map ( |alias| ( alias . member . clone (),
                        ( alias . relRepo != self . home_skgrepo )
                          . then ( || alias . relRepo . clone () ) ))
      . collect () }
}
