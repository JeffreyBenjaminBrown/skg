//! GraphnodeInRust: the projection held in the in-Rust graph.
//!
//! Wide enough to match everything Graphnode carries (except
//! derived fields), plus linksTo — derived from body parsing at
//! GraphnodeInRust construction time.
//!
//! It holds a node as its files read back: an empty list is
//! 'MSV::Unspecified', since no file writes an empty list
//! ('GraphnodeOnDisk'), so a node a save put in the graph equals the
//! one a reload of its files would.

use crate::types::misc::{ID, MSV, RelPartner, SkgrepoName, members_msv};
use crate::types::nodes::attribution::Attribution;
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
  pub forks                        : MSV<RelPartner<ID>>,
  pub hides                        : MSV<RelPartner<ID>>,
  pub flags                        : Vec<Flag>,
  pub attribution                  : Option<Attribution>,
  // PITFALL: derived from the text.
  // Parsed from title+body via 'links_from_node' during
  // construction; never read from disk.
  pub linksTo                  : Vec<ID>,
}

impl From<&Graphnode> for GraphnodeInRust {
  /// Derive 'linksTo' by parsing title+body; copy everything else, an
  /// empty list as 'MSV::Unspecified' (see the module comment).
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
      aliases                      : unspecified_if_empty (&c . aliases),
      body                         : c . body . clone (),
      contains                     : c . contains . clone (),
      forks                        : unspecified_if_empty (&c . forks),
      hides                        : unspecified_if_empty (&c . hides),
      flags                        : c . flags . clone (),
      attribution                  : c . attribution . clone (),
      linksTo,
    }
  }
}

impl From<&GraphnodeInRust> for Graphnode {
  /// Copy every field but the derived 'linksTo', verbatim: an
  /// unspecified list stays unspecified.
  fn from (rust: &GraphnodeInRust) -> Self {
    Graphnode {
      pid                          : rust . pid . clone (),
      home_skgrepo                 : rust . home_skgrepo . clone (),
      extra_ids                    : rust . extra_ids . clone (),
      title                        : rust . title . clone (),
      overPrivateText_telescope               : rust . overPrivateText_telescope,
      aliases                      : rust . aliases . clone (),
      body                         : rust . body . clone (),
      contains                     : rust . contains . clone (),
      forks                        : rust . forks . clone (),
      hides                        : rust . hides . clone (),
      flags                        : rust . flags . clone (),
      attribution                  : rust . attribution . clone (),
    }
  }
}

/// 'list', or 'MSV::Unspecified' if it is empty.
fn unspecified_if_empty<T : Clone> (
  list : &MSV<T>,
) -> MSV<T> {
  match list {
    MSV::Specified (members) if members . is_empty () => MSV::Unspecified,
    _ => list . clone (), }}

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
    restriction : &SkgrepoRestriction,
  ) -> Vec<(String, Option<SkgrepoName>)> {
    SkgrepoRestriction::visible_records (restriction, self . aliases . or_default ())
      . map ( |alias| ( alias . member . clone (),
                        ( alias . relRepo != self . home_skgrepo )
                          . then ( || alias . relRepo . clone () ) ))
      . collect () }
}
