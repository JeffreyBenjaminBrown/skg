use serde::{Serialize, Deserialize};

use crate::util::option_vec_is_empty_or_none;
use super::misc::{ID, SourceName};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct SkgNode {
  // There is a 1-to-1 correspondence between SkgNodes and actual .skg files -- a file can be read to a SkgNode, and a SkgNode can be written to a file. The files are the only permanent data. SkgNode is the format used to initialize the TypeDB and Tantivy databases.
  // Tantivy will receive some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections between nodes (see 'schema.tql' for how). At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.
  // PITFALL: Some([]) vs. None in the Optional lists:
  // - On disk the distinction is not needed. Both Some([]) and None are both rendered on disk as a missing field.
  // - When multiple SkgNodes need to be reconciled (as in reconcile_same_id_instructions or noneclobber_skgnode), the distinction matters. This is because a SkgNode can be built from an ViewNode, which might or might not say something about the relevant field. If the ViewNode intends to convey "this field *should* be empty", then it reads 'Some([])'. If instead the ViewNode did not say anything about that field, we use None -- and later clobber it with whatever was on disk for that field, via 'noneclobber_skgnode'.

  pub title: String,

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub aliases: Option<Vec<String>>, // A node can be searched for using its title or any of its aliases, and so far using its body text too. (I might later decide not to index bodies, or to give the choice to the user.)

  #[serde(skip_serializing, skip_deserializing)]
  pub source: SourceName, // source nickname, inferred from file location and SkgConfig

  pub ids: Vec<ID>, // Must be nonempty. Can have length > 1 because nodes might be merged, but will usually have length = 1.
  // TODO: Use a nonempty list type (e.g. the "nonempty" crate), or else separate fields pid : String and extra_ids : Vec<String>. I'm leaning toward the latter, as the pid is special among those ids.

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub contains: Option<Vec<ID>>, // See schema.tql.

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub subscribes_to: Option<Vec<ID>>, // See schema.tql.

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub hides_from_its_subscriptions: Option<Vec<ID>>, // See schema.tql.

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub overrides_view_of: Option<Vec<ID>>, // See schema.tql.
}

impl SkgNode {
  /// Returns the primary ID, or an error if ids is empty.
  pub fn primary_id(&self) -> Result<&ID, String> {
    self.ids.get(0).ok_or_else(||
      format!("SkgNode '{}' has no IDs", self.title)) }
}

//
// Functions
//

/// Useful for making tests more readable.
pub fn empty_skgnode () -> SkgNode {
  SkgNode {
    title                        : String::new (),
    aliases                      : None,
    source                       : SourceName::from("main"),
    ids                          : vec![],
    body                         : None,
    contains                     : None,
    subscribes_to                : None,
    hides_from_its_subscriptions : None,
    overrides_view_of            : None,
  }}
