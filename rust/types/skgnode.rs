use serde::{Serialize, Deserialize, Deserializer};

use super::misc::ID;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct SkgNode {
  // There is a 1-to-1 correspondence between SkgNodes and actual .skg files -- a file can be read to a SkgNode, and a SkgNode can be written to a file. The files are the only permanent data. SkgNode is the format used to initialize the TypeDB and Tantivy databases.
  // Tantivy will receive some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections between nodes (see 'schema.tql' for how). At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.
  // PITFALL: In the Optional lists, it is important to recognize how None differs from Some( [] ). A SkgNode can be built from an OrgNode. The OrgNode might say something about the relevant field, or it might not. If the OrgNode says "this field should be empty", then we use 'Some([])'. But if the OrgNode did not mention it, we use None. Those None values will later be clobbered by whatever was on disk, via the function 'clobber_none_fields_with_data_from_disk'.

  pub title: String,

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub aliases: Option<Vec<String>>, // A node can be searched for using its title or any of its aliases, and so far using its body text too. (I might later decide not to index bodies, or to give the choice to the user.)

  pub ids: Vec<ID>, // Must be nonempty. Can have length > 1 because nodes might be merged, but will usually have length = 1.
  // TODO: Use a nonempty list type (e.g. the "nonempty" crate), or else separate fields pid : String and extra_ids : Vec<String>. I'm leaning toward the latter, as the pid is special among those ids.

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ID>, // See schema.tql.

  #[serde(
    default,
    deserialize_with = "deserialize_optional_vec_as_some_empty",
    skip_serializing_if = "option_vec_is_none"
  )]
  pub subscribes_to: Option<Vec<ID>>, // See schema.tql.

  #[serde(
    default,
    deserialize_with = "deserialize_optional_vec_as_some_empty",
    skip_serializing_if = "option_vec_is_none"
  )]
  pub hides_from_its_subscriptions: Option<Vec<ID>>, // See schema.tql.

  #[serde(
    default,
    deserialize_with = "deserialize_optional_vec_as_some_empty",
    skip_serializing_if = "option_vec_is_none"
  )]
  pub overrides_view_of: Option<Vec<ID>>, // See schema.tql.
}

//
// Helper functions
//

fn option_vec_is_empty_or_none<T> (
  option_vec: &Option<Vec<T>>
) -> bool {
  match option_vec {
    None => true,
    Some(vec) => vec.is_empty(), }}

fn option_vec_is_none<T> (
  option_vec: &Option<Vec<T>>
) -> bool {
  option_vec.is_none() }

/// Custom deserializer that converts missing fields to Some([]) instead of None.
/// This is used when reading SkgNodes from disk, where absence of a field
/// means "definitely empty" (Some([])), not "unspecified" (None).
fn deserialize_optional_vec_as_some_empty<'de, D, T>(
  deserializer: D
) -> Result<Option<Vec<T>>,
            D::Error> where
  D: Deserializer<'de>,
  T: Deserialize<'de>, {

  let opt: Option<Vec<T>> = Option::deserialize(
    deserializer )? ;
  Ok ( Some ( opt.unwrap_or_else(Vec::new )) ) }


//
// Functions
//

pub fn skgnode_example () -> SkgNode {
  SkgNode {
    title: "This text gets indexed.".to_string(),
    aliases: None,
    ids: vec![ ID::new("example") ],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: vec![ ID::new("1"),
                    ID::new("2"),
                    ID::new("3")],
    subscribes_to: Some(vec![ID::new("11"),
                             ID::new("12"),
                             ID::new("13")]),
    hides_from_its_subscriptions: None,
    overrides_view_of: None, }}

/// Useful for making tests more readable.
pub fn empty_skgnode () -> SkgNode {
  SkgNode {
    title                        : String::new (),
    aliases                      : None,
    ids                          : vec![],
    body                         : None,
    contains                     : vec![],
    subscribes_to                : None,
    hides_from_its_subscriptions : None,
    overrides_view_of            : None,
  }}
