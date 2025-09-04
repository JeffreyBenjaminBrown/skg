use serde::{Serialize, Deserialize};

use super::misc::ID;
use super::orgnode::OrgNodeInterp;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Node {
  // There is a 1-to-1 correspondence between Nodes and actual files -- a file can be read to a Node, and a Node can be written to a file. The files are the only permanent data. Node is the format used to initialize the TypeDB and Tantivy databases.
  // Tantivy will receive some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections between nodes. At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.

  pub title: String,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub aliases: Vec<String>,

  pub ids: Vec<ID>, // Must be nonempty. Can have length > 1 because nodes might be merged, but will usually have length = 1.
  // TODO: Use a nonempty list type (e.g. the "nonempty" crate), or else separate fields pid : String and extra_ids : Vec<String>. I'm leaning toward the latter, as the pid is special among those ids.

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub subscribes_to: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub hides_from_its_subscriptions: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub overrides_view_of: Vec<ID>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct NodeWithEphem {
  // See also /api.md.
  // The data that can be seen about a node in an Emacs buffer. Includes ephemeral view data ("folded", "focused", and "repeated"), and omits long-term data that a Node would include.
  // The same structure is used to send to and receive from Emacs. However, the `id` can only be `None` when receiving from Emacs.
  pub id       : Option<ID>,
  pub title    : String,         // See comment in the type `OrgNode`
  pub aliases  : Option<Vec<String>>, // aliases in the org-roam sense
  pub body     : Option<String>, // See comment in the type `OrgNode`
  pub folded   : bool,           // folded in the org-mode sense
  pub focused  : bool,           // where the Emacs cursor is
  pub repeated : bool, /* A node might appear in multiple places in a document. When Rust sends such a document, the second and later instances of such a node are marked "repeated". Their body and children are not displayed in Emacs. Moreover when Emacs sends them back to Rust, Rust should ignore any edits made under such repeated nodes. This permits handling infinite (recursive) data.
.
Both Rust and Emacs need to know this, because:
.
Emacs has to display repeated nodes differently, and report to Rust whether the node was repeated when saving.
.
Rust needs to save repeated nodes differently. It should ignore their content and changes to their text, because the single source of truth lies elsewhere in the view that Emacs sent Rust to save. */
  pub branches : Vec<OrgNodeInterp>, 
}

//
// Functions
//

pub fn node_example () -> Node {
  Node {
    title: "This text gets indexed.".to_string(),
    aliases: vec![],
    ids: vec![ ID::new("example") ],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: vec![ ID::new("1"),
                    ID::new("2"),
                    ID::new("3")],
    subscribes_to: vec![ID::new("11"),
                        ID::new("12"),
                        ID::new("13")],
    hides_from_its_subscriptions: vec![],
    overrides_view_of: vec![],
  } 
}