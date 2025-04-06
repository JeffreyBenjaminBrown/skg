// This code is tested by /tests/file_io.rs

use serde::{Serialize, Deserialize};
use std::path::{PathBuf};

#[derive(Serialize, Deserialize, Debug)]
pub struct SkgNode {
    // Tantivy will receive some of this data,
    // and TypeDB some other subset.
    // And at least one field, `unindexed_text`, is known to neither.

    #[serde(skip)]  // Next field is not read from or written to JSON.
    pub path: PathBuf,  // Inferred from file location.

    pub format: String,
    pub id: String,
    pub context: Option<String>,
    pub is_comment: bool,
    pub titles: Vec<String>,
    pub unindexed_text: String, // unknown to both Tantivy & TypeDB
    pub nodes_contained: Vec<String>,
    pub nodes_subscribed: Vec<String>, }

pub fn skgnode_example() -> SkgNode
{ SkgNode {
    path: PathBuf::from(
	"tests/file_io/generated/example.skg"),
    format: "base".to_string(),
    id: "123".to_string(),
    context: Some("456".to_string()),
    is_comment: false,
    titles: vec![
        "This text gets indexed.".to_string(),
        "Maybe searching other text could find this note.".to_string(),
	"\"Quotation marks\" in titles are escaped.".to_string() ],
    unindexed_text: "this one string could span pages".to_string(),
    nodes_contained: vec!["1".to_string(),
                          "2".to_string(),
                          "3".to_string()],
    nodes_subscribed: vec!["11".to_string(),
                           "12".to_string(),
                           "13".to_string()], } }
