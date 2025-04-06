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

pub fn reverse_skgnode(node: &SkgNode) -> SkgNode {
    // Create a new SkgNode with reversed lists.
    // This is only for testing purposes,
    // to show reading from and writing to disk work.
    let mut reversed_titles = node.titles.clone();
    reversed_titles.reverse();

    let mut reversed_nodes_contained =
        node.nodes_contained.clone();
    reversed_nodes_contained.reverse();

    let mut reversed_nodes_subscribed =
        node.nodes_subscribed.clone();
    reversed_nodes_subscribed.reverse();

    SkgNode {
        path: node.path.clone(),
        format: node.format.clone(),
        id: node.id.clone(),
        context: node.context.clone(),
        is_comment: node.is_comment,
        titles: reversed_titles,
        unindexed_text: node.unindexed_text.clone(),
        nodes_contained: reversed_nodes_contained,
        nodes_subscribed: reversed_nodes_subscribed,
    } }
