// This code is tested by /tests/file_io.rs

use serde::{Serialize, Deserialize};
use std::fmt;
use std::ops::Deref;
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ID(String);

impl ID {
    pub fn new<S: Into<String>>(s: S) -> Self {
        ID(s.into()) }
    pub fn as_str(&self) -> &str {
        &self.0 } }

impl Deref for ID { // Lets ID be used like a String in (more?) cases.
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0 } }

impl fmt::Display for ID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0) } }

impl From<String> for ID {
    fn from(s: String) -> Self {
        ID(s) } }

impl From<&str> for ID {
    fn from(s: &str) -> Self {
        ID(s.to_string()) } }

#[derive(Serialize, Deserialize, Debug)]
pub struct SkgNode {
    // Tantivy will receive some of this data,
    // and TypeDB some other subset.
    // And at least one field, `unindexed_text`, is known to neither.

    #[serde(skip)]  // Next field is not read from or written to JSON.
    pub path: PathBuf,  // Inferred from file location.

    pub format: String,
    pub id: ID,
    pub context: Option<ID>,
    pub is_comment: bool,
    pub titles: Vec<String>,
    pub unindexed_text: String, // unknown to both Tantivy & TypeDB
    pub nodes_contained: Vec<ID>,
    pub nodes_subscribed: Vec<ID>, }

pub fn skgnode_example() -> SkgNode
{ SkgNode {
    path: PathBuf::from(
	"tests/file_io/generated/example.skg"),
    format: "base".to_string(),
    id: ID::new("123"),
    context: Some(ID::new("456")),
    is_comment: false,
    titles: vec![
        "This text gets indexed.".to_string(),
        "Maybe searching other text could find this note.".to_string(),
	"\"Quotation marks\" in titles are escaped.".to_string() ],
    unindexed_text: "this one string could span pages".to_string(),
    nodes_contained: vec![ID::new("1"),
                          ID::new("2"),
                          ID::new("3")],
    nodes_subscribed: vec![ID::new("11"),
                           ID::new("12"),
                           ID::new("13")], } }
