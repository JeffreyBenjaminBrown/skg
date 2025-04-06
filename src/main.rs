use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::error::Error;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ID(String);

#[derive(Debug, Serialize, Deserialize)]
pub struct Node {
    id: ID,
    is_comment: bool,
    #[serde(skip)]  // This field is not serialized/deserialized
    path: PathBuf,  // Inferred from file location, not part of JSON
    context: Option<ID>,
    contains: Vec<ID>,
    subscribes_to: Vec<ID>,
}

impl Node {
    pub fn from_file(file_path: &Path)
		     -> Result<Self, Box<dyn Error>> {
        let content = fs::read_to_string(file_path)?;
        // Deserialize, then set the path field afterward
        let mut node: Node = serde_json::from_str(&content)?;
        node.path = file_path.to_path_buf();
        Ok(node) }

    pub fn save_to_file(&self, file_path: &Path) -> Result<(), Box<dyn Error>> {
        let json = serde_json::to_string_pretty(&self)?;
        fs::write(file_path, json)?;
        Ok(()) }

    pub fn add_to_contains(&mut self, ids: Vec<ID>) {
        self.contains.extend(ids); } }

fn main() -> Result<(), Box<dyn Error>> {
    create_sample_input()?;
    let input_file = Path::new("input_node.skg");
    let output_file = Path::new("output_node.skg");
    let mut node = Node::from_file(input_file)?;
    node.add_to_contains(vec![
        ID("a".to_string()),
        ID("b".to_string()),
        ID("c".to_string())
    ]);
    node.save_to_file(output_file)?;
    println!(
	"Successfully read from {:?}, modified, and wrote to {:?}",
        node.path, output_file);
    Ok(()) }

fn create_sample_input() -> Result<(), Box<dyn Error>> {
    let sample_node = Node {
        id: ID("node1".to_string()),
        is_comment: false,
        path: PathBuf::new(),
        context: Some(ID("parent_context".to_string())),
        contains: vec![ID("child1".to_string()),
		       ID("child2".to_string())],
        subscribes_to: vec![ID("topic1".to_string())],
    };

    // The path field will be skipped during serialization;
    // per the type definition.
    let json = serde_json::to_string_pretty(&sample_node)?;
    fs::write("input_node.skg", json)?;
    println!("Sample input file created: input_node.skg");
    Ok(()) }
