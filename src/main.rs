use std::fs::File;
use std::io::{self, Write, Read};
use serde::{Serialize, Deserialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug)]
struct SkgNode {
    format: String,
    id: String,
    context: Option<String>,
    is_comment: bool,
    titles: Vec<String>,
    unindexed_text: String,
    nodes_contained: Vec<String>,
    nodes_subscribed: Vec<String>, }

fn main() -> io::Result<()> {
    // Create and write the example node to file
    let example = skgnode_example();
    let out_filename = "example_generated.skg";
    write_skgnode_to_file(&example, out_filename)?;
    println!("SkgNode has been written to {}", out_filename);

    let read_node = skgnode_from_file(out_filename)?;
    let reversed = reverse_skgnode(&read_node);
    let reversed_filename = "example_reversed.skg";
    write_skgnode_to_file(&reversed, reversed_filename)?;
    println!("Reversed SkgNode has been written to {}",
             reversed_filename);
    Ok(()) }

fn skgnode_from_file(file_path: &str) -> io::Result<SkgNode> {
    // Open the file
    let mut file = File::open(file_path)?;

    // Read file contents into a string
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    // Deserialize the JSON string into a SkgNode
    let skgnode: SkgNode = serde_json::from_str(&contents)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    Ok(skgnode)
}

fn reverse_skgnode(node: &SkgNode) -> SkgNode {
    // Create a new SkgNode with reversed lists
    let mut reversed_titles = node.titles.clone();
    reversed_titles.reverse();

    let mut reversed_nodes_contained = node.nodes_contained.clone();
    reversed_nodes_contained.reverse();

    let mut reversed_nodes_subscribed = node.nodes_subscribed.clone();
    reversed_nodes_subscribed.reverse();

    SkgNode {
        format: node.format.clone(),
        id: node.id.clone(),
        context: node.context.clone(),
        is_comment: node.is_comment,
        titles: reversed_titles,
        unindexed_text: node.unindexed_text.clone(),
        nodes_contained: reversed_nodes_contained,
        nodes_subscribed: reversed_nodes_subscribed,
    }
}

fn skgnode_example() -> SkgNode
{ SkgNode {
    format: "base".to_string(),
    id: "123".to_string(),
    context: Some("456".to_string()),
    is_comment: false,
    titles: vec![
        "This text gets indexed.".to_string(),
        "Maybe searching other text could find this note.".to_string(),
    ],
    unindexed_text: "this one string could span pages".to_string(),
    nodes_contained: vec!["1".to_string(),
                          "2".to_string(),
                          "3".to_string()],
    nodes_subscribed: vec!["11".to_string(),
                           "12".to_string(),
                           "13".to_string()],
} }

fn write_skgnode_to_file(
    skgnode: &SkgNode, file_path: &str)
    -> io::Result<()> {
    let mut file = File::create(file_path)?;

    writeln!(file, "{{ \"format\" : \"{}\",",
             skgnode.format)?;
    writeln!(file, "  \"id\" : \"{}\",",
             skgnode.id)?;
    if let Some(context) = &skgnode.context {
        writeln!(file, "  \"context\" : \"{}\",", context)?;
    }
    writeln!(file, "  \"is_comment\": {},", skgnode.is_comment)?;
    writeln!(file, "  \"titles\":[")?;
    for (i, title) in skgnode.titles.iter().enumerate() {
        if i < skgnode.titles.len() - 1 {
            writeln!(file, "    \"{}\",", escape_string(title))?;
        } else {
            writeln!(file, "    \"{}\"", escape_string(title))?;
        }
    }
    writeln!(file, "  ], \"unindexed_text\":\n    \"{}\",",
             escape_string(&skgnode.unindexed_text))?;
    writeln!(file, "  \"nodes_contained\": [")?;
    for (i, content_id) in
        skgnode.nodes_contained.iter().enumerate() {
        if i < skgnode.nodes_contained.len() - 1 {
            writeln!(file, "    \"{}\",", content_id)?;
        } else {
            writeln!(file, "    \"{}\"", content_id)?;
        }
    }
    writeln!(file, "  ], \"nodes_subscribed\": [")?;
    for (i, subscription_id)
        in skgnode.nodes_subscribed.iter().enumerate() {
        if i < skgnode.nodes_subscribed.len() - 1 {
            writeln!(file, "    \"{}\",", subscription_id)?;
        } else {
            writeln!(file, "    \"{}\"", subscription_id)?;
        }
    }
    writeln!(file, "]}}")?;
    Ok(()) }

fn escape_string(s: &str) -> String {
    s.replace("\"", "\\\"") }
