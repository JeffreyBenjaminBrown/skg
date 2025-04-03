use std::fs::File;
use std::io::{self, Write};
use serde::{Serialize, Deserialize};

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
    let example = SkgNode {
        format: "base".to_string(),
        id: "123".to_string(),
        context: Some("456".to_string()),
        is_comment: false,
        titles: vec![
            "This text gets indexed.".to_string(),
            "Maybe searching other text could find this note.".to_string(),
        ],
        unindexed_text: "this single string could span pages".to_string(),
        nodes_contained: vec!["1".to_string(), "2".to_string(), "3".to_string()],
        nodes_subscribed: vec!["11".to_string(), "12".to_string(), "13".to_string()],
    };

    let out_filename = "example_generated.skg";
    write_skgnode_to_file(&example, out_filename)?;
    println!("SkgNode has been written to {}", out_filename);
    Ok(()) }

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
