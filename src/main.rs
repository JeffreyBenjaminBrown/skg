use std::fs::File;
use std::io::{self, Write};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
struct Schema {
    format: String,
    id: String,
    context: Option<String>,
    is_comment: bool,
    titles: Vec<String>,
    unindexed_text: String,
    nodes_contained: Vec<String>,
    nodes_subscribed: Vec<String>,
}

fn write_schema_to_file(schema: &Schema, file_path: &str) -> io::Result<()> {
    let mut file = File::create(file_path)?;

    // Opening brace
    writeln!(file, "{{")?;

    // format (string)
    writeln!(file, "  \"format\" : \"{}\",", schema.format)?;

    // id (string)
    writeln!(file, "  \"id\" : \"{}\",", schema.id)?;

    // context (optional string)
    if let Some(context) = &schema.context {
        writeln!(file, "  \"context\" : \"{}\",", context)?;
    }

    // is_comment (boolean) - Changed to match Rust field name
    writeln!(file, "  \"is_comment\": {},", schema.is_comment)?;

    // titles (array of strings)
    writeln!(file, "  \"title\":[")?;
    for (i, title) in schema.titles.iter().enumerate() {
        if i < schema.titles.len() - 1 {
            writeln!(file, "    \"{}\",", escape_string(title))?;
        } else {
            writeln!(file, "    \"{}\"", escape_string(title))?;
        }
    }
    writeln!(file, "  ],")?;

    // unindexed_text (string)
    writeln!(file, "  \"unindexed\":\n    \"{}\",", escape_string(&schema.unindexed_text))?;

    // nodes_contained (array of strings)
    writeln!(file, "  \"content\": [")?;
    for (i, content_id) in schema.nodes_contained.iter().enumerate() {
        if i < schema.nodes_contained.len() - 1 {
            writeln!(file, "    \"{}\",", content_id)?;
        } else {
            writeln!(file, "    \"{}\"", content_id)?;
        }
    }
    writeln!(file, "  ],")?;

    // nodes_subscribed (array of strings)
    writeln!(file, "  \"subscriptions\": [")?;
    for (i, subscription_id) in schema.nodes_subscribed.iter().enumerate() {
        if i < schema.nodes_subscribed.len() - 1 {
            writeln!(file, "    \"{}\",", subscription_id)?;
        } else {
            writeln!(file, "    \"{}\"", subscription_id)?;
        }
    }
    writeln!(file, "  ]")?;

    // Closing brace
    write!(file, "}}")?;

    Ok(())
}

fn escape_string(s: &str) -> String {
    s.replace("\"", "\\\"")
}

fn main() -> io::Result<()> {
    // Create an example schema instance
    let schema = Schema {
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

    // Write the schema to a file
    write_schema_to_file(&schema, "TADA.skg")?;

    println!("Schema has been written to TADA.skg");

    Ok(())
}
