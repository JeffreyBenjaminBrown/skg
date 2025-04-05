// This code is tested by /tests/file_io.rs

use std::fs::File;
use std::io::{self, Write, Read};
use serde_json;

use crate::types::SkgNode;

pub fn read_skgnode_from_path(file_path: &str) -> io::Result<SkgNode> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let skgnode: SkgNode = serde_json::from_str(&contents)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
    Ok(skgnode) }

pub fn write_skgnode_to_path(
    skgnode: &SkgNode, file_path: &str)
    -> io::Result<()> {
    let mut file = File::create(file_path)?;

    writeln!(file, "{{\"format\" : \"{}\",",
             skgnode.format)?;
    writeln!(file, "\"id\" : \"{}\",",
             skgnode.id)?;
    if let Some(context) = &skgnode.context {
        writeln!(file, "\"context\" : \"{}\",", context)?;
    }
    writeln!(file, "\"is_comment\": {},", skgnode.is_comment)?;
    writeln!(file, "\"titles\":[")?;
    for (i, title) in skgnode.titles.iter().enumerate() {
        if i < skgnode.titles.len() - 1 {
            writeln!(file, "  \"{}\",",
		     escape_quotes_in_string(title))?;
        } else {
            writeln!(file, "  \"{}\"",
		     escape_quotes_in_string(title))?; } }
    writeln!(file, "],\"unindexed_text\":\n  \"{}\",",
             escape_quotes_in_string(&skgnode.unindexed_text))?;
    writeln!(file, "\"nodes_contained\":[")?;
    for (i, content_id) in
        skgnode.nodes_contained.iter().enumerate() {
        if i < skgnode.nodes_contained.len() - 1 {
            writeln!(file, "  \"{}\",", content_id)?;
        } else {
            writeln!(file, "  \"{}\"", content_id)?;
        }
    }
    writeln!(file, "],\"nodes_subscribed\":[")?;
    for (i, subscription_id)
        in skgnode.nodes_subscribed.iter().enumerate() {
        if i < skgnode.nodes_subscribed.len() - 1 {
            writeln!(file, "  \"{}\",", subscription_id)?;
        } else {
            writeln!(file, "  \"{}\"]}}", subscription_id)?;
        }
    }
    Ok(()) }

pub fn escape_quotes_in_string(s: &str) -> String {
    s.replace("\"", "\\\"") }
