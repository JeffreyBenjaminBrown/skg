// This code is tested by /tests/file_io.rs

use std::fs::{self};
use std::io::{self};
use std::path::{Path};
use serde_json;

use crate::types::SkgNode;

pub fn read_skgnode_from_path
    <P: AsRef<Path>>
    (file_path: P)
     -> io::Result<SkgNode>
{ let file_path = file_path.as_ref();
  let contents = fs::read_to_string(file_path)?;
  let mut skgnode: SkgNode = serde_json::from_str(&contents)
      .map_err(
	  |e| io::Error::new(
	      io::ErrorKind::InvalidData, e.to_string()))?;
  skgnode.path = file_path.to_path_buf(); // not part of the JSON
  Ok(skgnode) }

/// A line in the typedef of SkgNode prevents the field `path`
/// from being part of the JSON representation.
pub fn write_skgnode_to_path
    <P: AsRef<Path>>
    (skgnode: &SkgNode, file_path: P)
     -> io::Result<()>
{ let json_string = serde_json::to_string_pretty(skgnode)
      .map_err(
	  |e| io::Error::new(
	      io::ErrorKind::InvalidData, e.to_string()))?;
  fs::write(file_path, json_string)?;
  Ok(()) }
