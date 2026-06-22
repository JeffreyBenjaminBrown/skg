/// One-time migration: normalize the body of every '.skg' file under a
/// folder so that "bodyless" is exactly 'body == None' (the invariant
/// the uniform-heralds surprising-links =bodyless= test relies on).
///
/// YAML-aware by construction: each file is round-tripped through
/// 'NodeFS' (the same serde type the server reads/writes), so a body
/// block scalar is never corrupted the way naive line-stripping could.
/// 'skg::types::nodes::complete::normalize_body' drops leading and
/// trailing whitespace-only lines and turns an all-whitespace body into
/// None (the field then vanishes from the YAML).
///
/// Usage: cargo run --bin trim-node-bodies -- <folder>
/// Driven by 'data/bash/trim-node-bodies.org'.

use skg::types::nodes::complete::normalize_body;
use skg::types::nodes::fs::NodeFS;

use std::fs;
use std::path::{Path, PathBuf};

fn main () {
  let args : Vec<String> = std::env::args () . collect ();
  let dir : &str = match args . get (1) {
    Some (d) => d,
    None => {
      eprintln! ( "usage: trim-node-bodies <folder>" );
      std::process::exit (1); } };
  let mut skg_files : Vec<PathBuf> = Vec::new ();
  collect_skg_files ( Path::new (dir), &mut skg_files );
  let mut changed : usize = 0;
  for path in &skg_files {
    match trim_one_file (path) {
      Ok (true)  => { changed += 1; }
      Ok (false) => {}
      Err (e)    => eprintln! ( "skipped {}: {}", path . display (), e ), } }
  println! ( "trim-node-bodies: scanned {} .skg files, rewrote {}.",
             skg_files . len (), changed ); }

fn collect_skg_files (
  dir : &Path,
  out : &mut Vec<PathBuf>,
) {
  let entries = match fs::read_dir (dir) {
    Ok (e) => e,
    Err (_) => return, };
  for entry in entries . flatten () {
    let path : PathBuf = entry . path ();
    if path . is_dir () {
      collect_skg_files (&path, out);
    } else if path . extension () . and_then (|e| e . to_str ())
              == Some ("skg") {
      out . push (path); } } }

/// Returns Ok(true) iff the file's body changed and was rewritten.
fn trim_one_file (
  path : &Path,
) -> Result<bool, Box<dyn std::error::Error>> {
  let contents : String = fs::read_to_string (path) ?;
  let mut node : NodeFS = serde_yaml::from_str (&contents) ?;
  let before : Option<String> = node . body . clone ();
  let after  : Option<String> = normalize_body (before . clone ());
  if before == after { return Ok (false); }
  node . body = after;
  fs::write ( path, node . to_yaml () ? ) ?;
  Ok (true) }
