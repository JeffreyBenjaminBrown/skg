//! Emit etags TAGS entries for Rust struct fields and enum variants.
//!
//! This fills a gap left by line-based regexes in `bash/etags-rebuild.sh`:
//! a regex can't tell a struct field (`name: Type` inside `struct { ... }`)
//! from a function argument (`name: Type` inside `fn f(...)`). Here we
//! parse each file with `syn` and walk the AST, so only real fields and
//! variants get emitted.
//!
//! Output goes to stdout in etags TAGS format. Usage:
//!   rust-field-tags FILE.rs [FILE2.rs ...] > extra-tags.partial
//! Then append that to the main TAGS file.

use std::env;
use std::fs;
use std::path::PathBuf;

use syn::visit::Visit;
use syn::{parse_file, Fields, ItemEnum, ItemStruct};

struct Tagger <'a> {
  source       : &'a str,
  line_offsets : Vec<usize>,  // byte offset of the start of each 1-indexed line
  out          : String,
  byte_count   : usize,       // total bytes written to `out`
}

impl <'a> Tagger <'a> {
  fn emit (
    &mut self,
    line : usize,
    name : &str,
  ) {
    // Line-text shown in the TAGS entry: the line up to and including
    // the tag's identifier. Emacs uses this to relocate the tag.
    let line_start : usize = self . line_offsets [line - 1];
    let line_end   : usize =
      self . line_offsets . get (line)
        . copied ()
        . unwrap_or (self . source . len () + 1) - 1;
    // Clamp line_end in case the last line has no trailing newline.
    let line_end : usize =
      line_end . min (self . source . len ());
    let line_text : &str =
      & self . source [line_start .. line_end];
    let tag_end : usize =
      line_text . find (name)
        . map (|p| p + name . len ())
        . unwrap_or (line_text . len ());
    let tag_text : &str =
      & line_text [.. tag_end];
    // Explicit-name format: TEXT\x7fNAME\x01LINE,OFFSET\n
    let entry : String =
      format! ( "{}\x7f{}\x01{},{}\n",
                tag_text, name, line, line_start );
    self . byte_count += entry . len ();
    self . out . push_str (&entry); }

  fn handle_struct (
    &mut self,
    s : &ItemStruct,
  ) {
    if let Fields::Named (ref fs) = s . fields {
      for f in & fs . named {
        if let Some (ident) = & f . ident {
          let line : usize = ident . span () . start () . line;
          self . emit (line, & ident . to_string ()); }} } }

  fn handle_enum (
    &mut self,
    e : &ItemEnum,
  ) {
    for v in & e . variants {
      let line : usize =
        v . ident . span () . start () . line;
      self . emit (line, & v . ident . to_string ());
      if let Fields::Named (ref fs) = v . fields {
        for f in & fs . named {
          if let Some (ident) = & f . ident {
            let fline : usize =
              ident . span () . start () . line;
            self . emit (fline, & ident . to_string ()); }} } } }
}

impl <'ast, 'a> Visit <'ast> for Tagger <'a> {
  fn visit_item_struct (
    &mut self,
    s : &'ast ItemStruct,
  ) {
    self . handle_struct (s);
    syn::visit::visit_item_struct (self, s); }

  fn visit_item_enum (
    &mut self,
    e : &'ast ItemEnum,
  ) {
    self . handle_enum (e);
    syn::visit::visit_item_enum (self, e); }
}

fn compute_line_offsets (
  src : &str,
) -> Vec<usize> {
  let mut offs : Vec<usize> = vec! [0];
  for (i, b) in src . bytes () . enumerate () {
    if b == b'\n' {
      offs . push (i + 1); } }
  offs }

fn process_file (
  path : &str,
) -> Option<String> {
  let src : String = fs::read_to_string (path) . ok ()?;
  let file : syn::File = parse_file (&src) . ok ()?;
  let mut tagger : Tagger =
    Tagger { source       : &src,
             line_offsets : compute_line_offsets (&src),
             out          : String::new (),
             byte_count   : 0, };
  tagger . visit_file (&file);
  if tagger . out . is_empty () { return None; }
  // etags file section: \x0c\nPATH,BYTES\n<entries>
  Some ( format! ( "\x0c\n{},{}\n{}",
                   path, tagger . byte_count, tagger . out )) }

fn main () {
  let args : Vec<String> = env::args () . skip (1) . collect ();
  let mut total : String = String::new ();
  for raw in &args {
    let path : PathBuf = PathBuf::from (raw);
    let display : &str = raw . as_str ();
    if let Some (section) = process_file (display) {
      total . push_str (&section);
    } else {
      // Silently skip unreadable / unparseable files; they would have
      // fallen through the regex approach too.
      let _ = path;
    } }
  print! ("{}", total); }
