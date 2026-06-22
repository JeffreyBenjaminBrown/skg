//! NodeFS: the on-disk shape of a node.
//!
//! What the YAML on disk literally holds. Bit-exact with today's
//! NodeComplete on-disk serialization — does NOT include 'source'
//! (which is inferred from file location, not stored in YAML).
//!
//! Conversions:
//! - 'NodeFS::into_complete(source)' produces a NodeComplete.
//! - 'From<&NodeComplete> for NodeFS' drops source for writing.

use serde::{Serialize, Deserialize};

use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct NodeFS {
  // Field order and serde attributes mirror 'NodeComplete' exactly, so YAML output is bit-exact. When anything here changes, update the matching field in 'NodeComplete' (see [[./complete.rs][server/types/nodes/complete.rs]]) and vice versa.

  pub title: String,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub aliases: MSV<String>,

  pub pid: ID,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub extra_ids: Vec<ID>,

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ID>,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub subscribes_to: MSV<ID>,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub hides_from_its_subscriptions: MSV<ID>,

  #[serde(default, skip_serializing_if = "MSV::skip_serializing")]
  pub overrides_view_of: MSV<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub misc: Vec<FileProperty>,
}

impl NodeFS {
  /// Attach a source to produce a NodeComplete.
  pub fn into_complete (
    self,
    source : SourceName,
  ) -> NodeComplete {
    NodeComplete {
      title                        : self . title,
      aliases                      : self . aliases,
      source,
      pid                          : self . pid,
      extra_ids                    : self . extra_ids,
      body                         : self . body,
      contains                     : self . contains,
      subscribes_to                : self . subscribes_to,
      hides_from_its_subscriptions : self . hides_from_its_subscriptions,
      overrides_view_of            : self . overrides_view_of,
      misc                         : self . misc,
    }
  }

  /// Serialize to YAML, emitting the 'body' field as a block literal
  /// (`body: |2-` / `body: |2`) so multi-line bodies are readable in
  /// version control rather than collapsed to a single-line
  /// double-quoted string with `\n` escapes.
  ///
  /// Field order matches the struct declaration, matching what a
  /// plain `serde_yaml::to_string(self)` would produce. Reads
  /// (`serde_yaml::from_str`) already accept every scalar style, so
  /// existing .skg files need no migration.
  ///
  /// Falls back to the default `serde_yaml` rendering for body strings
  /// that cannot be losslessly represented in block form (e.g. ones
  /// containing carriage returns or NUL bytes).
  pub fn to_yaml (
    &self,
  ) -> Result<String, serde_yaml::Error> {
    let value : serde_yaml::Value =
      serde_yaml::to_value (self)?;
    let mapping : &serde_yaml::Mapping =
      value . as_mapping ()
      . expect ("NodeFS serializes as a YAML mapping");
    let mut out : String = String::new();
    for (key, val) in mapping . iter () {
      let is_body : bool =
        key . as_str() == Some ("body");
      let block : Option<String> =
        if is_body {
          val . as_str()
            . and_then (block_scalar_for_body) }
        else { None };
      match block {
        Some (rendered) => out . push_str (&rendered),
        None => {
          // Singleton mapping, so 'serde_yaml::to_string'
          // renders just this one key-value pair.
          let singleton : serde_yaml::Mapping =
            [(key . clone (), val . clone ())]
            . into_iter () . collect ();
          out . push_str (
            & serde_yaml::to_string (&singleton)?); } } }
    Ok (out) }
}

/// Render BODY as a YAML block-literal key-value of the form
/// `body: |2…\n  line\n  line\n`. Returns Some only when the result
/// round-trips exactly through `serde_yaml::from_str`; otherwise None,
/// signalling the caller to fall back to the default rendering.
fn block_scalar_for_body (
  body : &str,
) -> Option<String> {
  let ends_nl : bool = body . ends_with ('\n');
  let inner : &str =
    // Block form's final newline is implied by `|2`; `|2-` strips it.
    if ends_nl { & body [.. body . len() - 1] }
    else       { body };
  let chomp : &str = if ends_nl { "" } else { "-" };
  // Indentation indicator first, then chomping indicator — matches
  // serde_yaml's own emitter style (e.g. `|2-`).
  let mut out : String =
    format! ("body: |2{}\n", chomp);
  for line in inner . split ('\n') {
    if line . is_empty () { out . push ('\n'); }
    else {
      out . push_str ("  ");
      out . push_str (line);
      out . push ('\n'); } }
  // Round-trip guard: parse what we emitted and accept only if the
  // decoded body is bit-for-bit identical to the input.
  let parsed : Result<serde_yaml::Value, _> =
    serde_yaml::from_str (&out);
  match parsed {
    Ok (v) => {
      let got : Option<&str> =
        v . get ("body") . and_then (|b| b . as_str ());
      if got == Some (body) { Some (out) }
      else { None } }
    Err (_) => None, } }

impl From<&NodeComplete> for NodeFS {
  /// Drop 'source'. Everything else is cloned.
  fn from (c: &NodeComplete) -> Self {
    NodeFS {
      title                        : c . title . clone (),
      aliases                      : c . aliases . clone (),
      pid                          : c . pid . clone (),
      extra_ids                    : c . extra_ids . clone (),
      body                         :
        crate::types::nodes::complete::normalize_body (
          c . body . clone () ),
      contains                     : c . contains . clone (),
      subscribes_to                : c . subscribes_to . clone (),
      hides_from_its_subscriptions : c . hides_from_its_subscriptions . clone (),
      overrides_view_of            : c . overrides_view_of . clone (),
      misc                         : c . misc . clone (),
    }
  }
}
