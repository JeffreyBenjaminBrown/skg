//! Printing a string as a quoted sexp atom, for responses the clients
//! read.
//!
//! Escaping the backslash and the double quote, and nothing else, is
//! what both readers accept. Emacs's 'read' decodes every escape;
//! Neovim's 'read_string' ('nvim/lua/skg/sexpr/parse.lua') decodes
//! '\n' and '\t' and turns any other escaped byte into itself. Both
//! read a literal newline inside the quotes as itself, so a newline
//! needs no escape, and an escape such as '\r' would reach Neovim as
//! 'r'.

/// TEXT as a quoted sexp atom: the backslash and the double quote
/// escaped, the quotes added.
pub fn quote_sexp_string (
  text : &str,
) -> String {
  let mut quoted : String =
    String::with_capacity ( text . len () + 2 );
  quoted . push ('"');
  for ch in text . chars () {
    match ch {
      '\\' => quoted . push_str ("\\\\"),
      '"'  => quoted . push_str ("\\\""),
      _    => quoted . push (ch), }}
  quoted . push ('"');
  quoted }

#[cfg(test)]
#[path = "../tests/unit/sexp_string.rs"]
mod tests;
