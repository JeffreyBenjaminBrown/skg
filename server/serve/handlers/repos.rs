//! PURPOSE: Report the configured skgrepos, in privacy order, and the
//! connection's skgrepo restriction. A client that needs to know which
//! skgrepos are owned, or what they are called, asks this instead of
//! inferring it from nodes.
//!
//! Response: ((response-type repos) (restriction "NAME")
//!            (repos ((name "NAME") (path "PATH") [(abbreviation "ABBR")]
//!                    (owned "true"|"false")) ...))

use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_sexp_response};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{SkgConfig, Skgrepo};

use std::net::TcpStream;
use std::path::Path;

pub fn handle_repos_request (
  stream              : &mut TcpStream,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  send_response_with_length_prefix (
    stream,
    & repos_response (config, skgrepo_restriction) ); }

pub fn repos_response (
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> String {
  let entries : Vec<String> =
    // Privacy order, most public first. Order is meaningful; do not sort.
    config . ordered_skgrepos () . iter ()
    . filter_map ( |name| config . skgrepos . get (name) )
    . map ( |skgrepo| skgrepo_entry_sexp (skgrepo, &config . data_root) )
    . collect ();
  tag_sexp_response (
    TcpToClient::Repos,
    & format! ( "((restriction {}) (repos ({})))",
                quote_sexp_string (
                  &skgrepo_restriction . most_private_shown . 0 ),
                entries . join (" ") )) }

fn skgrepo_entry_sexp (
  skgrepo   : &Skgrepo,
  data_root : &Path,
) -> String {
  let abbreviation : String =
    match &skgrepo . abbreviation {
      Some (abbreviation) =>
        format! ( " (abbreviation {})", quote_sexp_string (abbreviation) ),
      None => String::new (), };
  format! ( "((name {}) (path {}){} (owned {}))",
            quote_sexp_string (&skgrepo . name . 0),
            quote_sexp_string (
              &path_as_the_config_writes_it (&skgrepo . path, data_root) ),
            abbreviation,
            quote_sexp_string (
              if skgrepo . owned { "true" } else { "false" } )) }

/// The config loader resolves a relative skgrepo path against the
/// data root ('make_paths_absolute'). This undoes that: a path below
/// the data root is given relative to it, any other absolutely.
fn path_as_the_config_writes_it (
  path      : &Path,
  data_root : &Path,
) -> String {
  path . strip_prefix (data_root)
    . unwrap_or (path)
    . to_string_lossy ()
    . into_owned () }

#[cfg(test)]
#[path = "../../../tests/unit/repos.rs"]
mod tests;
