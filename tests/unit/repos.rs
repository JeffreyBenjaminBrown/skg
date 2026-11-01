//! Unit tests for the 'repos' endpoint ('repos_response';
//! 'docs/api-and-formats.org', "Repos"): every configured skgrepo in
//! fold order, with its path as the config writes it, its
//! abbreviation when it has one, whether it is owned, and whether the
//! connection's skgrepo restriction shows it; and that restriction.

use super::repos_response;
use crate::serve::protocol::RequestType;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{SkgConfig, Skgrepo, SkgrepoName};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  let skgrepo = |name : &str, path : &str, abbreviation : Option<&str>,
                 owned : bool| (
    SkgrepoName::from (name),
    Skgrepo { name : SkgrepoName::from (name),
              abbreviation : abbreviation . map (String::from),
              path : PathBuf::from (path),
              owned } );
  let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (
    HashMap::from ([
      skgrepo ("shared", "/elsewhere/shared", Some ("sh"), true),
      skgrepo ("fred", "/data/fred/public", None, false),
      skgrepo ("main", "/data/me/main", Some ("m"), true) ]));
  config . data_root = PathBuf::from ("/data");
  config . skgrepo_order = vec! [ SkgrepoName::from ("shared"),
                                  SkgrepoName::from ("fred"),
                                  SkgrepoName::from ("main") ];
  config }

#[test]
fn repos_request_lists_every_skgrepo_in_config_order () {
  let config : SkgConfig = config ();
  assert_eq! ( RequestType::from_client_string ("repos"),
               Ok (RequestType::Repos) );
  assert_eq! (
    repos_response ( &config, &SkgrepoRestriction::unrestricted (&config) ),
    concat! (
      r#"((response-type repos) (restriction "main") (foreign-repos "shown") (repos ("#,
      r#"((name "shared") (path "/elsewhere/shared") (abbreviation "sh") (owned "true") (shown "true")) "#,
      r#"((name "fred") (path "fred/public") (owned "false") (shown "true")) "#,
      r#"((name "main") (path "me/main") (abbreviation "m") (owned "true") (shown "true"))"#,
      ")))" )); }

#[test]
fn repos_response_names_the_skgrepo_restriction () {
  let config : SkgConfig = config ();
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named ( &config, SkgrepoName::from ("shared") )
    . unwrap ();
  assert_eq! (
    repos_response (&config, &restriction),
    concat! (
      r#"((response-type repos) (restriction "shared") (foreign-repos "shown") (repos ("#,
      r#"((name "shared") (path "/elsewhere/shared") (abbreviation "sh") (owned "true") (shown "true")) "#,
      r#"((name "fred") (path "fred/public") (owned "false") (shown "true")) "#,
      r#"((name "main") (path "me/main") (abbreviation "m") (owned "true") (shown "false"))"#,
      ")))" )); }

#[test]
fn repos_response_says_when_foreign_repos_are_hidden () {
  let config : SkgConfig = config ();
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named_with_foreign_shown (
      &config, SkgrepoName::from ("main"), false )
    . unwrap ();
  assert_eq! (
    repos_response (&config, &restriction),
    concat! (
      r#"((response-type repos) (restriction "main") (foreign-repos "hidden") (repos ("#,
      r#"((name "shared") (path "/elsewhere/shared") (abbreviation "sh") (owned "true") (shown "true")) "#,
      r#"((name "fred") (path "fred/public") (owned "false") (shown "false")) "#,
      r#"((name "main") (path "me/main") (abbreviation "m") (owned "true") (shown "true"))"#,
      ")))" )); }
