//! Unit tests for the 'repos' endpoint ('repos_response';
//! 'docs/api-and-formats.org', "Repos"): every configured skgrepo in
//! privacy order, with its path as the config writes it, its
//! abbreviation when it has one, and whether it is owned; and the
//! connection's skgrepo restriction.

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
      skgrepo ("fred", "/data/fred/public", None, false),
      skgrepo ("shared", "/elsewhere/shared", Some ("sh"), true),
      skgrepo ("main", "/data/me/main", Some ("m"), true) ]));
  config . data_root = PathBuf::from ("/data");
  config . skgrepo_order = vec! [ SkgrepoName::from ("fred"),
                                  SkgrepoName::from ("shared"),
                                  SkgrepoName::from ("main") ];
  config }

#[test]
fn repos_request_lists_every_skgrepo_in_privacy_order () {
  let config : SkgConfig = config ();
  assert_eq! ( RequestType::from_client_string ("repos"),
               Ok (RequestType::Repos) );
  assert_eq! (
    repos_response ( &config, &SkgrepoRestriction::unrestricted (&config) ),
    concat! (
      r#"((response-type repos) (restriction "main") (repos ("#,
      r#"((name "fred") (path "fred/public") (owned "false")) "#,
      r#"((name "shared") (path "/elsewhere/shared") (abbreviation "sh") (owned "true")) "#,
      r#"((name "main") (path "me/main") (abbreviation "m") (owned "true"))"#,
      ")))" )); }

#[test]
fn repos_response_names_the_skgrepo_restriction () {
  let config : SkgConfig = config ();
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named ( &config, SkgrepoName::from ("shared") )
    . unwrap ();
  assert! ( repos_response (&config, &restriction)
            . starts_with (
              r#"((response-type repos) (restriction "shared") (repos ("# )); }
