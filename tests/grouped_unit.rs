//! One of the five test binaries. Cargo.toml sets 'autotests = false'
//! and declares five '[[test]]' targets, each gathering many 'tests/'
//! files as modules, so that a change to 'server/' relinks five
//! binaries rather than one per file. This one gathers the unit-style
//! files, each about one module, format or request:
//! - formats and parsing: body_yaml_format, metadata_grammar,
//!   metadata_fields_table, change_facts_metadata, links, types;
//! - loading and config: dbs, init_refusal, multi_skgrepo_loading,
//!   multi_skgrepo_errors, skgrepo_path_validation,
//!   skgrepo_inheritance_for_non_content, runtime_generation,
//!   server_warnings, subcommand_usage, import_org_roam;
//! - search and Tantivy: tantivy, serve_text_search_test, prominence,
//!   titles_by_gnids;
//! - drawing: render_util, herald_tokens, graphnodestats_hiding,
//!   forks_folder_by_default;
//! - git: git_ops, investigate_unknown_node;
//! - the helpers themselves: test_utils.
//! The other binaries: 'grouped_views.rs', 'grouped_forks.rs',
//! 'grouped_saves.rs', 'grouped_repos.rs'. The library's own unit tests
//! are in 'tests/unit/', each included by its module in 'server/'.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;

#[path = "body_yaml_format.rs"]
mod body_yaml_format;

#[path = "change_facts_metadata.rs"]
mod change_facts_metadata;

#[path = "dbs.rs"]
mod dbs;

#[path = "git_ops.rs"]
mod git_ops;

#[path = "graphnodestats_hiding.rs"]
mod graphnodestats_hiding;

#[path = "herald_tokens.rs"]
mod herald_tokens;

#[path = "import_org_roam.rs"]
mod import_org_roam;

#[path = "investigate_unknown_node.rs"]
mod investigate_unknown_node;

#[path = "init_refusal.rs"]
mod init_refusal;

#[path = "metadata_fields_table.rs"]
mod metadata_fields_table;

#[path = "metadata_grammar.rs"]
mod metadata_grammar;

#[path = "multi_repo_errors.rs"]
mod multi_skgrepo_errors;

#[path = "multi_repo_loading.rs"]
mod multi_skgrepo_loading;

#[path = "prominence.rs"]
mod prominence;

#[path = "render_util.rs"]
mod render_util;

#[path = "runtime_generation.rs"]
mod runtime_generation;

#[path = "serve_text_search_test.rs"]
mod serve_text_search_test;

#[path = "repo_inheritance_for_non_content.rs"]
mod skgrepo_inheritance_for_non_content;

#[path = "repo_path_validation.rs"]
mod skgrepo_path_validation;

#[path = "server_warnings.rs"]
mod server_warnings;

#[path = "subcommand_usage.rs"]
mod subcommand_usage;

#[path = "forks_folder_by_default.rs"]
mod forks_folder_by_default;

#[path = "tantivy.rs"]
mod tantivy;

#[path = "test_utils.rs"]
mod test_utils;

#[path = "links.rs"]
mod links;

#[path = "titles_by_ids.rs"]
mod titles_by_gnids;

#[path = "types.rs"]
mod types;
