// Binary grouping (TODO/faster-tests.org): every tests/*.rs file
// used to be its own ~quarter-GB test binary; after any change to
// server/ code, relinking all of them dominated the test cycle.
// Cargo.toml sets autotests = false and declares a few [[test]]
// targets; this one gathers the db-free unit-style files.
//
// PITFALL: graphnodestats_hiding belongs here and NOT in a group
// with global-handle-installing tests: its TypeDB-path test relies
// on the process-global in-Rust graph handle being uninstalled,
// which under plain 'cargo test' only holds if no other test in the
// same binary installs it. (Under nextest every test is its own
// process, so this only matters for plain 'cargo test'.)

#[path = "body_yaml_format.rs"]
mod body_yaml_format;

#[path = "contexts.rs"]
mod contexts;

#[path = "dbs.rs"]
mod dbs;

#[path = "diff_analysis.rs"]
mod diff_analysis;

#[path = "git_ops.rs"]
mod git_ops;

#[path = "graphnodestats_hiding.rs"]
mod graphnodestats_hiding;

#[path = "import_org_roam.rs"]
mod import_org_roam;

#[path = "init_refusal.rs"]
mod init_refusal;

#[path = "multi_source_errors.rs"]
mod multi_source_errors;

#[path = "multi_source_loading.rs"]
mod multi_source_loading;

#[path = "render_util.rs"]
mod render_util;

#[path = "serve_text_search_test.rs"]
mod serve_text_search_test;

#[path = "source_inheritance_for_non_content.rs"]
mod source_inheritance_for_non_content;

#[path = "source_path_validation.rs"]
mod source_path_validation;

#[path = "subscribee_col.rs"]
mod subscribee_col;

#[path = "tantivy.rs"]
mod tantivy;

#[path = "test_utils.rs"]
mod test_utils;

#[path = "textlinks.rs"]
mod textlinks;

#[path = "titles_by_ids.rs"]
mod titles_by_ids;

#[path = "to_org.rs"]
mod to_org;

#[path = "types.rs"]
mod types;
