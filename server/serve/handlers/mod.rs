//! One file per request type, each with a 'handle_*_request' that
//! 'serve.rs' ('dispatch_request') calls; the file's header says what
//! the request does. Views: 'single_root_view', 'text_search' (and
//! 'text_search/'), 'close_view', 'rerender_views', 'rerender_all_views',
//! 'perspective', 'titles_by_gnids'. Saving: 'save_buffer', with its
//! helpers 'save_staleness' (stale graphnodes), 'save_dependencies'
//! (what a save touches, and the dirty views it conflicts with),
//! 'telescope_hoist' and 'text_release' (text from below a node's
//! home). The diff view: 'diff_view', 'extend_diff_view',
//! 'stage_and_commit'. Asking about nodes: 'link_statuses',
//! 'flag_state', 'relRepo_info', 'get_file_path',
//! 'investigate_unknown_node', 'describe_heralds'. Skgrepos and the
//! server: 'repos', 'skgrepo_sets', 'herald_rules', 'server_warnings',
//! 'rebuild_session_lifetime_stores'. Bulk changes:
//! 'delete_references_to_absent_node', 'strip_body_whitespace',
//! 'receipt', 'export_to_org', 'import_md_and_org'. Unrequested:
//! 'repo_files_reloaded', what the repo watcher reloaded.

pub mod close_view;
pub mod delete_references_to_absent_node;
pub mod describe_heralds;
pub mod relRepo_info;
pub mod flag_state;
pub mod export_to_org;
pub mod import_md_and_org;
pub mod get_file_path;
pub mod herald_rules;
pub mod investigate_unknown_node;
pub mod rebuild_session_lifetime_stores;
pub mod repo_files_reloaded;
pub mod repos;
pub mod rerender_all_views;
pub mod rerender_views;
pub mod save_buffer;
pub mod server_warnings;
pub mod extend_diff_view;
pub(crate) mod save_dependencies;
pub mod save_staleness;
pub mod text_release;
pub mod single_root_view;
pub mod skgrepo_sets;
pub mod stage_and_commit;
pub mod strip_body_whitespace;
pub mod telescope_hoist;
pub mod text_search;
pub mod titles_by_gnids;
pub mod diff_view;
pub mod link_statuses;
pub mod perspective;
pub mod receipt;
