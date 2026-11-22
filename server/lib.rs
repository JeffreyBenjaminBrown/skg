//! The skg server. A user edits a graph of notes through views, Org
//! buffers in a client (Emacs or Neovim). The '.skg' files on disk are
//! the data; the server reads them into memory, draws views from them,
//! and turns a saved buffer back into file writes. Each module's header
//! says what it is for and lists its children; to find the code for a
//! behaviour, start below, then read that module's header.
//!
//! The server and its requests:
//! - 'serve': the TCP connection loop and one handler per request
//!   ('serve/handlers/'); start there to find what a request does.
//! - 'subcommands': the 'skg' binary's subcommands ('main.rs' runs them).
//!
//! Where the data lives, and its shapes:
//! - 'dbs': the '.skg' files, the in-Rust graph, the Tantivy index.
//! - 'types': the shared types (viewnodes, nodes, config, save plans).
//! - 'telescope': a node as one section per skgrepo, composed and split.
//! - 'skgrepo_sets': the skgrepo restriction, what a view may show.
//! - 'repo_watch': reloading '.skg' files that change on disk.
//!
//! Drawing views:
//! - 'refresh': bringing a view's tree into line with the graph.
//! - 'update_buffer': content views, drawn fresh or refreshed.
//! - 'rerender_pass': refreshing every open view a change touched.
//! - 'org_to_text': a view's tree as Org text.
//! - 'view_text_delta': which headlines of a redrawn view changed.
//! - 'heralds', 'herald_tokens', 'herald_descriptions': the marks
//!   drawn beside a headline, and what they mean.
//! - 'prominence': which nodes search ranks higher.
//!
//! Saving:
//! - 'from_text': a saved buffer to a validated plan.
//! - 'save': carrying the plan out.
//! - 'nodeMerge': merging two nodes.
//! - 'receipt': a read receipt.
//! - 'attribution': what a fork records of its original.
//! - 'delete_references_to_absent_node': removing links to an ID no
//!   node answers to.
//!
//! The diff view and git:
//! - 'diff_view': the diff view's buffers, built on the graph straddle.
//! - 'graph_straddle': the graph at HEAD beside the live graph.
//! - 'git_ops': reading, staging and committing with git.
//!
//! Import and export:
//! - 'import_md_and_org': Markdown and Org files, from a running server.
//! - 'import_org_roam': org-roam files, by a subcommand.
//! - 'export_org': plain Org files.
//! - 'obsidian': Obsidian vaults, both ways.
//!
//! Small and shared: 'view_snapshots' (what each view last held, kept
//! beside the logs), 'body_view_escape', 'sexp_string', 'consts',
//! 'util'; for tests only, 'test_utils' and 'metadata_canon'.

pub mod telescope;
pub mod attribution;
pub mod consts;
pub mod prominence;
pub mod receipt;
pub mod delete_references_to_absent_node;
pub mod dbs;
pub mod diff_view;
pub mod export_org;
pub mod obsidian;
pub mod from_text;
pub mod git_ops;
pub mod graph_straddle;
pub mod herald_descriptions;
pub mod herald_tokens;
pub mod heralds;
pub mod import_org_roam;
pub mod import_md_and_org;
pub(crate) mod body_view_escape;
pub mod metadata_canon;
pub mod nodeMerge;
pub mod org_to_text;
pub mod refresh;
pub mod repo_watch;
pub mod rerender_pass;
pub mod save;
pub mod serve;
pub mod sexp_string;
pub mod skgrepo_sets;
pub mod subcommands;
pub mod test_utils;
pub mod types;
pub mod update_buffer;
pub mod util;
pub mod view_snapshots;
pub mod view_text_delta;
