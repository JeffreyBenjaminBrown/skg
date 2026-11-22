//! Git, through the git2 crate: reading the owned skgrepos at HEAD for
//! the diff view, staging and committing chosen nodes, and searching
//! history for an ID no node answers to.
//! - 'git_snapshot': the graph at HEAD (the graph straddle's
//!   before-graph), and the graph a commit would leave.
//! - 'git_snapshot_types': the snapshot's types.
//! - 'diff': the nodes a skgrepo's changed files add or delete.
//! - 'read_gitrepo': opening a gitrepo, its changed files, a file's
//!   content at HEAD or in the index, and its remote.
//! - 'stage_and_commit': the commit of the "stage and commit" request.
//! - 'vanished': what an unknown ID used to be.
//! - 'misc': path and diff-entry conversions.

pub mod misc;
pub mod read_gitrepo;
pub mod diff;
pub mod stage_and_commit;
pub mod git_snapshot;
pub mod git_snapshot_types;
pub mod vanished;
