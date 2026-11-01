//! The server learns when a skgrepo's '.skg' files change on disk
//! (another writer, a 'git pull', an editor), and integrates them as
//! a new runtime generation. 'watcher' watches every configured
//! skgrepo directory and gathers events into batches; 'integrate'
//! turns a batch into a generation and a 'RepoChangeNotice';
//! 'own_writes' lets the watcher drop the events of the server's own
//! writes. Each connection delivers the notices
//! ('server/serve/handlers/repo_files_integrated.rs').

pub mod integrate;
pub mod own_writes;
pub mod watcher;
