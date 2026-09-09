//! Process-owned observation and maintenance state.
//!
//! The pure coordinator is deliberately independent of TCP, editor APIs and
//! databases. Socket handlers translate messages into its typed transitions;
//! durable journals serialize the same state before irreversible boundaries.

pub mod coordinator;
pub mod archive;
pub mod candidate;
pub mod evidence;
pub mod journal;
pub mod observation;
pub mod pull;
pub mod query_waits;
pub mod query_artifacts;
pub(crate) mod save_journal;
pub mod selection;
pub mod types;
pub mod view_impact;

pub use coordinator::MaintenanceCoordinator;
pub use types::*;
