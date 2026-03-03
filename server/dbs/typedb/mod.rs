pub mod nodes;
pub mod relationships;
pub mod search;
pub mod util;

/// TypeDB's default transaction timeout is too short for bulk init.
const TRANSACTION_TIMEOUT_SECS : u64 = 600;
