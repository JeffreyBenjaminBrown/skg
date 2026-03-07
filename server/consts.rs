/// Tantivy returns at most this many raw results per search.
/// Context-based reranking can reorder results arbitrarily,
/// so this must be large enough to capture every match.
/// In practice, typical queries return tens to low hundreds of matches,
/// so this limit is rarely hit.
pub const TANTIVY_SEARCH_LIMIT : usize = 100_000;

/// How many unique-ID search results to show the user.
pub const SEARCH_DISPLAY_LIMIT : usize = 10;

/// Tantivy IndexWriter heap buffer size in bytes.
pub const TANTIVY_WRITER_BUFFER_BYTES : usize = 50_000_000;

/// Max documents to retrieve when looking up a single ID
/// in Tantivy (e.g. during context type updates).
pub const TANTIVY_PER_ID_LOOKUP_LIMIT : usize = 100;

/// TypeDB transaction timeout in seconds.
/// The default is too short for bulk init.
pub const TYPEDB_TRANSACTION_TIMEOUT_SECS : u64 = 600;

/// TypeDB server address.
pub const TYPEDB_ADDRESS : &str = "127.0.0.1:1729";

/// Default TCP port for Rust-Emacs communication.
pub const DEFAULT_PORT : u16 = 1730;

/// Max nodes to render in initial content views.
pub const DEFAULT_INITIAL_NODE_LIMIT : usize = 1000;

/// Milliseconds to wait before deleting TypeDB database on shutdown,
/// allowing pending operations to complete.
pub const SHUTDOWN_DB_DELETE_DELAY_MS : u64 = 100;

/// Milliseconds read timeout for busysignal connections during init.
pub const BUSYSIGNAL_READ_TIMEOUT_MS : u64 = 500;

/// Milliseconds sleep between busysignal accept attempts.
pub const BUSYSIGNAL_POLL_INTERVAL_MS : u64 = 100;

/// Context-based search score multipliers by origin type.
/// READONING:
/// These are in approximate logarithmic order.
/// (3.2 is approximately the square root of 10.)
pub const MULTIPLIER_ROOT             : f32 = 100.0;
pub const MULTIPLIER_CYCLE_MEMBER     : f32 =  32.0;
pub const MULTIPLIER_TARGET           : f32 =  10.0;
pub const MULTIPLIER_HAD_ID           : f32 =  10.0;
pub const MULTIPLIER_MULTI_CONTAINED  : f32 =   3.2;
