/// Tantivy returns at most this many raw results per search.
/// Context-based reranking can reorder results arbitrarily,
/// so this must be large enough to capture every match.
/// In practice, typical queries return tens to low hundreds of matches,
/// so this limit is rarely hit.
pub const TANTIVY_SEARCH_LIMIT : usize = 100_000;

/// How many unique-ID search results to show the user.
pub const SEARCH_DISPLAY_LIMIT : usize = 1000;

/// Tantivy IndexWriter heap buffer size in bytes.
pub const TANTIVY_WRITER_BUFFER_BYTES : usize = 50_000_000;

/// Max documents to retrieve when looking up a single ID
/// in Tantivy (e.g. during context type updates).
pub const TANTIVY_PER_ID_LOOKUP_LIMIT : usize = 100;

/// TypeDB transaction timeout in seconds.
/// The default is too short for bulk init.
pub const TYPEDB_TRANSACTION_TIMEOUT_SECS : u64 = 600;

/// Default max concurrent TypeDB transactions during bulk operations.
/// (Massively improves performance --
/// initial relationship creation fell from > 10 min to ~20 sec.)
pub const DEFAULT_TYPEDB_CONCURRENT_TRANSACTIONS : usize = 64;

/// Runtime TypeDB transaction fanout.
/// Tests can lower this with 'SKG_TYPEDB_CONCURRENT_TRANSACTIONS'
/// so the full suite does not overwhelm an interactive machine.
pub fn typedb_concurrent_transactions () -> usize {
  std::env::var ("SKG_TYPEDB_CONCURRENT_TRANSACTIONS")
    . ok ()
    . and_then ( |s| s . parse::<usize> () . ok () )
    . filter ( |n| *n > 0 )
    . unwrap_or (DEFAULT_TYPEDB_CONCURRENT_TRANSACTIONS) }

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

/// Exponent applied to (matched_query_terms / total_query_terms)
/// when computing the per-hit coverage multiplier for search.
/// Bigger -> missing terms hurt more.
/// k=2 (current): 4/5 of terms matched -> 0.64x, 1/5 -> 0.04x.
/// Applied in literal and regex search modes; skipped in
/// operators mode (where MUST/MUSTNOT semantics make "fraction
/// matched" ill-defined).
pub const SEARCH_COVERAGE_EXPONENT    : f32 =   2.0;
