// Binary grouping (TODO/faster-tests.org): multi-source, source-set,
// and TypeDB-layer tests. See tests/grouped_unit.rs for why test
// files are grouped into a few [[test]] targets.

#[path = "diff_mode_refusals.rs"]
mod diff_mode_refusals;

#[path = "move_source.rs"]
mod move_source;

#[path = "shared_db_session.rs"]
mod shared_db_session;

#[path = "source_sets.rs"]
mod source_sets;

#[path = "typedb.rs"]
mod typedb;
