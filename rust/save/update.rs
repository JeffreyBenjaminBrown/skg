pub mod fs;
pub use fs::update_fs_from_saveinstructions;

pub mod tantivy;
pub use tantivy::update_index_from_saveinstructions;

pub mod typedb;
pub use typedb::update_typedb_from_saveinstructions;
