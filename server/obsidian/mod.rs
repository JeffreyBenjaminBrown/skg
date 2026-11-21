//! Obsidian vaults: 'skg export-obsidian' writes one skgrepo as a
//! vault ('export'), in the dialect 'dialect' defines, and 'skg
//! import-obsidian' reads it back into .skg files ('import');
//! 'read_text' reads a title or body back; 'vault_check' checks a
//! vault against Obsidian's rules. The plan is
//! 'TODO/export-obsidian/plan.org'.

pub mod dialect;
pub mod export;
pub mod import;
pub mod read_text;
pub mod vault_check;
