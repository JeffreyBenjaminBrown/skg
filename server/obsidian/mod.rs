//! Obsidian vaults: 'skg export-obsidian' writes one skgrepo as a
//! vault ('export'), in the dialect 'dialect' defines, and 'skg
//! import-obsidian' reads it back into .skg files ('import');
//! 'org_markup' finds the Org markup the export writes as Markdown;
//! 'read_text' reads a title or body back; 'vault_check' checks a
//! vault against Obsidian's rules. The plan is
//! 'TODO/export-obsidian/plan.org'.

pub mod dialect;
pub mod export;
pub mod import;
pub mod org_markup;
pub mod read_text;
pub mod vault_check;
