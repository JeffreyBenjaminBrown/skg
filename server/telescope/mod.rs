pub mod dependencies_manifest;
pub mod compose;
pub mod invariants;
pub mod types;
pub mod decompose;

#[cfg(test)]
#[path = "../../tests/unit/telescope.rs"]
mod tests;
