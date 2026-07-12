pub mod dependencies_manifest;
pub mod fold;
pub mod invariants;
pub mod types;
pub mod unfold;

#[cfg(test)]
#[path = "../../tests/unit/accordion.rs"]
mod tests;
