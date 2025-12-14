pub mod aliascol;
pub mod contents;

pub use aliascol::completeAliasCol;

pub use contents::{
  completeDefinitiveOrgnode,
  clobberIndefinitiveOrgnode,
  make_indefinitive_if_repeated,
  completeAndRestoreForest_collectingDefinitiveRequests,
};
