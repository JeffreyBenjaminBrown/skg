//! Trees and forests ('ego_tree'):
//! - 'forest': 'Forest' and 'ViewForest', a view's trees.
//! - 'generic': walking and mutating any tree.
//! - 'generations': the nodes at one depth.
//! - 'moves': moving a node within a tree, safely.
//! - 'viewnode_graphnode': questions about a tree of viewnodes.
//! - 'from_org_text': a plain tree from Org headlines, for tests.

pub mod viewnode_graphnode;
pub mod generic;
pub mod generations;
pub mod from_org_text;
pub mod forest;
pub mod moves;
