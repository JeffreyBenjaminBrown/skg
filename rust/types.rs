// DON'T IMPORT these re-exports. They are for documentation.
// INSTEAD, imports in the codebase should use the original,
// longer definition path. That makes it easier to find definitions.

pub mod misc;
pub use misc::{ ID,
                SkgConfig,
                SkgfileSource,
                TantivyIndex,
                TextLink, };
pub mod errors;
pub use errors::{ TextLinkParseError,
                  SaveError,
                  BufferValidationError, };
pub mod orgnode;
pub use orgnode::{
  OrgNode, // metadata, title, body
    OrgnodeMetadata, // id, viewdata, code
      OrgnodeViewData, // cycle, focused, folded, repeat, relationships
        OrgnodeRelationships, // 2 bools, 3 ints
      OrgnodeCode, // relToParent, indef, editRequest, viewRequests
        RelToParent, // content | for aliases | ignored
        EditRequest, // merge | delete. Mutually excclusive.
        ViewRequest, // containerward | sourceward. Can request both.
};
pub use crate::serve::parse_headline_md_sexp::orgnodemd_to_string;
pub mod skgnode;
pub use skgnode::{ SkgNode,
                   skgnode_example,
                   empty_skgnode, };
pub mod save;
pub use save::{ NonMerge_NodeAction,
                SaveInstruction,
                MergeInstructionTriple, };
