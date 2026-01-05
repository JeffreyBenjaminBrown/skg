// DON'T IMPORT these re-exports. They are for documentation.
// INSTEAD, imports in the codebase should use the original,
// longer definition path. That makes it easier to find definitions.

pub mod sexp;
pub mod textlinks;
pub use textlinks::TextLink;
pub mod misc;
pub use misc::{ ID,
                SourceNickname,
                SkgConfig,
                SkgfileSource,
                TantivyIndex, };
pub mod errors;
pub use errors::{ TextLinkParseError,
                  SaveError,
                  BufferValidationError, };
pub mod orgnode;
pub use orgnode::{
  OrgNode, // metadata, title, opt body
    OrgnodeMetadata, // opt id, opt source, viewdata, code
      OrgnodeViewData, // cycle, focused, folded, relationships
        OrgnodeRelationships, // 2 bools, 3 opt ints
      OrgnodeCode, // interp, indef, opt editRequest, viewRequests
        Interp, // content | for aliases | ignored
        EditRequest, // merge | delete
                     //   Mutually excclusive.
        ViewRequest, // aliases | containerward | sourceward
                     //   *Not* mutually exclusive.
  orgnodemd_to_string,
};
pub mod skgnode;
pub use skgnode::{ SkgNode,
                   skgnode_example,
                   empty_skgnode, };
pub mod save;
pub use save::{ NonMerge_NodeAction,
                SaveInstruction,
                MergeInstructionTriple, };
pub mod tree;
pub use tree::PairTree;
