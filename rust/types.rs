pub mod misc;
pub use misc::{ ID,
                SkgConfig,
                TantivyIndex,
                Hyperlink, };
pub mod errors;
pub use errors::{ HyperlinkParseError,
                  SaveError,
                  Buffer_Cannot_Be_Saved, };
pub mod orgnode;
pub use orgnode::{
  OrgNode, // metadata, title, body
    OrgnodeMetadata, // id, viewdata, code
      OrgnodeViewData, // cycle, focused, folded, repeat, relationships
        OrgnodeRelationships, // 2 bools, 3 ints
      OrgnodeCode, // relToParent, indef, toDelete, nodeReqs
        RelToParent, // content, for aliases, or ignored
        NodeRequest, // what's asked for: views, merge
};
pub use crate::serve::parse_headline_md_sexp::orgnodemd_to_string;
pub mod skgnode;
pub use skgnode::{ SkgNode,
                   skgnode_example,
                   empty_skgnode, };
pub mod save;
pub use save::{ NodeSaveAction,
                SaveInstruction,
                MergeInstructionTriple, };
