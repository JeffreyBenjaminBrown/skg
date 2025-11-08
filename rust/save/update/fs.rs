use crate::file_io::multiple_nodes::{write_all_nodes_to_fs, delete_all_nodes_from_fs};
use crate::types::misc::SkgConfig;
use crate::types::save::SaveInstruction;
use crate::types::skgnode::SkgNode;

use std::io;

pub fn update_fs_from_saveinstructions (
  instructions : Vec<SaveInstruction>,
  config       : SkgConfig,
) -> io::Result<(usize, usize)> { // (deleted, written)
  let ( to_delete, to_write ) // functional; no IO
    : ( Vec<SaveInstruction>, Vec<SaveInstruction> ) =
    instructions . into_iter ()
    . partition (|(_, action)|
                 action . toDelete );
  let delete_nodes : Vec<SkgNode> = // functional; no IO
    to_delete . into_iter ()
    . map ( |(node, _)| node )
    . collect ();
  let write_nodes : Vec<SkgNode> = // functional; no IO
    to_write . into_iter ()
    . map ( |(node, _)| node )
    . collect ();

  { // Modify the FS.
    let deleted : usize =
      if ! delete_nodes . is_empty () {
        delete_all_nodes_from_fs (
          delete_nodes, config . clone () ) ?
      } else { 0 };
    let written : usize =
      if ! write_nodes . is_empty () {
        write_all_nodes_to_fs (
          write_nodes, config ) ?
      } else { 0 };
    Ok ( (deleted, written) ) }}
