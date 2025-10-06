use crate::types::{SaveInstruction, SkgNode, SkgConfig};
use crate::file_io::{delete_all_nodes_from_fs, write_all_nodes_to_fs};

use std::io;


pub fn update_fs_from_saveinstructions (
  instructions : Vec<SaveInstruction>,
  config       : SkgConfig,
) -> io::Result<(usize, usize)> { // (deleted, written)

  let ( to_delete, to_write ) : ( Vec<_>, Vec<_> ) =
    instructions . into_iter ()
    . partition ( |(_, action)| action . toDelete );

  let delete_nodes : Vec<SkgNode> =
    to_delete . into_iter ()
    . map ( |(node, _)| node )
    . collect ();

  let write_nodes : Vec<SkgNode> =
    to_write . into_iter ()
    . map ( |(node, _)| node )
    . collect ();

  let deleted = if ! delete_nodes . is_empty () {
    delete_all_nodes_from_fs (
      delete_nodes, config . clone () ) ?
  } else { 0 };

  let written = if ! write_nodes . is_empty () {
    write_all_nodes_to_fs (
      write_nodes, config ) ?
  } else { 0 };

  Ok ( (deleted, written) ) }
