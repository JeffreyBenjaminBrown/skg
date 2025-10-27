/*
PURPOSE:
When a SkgNode is created from user input,
it might not have all information relevant to the node.
This supplements the information from the user (from Emacs)
with information from disk.
.
EXAMPLE:
The node might have some aliases stored on disk,
but the information the user sent about that node from Emacs
does not mention the aliases at all.
In that case the 'aliases' field should be whatever is on disk,
rather than the None value that the user's input would suggest.
*/

use std::io;
use std::path::Path;

use crate::types::{ ID, SkgConfig, SkgNode };
use crate::file_io::read_node;
use crate::util::path_from_pid;

pub fn clobber_none_fields_with_data_from_disk (
  config    : &SkgConfig,
  from_user : SkgNode,
) -> io::Result<SkgNode> {

  let pid : ID =
    from_user . ids . first ()
    . ok_or_else (
      || io::Error::new (
        io::ErrorKind::InvalidInput,
        "SkgNode has no IDs" ))?
    . clone ();
  let from_disk : Result<SkgNode, io::Error> =
    read_node (
      & Path::new (
        & path_from_pid (
          config, pid )) );
  match from_disk {
    Err (_) => { // No such file => return input unchanged.
      Ok ( from_user ) },
    Ok ( disk_node ) => {
      let mut result : SkgNode =
        from_user;
      if result.aliases.is_none () {
        // TODO: Include subscription, override and hide relationships.
        // (See schema.tql and the SkgNode typedef.)
        // This will require making those fields Optional in SkgNode,
        // like 'aliases' already is.
        result.aliases = disk_node.aliases; }
      Ok (result) }} }
