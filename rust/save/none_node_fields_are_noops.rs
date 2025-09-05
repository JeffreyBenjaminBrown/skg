pub fun clobber_none_fields_with_data_from_disk (
  config    : SkgConfig,
  from_user : SkgNode, // instructions from the user
) -> Result<SkgNode, Whatever> {
  id : ID = _; // get the pid from 'from_user'
  from_disk : Optional<SkgNode> = _; // try to read a file named "<pid>.skg" from the folder as indicated by 'config'

  // If no such file was found, return 'from_user' unchanged.

  // If 'from_disk' was found:
    // If 'aliases' in 'from_user' is None, replace it with 'aliases' from 'from_disk'. Otherwise keep it unchanged.
}
