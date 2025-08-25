use skg::types::{ SkgConfig };

fn main (
) -> std::io::Result<()> {
  skg::serve::serve (
    SkgConfig {
      db_name        : "skg-test"           . into (),
      skg_folder     : "data/skg"           . into (),
      tantivy_folder : "data/index.tantivy" . into (),
    } ) }
