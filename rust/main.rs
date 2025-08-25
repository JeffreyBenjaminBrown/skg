use skg::types::{ SkgConfig };

fn main (
) -> std::io::Result<()> {
  skg::serve::serve (
    SkgConfig {
      skg_folder     : "data/skg"           . into (),
      tantivy_folder : "data/index.tantivy" . into (),
    } ) }
