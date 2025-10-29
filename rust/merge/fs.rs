use crate::file_io::{write_node};
use crate::types::{SaveInstruction, SkgConfig, SkgNode, ID};
use crate::util::path_from_pid;
use std::collections::HashSet;
use std::error::Error;
use std::fs;

/// Merges nodes in filesystem by applying merge SaveInstructions.
pub(crate) fn merge_nodes_in_fs (
  _instructions : Vec<SaveInstruction>,
  config       : SkgConfig,
  acquiree_text_preservers : Vec<&SaveInstruction>,
  updated_acquirers : Vec<&SaveInstruction>,
  deleted_acquirees : Vec<&SaveInstruction>,
) -> Result < (), Box<dyn Error> > {

  if acquiree_text_preservers.is_empty() {
    return Ok(());
  }

  // Process each merge
  for i in 0..acquiree_text_preservers.len() {
    let acquiree_text_preserver : &SkgNode = &acquiree_text_preservers[i].0;
    let updated_acquirer : &SkgNode = &updated_acquirers[i].0;
    let acquiree : &SkgNode = &deleted_acquirees[i].0;

    // Write acquiree_text_preserver to disk
    let acquiree_text_preserver_path : String =
      path_from_pid(&config, acquiree_text_preserver.ids[0].clone());
    write_node(acquiree_text_preserver, &acquiree_text_preserver_path)?;

    // Compute final relationship fields for acquirer
    let acquirer_final_contains : HashSet<ID> =
      updated_acquirer.contains.iter().cloned().collect();

    let mut acquirer_to_write : SkgNode = updated_acquirer.clone();

    // Combine subscribes_to
    acquirer_to_write.subscribes_to = Some(
      updated_acquirer.subscribes_to.clone().unwrap_or_default()
        .into_iter()
        .chain(acquiree.subscribes_to.clone().unwrap_or_default())
        .collect()
    );

    // Combine hides_from_its_subscriptions (with filtering)
    let mut combined_hides : Vec<ID> = Vec::new();
    for list in [&updated_acquirer.hides_from_its_subscriptions,
                 &acquiree.hides_from_its_subscriptions] {
      if let Some(hides_list) = list {
        for hidden_id in hides_list {
          if !acquirer_final_contains.contains(hidden_id)
             && !combined_hides.contains(hidden_id) {
            combined_hides.push(hidden_id.clone());
          }
        }
      }
    }
    acquirer_to_write.hides_from_its_subscriptions = Some(combined_hides);

    // Combine overrides_view_of
    acquirer_to_write.overrides_view_of = Some(
      updated_acquirer.overrides_view_of.clone().unwrap_or_default()
        .into_iter()
        .chain(acquiree.overrides_view_of.clone().unwrap_or_default())
        .collect()
    );

    // Write updated acquirer to disk
    let acquirer_path : String =
      path_from_pid(&config, acquirer_to_write.ids[0].clone());
    write_node(&acquirer_to_write, &acquirer_path)?;

    // Delete acquiree from disk
    let acquiree_path : String =
      path_from_pid(&config, acquiree.ids[0].clone());
    fs::remove_file(&acquiree_path)?;
  }

  Ok(())
}
