//! Save-time publication gate for malformed title/body-text placement.
//!
//! A buffer-authored Graphnode has already lost the provenance of its
//! title and body. Before writing it, reread the current disk telescope and
//! fold the title/body text with the load path. If either selected title/body text lives below
//! home, the save must carry an exact PID approval obtained from the typed
//! confirmation response.

use crate::dbs::filesystem::one_node::telescope_from_disk;
use crate::telescope::fold::fold_telescope_collecting_warnings;
use crate::types::misc::{ID, SkgConfig, SkgRepoName};
use crate::types::save::{NodeInstruction, NodeMerge, SaveNode};
use crate::types::sexp::extract_string_list_from_sexp;

use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::io;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HoistCandidate {
  pub pid  : ID,
  pub home : SkgRepoName,
}

/// Exact approvals have their own field because permission to release overPrivateText
/// text from the server is not permission to publish it on disk.
pub fn approved_pids_from_request (
  request : &str,
) -> HashSet<ID> {
  let values : Vec<String> = sexp::parse (request) . ok ()
    .and_then ( |parsed|
      extract_string_list_from_sexp (
        &parsed, "approved-hoist-pids" ) . ok () )
    .unwrap_or_default ();
  // The sexp crate represents a dotted pair as [key, ".", value]. The
  // generic list helper therefore accepts it syntactically; reject that
  // shape here so publication authority is always a proper PID list.
  if values . iter () . any ( |value| value == "." ) {
    return HashSet::new (); }
  values
    .into_iter ()
    .map (ID)
    .collect ()
}

/// Reread and classify every existing telescope that this save will write.
/// New nodes have no disk telescope and therefore cannot be Hoist candidates.
pub fn candidates_from_disk (
  node_instructions : &[NodeInstruction],
  node_merges       : &[NodeMerge],
  config            : &SkgConfig,
) -> io::Result<Vec<HoistCandidate>> {
  let mut touched : HashSet<ID> = HashSet::new ();
  let mut note_save = |node_instruction : &NodeInstruction| {
    if let NodeInstruction::Save (SaveNode (node)) = node_instruction {
      touched . insert ( node . pid . clone () ); }};
  for node_instruction in node_instructions {
    note_save (node_instruction); }
  for node_instruction in node_merges . iter ()
      . flat_map ( |merge| merge . to_vec () ) {
    note_save (&node_instruction); }
  // A nodeMerge copies the acquiree's folded title/body into a fresh
  // preservation node before deleting the acquiree. That textual input is a
  // touched telescope even though the primary nodeInstruction for its old PID is
  // Delete, not Save.
  for node_merge in node_merges {
    touched . insert ( node_merge . acquiree_skgid () . clone () ); }
  let mut touched : Vec<ID> = touched . into_iter () . collect ();
  touched . sort_by ( |a, b| a . as_str () . cmp (b . as_str ()) );

  let mut candidates : Vec<HoistCandidate> = Vec::new ();
  for pid in touched {
    let Some (telescope) = telescope_from_disk (config, &pid) ?
      else { continue; };
    let home : SkgRepoName = telescope . home () . clone ();
    if ! config . skgrepo_is_owned (&home) {
      return Err ( io::Error::new (
        io::ErrorKind::PermissionDenied,
        format! (
          "Refusing to offer Hoist for '{}': its selected home '{}' is not owned.",
          pid, home ))); }
    let (node, _warnings) = fold_telescope_collecting_warnings (
      telescope, & |skgid : &ID| skgid . clone () ) ?;
    if node . overPrivateText_telescope {
      candidates . push ( HoistCandidate { pid, home } ); }}
  Ok (candidates)
}

/// Some operations consume an overPrivateText telescope without writing that same PID.
/// NodeMerge is the important case: it copies the acquiree's text to a fresh
/// preservation node and deletes the acquiree. Add a disk-folded Save first so
/// the approved interactive pipeline genuinely Hoists and verifies that input
/// before the operation consumes it. Existing buffer-authored Saves win; their
/// edits, rather than the pre-save disk title/body text, must land at home.
pub fn repair_saves_for_unwritten_candidates (
  candidates        : &[HoistCandidate],
  node_instructions : &[NodeInstruction],
  config            : &SkgConfig,
) -> io::Result<Vec<NodeInstruction>> {
  let already_written : HashSet<ID> = node_instructions . iter ()
    .filter_map ( |node_instruction| match node_instruction {
      NodeInstruction::Save (SaveNode (node)) => Some (node . pid . clone ()),
      NodeInstruction::Delete (_)             => None,
    } )
    .collect ();
  let mut repairs : Vec<NodeInstruction> = Vec::new ();
  for candidate in candidates {
    if already_written . contains (&candidate . pid) { continue; }
    let Some (telescope) = telescope_from_disk (
        config, &candidate . pid) ? else { continue; };
    let (mut node, _warnings) = fold_telescope_collecting_warnings (
      telescope, & |skgid : &ID| skgid . clone () ) ?;
    node . overPrivateText_telescope = false;
    repairs . push ( NodeInstruction::Save (SaveNode (node)) ); }
  Ok (repairs)
}

pub fn needs_confirmation (
  candidates : &[HoistCandidate],
  approved   : &HashSet<ID>,
) -> bool {
  candidates . iter () . any (
    |candidate| ! approved . contains (&candidate . pid) )
}

/// Text-free response: homes and IDs are structural facts, but no selected
/// title or body crosses the boundary before the user approves publication.
pub fn confirmation_response (
  candidates : &[HoistCandidate],
) -> String {
  let telescope_entries : Vec<Sexp> = candidates . iter ()
    .map ( |candidate| Sexp::List ( vec! [
      pair ("pid", candidate . pid . as_str ()),
      pair ("home", candidate . home . as_str ()),
    ] ) )
    .collect ();
  let count : usize = candidates . len ();
  let prompt : String = format! (
    "Saving would publish title or body text selected below home for {} telescope{}. Hoist writes the selected text at home and removes lower title/body copies while preserving lower relationships and aliases. Abort writes nothing; manual repair requires editing the .skg files. Hoist?",
    count, if count == 1 { "" } else { "s" } );
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      atom ("telescopes"), Sexp::List (telescope_entries) ] ),
    pair ("prompt", &prompt),
  ] ) . to_string ()
}

fn atom (value : &str) -> Sexp {
  Sexp::Atom ( Atom::S ( value . to_string () ) )
}

fn pair (key : &str, value : &str) -> Sexp {
  Sexp::List ( vec! [atom (key), atom (value)] )
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::filesystem::one_node::graphnode_from_pid_and_skgrepo;
  use crate::save::update_fs_from_nodeInstructions_with_hoist_approval;
  use crate::types::misc::SkgRepo;
  use crate::types::nodes::fs::GraphnodeOnDisk;
  use std::collections::HashMap;
  use std::fs;
  use std::path::PathBuf;
  use tempfile::{TempDir, tempdir};

  fn config_and_paths () -> (TempDir, SkgConfig, HashMap<&'static str, PathBuf>) {
    let temp : TempDir = tempdir () . unwrap ();
    let mut skgrepos : HashMap<SkgRepoName, SkgRepo> = HashMap::new ();
    let mut paths : HashMap<&'static str, PathBuf> = HashMap::new ();
    for (name, owned) in [
        ("public", true), ("middle", true),
        ("private", true), ("foreign", false)] {
      let path : PathBuf = temp . path () . join (name);
      fs::create_dir_all (&path) . unwrap ();
      paths . insert (name, path . clone ());
      skgrepos . insert ( SkgRepoName::from (name), SkgRepo {
        name         : SkgRepoName::from (name),
        abbreviation : None,
        path,
        owned : owned,
      } ); }
    let mut config : SkgConfig = SkgConfig::dummyFromSkgRepos (skgrepos);
    config . data_root = temp . path () . to_path_buf ();
    config . skgrepo_order = ["public", "middle", "private", "foreign"]
      . into_iter () . map (SkgRepoName::from) . collect ();
    (temp, config, paths)
  }

  fn save_from_disk (
    pid    : &str,
    config : &SkgConfig,
  ) -> NodeInstruction {
    let mut node = graphnode_from_pid_and_skgrepo (
      config, ID::from (pid), &SkgRepoName::from ("public") ) . unwrap ();
    // Buffer-authored nodes carry no disk overPrivateTextness authority. The save gate
    // has just rederived that fact from disk.
    node . overPrivateText_telescope = false;
    NodeInstruction::Save (SaveNode (node))
  }

  #[test]
  fn approvals_are_exact_pids_and_malformed_authority_fails_closed () {
    assert_eq! (
      approved_pids_from_request (
        "((request . \"save buffer\") (approved-hoist-pids \"A\" \"B\"))" ),
      [ID::from ("A"), ID::from ("B")]
      . into_iter () . collect () );
    assert! ( approved_pids_from_request (
      "((approved-hoist-pids . \"all\"))" ) . is_empty () );
  }

  #[test]
  fn confirmation_contains_structure_but_no_text () {
    let response : String = confirmation_response (&[
      HoistCandidate {
        pid  : ID::from ("P"),
        home : SkgRepoName::from ("public"),
      },
    ]);
    assert! (response . contains ("P"));
    assert! (response . contains ("public"));
    assert! (response . contains ("manual repair"));
    assert! (! response . contains ("SECRET"));
  }

  #[test]
  fn approved_hoist_moves_independent_text_and_preserves_lower_data () {
    let (_temp, config, paths) = config_and_paths ();
    fs::write (
      paths ["public"] . join ("P.skg"),
      "pid: P\ncontains:\n- public-child\n" ) . unwrap ();
    fs::write (
      paths ["middle"] . join ("P.skg"),
      "title: lower title\naliases:\n- lower alias\npid: P\n" ) . unwrap ();
    fs::write (
      paths ["private"] . join ("P.skg"),
      "pid: P\nbody: lower body\ncontains:\n- private-child\n" ) . unwrap ();
    fs::write ( paths ["public"] . join ("T.skg"),
                "pid: T\n" ) . unwrap ();
    fs::write ( paths ["private"] . join ("T.skg"),
                "title: title only\npid: T\n" ) . unwrap ();
    fs::write ( paths ["public"] . join ("B.skg"),
                "title: body-only title\npid: B\n" ) . unwrap ();
    fs::write ( paths ["private"] . join ("B.skg"),
                "pid: B\nbody: body only\n" ) . unwrap ();
    let node_instructions : Vec<NodeInstruction> = ["P", "T", "B"]
      .into_iter () . map ( |pid| save_from_disk (pid, &config) )
      .collect ();
    let candidates : Vec<HoistCandidate> = candidates_from_disk (
      &node_instructions, &[], &config ) . unwrap ();
    assert_eq! (
      candidates . iter () . map ( |candidate| candidate . pid . clone () )
      .collect::<Vec<ID>> (),
      vec! [ID::from ("B"), ID::from ("P"), ID::from ("T")]);

    let approved : HashSet<ID> =
      [ID::from ("P"), ID::from ("T"), ID::from ("B")]
      .into_iter () . collect ();
    update_fs_from_nodeInstructions_with_hoist_approval (
      &node_instructions, &[], config . clone (), &approved ) . unwrap ();
    let reread = graphnode_from_pid_and_skgrepo (
      &config, ID::from ("P"), &SkgRepoName::from ("public") ) . unwrap ();
    assert! (! reread . overPrivateText_telescope);
    assert_eq! (reread . title, "lower title");
    assert_eq! (reread . body . as_deref (), Some ("lower body"));

    let home : GraphnodeOnDisk = serde_yaml::from_str (&fs::read_to_string (
      paths ["public"] . join ("P.skg") ) . unwrap ()) . unwrap ();
    let middle : GraphnodeOnDisk = serde_yaml::from_str (&fs::read_to_string (
      paths ["middle"] . join ("P.skg") ) . unwrap ()) . unwrap ();
    let lower : GraphnodeOnDisk = serde_yaml::from_str (&fs::read_to_string (
      paths ["private"] . join ("P.skg") ) . unwrap ()) . unwrap ();
    assert_eq! (home . title . as_deref (), Some ("lower title"));
    assert_eq! (home . body . as_deref (), Some ("lower body"));
    assert_eq! (middle . title, None);
    assert_eq! (middle . body, None);
    assert_eq! (middle . aliases, vec! ["lower alias"]);
    assert_eq! (lower . title, None);
    assert_eq! (lower . body, None);
    assert! (lower . to_yaml () . unwrap () . contains ("private-child"));
    for pid in ["T", "B"] {
      assert! (! graphnode_from_pid_and_skgrepo (
        &config, ID::from (pid), &SkgRepoName::from ("public") )
        .unwrap () . overPrivateText_telescope); }
  }

  #[test]
  fn a_new_dirty_pid_after_approval_requires_a_fresh_batch_decision () {
    let (_temp, config, paths) = config_and_paths ();
    fs::write ( paths ["public"] . join ("A.skg"),
                "pid: A\n" ) . unwrap ();
    fs::write ( paths ["private"] . join ("A.skg"),
                "title: A lower\npid: A\n" ) . unwrap ();
    let save_a   : NodeInstruction = save_from_disk ("A", &config);
    let approved : HashSet<ID> =
      [ID::from ("A")] . into_iter () . collect ();
    let first : Vec<HoistCandidate> = candidates_from_disk (
      &[save_a . clone ()], &[], &config ) . unwrap ();
    assert! (! needs_confirmation (&first, &approved));

    // Another touched telescope changes on disk while the user answers.
    fs::write ( paths ["public"] . join ("B.skg"),
                "title: B home\npid: B\n" ) . unwrap ();
    fs::write ( paths ["private"] . join ("B.skg"),
                "pid: B\nbody: B lower\n" ) . unwrap ();
    let save_b : NodeInstruction = save_from_disk ("B", &config);
    let second : Vec<HoistCandidate> = candidates_from_disk (
      &[save_a, save_b], &[], &config ) . unwrap ();
    assert_eq! (second . len (), 2);
    assert! (needs_confirmation (&second, &approved));
  }
}
