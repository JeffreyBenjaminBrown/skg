//! Save-time publication gate for malformed scalar placement.
//!
//! A buffer-authored NodeComplete has already lost the provenance of its
//! title and body. Before writing it, reread the current disk telescope and
//! fold the scalars with the load path. If either selected scalar lives below
//! home, the save must carry an exact PID approval obtained from the typed
//! confirmation response.

use crate::dbs::filesystem::one_node::telescope_from_disk;
use crate::telescope::fold::fold_telescope_collecting_warnings;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::save::{DefineNode, NodeMerge, SaveNode};
use crate::types::sexp::extract_string_list_from_sexp;

use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::io;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HoistCandidate {
  pub pid  : ID,
  pub home : SourceName,
}

/// Exact approvals have their own field because permission to release ugly
/// text from the server is not permission to publish it on disk.
pub fn approved_pids_from_request (
  request : &str,
) -> HashSet<ID> {
  let values : Vec<String> = sexp::parse (request) . ok ()
    .and_then ( |parsed|
      extract_string_list_from_sexp (
        &parsed, "hoist-approved-pids" ) . ok () )
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
  define_nodes : &[DefineNode],
  node_merges  : &[NodeMerge],
  config       : &SkgConfig,
) -> io::Result<Vec<HoistCandidate>> {
  let mut touched : HashSet<ID> = HashSet::new ();
  let mut note_save = |define_node : &DefineNode| {
    if let DefineNode::Save (SaveNode (node)) = define_node {
      touched . insert ( node . pid . clone () ); }};
  for define_node in define_nodes {
    note_save (define_node); }
  for define_node in node_merges . iter ()
      . flat_map ( |merge| merge . to_vec () ) {
    note_save (&define_node); }
  // A nodeMerge copies the acquiree's folded title/body into a fresh
  // preservation node before deleting the acquiree. That textual input is a
  // touched telescope even though the primary instruction for its old PID is
  // Delete, not Save.
  for node_merge in node_merges {
    touched . insert ( node_merge . acquiree_id () . clone () ); }
  let mut touched : Vec<ID> = touched . into_iter () . collect ();
  touched . sort_by ( |a, b| a . as_str () . cmp (b . as_str ()) );

  let mut candidates : Vec<HoistCandidate> = Vec::new ();
  for pid in touched {
    let Some (telescope) = telescope_from_disk (config, &pid) ?
      else { continue; };
    let home : SourceName = telescope . home () . clone ();
    if ! config . user_owns_source (&home) {
      return Err ( io::Error::new (
        io::ErrorKind::PermissionDenied,
        format! (
          "Refusing to offer Hoist for '{}': its selected home '{}' is not owned.",
          pid, home ))); }
    let (node, _warnings) = fold_telescope_collecting_warnings (
      telescope, & |id : &ID| id . clone () ) ?;
    if node . ugly_telescope {
      candidates . push ( HoistCandidate { pid, home } ); }}
  Ok (candidates)
}

/// Some operations consume an ugly telescope without writing that same PID.
/// NodeMerge is the important case: it copies the acquiree's text to a fresh
/// preservation node and deletes the acquiree. Add a disk-folded Save first so
/// the approved interactive pipeline genuinely Hoists and verifies that input
/// before the operation consumes it. Existing buffer-authored Saves win; their
/// edits, rather than the pre-save disk scalar, must land at home.
pub fn repair_saves_for_unwritten_candidates (
  candidates   : &[HoistCandidate],
  define_nodes : &[DefineNode],
  config       : &SkgConfig,
) -> io::Result<Vec<DefineNode>> {
  let already_written : HashSet<ID> = define_nodes . iter ()
    .filter_map ( |define_node| match define_node {
      DefineNode::Save (SaveNode (node)) => Some (node . pid . clone ()),
      DefineNode::Delete (_)             => None,
    } )
    .collect ();
  let mut repairs : Vec<DefineNode> = Vec::new ();
  for candidate in candidates {
    if already_written . contains (&candidate . pid) { continue; }
    let Some (telescope) = telescope_from_disk (
        config, &candidate . pid) ? else { continue; };
    let (mut node, _warnings) = fold_telescope_collecting_warnings (
      telescope, & |id : &ID| id . clone () ) ?;
    node . ugly_telescope = false;
    repairs . push ( DefineNode::Save (SaveNode (node)) ); }
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
    "Saving would publish title or body text selected below home for {} telescope{}. Hoist writes the selected text at home and removes lower scalar copies while preserving lower relationships and aliases. Abort writes nothing; manual repair requires editing the .skg files. Hoist?",
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
  use crate::dbs::filesystem::one_node::nodecomplete_from_pid_and_source;
  use crate::save::update_fs_from_saveinstructions_with_hoist_approval;
  use crate::types::misc::SkgfileSource;
  use crate::types::nodes::fs::NodeFS;
  use std::collections::HashMap;
  use std::fs;
  use std::path::PathBuf;
  use tempfile::{TempDir, tempdir};

  fn config_and_paths () -> (TempDir, SkgConfig, HashMap<&'static str, PathBuf>) {
    let temp : TempDir = tempdir () . unwrap ();
    let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new ();
    let mut paths : HashMap<&'static str, PathBuf> = HashMap::new ();
    for (name, owned) in [
        ("public", true), ("middle", true),
        ("private", true), ("foreign", false)] {
      let path : PathBuf = temp . path () . join (name);
      fs::create_dir_all (&path) . unwrap ();
      paths . insert (name, path . clone ());
      sources . insert ( SourceName::from (name), SkgfileSource {
        name         : SourceName::from (name),
        abbreviation : None,
        path,
        user_owns_it : owned,
      } ); }
    let mut config : SkgConfig = SkgConfig::dummyFromSources (sources);
    config . data_root = temp . path () . to_path_buf ();
    config . source_order = ["public", "middle", "private", "foreign"]
      . into_iter () . map (SourceName::from) . collect ();
    (temp, config, paths)
  }

  fn save_from_disk (
    pid    : &str,
    config : &SkgConfig,
  ) -> DefineNode {
    let mut node = nodecomplete_from_pid_and_source (
      config, ID::from (pid), &SourceName::from ("public") ) . unwrap ();
    // Buffer-authored nodes carry no disk ugliness authority. The save gate
    // has just rederived that fact from disk.
    node . ugly_telescope = false;
    DefineNode::Save (SaveNode (node))
  }

  #[test]
  fn approvals_are_exact_pids_and_malformed_authority_fails_closed () {
    assert_eq! (
      approved_pids_from_request (
        "((request . \"save buffer\") (hoist-approved-pids \"A\" \"B\"))" ),
      [ID::from ("A"), ID::from ("B")]
      . into_iter () . collect () );
    assert! ( approved_pids_from_request (
      "((hoist-approved-pids . \"all\"))" ) . is_empty () );
  }

  #[test]
  fn confirmation_contains_structure_but_no_scalar_text () {
    let response : String = confirmation_response (&[
      HoistCandidate {
        pid  : ID::from ("P"),
        home : SourceName::from ("public"),
      },
    ]);
    assert! (response . contains ("P"));
    assert! (response . contains ("public"));
    assert! (response . contains ("manual repair"));
    assert! (! response . contains ("SECRET"));
  }

  #[test]
  fn approved_hoist_moves_independent_scalars_and_preserves_lower_data () {
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
    let define_nodes : Vec<DefineNode> = ["P", "T", "B"]
      .into_iter () . map ( |pid| save_from_disk (pid, &config) )
      .collect ();
    let candidates : Vec<HoistCandidate> = candidates_from_disk (
      &define_nodes, &[], &config ) . unwrap ();
    assert_eq! (
      candidates . iter () . map ( |candidate| candidate . pid . clone () )
      .collect::<Vec<ID>> (),
      vec! [ID::from ("B"), ID::from ("P"), ID::from ("T")]);

    let approved : HashSet<ID> =
      [ID::from ("P"), ID::from ("T"), ID::from ("B")]
      .into_iter () . collect ();
    update_fs_from_saveinstructions_with_hoist_approval (
      &define_nodes, &[], config . clone (), &approved ) . unwrap ();
    let reread = nodecomplete_from_pid_and_source (
      &config, ID::from ("P"), &SourceName::from ("public") ) . unwrap ();
    assert! (! reread . ugly_telescope);
    assert_eq! (reread . title, "lower title");
    assert_eq! (reread . body . as_deref (), Some ("lower body"));

    let home : NodeFS = serde_yaml::from_str (&fs::read_to_string (
      paths ["public"] . join ("P.skg") ) . unwrap ()) . unwrap ();
    let middle : NodeFS = serde_yaml::from_str (&fs::read_to_string (
      paths ["middle"] . join ("P.skg") ) . unwrap ()) . unwrap ();
    let lower : NodeFS = serde_yaml::from_str (&fs::read_to_string (
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
      assert! (! nodecomplete_from_pid_and_source (
        &config, ID::from (pid), &SourceName::from ("public") )
        .unwrap () . ugly_telescope); }
  }

  #[test]
  fn a_new_dirty_pid_after_approval_requires_a_fresh_batch_decision () {
    let (_temp, config, paths) = config_and_paths ();
    fs::write ( paths ["public"] . join ("A.skg"),
                "pid: A\n" ) . unwrap ();
    fs::write ( paths ["private"] . join ("A.skg"),
                "title: A lower\npid: A\n" ) . unwrap ();
    let save_a : DefineNode = save_from_disk ("A", &config);
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
    let save_b : DefineNode = save_from_disk ("B", &config);
    let second : Vec<HoistCandidate> = candidates_from_disk (
      &[save_a, save_b], &[], &config ) . unwrap ();
    assert_eq! (second . len (), 2);
    assert! (needs_confirmation (&second, &approved));
  }
}
