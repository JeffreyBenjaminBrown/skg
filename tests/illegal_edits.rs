// cargo nextest run --test grouped_repos -E 'test(illegal_edits::)'
//
// Illegal edits to foreign nodes ('docs/telescopes.org', "Illegal
// edits"): a section of a node in a skgrepo of another owner than its
// home's. On the server of the user who wrote it, an owned section
// under a foreign home is drawn under the node as an ILLEGAL EDIT TO
// FOREIGN NODE child and is no part of the node; nothing saves it.
//
// The fixture has the owned skgrepo "public" and the foreign one
// "theirs": theirs' f contains g; public's f, the illegal edit, is
// titled "my note on f", has a body and contains c.

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, open_content_view, save_view_under,
  with_git_fixture, write_unchanged_skgrepos};

use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::misc::{ID, SkgrepoName};

use std::error::Error;
use std::fs;
use tempfile::TempDir;

const VIEW : &str = "view";

fn with_an_illegal_edit (
  body : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let case : TempDir = TempDir::new () ?;
  write_unchanged_skgrepos ( case . path (), &[
    ("public", &[ ("f", "pid: f\ntitle: my note on f\nbody: what I wrote\ncontains:\n- c\n"),
                  ("c", "pid: c\ntitle: c\n") ]),
    ("theirs", &[ ("f", "pid: f\ntitle: their f\ncontains:\n- g\n"),
                  ("g", "pid: g\ntitle: their g\n") ]) ] ) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public"]), &["theirs"], body ) }

#[test]
fn an_owned_section_of_a_foreign_node_is_drawn_as_an_illegal_edit_and_saves_nothing (
) -> Result<(), Box<dyn Error>> {
  with_an_illegal_edit ( |f| {
    let node = f . graph . load_full () . nodes . get (&ID::from ("f")) . cloned ()
      . expect ("f is in the graph");
    assert_eq! ( node . home_skgrepo, SkgrepoName::from ("theirs"),
                 "the foreign section is the node" );
    assert_eq! ( node . title, "their f" );
    let unrestricted : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    let opened : String = open_content_view (f, "f", VIEW, &unrestricted) ?;
    assert_eq! (
      skg::metadata_canon::without_viewnode_ids (&opened),
      concat! (
        "* (skg node (gnid f) (skgRepos (nodeRepo theirs)) (view (rels (contains (out 1))) repoBoundary)) their f\n",
        "** (skg illegalEditFolder) my note on f\n",
        "what I wrote\n",
        "*** (skg node (gnid c) (skgRepos (nodeRepo public)) writeProtected (view repoBoundary)) c\n",
        "** (skg node (gnid g) (skgRepos (nodeRepo theirs)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) their g\n" ),
      "the illegal section first, under the node it is no part of: f contains g, \
       not c" );
    { // A save of the view, its illegal-edit text retyped, writes nothing.
      let before_public : String = fs::read_to_string (
        f . root . path () . join ("public/f.skg") ) ?;
      let before_theirs : String = fs::read_to_string (
        f . root . path () . join ("theirs/f.skg") ) ?;
      let edited : String = opened . replace ("my note on f", "my retyped note");
      let saved : SaveResponse = save_view_under (f, VIEW, &edited, &unrestricted) ?;
      assert! ( saved . errors . is_empty (), "{:?}", saved . errors );
      assert! ( saved . saved_view . contains ("illegalEditFolder) my note on f"),
                "the refresh redraws the illegal section's own text:\n{}",
                saved . saved_view );
      assert_eq! ( fs::read_to_string ( f . root . path () . join ("public/f.skg") ) ?,
                   before_public );
      assert_eq! ( fs::read_to_string ( f . root . path () . join ("theirs/f.skg") ) ?,
                   before_theirs ); }
    Ok (( )) } ) }
