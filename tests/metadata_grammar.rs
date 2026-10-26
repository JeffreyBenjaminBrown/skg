// cargo nextest run --test grouped_unit -E 'test(metadata_grammar::)'
//
// The shape of every headline's '(skg ...)' metadata
// ('METADATA_GRAMMAR' in server/serve/parse_metadata_sexp.rs): the
// viewnode ID first, the kind a bare atom, every part beside it at the
// top level, in any order; the retired spellings refused with a
// message naming the grammar; and each kind taking only its own parts.

use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  METADATA_GRAMMAR, ViewnodeMetadata, parse_metadata_to_viewnodemd};
use skg::types::misc::{ID, SkgConfig, SkgrepoName};
use skg::types::viewnode::{
  Ghost, PropertyMember, Unknown, Viewnode, ViewnodeId, ViewnodeKind, Vognode, Warrant,
  mk_editable_viewnode, mk_writeProtected_viewnode, viewnode_of};

use std::collections::HashMap;

fn config (
) -> SkgConfig {
  SkgConfig::dummyFromSkgrepos (HashMap::new ()) }

/// An editable node n of 'main' whose relationship to its viewparent
/// is recorded in 'pub', and which stands for the original o.
fn node_with_its_declared_parts (
) -> Viewnode {
  let mut viewnode : Viewnode =
    mk_editable_viewnode ( ID::from ("n"), SkgrepoName::from ("main"),
                           "n" . into (), None );
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
    = &mut viewnode . kind {
    occurrence . viewStats . relRepo = Some ( SkgrepoName::from ("pub") );
    occurrence . overridesHere = Some ( ID::from ("o") ); }
  viewnode }

#[test]
fn each_kind_prints_its_parts_beside_its_atom (
) {
  let rendered = |viewnode : Viewnode| -> String {
    viewnode_to_string (&viewnode, &config ()) . unwrap () };
  assert_eq! (
    rendered ( Viewnode { viewnode_id : Some ( ViewnodeId::from_counter (10) ),
                          .. node_with_its_declared_parts () } ),
    format! ( "(vnid {}) node (gnid n) (skgRepos (nodeRepo main) (relRepo pub)) (overridesHere o)",
              ViewnodeId::from_counter (10) ));
  assert_eq! (
    rendered ( mk_writeProtected_viewnode (
      ID::from ("w"), SkgrepoName::from ("main"), "w" . into (), false )),
    "node (gnid w) (skgRepos (nodeRepo main)) noClaim writeProtected" );
  assert_eq! (
    rendered ( viewnode_of ( ViewnodeKind::Vognode ( Vognode::Ghost ( Ghost {
      gnid : ID::from ("g"), home_skgrepo : SkgrepoName::from ("main"),
      title : "g" . into (), body : None,
      restoreNode_request : true,
      claimsMembership : true, isWarranted : Warrant::ByGraph } )))),
    "ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest restoreNode)" );
  assert_eq! (
    rendered ( viewnode_of ( ViewnodeKind::Vognode ( Vognode::Unknown ( Unknown {
      gnid : ID::from ("u"), relRepo : Some ( SkgrepoName::from ("pub") ),
      relationshipEditRequest : None, claimsMembership : true } )))),
    "unknown (gnid u) (skgRepos (relRepo pub))" );
  assert_eq! (
    rendered ( viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::alias ( "an alias" . into (), Some ( SkgrepoName::from ("pub") ), None ) ))),
    "alias (skgRepos (relRepo pub))" ); }

#[test]
fn the_declared_parts_parse_back_in_any_order (
) {
  let in_print_order : ViewnodeMetadata = parse_metadata_to_viewnodemd (
    "(skg (vnid 1a) folded node (gnid n) (skgRepos (nodeRepo main) (relRepo pub)) \
     noClaim writeProtected (overridesHere o) (viewRequests editableView))" ) . unwrap ();
  let shuffled : ViewnodeMetadata = parse_metadata_to_viewnodemd (
    "(skg (viewRequests editableView) (overridesHere o) writeProtected \
     (skgRepos (relRepo pub) (nodeRepo main)) noClaim (gnid n) node folded (vnid 1a))" )
    . unwrap ();
  assert_eq! ( in_print_order, shuffled );
  assert_eq! ( in_print_order . gnid, Some ( ID::from ("n") ));
  assert_eq! ( in_print_order . home_skgrepo, Some ( SkgrepoName::from ("main") ));
  assert_eq! ( in_print_order . viewStats . relRepo, Some ( SkgrepoName::from ("pub") ));
  assert_eq! ( in_print_order . overridesHere, Some ( ID::from ("o") ));
  assert! ( in_print_order . writeProtected
            && ! in_print_order . claimsMembership
            && in_print_order . foldAndFocus . folded ); }

#[test]
fn the_retired_spellings_are_refused_naming_the_grammar (
) {
  for retired in [
    "(skg (node (id n) (repo main)))",
    "(skg (ghost (gnid g) (skgRepos (nodeRepo main))))",
    "(skg (unknown (id u)))",
    "(skg (deleted (id d)))",
    "(skg (historical (id h)))",
    "(skg node (id n))",
    "(skg node (gnid n) (repo main))",
    "(skg alias (relRepo pub))",
    "(skg node (gnid n) (viewStats (relRepo pub)))",
    "(skg node (gnid n) (viewStats (overridesHere o)))",
    "(skg node (gnid n) nonMember)",
    "(skg restrictedNode)", ] {
    let refusal : String = parse_metadata_to_viewnodemd (retired) . unwrap_err ();
    assert! ( refusal . contains ("retired") && refusal . contains (METADATA_GRAMMAR),
              "{} was refused with {:?}", retired, refusal ); }}

#[test]
fn each_kind_takes_only_its_own_parts (
) {
  for (metadata, refusal) in [
    ( "(skg ghost (gnid g) (skgRepos (nodeRepo main)) writeProtected)",
      "A headline of kind ghost does not take writeProtected" ),
    ( "(skg ghost (gnid g) (skgRepos (nodeRepo main) (relRepo pub)))",
      "A ghost's skgRepos holds only (nodeRepo R)" ),
    ( "(skg unknown (gnid u) writeProtected)",
      "A headline of kind unknown does not take writeProtected" ),
    ( "(skg unknown (gnid u) (skgRepos (nodeRepo main)))",
      "The skgRepos of a headline of kind unknown holds only (relRepo R)" ),
    ( "(skg unknown (skgRepos (relRepo pub)))",
      "An unknown requires its (gnid X)" ),
    ( "(skg alias (gnid a))",
      "A headline of kind alias does not take gnid" ),
    ( "(skg alias (skgRepos (nodeRepo main)))",
      "The skgRepos of a headline of kind alias holds only (relRepo R)" ),
    ( "(skg id (skgRepos (relRepo pub)))",
      "A headline of kind id does not take skgRepos" ),
    ( "(skg tombstone (gnid t))",
      "A headline of kind tombstone does not take gnid" ),
    ( "(skg node alias)",
      "A headline has one kind; this one names node and alias" ),
    ( "(skg node (gnid a) (gnid b))",
      "gnid may appear only once" ),
    ( "(skg node (skgRepos (nodeRepo a) (nodeRepo b)))",
      "skgRepos names nodeRepo twice" ), ] {
    assert_eq! ( parse_metadata_to_viewnodemd (metadata),
                 Err ( refusal . to_string () ), "{}", metadata ); }}

#[test]
fn a_headline_naming_no_kind_is_a_node (
) {
  let metadata : ViewnodeMetadata =
    parse_metadata_to_viewnodemd ("(skg focused)") . unwrap ();
  assert! ( metadata . non_vognode . is_none () && ! metadata . is_ghost
            && ! metadata . is_tombstone
            && metadata . unknown_node_gnid . is_none ()
            && metadata . foldAndFocus . focused ); }
