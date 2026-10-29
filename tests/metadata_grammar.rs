// cargo nextest run --test grouped_unit -E 'test(metadata_grammar::)'
//
// The shape of every headline's '(skg ...)' metadata
// ('METADATA_GRAMMAR' in server/serve/parse_metadata_sexp.rs): the
// viewnode ID first, the kind a bare atom, every part beside it at the
// top level, in any order; the retired spellings refused with a
// message naming the grammar; each kind taking only its own parts; and
// the parse keeping the kind it read, with that kind's parts.

use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  GnidOnlyMetadata, METADATA_GRAMMAR, MetadataKind, ViewnodeMetadata,
  parse_metadata_to_viewnodemd, viewnode_from_metadata};
use skg::types::misc::{ID, SkgConfig, SkgrepoName};
use skg::types::viewnode::{
  Ghost, GnidOnlyKind, GnidOnlyVognode, PropertyMember, Viewnode, ViewnodeId, ViewnodeKind,
  Vognode, Warrant,
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
      claimsMembership : true, warrant : Warrant::ByGraph } )))),
    "ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest restoreNode)" );
  assert_eq! (
    rendered ( viewnode_of ( ViewnodeKind::Vognode ( Vognode::GnidOnly ( GnidOnlyVognode::new (
      GnidOnlyKind::Unknown, ID::from ("u"), Some ( SkgrepoName::from ("pub") ),
      true, Warrant::ByGraph ) )))),
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
              "{} was refused with {:?}", retired, refusal ); }
  { // The compound form 'restrictedNode' also took is no key at all.
    let compound : &str =
      "(skg (restrictedNode (gnid hidden) (skgRepos (nodeRepo private))))";
    assert! ( parse_metadata_to_viewnodemd (compound) . is_err (),
              "{} must be refused", compound ); }}

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
    ( "(skg restricted (gnid r) writeProtected)",
      "A headline of kind restricted does not take writeProtected" ),
    ( "(skg restricted (gnid r) (skgRepos (nodeRepo main)))",
      "The skgRepos of a headline of kind restricted holds only (relRepo R)" ),
    ( "(skg restricted (skgRepos (relRepo pub)))",
      "A Restricted vognode requires its (gnid X)" ),
    ( "(skg restricted (gnid r) (editRequest delete))",
      "A headline of kind restricted accepts no edit request but (relRepo R) and a readiness request" ),
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
  assert! ( metadata . kind == MetadataKind::Node
            && metadata . foldAndFocus . focused ); }

/// The parse keeps a gnid-only vognode's kind with its parts, and
/// refuses text on its line as it always has, by its kind's atom.
#[test]
fn a_gnid_only_vognode_keeps_its_kind_and_its_parts (
) {
  for kind in [ GnidOnlyKind::Unknown, GnidOnlyKind::Restricted ] {
    let metadata : ViewnodeMetadata = parse_metadata_to_viewnodemd ( &format! (
      "(skg {} (gnid g) (skgRepos (relRepo pub)) noClaim (editRequest (relRepo priv)))",
      kind . wire_atom () )) . unwrap ();
    assert_eq! ( metadata . kind, MetadataKind::GnidOnly ( GnidOnlyMetadata {
      kind,
      gnid            : ID::from ("g"),
      relRepo         : Some ( SkgrepoName::from ("pub") ),
      relRepo_request : Some ( SkgrepoName::from ("priv") ), } ));
    assert! ( ! metadata . names_an_occurrence () );
    let (viewnode, error, _warning) =
      viewnode_from_metadata (&metadata, String::new (), None);
    assert_eq! ( error, None );
    let ViewnodeKind::Vognode ( Vognode::GnidOnly (gnid_only) ) = &viewnode . kind
      else { panic! ("{:?} parsed to {:?}", kind, viewnode . kind) };
    assert_eq! ( ( gnid_only . kind, gnid_only . claimsMembership ), (kind, false) );
    let (_viewnode, error, _warning) =
      viewnode_from_metadata (&metadata, "typed text" . into (), None);
    let refusal : String = format! ( "{:?}", error . expect ("text is refused") );
    assert! ( refusal . contains ( &format! (
                "{} content cannot be edited.", kind . wire_atom () )),
              "{}", refusal ); }}
