// cargo nextest run --test grouped_unit -E 'test(change_facts_metadata::)'
//
// The diff view's metadata forms -- '(diff ...)', '(ready ...)',
// '(readyRequest ...)' and '(unreadyRequest ...)' -- on every viewnode
// that carries them: each renders, then parses back to the same
// viewnode. '(leaves ...)' renders and parses back to nothing.

use skg::org_to_text::{FolderChangeFacts, folder_change_facts, viewnode_to_string};
use skg::serve::parse_metadata_sexp::{
  ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use skg::types::git::Sign;
use skg::types::maybe_placed_viewnode::MpViewnode;
use skg::types::misc::{ID, SkgConfig, SkgrepoName};
use skg::types::nodes::complete::Flag;
use skg::types::viewnode::{
  AffectsParent, ChangeFacts, DiffPresence, LeavesFact, Phantom,
  PhantomDeleted, PhantomUnknown, Property, DiffObject, ReadinessRequest, Viewnode,
  PartnerFolder, ViewnodeKind, Vognode, mk_editable_viewnode,
  mk_unknown_viewnode, mk_writeProtected_viewnode, viewnode_of};

use ego_tree::{NodeId, Tree};
use std::collections::{BTreeSet, HashMap};

fn objects (
  objects : &[DiffObject],
) -> BTreeSet<DiffObject> {
  objects . iter () . copied () . collect () }

fn facts (
  node  : Option<Sign>,
  rel   : Option<Sign>,
  ready : Option<&[DiffObject]>,
) -> ChangeFacts {
  ChangeFacts { presence : DiffPresence { node, rel },
                ready    : ready . map (objects),
                leaves   : None } }

fn request (
  ready : bool,
  on    : &[DiffObject],
) -> Option<ReadinessRequest> {
  Some ( ReadinessRequest { ready, objects : objects (on) } ) }

/// Render 'node', check its metadata against 'expected', parse it
/// back, and check that the result is 'node' again.
fn assert_round_trip (
  node     : Viewnode,
  expected : &str,
) {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
  let rendered : String = viewnode_to_string (&node, &config) . unwrap ();
  assert_eq! ( rendered, expected );
  let metadata : ViewnodeMetadata =
    parse_metadata_to_viewnodemd ( &format! ("(skg {})", rendered) )
    . unwrap ();
  let (parsed, error, _warning) : (MpViewnode, _, _) =
    viewnode_from_metadata (
      &metadata, node . title () . to_string (), node . body () . cloned () );
  assert_eq! ( error, None );
  assert_eq! ( Viewnode::try_from (parsed) . unwrap (), node ); }

fn with_facts (
  mut node : Viewnode,
  change_facts      : ChangeFacts,
  readiness_request : Option<ReadinessRequest>,
) -> Viewnode {
  node . change_facts = change_facts;
  node . readiness_request = readiness_request;
  node }

#[test]
fn a_vognode_round_trips_its_presence_readiness_and_request () {
  assert_round_trip (
    with_facts (
      mk_editable_viewnode (
        ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
      facts ( Some (Sign::Plus), Some (Sign::Minus),
              Some (&[DiffObject::Node, DiffObject::Rel]) ),
      request (true, &[DiffObject::All]) ),
    "(node (id a) (repo main) (diff node+ rel-) (ready node rel) (readyRequest all))" );
  // A readiness request is allowed on a write-protected occurrence.
  assert_round_trip (
    with_facts (
      mk_writeProtected_viewnode (
        ID::from ("b"), SkgrepoName::from ("main"), "B" . into (),
        AffectsParent::True ),
      facts ( None, Some (Sign::Plus), Some (&[]) ),
      request (false, &[DiffObject::Rel]) ),
    "(node (id b) (repo main) writeProtected (diff rel+) (ready) (unreadyRequest rel))" ); }

#[test]
fn phantoms_round_trip_their_change_facts () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Phantom (
        Phantom::Deleted ( PhantomDeleted {
          skgid : ID::from ("d"), home_skgrepo : SkgrepoName::from ("main"),
          title : "D" . into (), body : None, } )))),
      facts ( Some (Sign::Minus), None, Some (&[]) ),
      request (false, &[]) ),
    "(deleted (id d) (repo main) (diff node-) (ready) (unreadyRequest))" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Phantom (
        Phantom::Unknown ( PhantomUnknown {
          skgid : ID::from ("u"), relRepo : None, relRepo_request : None, } )))),
      facts ( None, Some (Sign::Plus), None ),
      request (true, &[]) ),
    "(unknown (id u) (diff rel+) (readyRequest))" ); }

#[test]
fn property_leaves_round_trip_their_change_facts () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Property ( Property::Alias {
        text : "an alias" . into (), relRepo : None, relRepo_request : None, } )),
      facts ( None, Some (Sign::Plus), Some (&[]) ),
      None ),
    "alias (diff rel+) (ready)" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Property ( Property::ID {
        skgid : ID::from ("x"), } )),
      facts ( None, Some (Sign::Minus), None ),
      request (true, &[]) ),
    "id (diff rel-) (readyRequest)" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Property ( Property::Flag {
        flag : Flag::NoSearchMatching, title : String::new (), body : None, } )),
      facts ( None, Some (Sign::Plus), None ),
      None ),
    &format! ( "(flag {}) (diff rel+)", Flag::NoSearchMatching . wire_name () )); }

/// '(leaves N OBJECT...)' is written beside '(ready ...)', and the
/// parser accepts and ignores it: the view derives it again.
#[test]
fn the_leaves_fact_renders_and_parses_back_to_nothing () {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
  let mut change_facts : ChangeFacts =
    facts ( None, Some (Sign::Plus), Some (&[DiffObject::Rel]) );
  change_facts . leaves = Some ( LeavesFact {
    count : 2, objects : objects (&[DiffObject::Node, DiffObject::Rel]) } );
  let node : Viewnode = with_facts (
    mk_editable_viewnode (
      ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
    change_facts, None );
  let rendered : String = viewnode_to_string (&node, &config) . unwrap ();
  assert_eq! ( rendered,
               "(node (id a) (repo main) (diff rel+) (ready rel) (leaves 2 node rel))" );
  let metadata : ViewnodeMetadata =
    parse_metadata_to_viewnodemd ( &format! ("(skg {})", rendered) ) . unwrap ();
  assert_eq! ( metadata . change_facts,
               facts ( None, Some (Sign::Plus), Some (&[DiffObject::Rel]) ));
  let marker : &str = "(skg (changesetMarker content) (leaves 3 all) (readyRequest all))";
  assert! ( parse_metadata_to_viewnodemd (marker) . is_ok (), "{}", marker ); }

#[test]
fn malformed_change_facts_are_refused () {
  for metadata in [
    "(skg (diff rel+))",                              // top level, not a property
    "(skg aliasFolder (diff rel+))",                  // a folder's only fact is '(ready)'
    "(skg aliasFolder (leaves 1))",
    "(skg (aliasFolder sideways))",                   // a folder's form is '(KIND changed)'
    "(skg (node (id a) (ready all)))",                // 'all' only in a request
    "(skg (node (id a) (diff node+ node-)))",         // an object twice
    "(skg (node (id a) (readyRequest) (unreadyRequest node)))", // two requests
    "(skg (node (id a) (readyRequest sideways)))", ] {
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }}

/// The facts of a folder holding 'members', as the renderer derives
/// them.
fn facts_of_folder_holding (
  members : &[Viewnode],
) -> Option<FolderChangeFacts> {
  let mut tree : Tree<Viewnode> = Tree::new (
    viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee) ));
  let folder : NodeId = tree . root () . id ();
  for member in members {
    tree . get_mut (folder) . unwrap () . append (member . clone ()); }
  folder_change_facts ( tree . get (folder) . unwrap () ) }

/// A folder is drawn as changed when some member's relationship to it
/// carries a sign, and is ready when each such member is: a member
/// whose '(leaves ...)' lists 'rel' when its '(ready ...)' names
/// 'rel'; any other when it carries a '(ready ...)'. A relationship
/// the member records nothing of leaves the folder unchanged.
#[test]
fn a_folder_is_ready_when_each_changed_member_is () {
  let rel_leaf = |ready : Option<&[DiffObject]>| -> ChangeFacts {
    let mut change_facts : ChangeFacts = facts ( None, Some (Sign::Plus), ready );
    change_facts . leaves = Some ( LeavesFact {
      count : 2, objects : objects (&[DiffObject::Node, DiffObject::Rel]) } );
    change_facts };
  let member = |id : &str| -> Viewnode {
    mk_writeProtected_viewnode (
      ID::from (id), SkgrepoName::from ("main"), id . into (),
      AffectsParent::True ) };
  let rel_ready : Viewnode = with_facts (
    member ("ready"), rel_leaf ( Some (&[DiffObject::Rel]) ), None );
  let rel_unready : Viewnode = with_facts (
    member ("unready"), rel_leaf ( Some (&[DiffObject::Node]) ), None );
  let unknown_ready : Viewnode = {
    let mut change_facts : ChangeFacts = facts ( None, Some (Sign::Plus), Some (&[]) );
    change_facts . leaves = Some ( LeavesFact { count : 1, objects : BTreeSet::new () } );
    with_facts ( mk_unknown_viewnode (ID::from ("unknown")), change_facts, None ) };
  let entry_unready : Viewnode = with_facts (
    viewnode_of ( ViewnodeKind::Property ( Property::Alias {
      text : "an alias" . into (), relRepo : None, relRepo_request : None } )),
    facts ( None, Some (Sign::Minus), None ),
    None );
  let unchanged : Viewnode = with_facts (
    member ("unchanged"), facts ( Some (Sign::Plus), None, Some (&[DiffObject::Node]) ),
    None );
  let ready   : Option<FolderChangeFacts> = Some ( FolderChangeFacts { ready : true } );
  let unready : Option<FolderChangeFacts> = Some ( FolderChangeFacts { ready : false } );
  assert_eq! ( facts_of_folder_holding (&[ rel_ready . clone () ]), ready );
  assert_eq! ( facts_of_folder_holding (&[ rel_unready . clone () ]), unready );
  assert_eq! ( facts_of_folder_holding (&[ unknown_ready . clone () ]), ready );
  assert_eq! ( facts_of_folder_holding (&[ entry_unready . clone () ]), unready );
  assert_eq! ( facts_of_folder_holding (&[ unchanged . clone () ]), None );
  assert_eq! ( facts_of_folder_holding (
                 &[ rel_ready . clone (), unknown_ready . clone (), unchanged ] ),
               ready );
  assert_eq! ( facts_of_folder_holding (
                 &[ rel_ready, rel_unready, unknown_ready, entry_unready ] ),
               unready ); }
