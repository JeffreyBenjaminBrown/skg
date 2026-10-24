// cargo nextest run --test grouped_overrides -E 'test(partner_folder_matrix::)'
//
// The batched relationship-matrix target
// ('docs/sharing-model.org', "The write-protected folders: who
// subscribes to, overrides, or hides this node"). One test function builds
// one database of mutually independent subgraphs (IDs prefixed by
// scenario), then runs the matrix scenarios serially against it: de
// novo omission of unrequested write-protected folders, and -- after explicit
// folder requests, per folder -- save after
// reorder, insertion of a non-member, deletion of a member, plus the
// editable folders' membership edits and the restricted-set omission.
// Scenario failures accumulate: every mismatch is collected and the
// test fails once at the end, so one broken folder does not mask the
// rest.
//
// Instead of before.org/after.org file pairs or hand-authored expected
// buffers, each scenario uses a fixture-local graph, renders de novo, edits
// the real rendered text, saves, and asserts on the saved view, its
// warnings, and (for editable folders) the would-be disk lists -- the
// established style of partner_folder_order / partner_folder_warnings, which
// keeps the metadata always correct.

use skg::metadata_canon::without_viewnode_ids;
use std::error::Error;
use std::net::TcpStream;

use skg::skgrepo_sets::{
  SkgrepoRestriction, SkgrepoSetName, run_with_skgrepo_set_test_db};
use skg::test_utils::graph_handle_from_config;
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::{
  multi_root_view, multi_root_view_with_skgrepo_set};
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, MSV, SkgConfig, TantivyIndex, members_of, members_msv};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::graphnode_by_skgid;
use skg::from_text::buffer_to_validated_saveplan;
use skg::types::errors::{SaveError, BufferValidationError};
use skg::types::viewnode::Viewnode;
use ego_tree::Tree;
use indoc::indoc;

//////////////////////////////////////////////////////////////
// Accumulating-failure harness
//////////////////////////////////////////////////////////////

struct Fails { msgs : Vec<String> }
impl Fails {
  fn new () -> Fails { Fails { msgs : Vec::new () } }
  fn record (&mut self, scenario : &str, msg : String) {
    self . msgs . push (format! ("[{}] {}", scenario, msg)); }
  fn want_contains (
    &mut self, scenario : &str, buf : &str, needle : &str) {
    if ! without_viewnode_ids (buf) . contains (needle) {
      self . record (scenario, format! (
        "expected to contain {:?}, in:\n{}", needle, buf )); } }
  fn want_absent (
    &mut self, scenario : &str, buf : &str, needle : &str) {
    if without_viewnode_ids (buf) . contains (needle) {
      self . record (scenario, format! (
        "expected NOT to contain {:?}, in:\n{}", needle, buf )); } }
  fn want_before (
    &mut self, scenario : &str, buf : &str,
    first : &str, second : &str) {
    let buf_without_viewnode_ids : String = without_viewnode_ids (buf);
    match ( buf_without_viewnode_ids . find (first), buf_without_viewnode_ids . find (second) ) {
      ( Some (i), Some (j) ) => if ! ( i < j ) {
        self . record (scenario, format! (
          "expected {:?} before {:?}, in:\n{}", first, second, buf )); },
      _ => self . record (scenario, format! (
        "want_before: {:?} and {:?} not both present, in:\n{}",
        first, second, buf )), } }
  fn finish (self) -> Result<(), Box<dyn Error>> {
    if self . msgs . is_empty () { Ok (( )) }
    else { Err (format! (
      "{} matrix scenario failure(s):\n\n{}",
      self . msgs . len (), self . msgs . join ("\n\n") ) . into ()) } }
}

//////////////////////////////////////////////////////////////
// Small helpers
//////////////////////////////////////////////////////////////

fn saved_node_by_skgid<'a> (
  instructions : &'a [NodeInstruction], skgid : &str,
) -> Option<&'a Graphnode> {
  for instruction in instructions {
    if let NodeInstruction::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (skgid) { return Some (node); }}}
  None }

fn line_containing<'a> ( buf : &'a str, fragment : &str ) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ( "no line contains {:?} in:\n{}", fragment, buf )) }

async fn render (
  root    : &str,
  config : &SkgConfig,
) -> Result<String, Box<dyn Error>> {
  let (buf, _pids, _tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (
      config, None, &[ ID::from (root) ] ) ?;
  Ok (buf) }

async fn save (
  buf     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None, };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  update_from_and_rerender_buffer (
    &mut stream, buf, config, tantivy, graph,
    &Err ( String::new () ), &mut views_state ) . await }

async fn render_with_requested_relation_folders (
  root    : &str,
  relation : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<String, Box<dyn Error>> {
  let initial : String = render (root, config) . await ?;
  let request : String = initial . replacen (
    "(node ",
    &format! (
      "(node (viewRequests (folder {}))", relation ), 1 );
  let response : SaveResponse =
    save (&request, config, tantivy, graph) . await ?;
  if ! response . errors . is_empty () {
    return Err (format! (
      "{} folder request for {} failed: {:?}",
      relation, root, response . errors )
      . into ()); }
  Ok (response . saved_view)
}

/// Swap the two whole lines that carry these metadata fragments.
fn swap_lines ( buf : &str, a : &str, b : &str ) -> String {
  let a_line : String = line_containing (buf, a) . to_string ();
  let b_line : String = line_containing (buf, b) . to_string ();
  buf . replace ( &a_line, "\u{0}SWAP\u{0}" )
      . replace ( &b_line, &a_line )
      . replace ( "\u{0}SWAP\u{0}", &b_line ) }

/// Fabricate an intruder member line (an editable public leaf) at the
/// member's indentation, plus a child one level deeper, so the repair
/// makes it a non-member rather than removing it. Built fresh
/// (not cloned from a member line) so it never inherits a foreign
/// skgrepo -- the overriderFolder's members are foreign.
fn intruder_with_child (
  member_line : &str,
  intruder_skgid : &str,
) -> (String, String) {
  let stars : usize =
    member_line . chars () . take_while ( |c| *c == '*' ) . count ();
  let line : String = format! (
    "{} (skg (node (id {}) (repo public))) {}",
    "*" . repeat (stars), intruder_skgid, intruder_skgid );
  let child : String =
    format! ( "{} {}-child", "*" . repeat (stars + 1), intruder_skgid );
  ( line, child ) }

//////////////////////////////////////////////////////////////
// Write-protected folder spec and per-behavior scenario helpers
//////////////////////////////////////////////////////////////

struct FolderSpec {
  atom     : &'static str, // e.g. "subscriberFolder"
  relation : &'static str,
  recorder : &'static str,
  member_a : &'static str, // sorts before member_b
  member_b : &'static str,
  intruder : &'static str, // a public non-member to place in the folder
}

const WRITE_PROTECTED_FOLDERS : [FolderSpec; 6] = [
  FolderSpec { atom : "subscriberFolder", recorder : "roSub-owner",
            relation : "subscribesTo",
            member_a : "roSub-a", member_b : "roSub-b",
            intruder : "roSub-x" },
  FolderSpec { atom : "overriderFolder", recorder : "roOvr-owner",
            relation : "overrides",
            member_a : "roOvr-a", member_b : "roOvr-b",
            intruder : "roOvr-x" },
  FolderSpec { atom : "hiderFolder", recorder : "roHider-owner",
            relation : "hidesFromSubs",
            member_a : "roHider-a", member_b : "roHider-b",
            intruder : "roHider-x" },
  FolderSpec { atom : "hiddenFolder", recorder : "roHidden-owner",
            relation : "hidesFromSubs",
            member_a : "roHidden-a", member_b : "roHidden-b",
            intruder : "roHidden-x" },
  FolderSpec { atom : "containerFolder", recorder : "roCont-owner",
            relation : "containers",
            member_a : "roCont-a", member_b : "roCont-b",
            intruder : "roCont-x" },
  FolderSpec { atom : "mentionerFolder", recorder : "roMent-owner",
            relation : "mentioners",
            member_a : "roMent-a", member_b : "roMent-b",
            intruder : "roMent-x" },
];

async fn write_protected_reorder (
  fails : &mut Fails, spec : &FolderSpec,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let scenario : String = format! ("{}/reorder", spec . atom);
  let buf : String = render_with_requested_relation_folders (
    spec . recorder, spec . relation, config, tantivy, graph ) . await ?;
  let swapped : String = swap_lines (
    &buf,
    &format! ("(id {})", spec . member_a),
    &format! ("(id {})", spec . member_b) );
  let resp : SaveResponse = match save (
    &swapped, config, tantivy, graph) . await {
    Ok (r) => r,
    Err (e) => { fails . record (&scenario, format! (
      "save errored: {}", e)); return Ok (( )); } };
  if ! resp . errors . is_empty () {
    fails . record (&scenario, format! (
      "save reported errors: {:?}", resp . errors)); return Ok (( )); }
  // Order preserved view-locally: member_b now before member_a.
  fails . want_before (
    &scenario, &resp . saved_view,
    &format! ("(id {})", spec . member_b),
    &format! ("(id {})", spec . member_a) );
  // Reordering is not a repair.
  if resp . warnings . iter () . any (
    |w| w . contains (&format! ("Repaired {}", spec . atom)) ) {
    fails . record (&scenario, format! (
      "reorder should not warn of a repair: {:?}", resp . warnings)); }
  Ok (( )) }

async fn write_protected_insert (
  fails : &mut Fails, spec : &FolderSpec,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let scenario : String = format! ("{}/insert", spec . atom);
  let buf : String = render_with_requested_relation_folders (
    spec . recorder, spec . relation, config, tantivy, graph ) . await ?;
  let member_b_line : String =
    line_containing (&buf, &format! ("(id {})", spec . member_b))
    . to_string ();
  let (intruder_line, child_line) : (String, String) =
    intruder_with_child ( &member_b_line, spec . intruder );
  let edited : String = buf . replace (
    &member_b_line,
    &format! ("{}\n{}\n{}", member_b_line, intruder_line, child_line) );
  let resp : SaveResponse = match save (
    &edited, config, tantivy, graph) . await {
    Ok (r) => r,
    Err (e) => { fails . record (&scenario, format! (
      "save errored: {}", e)); return Ok (( )); } };
  if ! resp . errors . is_empty () {
    fails . record (&scenario, format! (
      "save reported errors: {:?}", resp . errors)); return Ok (( )); }
  // The intruder is made a non-member (nonMember) in the rerendered view.
  { let intruder_after : &str = line_containing (
      &resp . saved_view, &format! ("(id {})", spec . intruder) );
    if ! intruder_after . contains ("nonMember") {
      fails . record (&scenario, format! (
        "intruder must be made a non-member (nonMember): {}",
        intruder_after )); } }
  // ... and a warning says so.
  if ! resp . warnings . iter () . any ( |w|
      w . contains (&format! ("Repaired {}", spec . atom))
      && w . contains ("non-members, since they are not members")
      && w . contains (spec . intruder) ) {
    fails . record (&scenario, format! (
      "expected a made-a-non-member warning naming {}: {:?}",
      spec . intruder, resp . warnings )); }
  Ok (( )) }

async fn write_protected_delete (
  fails : &mut Fails, spec : &FolderSpec,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let scenario : String = format! ("{}/delete", spec . atom);
  let buf : String = render_with_requested_relation_folders (
    spec . recorder, spec . relation, config, tantivy, graph ) . await ?;
  let member_a_line : String =
    line_containing (&buf, &format! ("(id {})", spec . member_a))
    . to_string ();
  let edited : String =
    buf . replace ( &format! ("{}\n", member_a_line), "" );
  let resp : SaveResponse = match save (
    &edited, config, tantivy, graph) . await {
    Ok (r) => r,
    Err (e) => { fails . record (&scenario, format! (
      "save errored: {}", e)); return Ok (( )); } };
  if ! resp . errors . is_empty () {
    fails . record (&scenario, format! (
      "save reported errors: {:?}", resp . errors)); return Ok (( )); }
  // The deleted member respawns (write-protected set).
  fails . want_contains (
    &scenario, &resp . saved_view,
    &format! ("(id {})", spec . member_a) );
  // ... with a reinstatement warning.
  if ! resp . warnings . iter () . any ( |w|
      w . contains (&format! ("Repaired {}", spec . atom))
      && w . contains ("reinstated")
      && w . contains (spec . member_a) ) {
    fails . record (&scenario, format! (
      "expected a reinstated-member warning naming {}: {:?}",
      spec . member_a, resp . warnings )); }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// De-novo omission of the six unrequested write-protected folders
//////////////////////////////////////////////////////////////

async fn denovo_omits_unrequested_write_protected_folders (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let s : &str = "denovo/write-protected-folders";
  let buf : String = render ("dn-owner", config) . await ?;
  for atom in ["subscriberFolder", "overriderFolder",
               "hiderFolder", "hiddenFolder", "mentionerFolder"] {
    fails . want_absent (s, &buf, &format! ("(skg {})", atom)); }
  // But a root shows its containers folder, with the containerward
  // chain under its members.
  fails . want_contains (s, &buf, "** (skg containerFolder)");
  Ok (( )) }

//////////////////////////////////////////////////////////////
// The matrix test
//////////////////////////////////////////////////////////////

#[test]
fn relationship_matrix
  () -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-partner-folder-matrix",
    "tests/partner_folder_matrix/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-partner-folder-matrix",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let mut fails : Fails = Fails::new ();

      denovo_omits_unrequested_write_protected_folders (
        &mut fails, config ) . await ?;
      for spec in &WRITE_PROTECTED_FOLDERS {
        write_protected_reorder (
          &mut fails, spec, config, tantivy, &graph) . await ?;
        write_protected_insert (
          &mut fails, spec, config, tantivy, &graph) . await ?;
        write_protected_delete (
          &mut fails, spec, config, tantivy, &graph) . await ?;
      }
      editable_subscribeeFolder (&mut fails, config) . await ?;
      editable_overriddenFolder (
        &mut fails, config, tantivy, &graph ) . await ?;
      hiddenFolder_delete_does_not_unhide (
        &mut fails, config, tantivy, &graph ) . await ?;
      omission_scenarios (&mut fails, config) . await ?;
      folder_request_scenarios (
        &mut fails, config, tantivy, &graph) . await ?;
      roleTree_request_is_refused (
        &mut fails, config, tantivy, &graph) . await ?;

      fails . finish () } )) }

//////////////////////////////////////////////////////////////
// No role-tree request: a partner stands only in a partner folder of
// its role, which a folder request draws ('folder_request_scenarios').
// The server refuses '(viewRequests (roleTree ROLENAME))' as an
// unknown view request, for every ROLENAME.
//////////////////////////////////////////////////////////////

async fn roleTree_request_is_refused (
  fails : &mut Fails,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  for role in [ "container", "mentioner", "mentioned", "overridden",
                "subscriber" ] {
    let s : String = format! ("roleTree/{}", role);
    let buffer : String = format! (
      "* (skg (node (id wOvr-owner) (repo public) (viewRequests (roleTree {})))) wOvr-owner\n",
      role );
    match save (&buffer, config, tantivy, graph) . await {
      Ok (resp) if resp . errors . is_empty () =>
        fails . record (&s, format! (
          "expected a refusal, got the view:\n{}", resp . saved_view )),
      Ok (resp) if ! resp . errors . iter () . any ( |error|
          error . contains ("Unknown view request form: (roleTree ...)") ) =>
        fails . record (&s, format! (
          "expected an unknown-request refusal, got {:?}", resp . errors )),
      Ok (_) => {},
      Err (error) if ! error . to_string () . contains (
          "Unknown view request form: (roleTree ...)") =>
        fails . record (&s, format! (
          "expected an unknown-request refusal, got {}", error )),
      Err (_) => {}, }}
  Ok (( )) }

//////////////////////////////////////////////////////////////
// The Folder view-request, '(viewRequests (folder RELNAME))': build both
// folders of the relation, the writable one even when empty (decision A).
// Save a minimal editable buffer carrying just the request and assert
// on the rerendered view. An absent editable folder means "no opinion"
// (MSV::Unspecified, filled from disk), so these saves never wipe the
// relation -- the folders come back populated/empty in the rerender.
//////////////////////////////////////////////////////////////

async fn folder_request_scenarios (
  fails : &mut Fails,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let request_buf = | recorder : &str, rel : &str | -> String {
    format! (
      "* (skg (node (id {}) (repo public) (viewRequests (folder {})))) {}\n",
      recorder, rel, recorder ) };
  { // (folder overrides) on wSub-owner, which overrides nothing and is
    // overridden by nothing: the editable overriddenFolder appears empty
    // (the "add an override here" surface); the write-protected overriderFolder
    // does not appear (empty write-protected folders are pruned).
    let s : &str = "folder-request/overrides-empty";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "overrides"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg overriddenFolder)");
    fails . want_absent  (s, &resp . saved_view, "overriderFolder"); }
  { // (folder overrides) on wOvr-owner, which overrides wOvr-a and wOvr-b:
    // the overriddenFolder appears populated with both.
    let s : &str = "folder-request/overrides-populated";
    let resp : SaveResponse = save (
      &request_buf ("wOvr-owner", "overrides"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg overriddenFolder)");
    fails . want_contains (s, &resp . saved_view, "(id wOvr-a)");
    fails . want_contains (s, &resp . saved_view, "(id wOvr-b)"); }
  { // (folder subscribesTo) on wSub-owner: subscribeeFolder populated (a,b,c).
    let s : &str = "folder-request/subscribes-populated";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "subscribesTo"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg subscribeeFolder)");
    fails . want_contains (s, &resp . saved_view, "(id wSub-a)"); }
  { // (folder hidesFromSubs) on wSub-owner, which neither hides nor is hidden:
    // both sides write-protected and empty, so nothing appears.
    let s : &str = "folder-request/hides-empty";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "hidesFromSubs"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_absent (s, &resp . saved_view, "hiderFolder");
    fails . want_absent (s, &resp . saved_view, "hiddenFolder"); }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Editable folders (subscribeeFolder, overriddenFolder): the membership
// edits land on disk. Checked through buffer_to_validated_saveplan,
// which builds (but does not write) the plan, so we read the
// would-be Graphnode for the recorder.
//////////////////////////////////////////////////////////////

async fn saveplan_nodes (
  buf    : &str,
  config : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<Vec<NodeInstruction>, Box<dyn Error>> {
  let (_vf, plan, _warnings) =
    buffer_to_validated_saveplan (buf, config, restriction)  ?;
  Ok (plan . node_instructions) }

/// A fresh write-protected public member line at the given indentation.
fn member_line ( stars : usize, skgid : &str ) -> String {
  format! ( "{} (skg (node (id {}) (repo public) writeProtected)) {}",
            "*" . repeat (stars), skgid, skgid ) }

fn folder_member_stars ( buf : &str, any_member_fragment : &str ) -> usize {
  line_containing (buf, any_member_fragment)
    . chars () . take_while ( |c| *c == '*' ) . count () }

async fn editable_subscribeeFolder (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let buf : String = render ("wSub-owner", config) . await ?;
  let stars : usize = folder_member_stars (&buf, "(id wSub-a)");
  { // reorder: [a,b,c] -> swap a,c -> [c,b,a]
    let s : &str = "subscribeeFolder/reorder";
    let reordered : String =
      swap_lines (&buf, "(id wSub-a)", "(id wSub-c)");
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&reordered, config, None) . await ?;
    match saved_node_by_skgid (&nodes, "wSub-owner") {
      Some (n) => if members_msv (&n . subscribesTo) != MSV::Specified (vec![
          ID::from ("wSub-c"), ID::from ("wSub-b"), ID::from ("wSub-a")]) {
        fails . record (s, format! (
          "reordered subscribesTo wrong: {:?}", n . subscribesTo)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  { // delete one: remove b -> [a,c]
    let s : &str = "subscribeeFolder/delete";
    let b_line : String =
      line_containing (&buf, "(id wSub-b)") . to_string ();
    let edited : String = buf . replace (&format! ("{}\n", b_line), "");
    let nodes  : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, None) . await ?;
    match saved_node_by_skgid (&nodes, "wSub-owner") {
      Some (n) => if members_msv (&n . subscribesTo) != MSV::Specified (vec![
          ID::from ("wSub-a"), ID::from ("wSub-c")]) {
        fails . record (s, format! (
          "after delete, subscribesTo wrong: {:?}", n . subscribesTo)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  { // insert a member: add d -> [a,b,c,d]
    let s : &str = "subscribeeFolder/insert";
    let c_line : String =
      line_containing (&buf, "(id wSub-c)") . to_string ();
    let edited : String = buf . replace (
      &c_line, &format! ("{}\n{}", c_line, member_line (stars, "wSub-d")) );
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, None) . await ?;
    match saved_node_by_skgid (&nodes, "wSub-owner") {
      Some (n) => if members_msv (&n . subscribesTo) != MSV::Specified (vec![
          ID::from ("wSub-a"), ID::from ("wSub-b"),
          ID::from ("wSub-c"), ID::from ("wSub-d")]) {
        fails . record (s, format! (
          "after insert, subscribesTo wrong: {:?}", n . subscribesTo)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  Ok (( )) }

fn override_set ( n : &Graphnode ) -> Vec<ID> {
  match &n . overrides {
    MSV::Specified (skgids) => { let mut v = members_of (skgids); v . sort (); v }
    MSV::Unspecified => Vec::new (), } }

async fn editable_overriddenFolder (
  fails : &mut Fails,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let buf : String = render_with_requested_relation_folders (
    "wOvr-owner", "overrides", config, tantivy, graph ) . await ?;
  let stars : usize = folder_member_stars (&buf, "(id wOvr-a)");
  { // reorder is harmless: order-free set unchanged
    let s : &str = "overriddenFolder/reorder";
    let reordered : String =
      swap_lines (&buf, "(id wOvr-a)", "(id wOvr-b)");
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&reordered, config, None) . await ?;
    match saved_node_by_skgid (&nodes, "wOvr-owner") {
      Some (n) => if override_set (n) != vec![
          ID::from ("wOvr-a"), ID::from ("wOvr-b")] {
        fails . record (s, format! (
          "reorder changed the override set: {:?}", n . overrides)); },
      None => fails . record (s, "no SaveNode for wOvr-owner" . into ()), } }
  { // delete one: remove a -> [b]
    let s : &str = "overriddenFolder/delete";
    let a_line : String =
      line_containing (&buf, "(id wOvr-a)") . to_string ();
    let edited : String = buf . replace (&format! ("{}\n", a_line), "");
    let nodes  : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, None) . await ?;
    match saved_node_by_skgid (&nodes, "wOvr-owner") {
      Some (n) => if override_set (n) != vec![ID::from ("wOvr-b")] {
        fails . record (s, format! (
          "after delete, override set wrong: {:?}", n . overrides)); },
      None => fails . record (s, "no SaveNode for wOvr-owner" . into ()), } }
  { // insert a member: add c -> {a,b,c}
    let s : &str = "overriddenFolder/insert";
    let b_line : String =
      line_containing (&buf, "(id wOvr-b)") . to_string ();
    let edited : String = buf . replace (
      &b_line, &format! ("{}\n{}", b_line, member_line (stars, "wOvr-c")) );
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, None) . await ?;
    match saved_node_by_skgid (&nodes, "wOvr-owner") {
      Some (n) => if override_set (n) != vec![
          ID::from ("wOvr-a"), ID::from ("wOvr-b"), ID::from ("wOvr-c")] {
        fails . record (s, format! (
          "after insert, override set wrong: {:?}", n . overrides)); },
      None => fails . record (s, "no SaveNode for wOvr-owner" . into ()), } }
  Ok (( )) }

/// Deleting a member from the write-protected hiddenFolder must not unhide it
/// on disk: the recorder's hidesFromSubs is never read
/// from the folder, so the deleted member stays hidden. (The view-level
/// twin -- respawn in the saved view -- is the hiddenFolder case of
/// write_protected_delete; the extraction-seam twin is in commit 1.)
async fn hiddenFolder_delete_does_not_unhide (
  fails : &mut Fails,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let s : &str = "hiddenFolder/no-unhide-on-disk";
  let buf : String = render_with_requested_relation_folders (
    "roHidden-owner", "hidesFromSubs", config, tantivy, graph ) . await ?;
  let a_line : String =
    line_containing (&buf, "(id roHidden-a)") . to_string ();
  let edited : String = buf . replace (&format! ("{}\n", a_line), "");
  let nodes  : Vec<NodeInstruction> =
    saveplan_nodes (&edited, config, None) . await ?;
  // Either the recorder is a no-op (absent from the plan), or its SaveNode
  // still hides roHidden-a; in no case is roHidden-a unhidden.
  if let Some (n) = saved_node_by_skgid (&nodes, "roHidden-owner") {
    let hides : Vec<ID> = match &n . hidesFromSubs {
      MSV::Specified (skgids) => members_of (skgids),
      MSV::Unspecified => Vec::new (), };
    if ! hides . is_empty () && ! hides . contains (&ID::from ("roHidden-a")) {
      fails . record (s, format! (
        "deleting from hiddenFolder unhid roHidden-a: hides = {:?}", hides)); } }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Omission under a restricted skgrepo-set: a restricted-skgrepo member
// beside an unrestricted one is omitted from the render (no placeholder);
// for the editable folder, the save weaves the omitted member back.
//////////////////////////////////////////////////////////////

async fn omission_scenarios (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoSetName::from ("public")) ?;
  { // write-protected subscriberFolder: restricted omitted, unrestricted shown
    let s : &str = "subscriberFolder/omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view_with_skgrepo_set (
        config, None, &[ID::from ("omSub-owner")], &restriction ) ?;
    fails . want_absent (s, &buf, "subscriberFolder");
    fails . want_absent (s, &buf, "omSub-active");
    fails . want_absent (s, &buf, "omSub-inactive"); }
  { // editable subscribeeFolder: restricted omitted from render, but the
    // restricted save weaves it back into subscribesTo.
    let s : &str = "subscribeeFolder/omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view_with_skgrepo_set (
        config, None, &[ID::from ("omWsub-owner")], &restriction ) ?;
    fails . want_contains (s, &buf, "(id omWsub-active)");
    fails . want_absent (s, &buf, "omWsub-inactive");
    // Delete the only visible subscribee and save under the restricted
    // set: the weave must still preserve the invisible omWsub-inactive
    // (a restricted save cannot delete what it cannot see), while the
    // deleted unrestricted member is removed. So subscribesTo = [restricted].
    let unrestricted_line : String =
      line_containing (&buf, "(id omWsub-active)") . to_string ();
    let edited : String =
      buf . replace (&format! ("{}\n", unrestricted_line), "");
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, Some (&restriction)) . await ?;
    match saved_node_by_skgid (&nodes, "omWsub-owner") {
      Some (n) => { let subs : Vec<ID> = match &n . subscribesTo {
          MSV::Specified (skgids) => members_of (skgids),
          MSV::Unspecified => Vec::new (), };
        if ! subs . contains (&ID::from ("omWsub-inactive")) {
          fails . record (s, format! (
            "restricted save dropped the invisible subscribee: {:?}",
            subs)); }
        if subs . contains (&ID::from ("omWsub-active")) {
          fails . record (s, format! (
            "deleted visible subscribee should be gone: {:?}", subs)); } }
      None => fails . record (s,
        "deleting the visible subscribee should change the recorder" . into ()), } }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Function 2 (its own database, since it must observe a rejected
// save): the buffer-level half of "two owned overriders rejected
// at save". A save adds a second owned overrider for an
// already-overridden target via an overriddenFolder; the save is
// rejected with the monogamy error and disk is unchanged.
//
// The override-invariant check reads the save's own graph handle, and
// the buffer is hand-written rather than rendered.
//////////////////////////////////////////////////////////////

#[test]
fn buffer_save_rejects_second_owned_overrider
  () -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-partner-folder-matrix-monogamy",
    "tests/partner_folder_matrix/fixtures-monogamy/skgconfig.toml",
    "/tmp/tantivy-test-partner-folder-matrix-monogamy",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      // mono-r1 already overrides mono-target on disk; this buffer
      // makes mono-r2 override it too.
      let buffer : &str = indoc! {"
        * (skg (node (id mono-r2) (repo public))) mono-r2
        ** (skg overriddenFolder)
        *** (skg (node (id mono-target) (repo public) writeProtected)) mono-target
      "};
      let result : Result<SaveResponse, Box<dyn Error>> =
        save (buffer, config, tantivy, &graph) . await;
      let err : Box<dyn Error> = match result {
        Ok (_)  => panic! (
          "save must be rejected by monogamy, but it succeeded"),
        Err (e) => e, };
      let save_error : &SaveError =
        err . downcast_ref::<SaveError> ()
        . unwrap_or_else (
          || panic! ("expected a SaveError, got: {}", err) );
      assert! ( matches! (
        save_error,
        SaveError::BufferValidationErrors { errors, .. }
          if errors . iter () . any (
            |e| matches! (
              e, BufferValidationError::OverrideInvariantViolation (_) )) ),
        "expected an override-invariant violation, got {:?}", save_error );
      // Disk unchanged: the override-invariant check runs before the
      // filesystem write, so mono-r2 still overrides nothing.
      let r2 : Graphnode =
        graphnode_by_skgid (
          &skg::test_utils::graph_handle_from_config (config)? . load_full (),
          config, &ID::from ("mono-r2") ) ?;
      let overrides_empty : bool = match &r2 . overrides {
        MSV::Unspecified       => true,
        MSV::Specified (skgids)   => skgids . is_empty (), };
      assert! ( overrides_empty,
        "a rejected save must not write mono-r2's override relationship: {:?}",
        r2 . overrides );
      Ok (( )) } )) }
