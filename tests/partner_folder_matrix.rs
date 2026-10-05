// cargo nextest run --test grouped_overrides -E 'test(partner_folder_matrix::)'
//
// The batched relationship-matrix target
// (TODO/DONE/full-schema/DONE/13_test-rel-matrix.org). ONE test function builds
// ONE database of mutually independent subgraphs (IDs prefixed by
// scenario), then runs the matrix scenarios serially against it: de
// novo omission of unrequested write-protected folders, and -- after explicit
// folder requests, per folder -- save after
// reorder, insertion of a non-member, deletion of a member, plus the
// editable folders' membership edits and the restricted-set omission.
// Scenario failures ACCUMULATE: every mismatch is collected and the
// test fails once at the end, so one broken folder does not mask the
// rest.
//
// DEVIATION from the plan (recorded in progress.org): instead of
// before.org/after.org file pairs, each scenario uses a fixture-local graph
// to render de novo. Rather than
// hand-author expected buffers, each scenario renders de novo, edits
// the real rendered text, saves, and asserts on the saved view, its
// warnings, and (for editable folders) the would-be disk lists -- the
// established style of partner_folder_order / partner_folder_warnings, which
// keeps the metadata always correct.

use std::error::Error;
use std::net::TcpStream;

use skg::skgrepo_sets::{
  ActiveSkgRepoSet, SkgRepoSetName, run_with_skgrepo_set_test_db};
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
    if ! buf . contains (needle) {
      self . record (scenario, format! (
        "expected to contain {:?}, in:\n{}", needle, buf )); } }
  fn want_absent (
    &mut self, scenario : &str, buf : &str, needle : &str) {
    if buf . contains (needle) {
      self . record (scenario, format! (
        "expected NOT to contain {:?}, in:\n{}", needle, buf )); } }
  fn want_before (
    &mut self, scenario : &str, buf : &str,
    first : &str, second : &str) {
    match ( buf . find (first), buf . find (second) ) {
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
      config, None, &[ ID::from (root) ], false ) ?;
  Ok (buf) }

async fn save (
  buf     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (), };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  update_from_and_rerender_buffer (
    &mut stream, buf, config, tantivy, graph, false,
    &Err ( String::new () ), &mut views_state ) . await }

async fn render_with_requested_relation_folders (
  root    : &str,
  relation : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<String, Box<dyn Error>> {
  let initial : String = render (root, config) . await ?;
  let request : String = initial . replace (
    "(affectsParent na)",
    &format! (
      "(affectsParent na) (viewRequests (folder {}))", relation ) );
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
/// is a demotion-to-independent rather than a removal. Built fresh
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
  intruder : &'static str, // a public non-member to park in the folder
}

const WRITE_PROTECTED_FOLDERS : [FolderSpec; 4] = [
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
  // The intruder is demoted to affectsParent=false in the rerendered view.
  { let intruder_after : &str = line_containing (
      &resp . saved_view, &format! ("(id {})", spec . intruder) );
    if ! intruder_after . contains ("(affectsParent false)") {
      fails . record (&scenario, format! (
        "intruder must be demoted to affectsParent=false: {}",
        intruder_after )); } }
  // ... and a warning says so.
  if ! resp . warnings . iter () . any ( |w|
      w . contains (&format! ("Repaired {}", spec . atom))
      && w . contains ("false")
      && w . contains (spec . intruder) ) {
    fails . record (&scenario, format! (
      "expected an affectsParent=false warning naming {}: {:?}",
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
  // ... with a restoration warning.
  if ! resp . warnings . iter () . any ( |w|
      w . contains (&format! ("Repaired {}", spec . atom))
      && w . contains ("restored")
      && w . contains (spec . member_a) ) {
    fails . record (&scenario, format! (
      "expected a restored-member warning naming {}: {:?}",
      spec . member_a, resp . warnings )); }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// De-novo omission of the four unrequested write-protected folders
//////////////////////////////////////////////////////////////

async fn denovo_omits_unrequested_write_protected_folders (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let s : &str = "denovo/write-protected-folders";
  let buf : String = render ("dn-owner", config) . await ?;
  for atom in ["subscriberFolder", "overriderFolder",
               "hiderFolder", "hiddenFolder"] {
    fails . want_absent (s, &buf, &format! ("(skg {})", atom)); }
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
      path_request_scenarios (
        &mut fails, config, tantivy, &graph) . await ?;

      fails . finish () } )) }

//////////////////////////////////////////////////////////////
// The Path view-request, '(viewRequests (roleTree ROLENAME))': graft the
// partners playing ROLENAME toward the node as inverted write-protected
// children with a '(birth roleGraft ROLENAME)' marker. One generic
// role-tree engine serves all nine roles; these cover the seven new
// ones (the container/mentioner roles keep their own golden tests).
// An absent relation field stays MSV::Unspecified on save, so these
// saves never disturb the graph the path then reads.
//////////////////////////////////////////////////////////////

async fn path_request_scenarios (
  fails : &mut Fails,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let req = | recorder : &str, role : &str, title : &str | -> String {
    format! (
      "* (skg (node (id {}) (repo public) (viewRequests (roleTree {})))) {}\n",
      recorder, role, title ) };
  // Each row: (scenario, recorder, role, partner-id, birth-span-fragment).
  // Since uniform-heralds, the grafted partner no longer carries the
  // old (birth roleGraft ROLE) marker nor a parent-relative viewStat;
  // instead its relationship TO its viewparent (the origin) shows as the
  // birth (black-on-white) token inside its (rels ...) spans.
  // E.g. the 'overridden' partner is overridden BY the origin -> its
  // parent (a) overrides it -> token "aO", rendered as the ancestor
  // letter 'a' (yellow) then the birth 'O' (white). The 'overrider'
  // partner overrides its parent among others -> "O2a". The birth token's
  // spans appear consecutively inside (rels ...) on the partner's line.
  // Each row's 5th field is the SEMANTIC relationship the grafted
  // partner has to the origin (its viewparent, generation 1), which is
  // its birth. The direction (in vs out) distinguishes e.g.
  // overridden (the origin overrides it) from overrider (it overrides
  // the origin, among others).
  let sharing : [(&str, &str, &str, &str, &str); 6] = [
    ("path/overridden", "wOvr-owner",     "overridden", "wOvr-a",         "(overrides (in 1 (ancestors 1)))"),
    ("path/overrider",  "wOvr-a",         "overrider",  "wOvr-owner",     "(overrides (out 2 (ancestors 1)))"),
    ("path/subscribee", "wSub-owner",     "subscribee", "wSub-a",         "(subscribesTo (in 1 (ancestors 1)))"),
    ("path/subscriber", "wSub-a",         "subscriber", "wSub-owner",     "(subscribesTo (out 3 (ancestors 1)))"),
    ("path/hidden",     "roHidden-owner", "hidden",     "roHidden-a",     "(hidesFromSubs (in 1 (ancestors 1)))"),
    ("path/hider",      "roHidden-a",     "hider",      "roHidden-owner", "(hidesFromSubs (out 2 (ancestors 1)))"),
  ];
  for (s, recorder, role, partner, birth_rel) in sharing {
    let _ = role;
    let resp : SaveResponse = save (
      &req (recorder, role, recorder), // title == recorder (matches its disk title)
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view,
                           &format! ("(id {})", partner) );
    // The matching birth relationship, on the grafted partner's own line.
    match resp . saved_view . lines ()
            . find ( |l| l . contains (&format! ("(id {})", partner)) ) {
      Some (line) => if ! line . contains (birth_rel) {
        fails . record (s, format! (
          "partner {} missing birth relation {:?}: {}",
          partner, birth_rel, line )); },
      None => {}, // already recorded by want_contains above
    } }
  { // (roleTree mentioned) on a node whose TITLE carries [[id:pathLink-dst]]:
    // the dest node is grafted. (mentioner is the existing mentionerward
    // golden; this is its mirror.)
    let s : &str = "path/mentioned";
    let resp : SaveResponse = save (
      &req ("pathLink-src", "mentioned", "[[id:pathLink-dst][to dst]]"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(id pathLink-dst)");
    // The grafted dest is born of a single inbound link (from the
    // origin), no outbound.
    fails . want_contains (
      s, &resp . saved_view, "(linksTo (in 1 (ancestors 1) (substantive 0)))" ); }
  { // Self-referential fixture: a node that links to ITSELF. A
    // non-container path role is cycle-guarded and needs NO view-root
    // special-case (unlike containerward) -- the build must not panic,
    // and the node reappears as its own grafted dest.
    let s : &str = "path/self-referential-no-special-case";
    let resp : SaveResponse = save (
      &req ("pathSelf", "mentioned", "[[id:pathSelf][to self]]"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(id pathSelf)"); }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// The Folder view-request, '(viewRequests (folder RELNAME))': build BOTH
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
    // overridden by nothing: the EDITABLE overriddenFolder appears EMPTY
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
    // the overriddenFolder appears POPULATED with both.
    let s : &str = "folder-request/overrides-populated";
    let resp : SaveResponse = save (
      &request_buf ("wOvr-owner", "overrides"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg overriddenFolder)");
    fails . want_contains (s, &resp . saved_view, "(id wOvr-a)");
    fails . want_contains (s, &resp . saved_view, "(id wOvr-b)"); }
  { // (folder subscribesTo) on wSub-owner: subscribeeFolder POPULATED (a,b,c).
    let s : &str = "folder-request/subscribes-populated";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "subscribesTo"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg subscribeeFolder)");
    fails . want_contains (s, &resp . saved_view, "(id wSub-a)"); }
  { // (folder hidesFromSubs) on wSub-owner, which neither hides nor is hidden:
    // both sides write-protected and empty, so NOTHING appears.
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
  active : Option<&ActiveSkgRepoSet>,
) -> Result<Vec<NodeInstruction>, Box<dyn Error>> {
  let (_vf, plan, _warnings) =
    buffer_to_validated_saveplan (buf, config, active)  ?;
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
// Omission under a restricted skgrepo-set: an inactive-repo member
// beside an active one is omitted from the render (no placeholder);
// for the editable folder, the save weaves the omitted member back.
//////////////////////////////////////////////////////////////

async fn omission_scenarios (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let active : ActiveSkgRepoSet =
    ActiveSkgRepoSet::named (config, SkgRepoSetName::from ("public")) ?;
  { // write-protected subscriberFolder: inactive omitted, active shown
    let s : &str = "subscriberFolder/omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view_with_skgrepo_set (
        config, None, &[ID::from ("omSub-owner")],
        false, &active ) ?;
    fails . want_absent (s, &buf, "subscriberFolder");
    fails . want_absent (s, &buf, "omSub-active");
    fails . want_absent (s, &buf, "omSub-inactive"); }
  { // editable subscribeeFolder: inactive omitted from render, but the
    // restricted save weaves it back into subscribesTo.
    let s : &str = "subscribeeFolder/omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view_with_skgrepo_set (
        config, None, &[ID::from ("omWsub-owner")],
        false, &active ) ?;
    fails . want_contains (s, &buf, "(id omWsub-active)");
    fails . want_absent (s, &buf, "omWsub-inactive");
    // Delete the only VISIBLE subscribee and save under the restricted
    // set: the weave must still preserve the invisible omWsub-inactive
    // (a restricted save cannot delete what it cannot see), while the
    // deleted active member is removed. So subscribesTo = [inactive].
    let active_line : String =
      line_containing (&buf, "(id omWsub-active)") . to_string ();
    let edited : String =
      buf . replace (&format! ("{}\n", active_line), "");
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, Some (&active)) . await ?;
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
// Function 2 (its own database, since it must observe a REJECTED
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
        "a rejected save must not write mono-r2's override edge: {:?}",
        r2 . overrides );
      Ok (( )) } )) }
