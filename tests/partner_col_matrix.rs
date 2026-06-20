// cargo nextest run --test grouped_overrides -E 'test(partner_col_matrix::)'
//
// The batched relationship-matrix target
// (TODO/full-schema/13_test-rel-matrix.org). ONE test function builds
// ONE database of mutually independent subgraphs (IDs prefixed by
// scenario), then runs the matrix scenarios serially against it: de
// novo rendering of the read-only cols, and -- per col -- save after
// reorder, insertion of a non-member, deletion of a member, plus the
// writable cols' membership edits and the restricted-set omission.
// Scenario failures ACCUMULATE: every mismatch is collected and the
// test fails once at the end, so one broken col does not mask the
// rest.
//
// DEVIATION from the plan (recorded in progress.org): the plan
// envisioned before.org/after.org file pairs and claimed the target
// "should not need the global graph handle". In fact the PartnerCol
// scaffolds are created from snapshot_global(), so de-novo rendering
// of subscriberCol / overriderCol / hiderCol / hiddenCol / and even
// the outbound overriddenCol shows NOTHING without the installed
// global handle. This target therefore installs it (in the matrix
// function only -- the monogamy function below does not, so the two
// never double-install under plain `cargo test`). And rather than
// hand-author expected buffers, each scenario renders de novo, edits
// the real rendered text, saves, and asserts on the saved view, its
// warnings, and (for writable cols) the would-be disk lists -- the
// established style of partner_col_order / partner_col_warnings, which
// keeps the metadata always correct.

use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::source_sets::{
  ActiveSourceSet, SourceSetName, run_with_source_set_test_db};
use skg::test_utils::graph_handle_from_config;
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::{
  multi_root_view, multi_root_view_with_source_set};
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, MSV, SkgConfig, TantivyIndex};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::nodeComplete_rustFIrst_by_id;
use skg::from_text::buffer_to_validated_saveplan;
use skg::types::errors::{SaveError, BufferValidationError};
use skg::types::viewnode::ViewNode;
use ego_tree::Tree;
use indoc::indoc;
use typedb_driver::TypeDBDriver;

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

fn saved_node_by_id<'a> (
  instructions : &'a [DefineNode], id : &str,
) -> Option<&'a NodeComplete> {
  for instruction in instructions {
    if let DefineNode::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (id) { return Some (node); }}}
  None }

fn line_containing<'a> ( buf : &'a str, fragment : &str ) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ( "no line contains {:?} in:\n{}", fragment, buf )) }

async fn render (
  root    : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
) -> Result<String, Box<dyn Error>> {
  let (buf, _pids, _tree) : (String, Vec<ID>, Tree<ViewNode>) =
    multi_root_view (
      driver, config, None, &[ ID::from (root) ], false ) . await ?;
  Ok (buf) }

async fn save (
  buf     : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
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
    &mut stream, buf, driver, config, tantivy, graph, false,
    &Err ( String::new () ), &mut views_state ) . await }

/// Swap the two whole lines that carry these metadata fragments.
fn swap_lines ( buf : &str, a : &str, b : &str ) -> String {
  let a_line : String = line_containing (buf, a) . to_string ();
  let b_line : String = line_containing (buf, b) . to_string ();
  buf . replace ( &a_line, "\u{0}SWAP\u{0}" )
      . replace ( &b_line, &a_line )
      . replace ( "\u{0}SWAP\u{0}", &b_line ) }

/// Fabricate an intruder member line (a definitive public leaf) at the
/// member's indentation, plus a child one level deeper, so the repair
/// is a demotion-to-independent rather than a removal. Built fresh
/// (not cloned from a member line) so it never inherits a foreign
/// source -- the overriderCol's members are foreign.
fn intruder_with_child (
  member_line : &str,
  intruder_id : &str,
) -> (String, String) {
  let stars : usize =
    member_line . chars () . take_while ( |c| *c == '*' ) . count ();
  let line : String = format! (
    "{} (skg (node (id {}) (source public))) {}",
    "*" . repeat (stars), intruder_id, intruder_id );
  let child : String =
    format! ( "{} {}-child", "*" . repeat (stars + 1), intruder_id );
  ( line, child ) }

//////////////////////////////////////////////////////////////
// Read-only col spec and per-behavior scenario helpers
//////////////////////////////////////////////////////////////

struct ColSpec {
  atom     : &'static str, // e.g. "subscriberCol"
  owner    : &'static str,
  member_a : &'static str, // sorts before member_b
  member_b : &'static str,
  intruder : &'static str, // a public non-member to park in the col
}

const READONLY_COLS : [ColSpec; 4] = [
  ColSpec { atom : "subscriberCol", owner : "roSub-owner",
            member_a : "roSub-a", member_b : "roSub-b",
            intruder : "roSub-x" },
  ColSpec { atom : "overriderCol", owner : "roOvr-owner",
            member_a : "roOvr-a", member_b : "roOvr-b",
            intruder : "roOvr-x" },
  ColSpec { atom : "hiderCol", owner : "roHider-owner",
            member_a : "roHider-a", member_b : "roHider-b",
            intruder : "roHider-x" },
  ColSpec { atom : "hiddenCol", owner : "roHidden-owner",
            member_a : "roHidden-a", member_b : "roHidden-b",
            intruder : "roHidden-x" },
];

async fn readonly_reorder (
  fails : &mut Fails, spec : &ColSpec,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let scenario : String = format! ("{}/reorder", spec . atom);
  let buf : String = render (spec . owner, config, driver) . await ?;
  let swapped : String = swap_lines (
    &buf,
    &format! ("(id {})", spec . member_a),
    &format! ("(id {})", spec . member_b) );
  let resp : SaveResponse = match save (
    &swapped, config, driver, tantivy, graph) . await {
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

async fn readonly_insert (
  fails : &mut Fails, spec : &ColSpec,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let scenario : String = format! ("{}/insert", spec . atom);
  let buf : String = render (spec . owner, config, driver) . await ?;
  let member_b_line : String =
    line_containing (&buf, &format! ("(id {})", spec . member_b))
    . to_string ();
  let (intruder_line, child_line) : (String, String) =
    intruder_with_child ( &member_b_line, spec . intruder );
  let edited : String = buf . replace (
    &member_b_line,
    &format! ("{}\n{}\n{}", member_b_line, intruder_line, child_line) );
  let resp : SaveResponse = match save (
    &edited, config, driver, tantivy, graph) . await {
    Ok (r) => r,
    Err (e) => { fails . record (&scenario, format! (
      "save errored: {}", e)); return Ok (( )); } };
  if ! resp . errors . is_empty () {
    fails . record (&scenario, format! (
      "save reported errors: {:?}", resp . errors)); return Ok (( )); }
  // The intruder is demoted to independent in the rerendered view.
  { let intruder_after : &str = line_containing (
      &resp . saved_view, &format! ("(id {})", spec . intruder) );
    if ! intruder_after . contains ("independent") {
      fails . record (&scenario, format! (
        "intruder must be demoted to independent: {}",
        intruder_after )); } }
  // ... and a warning says so.
  if ! resp . warnings . iter () . any ( |w|
      w . contains (&format! ("Repaired {}", spec . atom))
      && w . contains ("independent")
      && w . contains (spec . intruder) ) {
    fails . record (&scenario, format! (
      "expected a demote-to-independent warning naming {}: {:?}",
      spec . intruder, resp . warnings )); }
  Ok (( )) }

async fn readonly_delete (
  fails : &mut Fails, spec : &ColSpec,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let scenario : String = format! ("{}/delete", spec . atom);
  let buf : String = render (spec . owner, config, driver) . await ?;
  let member_a_line : String =
    line_containing (&buf, &format! ("(id {})", spec . member_a))
    . to_string ();
  let edited : String =
    buf . replace ( &format! ("{}\n", member_a_line), "" );
  let resp : SaveResponse = match save (
    &edited, config, driver, tantivy, graph) . await {
    Ok (r) => r,
    Err (e) => { fails . record (&scenario, format! (
      "save errored: {}", e)); return Ok (( )); } };
  if ! resp . errors . is_empty () {
    fails . record (&scenario, format! (
      "save reported errors: {:?}", resp . errors)); return Ok (( )); }
  // The deleted member respawns (read-only set).
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
// De-novo render of the four read-only cols on one owner
//////////////////////////////////////////////////////////////

async fn denovo_readonly_render (
  fails : &mut Fails,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let s : &str = "denovo/read-only-cols";
  let buf : String = render ("dn-owner", config, driver) . await ?;
  for atom in ["subscriberCol", "overriderCol",
               "hiderCol", "hiddenCol"] {
    fails . want_contains (s, &buf, &format! ("(skg {})", atom)); }
  for (a, b) in [("dn-sub-a", "dn-sub-b"),
                 ("dn-ovr-a", "dn-ovr-b"),
                 ("dn-hider-a", "dn-hider-b"),
                 ("dn-hid-a", "dn-hid-b")] {
    // members present, indefinitive, and in sorted-ID order
    for id in [a, b] {
      let line : &str = line_containing (&buf, &format! ("(id {})", id));
      if ! line . contains (" indef") {
        fails . record (s, format! (
          "member {} should render indefinitive: {}", id, line)); } }
    fails . want_before (
      s, &buf, &format! ("(id {})", a), &format! ("(id {})", b) ); }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// The matrix test
//////////////////////////////////////////////////////////////

#[test]
fn relationship_matrix
  () -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-partner-col-matrix",
    "tests/partner_col_matrix/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-partner-col-matrix",
    |config, driver, tantivy| Box::pin ( async move {
      // PartnerCol scaffolds are created from snapshot_global(); install
      // it (this is the only installer in the target).
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      skg::dbs::in_rust_graph::init_global_handle_for_first_time_or_panic (
        graph . clone () );
      let mut fails : Fails = Fails::new ();

      denovo_readonly_render (&mut fails, config, driver) . await ?;
      for spec in &READONLY_COLS {
        readonly_reorder (
          &mut fails, spec, config, driver, tantivy, &graph) . await ?;
        readonly_insert (
          &mut fails, spec, config, driver, tantivy, &graph) . await ?;
        readonly_delete (
          &mut fails, spec, config, driver, tantivy, &graph) . await ?;
      }
      writable_subscribeeCol (&mut fails, config, driver) . await ?;
      writable_overriddenCol (&mut fails, config, driver) . await ?;
      hiddenCol_delete_does_not_unhide (
        &mut fails, config, driver) . await ?;
      omission_scenarios (&mut fails, config, driver) . await ?;
      col_request_scenarios (
        &mut fails, config, driver, tantivy, &graph) . await ?;

      fails . finish () } )) }

//////////////////////////////////////////////////////////////
// The Col view-request, '(viewRequests (col RELNAME))': build BOTH
// cols of the relation, the writable one even when empty (decision A).
// Save a minimal definitive buffer carrying just the request and assert
// on the rerendered view. An absent writable col means "no opinion"
// (MSV::Unspecified, filled from disk), so these saves never wipe the
// relation -- the cols come back populated/empty in the rerender.
//////////////////////////////////////////////////////////////

async fn col_request_scenarios (
  fails : &mut Fails,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let request_buf = | owner : &str, rel : &str | -> String {
    format! (
      "* (skg (node (id {}) (source public) (viewRequests (col {})))) {}\n",
      owner, rel, owner ) };
  { // (col overrides) on wSub-owner, which overrides nothing and is
    // overridden by nothing: the WRITABLE overriddenCol appears EMPTY
    // (the "add an override here" surface); the read-only overriderCol
    // does not appear (empty read-only cols are pruned).
    let s : &str = "col-request/overrides-empty";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "overrides"),
      config, driver, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg overriddenCol)");
    fails . want_absent  (s, &resp . saved_view, "overriderCol"); }
  { // (col overrides) on wOvr-owner, which overrides wOvr-a and wOvr-b:
    // the overriddenCol appears POPULATED with both.
    let s : &str = "col-request/overrides-populated";
    let resp : SaveResponse = save (
      &request_buf ("wOvr-owner", "overrides"),
      config, driver, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg overriddenCol)");
    fails . want_contains (s, &resp . saved_view, "(id wOvr-a)");
    fails . want_contains (s, &resp . saved_view, "(id wOvr-b)"); }
  { // (col subscribes) on wSub-owner: subscribeeCol POPULATED (a,b,c).
    let s : &str = "col-request/subscribes-populated";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "subscribes"),
      config, driver, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg subscribeeCol)");
    fails . want_contains (s, &resp . saved_view, "(id wSub-a)"); }
  { // (col hides) on wSub-owner, which neither hides nor is hidden:
    // both sides read-only and empty, so NOTHING appears.
    let s : &str = "col-request/hides-empty";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "hides"),
      config, driver, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_absent (s, &resp . saved_view, "hiderCol");
    fails . want_absent (s, &resp . saved_view, "hiddenCol"); }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Writable cols (subscribeeCol, overriddenCol): the membership
// edits land on disk. Checked through buffer_to_validated_saveplan,
// which builds (but does not write) the plan, so we read the
// would-be NodeComplete for the owner.
//////////////////////////////////////////////////////////////

async fn saveplan_nodes (
  buf    : &str,
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
  active : Option<&ActiveSourceSet>,
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let (_vf, plan, _warnings) =
    buffer_to_validated_saveplan (buf, config, driver, active) . await ?;
  Ok (plan . define_nodes) }

/// A fresh indefinitive public member line at the given indentation.
fn member_line ( stars : usize, id : &str ) -> String {
  format! ( "{} (skg (node (id {}) (source public) indef)) {}",
            "*" . repeat (stars), id, id ) }

fn col_member_stars ( buf : &str, any_member_fragment : &str ) -> usize {
  line_containing (buf, any_member_fragment)
    . chars () . take_while ( |c| *c == '*' ) . count () }

async fn writable_subscribeeCol (
  fails : &mut Fails,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let buf : String = render ("wSub-owner", config, driver) . await ?;
  let stars : usize = col_member_stars (&buf, "(id wSub-a)");
  { // reorder: [a,b,c] -> swap a,c -> [c,b,a]
    let s : &str = "subscribeeCol/reorder";
    let reordered : String =
      swap_lines (&buf, "(id wSub-a)", "(id wSub-c)");
    let nodes : Vec<DefineNode> =
      saveplan_nodes (&reordered, config, driver, None) . await ?;
    match saved_node_by_id (&nodes, "wSub-owner") {
      Some (n) => if n . subscribes_to != MSV::Specified (vec![
          ID::from ("wSub-c"), ID::from ("wSub-b"), ID::from ("wSub-a")]) {
        fails . record (s, format! (
          "reordered subscribes_to wrong: {:?}", n . subscribes_to)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  { // delete one: remove b -> [a,c]
    let s : &str = "subscribeeCol/delete";
    let b_line : String =
      line_containing (&buf, "(id wSub-b)") . to_string ();
    let edited : String = buf . replace (&format! ("{}\n", b_line), "");
    let nodes : Vec<DefineNode> =
      saveplan_nodes (&edited, config, driver, None) . await ?;
    match saved_node_by_id (&nodes, "wSub-owner") {
      Some (n) => if n . subscribes_to != MSV::Specified (vec![
          ID::from ("wSub-a"), ID::from ("wSub-c")]) {
        fails . record (s, format! (
          "after delete, subscribes_to wrong: {:?}", n . subscribes_to)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  { // insert a member: add d -> [a,b,c,d]
    let s : &str = "subscribeeCol/insert";
    let c_line : String =
      line_containing (&buf, "(id wSub-c)") . to_string ();
    let edited : String = buf . replace (
      &c_line, &format! ("{}\n{}", c_line, member_line (stars, "wSub-d")) );
    let nodes : Vec<DefineNode> =
      saveplan_nodes (&edited, config, driver, None) . await ?;
    match saved_node_by_id (&nodes, "wSub-owner") {
      Some (n) => if n . subscribes_to != MSV::Specified (vec![
          ID::from ("wSub-a"), ID::from ("wSub-b"),
          ID::from ("wSub-c"), ID::from ("wSub-d")]) {
        fails . record (s, format! (
          "after insert, subscribes_to wrong: {:?}", n . subscribes_to)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  Ok (( )) }

fn override_set ( n : &NodeComplete ) -> Vec<ID> {
  match &n . overrides_view_of {
    MSV::Specified (ids) => { let mut v = ids . clone (); v . sort (); v }
    MSV::Unspecified => Vec::new (), } }

async fn writable_overriddenCol (
  fails : &mut Fails,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let buf : String = render ("wOvr-owner", config, driver) . await ?;
  let stars : usize = col_member_stars (&buf, "(id wOvr-a)");
  { // reorder is harmless: order-free set unchanged
    let s : &str = "overriddenCol/reorder";
    let reordered : String =
      swap_lines (&buf, "(id wOvr-a)", "(id wOvr-b)");
    let nodes : Vec<DefineNode> =
      saveplan_nodes (&reordered, config, driver, None) . await ?;
    match saved_node_by_id (&nodes, "wOvr-owner") {
      Some (n) => if override_set (n) != vec![
          ID::from ("wOvr-a"), ID::from ("wOvr-b")] {
        fails . record (s, format! (
          "reorder changed the override set: {:?}", n . overrides_view_of)); },
      None => fails . record (s, "no SaveNode for wOvr-owner" . into ()), } }
  { // delete one: remove a -> [b]
    let s : &str = "overriddenCol/delete";
    let a_line : String =
      line_containing (&buf, "(id wOvr-a)") . to_string ();
    let edited : String = buf . replace (&format! ("{}\n", a_line), "");
    let nodes : Vec<DefineNode> =
      saveplan_nodes (&edited, config, driver, None) . await ?;
    match saved_node_by_id (&nodes, "wOvr-owner") {
      Some (n) => if override_set (n) != vec![ID::from ("wOvr-b")] {
        fails . record (s, format! (
          "after delete, override set wrong: {:?}", n . overrides_view_of)); },
      None => fails . record (s, "no SaveNode for wOvr-owner" . into ()), } }
  { // insert a member: add c -> {a,b,c}
    let s : &str = "overriddenCol/insert";
    let b_line : String =
      line_containing (&buf, "(id wOvr-b)") . to_string ();
    let edited : String = buf . replace (
      &b_line, &format! ("{}\n{}", b_line, member_line (stars, "wOvr-c")) );
    let nodes : Vec<DefineNode> =
      saveplan_nodes (&edited, config, driver, None) . await ?;
    match saved_node_by_id (&nodes, "wOvr-owner") {
      Some (n) => if override_set (n) != vec![
          ID::from ("wOvr-a"), ID::from ("wOvr-b"), ID::from ("wOvr-c")] {
        fails . record (s, format! (
          "after insert, override set wrong: {:?}", n . overrides_view_of)); },
      None => fails . record (s, "no SaveNode for wOvr-owner" . into ()), } }
  Ok (( )) }

/// Deleting a member from the read-only hiddenCol must not unhide it
/// on disk: the owner's hides_from_its_subscriptions is never read
/// from the col, so the deleted member stays hidden. (The view-level
/// twin -- respawn in the saved view -- is the hiddenCol case of
/// readonly_delete; the extraction-seam twin is in commit 1.)
async fn hiddenCol_delete_does_not_unhide (
  fails : &mut Fails,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let s : &str = "hiddenCol/no-unhide-on-disk";
  let buf : String = render ("roHidden-owner", config, driver) . await ?;
  let a_line : String =
    line_containing (&buf, "(id roHidden-a)") . to_string ();
  let edited : String = buf . replace (&format! ("{}\n", a_line), "");
  let nodes : Vec<DefineNode> =
    saveplan_nodes (&edited, config, driver, None) . await ?;
  // Either the owner is a no-op (absent from the plan), or its SaveNode
  // still hides roHidden-a; in no case is roHidden-a unhidden.
  if let Some (n) = saved_node_by_id (&nodes, "roHidden-owner") {
    let hides : Vec<ID> = match &n . hides_from_its_subscriptions {
      MSV::Specified (ids) => ids . clone (),
      MSV::Unspecified => Vec::new (), };
    if ! hides . is_empty () && ! hides . contains (&ID::from ("roHidden-a")) {
      fails . record (s, format! (
        "deleting from hiddenCol unhid roHidden-a: hides = {:?}", hides)); } }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Omission under a restricted source-set: an inactive-source member
// beside an active one is omitted from the render (no placeholder);
// for the writable col, the save weaves the omitted member back.
//////////////////////////////////////////////////////////////

async fn omission_scenarios (
  fails : &mut Fails,
  config : &SkgConfig, driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let active : ActiveSourceSet =
    ActiveSourceSet::named (config, SourceSetName::from ("public")) ?;
  { // read-only subscriberCol: inactive omitted, active shown
    let s : &str = "subscriberCol/omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<ViewNode>) =
      multi_root_view_with_source_set (
        driver, config, None, &[ID::from ("omSub-owner")],
        false, &active ) . await ?;
    fails . want_contains (s, &buf, "(id omSub-active)");
    fails . want_absent (s, &buf, "omSub-inactive"); }
  { // writable subscribeeCol: inactive omitted from render, but the
    // restricted save weaves it back into subscribes_to.
    let s : &str = "subscribeeCol/omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<ViewNode>) =
      multi_root_view_with_source_set (
        driver, config, None, &[ID::from ("omWsub-owner")],
        false, &active ) . await ?;
    fails . want_contains (s, &buf, "(id omWsub-active)");
    fails . want_absent (s, &buf, "omWsub-inactive");
    // Delete the only VISIBLE subscribee and save under the restricted
    // set: the weave must still preserve the invisible omWsub-inactive
    // (a restricted save cannot delete what it cannot see), while the
    // deleted active member is removed. So subscribes_to = [inactive].
    let active_line : String =
      line_containing (&buf, "(id omWsub-active)") . to_string ();
    let edited : String =
      buf . replace (&format! ("{}\n", active_line), "");
    let nodes : Vec<DefineNode> =
      saveplan_nodes (&edited, config, driver, Some (&active)) . await ?;
    match saved_node_by_id (&nodes, "omWsub-owner") {
      Some (n) => { let subs : Vec<ID> = match &n . subscribes_to {
          MSV::Specified (ids) => ids . clone (),
          MSV::Unspecified => Vec::new (), };
        if ! subs . contains (&ID::from ("omWsub-inactive")) {
          fails . record (s, format! (
            "restricted save dropped the invisible subscribee: {:?}",
            subs)); }
        if subs . contains (&ID::from ("omWsub-active")) {
          fails . record (s, format! (
            "deleted visible subscribee should be gone: {:?}", subs)); } }
      None => fails . record (s,
        "deleting the visible subscribee should change the owner" . into ()), } }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Function 2 (its own database, since it must observe a REJECTED
// save): the buffer-level half of "two user-owned overriders rejected
// at save". A save adds a second user-owned overrider for an
// already-overridden target via an overriddenCol; the save is
// rejected with the monogamy error and disk is unchanged.
//
// This function does NOT install the global graph handle (so it never
// double-installs with relationship_matrix under plain `cargo test`):
// the override-invariant check reads the save's own graph handle, and
// the buffer is hand-written rather than rendered.
//////////////////////////////////////////////////////////////

#[test]
fn buffer_save_rejects_second_user_owned_overrider
  () -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-partner-col-matrix-monogamy",
    "tests/partner_col_matrix/fixtures-monogamy/skgconfig.toml",
    "/tmp/tantivy-test-partner-col-matrix-monogamy",
    |config, driver, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      // mono-r1 already overrides mono-target on disk; this buffer
      // makes mono-r2 override it too.
      let buffer : &str = indoc! {"
        * (skg (node (id mono-r2) (source public))) mono-r2
        ** (skg overriddenCol)
        *** (skg (node (id mono-target) (source public) indef)) mono-target
      "};
      let result : Result<SaveResponse, Box<dyn Error>> =
        save (buffer, config, driver, tantivy, &graph) . await;
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
      let r2 : NodeComplete =
        nodeComplete_rustFIrst_by_id (
          config, driver, &ID::from ("mono-r2") ) . await ?;
      let overrides_empty : bool = match &r2 . overrides_view_of {
        MSV::Unspecified       => true,
        MSV::Specified (ids)   => ids . is_empty (), };
      assert! ( overrides_empty,
        "a rejected save must not write mono-r2's override edge: {:?}",
        r2 . overrides_view_of );
      Ok (( )) } )) }
