// cargo nextest run --test grouped_forks -E 'test(partner_folder_matrix::)'
//
// The batched relationship-matrix target
// ('docs/sharing-model.org', "The write-protected folders"). One test
// function builds one database of mutually independent subgraphs (IDs
// prefixed by scenario), then runs the matrix scenarios serially
// against it: de novo omission of unrequested write-protected folders,
// and -- after explicit folder requests, per folder -- save after
// reorder, insertion of a child claiming nothing, deletion of a member;
// the same three for the forks folder's forker-only members, which
// claim nothing and keep the graph's order; plus the forks folder's membership edits and the
// restricted-set omission.
// Scenario failures accumulate: every mismatch is collected and the
// test fails once at the end, so one broken folder does not mask the
// rest. The last scenarios check the containerward chain under each
// folder kind's member.
//
// Instead of before.org/after.org file pairs or hand-authored expected
// buffers, each scenario uses a fixture-local graph, renders de novo, edits
// the real rendered text, saves, and asserts on the saved view, its
// warnings, and (for editable folders) the would-be disk lists -- the
// established style of partner_folder_order / partner_folder_warnings, which
// keeps the metadata always correct.

use crate::view_and_save_helpers::line_containing;
use skg::metadata_canon::without_viewnode_ids;
use std::error::Error;

use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{run_with_skgrepo_set_test_db, save_added_forks};
use skg::types::misc::SkgrepoName;
use skg::test_utils::graph_handle_from_config;
use skg::test_utils::{
  opt_saved_node_by_gnid,
  save_ignoring_streamed_messages,
  empty_views_state,
  multi_root_view, multi_root_view_with_skgrepo_set};
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::ViewforestId;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, MSV, TantivyIndex, members_of, members_msv};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::NodeInstruction;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::graphnode_by_gnid;
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
  let mut views_state : ViewsState = empty_views_state ();
  save_ignoring_streamed_messages (
    buf, config, tantivy, graph,
    // The saved view's ID.
    &Ok ( ViewforestId::ContentView ("matrix view" . to_string ()) ),
    &mut views_state ) . await }

async fn render_with_requested_relation_folders (
  root    : &str,
  relation : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<String, Box<dyn Error>> {
  let initial : String = render (root, config) . await ?;
  let request : String = initial . replacen (
    "node ",
    &format! (
      "node (viewRequests (folder {}))", relation ), 1 );
  let response : SaveResponse =
    save (&request, config, tantivy, graph) . await ?;
  if ! response . errors . is_empty () {
    return Err (format! (
      "{} folder request for {} failed: {:?}",
      relation, root, response . errors )
      . into ()); }
  Ok (response . saved_view)
}

/// 'buf' without the first headline carrying this metadata fragment
/// and the lines under it: deleting a forkee as such, drawn editable
/// with its own folders, deletes what it shows too.
fn without_subtree_of ( buf : &str, fragment : &str ) -> String {
  let depth = |line : &str| -> usize { line . chars () . take_while ( |c| *c == '*' ) . count () };
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter () . position ( |l| l . contains (fragment) )
    . unwrap_or_else ( || panic! ( "no line contains {:?} in:\n{}", fragment, buf ));
  let below : usize = lines [at + 1 ..] . iter ()
    . take_while ( |l| depth (l) == 0 || depth (l) > depth (lines [at]) )
    . count ();
  lines [.. at] . iter () . chain ( lines [at + 1 + below ..] . iter () )
    . copied () . collect::<Vec<&str>> () . join ("\n") + "\n" }

/// The first headline carrying this metadata fragment, with the lines
/// under it, each ending in a newline.
fn subtree_of ( buf : &str, fragment : &str ) -> String {
  let depth = |line : &str| -> usize { line . chars () . take_while ( |c| *c == '*' ) . count () };
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter () . position ( |l| l . contains (fragment) )
    . unwrap_or_else ( || panic! ( "no line contains {:?} in:\n{}", fragment, buf ));
  let below : usize = lines [at + 1 ..] . iter ()
    . take_while ( |l| depth (l) == 0 || depth (l) > depth (lines [at]) )
    . count ();
  lines [at ..= at + below] . iter ()
    . map ( |line| format! ("{}\n", line) ) . collect () }

/// Swap the two subtrees headed by the lines carrying these metadata
/// fragments: a forker as such, drawn editable, carries its own folders.
fn swap_subtrees ( buf : &str, a : &str, b : &str ) -> String {
  let a_tree : String = subtree_of (buf, a);
  let b_tree : String = subtree_of (buf, b);
  buf . replace ( &a_tree, "\u{0}SWAP\u{0}" )
      . replace ( &b_tree, &a_tree )
      . replace ( "\u{0}SWAP\u{0}", &b_tree ) }

/// Swap the two whole lines that carry these metadata fragments.
fn swap_lines ( buf : &str, a : &str, b : &str ) -> String {
  let a_line : String = line_containing (buf, a) . to_string ();
  let b_line : String = line_containing (buf, b) . to_string ();
  buf . replace ( &a_line, "\u{0}SWAP\u{0}" )
      . replace ( &b_line, &a_line )
      . replace ( "\u{0}SWAP\u{0}", &b_line ) }

/// Fabricate an intruder member line (an editable public leaf) at the
/// member's indentation, plus a child one level deeper, so the repair
/// marks it noClaim rather than removing it. Built fresh
/// (not cloned from a member line) so it never inherits a foreign
/// skgrepo from a folder whose members are foreign.
fn intruder_with_child (
  member_line : &str,
  intruder_gnid : &str,
) -> (String, String) {
  let stars : usize =
    member_line . chars () . take_while ( |c| *c == '*' ) . count ();
  let line : String = format! (
    "{} (skg node (gnid {}) (skgRepos (nodeRepo public))) {}",
    "*" . repeat (stars), intruder_gnid, intruder_gnid );
  let child : String =
    format! ( "{} {}-child", "*" . repeat (stars + 1), intruder_gnid );
  ( line, child ) }

//////////////////////////////////////////////////////////////
// Write-protected folder spec and per-behavior scenario helpers
//////////////////////////////////////////////////////////////

struct FolderSpec {
  atom     : &'static str, // e.g. "forksFolder"
  relation : &'static str,
  recorder : &'static str,
  member_a : &'static str, // sorts before member_b
  member_b : &'static str,
  intruder : &'static str, // a public node to place in the folder, claiming nothing
}

const WRITE_PROTECTED_FOLDERS : [FolderSpec; 4] = [
  FolderSpec { atom : "hiderFolder", recorder : "roHider-owner",
            relation : "hides",
            member_a : "roHider-a", member_b : "roHider-b",
            intruder : "roHider-x" },
  FolderSpec { atom : "hiddenFolder", recorder : "roHidden-owner",
            relation : "hides",
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
    &format! ("(gnid {})", spec . member_a),
    &format! ("(gnid {})", spec . member_b) );
  let resp : SaveResponse = match save (
    &swapped, config, tantivy, graph) . await {
    Ok (r) => r,
    Err (e) => { fails . record (&scenario, format! (
      "save errored: {}", e)); return Ok (( )); } };
  if ! resp . errors . is_empty () {
    fails . record (&scenario, format! (
      "save reported errors: {:?}", resp . errors)); return Ok (( )); }
  // Order preserved view-locally: member_b now before member_a. But
  // the hidden folder's goal has an order, its recorder's hides list,
  // which its members take again.
  if spec . atom == "hiddenFolder" {
    fails . want_before (
      &scenario, &resp . saved_view,
      &format! ("(gnid {})", spec . member_a),
      &format! ("(gnid {})", spec . member_b) );
  } else {
    fails . want_before (
      &scenario, &resp . saved_view,
      &format! ("(gnid {})", spec . member_b),
      &format! ("(gnid {})", spec . member_a) ); }
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
    line_containing (&buf, &format! ("(gnid {})", spec . member_b))
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
  // The intruder stays in the rerendered view, unwarranted. Under a
  // write-protected folder no child claims, so it carries no claim
  // atom, and there is no claim to correct. The hides folder is the
  // exception: a graphnode its viewparent hides by no record vanishes
  // from it ('unhidden_in_the_hides_folder' in
  // 'server/refresh/visit.rs').
  if spec . atom == "hiddenFolder" {
    if resp . saved_view . contains (&format! ("(gnid {})", spec . intruder)) {
      fails . record (&scenario, format! (
        "the intruder must vanish from the hides folder: {}",
        resp . saved_view )); }
    return Ok (( )); }
  { let intruder_after : &str = line_containing (
      &resp . saved_view, &format! ("(gnid {})", spec . intruder) );
    if ! intruder_after . contains ("noWarrant")
       || intruder_after . contains ("noClaim") {
      fails . record (&scenario, format! (
        "intruder must stay unwarranted, with no claim atom: {}",
        intruder_after )); } }
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
    line_containing (&buf, &format! ("(gnid {})", spec . member_a))
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
    &format! ("(gnid {})", spec . member_a) );
  Ok (( )) }

//////////////////////////////////////////////////////////////
// The forks folder's forker-only members: roSub-a and roSub-b fork
// roSub-owner, which forks nothing. Its forks folder, drawn unasked,
// holds them claiming nothing, each drawn editable (forkers as such).
// Reordering them does not last: the refresh puts them back in the
// graph's and the config's order (here, by pid); a note placed there
// claiming nothing stays, unwarranted; and
// deleting one hides it in roSub-owner, so that it leaves the folder
// for the folder of what roSub-owner hides outside its fork partners.
// They run in that order: the deletion writes the hide.
//////////////////////////////////////////////////////////////

async fn forker_only_members (
  fails : &mut Fails,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let buf : String = render ("roSub-owner", config) . await ?;
  for member in [ "roSub-a", "roSub-b" ] {
    let line : &str = line_containing (&buf, &format! ("(gnid {})", member));
    if ! line . contains ("noClaim") {
      fails . record ("forksFolder/forker-only", format! (
        "a forker-only member must claim nothing: {}", line )); }}
  { let s : &str = "forksFolder/forker-only/reorder";
    let swapped : String = swap_subtrees (&buf, "(gnid roSub-a)", "(gnid roSub-b)");
    let resp : SaveResponse = save (&swapped, config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save reported errors: {:?}", resp . errors)); }
    fails . want_before (s, &resp . saved_view, "(gnid roSub-a)", "(gnid roSub-b)"); }
  { let s : &str = "forksFolder/forker-only/insert";
    let b_line : String = line_containing (&buf, "(gnid roSub-b)") . to_string ();
    let b_tree : String = subtree_of (&buf, "(gnid roSub-b)");
    let (intruder_line, child_line) : (String, String) =
      intruder_with_child (&b_line, "roSub-x");
    let intruder_line : String = intruder_line . replace (
      "(nodeRepo public))", "(nodeRepo public)) noClaim" );
    let edited : String = buf . replace (
      &b_tree, &format! ("{}{}\n{}\n", b_tree, intruder_line, child_line) );
    let resp : SaveResponse = save (&edited, config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save reported errors: {:?}", resp . errors)); }
    let intruder_after : &str = line_containing (&resp . saved_view, "(gnid roSub-x)");
    if ! ( intruder_after . contains ("noWarrant") && intruder_after . contains ("noClaim") ) {
      fails . record (s, format! (
        "the intruder must stay unwarranted, claiming nothing: {}", intruder_after )); } }
  { let s : &str = "forksFolder/forker-only/delete";
    let edited : String = without_subtree_of (&buf, "(gnid roSub-a)");
    let resp : SaveResponse = save (&edited, config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save reported errors: {:?}", resp . errors)); }
    fails . want_before (s, &resp . saved_view,
                         "(skg hiddenOutsideOfForkPartnersFolder)", "(gnid roSub-a)");
    let owner : Graphnode = graphnode_by_gnid (
      &graph . load_full (), &ID::from ("roSub-owner") ) ?;
    if members_of (owner . hides . or_default ()) != vec! [ ID::from ("roSub-a") ] {
      fails . record (s, format! (
        "deleting a forker-only member must hide it: hides = {:?}",
        owner . hides )); } }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// De-novo omission of the unrequested write-protected folders
//////////////////////////////////////////////////////////////

async fn denovo_omits_unrequested_write_protected_folders (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let s : &str = "denovo/write-protected-folders";
  let buf : String = render ("dn-owner", config) . await ?;
  for atom in ["hiderFolder", "hiddenFolder", "mentionerFolder"] {
    fails . want_absent (s, &buf, &format! ("(skg {})", atom)); }
  // But a root shows its containers folder, with the containerward
  // chain under its members, and its forks folder, holding the notes
  // forking it, claiming nothing.
  fails . want_contains (s, &buf, "** (skg containerFolder)");
  fails . want_contains (s, &buf, "** (skg forksFolder)");
  if ! line_containing (&buf, "(gnid dn-sub-a)") . contains ("noClaim") {
    fails . record (s, format! ("a forker must claim nothing:\n{}", buf )); }
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
      forker_only_members (
        &mut fails, config, tantivy, &graph ) . await ?;
      editable_forksFolder (&mut fails, config) . await ?;
      hiddenFolder_delete_does_not_unhide (
        &mut fails, config, tantivy, &graph ) . await ?;
      omission_scenarios (&mut fails, config) . await ?;
      folder_request_scenarios (
        &mut fails, config, tantivy, &graph) . await ?;
      roleTree_request_is_refused (
        &mut fails, config, tantivy, &graph) . await ?;
      for spec in &CHAIN_FOLDERS {
        member_carries_its_chain (
          &mut fails, spec, config, tantivy, &graph) . await ?; }

      fails . finish () } )) }

//////////////////////////////////////////////////////////////
// The containerward chain under every partner folder's member
// ('docs/glossary.org', "foldering, the containerward chain, chain
// node"). One subgraph per folder kind: the member m of
// the owner's folder is contained by chain-c, which chain-cc contains.
// Under m hang chain-c and, under it, chain-cc, each
// write-protected, claiming nothing, and the view saves back unchanged.
// A member of the forks folder holds them in a containers folder,
// chain-c folded.
//////////////////////////////////////////////////////////////

struct ChainFolderSpec {
  atom     : &'static str,
  owner    : &'static str,
  member   : &'static str,
  relation : Option<&'static str>, // None: drawn by default
  in_a_containers_folder : bool,
}

const CHAIN_FOLDERS : [ChainFolderSpec; 6] = [
  ChainFolderSpec { atom : "forksFolder", owner : "chSubr-owner",
                    member : "chSubr-m", relation : None,
                    in_a_containers_folder : true },
  ChainFolderSpec { atom : "forksFolder", owner : "chSube-owner",
                    member : "chSube-m", relation : None,
                    in_a_containers_folder : true },
  ChainFolderSpec { atom : "hiderFolder", owner : "chHider-owner",
                    member : "chHider-m", relation : Some ("hides"),
                    in_a_containers_folder : false },
  ChainFolderSpec { atom : "hiddenFolder", owner : "chHidden-owner",
                    member : "chHidden-m", relation : Some ("hides"),
                    in_a_containers_folder : false },
  ChainFolderSpec { atom : "containerFolder", owner : "chCont-owner",
                    member : "chCont-m", relation : Some ("containers"),
                    in_a_containers_folder : false },
  ChainFolderSpec { atom : "mentionerFolder", owner : "chMent-owner",
                    member : "chMent-m", relation : Some ("mentioners"),
                    in_a_containers_folder : false },
];

async fn member_carries_its_chain (
  fails : &mut Fails, spec : &ChainFolderSpec,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex, graph : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let scenario : String = format! ("{}/chain", spec . atom);
  let buf : String = match spec . relation {
    Some (relation) => render_with_requested_relation_folders (
      spec . owner, relation, config, tantivy, graph ) . await ?,
    None => render (spec . owner, config) . await ?, };
  let buf : String = without_viewnode_ids (&buf);
  let lines : Vec<&str> = buf . lines () . collect ();
  let Some (folder_index) : Option<usize> = lines . iter ()
    . position ( |line| line . contains (&format! ("(skg {}", spec . atom)) )
    else { fails . record (&scenario, format! ("no {} in:\n{}", spec . atom, buf));
           return Ok (( )); };
  let folder_stars : usize = folder_member_stars (&buf, &format! ("(skg {}", spec . atom));
  let member_fragment : String = format! ("(gnid {})", spec . member);
  let Some (member_index) : Option<usize> = lines . iter () . skip (folder_index)
    . position ( |line| line . contains (&member_fragment) )
    . map ( |offset| folder_index + offset )
    else { fails . record (&scenario, format! ("no member {} in:\n{}", spec . member, buf));
           return Ok (( )); };
  let shift : usize = if spec . in_a_containers_folder { 1 } else { 0 };
  let chain_line = |offset : usize, gnid : &str| -> bool {
    lines . get (member_index + shift + offset) . is_some_and ( |line|
      line . starts_with (&format! ("{} ", "*" . repeat (folder_stars + 1 + shift + offset)))
      && line . contains (&format! ("(gnid {})", gnid))
      && ! line . contains ("noClaim") && line . contains ("writeProtected")
      && ( line . contains ("folded") == ( spec . in_a_containers_folder && offset == 1 )) ) };
  let containers_folder_line : bool = ! spec . in_a_containers_folder
    || lines . get (member_index + 1) . is_some_and ( |line|
         line . starts_with (&format! ("{} (skg containerFolder)",
                                       "*" . repeat (folder_stars + 2) )) );
  if ! ( lines [member_index] . starts_with (&format! ("{} ", "*" . repeat (folder_stars + 1)))
         && containers_folder_line
         && chain_line (1, "chain-c") && chain_line (2, "chain-cc") ) {
    fails . record (&scenario, format! (
      "{} should hold its chain{}, chain-c holding chain-cc, each write-protected, with no claim atom:\n{}",
      spec . member,
      if spec . in_a_containers_folder { " in a containers folder, chain-c folded" } else { "" },
      buf )); }
  let resp : SaveResponse = save (&buf, config, tantivy, graph) . await ?;
  if ! resp . errors . is_empty () || ! resp . warnings . is_empty () {
    fails . record (&scenario, format! (
      "saving the view back: errors {:?}, warnings {:?}", resp . errors, resp . warnings )); }
  if without_viewnode_ids (&resp . saved_view) != buf {
    fails . record (&scenario, format! (
      "the view should save back unchanged:\n{}\nbecame\n{}",
      buf, without_viewnode_ids (&resp . saved_view) )); }
  Ok (( )) }

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
  for role in [ "container", "mentioner", "mentioned", "forkee",
                "forker" ] {
    let s : String = format! ("roleTree/{}", role);
    let buffer : String = format! (
      "* (skg node (gnid wOvr-owner) (skgRepos (nodeRepo public)) (viewRequests (roleTree {}))) wOvr-owner\n",
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
      "* (skg node (gnid {}) (skgRepos (nodeRepo public)) (viewRequests (folder {}))) {}\n",
      recorder, rel, recorder ) };
  { // No folder shows overrides: '(folder overrides)' names no
    // folder relation, so the save refuses the buffer.
    let s : &str = "folder-request/overrides-refused";
    match save ( &request_buf ("wOvr-owner", "overrides"),
                 config, tantivy, graph) . await {
      Ok (resp) if resp . errors . is_empty () =>
        fails . record (s, format! (
          "expected a refusal, got the view:\n{}", resp . saved_view )),
      Ok (_) | Err (_) => {}, } }
  { // (folder forks) on wSub-owner: forksFolder populated (a,b,c).
    let s : &str = "folder-request/forks-populated";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "forks"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_contains (s, &resp . saved_view, "(skg forksFolder)");
    fails . want_contains (s, &resp . saved_view, "(gnid wSub-a)"); }
  { // (folder hides) on wSub-owner, which neither hides nor is hidden:
    // both sides write-protected and empty, so nothing appears.
    let s : &str = "folder-request/hides-empty";
    let resp : SaveResponse = save (
      &request_buf ("wSub-owner", "hides"),
      config, tantivy, graph) . await ?;
    if ! resp . errors . is_empty () {
      fails . record (s, format! ("save errors: {:?}", resp . errors)); }
    fails . want_absent (s, &resp . saved_view, "hiderFolder");
    fails . want_absent (s, &resp . saved_view, "hiddenFolder"); }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// The editable forksFolder: the membership
// edits land on disk. Checked through buffer_to_validated_saveplan,
// which builds (but does not write) the plan, so we read the
// would-be Graphnode for the recorder.
//////////////////////////////////////////////////////////////

async fn saveplan_nodes (
  buf    : &str,
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result<Vec<NodeInstruction>, Box<dyn Error>> {
  let (_vf, plan, _warnings) =
    buffer_to_validated_saveplan (buf, config, restriction)  ?;
  Ok (plan . node_instructions) }

/// A fresh write-protected public member line at the given indentation.
fn member_line ( stars : usize, gnid : &str ) -> String {
  format! ( "{} (skg node (gnid {}) (skgRepos (nodeRepo public)) writeProtected) {}",
            "*" . repeat (stars), gnid, gnid ) }

fn folder_member_stars ( buf : &str, any_member_fragment : &str ) -> usize {
  line_containing (buf, any_member_fragment)
    . chars () . take_while ( |c| *c == '*' ) . count () }

async fn editable_forksFolder (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let buf : String = render ("wSub-owner", config) . await ?;
  let stars : usize = folder_member_stars (&buf, "(gnid wSub-a)");
  { // reorder: [a,b,c] -> swap a,c -> [c,b,a]
    let s : &str = "forksFolder/reorder";
    let reordered : String =
      swap_lines (&buf, "(gnid wSub-a)", "(gnid wSub-c)");
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&reordered, config, &SkgrepoRestriction::unrestricted (config)) . await ?;
    match opt_saved_node_by_gnid (&nodes, "wSub-owner") {
      Some (n) => if members_msv (&n . forks) != MSV::Specified (vec![
          ID::from ("wSub-c"), ID::from ("wSub-b"), ID::from ("wSub-a")]) {
        fails . record (s, format! (
          "reordered forks wrong: {:?}", n . forks)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  { // delete one: remove b -> [a,c]
    let s : &str = "forksFolder/delete";
    let edited : String = without_subtree_of (&buf, "(gnid wSub-b)");
    let nodes  : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, &SkgrepoRestriction::unrestricted (config)) . await ?;
    match opt_saved_node_by_gnid (&nodes, "wSub-owner") {
      Some (n) => if members_msv (&n . forks) != MSV::Specified (vec![
          ID::from ("wSub-a"), ID::from ("wSub-c")]) {
        fails . record (s, format! (
          "after delete, forks wrong: {:?}", n . forks)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  { // insert a member: add d -> [a,b,c,d]
    let s : &str = "forksFolder/insert";
    let c_line : String =
      line_containing (&buf, "(gnid wSub-c)") . to_string ();
    let edited : String = buf . replace (
      &c_line, &format! ("{}\n{}", c_line, member_line (stars, "wSub-d")) );
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, &SkgrepoRestriction::unrestricted (config)) . await ?;
    match opt_saved_node_by_gnid (&nodes, "wSub-owner") {
      Some (n) => if members_msv (&n . forks) != MSV::Specified (vec![
          ID::from ("wSub-a"), ID::from ("wSub-b"),
          ID::from ("wSub-c"), ID::from ("wSub-d")]) {
        fails . record (s, format! (
          "after insert, forks wrong: {:?}", n . forks)); },
      None => fails . record (s, "no SaveNode for wSub-owner" . into ()), } }
  Ok (( )) }

/// Deleting a member from the write-protected hiddenFolder must not unhide it
/// on disk: the recorder's hides is never read
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
    "roHidden-owner", "hides", config, tantivy, graph ) . await ?;
  let a_line : String =
    line_containing (&buf, "(gnid roHidden-a)") . to_string ();
  let edited : String = buf . replace (&format! ("{}\n", a_line), "");
  let nodes  : Vec<NodeInstruction> =
    saveplan_nodes (&edited, config, &SkgrepoRestriction::unrestricted (config)) . await ?;
  // Either the recorder is a no-op (absent from the plan), or its SaveNode
  // still hides roHidden-a; in no case is roHidden-a unhidden.
  if let Some (n) = opt_saved_node_by_gnid (&nodes, "roHidden-owner") {
    let hides : Vec<ID> = match &n . hides {
      MSV::Specified (gnids) => members_of (gnids),
      MSV::Unspecified => Vec::new (), };
    if ! hides . is_empty () && ! hides . contains (&ID::from ("roHidden-a")) {
      fails . record (s, format! (
        "deleting from hiddenFolder unhid roHidden-a: hides = {:?}", hides)); } }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Omission under a restricted skgrepo-set: a member whose record is
// restricted is omitted from the render (no placeholder), and one whose
// record is visible and whose node is restricted stands as a Restricted
// vognode; for the editable folder, the save keeps the Restricted
// vognode's member and weaves the omitted one back.
//////////////////////////////////////////////////////////////

async fn omission_scenarios (
  fails : &mut Fails,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoName::from ("public")) ?;
  { // the forks folder's forkers, drawn unasked: the one whose record
    // the restriction keeps out is omitted, the other shown
    let s : &str = "forksFolder/forker-omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view_with_skgrepo_set (
        config, None, &[ID::from ("omSub-owner")], &restriction ) ?;
    fails . want_contains (s, &buf, "(gnid omSub-active)");
    fails . want_absent (s, &buf, "omSub-inactive"); }
  { // editable forksFolder: omWsub-inactive, private, recorded in
    // the owner's public section, stands as a Restricted vognode;
    // omWsub-unseen, recorded in its private section, is omitted from
    // the render, and the restricted save weaves it back into
    // forks.
    let s : &str = "forksFolder/omission";
    let (buf, _p, _t) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view_with_skgrepo_set (
        config, None, &[ID::from ("omWsub-owner")], &restriction ) ?;
    fails . want_contains (s, &buf, "(gnid omWsub-active)");
    fails . want_contains (s, &buf, "restricted (gnid omWsub-inactive)");
    fails . want_absent (s, &buf, "omWsub-unseen");
    // Delete the only unrestricted forkee and save under the
    // restricted set: the Restricted vognode keeps omWsub-inactive, the
    // weave preserves the invisible omWsub-unseen (a restricted save
    // cannot delete what it cannot see), and the deleted unrestricted
    // member is removed.
    let edited : String = without_subtree_of (&buf, "(gnid omWsub-active)");
    let nodes : Vec<NodeInstruction> =
      saveplan_nodes (&edited, config, &restriction) . await ?;
    match opt_saved_node_by_gnid (&nodes, "omWsub-owner") {
      Some (n) => { let subs : Vec<ID> = match &n . forks {
          MSV::Specified (gnids) => members_of (gnids),
          MSV::Unspecified => Vec::new (), };
        if ! subs . contains (&ID::from ("omWsub-inactive")) {
          fails . record (s, format! (
            "restricted save dropped the Restricted vognode's forkee: {:?}",
            subs)); }
        if ! subs . contains (&ID::from ("omWsub-unseen")) {
          fails . record (s, format! (
            "restricted save dropped the invisible forkee: {:?}",
            subs)); }
        if subs . contains (&ID::from ("omWsub-active")) {
          fails . record (s, format! (
            "deleted visible forkee should be gone: {:?}", subs)); } }
      None => fails . record (s,
        "deleting the visible forkee should change the recorder" . into ()), } }
  Ok (( )) }

//////////////////////////////////////////////////////////////
// Function 2 (each its own database, since each must observe the
// disk after a save): a save adding a second owned fork of a note
// saves (through the save's graph update); a buffer save making a
// note fork itself is refused, and disk is unchanged. The buffer is hand-written rather than rendered.
//////////////////////////////////////////////////////////////

#[test]
fn a_save_admits_a_second_owned_fork
  () -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-partner-folder-matrix-second-owned-fork",
    "tests/partner_folder_matrix/fixtures-monogamy/skgconfig.toml",
    "/tmp/tantivy-test-partner-folder-matrix-second-owned-fork",
    |config, tantivy| Box::pin ( async move {
      // mono-r1 already forks mono-target on disk; this save
      // makes mono-r2 fork it too.
      save_added_forks (
        &ID::from ("mono-r2"), &[ ID::from ("mono-target") ],
        config, tantivy ) . await ?;
      let r2 : Graphnode =
        graphnode_by_gnid (
          &graph_handle_from_config (config) ? . load_full (),
          &ID::from ("mono-r2") ) ?;
      assert_eq! ( members_msv (&r2 . forks) . or_default (),
                   &[ ID::from ("mono-target") ],
                   "the save must write mono-r2's fork" );
      Ok (( )) } )) }

#[test]
fn buffer_save_refuses_a_self_fork
  () -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-partner-folder-matrix-self-fork",
    "tests/partner_folder_matrix/fixtures-monogamy/skgconfig.toml",
    "/tmp/tantivy-test-partner-folder-matrix-self-fork",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let buffer : &str = indoc! {"
        * (skg node (gnid mono-r2) (skgRepos (nodeRepo public))) mono-r2
        ** (skg forksFolder)
        *** (skg node (gnid mono-r2) (skgRepos (nodeRepo public)) writeProtected) mono-r2
      "};
      let result : Result<SaveResponse, Box<dyn Error>> =
        save (buffer, config, tantivy, &graph) . await;
      let err : Box<dyn Error> = match result {
        Ok (_)  => panic! (
          "a save making a note fork itself must be refused"),
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
              e, BufferValidationError::ForkRuleViolation (message)
                if message . contains ("node mono-r2 forks itself") )) ),
        "expected a self-fork violation, got {:?}", save_error );
      // Disk unchanged: the check runs before the filesystem write.
      let r2 : Graphnode =
        graphnode_by_gnid (
          &skg::test_utils::graph_handle_from_config (config)? . load_full (),
          &ID::from ("mono-r2") ) ?;
      let forks_empty : bool = match &r2 . forks {
        MSV::Unspecified       => true,
        MSV::Specified (gnids)   => gnids . is_empty (), };
      assert! ( forks_empty,
        "a refused save must not write mono-r2's fork: {:?}",
        r2 . forks );
      Ok (( )) } )) }

/// Owned E contains foreign R, which forks E: the
/// server starts on this, with a warning ('init_refusal.rs'), but a
/// save that leaves the pair is refused, naming both notes; a save
/// that ends it saves.
#[test]
fn a_save_leaving_an_owned_note_containing_its_foreign_forker_is_refused
  () -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-partner-folder-matrix-foreign-forker",
    "tests/partner_folder_matrix/fixtures-foreign-forker/skgconfig.toml",
    "/tmp/tantivy-test-partner-folder-matrix-foreign-forker",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let keeping_r : &str = indoc! {"
        * (skg node (gnid E) (skgRepos (nodeRepo public))) E, retitled
        ** (skg node (gnid R) (skgRepos (nodeRepo foreign)) writeProtected) R
      "};
      let err : Box<dyn Error> =
        match save (keeping_r, config, tantivy, &graph) . await {
          Ok (resp) => panic! (
            "a save leaving E containing R must be refused: {:?}",
            resp . saved_view ),
          Err (e) => e, };
      assert! ( matches! (
                  err . downcast_ref::<SaveError> (),
                  Some (SaveError::BufferValidationErrors { errors, .. })
                    if errors . iter () . any ( |e| matches! (
                      e, BufferValidationError::ForkRuleViolation (message)
                        if message . contains ("node E contains node R, which forks it") ))),
                "expected the pair named: {}", err );
      let dropping_r : &str = indoc! {"
        * (skg node (gnid E) (skgRepos (nodeRepo public))) E, retitled
      "};
      let resp : SaveResponse =
        save (dropping_r, config, tantivy, &graph) . await ?;
      assert! ( resp . errors . is_empty (), "{:?}", resp . errors );
      let e : Graphnode =
        graphnode_by_gnid (
          &graph_handle_from_config (config) ? . load_full (),
          &ID::from ("E") ) ?;
      assert! ( members_of (&e . contains) . is_empty (),
        "the save ending the pair writes E without R: {:?}", e . contains );
      Ok (( )) } )) }
