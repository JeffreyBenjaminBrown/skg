//! The "stage and commit" request ('docs/diff-view.org', "Stage and
//! commit"). With '(processed-only . t)' it commits the processed
//! nodes, less those that must wait for a node that does not go in
//! ('processed_commit_choice'), and asks nothing. A node one of whose
//! files is not what skg writes for it ('files_hold_more_than_their_node'
//! in 'server/diff_view/other_changes.rs') is not processed, and a file
//! Git lists as changed whose node did not change in the graph goes in
//! only when it is what skg writes, which it is not. Without it, it
//! commits every changed node by the same commit, and first asks for
//! confirmation, unless the request carries '(confirmed . t)', when
//! some changed node is not processed.
//!
//! The commit is checked before anything is committed
//! ('check_candidate'): the graph it would leave must read back,
//! validate, name no node it lacks that neither HEAD nor the worktree
//! lacks too, and differ from the worktree by exactly the changes of
//! the nodes left out. The save's mutation gate is held from the
//! choice of nodes to the last git-commit.
//!
//! Every answer is followed by a rerender stream, as the
//! delete-references command's is: empty unless something was
//! committed, else one view per open buffer of the diff view,
//! rerendered as any rerender is, over the rebuilt graph straddle
//! ('stream_rerender_diffView_views'). Its roots stay, so a buffer whose
//! changes were all committed comes back as plain context.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::complete_validation::validated_graph;
use crate::dbs::in_rust_graph::relation_accessors::RelationshipKey;
use crate::diff_view::change_leaves::{
  BufferProcessedSummary, LeafKey, ProcessedSummary,
  processed_response_sexp, processed_response_trailing_entries,
  processed_summary, title_in_straddle};
use crate::diff_view::commit_choice::{
  CommitChoice, WaitingNode, processed_commit_choice, whole_commit_choice};
use crate::diff_view::processed_store::ProcessedStore;
use crate::diff_view::other_changes::{
  FILE_NOT_AS_SKG_WRITES_IT, files_hold_more_than_their_node};
use crate::git_ops::git_snapshot::read_candidate_graph_snapshot;
use crate::git_ops::git_snapshot_types::GraphSnapshot;
use crate::git_ops::stage_and_commit::{
  ChangedSkgPath, PreparedCommits, changed_skg_paths_by_stem,
  commit_prepared, prepare_commits};
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::graph_straddle::union_walk::ROLES_OF_LISTED_MEMBERS;
use crate::rerender_pass::DiffViewBufferRerender;
use crate::serve::ViewsState;
use crate::serve::rerender_stream::{
  PreparedRerenders, RerenderRequest, RerenderSelector, prepare_rerender_views,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_string_list_sexp, send_response_with_length_prefix,
  sexp_to_client_string, tag_sexp_response, true_entry_sexp,
  value_from_request_sexp};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::{SkgrepoRestriction, require_unrestricted};
use crate::types::env::{MutationGate, RuntimeGeneration, SkgEnv};
use crate::types::config::SkgConfig;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::sexp::extract_string_list_from_sexp;

use futures::executor::block_on;
use git2::Oid;
use sexp::Sexp;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::iter::once;
use std::net::TcpStream;
use std::sync::{Arc, MutexGuard};

/// Which nodes a request commits.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommitScope {
  /// '(processed-only . t)': the processed nodes, less those that wait.
  ProcessedOnly,
  /// Every changed node. 'confirmed' is the request's '(confirmed . t)'.
  Everything { confirmed : bool },
}

/// What a "stage and commit" request did.
#[derive(Debug)]
pub enum StageAndCommitAnswer {
  /// Nothing was committed, for the reason given.
  Refused (String),
  /// A commit of everything, unconfirmed, while some changed node is
  /// not processed. Nothing changed.
  NeedsConfirmation {
    summary : ProcessedSummary, },
  Committed (CommitReport),
}

/// What a commit did: the git-commit of each skgrepo committed, how
/// many changed nodes went in and how many did not, the processed
/// nodes held back (each with its title, and the title of what it
/// waits for), the nodes left out because one of their files is not
/// what skg writes for them though every mark is set (each with its
/// title), and any error met during or after the git-commits.
#[derive(Debug, Default)]
pub struct CommitReport {
  pub commits         : Vec<(SkgrepoName, Oid)>,
  pub committed_nodes : usize,
  pub left_nodes      : usize,
  pub waiting         : Vec<(WaitingNode, String, String)>,
  pub unmarkable      : Vec<(ID, String)>,
  pub errors          : Vec<String>,
}

pub fn handle_stage_and_commit_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let flag = |key : &str| -> bool {
    value_from_request_sexp (key, request) . is_ok_and ( |value| value == "t" ) };
  let scope : CommitScope =
    if flag ("processed-only") { CommitScope::ProcessedOnly }
    else { CommitScope::Everything { confirmed : flag ("confirmed") } };
  let answer : StageAndCommitAnswer =
    match commit_message_from_request (request) {
      Err (e) => StageAndCommitAnswer::Refused (e),
      Ok (message) => stage_and_commit (
        env, views_state, skgrepo_restriction, &message, scope ), };
  send_response_with_length_prefix (
    stream, &tag_sexp_response (
      TcpToClient::StageAndCommit,
      &stage_and_commit_response_sexp (&answer) ));
  match answer {
    StageAndCommitAnswer::Committed (report) if ! report . commits . is_empty () =>
      stream_rerender_diffView_views (
        stream, env, views_state, skgrepo_restriction ),
    _ => stream_empty_rerender (stream), }}

/// The commit message, sent as '(message-lines "LINE" ...)' and
/// joined here with newlines: the server reads one request per line,
/// so a newline cannot be sent inside a string.
fn commit_message_from_request (
  request : &str,
) -> Result<String, String> {
  let parsed : Sexp = sexp::parse (request)
    . map_err ( |e| format! ( "Failed to parse S-expression: {}", e )) ?;
  Ok ( extract_string_list_from_sexp (&parsed, "message-lines") ?
       . join ("\n") ) }

/// After a git-commit: rerender each open buffer of the diff view as
/// any rerender does, over the graph straddle the commit rebuilt. Its
/// roots stay ('docs/diff-view.org', "What the refresh keeps"), and
/// what the commit made unchanged loses its decorations. The commit requires an unrestricted connection, so no text is
/// withheld and no release is asked for.
fn stream_rerender_diffView_views (
  stream              : &mut TcpStream,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let prepared : PreparedRerenders = prepare_rerender_views (
    env, env . runtime_snapshot (), views_state,
    RerenderRequest {
      restriction : skgrepo_restriction,
      selector    : RerenderSelector::Where ( Box::new (
        |_, state| state . diffView_buffer_kind . is_some () )),
      diffView_buffer_rerender : DiffViewBufferRerender::RefreshOnly, } );
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// See the module comment. Refused unless the connection is
/// unrestricted, as the diff view is, and for a blank message. The
/// marks are read once, under the processed store's lock, when the
/// nodes are chosen; a save, which carries marks, waits for the
/// mutation gate, held until the last git-commit.
pub fn stage_and_commit (
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  message             : &str,
  scope               : CommitScope,
) -> StageAndCommitAnswer {
  if let Err (refusal) =
    require_unrestricted ("Stage and commit", skgrepo_restriction) {
    return StageAndCommitAnswer::Refused (refusal); }
  if message . trim () . is_empty () {
    return StageAndCommitAnswer::Refused (
      "A commit message is required." . to_string () ); }
  let mutation_gate : MutationGate = env . mutation_gate ();
  let mutation_guard = block_on ( mutation_gate . lock () );
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let straddle : Arc<GraphStraddle> = match current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &runtime . config,
    runtime . graph . clone () ) {
    Ok (straddle) => straddle,
    Err (e) => return StageAndCommitAnswer::Refused ( format! (
      "Cannot stage and commit: {}", e )), };
  let paths_by_stem : BTreeMap<ID, Vec<ChangedSkgPath>> =
    match changed_skg_paths_by_stem (&runtime . config) {
      Ok (paths) => paths,
      Err (e) => return StageAndCommitAnswer::Refused ( format! (
        "Nothing was committed: {}", e )), };
  let files_not_as_skg_writes_them : BTreeSet<ID> = match scope {
    CommitScope::ProcessedOnly => stems_whose_files_are_not_as_skg_writes_them (
      &runtime . config, &straddle, &paths_by_stem ),
    CommitScope::Everything { .. } => BTreeSet::new (), };
  let (choice, summary) : (CommitChoice, ProcessedSummary) = {
    let store : MutexGuard<ProcessedStore> =
      env . runtime . lock_processed_store ();
    ( match scope {
        CommitScope::ProcessedOnly => processed_commit_choice (
          &runtime . config, &straddle, &store, &files_not_as_skg_writes_them ),
        CommitScope::Everything { .. } =>
          whole_commit_choice (&runtime . config, &straddle, &store), },
      processed_summary (&runtime . config, &straddle, &store) ) };
  if let CommitScope::Everything { confirmed : false } = scope {
    if ! summary . is_complete () {
      return StageAndCommitAnswer::NeedsConfirmation { summary }; }}
  let left_out : BTreeSet<ID> = choice . left_out ();
  let answer : StageAndCommitAnswer = commit_nodes (
    &runtime . config, &straddle, &paths_by_stem, &choice . going_in, &left_out,
    &choice . waiting, &choice . unmarkable, &files_not_as_skg_writes_them, message );
  drop (mutation_guard);
  match answer {
    StageAndCommitAnswer::Committed (mut report) => {
      if ! report . commits . is_empty () {
        // A fresh snapshot: a save that took the gate since may have
        // changed the graph and marked its changes, and those marks stay.
        report . errors . extend ( rebuild_straddle_and_drop_committed_marks (
          env, views_state, &env . runtime_snapshot () )); }
      StageAndCommitAnswer::Committed (report) },
    other => other, }}

/// The stems among 'paths_by_stem' that name a node of either graph
/// some changed file of which is not what skg writes for it, at HEAD or
/// in the worktree ('files_hold_more_than_their_node').
pub fn stems_whose_files_are_not_as_skg_writes_them (
  config        : &SkgConfig,
  straddle      : &GraphStraddle,
  paths_by_stem : &BTreeMap<ID, Vec<ChangedSkgPath>>,
) -> BTreeSet<ID> {
  paths_by_stem . iter ()
    . filter ( |(stem, paths)| {
      let (before, after) = ( straddle . before . get (stem), straddle . after . get (stem) );
      ( before . is_some () || after . is_some () )
        && files_hold_more_than_their_node (config, before, after, paths) } )
    . map ( |(stem, _)| stem . clone () )
    . collect () }

/// Commit the nodes 'going_in', and with them every owned node of the
/// live graph whose files Git lists as changed ('paths_by_stem') but
/// which 'going_in', 'left_out' and the changed nodes all lack: it
/// changed only in bytes the parser does not read. Such a node among
/// 'withheld' (whose files are not what skg writes for it, which a
/// commit of the processed withholds) does not go in: it is reported
/// among 'unmarkable', and counted among the nodes left. A stem Git
/// lists that names no owned node (a section of a foreign node in an
/// owned skgrepo, which the graph sets apart as an illegal edit) never
/// goes in. 'left_out' are the changed nodes that do not go in: the
/// commit is checked to leave exactly their changes
/// ('check_candidate'), and nothing is committed otherwise. 'waiting'
/// and 'unmarkable' are reported as they are. The caller holds the
/// mutation gate, and 'straddle' is current.
pub fn commit_nodes (
  config        : &SkgConfig,
  straddle      : &GraphStraddle,
  paths_by_stem : &BTreeMap<ID, Vec<ChangedSkgPath>>,
  going_in      : &BTreeSet<ID>,
  left_out      : &BTreeSet<ID>,
  waiting       : &[WaitingNode],
  unmarkable    : &[ID],
  withheld      : &BTreeSet<ID>,
  message       : &str,
) -> StageAndCommitAnswer {
  let changed : BTreeSet<ID> = straddle . change_leaves () . changed_nodes (config);
  let byte_only = |stem : &ID| -> bool { // see the doc comment
    ! going_in . contains (stem) && ! left_out . contains (stem)
      && ! changed . contains (stem)
      && straddle . after . get (stem) . is_some_and (
        |node| config . skgrepo_is_owned (&node . home_skgrepo) ) };
  let byte_only_withheld : Vec<ID> = paths_by_stem . keys ()
    . filter ( |stem| byte_only (stem) && withheld . contains (*stem) )
    . cloned () . collect ();
  let committed : BTreeSet<ID> = going_in . iter () . cloned ()
    . chain ( paths_by_stem . keys ()
              . filter ( |stem| byte_only (stem) && ! withheld . contains (*stem) )
              . cloned () )
    . collect ();
  let paths : Vec<ChangedSkgPath> = committed . iter ()
    . filter_map ( |pid| paths_by_stem . get (pid) )
    . flatten () . cloned () . collect ();
  let mut report : CommitReport = CommitReport {
    committed_nodes : committed . len (),
    left_nodes      : left_out . len () + byte_only_withheld . len (),
    waiting         : waiting . iter ()
      . map ( |node| ( node . clone (),
                       title_in_straddle (straddle, &node . pid),
                       title_in_straddle (straddle, &node . waits_for) ))
      . collect (),
    unmarkable      : {
      let mut unmarkable : Vec<(ID, String)> =
        unmarkable . iter () . cloned ()
        . chain ( byte_only_withheld )
        . map ( |pid| {
          let title : String = title_in_straddle (straddle, &pid);
          (pid, title) } )
        . collect ();
      unmarkable . sort_by ( |a, b| a . 0 . cmp (&b . 0) );
      unmarkable },
    .. CommitReport::default () };
  let prepared : PreparedCommits = match prepare_commits (config, &paths) {
    Ok (prepared) => prepared,
    Err (e) => return StageAndCommitAnswer::Refused ( format! (
      "Nothing was committed: {}", e )), };
  if prepared . commits . is_empty () {
    // Nothing differs from HEAD, so nothing went in.
    report . committed_nodes = 0;
    return StageAndCommitAnswer::Committed (report); }
  if let Err (e) = check_candidate (config, straddle, &prepared, left_out) {
    return StageAndCommitAnswer::Refused (e); }
  let (commits, errors) = commit_prepared (&prepared, message);
  if commits . len () < prepared . commits . len () {
    // Some commit was not made: count only the nodes each of whose
    // files went in.
    let committed_skgrepos : BTreeSet<&SkgrepoName> =
      commits . iter () . map ( |(skgrepo, _)| skgrepo ) . collect ();
    report . committed_nodes = committed . iter ()
      . filter_map ( |pid| paths_by_stem . get (pid) )
      . filter ( |paths| paths . iter ()
                 . all ( |path| committed_skgrepos . contains (&path . skgrepo) ))
      . count (); }
  report . commits = commits;
  report . errors = errors;
  StageAndCommitAnswer::Committed (report) }

/// The check before anything is committed: the graph the prepared
/// commits would leave ('PreparedCommits::final_tree_of', with the
/// foreign skgrepos at their worktrees) must read back, pass the
/// whole-graph validation, name in the three lists no node it lacks
/// unless HEAD or the worktree names it so too, and differ from the
/// worktree by exactly the change-forest leaves the nodes 'left_out'
/// record, keys and fingerprints both, among the leaves recorded in an
/// owned skgrepo: a leaf recorded only in foreign skgrepos (a foreign
/// node listing an ID a merge re-resolved) is no commit's to make or
/// leave. Mentions are not checked: a link is text.
pub fn check_candidate (
  config   : &SkgConfig,
  straddle : &GraphStraddle,
  prepared : &PreparedCommits,
  left_out : &BTreeSet<ID>,
) -> Result<(), String> {
  let snapshot : GraphSnapshot =
    read_candidate_graph_snapshot (config, &prepared . final_tree_of)
    . map_err ( |e| format! (
      "Nothing was committed: the commit could not be read back: {}", e )) ?;
  let nodes : Vec<Graphnode> = snapshot . nodes . into_values () . collect ();
  let (candidate, _telescope_warnings, _fork_rule_warnings) = validated_graph (config, &nodes)
    . map_err ( |e| format! (
      "Nothing was committed: the commit would not validate: {}",
      e . replace ('\n', " ") )) ?;
  let allowed : HashSet<RelationshipKey> =
    dangling_relationships (&straddle . before) . into_iter ()
    . chain ( dangling_relationships (&straddle . after) )
    . collect ();
  let mut new_dangling : Vec<RelationshipKey> =
    dangling_relationships (&candidate) . into_iter ()
    . filter ( |key| ! allowed . contains (key) )
    . collect ();
  if ! new_dangling . is_empty () {
    new_dangling . sort_by ( |a, b|
      ( &a . recorder, &a . member ) . cmp (&( &b . recorder, &b . member )) );
    tracing::error! ( dangling = ?new_dangling,
                      "a commit of processed nodes would name nodes it lacks" );
    return Err ( format! (
      "Nothing was committed: the commit would leave {} naming {}, which it lacks. {}",
      new_dangling [0] . recorder, new_dangling [0] . member, NOT_EXPECTED )); }
  let candidate_straddle : GraphStraddle = GraphStraddle::new (
    Arc::new (candidate), straddle . after . clone (), String::new (), 0 );
  let leaves_of = |straddle : &GraphStraddle, pids : Option<&BTreeSet<ID>>|
    -> HashSet<(LeafKey, u64)> {
    let leaves = straddle . change_leaves ();
    let recorders : Vec<ID> = match pids {
      Some (pids) => pids . iter () . cloned () . collect (),
      None => leaves . recorders () . cloned () . collect (), };
    leaves . leaves_recorded_by_nodes (&recorders) . into_iter ()
      . filter ( |leaf| leaf . files . iter ()
                 . any ( |file| config . skgrepo_is_owned (&file . skgrepo) ))
      . map ( |leaf| ( leaf . key . clone (), leaf . fingerprint ))
      . collect () };
  let expected : HashSet<(LeafKey, u64)> = leaves_of (straddle, Some (left_out));
  let actual   : HashSet<(LeafKey, u64)> = leaves_of (&candidate_straddle, None);
  if expected != actual {
    tracing::error! (
      expected = ?expected, actual = ?actual,
      "a commit would not leave exactly the changes of the nodes left out" );
    return Err ( format! (
      "Nothing was committed: the commit would not leave exactly the changes of the nodes left out. {}",
      NOT_EXPECTED )); }
  Ok (( )) }

/// Why a refusal of the last two checks can happen: the choice of
/// nodes should have prevented it.
const NOT_EXPECTED : &str =
  "Either a '.skg' file changed on disk and the server has not reloaded it yet, so try again in a moment, or this is a bug; the server's log has what was compared.";

/// The relationships of 'graph', in the three lists, whose member
/// resolves to no node.
fn dangling_relationships (
  graph : &InRustGraph,
) -> Vec<RelationshipKey> {
  graph . nodes . keys ()
    . flat_map ( |recorder| ROLES_OF_LISTED_MEMBERS . iter ()
      . flat_map ( move |role|
        graph . outbound_gnids_for_relation (recorder, role . relation)
          . into_iter ()
          . filter ( |gnid| graph . pid_of (gnid) . is_none () )
          . map ( move |member| RelationshipKey {
            relation : role . relation,
            recorder : recorder . clone (),
            member } )) )
    . collect () }

/// After the git-commits the connection's graph straddle is rebuilt
/// (each committed HEAD moved), and the store keeps exactly the marks
/// whose change-forest leaves still exist in it, so the marks of the
/// changes committed are dropped. Returns the errors met.
fn rebuild_straddle_and_drop_committed_marks (
  env         : &SkgEnv,
  views_state : &mut ViewsState,
  runtime     : &RuntimeGeneration,
) -> Vec<String> {
  match current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &runtime . config,
    runtime . graph . clone () ) {
    Err (e) => vec! [ format! (
      "The graph straddle could not be rebuilt after the commits: {}", e ) ],
    Ok (rebuilt) => {
      let mut store : MutexGuard<ProcessedStore> =
        env . runtime . lock_processed_store ();
      store . drop_marks_of_vanished_leaves (rebuilt . change_leaves ());
      match store . save ( &runtime . config . processed_marks_path () ) {
        Ok (()) => Vec::new (),
        Err (e) => vec! [ format! (
          "The processed marks were not written to disk: {}", e ) ], }}, }}

/// Format, for each answer:
///   ((errors ("REFUSAL")) (warnings ()))
///   ((needs-confirmation . t) (processed-summary ...) (errors ()) (warnings ()))
///   ((commits (("SKGREPO" "COMMIT-ID") ...)) (committed-nodes N)
///    (left-nodes L) (waiting (("PID" "TITLE" REASON "PID" "TITLE") ...))
///    (unmarkable (("PID" "TITLE" ("CHANGE" ...)) ...))
///    (errors (...)) (warnings ()))
/// Each CHANGE is a change no mark covers, in the user's words: that
/// one of its files is not what skg writes ('FILE_NOT_AS_SKG_WRITES_IT').
/// The processed summary has the save response's form, with a
/// not-editable count of 0. 'commits' lists the skgrepos
/// git-committed. Printed by hand, each string through
/// 'quote_sexp_string': the 'sexp' crate prints an atom bare unless it
/// holds whitespace or a quote, and a bare title holding ';' or '('
/// would not read back as one atom.
pub fn stage_and_commit_response_sexp (
  answer : &StageAndCommitAnswer,
) -> String {
  match answer {
    StageAndCommitAnswer::Refused (error) =>
      sexp_to_client_string ( &processed_response_sexp (
        Vec::new (), None, &[ error . clone () ], &[] )),
    StageAndCommitAnswer::NeedsConfirmation { summary } => {
      let entries : Vec<String> =
        once ( true_entry_sexp ("needs-confirmation") )
        . chain ( processed_response_trailing_entries (
          Some ( &BufferProcessedSummary {
            summary : summary . clone (), changes_not_editable_here : 0 } ),
          &[], &[] ))
        . map ( |entry| sexp_to_client_string (&entry) )
        . collect ();
      format! ( "({})", entries . join (" ") ) },
    StageAndCommitAnswer::Committed (report) => {
      let quoted = |text : &str| -> String { quote_sexp_string (text) };
      let commits : Vec<String> = report . commits . iter ()
        . map ( |(skgrepo, commit_id)| format! (
          "({} {})", quoted (&skgrepo . to_string ()),
          quoted (&commit_id . to_string ()) ))
        . collect ();
      let waiting : Vec<String> = report . waiting . iter ()
        . map ( |(node, title, waits_for_title)| format! (
          "({} {} {} {} {})",
          quoted (&node . pid . 0), quoted (title), node . reason . wire_name (),
          quoted (&node . waits_for . 0), quoted (waits_for_title) ))
        . collect ();
      let unmarkable : Vec<String> = report . unmarkable . iter ()
        . map ( |(pid, title)| format! (
          "({} {} ({}))",
          quoted (&pid . 0), quoted (title),
          quoted (FILE_NOT_AS_SKG_WRITES_IT) ))
        . collect ();
      let trailing : Vec<String> =
        [ format_string_list_sexp ("errors", &report . errors),
          format_string_list_sexp ("warnings", &[]) ]
        . iter () . map (sexp_to_client_string) . collect ();
      format! (
        "((commits ({})) (committed-nodes {}) (left-nodes {}) (waiting ({})) (unmarkable ({})) {})",
        commits . join (" "), report . committed_nodes, report . left_nodes,
        waiting . join (" "), unmarkable . join (" "), trailing . join (" ") ) }, }}
