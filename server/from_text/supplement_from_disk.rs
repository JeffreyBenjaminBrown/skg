/// PURPOSE:
/// When a Graphnode is created from user input,
/// it might not mention every Graphnode field.
/// If it contains Some([]) for that field,
/// then the user is asking to empty the field.
/// But if it has None for that field,
/// then the field should not be changed --
/// which means it must be read from disk
/// and inserted into the Graphnode.

use crate::from_text::local_instruction_collection::lower::{
  RequestedRelRepos, NodeIntent, NodeSaveIntent };
use crate::from_text::weave::{relationship_member_is_visible, set_difference_merge, weave};
use crate::repo_sets::ActiveRepoSet;
use crate::types::errors::BufferValidationError;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::opt_graphnode_by_id;
use crate::types::misc::{ID, MSV, RelPartner, RelationshipMemberKey, SkgConfig, RepoName, members_of, rel_partners_at_relRepo};
use crate::types::phantom::home_from_disk;
use crate::types::nodes::complete::{
  Graphnode, empty_node_complete, set_flag};
use crate::types::save::{DefineNode, SaveNode, RepoMove};
use std::collections::HashMap;
use std::error::Error;

pub struct Definenodes_with_Repomoves {
  pub instructions : Vec<DefineNode>,
  pub repo_moves : Vec<RepoMove>,
}

struct Definenode_with_Opt_Repomove {
  instruction : DefineNode,
  repo_move : Option<RepoMove>,
}

impl Definenodes_with_Repomoves {
  fn with_capacity (
    capacity : usize,
  ) -> Definenodes_with_Repomoves {
    Definenodes_with_Repomoves {
      instructions : Vec::with_capacity (capacity),
      repo_moves : Vec::new(),
    }}

  fn push (
    &mut self,
    node : Definenode_with_Opt_Repomove,
  ) {
    self . instructions . push (node . instruction);
    if let Some (sm) = node . repo_move {
      let sm : RepoMove = sm;
      self . repo_moves . push (sm); }}
}

pub fn build_diskSupplemented_defineNodes (
  intents : Vec<NodeIntent>,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  restricted_repo_set : Option<&ActiveRepoSet>, // None means no restriction; callers normalize 'all' to None.
) -> Result<Definenodes_with_Repomoves, Box<dyn Error>> {
  let mut result : Definenodes_with_Repomoves =
    Definenodes_with_Repomoves::with_capacity (intents . len());
  let prospective_homes : HashMap<ID, RepoName> =
    homes_declared_by_save_intents (&intents);
  for intent in intents {
    let supplemented : Definenode_with_Opt_Repomove =
      supplement_nodeeditintent_from_disk (
        intent, graph, config, restricted_repo_set, &prospective_homes ) ?;
    result . push (supplemented); }
  Ok (result) }

/// Each Save intent's repo is the node home after this save. Relationship
/// floors must see these homes across the entire batch: a parent may name a
/// child before the child intent is supplemented, and a same-save home move
/// must make its newly public edge legal.
fn homes_declared_by_save_intents (
  intents : &[NodeIntent],
) -> HashMap<ID, RepoName> {
  let mut homes : HashMap<ID, RepoName> = HashMap::new ();
  for intent in intents {
    let NodeIntent::Save (intent) = intent else { continue; };
    for id in std::iter::once (&intent . pid) . chain (
      intent . extra_ids . iter ()) {
      homes . insert (id . clone (), intent . home_repo . clone ()); }}
  homes }

fn supplement_nodeeditintent_from_disk (
  intent : NodeIntent,
  graph  : &InRustGraph,
  config : &SkgConfig,
  restricted_repo_set : Option<&ActiveRepoSet>,
  prospective_homes : &HashMap<ID, RepoName>,
) -> Result<Definenode_with_Opt_Repomove, Box<dyn Error>> {
  match intent {
    NodeIntent::Delete (ref delete) => {
      if let Some (active) = restricted_repo_set {
        refuse_delete_with_inactive_sections (
          config, active, & delete . id )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
      Ok (Definenode_with_Opt_Repomove {
        instruction : intent . into_define_node()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        repo_move : None,
      }) },
    _ => supplement_saveintent_from_disk (
      intent . save_intent()
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      graph, config, restricted_repo_set, prospective_homes ),
  }}

fn supplement_saveintent_from_disk (
  from_buffer : NodeSaveIntent,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restricted_repo_set : Option<&ActiveRepoSet>,
  prospective_homes : &HashMap<ID, RepoName>,
) -> Result<Definenode_with_Opt_Repomove, Box<dyn Error>> {
  let pid : ID =
    from_buffer . pid . clone();
  let from_disk : Option<Graphnode> =
    opt_graphnode_by_id (
      graph, config, &pid) ?;
  match from_disk {
    None => {
      // A brand-new node has no sticky repos (no disk edges to be
      // sticky about), but an explicit '(editRequest (relRepo ...))'
      // request must
      // still be validated against the DEFAULT floor -- an empty
      // disk stand-in reuses 'apply_sticky_relRepos' unchanged (its
      // sticky lookups simply find nothing, falling through to
      // default every time).
      let requested_relRepos : RequestedRelRepos =
        from_buffer . requested_relRepos ();
      let flag_request = from_buffer . flag_request;
      let mut supplemented : Graphnode =
        from_buffer . into_graphnode ();
      if let Some ((flag, value)) = flag_request {
        set_flag (&mut supplemented . misc, flag, value); }
      let empty_disk : Graphnode = Graphnode {
        pid    : supplemented . pid    . clone (),
        home_repo : supplemented . home_repo . clone (),
        .. empty_node_complete () };
      let supplemented : Graphnode =
        apply_sticky_relRepos_in_graph_with_prospective_homes (
          supplemented, &empty_disk, &requested_relRepos,
          graph, config, prospective_homes )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
      Ok (Definenode_with_Opt_Repomove {
        instruction : DefineNode::Save (SaveNode (supplemented)),
        repo_move : None, } ) },
    Some (disk_node) => {
      let disk_node : Graphnode = disk_node;
      let mut from_buffer : NodeSaveIntent = from_buffer;
      from_buffer . fill_unspecified_contains (
        &members_of (&disk_node . contains));
      let requested_relRepos : RequestedRelRepos =
        from_buffer . requested_relRepos ();
      let flag_request = from_buffer . flag_request;
      let from_buffer : Graphnode =
        from_buffer . into_graphnode();
      let canonicalized : Graphnode =
        canonicalize_ids_from_disk (from_buffer, &disk_node) ?;
      let maybe_move : Option<RepoMove> =
        detect_repo_move ( config,  &pid,
                             &canonicalized . home_repo,
                             &disk_node . home_repo) ?;
      let supplemented : Graphnode = {
        let mut supplemented : Graphnode =
          supplement_unspecified_fields_from_disk (
            canonicalized, &disk_node);
        if let Some ((flag, value)) = flag_request {
          set_flag (&mut supplemented . misc, flag, value); }
        let supplemented : Graphnode =
          match restricted_repo_set {
            None => supplemented,
            Some (active) => preserve_invisible_members (
              supplemented, &disk_node, graph, config, active ) };
        apply_sticky_relRepos_in_graph_with_prospective_homes (
          supplemented, &disk_node, &requested_relRepos,
          graph, config, prospective_homes )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ? };
      Ok (Definenode_with_Opt_Repomove {
        instruction : DefineNode::Save (SaveNode (supplemented)),
        repo_move : maybe_move,
      }) }}}

/// Under a restricted repo-set, the buffer shows only some of a
/// node's relationship-list members, so its lists describe only the
/// visible subset.  This merges each list with its disk counterpart
/// (TODO/full-schema/9-2_repo-set-safety.org): the anchored
/// 'weave' for the order-meaningful 'contains' and 'subscribes_to',
/// the 'set_difference_merge' for the order-meaningless
/// 'overrides_view_of'.  A field is replaced only when the merge
/// changed it, so an untouched field keeps its MSV shape (and the
/// noop filter can still recognize an unchanged node).
fn preserve_invisible_members (
  mut supplemented : Graphnode,
  disk_node        : &Graphnode,
  graph            : &InRustGraph,
  config           : &SkgConfig,
  active           : &ActiveRepoSet,
) -> Graphnode {
  let member_key = |id : &ID| -> RelationshipMemberKey {
    graph . relationship_member_key (id) };
  let contains_visible = |id : &ID| -> bool {
    disk_node . contains . iter ()
      . find (|member| &member . member == id)
      . is_some_and (|member| relationship_member_is_visible (
        graph, member, config, active)) };
  let subscribes_visible = |id : &ID| -> bool {
    disk_node . subscribes_to . or_default () . iter ()
      .find (|member| &member . member == id)
      .is_some_and (|member| relationship_member_is_visible (
        graph, member, config, active)) };
  let overrides_visible = |id : &ID| -> bool {
    disk_node . overrides_view_of . or_default () . iter ()
      .find (|member| &member . member == id)
      .is_some_and (|member| relationship_member_is_visible (
        graph, member, config, active)) };
  // Rendering may canonicalize a resolvable extra ID to its primary PID.  The
  // comparison key says that is the same relationship, but the disk spelling
  // is load-bearing: restore it before the weave so an untouched round trip
  // cannot rewrite an edge merely because its target was displayed by PID.
  let normalize_to_disk_raw = |buffer : &[ID], disk : &[RelPartner<ID>]| {
    buffer . iter () . map (|id| {
      let key : RelationshipMemberKey = member_key (id);
      disk . iter ()
        . find (|member| member_key (&member . member) == key)
        . map (|member| member . member . clone ())
        . unwrap_or_else (|| id . clone ())
    }) . collect::<Vec<ID>>() };
  let owner_repo : RepoName = supplemented . home_repo . clone ();
  { let disk_contains : Vec<ID> = members_of (&disk_node . contains);
    let buffer_contains : Vec<ID> = normalize_to_disk_raw (
      &members_of (&supplemented . contains), &disk_node . contains);
    let merged : Vec<ID> = weave (
      &disk_contains, &contains_visible,
      &buffer_contains );
    supplemented . contains =
      rel_partners_at_relRepo (&owner_repo, merged); }
  { let disk_subscribes : Vec<ID> =
      members_of (disk_node . subscribes_to . or_default ());
    let submitted_subscribes : Vec<ID> =
      members_of (supplemented . subscribes_to . or_default ());
    let buffer_subscribes : Vec<ID> = normalize_to_disk_raw (
      &submitted_subscribes,
      disk_node . subscribes_to . or_default ());
    let merged : Vec<ID> = weave (
      &disk_subscribes, &subscribes_visible,
      &buffer_subscribes );
    if merged != submitted_subscribes {
      supplemented . subscribes_to =
        MSV::Specified (rel_partners_at_relRepo (&owner_repo, merged)); }}
  { let disk_overrides : Vec<ID> =
      members_of (disk_node . overrides_view_of . or_default ());
    let submitted_overrides : Vec<ID> =
      members_of (supplemented . overrides_view_of . or_default ());
    let buffer_overrides : Vec<ID> = normalize_to_disk_raw (
      &submitted_overrides,
      disk_node . overrides_view_of . or_default ());
    let merged : Vec<ID> = set_difference_merge (
      &disk_overrides, &overrides_visible,
      &buffer_overrides );
    if merged != submitted_overrides {
      supplemented . overrides_view_of =
        MSV::Specified (rel_partners_at_relRepo (&owner_repo, merged)); }}
  supplemented }

/// Deleting a node deletes its whole TELESCOPE, including sections
/// the active repo-set cannot see; refuse rather than silently
/// destroy them. (The agreed small leak: the refusal reveals that
/// inactive sections exist.)
pub fn refuse_delete_with_inactive_sections (
  config : &SkgConfig,
  active : &ActiveRepoSet,
  pid    : &ID,
) -> Result<(), String> {
  for repo_name in config . ordered_repos () {
    if active . contains_repo (&repo_name) { continue; }
    if let Ok (path) = crate::util::path_from_pid_and_repo (
      config, &repo_name, pid . clone () ) {
      if std::path::Path::new (&path) . is_file () {
        return Err ( format! (
          "Cannot delete '{}': it has telescope sections in inactive repos. Widen the repo-set (e.g. to 'all') and retry.",
          pid )); }} }
  Ok (( )) }

/// THE STICKY-ELSE-DEFAULT RULE (5_plan.org, work item
/// save-leveling), extended by an EXPLICIT third path (work item
/// render-and-gating). The lowering stages tag every edge with the
/// node's own repo (a placeholder); this pass resolves the real
/// relRepos:
/// - EXPLICIT: a member named in 'explicit' (the buffer headline's
///   '(relRepo NAME)' atom, threaded in as a side-channel because
///   Graphnode's 'RelPartner::repo' carries no "was this
///   explicit" flag) wins outright, PROVIDED it is at least as
///   private as the DEFAULT floor -- normally the more private of
///   the two endpoints' homes, NOT the disk relRepo. An explicit atom
///   is therefore the one path that can make an existing edge more
///   public, down to but never more public than its default
///   (BUG-and-fix_make-edge-more-public.org). One exception keeps
///   the render->save round-trip lossless: when the DISK relRepo
///   already sits more public than the default (a legacy or
///   hand-authored shape), the explicit floor relaxes to that disk
///   repo -- such an edge can be held or made more private, never
///   moved still more public. A choice more public than its floor
///   fails with a validation error naming the member, offered repo,
///   and floor.
/// - STICKY: absent an explicit repo, an edge that already exists
///   on disk (same relation, same endpoints, through 'pid_of')
///   keeps its DISK relRepo. Renormalization never lowers an edge's
///   privacy silently; removing the atom means "no opinion", not
///   "reset to default".
/// - DEFAULT: a new edge between owned nodes gets the more private
///   endpoint home. If the member is foreign and the owner is owned,
///   the edge instead stays at the owner's home: the relationship
///   and foreign ID are intentionally shared with that repo.
/// - HIDES additionally floor at the most public EXPLAINING
///   subscription (see 'hide_repo'): a hide is only as public as
///   some subscription that makes it meaningful, else it leaks the
///   inference that a private subscription exists. Hides carry no
///   explicit-repo path: the folder that displays them is write-protected
///   (the set-relRepo gesture refuses there).
#[cfg(test)]
pub(crate) fn apply_sticky_relRepos_in_graph (
  supplemented : Graphnode,
  disk_node    : &Graphnode,
  explicit     : &RequestedRelRepos,
  graph        : &InRustGraph,
  config       : &SkgConfig,
) -> Result<Graphnode, String> {
  apply_sticky_relRepos_in_graph_with_prospective_homes (
    supplemented, disk_node, explicit, graph, config, &HashMap::new ()) }

fn apply_sticky_relRepos_in_graph_with_prospective_homes (
  mut supplemented : Graphnode,
  disk_node        : &Graphnode,
  explicit         : &RequestedRelRepos,
  graph            : &InRustGraph,
  config           : &SkgConfig,
  prospective_homes : &HashMap<ID, RepoName>,
) -> Result<Graphnode, String> {
  let owner_pid  : ID         = supplemented . pid    . clone ();
  let owner_home : RepoName = supplemented . home_repo . clone ();
  let resolve = |id : &ID| -> ID {
    graph . pid_of (id)
      . unwrap_or_else ( || id . clone () ) };
  let member_key = |id : &ID| -> RelationshipMemberKey {
    graph . relationship_member_key (id) };
  let home_of = |id : &ID| -> Option<RepoName> {
    prospective_homes . get ( &resolve (id) ) . cloned ()
      .or_else ( || prospective_homes . get (id) . cloned () )
      .or_else ( || graph . pid_and_repo (id)
      . map ( |(_pid, src)| src )
      . or_else ( || home_from_disk (id, config) ) ) };
  // The DEFAULT floor for one member. Owned-to-owned edges use the
  // more private endpoint home. An owned-to-foreign edge stays at
  // the owner's home; Skg never proposes writing a foreign section.
  // An unknown target also falls back to the owner's home.
  let default_floor_for = |member : &ID| -> RepoName {
    match home_of (member) {
      Some (target_home) =>
        config . default_relRepo (
          &owner_home, &target_home ),
      None => owner_home . clone (), }};
  // The sticky-else-default relRepo for one member -- what an ABSENT
  // atom resolves to.
  let sticky_repo_for = |disk_list : &[RelPartner<ID>],
                            member    : &ID|
  -> RepoName {
    let key : RelationshipMemberKey = member_key (member);
    let unclamped : RepoName = 'unclamped : {
      for d in disk_list { // sticky
        if member_key ( &d . member ) == key {
          break 'unclamped d . relRepo . clone (); }}
      default_floor_for (member) };
    // Clamp: no section may be more public than the home (the
    // "extends on the other side" junk shape), so when a HOME MOVE
    // makes the node more private, its edges rise with it. (The
    // converse move leaves old, more-private repos in place:
    // publicizing memberships takes the explicit gesture.)
    config . more_private_of (unclamped, owner_home . clone ()) };
  let raw_disk_member = |disk_list : &[RelPartner<ID>], member : &ID| {
    let key : RelationshipMemberKey = member_key (member);
    disk_list . iter ()
      . find (|disk| member_key (&disk . member) == key)
      . map (|disk| disk . member . clone ())
      . unwrap_or_else (|| member . clone ()) };
  // EXPLICIT wins when at least as private as its floor: the more PUBLIC of the
  // DEFAULT floor and the sticky repo. Flooring at the default
  // (not at sticky) is what lets an atom LOWER a stuck edge's
  // privacy back down to the default
  // (BUG-and-fix_make-edge-more-public.org); admitting the sticky
  // repo when IT sits more public than the default covers legacy
  // or hand-authored data. Render emits the '(relRepo ...)' atom
  // for every off-default edge, and that atom must round-trip through
  // save unchanged. Net: a normal edge never moves more public than
  // the default, and a preexisting more-public edge can only be held
  // or made more private. Absent an atom, sticky-else-default.
  let resolve_repo = |disk_list      : &[RelPartner<ID>],
                        member         : &ID,
                        explicit_here  : &HashMap<ID, RepoName>,
                        relation_label : &str|
  -> Result<RepoName, String> {
    match explicit_here . get (member) {
      Some (repo) => {
        if config . repo_position (repo) . is_none () {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested unconfigured repo '{}'.",
            owner_pid, relation_label, member, repo )); }
        if ! config . user_owns_repo (repo) {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested non-owned repo '{}'. relRepos must be owned.",
            owner_pid, relation_label, member, repo )); }
        if config . is_strictly_more_public (repo, &owner_home) {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested repo '{}', which is more public than the owner's home '{}'.",
            owner_pid, relation_label, member, repo, owner_home )); }
        let default : RepoName = default_floor_for (member);
        let sticky  : RepoName =
          sticky_repo_for (disk_list, member);
        let floor : RepoName = // the more PUBLIC of the two
          if config . is_strictly_more_public (&sticky, &default) {
            sticky } else { default };
        if config . is_strictly_more_public (repo, &floor) {
          Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested \
             repo '{}', but this edge's floor is '{}'. An edge's \
             privacy can never move more public than its applicable \
             default, nor more public than its current relRepo when \
             that repo already precedes the default. To publicize \
             the edge further, first \
             publicize the more private endpoint's home.",
            owner_pid, relation_label, member, repo, floor ))
        } else { Ok ( repo . clone () ) } },
      None => Ok ( sticky_repo_for (disk_list, member) ), }};
  { let disk : &[RelPartner<ID>] = &disk_node . contains;
    for m in supplemented . contains . iter_mut () {
      let submitted : ID = m . member . clone ();
      m . relRepo = resolve_repo (
        disk, &submitted, &explicit . contains, "contains") ?;
      m . member = raw_disk_member (disk, &submitted); }}
  { let disk : &[RelPartner<ID>] =
      disk_node . subscribes_to . or_default ();
    if let MSV::Specified (v) = &mut supplemented . subscribes_to {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        m . relRepo = resolve_repo (
          disk, &submitted, &explicit . subscribes_to,
          "subscribes_to") ?;
        m . member = raw_disk_member (disk, &submitted); }} }
  { let disk : &[RelPartner<ID>] =
      disk_node . overrides_view_of . or_default ();
    if let MSV::Specified (v) = &mut supplemented . overrides_view_of {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        m . relRepo = resolve_repo (
          disk, &submitted, &explicit . overrides_view_of,
          "overrides_view_of") ?;
        m . member = raw_disk_member (disk, &submitted); }} }
  { let disk : &[RelPartner<ID>] =
      disk_node . hides_from_its_subscriptions . or_default ();
    let subscribes : Vec<RelPartner<ID>> =
      supplemented . subscribes_to . or_default () . to_vec ();
    if let MSV::Specified (v) =
      &mut supplemented . hides_from_its_subscriptions {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        let key : RelationshipMemberKey = member_key ( &submitted );
        let sticky : Option<RepoName> =
          disk . iter ()
          . find ( |d| member_key ( &d . member ) == key )
          . map ( |d| d . relRepo . clone () );
        let unclamped : RepoName = match sticky {
          Some (repo) => repo,
          None => hide_repo (
            graph, config, &owner_home, &m . member, &subscribes,
            &resolve, &home_of ), };
        m . relRepo = config . more_private_of (
          unclamped, owner_home . clone () );
        m . member = raw_disk_member (disk, &submitted); }} }
  { // Aliases are relation partners too: explicit request, then
    // sticky repo by alias text, then the owner's home. Their
    // floor is always the owner home because aliases have no target.
    let disk : &[RelPartner<String>] =
      disk_node . aliases . or_default ();
    if let MSV::Specified (v) = &mut supplemented . aliases {
      for m in v . iter_mut () {
        let explicit_repo : Option<&RepoName> =
          explicit . aliases . get (&m . member);
        if let Some (repo) = explicit_repo {
          if config . repo_position (repo) . is_none () {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested unconfigured repo '{}'.",
              owner_pid, m . member, repo )); }
          if ! config . user_owns_repo (repo) {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested non-owned relRepo '{}'. Alias relRepos must be owned.",
              owner_pid, m . member, repo )); }
          if config . is_strictly_more_public (repo, &owner_home) {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested repo '{}' is more public than the owner's home '{}'.",
              owner_pid, m . member, repo, owner_home )); }
          m . relRepo = repo . clone ();
        } else {
          let sticky_or_default : RepoName = disk . iter ()
            . find ( |d| d . member == m . member )
            . map ( |d| d . relRepo . clone () )
            . unwrap_or_else ( || owner_home . clone () );
          m . relRepo = config . more_private_of (
            sticky_or_default, owner_home . clone () ); } }} }
  Ok (supplemented) }

/// A NEW hide's relRepo: at least the more private of the endpoints'
/// homes, and at least the most PUBLIC subscription of the hider
/// that explains it (one whose subscribee contains the hidden
/// node). The most public explanation is the floor because the
/// inference "the hider subscribes to something containing X" is
/// innocent whenever any explanation is visible; with no
/// explanation found, fall back to the most private subscription
/// repo, and with no subscriptions at all, to the endpoint rule
/// alone (junk-tolerant; the validators report residue).
fn hide_repo (
  graph      : &InRustGraph,
  config     : &SkgConfig,
  owner_home : &RepoName,
  hidden     : &ID,
  subscribes : &[RelPartner<ID>],
  resolve    : &dyn Fn (&ID) -> ID,
  home_of    : &dyn Fn (&ID) -> Option<RepoName>,
) -> RepoName {
  let endpoint_floor : RepoName = {
    match home_of (hidden) {
      Some (h) => config . more_private_of (
        owner_home . clone (), h ),
      None => owner_home . clone (), }};
  let hidden_key : ID = resolve (hidden);
  let explaining_repos : Vec<RepoName> = {
    subscribes . iter ()
      . filter ( |sub| {
        graph . pid_of ( & sub . member )
          . and_then ( |p| graph . nodes . get (&p) )
          . map ( |subscribee| subscribee . contains . iter ()
                  . any ( |c| resolve ( &c . member ) == hidden_key ))
          . unwrap_or (false) } )
      . map ( |sub| sub . relRepo . clone () )
      . collect () };
  let subscription_floor : Option<RepoName> =
    explaining_repos . into_iter ()
    . reduce ( |a, b| // keep the more PUBLIC of the two
               if config . is_strictly_more_public (&a, &b) { a }
               else { b } );
  match subscription_floor {
    Some (floor) =>
      config . more_private_of (endpoint_floor, floor),
    None => endpoint_floor, }}

/// Replace buffer's (singleton) ids with disk's (possibly multiple) ids.
pub fn canonicalize_ids_from_disk (
  mut from_buffer : Graphnode,
  disk_node       : &Graphnode,
) -> Result<Graphnode, Box<dyn Error>> {
  for buffer_id in from_buffer . all_ids() {
    let buffer_id : &ID = buffer_id;
    if ! disk_node . all_ids() . any ( |id| id == buffer_id ) {
      return Err(format!(
        "ID '{}' from buffer not found in IDs form disk.",
        buffer_id ) . into() ); }}
  from_buffer . pid = disk_node . pid . clone();
  from_buffer . extra_ids = disk_node . extra_ids . clone();
  Ok (from_buffer) }

/// Return a RepoMove when the repo changes
/// between two owned repos.
pub fn detect_repo_move (
  config        : &SkgConfig,
  pid           : &ID,
  buffer_repo : &RepoName,
  disk_repo   : &RepoName,
) -> Result<Option<RepoMove>, Box<dyn Error>> {
  if buffer_repo == disk_repo {
    return Ok (None); }
  if config . user_owns_repo (disk_repo)
  && config . user_owns_repo (buffer_repo) {
    Ok (Some (RepoMove {
      pid        : pid . clone(),
      old_repo : disk_repo . clone(),
      new_repo : buffer_repo . clone() }))
  } else {
    Err(Box::new(
      BufferValidationError::CannotMoveToOrFromForeignRepo(
        pid . clone(),
        disk_repo . clone(),
        buffer_repo . clone() )) ) }}

/// Fill buffer fields that the buffer left unspecified.
pub fn supplement_unspecified_fields_from_disk (
  mut from_buffer : Graphnode,
  disk_node       : &Graphnode,
) -> Graphnode {
  if from_buffer . aliases . is_unspecified() {
    from_buffer . aliases = disk_node . aliases . clone(); }
  if from_buffer . subscribes_to . is_unspecified() {
    from_buffer . subscribes_to =
      disk_node . subscribes_to . clone(); }
  if from_buffer . hides_from_its_subscriptions . is_unspecified() {
    from_buffer . hides_from_its_subscriptions =
      disk_node . hides_from_its_subscriptions . clone(); }
  if from_buffer . overrides_view_of . is_unspecified() {
    from_buffer . overrides_view_of =
      disk_node . overrides_view_of . clone(); }
  if from_buffer . misc . is_empty() {
    from_buffer . misc = disk_node . misc . clone(); }
  from_buffer }

#[cfg(test)]
#[path = "../../tests/unit/save_leveling.rs"]
mod tests;
