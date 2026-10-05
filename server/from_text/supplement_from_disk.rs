/// PURPOSE:
/// When a Graphnode is created from user input,
/// it might not mention every Graphnode field.
/// If it contains Some([]) for that field,
/// then the user is asking to empty the field.
/// But if it has None for that field,
/// then the field should not be changed --
/// which means it must be read from disk
/// and inserted into the Graphnode.

use crate::from_text::local_fieldintent_collection::lower::{
  RequestedRelRepos, NodeIntent, NodeSaveIntent };
use crate::from_text::weave::{relationship_member_is_visible, set_difference_merge, weave};
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::types::errors::BufferValidationError;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::opt_graphnode_by_skgid;
use crate::types::misc::{ID, MSV, RelPartner, RelationshipMemberKey, SkgConfig, SkgRepoName, members_of, rel_partners_at_relRepo};
use crate::types::phantom::home_from_disk;
use crate::types::nodes::complete::{
  Graphnode, empty_graphnode, set_flag};
use crate::types::save::{NodeInstruction, SaveNode, SkgRepoMove};
use std::collections::HashMap;
use std::error::Error;

pub struct NodeInstructions_with_Repomoves {
  pub instructions : Vec<NodeInstruction>,
  pub skgrepo_moves   : Vec<SkgRepoMove>,
}

struct NodeInstruction_with_Opt_Repomove {
  instruction : NodeInstruction,
  skgrepo_move   : Option<SkgRepoMove>,
}

impl NodeInstructions_with_Repomoves {
  fn with_capacity (
    capacity : usize,
  ) -> NodeInstructions_with_Repomoves {
    NodeInstructions_with_Repomoves {
      instructions : Vec::with_capacity (capacity),
      skgrepo_moves : Vec::new(),
    }}

  fn push (
    &mut self,
    node : NodeInstruction_with_Opt_Repomove,
  ) {
    self . instructions . push (node . instruction);
    if let Some (sm) = node . skgrepo_move {
      let sm : SkgRepoMove = sm;
      self . skgrepo_moves . push (sm); }}
}

pub fn build_diskSupplemented_nodeInstructions (
  intents                : Vec<NodeIntent>,
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  restricted_skgrepo_set : Option<&ActiveSkgRepoSet>, // None means no restriction; callers normalize 'all' to None.
) -> Result<NodeInstructions_with_Repomoves, Box<dyn Error>> {
  let mut result : NodeInstructions_with_Repomoves =
    NodeInstructions_with_Repomoves::with_capacity (intents . len());
  let prospective_homes : HashMap<ID, SkgRepoName> =
    homes_declared_by_nodeSaveIntents (&intents);
  for intent in intents {
    let supplemented : NodeInstruction_with_Opt_Repomove =
      supplement_nodeIntent_from_disk (
        intent, graph, config, restricted_skgrepo_set, &prospective_homes ) ?;
    result . push (supplemented); }
  Ok (result) }

/// Each Save nodeIntent's skgrepo is the node home after this save. Relationship
/// floors must see these homes across the entire batch: a parent may name a
/// child before the child nodeIntent is supplemented, and a same-save home move
/// must make its newly public relationship legal.
fn homes_declared_by_nodeSaveIntents (
  intents : &[NodeIntent],
) -> HashMap<ID, SkgRepoName> {
  let mut homes : HashMap<ID, SkgRepoName> = HashMap::new ();
  for intent in intents {
    let NodeIntent::Save (intent) = intent else { continue; };
    for skgid in std::iter::once (&intent . pid) . chain (
      intent . extra_ids . iter ()) {
      homes . insert (skgid . clone (), intent . home_skgrepo . clone ()); }}
  homes }

fn supplement_nodeIntent_from_disk (
  intent                 : NodeIntent,
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  restricted_skgrepo_set : Option<&ActiveSkgRepoSet>,
  prospective_homes      : &HashMap<ID, SkgRepoName>,
) -> Result<NodeInstruction_with_Opt_Repomove, Box<dyn Error>> {
  match intent {
    NodeIntent::Delete (ref delete) => {
      if let Some (active) = restricted_skgrepo_set {
        refuse_delete_with_inactive_sections (
          config, active, & delete . skgid )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
      Ok (NodeInstruction_with_Opt_Repomove {
        instruction : intent . into_node_instruction()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        skgrepo_move : None,
      }) },
    _ => supplement_nodeSaveIntent_from_disk (
      intent . save_intent()
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      graph, config, restricted_skgrepo_set, prospective_homes ),
  }}

fn supplement_nodeSaveIntent_from_disk (
  from_buffer            : NodeSaveIntent,
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  restricted_skgrepo_set : Option<&ActiveSkgRepoSet>,
  prospective_homes      : &HashMap<ID, SkgRepoName>,
) -> Result<NodeInstruction_with_Opt_Repomove, Box<dyn Error>> {
  let pid : ID =
    from_buffer . pid . clone();
  let from_disk : Option<Graphnode> =
    opt_graphnode_by_skgid (
      graph, config, &pid) ?;
  match from_disk {
    None => {
      // A brand-new node has no sticky skgrepos (no disk relationships to be
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
        set_flag (&mut supplemented . flags, flag, value); }
      let empty_disk : Graphnode = Graphnode {
        pid          : supplemented . pid    . clone (),
        home_skgrepo : supplemented . home_skgrepo . clone (),
        .. empty_graphnode () };
      let supplemented : Graphnode =
        apply_sticky_relRepos_in_graph_with_prospective_homes (
          supplemented, &empty_disk, &requested_relRepos,
          graph, config, prospective_homes )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
      Ok (NodeInstruction_with_Opt_Repomove {
        instruction : NodeInstruction::Save (SaveNode (supplemented)),
        skgrepo_move   : None, } ) },
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
        canonicalize_skgids_from_disk (from_buffer, &disk_node) ?;
      let maybe_move : Option<SkgRepoMove> =
        detect_skgrepo_move ( config,  &pid,
                             &canonicalized . home_skgrepo,
                             &disk_node . home_skgrepo) ?;
      let supplemented : Graphnode = {
        let mut supplemented : Graphnode =
          supplement_unspecified_fields_from_disk (
            canonicalized, &disk_node);
        if let Some ((flag, value)) = flag_request {
          set_flag (&mut supplemented . flags, flag, value); }
        let supplemented : Graphnode =
          match restricted_skgrepo_set {
            None => supplemented,
            Some (active) => preserve_invisible_members (
              supplemented, &disk_node, graph, config, active ) };
        apply_sticky_relRepos_in_graph_with_prospective_homes (
          supplemented, &disk_node, &requested_relRepos,
          graph, config, prospective_homes )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ? };
      Ok (NodeInstruction_with_Opt_Repomove {
        instruction : NodeInstruction::Save (SaveNode (supplemented)),
        skgrepo_move   : maybe_move,
      }) }}}

/// Under a restricted skgrepo-set, the buffer shows only some of a
/// node's relationship-list members, so its lists describe only the
/// visible subset.  This merges each list with its disk counterpart
/// (TODO/DONE/full-schema/DONE/9-2_source-set-safety.org): the anchored
/// 'weave' for the order-meaningful 'contains' and 'subscribesTo',
/// the 'set_difference_merge' for the order-meaningless
/// 'overrides'.  A field is replaced only when the merge
/// changed it, so an untouched field keeps its MSV shape (and the
/// noop filter can still recognize an unchanged node).
fn preserve_invisible_members (
  mut supplemented : Graphnode,
  disk_node        : &Graphnode,
  graph            : &InRustGraph,
  config           : &SkgConfig,
  active           : &ActiveSkgRepoSet,
) -> Graphnode {
  let member_key = |skgid : &ID| -> RelationshipMemberKey {
    graph . relationship_member_key (skgid) };
  let contains_visible = |skgid : &ID| -> bool {
    disk_node . contains . iter ()
      . find (|member| &member . member == skgid)
      . is_some_and (|member| relationship_member_is_visible (
        graph, member, config, active)) };
  let subscribes_visible = |skgid : &ID| -> bool {
    disk_node . subscribesTo . or_default () . iter ()
      .find (|member| &member . member == skgid)
      .is_some_and (|member| relationship_member_is_visible (
        graph, member, config, active)) };
  let overrides_visible = |skgid : &ID| -> bool {
    disk_node . overrides . or_default () . iter ()
      .find (|member| &member . member == skgid)
      .is_some_and (|member| relationship_member_is_visible (
        graph, member, config, active)) };
  // Rendering may canonicalize a resolvable extra ID to its primary PID.  The
  // comparison key says that is the same relationship, but the disk spelling
  // is load-bearing: restore it before the weave so an untouched round trip
  // cannot rewrite a relationship merely because its target was displayed by PID.
  let normalize_to_disk_raw = |buffer : &[ID], disk : &[RelPartner<ID>]| {
    buffer . iter () . map (|skgid| {
      let key : RelationshipMemberKey = member_key (skgid);
      disk . iter ()
        . find (|member| member_key (&member . member) == key)
        . map (|member| member . member . clone ())
        . unwrap_or_else (|| skgid . clone ())
    }) . collect::<Vec<ID>>() };
  let recorder_skgrepo : SkgRepoName = supplemented . home_skgrepo . clone ();
  { let disk_contains : Vec<ID> = members_of (&disk_node . contains);
    let buffer_contains : Vec<ID> = normalize_to_disk_raw (
      &members_of (&supplemented . contains), &disk_node . contains);
    let merged : Vec<ID> = weave (
      &disk_contains, &contains_visible,
      &buffer_contains );
    supplemented . contains =
      rel_partners_at_relRepo (&recorder_skgrepo, merged); }
  { let disk_subscribes : Vec<ID> =
      members_of (disk_node . subscribesTo . or_default ());
    let submitted_subscribes : Vec<ID> =
      members_of (supplemented . subscribesTo . or_default ());
    let buffer_subscribes : Vec<ID> = normalize_to_disk_raw (
      &submitted_subscribes,
      disk_node . subscribesTo . or_default ());
    let merged : Vec<ID> = weave (
      &disk_subscribes, &subscribes_visible,
      &buffer_subscribes );
    if merged != submitted_subscribes {
      supplemented . subscribesTo =
        MSV::Specified (rel_partners_at_relRepo (&recorder_skgrepo, merged)); }}
  { let disk_overrides : Vec<ID> =
      members_of (disk_node . overrides . or_default ());
    let submitted_overrides : Vec<ID> =
      members_of (supplemented . overrides . or_default ());
    let buffer_overrides : Vec<ID> = normalize_to_disk_raw (
      &submitted_overrides,
      disk_node . overrides . or_default ());
    let merged : Vec<ID> = set_difference_merge (
      &disk_overrides, &overrides_visible,
      &buffer_overrides );
    if merged != submitted_overrides {
      supplemented . overrides =
        MSV::Specified (rel_partners_at_relRepo (&recorder_skgrepo, merged)); }}
  supplemented }

/// Deleting a node deletes its whole TELESCOPE, including sections
/// the active skgrepo-set cannot see; refuse rather than silently
/// destroy them. (The agreed small leak: the refusal reveals that
/// inactive sections exist.)
pub fn refuse_delete_with_inactive_sections (
  config : &SkgConfig,
  active : &ActiveSkgRepoSet,
  pid    : &ID,
) -> Result<(), String> {
  for skgrepo_name in config . ordered_skgrepos () {
    if active . contains_skgrepo (&skgrepo_name) { continue; }
    if let Ok (path) = crate::util::path_from_pid_and_skgrepo (
      config, &skgrepo_name, pid . clone () ) {
      if std::path::Path::new (&path) . is_file () {
        return Err ( format! (
          "Cannot delete '{}': it has telescope sections in inactive repos. Widen the repo-set (e.g. to 'all') and retry.",
          pid )); }} }
  Ok (( )) }

/// THE STICKY-ELSE-DEFAULT RULE (5_plan.org, work item
/// save-leveling), extended by an EXPLICIT third path (work item
/// render-and-gating). The lowering stages tag every relationship with the
/// node's own skgrepo (a placeholder); this pass resolves the real
/// relRepos:
/// - EXPLICIT: a member named in 'explicit' (the buffer headline's
///   '(relRepo NAME)' atom, threaded in as a side-channel because
///   Graphnode's 'RelPartner::skgrepo' carries no "was this
///   explicit" flag) wins outright, PROVIDED it is at least as
///   private as the DEFAULT floor -- normally the more private of
///   the two endpoints' homes, NOT the disk relRepo. An explicit atom
///   is therefore the one path that can make an existing relationship more
///   public, down to but never more public than its default
///   (BUG-and-fix_make-edge-more-public.org). One exception keeps
///   the render->save round-trip lossless: when the DISK relRepo
///   already sits more public than the default (a legacy or
///   hand-authored shape), the explicit floor relaxes to that disk
///   skgrepo -- such a relationship can be held or made more private, never
///   moved still more public. A choice more public than its floor
///   fails with a validation error naming the member, offered skgrepo,
///   and floor.
/// - STICKY: absent an explicit skgrepo, a relationship that already exists
///   on disk (same relation, same endpoints, through 'pid_of')
///   keeps its DISK relRepo. Renormalization never lowers a relationship's
///   privacy silently; removing the atom means "no opinion", not
///   "reset to default".
/// - DEFAULT: a new relationship between owned nodes gets the more private
///   endpoint home. If the member is foreign and the recorder is owned,
///   the relationship instead stays at the recorder's home: the relationship
///   and foreign ID are intentionally shared with that skgrepo.
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
  disk_node         : &Graphnode,
  explicit          : &RequestedRelRepos,
  graph             : &InRustGraph,
  config            : &SkgConfig,
  prospective_homes : &HashMap<ID, SkgRepoName>,
) -> Result<Graphnode, String> {
  let recorder_pid  : ID         = supplemented . pid    . clone ();
  let recorder_home : SkgRepoName = supplemented . home_skgrepo . clone ();
  let resolve = |skgid : &ID| -> ID {
    graph . pid_of (skgid)
      . unwrap_or_else ( || skgid . clone () ) };
  let member_key = |skgid : &ID| -> RelationshipMemberKey {
    graph . relationship_member_key (skgid) };
  let home_of = |skgid : &ID| -> Option<SkgRepoName> {
    prospective_homes . get ( &resolve (skgid) ) . cloned ()
      .or_else ( || prospective_homes . get (skgid) . cloned () )
      .or_else ( || graph . pid_and_skgrepo (skgid)
      . map ( |(_pid, src)| src )
      . or_else ( || home_from_disk (skgid, config) ) ) };
  // The DEFAULT floor for one member. Owned-to-owned relationships use the
  // more private endpoint home. An owned-to-foreign relationship stays at
  // the recorder's home; Skg never proposes writing a foreign section.
  // An unknown target also falls back to the recorder's home.
  let default_floor_for = |member : &ID| -> SkgRepoName {
    match home_of (member) {
      Some (target_home) =>
        config . default_relRepo (
          &recorder_home, &target_home ),
      None => recorder_home . clone (), }};
  // The sticky-else-default relRepo for one member -- what an ABSENT
  // atom resolves to.
  let sticky_skgrepo_for = |disk_list : &[RelPartner<ID>],
                            member    : &ID|
  -> SkgRepoName {
    let key : RelationshipMemberKey = member_key (member);
    let unclamped : SkgRepoName = 'unclamped : {
      for d in disk_list { // sticky
        if member_key ( &d . member ) == key {
          break 'unclamped d . relRepo . clone (); }}
      default_floor_for (member) };
    // Clamp: no section may be more public than the home (the
    // "extends on the other side" junk shape), so when a HOME MOVE
    // makes the node more private, its relationships rise with it. (The
    // converse move leaves old, more-private skgrepos in place:
    // publicizing memberships takes the explicit gesture.)
    config . more_private_of (unclamped, recorder_home . clone ()) };
  let raw_disk_member = |disk_list : &[RelPartner<ID>], member : &ID| {
    let key : RelationshipMemberKey = member_key (member);
    disk_list . iter ()
      . find (|disk| member_key (&disk . member) == key)
      . map (|disk| disk . member . clone ())
      . unwrap_or_else (|| member . clone ()) };
  // EXPLICIT wins when at least as private as its floor: the more PUBLIC of the
  // DEFAULT floor and the sticky skgrepo. Flooring at the default
  // (not at sticky) is what lets an atom LOWER a stuck relationship's
  // privacy back down to the default
  // (BUG-and-fix_make-edge-more-public.org); admitting the sticky
  // skgrepo when IT sits more public than the default covers legacy
  // or hand-authored data. Render emits the '(relRepo ...)' atom
  // for every off-default relationship, and that atom must round-trip through
  // save unchanged. Net: a normal relationship never moves more public than
  // the default, and a preexisting more-public relationship can only be held
  // or made more private. Absent an atom, sticky-else-default.
  let resolve_skgrepo = |disk_list      : &[RelPartner<ID>],
                        member         : &ID,
                        explicit_here  : &HashMap<ID, SkgRepoName>,
                        relation_label : &str|
  -> Result<SkgRepoName, String> {
    match explicit_here . get (member) {
      Some (skgrepo) => {
        if config . skgrepo_position (skgrepo) . is_none () {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested unconfigured repo '{}'.",
            recorder_pid, relation_label, member, skgrepo )); }
        if ! config . skgrepo_is_owned (skgrepo) {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested non-owned repo '{}'. relRepos must be owned.",
            recorder_pid, relation_label, member, skgrepo )); }
        if config . is_strictly_more_public (skgrepo, &recorder_home) {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested repo '{}', which is more public than the recorder's home '{}'.",
            recorder_pid, relation_label, member, skgrepo, recorder_home )); }
        let default : SkgRepoName = default_floor_for (member);
        let sticky  : SkgRepoName =
          sticky_skgrepo_for (disk_list, member);
        let floor : SkgRepoName = // the more PUBLIC of the two
          if config . is_strictly_more_public (&sticky, &default) {
            sticky } else { default };
        if config . is_strictly_more_public (skgrepo, &floor) {
          Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested \
             repo '{}', but this relationship's floor is '{}'. An relationship's \
             privacy can never move more public than its applicable \
             default, nor more public than its current relRepo when \
             that repo already precedes the default. To publicize \
             the relationship further, first \
             publicize the more private endpoint's home.",
            recorder_pid, relation_label, member, skgrepo, floor ))
        } else { Ok ( skgrepo . clone () ) } },
      None => Ok ( sticky_skgrepo_for (disk_list, member) ), }};
  { let disk : &[RelPartner<ID>] = &disk_node . contains;
    for m in supplemented . contains . iter_mut () {
      let submitted : ID = m . member . clone ();
      m . relRepo = resolve_skgrepo (
        disk, &submitted, &explicit . contains, "contains") ?;
      m . member = raw_disk_member (disk, &submitted); }}
  { let disk : &[RelPartner<ID>] =
      disk_node . subscribesTo . or_default ();
    if let MSV::Specified (v) = &mut supplemented . subscribesTo {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        m . relRepo = resolve_skgrepo (
          disk, &submitted, &explicit . subscribesTo,
          "subscribesTo") ?;
        m . member = raw_disk_member (disk, &submitted); }} }
  { let disk : &[RelPartner<ID>] =
      disk_node . overrides . or_default ();
    if let MSV::Specified (v) = &mut supplemented . overrides {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        m . relRepo = resolve_skgrepo (
          disk, &submitted, &explicit . overrides,
          "overrides") ?;
        m . member = raw_disk_member (disk, &submitted); }} }
  { let disk : &[RelPartner<ID>] =
      disk_node . hidesFromSubs . or_default ();
    let subscribes : Vec<RelPartner<ID>> =
      supplemented . subscribesTo . or_default () . to_vec ();
    if let MSV::Specified (v) =
      &mut supplemented . hidesFromSubs {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        let key : RelationshipMemberKey = member_key ( &submitted );
        let sticky : Option<SkgRepoName> =
          disk . iter ()
          . find ( |d| member_key ( &d . member ) == key )
          . map ( |d| d . relRepo . clone () );
        let unclamped : SkgRepoName = match sticky {
          Some (skgrepo) => skgrepo,
          None => hide_skgrepo (
            graph, config, &recorder_home, &m . member, &subscribes,
            &resolve, &home_of ), };
        m . relRepo = config . more_private_of (
          unclamped, recorder_home . clone () );
        m . member = raw_disk_member (disk, &submitted); }} }
  { // Aliases are relation partners too: explicit request, then
    // sticky skgrepo by alias text, then the recorder's home. Their
    // floor is always the recorder home because aliases have no target.
    let disk : &[RelPartner<String>] =
      disk_node . aliases . or_default ();
    if let MSV::Specified (v) = &mut supplemented . aliases {
      for m in v . iter_mut () {
        let explicit_skgrepo : Option<&SkgRepoName> =
          explicit . aliases . get (&m . member);
        if let Some (skgrepo) = explicit_skgrepo {
          if config . skgrepo_position (skgrepo) . is_none () {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested unconfigured repo '{}'.",
              recorder_pid, m . member, skgrepo )); }
          if ! config . skgrepo_is_owned (skgrepo) {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested non-owned relRepo '{}'. Alias relRepos must be owned.",
              recorder_pid, m . member, skgrepo )); }
          if config . is_strictly_more_public (skgrepo, &recorder_home) {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested repo '{}' is more public than the recorder's home '{}'.",
              recorder_pid, m . member, skgrepo, recorder_home )); }
          m . relRepo = skgrepo . clone ();
        } else {
          let sticky_or_default : SkgRepoName = disk . iter ()
            . find ( |d| d . member == m . member )
            . map ( |d| d . relRepo . clone () )
            . unwrap_or_else ( || recorder_home . clone () );
          m . relRepo = config . more_private_of (
            sticky_or_default, recorder_home . clone () ); } }} }
  Ok (supplemented) }

/// A NEW hide's relRepo: at least the more private of the endpoints'
/// homes, and at least the most PUBLIC subscription of the hider
/// that explains it (one whose subscribee contains the hidden
/// node). The most public explanation is the floor because the
/// inference "the hider subscribes to something containing X" is
/// innocent whenever any explanation is visible; with no
/// explanation found, fall back to the most private subscription
/// skgrepo, and with no subscriptions at all, to the endpoint rule
/// alone (junk-tolerant; the validators report residue).
fn hide_skgrepo (
  graph         : &InRustGraph,
  config        : &SkgConfig,
  recorder_home : &SkgRepoName,
  hidden        : &ID,
  subscribes    : &[RelPartner<ID>],
  resolve       : &dyn Fn (&ID) -> ID,
  home_of       : &dyn Fn (&ID) -> Option<SkgRepoName>,
) -> SkgRepoName {
  let endpoint_floor : SkgRepoName = {
    match home_of (hidden) {
      Some (h) => config . more_private_of (
        recorder_home . clone (), h ),
      None => recorder_home . clone (), }};
  let hidden_key : ID = resolve (hidden);
  let explaining_skgrepos : Vec<SkgRepoName> = {
    subscribes . iter ()
      . filter ( |sub| {
        graph . pid_of ( & sub . member )
          . and_then ( |p| graph . nodes . get (&p) )
          . map ( |subscribee| subscribee . contains . iter ()
                  . any ( |c| resolve ( &c . member ) == hidden_key ))
          . unwrap_or (false) } )
      . map ( |sub| sub . relRepo . clone () )
      . collect () };
  let subscription_floor : Option<SkgRepoName> =
    explaining_skgrepos . into_iter ()
    . reduce ( |a, b| // keep the more PUBLIC of the two
               if config . is_strictly_more_public (&a, &b) { a }
               else { b } );
  match subscription_floor {
    Some (floor) =>
      config . more_private_of (endpoint_floor, floor),
    None => endpoint_floor, }}

/// Replace buffer's (singleton) ids with disk's (possibly multiple) ids.
pub fn canonicalize_skgids_from_disk (
  mut from_buffer : Graphnode,
  disk_node       : &Graphnode,
) -> Result<Graphnode, Box<dyn Error>> {
  for buffer_skgid in from_buffer . all_skgids() {
    let buffer_skgid : &ID = buffer_skgid;
    if ! disk_node . all_skgids() . any ( |skgid| skgid == buffer_skgid ) {
      return Err(format!(
        "ID '{}' from buffer not found in IDs form disk.",
        buffer_skgid ) . into() ); }}
  from_buffer . pid = disk_node . pid . clone();
  from_buffer . extra_ids = disk_node . extra_ids . clone();
  Ok (from_buffer) }

/// Return a RepoMove when the skgrepo changes
/// between two owned skgrepos.
pub fn detect_skgrepo_move (
  config         : &SkgConfig,
  pid            : &ID,
  buffer_skgrepo : &SkgRepoName,
  disk_skgrepo   : &SkgRepoName,
) -> Result<Option<SkgRepoMove>, Box<dyn Error>> {
  if buffer_skgrepo == disk_skgrepo {
    return Ok (None); }
  if config . skgrepo_is_owned (disk_skgrepo)
  && config . skgrepo_is_owned (buffer_skgrepo) {
    Ok (Some (SkgRepoMove {
      pid         : pid . clone(),
      old_skgrepo : disk_skgrepo . clone(),
      new_skgrepo : buffer_skgrepo . clone() }))
  } else {
    Err(Box::new(
      BufferValidationError::CannotMoveToOrFromForeignSkgRepo(
        pid . clone(),
        disk_skgrepo . clone(),
        buffer_skgrepo . clone() )) ) }}

/// Fill buffer fields that the buffer left unspecified.
pub fn supplement_unspecified_fields_from_disk (
  mut from_buffer : Graphnode,
  disk_node       : &Graphnode,
) -> Graphnode {
  if from_buffer . aliases . is_unspecified() {
    from_buffer . aliases = disk_node . aliases . clone(); }
  if from_buffer . subscribesTo . is_unspecified() {
    from_buffer . subscribesTo =
      disk_node . subscribesTo . clone(); }
  if from_buffer . hidesFromSubs . is_unspecified() {
    from_buffer . hidesFromSubs =
      disk_node . hidesFromSubs . clone(); }
  if from_buffer . overrides . is_unspecified() {
    from_buffer . overrides =
      disk_node . overrides . clone(); }
  if from_buffer . flags . is_empty() {
    from_buffer . flags = disk_node . flags . clone(); }
  from_buffer }

#[cfg(test)]
#[path = "../../tests/unit/save_leveling.rs"]
mod tests;
