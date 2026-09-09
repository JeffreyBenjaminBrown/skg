/// PURPOSE:
/// When a NodeComplete is created from user input,
/// it might not mention every NodeComplete field.
/// If it contains Some([]) for that field,
/// then the user is asking to empty the field.
/// But if it has None for that field,
/// then the field should not be changed --
/// which means it must be read from disk
/// and inserted into the NodeComplete.

use crate::dbs::node_lookup::optNodeComplete_rustFIrst_by_id;
use crate::from_text::local_instruction_collection::lower::{
  ExplicitSources, NodeIntent, NodeSaveIntent };
use crate::from_text::weave::{member_is_visible, set_difference_merge, weave};
use crate::source_sets::ActiveSourceSet;
use crate::types::errors::BufferValidationError;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, MSV, MemberAtSource, SkgConfig, SourceName, members_of, members_at_source};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::store_state::SelectedPathManifest;
use crate::util::path_from_pid_and_source;
use std::collections::HashMap;
use std::error::Error;
use std::path::Path;

pub struct Definenodes_with_Sourcemoves {
  pub instructions : Vec<DefineNode>,
  pub source_moves : Vec<SourceMove>,
}

struct Definenode_with_Opt_Sourcemove {
  instruction : DefineNode,
  source_move : Option<SourceMove>,
}

impl Definenodes_with_Sourcemoves {
  fn with_capacity (
    capacity : usize,
  ) -> Definenodes_with_Sourcemoves {
    Definenodes_with_Sourcemoves {
      instructions : Vec::with_capacity (capacity),
      source_moves : Vec::new(),
    }}

  fn push (
    &mut self,
    node : Definenode_with_Opt_Sourcemove,
  ) {
    self . instructions . push (node . instruction);
    if let Some (sm) = node . source_move {
      let sm : SourceMove = sm;
      self . source_moves . push (sm); }}
}

pub async fn build_diskSupplemented_defineNodes (
  graph : &InRustGraph,
  intents : Vec<NodeIntent>,
  config  : &SkgConfig,
  restricted_source_set : Option<&ActiveSourceSet>, // None means no restriction; callers normalize 'all' to None.
  selected_manifest : &SelectedPathManifest,
) -> Result<Definenodes_with_Sourcemoves, Box<dyn Error>> {
  let mut result : Definenodes_with_Sourcemoves =
    Definenodes_with_Sourcemoves::with_capacity (intents . len());
  for intent in intents {
    let supplemented : Definenode_with_Opt_Sourcemove =
      supplement_nodeeditintent_from_disk (graph,
        intent, config, restricted_source_set,
        selected_manifest ) . await ?;
    result . push (supplemented); }
  Ok (result) }

async fn supplement_nodeeditintent_from_disk (
  graph : &InRustGraph,
  intent : NodeIntent,
  config : &SkgConfig,
  restricted_source_set : Option<&ActiveSourceSet>,
  selected_manifest : &SelectedPathManifest,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  match intent {
    NodeIntent::Delete (ref delete) => {
      if let Some (active) = restricted_source_set {
        refuse_delete_with_inactive_sections (
          config, active, selected_manifest, & delete . id )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : intent . into_define_node()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        source_move : None,
      }) },
    _ => supplement_saveintent_from_disk (graph,
      intent . save_intent()
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      config, restricted_source_set ) . await,
  }}

async fn supplement_saveintent_from_disk (
  graph : &InRustGraph,
  from_buffer : NodeSaveIntent,
  config      : &SkgConfig,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  let pid : ID =
    from_buffer . pid . clone();
  let from_disk : Option<NodeComplete> =
    optNodeComplete_rustFIrst_by_id (graph, &pid) . await ?;
  match from_disk {
    None => {
      // A brand-new node has no sticky sources (no disk edges to be
      // sticky about), but an explicit '(relSource ...)' atom must
      // still be validated against the DEFAULT floor -- an empty
      // disk stand-in reuses 'apply_sticky_sources' unchanged (its
      // sticky lookups simply find nothing, falling through to
      // default every time).
      let explicit_sources : ExplicitSources =
        from_buffer . explicit_sources ();
      let supplemented : NodeComplete =
        from_buffer . into_nodecomplete ();
      let empty_disk : NodeComplete = NodeComplete {
        pid    : supplemented . pid    . clone (),
        source : supplemented . source . clone (),
        .. empty_node_complete () };
      let supplemented : NodeComplete =
        apply_sticky_sources (graph,
          supplemented, &empty_disk, &explicit_sources, config )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : DefineNode::Save (SaveNode (supplemented)),
        source_move : None, } ) },
    Some (disk_node) => {
      let disk_node : NodeComplete = disk_node;
      let mut from_buffer : NodeSaveIntent = from_buffer;
      from_buffer . fill_unspecified_contains (
        &members_of (&disk_node . contains));
      let explicit_sources : ExplicitSources =
        from_buffer . explicit_sources ();
      let from_buffer : NodeComplete =
        from_buffer . into_nodecomplete();
      let canonicalized : NodeComplete =
        canonicalize_ids_from_disk (from_buffer, &disk_node) ?;
      let maybe_move : Option<SourceMove> =
        detect_source_move ( config,  &pid,
                             &canonicalized . source,
                             &disk_node . source) ?;
      let supplemented : NodeComplete = {
        let supplemented : NodeComplete =
          supplement_unspecified_fields_from_disk (
            canonicalized, &disk_node);
        let supplemented : NodeComplete =
          match restricted_source_set {
            None => supplemented,
            Some (active) => preserve_invisible_members (graph,
              supplemented, &disk_node, config, active ) };
        apply_sticky_sources (graph,
          supplemented, &disk_node, &explicit_sources, config )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ? };
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : DefineNode::Save (SaveNode (supplemented)),
        source_move : maybe_move,
      }) }}}

/// Under a restricted source-set, the buffer shows only some of a
/// node's relationship-list members, so its lists describe only the
/// visible subset.  This merges each list with its disk counterpart
/// (TODO/full-schema/9-2_source-set-safety.org): the anchored
/// 'weave' for the order-meaningful 'contains' and 'subscribes_to',
/// the 'set_difference_merge' for the order-meaningless
/// 'overrides_view_of'.  A field is replaced only when the merge
/// changed it, so an untouched field keeps its MSV shape (and the
/// noop filter can still recognize an unchanged node).
fn preserve_invisible_members (
  graph : &InRustGraph,
  mut supplemented : NodeComplete,
  disk_node        : &NodeComplete,
  config           : &SkgConfig,
  active           : &ActiveSourceSet,
) -> NodeComplete {
  let is_visible = |id : &ID| -> bool {
    member_is_visible (graph, id, config, active) };
  let owner_source : SourceName = supplemented . source . clone ();
  { let disk_contains : Vec<ID> = members_of (&disk_node . contains);
    let buffer_contains : Vec<ID> = members_of (&supplemented . contains);
    let merged : Vec<ID> = weave (
      &disk_contains, &is_visible,
      &buffer_contains );
    supplemented . contains =
      members_at_source (&owner_source, merged); }
  { let disk_subscribes : Vec<ID> =
      members_of (disk_node . subscribes_to . or_default ());
    let buffer_subscribes : Vec<ID> =
      members_of (supplemented . subscribes_to . or_default ());
    let merged : Vec<ID> = weave (
      &disk_subscribes, &is_visible,
      &buffer_subscribes );
    if merged != buffer_subscribes {
      supplemented . subscribes_to =
        MSV::Specified (members_at_source (&owner_source, merged)); }}
  { let disk_overrides : Vec<ID> =
      members_of (disk_node . overrides_view_of . or_default ());
    let buffer_overrides : Vec<ID> =
      members_of (supplemented . overrides_view_of . or_default ());
    let merged : Vec<ID> = set_difference_merge (
      &disk_overrides, &is_visible,
      &buffer_overrides );
    if merged != buffer_overrides {
      supplemented . overrides_view_of =
        MSV::Specified (members_at_source (&owner_source, merged)); }}
  supplemented }

/// Deleting a node deletes its whole TELESCOPE, including sections
/// the active source-set cannot see; refuse rather than silently
/// destroy them. (The agreed small leak: the refusal reveals that
/// inactive sections exist.)
pub fn refuse_delete_with_inactive_sections (
  config            : &SkgConfig,
  active            : &ActiveSourceSet,
  selected_manifest : &SelectedPathManifest,
  pid               : &ID,
) -> Result<(), String> {
  for source_name in config . ordered_sources () {
    if active . contains_source (&source_name) { continue; }
    if let Ok (path) = path_from_pid_and_source (
      config, &source_name, pid . clone () ) {
      if selected_manifest . contains_key (
        Path::new (&path) ) {
        return Err ( format! (
          "Cannot delete '{}': it has telescope sections in inactive sources. Widen the source-set (e.g. to 'all') and retry.",
          pid )); }} }
  Ok (( )) }

/// THE STICKY-ELSE-DEFAULT RULE (5_plan.org, work item
/// save-leveling), extended by an EXPLICIT third path (work item
/// render-and-gating). The lowering stages tag every edge with the
/// node's own source (a placeholder); this pass resolves the real
/// recording sources:
/// - EXPLICIT: a member named in 'explicit' (the buffer headline's
///   '(relSource NAME)' atom, threaded in as a side-channel because
///   NodeComplete's 'MemberAtSource::source' carries no "was this
///   explicit" flag) wins outright, PROVIDED it is at least as
///   private as the DEFAULT floor -- normally the more private of
///   the two endpoints' homes, NOT the disk source. An explicit atom
///   is therefore the one path that can make an existing edge more
///   public, down to but never more public than its default
///   (BUG-and-fix_make-edge-more-public.org). One exception keeps
///   the render->save round-trip lossless: when the DISK source
///   already sits more public than the default (a legacy or
///   hand-authored shape), the explicit floor relaxes to that disk
///   source -- such an edge can be held or made more private, never
///   moved still more public. A choice more public than its floor
///   fails with a validation error naming the member, offered source,
///   and floor.
/// - STICKY: absent an explicit source, an edge that already exists
///   on disk (same relation, same endpoints, through 'pid_of')
///   keeps its DISK source. Renormalization never lowers an edge's
///   privacy silently; removing the atom means "no opinion", not
///   "reset to default".
/// - DEFAULT: a new edge between owned nodes gets the more private
///   endpoint home. If the member is foreign and the owner is owned,
///   the edge instead stays at the owner's home: the relationship
///   and foreign ID are intentionally shared with that source.
/// - HIDES additionally floor at the most public EXPLAINING
///   subscription (see 'hide_source'): a hide is only as public as
///   some subscription that makes it meaningful, else it leaks the
///   inference that a private subscription exists. Hides carry no
///   explicit-source path: the col that displays them is read-only
///   (the set-relationship-source gesture refuses there).
pub(crate) fn apply_sticky_sources (
  graph : &InRustGraph,
  mut supplemented : NodeComplete,
  disk_node        : &NodeComplete,
  explicit         : &ExplicitSources,
  config           : &SkgConfig,
) -> Result<NodeComplete, String> {
  let owner_pid  : ID         = supplemented . pid    . clone ();
  let owner_home : SourceName = supplemented . source . clone ();
  let resolve = |id : &ID| -> ID {
    graph . pid_of (id)
      . unwrap_or_else ( || id . clone () ) };
  let home_of = |id : &ID| -> Option<SourceName> {
    graph . pid_and_source (id) . map (|(_pid, src)| src) };
  // The DEFAULT floor for one member. Owned-to-owned edges use the
  // more private endpoint home. An owned-to-foreign edge stays at
  // the owner's home; Skg never proposes writing a foreign section.
  // An unknown target also falls back to the owner's home.
  let default_floor_for = |member : &ID| -> SourceName {
    match home_of (member) {
      Some (target_home) =>
        config . relationship_default_source (
          &owner_home, &target_home ),
      None => owner_home . clone (), }};
  // The sticky-else-default source for one member -- what an ABSENT
  // atom resolves to.
  let sticky_source_for = |disk_list : &[MemberAtSource<ID>],
                            member    : &ID|
  -> SourceName {
    let key : ID = resolve (member);
    let unclamped : SourceName = 'unclamped : {
      for d in disk_list { // sticky
        if resolve ( &d . member ) == key {
          break 'unclamped d . source . clone (); }}
      default_floor_for (member) };
    // Clamp: no section may be more public than the home (the
    // "extends on the other side" junk shape), so when a HOME MOVE
    // makes the node more private, its edges rise with it. (The
    // converse move leaves old, more-private sources in place:
    // publicizing memberships takes the explicit gesture.)
    config . more_private_of (unclamped, owner_home . clone ()) };
  // EXPLICIT wins when at least as private as its floor: the more PUBLIC of the
  // DEFAULT floor and the sticky source. Flooring at the default
  // (not at sticky) is what lets an atom LOWER a stuck edge's
  // privacy back down to the default
  // (BUG-and-fix_make-edge-more-public.org); admitting the sticky
  // source when IT sits more public than the default covers legacy
  // or hand-authored data. Render emits the '(relSource ...)' atom
  // for every off-default edge, and that atom must round-trip through
  // save unchanged. Net: a normal edge never moves more public than
  // the default, and a preexisting more-public edge can only be held
  // or made more private. Absent an atom, sticky-else-default.
  let resolve_source = |disk_list      : &[MemberAtSource<ID>],
                        member         : &ID,
                        explicit_here  : &HashMap<ID, SourceName>,
                        relation_label : &str|
  -> Result<SourceName, String> {
    match explicit_here . get (member) {
      Some (source) => {
        if config . source_position (source) . is_none () {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested unconfigured source '{}'.",
            owner_pid, relation_label, member, source )); }
        if ! config . user_owns_source (source) {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested non-owned source '{}'. Relationship sources must be owned.",
            owner_pid, relation_label, member, source )); }
        if config . is_strictly_more_public (source, &owner_home) {
          return Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested source '{}', which is more public than the owner's home '{}'.",
            owner_pid, relation_label, member, source, owner_home )); }
        let default : SourceName = default_floor_for (member);
        let sticky  : SourceName =
          sticky_source_for (disk_list, member);
        let floor : SourceName = // the more PUBLIC of the two
          if config . is_strictly_more_public (&sticky, &default) {
            sticky } else { default };
        if config . is_strictly_more_public (source, &floor) {
          Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested \
             source '{}', but this edge's floor is '{}'. An edge's \
             privacy can never move more public than its applicable \
             default, nor more public than its current source when \
             that source already precedes the default. To publicize \
             the edge further, first \
             publicize the more private endpoint's home.",
            owner_pid, relation_label, member, source, floor ))
        } else { Ok ( source . clone () ) } },
      None => Ok ( sticky_source_for (disk_list, member) ), }};
  { let disk : &[MemberAtSource<ID>] = &disk_node . contains;
    for m in supplemented . contains . iter_mut () {
      m . source = resolve_source (
        disk, &m . member, &explicit . contains, "contains") ?; }}
  { let disk : &[MemberAtSource<ID>] =
      disk_node . subscribes_to . or_default ();
    if let MSV::Specified (v) = &mut supplemented . subscribes_to {
      for m in v . iter_mut () {
        m . source = resolve_source (
          disk, &m . member, &explicit . subscribes_to,
          "subscribes_to") ?; }} }
  { let disk : &[MemberAtSource<ID>] =
      disk_node . overrides_view_of . or_default ();
    if let MSV::Specified (v) = &mut supplemented . overrides_view_of {
      for m in v . iter_mut () {
        m . source = resolve_source (
          disk, &m . member, &explicit . overrides_view_of,
          "overrides_view_of") ?; }} }
  { let disk : &[MemberAtSource<ID>] =
      disk_node . hides_from_its_subscriptions . or_default ();
    let subscribes : Vec<MemberAtSource<ID>> =
      supplemented . subscribes_to . or_default () . to_vec ();
    if let MSV::Specified (v) =
      &mut supplemented . hides_from_its_subscriptions {
      for m in v . iter_mut () {
        let key : ID = resolve ( &m . member );
        let sticky : Option<SourceName> =
          disk . iter ()
          . find ( |d| resolve ( &d . member ) == key )
          . map ( |d| d . source . clone () );
        let unclamped : SourceName = match sticky {
          Some (source) => source,
          None => hide_source (graph,
            config, &owner_home, &m . member, &subscribes,
            &resolve ), };
        m . source = config . more_private_of (
          unclamped, owner_home . clone () ); }} }
  { // Aliases are members at sources too: explicit request, then
    // sticky source by alias text, then the owner's home. Their
    // floor is always the owner home because aliases have no target.
    let disk : &[MemberAtSource<String>] =
      disk_node . aliases . or_default ();
    if let MSV::Specified (v) = &mut supplemented . aliases {
      for m in v . iter_mut () {
        let explicit_source : Option<&SourceName> =
          explicit . aliases . get (&m . member);
        if let Some (source) = explicit_source {
          if config . source_position (source) . is_none () {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested unconfigured source '{}'.",
              owner_pid, m . member, source )); }
          if ! config . user_owns_source (source) {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested non-owned source '{}'. Alias sources must be owned.",
              owner_pid, m . member, source )); }
          if config . is_strictly_more_public (source, &owner_home) {
            return Err ( format! (
              "Cannot save {} (alias '{}'): requested source '{}' is more public than the owner's home '{}'.",
              owner_pid, m . member, source, owner_home )); }
          m . source = source . clone ();
        } else {
          let sticky_or_default : SourceName = disk . iter ()
            . find ( |d| d . member == m . member )
            . map ( |d| d . source . clone () )
            . unwrap_or_else ( || owner_home . clone () );
          m . source = config . more_private_of (
            sticky_or_default, owner_home . clone () ); } }} }
  Ok (supplemented) }

/// A NEW hide's recording source: at least the more private of the endpoints'
/// homes, and at least the most PUBLIC subscription of the hider
/// that explains it (one whose subscribee contains the hidden
/// node). The most public explanation is the floor because the
/// inference "the hider subscribes to something containing X" is
/// innocent whenever any explanation is visible; with no
/// explanation found, fall back to the most private subscription
/// source, and with no subscriptions at all, to the endpoint rule
/// alone (junk-tolerant; the validators report residue).
fn hide_source (
  graph : &InRustGraph,
  config     : &SkgConfig,
  owner_home : &SourceName,
  hidden     : &ID,
  subscribes : &[MemberAtSource<ID>],
  resolve    : &dyn Fn (&ID) -> ID,
) -> SourceName {
  let endpoint_floor : SourceName = {
    let target_home : Option<SourceName> =
      graph . pid_and_source (hidden) . map (|(_pid, src)| src);
    match target_home {
      Some (h) => config . more_private_of (
        owner_home . clone (), h ),
      None => owner_home . clone (), }};
  let hidden_key : ID = resolve (hidden);
  let explaining_sources : Vec<SourceName> = {
    subscribes . iter ()
      . filter ( |sub| {
        graph . pid_of ( & sub . member )
          . and_then ( |p| graph . nodes . get (&p) )
          . map ( |subscribee| subscribee . contains . iter ()
                  . any ( |c| resolve ( &c . member ) == hidden_key ))
          . unwrap_or (false) } )
      . map ( |sub| sub . source . clone () )
      . collect () };
  let subscription_floor : Option<SourceName> =
    explaining_sources . into_iter ()
    . reduce ( |a, b| // keep the more PUBLIC of the two
               if config . is_strictly_more_public (&a, &b) { a }
               else { b } );
  match subscription_floor {
    Some (floor) =>
      config . more_private_of (endpoint_floor, floor),
    None => endpoint_floor, }}

/// Replace buffer's (singleton) ids with disk's (possibly multiple) ids.
pub fn canonicalize_ids_from_disk (
  mut from_buffer : NodeComplete,
  disk_node       : &NodeComplete,
) -> Result<NodeComplete, Box<dyn Error>> {
  for buffer_id in from_buffer . all_ids() {
    let buffer_id : &ID = buffer_id;
    if ! disk_node . all_ids() . any ( |id| id == buffer_id ) {
      return Err(format!(
        "ID '{}' from buffer not found in IDs form disk.",
        buffer_id ) . into() ); }}
  from_buffer . pid = disk_node . pid . clone();
  from_buffer . extra_ids = disk_node . extra_ids . clone();
  Ok (from_buffer) }

/// Return a SourceMove when the source changes
/// between two owned sources.
pub fn detect_source_move (
  config        : &SkgConfig,
  pid           : &ID,
  buffer_source : &SourceName,
  disk_source   : &SourceName,
) -> Result<Option<SourceMove>, Box<dyn Error>> {
  if buffer_source == disk_source {
    return Ok (None); }
  if config . user_owns_source (disk_source)
  && config . user_owns_source (buffer_source) {
    Ok (Some (SourceMove {
      pid        : pid . clone(),
      old_source : disk_source . clone(),
      new_source : buffer_source . clone() }))
  } else {
    Err(Box::new(
      BufferValidationError::CannotMoveToOrFromForeignSource(
        pid . clone(),
        disk_source . clone(),
        buffer_source . clone() )) ) }}

/// Fill buffer fields that the buffer left unspecified.
pub fn supplement_unspecified_fields_from_disk (
  mut from_buffer : NodeComplete,
  disk_node       : &NodeComplete,
) -> NodeComplete {
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
