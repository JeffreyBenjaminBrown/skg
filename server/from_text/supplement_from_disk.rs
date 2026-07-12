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
  ExplicitLevels, NodeIntent, NodeSaveIntent };
use crate::from_text::weave::{member_is_visible, set_difference_merge, weave};
use crate::source_sets::ActiveSourceSet;
use crate::types::errors::BufferValidationError;
use crate::dbs::in_rust_graph::snapshot_global;
use crate::types::misc::{ID, MSV, PrivaciedMember, SkgConfig, SourceName, members_of, privacied_all};
use crate::types::phantom::source_from_disk;
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

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
  intents : Vec<NodeIntent>,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>, // None means no restriction; callers normalize 'all' to None.
) -> Result<Definenodes_with_Sourcemoves, Box<dyn Error>> {
  let mut result : Definenodes_with_Sourcemoves =
    Definenodes_with_Sourcemoves::with_capacity (intents . len());
  for intent in intents {
    let supplemented : Definenode_with_Opt_Sourcemove =
      supplement_nodeeditintent_from_disk (
        intent, config, driver, restricted_source_set ) . await ?;
    result . push (supplemented); }
  Ok (result) }

async fn supplement_nodeeditintent_from_disk (
  intent : NodeIntent,
  config : &SkgConfig,
  driver : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  match intent {
    NodeIntent::Delete (ref delete) => {
      if let Some (active) = restricted_source_set {
        refuse_delete_with_inactive_sections (
          config, active, & delete . id )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : intent . into_define_node()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        source_move : None,
      }) },
    _ => supplement_saveintent_from_disk (
      intent . save_intent()
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      config, driver, restricted_source_set ) . await,
  }}

async fn supplement_saveintent_from_disk (
  from_buffer : NodeSaveIntent,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  let pid : ID =
    from_buffer . pid . clone();
  let from_disk : Option<NodeComplete> =
    optNodeComplete_rustFIrst_by_id (
      config, driver, &pid) . await ?;
  match from_disk {
    None => {
      // A brand-new node has no sticky levels (no disk edges to be
      // sticky about), but an explicit '(relSource ...)' atom must
      // still be validated against the DEFAULT floor -- an empty
      // disk stand-in reuses 'apply_sticky_levels' unchanged (its
      // sticky lookups simply find nothing, falling through to
      // default every time).
      let explicit_levels : ExplicitLevels =
        from_buffer . explicit_levels ();
      let supplemented : NodeComplete =
        from_buffer . into_nodecomplete ();
      let empty_disk : NodeComplete = NodeComplete {
        pid    : supplemented . pid    . clone (),
        source : supplemented . source . clone (),
        .. empty_node_complete () };
      let supplemented : NodeComplete =
        apply_sticky_levels (
          supplemented, &empty_disk, &explicit_levels, config )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : DefineNode::Save (SaveNode (supplemented)),
        source_move : None, } ) },
    Some (disk_node) => {
      let disk_node : NodeComplete = disk_node;
      let mut from_buffer : NodeSaveIntent = from_buffer;
      from_buffer . fill_unspecified_contains (
        &members_of (&disk_node . contains));
      let explicit_levels : ExplicitLevels =
        from_buffer . explicit_levels ();
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
            Some (active) => preserve_invisible_members (
              supplemented, &disk_node, config, active ) };
        apply_sticky_levels (
          supplemented, &disk_node, &explicit_levels, config )
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
  mut supplemented : NodeComplete,
  disk_node        : &NodeComplete,
  config           : &SkgConfig,
  active           : &ActiveSourceSet,
) -> NodeComplete {
  let is_visible = |id : &ID| -> bool {
    member_is_visible (id, config, active) };
  let owner_source : SourceName = supplemented . source . clone ();
  { let disk_contains : Vec<ID> = members_of (&disk_node . contains);
    let buffer_contains : Vec<ID> = members_of (&supplemented . contains);
    let merged : Vec<ID> = weave (
      &disk_contains, &is_visible,
      &buffer_contains );
    supplemented . contains =
      privacied_all (&owner_source, merged); }
  { let disk_subscribes : Vec<ID> =
      members_of (disk_node . subscribes_to . or_default ());
    let buffer_subscribes : Vec<ID> =
      members_of (supplemented . subscribes_to . or_default ());
    let merged : Vec<ID> = weave (
      &disk_subscribes, &is_visible,
      &buffer_subscribes );
    if merged != buffer_subscribes {
      supplemented . subscribes_to =
        MSV::Specified (privacied_all (&owner_source, merged)); }}
  { let disk_overrides : Vec<ID> =
      members_of (disk_node . overrides_view_of . or_default ());
    let buffer_overrides : Vec<ID> =
      members_of (supplemented . overrides_view_of . or_default ());
    let merged : Vec<ID> = set_difference_merge (
      &disk_overrides, &is_visible,
      &buffer_overrides );
    if merged != buffer_overrides {
      supplemented . overrides_view_of =
        MSV::Specified (privacied_all (&owner_source, merged)); }}
  supplemented }

/// Deleting a node deletes its whole TELESCOPE, including sections
/// the current level cannot see; refuse rather than silently
/// destroy them. (The agreed small leak: the refusal reveals that
/// inactive sections exist.)
pub fn refuse_delete_with_inactive_sections (
  config : &SkgConfig,
  active : &ActiveSourceSet,
  pid    : &ID,
) -> Result<(), String> {
  for source_name in config . ordered_sources () {
    if active . contains_source (&source_name) { continue; }
    if let Ok (path) = crate::util::path_from_pid_and_source (
      config, &source_name, pid . clone () ) {
      if std::path::Path::new (&path) . is_file () {
        return Err ( format! (
          "Cannot delete '{}': it has telescope sections in inactive sources. Widen the source-set (e.g. to 'all') and retry.",
          pid )); }} }
  Ok (( )) }

/// THE STICKY-ELSE-DEFAULT RULE (5_plan.org, work item
/// save-leveling), extended by an EXPLICIT third path (work item
/// render-and-gating). The lowering stages tag every edge with the
/// node's own source (a placeholder); this pass resolves the real
/// levels:
/// - EXPLICIT: a member named in 'explicit' (the buffer headline's
///   '(relSource NAME)' atom, threaded in as a side-channel because
///   NodeComplete's 'PrivaciedMember::level' carries no "was this
///   explicit" flag) wins outright, PROVIDED it is at or above (at
///   least as private as) the STICKY-OR-DEFAULT floor below;
///   otherwise the save fails with a validation error naming the
///   member, the offered level, and the floor.
/// - STICKY: absent an explicit level (or with one at the floor
///   exactly), an edge that already exists on disk (same relation,
///   same endpoints, through 'pid_of') keeps its DISK level.
///   Renormalization never lowers a level silently -- lowering
///   happens only through the explicit gesture's cycle
///   ('skg-privatize-relationship', whose least-private stop is the
///   default, which REMOVES the atom).
/// - DEFAULT: a new edge (no explicit level, no disk level) gets the
///   more private of its two endpoints' homes.
/// - HIDES additionally floor at the most public EXPLAINING
///   subscription (see 'hide_level'): a hide is only as public as
///   some subscription that makes it meaningful, else it leaks the
///   inference that a private subscription exists. Hides carry no
///   explicit-level path: the col that displays them is read-only
///   (the privatize gesture refuses there).
pub(crate) fn apply_sticky_levels (
  mut supplemented : NodeComplete,
  disk_node        : &NodeComplete,
  explicit         : &ExplicitLevels,
  config           : &SkgConfig,
) -> Result<NodeComplete, String> {
  let owner_pid  : ID         = supplemented . pid    . clone ();
  let owner_home : SourceName = supplemented . source . clone ();
  let resolve = |id : &ID| -> ID {
    snapshot_global ()
      . and_then ( |snap| snap . pid_of (id) )
      . unwrap_or_else ( || id . clone () ) };
  let home_of = |id : &ID| -> Option<SourceName> {
    snapshot_global ()
      . and_then ( |snap| snap . pid_and_source (id)
                   . map ( |(_pid, src)| src ))
      . or_else ( || source_from_disk (id, config) ) };
  // The sticky-or-default FLOOR for one member -- unchanged from
  // before this item, just factored out of 'level_for' so EXPLICIT
  // can validate against it.
  let floor_for = |disk_list : &[PrivaciedMember<ID>],
                    member    : &ID|
  -> SourceName {
    let key : ID = resolve (member);
    let unclamped : SourceName = 'unclamped : {
      for d in disk_list { // sticky
        if resolve ( &d . member ) == key {
          break 'unclamped d . level . clone (); }}
      match home_of (member) { // default
        Some (target_home) =>
          config . more_private_of (
            owner_home . clone (), target_home ),
        None => owner_home . clone (), }};
    // Clamp: no section may be more public than the home (the
    // "extends on the other side" junk shape), so when a HOME MOVE
    // makes the node more private, its edges rise with it. (The
    // converse move leaves old, more-private levels in place:
    // publicizing memberships takes the explicit gesture.)
    config . more_private_of (unclamped, owner_home . clone ()) };
  // EXPLICIT wins at-or-above the floor; else the floor itself.
  let resolve_level = |disk_list      : &[PrivaciedMember<ID>],
                        member         : &ID,
                        explicit_here  : &HashMap<ID, SourceName>,
                        relation_label : &str|
  -> Result<SourceName, String> {
    let floor : SourceName = floor_for (disk_list, member);
    match explicit_here . get (member) {
      Some (level) =>
        if config . is_strictly_more_public (level, &floor) {
          Err ( format! (
            "Cannot save {} (relation '{}'): member '{}' requested \
             level '{}', but the sticky/default floor for this edge \
             is '{}'. Levels never lower silently -- \
             skg-privatize-relationship's cycle stops at the default.",
            owner_pid, relation_label, member, level, floor ))
        } else { Ok ( level . clone () ) },
      None => Ok (floor), }};
  { let disk : &[PrivaciedMember<ID>] = &disk_node . contains;
    for m in supplemented . contains . iter_mut () {
      m . level = resolve_level (
        disk, &m . member, &explicit . contains, "contains") ?; }}
  { let disk : &[PrivaciedMember<ID>] =
      disk_node . subscribes_to . or_default ();
    if let MSV::Specified (v) = &mut supplemented . subscribes_to {
      for m in v . iter_mut () {
        m . level = resolve_level (
          disk, &m . member, &explicit . subscribes_to,
          "subscribes_to") ?; }} }
  { let disk : &[PrivaciedMember<ID>] =
      disk_node . overrides_view_of . or_default ();
    if let MSV::Specified (v) = &mut supplemented . overrides_view_of {
      for m in v . iter_mut () {
        m . level = resolve_level (
          disk, &m . member, &explicit . overrides_view_of,
          "overrides_view_of") ?; }} }
  { let disk : &[PrivaciedMember<ID>] =
      disk_node . hides_from_its_subscriptions . or_default ();
    let subscribes : Vec<PrivaciedMember<ID>> =
      supplemented . subscribes_to . or_default () . to_vec ();
    if let MSV::Specified (v) =
      &mut supplemented . hides_from_its_subscriptions {
      for m in v . iter_mut () {
        let key : ID = resolve ( &m . member );
        let sticky : Option<SourceName> =
          disk . iter ()
          . find ( |d| resolve ( &d . member ) == key )
          . map ( |d| d . level . clone () );
        let unclamped : SourceName = match sticky {
          Some (level) => level,
          None => hide_level (
            config, &owner_home, &m . member, &subscribes,
            &resolve ), };
        m . level = config . more_private_of (
          unclamped, owner_home . clone () ); }} }
  { // Aliases have no target to inherit a level from; sticky by
    // alias text, else the owner's home. No explicit-level path:
    // the atom names an EDGE (contains / subscribes_to /
    // overrides_view_of), not an alias.
    let disk : &[PrivaciedMember<String>] =
      disk_node . aliases . or_default ();
    if let MSV::Specified (v) = &mut supplemented . aliases {
      for m in v . iter_mut () {
        let unclamped : SourceName = disk . iter ()
          . find ( |d| d . member == m . member )
          . map ( |d| d . level . clone () )
          . unwrap_or_else ( || owner_home . clone () );
        m . level = config . more_private_of (
          unclamped, owner_home . clone () ); }} }
  Ok (supplemented) }

/// A NEW hide's level: at least the more private of the endpoints'
/// homes, and at least the most PUBLIC subscription of the hider
/// that explains it (one whose subscribee contains the hidden
/// node). The most public explanation is the floor because the
/// inference "the hider subscribes to something containing X" is
/// innocent whenever any explanation is visible; with no
/// explanation found, fall back to the most private subscription
/// level, and with no subscriptions at all, to the endpoint rule
/// alone (junk-tolerant; the validators report residue).
fn hide_level (
  config     : &SkgConfig,
  owner_home : &SourceName,
  hidden     : &ID,
  subscribes : &[PrivaciedMember<ID>],
  resolve    : &dyn Fn (&ID) -> ID,
) -> SourceName {
  let endpoint_floor : SourceName = {
    let target_home : Option<SourceName> =
      snapshot_global ()
      . and_then ( |snap| snap . pid_and_source (hidden)
                   . map ( |(_pid, src)| src ));
    match target_home {
      Some (h) => config . more_private_of (
        owner_home . clone (), h ),
      None => owner_home . clone (), }};
  let hidden_key : ID = resolve (hidden);
  let explaining_levels : Vec<SourceName> = {
    let Some (snap) = snapshot_global () else {
      return endpoint_floor; };
    subscribes . iter ()
      . filter ( |sub| {
        snap . pid_of ( & sub . member )
          . and_then ( |p| snap . nodes . get (&p) )
          . map ( |subscribee| subscribee . contains . iter ()
                  . any ( |c| resolve ( &c . member ) == hidden_key ))
          . unwrap_or (false) } )
      . map ( |sub| sub . level . clone () )
      . collect () };
  let subscription_floor : Option<SourceName> =
    explaining_levels . into_iter ()
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
