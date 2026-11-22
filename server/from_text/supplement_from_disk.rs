//! PURPOSE:
//! When a Graphnode is created from user input,
//! it might not mention every Graphnode field.
//! If it holds 'Specified(vec![])' ('MSV') for that field,
//! then the user is asking to empty the field.
//! But if it holds 'Unspecified' for that field,
//! then the field should not be changed --
//! which means it must be read from disk
//! and inserted into the Graphnode.

use crate::from_text::local_fieldintent_collection::lower::{
  RequestedRelRepos, NodeIntent, NodeSaveIntent };
use crate::from_text::weave::weave;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::BufferValidationError;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::types::config::SkgConfig;
use crate::types::misc::{
  ID, MSV, RelPartner, RelationshipMemberKey, SkgrepoChange, SkgrepoName, members_of, rel_partners_at_relRepo};
use crate::types::disk_home::home_from_disk;
use crate::types::nodes::complete::{
  Graphnode, empty_graphnode, set_flag};
use crate::types::save::{NodeInstruction, SaveNode, SkgrepoMove};
use std::collections::HashMap;
use std::error::Error;

pub struct NodeInstructions_with_Repomoves {
  pub instructions : Vec<NodeInstruction>,
  pub skgrepo_moves   : Vec<SkgrepoMove>,
}

struct NodeInstruction_with_Opt_Repomove {
  instruction : NodeInstruction,
  skgrepo_move   : Option<SkgrepoMove>,
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
      let sm : SkgrepoMove = sm;
      self . skgrepo_moves . push (sm); }}
}

pub fn build_diskSupplemented_nodeInstructions (
  intents                : Vec<NodeIntent>,
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
) -> Result<NodeInstructions_with_Repomoves, Box<dyn Error>> {
  let mut result : NodeInstructions_with_Repomoves =
    NodeInstructions_with_Repomoves::with_capacity (intents . len());
  let prospective_homes : HashMap<ID, SkgrepoName> =
    homes_declared_by_nodeSaveIntents (&intents);
  for intent in intents {
    let supplemented : NodeInstruction_with_Opt_Repomove =
      supplement_nodeIntent_from_disk (
        intent, graph, config, skgrepo_restriction, &prospective_homes ) ?;
    result . push (supplemented); }
  Ok (result) }

/// Each Save nodeIntent's skgrepo is the node home after this save. Relationship
/// floors must see these homes across the entire batch: a parent may name a
/// child before the child nodeIntent is supplemented, and a same-save home skgrepo
/// move must make its newly public relationship legal.
fn homes_declared_by_nodeSaveIntents (
  intents : &[NodeIntent],
) -> HashMap<ID, SkgrepoName> {
  let mut homes : HashMap<ID, SkgrepoName> = HashMap::new ();
  for intent in intents {
    let NodeIntent::Save (intent) = intent else { continue; };
    for gnid in std::iter::once (&intent . pid) . chain (
      intent . extra_ids . iter ()) {
      homes . insert (gnid . clone (), intent . home_skgrepo . clone ()); }}
  homes }

fn supplement_nodeIntent_from_disk (
  intent                 : NodeIntent,
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
  prospective_homes      : &HashMap<ID, SkgrepoName>,
) -> Result<NodeInstruction_with_Opt_Repomove, Box<dyn Error>> {
  match intent {
    NodeIntent::Delete (ref delete) => {
      refuse_delete_with_restricted_sections (
        config, skgrepo_restriction, & delete . gnid )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
      Ok (NodeInstruction_with_Opt_Repomove {
        instruction : intent . into_node_instruction()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        skgrepo_move : None,
      }) },
    _ => supplement_nodeSaveIntent_from_disk (
      intent . save_intent()
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      graph, config, skgrepo_restriction, prospective_homes ),
  }}

fn supplement_nodeSaveIntent_from_disk (
  from_buffer            : NodeSaveIntent,
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
  prospective_homes      : &HashMap<ID, SkgrepoName>,
) -> Result<NodeInstruction_with_Opt_Repomove, Box<dyn Error>> {
  let pid : ID =
    from_buffer . pid . clone();
  if from_buffer . strip_attribution
    && ! config . skgrepo_is_owned (&from_buffer . home_skgrepo) {
    return Err ( format! (
      "Cannot strip the attribution of '{}': its skgrepo '{}' is not yours.",
      from_buffer . title, from_buffer . home_skgrepo ) . into () ); }
  let from_disk : Option<Graphnode> =
    graphnode_from_graph (graph, &pid);
  match from_disk {
    None => {
      // A brand-new node has no sticky skgrepos (no disk relationships to be
      // sticky about), but an explicit '(editRequest (relRepo ...))'
      // request must
      // still be validated against the default floor -- an empty
      // disk stand-in reuses 'apply_sticky_relRepos_in_graph_with_prospective_homes' unchanged (its
      // sticky lookups simply find nothing, falling through to
      // default every time).
      let requested_relRepos : RequestedRelRepos =
        from_buffer . requested_relRepos ();
      let flag_request = from_buffer . flag_request;
      let strip_attribution : bool = from_buffer . strip_attribution;
      let mut supplemented : Graphnode =
        from_buffer . into_graphnode ();
      if strip_attribution { supplemented . attribution = None; }
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
      let strip_attribution : bool = from_buffer . strip_attribution;
      let from_buffer : Graphnode =
        from_buffer . into_graphnode();
      let canonicalized : Graphnode =
        canonicalize_gnids_from_disk (from_buffer, &disk_node) ?;
      let maybe_move : Option<SkgrepoMove> =
        detect_skgrepo_move ( config,  &pid,
                             &canonicalized . home_skgrepo,
                             &disk_node . home_skgrepo) ?;
      let supplemented : Graphnode = {
        let mut supplemented : Graphnode =
          supplement_unspecified_fields_from_disk (
            canonicalized, &disk_node);
        if let Some ((flag, value)) = flag_request {
          set_flag (&mut supplemented . flags, flag, value); }
        if strip_attribution { supplemented . attribution = None; }
        let supplemented : Graphnode =
          if skgrepo_restriction . is_unrestricted () { supplemented }
          else { preserve_invisible_members (
                   supplemented, &disk_node, graph, skgrepo_restriction ) };
        apply_sticky_relRepos_in_graph_with_prospective_homes (
          supplemented, &disk_node, &requested_relRepos,
          graph, config, prospective_homes )
          . map_err ( |e| -> Box<dyn Error> { e . into () } ) ? };
      Ok (NodeInstruction_with_Opt_Repomove {
        instruction : NodeInstruction::Save (SaveNode (supplemented)),
        skgrepo_move   : maybe_move,
      }) }}}

/// Under a skgrepo restriction, the buffer shows only some of a
/// node's relationship-list members, so its lists describe only the
/// visible subset.  This merges each list with its disk counterpart
/// (the API doc's "Restricted-skgrepo nodes"): the anchored
/// 'weave' for the order-meaningful 'contains', 'forks' and
/// 'aliases' (the alias folder omits an alias whose record is
/// restricted). A fork of a node the restriction keeps out is kept
/// too, though its record is visible: its restricted viewnode's line
/// edits no relationship.  A field is replaced only when the merge
/// changed it, so an untouched field keeps its MSV shape (and the
/// noop filter can still recognize an unchanged node).
fn preserve_invisible_members (
  mut supplemented : Graphnode,
  disk_node        : &Graphnode,
  graph            : &InRustGraph,
  restriction      : &SkgrepoRestriction,
) -> Graphnode {
  let member_key = |gnid : &ID| -> RelationshipMemberKey {
    graph . relationship_member_key (gnid) };
  let contains_visible : &dyn Fn (&ID) -> bool = &|gnid : &ID| -> bool {
    SkgrepoRestriction::member_record_is_visible (
      restriction, &disk_node . contains, gnid ) };
  let forks_visible : &dyn Fn (&ID) -> bool = &|gnid : &ID| -> bool {
    // A forks-folder member whose node the restriction keeps out
    // stood as a restricted viewnode, and deleting one changes no
    // relationship to the note it stands for, so it is kept as if
    // unseen.
    SkgrepoRestriction::member_record_is_visible (
      restriction, disk_node . forks . or_default (), gnid )
    && ! graph . node_is_restricted (gnid, restriction) };
  // Rendering may canonicalize a resolvable extra ID to its primary PID.  The
  // comparison key says that is the same relationship, but the disk spelling
  // is load-bearing: restore it before the weave so an untouched round trip
  // cannot rewrite a relationship merely because its target was displayed by PID.
  let normalize_to_disk_raw = |buffer : &[ID], disk : &[RelPartner<ID>]| {
    buffer . iter () . map (|gnid| {
      let key : RelationshipMemberKey = member_key (gnid);
      disk . iter ()
        . find (|member| member_key (&member . member) == key)
        . map (|member| member . member . clone ())
        . unwrap_or_else (|| gnid . clone ())
    }) . collect::<Vec<ID>>() };
  let recorder_skgrepo : SkgrepoName = supplemented . home_skgrepo . clone ();
  { let disk_contains : Vec<ID> = members_of (&disk_node . contains);
    let buffer_contains : Vec<ID> = normalize_to_disk_raw (
      &members_of (&supplemented . contains), &disk_node . contains);
    let merged : Vec<ID> = weave (
      &disk_contains, &contains_visible,
      &buffer_contains );
    supplemented . contains =
      rel_partners_at_relRepo (&recorder_skgrepo, merged); }
  { let disk_forks : Vec<ID> =
      members_of (disk_node . forks . or_default ());
    let submitted_forks : Vec<ID> =
      members_of (supplemented . forks . or_default ());
    let buffer_forks : Vec<ID> = normalize_to_disk_raw (
      &submitted_forks,
      disk_node . forks . or_default ());
    let merged : Vec<ID> = weave (
      &disk_forks, &forks_visible,
      &buffer_forks );
    if merged != submitted_forks {
      supplemented . forks =
        MSV::Specified (rel_partners_at_relRepo (&recorder_skgrepo, merged)); }}
  { // The sticky pass after this gives each alias its relRepo again,
    // by its text.
    let alias_visible : &dyn Fn (&String) -> bool = &|text : &String| -> bool {
      SkgrepoRestriction::member_record_is_visible (
        restriction, disk_node . aliases . or_default (), text ) };
    let submitted_aliases : Vec<String> =
      members_of (supplemented . aliases . or_default ());
    let merged : Vec<String> = weave (
      &members_of (disk_node . aliases . or_default ()), &alias_visible,
      &submitted_aliases );
    if merged != submitted_aliases {
      supplemented . aliases =
        MSV::Specified (rel_partners_at_relRepo (&recorder_skgrepo, merged)); }}
  supplemented }

/// Deleting a node deletes its whole telescope, including sections
/// the skgrepo restriction cannot see; refuse rather than silently
/// destroy them. (The agreed small leak: the refusal reveals that
/// restricted sections exist.)
pub fn refuse_delete_with_restricted_sections (
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
  pid    : &ID,
) -> Result<(), String> {
  if has_restricted_sections (config, restriction, pid) {
    return Err ( format! (
      "Cannot delete '{}': it has telescope sections in restricted repos. Widen the skgrepo restriction to show them and retry.",
      pid )); }
  Ok (( )) }

/// A nodeMerge rewrites both graphnodes' whole telescopes, the
/// acquiree's restricted sections moving into the acquirer's; refuse,
/// as a delete is refused, when either graphnode has a section the
/// skgrepo restriction cannot see.
pub fn refuse_nodeMerge_with_restricted_sections (
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  acquirer    : &ID,
  acquiree    : &ID,
) -> Result<(), String> {
  for pid in [acquirer, acquiree] {
    if has_restricted_sections (config, restriction, pid) {
      return Err ( format! (
        "Cannot merge '{}' into '{}': '{}' has telescope sections in restricted repos. Widen the skgrepo restriction to show them and retry.",
        acquiree, acquirer, pid )); }}
  Ok (( )) }

/// Whether the graphnode 'pid' has a section, a file, in a skgrepo the
/// skgrepo restriction keeps out.
fn has_restricted_sections (
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  pid         : &ID,
) -> bool {
  config . ordered_skgrepos () . into_iter ()
    . filter ( |skgrepo_name| ! SkgrepoRestriction::record_is_visible (
      restriction, skgrepo_name ))
    . any ( |skgrepo_name|
      crate::util::path_from_pid_and_skgrepo (
        config, &skgrepo_name, pid . clone () )
      . is_ok_and ( |path| std::path::Path::new (&path) . is_file () )) }

/// The sticky-else-default rule ('docs/telescopes.org', "Where new
/// relationships land: the sticky-else-default rule"), with its
/// explicit third path. The lowering stages tag every relationship with the
/// node's own skgrepo (a placeholder); this pass resolves the real
/// relRepos:
/// - EXPLICIT: a member named in 'explicit' (the buffer headline's
///   '(relRepo NAME)' atom, threaded in as a side-channel because
///   Graphnode's 'RelPartner::skgrepo' carries no "was this
///   explicit" flag) wins outright, provided it is at least as
///   private as the default floor -- normally the more private of the two
///   endpoints' homes, not the disk relRepo. An explicit atom is therefore
///   the one path that can make an existing relationship more public, down
///   to but never more public than its default ('docs/telescopes.org',
///   "Setting a relationship's skgrepo"). One exception keeps the
///   render->save round-trip lossless: when the disk relRepo already sits
///   more public than the default (a legacy or hand-authored shape), the
///   explicit floor relaxes to that disk skgrepo -- such a relationship can
///   be held or made more private, never moved still more public. A choice
///   more public than its floor fails with a validation error naming the
///   member, offered skgrepo, and floor.
/// - STICKY: absent an explicit skgrepo, a relationship that already exists
///   on disk (same relation, same endpoints, through 'pid_of')
///   keeps its disk relRepo. Renormalization never lowers a relationship's
///   privacy silently; removing the atom means "no opinion", not
///   "reset to default".
/// - DEFAULT: a new relationship between owned nodes gets the more private
///   endpoint home. If the member is foreign and the recorder is owned,
///   the relationship instead stays at the recorder's home: the relationship
///   and foreign ID are intentionally shared with that skgrepo.
/// - hides additionally floor at the most public partner on record,
///   a fork either way (see 'hide_skgrepo'): a hide is only as public as
///   some fork that makes it meaningful, else it leaks the
///   inference that a private fork exists. Hides carry no
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
  prospective_homes : &HashMap<ID, SkgrepoName>,
) -> Result<Graphnode, String> {
  let recorder_pid  : ID         = supplemented . pid    . clone ();
  let recorder_home : SkgrepoName = supplemented . home_skgrepo . clone ();
  let resolve = |gnid : &ID| -> ID {
    graph . pid_else_raw_id (gnid) };
  let member_key = |gnid : &ID| -> RelationshipMemberKey {
    graph . relationship_member_key (gnid) };
  let home_of = |gnid : &ID| -> Option<SkgrepoName> {
    prospective_homes . get ( &resolve (gnid) ) . cloned ()
      .or_else ( || prospective_homes . get (gnid) . cloned () )
      .or_else ( || graph . pid_and_skgrepo (gnid)
      . map ( |(_pid, src)| src )
      . or_else ( || home_from_disk (gnid, config) ) ) };
  // The default floor for one member. Owned-to-owned relationships use the
  // more private endpoint home. An owned-to-foreign relationship stays at
  // the recorder's home; Skg never proposes writing a foreign section.
  // An unknown target also falls back to the recorder's home.
  let default_floor_for = |member : &ID| -> SkgrepoName {
    match home_of (member) {
      Some (target_home) =>
        config . default_relRepo (
          &recorder_home, &target_home ),
      None => recorder_home . clone (), }};
  // The sticky-else-default relRepo for one member -- what an absent
  // atom resolves to.
  let sticky_skgrepo_for = |disk_list : &[RelPartner<ID>],
                            member    : &ID|
  -> SkgrepoName {
    let key : RelationshipMemberKey = member_key (member);
    let unclamped : SkgrepoName = 'unclamped : {
      for d in disk_list { // sticky
        if member_key ( &d . member ) == key {
          break 'unclamped d . relRepo . clone (); }}
      default_floor_for (member) };
    // Clamp: no section may be more public than the home (the
    // "extends on the other side" junk shape), so when a home skgrepo move
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
  // Explicit wins when at least as private as its floor: the more public of
  // the default floor and the sticky skgrepo. Flooring at the default (not
  // at sticky) is what lets an atom lower a stuck relationship's privacy
  // back down to the default ('docs/telescopes.org', "Setting a
  // relationship's skgrepo"); admitting the sticky skgrepo when it sits
  // more public than the default covers legacy or hand-authored data.
  // Render emits the '(relRepo ...)' atom for every off-default
  // relationship, and that atom must round-trip through save unchanged.
  // Net: a normal relationship never moves more public than the default,
  // and a preexisting more-public relationship can only be held
  // or made more private. Absent an atom, sticky-else-default.
  let resolve_skgrepo = |disk_list      : &[RelPartner<ID>],
                        member         : &ID,
                        explicit_here  : &HashMap<ID, SkgrepoName>,
                        relation_label : &str|
  -> Result<SkgrepoName, String> {
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
        let default : SkgrepoName = default_floor_for (member);
        let sticky  : SkgrepoName =
          sticky_skgrepo_for (disk_list, member);
        let floor : SkgrepoName = // the more public of the two
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
      disk_node . forks . or_default ();
    if let MSV::Specified (v) = &mut supplemented . forks {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        m . relRepo = resolve_skgrepo (
          disk, &submitted, &explicit . forks,
          "forks") ?;
        m . member = raw_disk_member (disk, &submitted); }} }
  { let disk : &[RelPartner<ID>] =
      disk_node . hides . or_default ();
    let forkees : Vec<RelPartner<ID>> =
      supplemented . forks . or_default () . to_vec ();
    if let MSV::Specified (v) =
      &mut supplemented . hides {
      for m in v . iter_mut () {
        let submitted : ID = m . member . clone ();
        let key : RelationshipMemberKey = member_key ( &submitted );
        let sticky : Option<SkgrepoName> =
          disk . iter ()
          . find ( |d| member_key ( &d . member ) == key )
          . map ( |d| d . relRepo . clone () );
        let unclamped : SkgrepoName = match sticky {
          Some (skgrepo) => skgrepo,
          None => hide_skgrepo (
            graph, config, &recorder_pid, &recorder_home, &m . member,
            &forkees, &resolve, &home_of ), };
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
        let explicit_skgrepo : Option<&SkgrepoName> =
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
          let sticky_or_default : SkgrepoName = disk . iter ()
            . find ( |d| d . member == m . member )
            . map ( |d| d . relRepo . clone () )
            . unwrap_or_else ( || recorder_home . clone () );
          m . relRepo = config . more_private_of (
            sticky_or_default, recorder_home . clone () ); } }} }
  Ok (supplemented) }

/// A new hide's relRepo: at least the more private of the endpoints'
/// homes, and at least as private as some partner on record for it
/// ('docs/glossary.org', "hides"). A *partner on record* is a fork
/// partner of the hider, either way, that contains the hidden node: a
/// note the hider forks ('forkees', as the save leaves them) or one
/// forking the hider (by the graph). It stands at the more private of
/// the 'forks' relationship's relRepo (in whichever file records it)
/// and the relRepo of the contains record: a hide more public than
/// either would name what only that record tells. The most public
/// partner on record is the floor, because the inference "the hider is
/// a fork partner of something containing X" is innocent whenever any
/// such pair of records is visible; with none found, the endpoint
/// rule alone applies (junk-tolerant; the validators report residue).
/// This is wider than the partners that explain a hide in a view
/// ('forks_folder_members_shown'): the floor asks who can read the hide's
/// record, not what a view draws, so it counts every record the graph
/// has, whatever the skgrepo restriction, and a forker the hider hides
/// too, whose records a reader of the hide sees all the same.
pub(crate) fn hide_skgrepo (
  graph         : &InRustGraph,
  config        : &SkgConfig,
  recorder      : &ID,
  recorder_home : &SkgrepoName,
  hidden        : &ID,
  forkees       : &[RelPartner<ID>],
  resolve       : &dyn Fn (&ID) -> ID,
  home_of       : &dyn Fn (&ID) -> Option<SkgrepoName>,
) -> SkgrepoName {
  let endpoint_floor : SkgrepoName = {
    match home_of (hidden) {
      // A foreign home has no rung on the privacy ladder, so it
      // raises no floor.
      Some (h) if config . skgrepo_is_owned (&h) =>
        config . more_private_of ( recorder_home . clone (), h ),
      _ => recorder_home . clone (), }};
  let hidden_key : ID = resolve (hidden);
  let floors_through_partner : &dyn Fn (&ID, &SkgrepoName) -> Vec<SkgrepoName> =
    // Where each pair of records through one fork partner stands, if
    // the partner contains the hidden node: its 'forks' relationship
    // with the hider stands at 'fork_relRepo'.
    &|partner : &ID, fork_relRepo : &SkgrepoName| -> Vec<SkgrepoName> {
      graph . pid_of (partner)
        . and_then ( |p| graph . nodes . get (&p) )
        . map ( |partner| partner . contains . iter ()
                . filter ( |c| resolve ( &c . member ) == hidden_key )
                . map ( |c|
                  // A foreign partner's contains record has no
                  // rung, so the fork's record alone sets the floor.
                  if config . skgrepo_is_owned (&c . relRepo) {
                    config . more_private_of (
                      fork_relRepo . clone (), c . relRepo . clone () )
                  } else { fork_relRepo . clone () } )
                . collect () )
        . unwrap_or_default () };
  let skgrepos_of_partners_on_record : Vec<SkgrepoName> =
    forkees . iter ()
    . flat_map ( |forkee| floors_through_partner (&forkee . member, &forkee . relRepo) )
    . chain (
      graph . inbound_pids_for_relation (&resolve (recorder), NodeRelation::Forks)
        . into_iter ()
        . filter_map ( |forker| graph . relRepo (&forker, NodeRelation::Forks, recorder)
                                . map ( |fork_relRepo| (forker, fork_relRepo) ))
        . flat_map ( |(forker, fork_relRepo)| floors_through_partner (&forker, &fork_relRepo) ))
    . collect ();
  let fork_floor : Option<SkgrepoName> =
    skgrepos_of_partners_on_record . into_iter ()
    . reduce ( |a, b| // keep the more public of the two
               if config . is_strictly_more_public (&a, &b) { a }
               else { b } );
  match fork_floor {
    Some (floor) =>
      config . more_private_of (endpoint_floor, floor),
    None => endpoint_floor, }}

/// Replace buffer's (singleton) ids with disk's (possibly multiple) ids.
pub fn canonicalize_gnids_from_disk (
  mut from_buffer : Graphnode,
  disk_node       : &Graphnode,
) -> Result<Graphnode, Box<dyn Error>> {
  for buffer_gnid in from_buffer . all_gnids() {
    let buffer_gnid : &ID = buffer_gnid;
    if ! disk_node . all_gnids() . any ( |gnid| gnid == buffer_gnid ) {
      return Err(format!(
        "ID '{}' from buffer not found in IDs form disk.",
        buffer_gnid ) . into() ); }}
  from_buffer . pid = disk_node . pid . clone();
  from_buffer . extra_ids = disk_node . extra_ids . clone();
  Ok (from_buffer) }

/// Return a RepoMove when the skgrepo changes
/// between two owned skgrepos.
pub fn detect_skgrepo_move (
  config         : &SkgConfig,
  pid            : &ID,
  buffer_skgrepo : &SkgrepoName,
  disk_skgrepo   : &SkgrepoName,
) -> Result<Option<SkgrepoMove>, Box<dyn Error>> {
  if buffer_skgrepo == disk_skgrepo {
    return Ok (None); }
  if config . skgrepo_is_owned (disk_skgrepo)
  && config . skgrepo_is_owned (buffer_skgrepo) {
    Ok (Some (SkgrepoMove {
      pid            : pid . clone(),
      skgrepo_change : SkgrepoChange { old : disk_skgrepo . clone(),
                                       new : buffer_skgrepo . clone() } }))
  } else {
    Err(Box::new(
      BufferValidationError::CannotMoveToOrFromForeignSkgrepo(
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
  if from_buffer . forks . is_unspecified() {
    from_buffer . forks =
      disk_node . forks . clone(); }
  if from_buffer . hides . is_unspecified() {
    from_buffer . hides =
      disk_node . hides . clone(); }
  if from_buffer . flags . is_empty() {
    from_buffer . flags = disk_node . flags . clone(); }
  if from_buffer . attribution . is_none() {
    from_buffer . attribution = disk_node . attribution . clone(); }
  from_buffer }

#[cfg(test)]
#[path = "../../tests/unit/save_leveling.rs"]
mod tests;
