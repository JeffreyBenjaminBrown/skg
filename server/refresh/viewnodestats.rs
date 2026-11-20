use crate::dbs::in_rust_graph::InRustGraph;
use crate::attribution::{AttributedSkgrepo, attributed_skgrepo};
use crate::dbs::in_rust_graph::mirrors::side_of_mirror_shown;
use crate::dbs::in_rust_graph::stats::mentioner_is_substantive;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelRepoLookup, RelationRole };
use crate::herald_tokens::{
  AncestorFlags, BirthFact, RelationshipHeralds, Side, relationship_heralds_sexp };
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::refresh::ancestry::parent_reads_claims;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::viewnode::{
  GraphnodeStats, Folder, Viewnode, ViewnodeKind, Vognode, Warrant };
use crate::refresh::ancestry::{
  CompoundFolderParties, compound_folder_parties, required_ancestors};
use crate::refresh::goal::{note_of_member_as_such, what_the_member_shows};
use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::{HashMap, HashSet};
use std::iter::once;

/// The three graph relations whose flags the H/K/L checks consult via
/// the in-Rust graph (contains, the fourth, is checked via the
/// containment maps).
const GRAPH_RELATIONS : [NodeRelation; 3] = [
  NodeRelation::LinksTo,
  NodeRelation::Hides,
  NodeRelation::Forks, ];

/// The stats of each occurrence (its rels, props, birth, 'cycle',
/// 'repoBoundary', 'omittedBody', relRepo). The warrants and the
/// claims are the refresh's rungs' ('server/refresh/'); this pass
/// leaves them as they are.
pub fn set_viewnodestats_in_viewforest (
  viewforest            : &mut Tree<Viewnode>,
  graph                 : &InRustGraph,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  config                : &SkgConfig,
  restriction           : &SkgrepoRestriction,
  relRepos              : &mut RelRepoLookup,
  is_diff_view          : bool, // where a member as such counts what the user keeps elsewhere among its content ('what_the_member_shows')
) {
  let multi_skgrepo : bool = config . skgrepos . len () > 1;
  let mut ancestor_gnids : HashSet<ID> = HashSet::new ();
  let root_treeid : NodeId = viewforest . root () . id ();
  set_viewnodestats_recursive (
    viewforest,
    root_treeid,
    multi_skgrepo,
    graph,
    config,
    restriction,
    &mut ancestor_gnids,
    relRepos,
    container_to_contents,
    content_to_containers,
    is_diff_view ); }

fn set_viewnodestats_recursive (
  tree                  : &mut Tree<Viewnode>,
  treeid                : NodeId,
  multi_skgrepo         : bool,
  graph                 : &InRustGraph,
  config                : &SkgConfig,
  restriction           : &SkgrepoRestriction,
  ancestor_gnids       : &mut HashSet<ID>,
  relRepos              : &mut RelRepoLookup,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  is_diff_view          : bool,
) {
  let opt_pid : Option<ID> =
    if let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    { let node_pid : ID = t . gnid . clone ();
      detect_and_mark_cycle (
        tree, treeid, &node_pid, ancestor_gnids );
      set_attribution (tree, treeid, &node_pid, graph, config);
      set_skgrepo_at_boundary (tree, treeid, multi_skgrepo);
      let fork_roles : Option<ForksFolderMemberRoles> =
        forks_folder_member_roles (tree, treeid, graph, restriction, relRepos);
      let stands_as_member_here : bool = match &fork_roles {
        Some (roles) => roles . as_forkee || roles . as_forker,
        None => stands_as_member (tree, treeid), };
      set_herald_strings_in_viewnode (
        tree, treeid, &node_pid, stands_as_member_here, fork_roles . as_ref (),
        graph, config, restriction, relRepos, container_to_contents, content_to_containers,
        is_diff_view );
      set_omitted_body (tree, treeid, &node_pid, graph);
      set_relRepo (tree, treeid, stands_as_member_here, fork_roles . as_ref (),
                   graph, relRepos, config);
      Some (node_pid)
    } else { None };
  let was_new : bool =
    if let Some ( ref pid ) = opt_pid
    { ancestor_gnids . insert ( pid . clone() ) }
    else { false };
  let child_treeids : Vec<NodeId> =
    tree . get (treeid) . unwrap ()
    . children () . map ( |c| c . id () ) . collect ();
  for child_treeid in child_treeids {
    set_viewnodestats_recursive (
      tree,
      child_treeid,
      multi_skgrepo,
      graph,
      config,
      restriction,
      ancestor_gnids,
      relRepos,
      container_to_contents,
      content_to_containers,
      is_diff_view ); }
  if was_new {
    if let Some ( ref pid ) = opt_pid
    { ancestor_gnids . remove (pid); } } }

/// Whether the occurrence at 'treeid' stands as a member of
/// its viewparent, for the facts its membership explains (its birth,
/// its relRepo). Where the viewparent reads its children's claims
/// ('parent_reads_claims'), that is its claim. Under any other parent
/// a child claims nothing: a root stands as one, a folder's member
/// does when the truth warrants it, and a write-protected occurrence's
/// child does not.
fn stands_as_member (
  tree     : &Tree<Viewnode>,
  treeid   : NodeId,
) -> bool {
  let node : NodeRef<Viewnode> = tree . get (treeid) . unwrap ();
  let ViewnodeKind::Vognode (Vognode::Occurrence (t)) = &node . value () . kind
    else { return false; };
  let Some (parent) = node . parent () else { return t . claimsMembership; };
  if parent_reads_claims (tree, parent . id ()) {
    return t . claimsMembership; }
  match &parent . value () . kind {
    ViewnodeKind::BufferRoot => true,
    ViewnodeKind::Folder (_) => t . viewStats . warrant == Warrant::ByGraph,
    _ => false, } }

/// The roles ('FORKS_FOLDER_MEMBER_ROLES') in which a member of a forks
/// folder stands as a member of it. As a forkee: where the folder
/// reads its members' claims, when it claims; elsewhere, when the
/// graph's warrant and a record the skgrepo restriction shows say the
/// viewparent forks it. As a forker: when such a warrant and record
/// say that it forks the viewparent, whatever it claims. A forkee the
/// user detached, and an indirect member, stand in neither: no birth
/// fact and no relRepo.
struct ForksFolderMemberRoles {
  as_forkee : bool,
  as_forker : bool,
}

/// The roles the occurrence at 'treeid' plays as a member of the
/// forks folder it stands in; None for any other viewnode.
fn forks_folder_member_roles (
  tree        : &Tree<Viewnode>,
  treeid      : NodeId,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  relRepos    : &mut RelRepoLookup,
) -> Option<ForksFolderMemberRoles> {
  let node : NodeRef<Viewnode> = tree . get (treeid) ?;
  let ViewnodeKind::Vognode (Vognode::Occurrence (t)) = &node . value () . kind
    else { return None; };
  let folder : NodeRef<Viewnode> = node . parent () ?;
  if ! matches! ( folder . value () . kind, ViewnodeKind::Folder (Folder::Forks) ) {
    return None; }
  let note : ID = folder . parent () ? . value () . occurrence_gnid () ? . clone ();
  let warranted : bool = t . viewStats . warrant == Warrant::ByGraph;
  let note_plays : &mut dyn FnMut (RelationRole) -> bool =
    &mut |role : RelationRole| -> bool {
    warranted && relationship_is_visible (
      graph, restriction, relRepos, &t . gnid, &note, role ) };
  let note_forks_it : bool = note_plays (RelationRole::FORKER);
  let it_forks_note : bool = note_plays (RelationRole::FORKEE);
  Some ( ForksFolderMemberRoles {
    as_forkee : if parent_reads_claims (tree, folder . id ()) { t . claimsMembership }
                else { note_forks_it },
    as_forker : it_forks_note, } ) }

/// What the occurrence at treeid is born of, and which ancestors to
/// flag. The viewparent is at ancestor generation 1 (a non-vognode folder
/// carries no flag); a folder member additionally flags the folder's
/// required-ancestry vognodes (recorder = the last entry) at their tree-gen
/// distances.
enum ParentKind {
  Vognode (ID),                 // an occurrence parent
  Folder (Folder, NodeId),   // a partner folder parent (its treeid)
  Other,
}

/// Compute and store semantic relationship and birth facts for
/// the occurrence at treeid.
fn set_herald_strings_in_viewnode (
  tree                  : &mut Tree<Viewnode>,
  treeid                : NodeId,
  node_pid              : &ID,
  stands_as_member_here : bool, // see 'stands_as_member'
  fork_roles            : Option<&ForksFolderMemberRoles>,
  graph                 : &InRustGraph,
  config                : &SkgConfig,
  restriction           : &SkgrepoRestriction,
  relRepos              : &mut RelRepoLookup,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  is_diff_view          : bool,
) {
  let (gstats, in_place_of)
    : (GraphnodeStats, Option<ID>) = {
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    ( t . graphStats . clone (), t . in_place_of . clone () ) };
  let counts = match & gstats . rels {
    Some (c) => c . clone (),
    None => return, }; // no stats -> no heralds
  let parent_kind : ParentKind = parent_kind_of (tree, treeid);
  // Gather tracked ancestors (pid, generation), then flag relations.
  let mut flags : AncestorFlags = AncestorFlags::default ();
  let ancestors : Vec<(ID, usize)> = tracked_ancestors (tree, &parent_kind);
  for (anc_pid, generation) in &ancestors {
    flag_ancestor_relations (
      &mut flags, graph, restriction, relRepos,
      container_to_contents, content_to_containers,
      node_pid, anc_pid, *generation ); }
  if ! stands_as_member_here {
    // A child standing as no member under a member as such: the
    // content the note hides, retained there claiming nothing,
    // with its subtree, says so, as the hiddenInForkPartnerFolder's
    // members do.
    if let Some (note_pid) =
      tree . get (treeid) . unwrap () . parent ()
      . and_then ( |parent| note_of_member_as_such (tree, parent . id ()) )
    { if relationship_is_visible (
        graph, restriction, relRepos, node_pid, &note_pid,
        RelationRole::new (NodeRelation::Hides, BinaryRolePosition::First) )
      { flags . record (NodeRelation::Hides, true, 3); }}}
  let unintegrated : Option<usize> =
    // A member as such counts the nodes its content goal
    // ('content_goal') draws as occurrences: nodes the skgrepo
    // restriction shows, as the count of its contents is ('RelationCounts').
    note_of_member_as_such (tree, treeid) . map ( |note| {
      let contents : Vec<ID> =
        graph . outbound_member_keys_for_relation_gated (
          node_pid, NodeRelation::Contains, restriction )
        . iter () . map ( |member| member . member . id () . clone () )
        . collect ();
      let members : HashSet<ID> = what_the_member_shows (
        graph, config, restriction, node_pid,
        Some (&note), &contents, ! is_diff_view )
        . into_iter ()
        . filter ( |gnid| graph . node_is_shown (gnid, restriction) )
        . collect ();
      for (gnid, generation) in &ancestors {
        if members . contains (gnid)
          && flags . contains_out . contains (generation) {
          flags . contents_unintegrated_out . push (*generation); } }
      members . len () });
  let side_of_mirror : Option<Side> = // shown in another's place: which side of it
    in_place_of . as_ref () . and_then ( |original|
      side_of_mirror_shown (graph, restriction, node_pid, original) );
  let birth_facts : Vec<BirthFact> =
    birth_facts (tree, &parent_kind, stands_as_member_here, fork_roles, &flags, side_of_mirror);
  let rel_heralds : RelationshipHeralds = relationship_heralds_sexp (
    &counts, gstats . aliases, gstats . extra_ids, gstats . flags,
    &flags, &birth_facts, unintegrated );
  if let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . rel_heralds = rel_heralds; } }

fn parent_kind_of (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> ParentKind {
  let parent_ref = match tree . get (treeid) . unwrap () . parent () {
    Some (p) => p, None => return ParentKind::Other, };
  match & parent_ref . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
      ParentKind::Vognode ( t . gnid . clone () ),
    ViewnodeKind::Folder (folder) if ! folder . members_are_properties () =>
      ParentKind::Folder ( folder . clone (), parent_ref . id () ),
    _ => ParentKind::Other, } }

/// The (pid, generation) of each tracked ancestor: the viewparent
/// vognode (gen 1), or -- for a folder member -- the occurrences of the
/// folder's required ancestry ('generation_above_a_member').
/// Non-vognode ancestors in the chain carry no flag and are skipped.
fn tracked_ancestors (
  tree        : &Tree<Viewnode>,
  parent_kind : &ParentKind,
) -> Vec<(ID, usize)> {
  match parent_kind {
    ParentKind::Vognode (pid) => vec![ (pid . clone (), 1) ],
    ParentKind::Folder (_folder, folder_treeid) =>
      required_ancestors (tree, *folder_treeid) . into_iter ()
      . filter_map ( |ancestor| {
        let pid : &ID = tree . get (ancestor) ? . value () . occurrence_gnid () ?;
        Some (( pid . clone (),
                generation_above_a_member (tree, *folder_treeid, ancestor) ? )) } )
      . collect (),
    ParentKind::Other => Vec::new (), } }

/// How many generations above a member of the folder at 'folder' its
/// ancestor 'ancestor' stands: the folder is 1, its viewparent 2.
fn generation_above_a_member (
  tree     : &Tree<Viewnode>,
  folder   : NodeId,
  ancestor : NodeId,
) -> Option<usize> {
  tree . get (folder) ? . ancestors ()
    . position ( |above| above . id () == ancestor )
    . map ( |steps_above_the_folder| steps_above_the_folder + 2 ) }

/// Record, for the tracked ancestor 'anc_pid' at 'generation', every
/// relation it is a member of on each side relative to 'node_pid'.
/// Contains membership is read from the repo-filtered containment
/// maps; the other four relations from the in-Rust graph. Every flag
/// is relRepo gated: a relationship recorded outside the skgrepo restriction
/// must not tint an ancestor herald in a more public view (it would
/// reveal the very relationship the user privatized). The contains
/// gate asks the graph, since the maps carry no relRepos.
fn flag_ancestor_relations (
  flags                 : &mut AncestorFlags,
  graph                 : &InRustGraph,
  restriction           : &SkgrepoRestriction,
  relRepos              : &mut RelRepoLookup,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  node_pid              : &ID,
  anc_pid               : &ID,
  generation            : usize,
) {
  let contains_rel_is_visible : &mut dyn FnMut (&ID, &ID) -> bool =
    &mut |recorder : &ID, target : &ID| -> bool {
    restriction . is_unrestricted ()
    || relRepos . relRepo_memoized (recorder, NodeRelation::Contains, target)
       . is_some_and ( |relRepo| SkgrepoRestriction::record_is_visible (
         restriction, &relRepo )) };
  // Contains, via the maps. inbound: ancestor contains node.
  if content_to_containers . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid))
    && contains_rel_is_visible (anc_pid, node_pid) {
    flags . record (NodeRelation::Contains, true, generation); }
  // outbound: node contains ancestor.
  if container_to_contents . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid))
    && contains_rel_is_visible (node_pid, anc_pid) {
    flags . record (NodeRelation::Contains, false, generation); }
  for rel in GRAPH_RELATIONS {
    // inbound: ancestor R's node (ancestor plays the first role).
    if relationship_is_visible (
      graph, restriction, relRepos, node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::First) ) {
      flags . record (rel, true, generation);
      if rel == NodeRelation::LinksTo
        && mentioner_is_substantive (graph, restriction, anc_pid) {
        flags . links_substantive_in . push (generation); } }
    // outbound: node R's ancestor (ancestor plays the second role).
    if relationship_is_visible (
      graph, restriction, relRepos, node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::Second) ) {
      flags . record (rel, false, generation); } } }

/// Whether 'member' plays 'member_role' to 'node' in a relationship
/// whose record the skgrepo restriction shows. The record is the
/// list of whichever of the two plays the relation's first role.
fn relationship_is_visible (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  relRepos    : &mut RelRepoLookup,
  node        : &ID,
  member      : &ID,
  member_role : RelationRole,
) -> bool {
  let (recorder, target) : (&ID, &ID) =
    if member_role . is_first_role () { (member, node) }
    else { (node, member) };
  graph . relation_membership_is_real (node, member, member_role)
  && ( restriction . is_unrestricted ()
       || relRepos . relRepo_memoized (recorder, member_role . relation, target)
          . is_some_and ( |relRepo| SkgrepoRestriction::record_is_visible (
            restriction, &relRepo )) ) }

/// The birth facts -- which relations explain this occurrence, on which
/// side of it, accounting for which ancestor -- read from the kind of
/// its viewparent: for a folder's member, the relation the folder
/// names, in the direction its role implies; for a content member, its
/// viewparent's containment; for a child standing as no member
/// ('stands_as_member'), none. Usually a singleton; a
/// HiddenInForkPartner member has two, and so has a mutual in a forks
/// folder, one per side it stands on ('fork_roles'). A mirror shown in
/// another's place leads with its fork relation, on the side
/// 'side_of_mirror_shown' gives.
fn birth_facts (
  tree                  : &Tree<Viewnode>,
  parent_kind           : &ParentKind,
  stands_as_member_here : bool,
  fork_roles            : Option<&ForksFolderMemberRoles>,
  flags                 : &AncestorFlags,
  side_of_mirror        : Option<Side>, // shown in another's place, reaching it by forker (out) or forkee (in) steps only
) -> Vec<BirthFact> {
  let mut facts : Vec<BirthFact> = {
    if ! stands_as_member_here { Vec::new ()
    } else {
      match parent_kind {
        ParentKind::Vognode (_) =>
          // Ordinary content: born of its parent containing it.
          if flags . contains_in . contains (&1) {
            vec![ BirthFact::new ( NodeRelation::Contains, Side::In, Some (1) ) ]
          } else { Vec::new () },
        ParentKind::Folder (folder, folder_treeid) => match fork_roles {
          Some (roles) => birth_facts_of_fork_roles (roles),
          None => birth_facts_for_folder (tree, folder, *folder_treeid), },
        ParentKind::Other => Vec::new (), }}};
  if let Some (side) = side_of_mirror {
    if ! facts . iter () . any ( |fact| fact . relation == NodeRelation::Forks ) {
      // A mirror shown in place of another is born of the forks
      // between them: it leads with their herald, like every other
      // birth relation. The original is not an ancestor.
      facts . insert ( 0, BirthFact::new ( NodeRelation::Forks, side, None ) ); }}
  facts }

/// A forks folder member's birth facts: '(forks in 2)' where it stands
/// as a forkee, '(forks out 2)' where it stands as a forker, both for
/// a mutual ('ForksFolderMemberRoles').
fn birth_facts_of_fork_roles (
  roles : &ForksFolderMemberRoles,
) -> Vec<BirthFact> {
  [ ( roles . as_forkee, Side::In ), ( roles . as_forker, Side::Out ) ]
    . into_iter ()
    . filter ( |(stands, _)| *stands )
    . map ( |(_, side)| BirthFact::new ( NodeRelation::Forks, side, Some (2) ))
    . collect () }

/// The side of a node playing ROLE: the first role states the relationship
/// ("it RELATIONs N nodes"), so it is on the out side.
fn side_of_role (
  role : RelationRole,
) -> Side {
  if role . is_first_role () { Side::Out } else { Side::In } }

/// A partner-folder member's birth facts. The folder's recorder is the
/// member's grandparent; the compound partner folders sit one or two
/// levels deeper under the hider. (The table of birth facts is in
/// docs/api-and-formats.org.)
fn birth_facts_for_folder (
  tree          : &Tree<Viewnode>,
  folder        : &Folder,
  folder_treeid : NodeId,
) -> Vec<BirthFact> {
  match folder {
    // A forks folder's member's facts are those of its roles
    // ('birth_facts'); one standing in neither has none.
    Folder::Forks => Vec::new (),
    Folder::Hider | Folder::Hidden
    | Folder::Container | Folder::Mentioner
    | Folder::AcquiredBy | Folder::Acquired =>
      match folder . relation_member_role () {
        Some (role) => vec![ BirthFact::new (
          role . relation, side_of_role (role), Some (2) ) ],
        None => Vec::new (), },
    // In
    //   note > forksFolder > member
    //     > integratedElsewhereFolder > member of it
    // the member as such (generation 2) contains it; whatever else
    // contains it is no ancestor.
    Folder::IntegratedElsewhere => vec! [ BirthFact::new (
      NodeRelation::Contains, Side::In, Some (2) ) ],
    // The other compound partner folders. In
    //   note > forksFolder > member
    //     > hiddenInForkPartnerFolder > member of it
    // the note (generation 4) hides it, and the member as such
    // (generation 2) contains it; in
    //   note > forksFolder > forker
    //     > hiddenByForkerFolder > member of it
    // the forker (generation 2) hides it, and the note (generation 4)
    // contains it. In
    //   note > forksFolder
    //     > hiddenOutsideOfForkPartnersFolder > member of it
    // the note (generation 3) hides it.
    Folder::HiddenInForkPartner | Folder::HiddenByForker
      | Folder::HiddenOutsideOfForkPartners => {
      let Some (parties) : Option<CompoundFolderParties> =
        compound_folder_parties (tree, folder_treeid)
        else { return Vec::new (); }; // unsayable, so entombstoned
      once ( BirthFact::new (
          NodeRelation::Hides, Side::In,
          generation_above_a_member (tree, folder_treeid, parties . hider) ))
        . chain ( parties . container . map ( |container| BirthFact::new (
            NodeRelation::Contains, Side::In,
            generation_above_a_member (tree, folder_treeid, container) )) )
        . collect () },
    // 'ParentKind::Folder' holds partner folders only.
    Folder::Alias | Folder::ID | Folder::Flags { .. } => Vec::new (),
    // What an illegal section contains is no relationship of the node's.
    Folder::IllegalEdit { .. } => Vec::new (), } }

/// Sets omitted_body on the occurrence at treeid: true iff the node
/// is shown WRITE_PROTECTED here while its graphnode has a body -- one
/// the rendering hides. Herald "B" on the ☮.
fn set_omitted_body (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  node_pid : &ID,
  graph    : &InRustGraph,
) {
  let omitted_body : bool = {
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    t . is_writeProtected ()
      && { let pid : ID = graph . pid_else_raw_id (node_pid);
           graph . nodes . get (&pid)
             . map_or ( false, |n| n . body . is_some () ) } };
  if let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . omitted_body = omitted_body; }}

/// Sets relRepo on the occurrence at treeid (see
/// 'ViewnodeStats::relRepo' for the full contract).
/// Computes the (recorder, relation, target) triple that identifies the
/// binding relationship this position represents -- contains for an ordinary
/// Vognode-parent content child; the folder's relation for a simple
/// partner folder member, oriented by which side owns the outbound relationship
/// (see 'RelationRole::is_first_role') -- then compares the relationship's
/// actual skgrepo ('InRustGraph::relRepo') against its applicable
/// relationship default. None on any of: the viewnode does not
/// stand as a member here ('stands_as_member'); a compound partner folder
/// (IntegratedElsewhere, HiddenInForkPartner, HiddenByForker,
/// HiddenOutsideOfForkPartners: no single
/// 'relation_member_role'); no recorded relationship; a relationship
/// recorded in a foreign skgrepo; unresolvable homes;
/// or the skgrepo equalling the default.
fn set_relRepo (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  stands_as_member_here : bool, // see 'stands_as_member'
  fork_roles : Option<&ForksFolderMemberRoles>, // in a forks folder: a forkee's relRepo is its viewparent's record, a forker-only member's its own
  graph  : &InRustGraph,
  relRepos : &mut RelRepoLookup,
  config : &SkgConfig,
) {
  let relRepo : Option<SkgrepoName> = 'compute : {
    let node_pid : ID = {
      let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
        & tree . get (treeid) . unwrap () . value () . kind
      else { break 'compute None; };
      t . collected_gnid () };
    if ! stands_as_member_here {
      // Not a genuine member at this position (a self-writer standing
      // claiming nothing under a folder, or a node of the containerward
      // chain): there
      // is no binding relationship here to have a skgrepo at all.
      break 'compute None; }
    let (recorder_pid, relation, target_pid) : (ID, NodeRelation, ID) =
      match parent_kind_of (tree, treeid) {
        ParentKind::Vognode (parent_pid) =>
          (parent_pid, NodeRelation::Contains, node_pid),
        ParentKind::Folder (folder, folder_treeid) => {
          let member_role : Option<RelationRole> = match fork_roles {
            Some (roles) if roles . as_forkee => Some (RelationRole::FORKEE),
            Some (_) => Some (RelationRole::FORKER),
            None => folder . relation_member_role (), };
          let Some (role) : Option<RelationRole> = member_role
          else { break 'compute None; }; // compound partner folders
          let Some (described_pid) : Option<ID> =
            tree . get (folder_treeid) . unwrap () . parent ()
            . and_then ( |p| p . value () . occurrence_gnid () . cloned () )
          else { break 'compute None; };
          if role . is_first_role () {
            // This position's own node owns the outbound relationship (e.g.
            // a forker-only member of a forks folder, which itself forks
            // the node the folder describes).
            (node_pid . clone (), role . relation, described_pid)
          } else {
            // The node the folder describes owns the outbound relationship
            // (e.g. a claiming forks folder member, which it forks).
            (described_pid, role . relation, node_pid . clone ())
          }},
        ParentKind::Other => break 'compute None, };
    let skgrepo : SkgrepoName =
      match relRepos . relRepo_memoized (&recorder_pid, relation, &target_pid) {
        Some (l) => l, None => break 'compute None, };
    if ! config . skgrepo_is_owned (&skgrepo) {
      // A foreign skgrepo has no rung on the privacy ladder, so a
      // relationship recorded there is more private than nothing; and
      // only a foreign node's own file records it, so no user of this
      // server privatized it.
      break 'compute None; }
    let default : SkgrepoName = {
      let recorder_home : Option<SkgrepoName> =
        graph . pid_and_skgrepo (&recorder_pid) . map ( |(_, s)| s );
      let target_home : Option<SkgrepoName> =
        graph . pid_and_skgrepo (&target_pid) . map ( |(_, s)| s );
      match (recorder_home, target_home) {
        (Some (a), Some (b)) =>
          config . default_relRepo (&a, &b),
        _ => break 'compute None, }};
    if skgrepo == default { None } else { Some (skgrepo) } };
  if let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . relRepo = relRepo; }}

#[cfg(test)]
mod relationship_default_tests {
  use super::*;
  use crate::types::misc::{RelPartner, Skgrepo};
  use crate::types::nodes::complete::{empty_graphnode, Graphnode};
  use crate::types::viewnode::{
    mk_editable_viewnode, viewforest_root_viewnode};
  use std::path::PathBuf;

  #[test]
  fn owned_to_foreign_recorder_home_relationship_has_no_override_herald () {
    let mut config : SkgConfig = {
      let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
        HashMap::new ();
      for (name, owned) in
          [("public", true), ("foreign", false), ("private", true)] {
        skgrepos . insert (
          SkgrepoName::from (name),
          Skgrepo {
            name         : SkgrepoName::from (name),
            abbreviation : None,
            path         : PathBuf::from (name),
            owned        : owned, } ); }
      SkgConfig::dummyFromSkgrepos (skgrepos) };
    config . skgrepo_order = ["public", "foreign", "private"]
      . into_iter () . map (SkgrepoName::from) . collect ();

    let mut recorder : Graphnode = empty_graphnode ();
    recorder . pid = ID::new ("recorder");
    recorder . title = "recorder" . to_string ();
    recorder . home_skgrepo = SkgrepoName::from ("public");
    recorder . contains = vec! [ RelPartner::at_relRepo (
      SkgrepoName::from ("public"), ID::new ("member") ) ];
    let mut member : Graphnode = empty_graphnode ();
    member . pid = ID::new ("member");
    member . title = "member" . to_string ();
    member . home_skgrepo = SkgrepoName::from ("foreign");
    let graph : InRustGraph =
      InRustGraph::from_graphnodes (&[recorder, member]);

    let mut tree : Tree<Viewnode> =
      Tree::new (viewforest_root_viewnode ());
    let recorder_treeid : NodeId = tree . root_mut () . append (
      mk_editable_viewnode (
        ID::new ("recorder"), SkgrepoName::from ("public"),
        "recorder" . to_string (), None ) ) . id ();
    let member_treeid : NodeId = tree . get_mut (recorder_treeid)
      . unwrap () . append ( mk_editable_viewnode (
        ID::new ("member"), SkgrepoName::from ("foreign"),
        "member" . to_string (), None ) ) . id ();

    set_relRepo (
      &mut tree, member_treeid, true, None, &graph,
      &mut RelRepoLookup::over (&graph), &config );
    let ViewnodeKind::Vognode (Vognode::Occurrence (rendered_member)) =
      & tree . get (member_treeid) . unwrap () . value () . kind
    else { panic! ("member should be unrestricted"); };
    assert_eq! ( rendered_member . viewStats . relRepo, None,
      "the recorder-home default must not render a fake relRepo override" );
  }

  /// A foreign node contains an owned one: the relationship is recorded
  /// in the foreign node's skgrepo, which has no privacy rank, so no
  /// relRepo is marked, though the default (the owned member's home)
  /// differs from it. The same for a foreign node containing a node of
  /// another foreign skgrepo.
  #[test]
  fn a_relationship_recorded_in_a_foreign_skgrepo_has_no_override_herald () {
    let config : SkgConfig = {
      let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
        HashMap::new ();
      for (name, owned) in [ ("public", true), ("ed", false),
                             ("private", true), ("other", false) ] {
        skgrepos . insert (
          SkgrepoName::from (name),
          Skgrepo {
            name         : SkgrepoName::from (name),
            abbreviation : None,
            path         : PathBuf::from (name),
            owned        : owned, } ); }
      let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (skgrepos);
      config . skgrepo_order = ["public", "private", "ed", "other"]
        . into_iter () . map (SkgrepoName::from) . collect ();
      config };
    let node = |pid : &str, home : &str, contains : &[&str]| -> Graphnode {
      let mut node : Graphnode = empty_graphnode ();
      node . pid = ID::new (pid);
      node . title = pid . to_string ();
      node . home_skgrepo = SkgrepoName::from (home);
      node . contains = contains . iter ()
        . map ( |member| RelPartner::at_relRepo (
          SkgrepoName::from (home), ID::new (*member) ))
        . collect ();
      node };
    let graph : InRustGraph = InRustGraph::from_graphnodes (&[
      node ("inboxes", "ed", &["to-ed", "elsewhere"]),
      node ("to-ed", "private", &[]),
      node ("elsewhere", "other", &[]) ]);
    let mut tree : Tree<Viewnode> =
      Tree::new (viewforest_root_viewnode ());
    let recorder_treeid : NodeId = tree . root_mut () . append (
      mk_editable_viewnode (
        ID::new ("inboxes"), SkgrepoName::from ("ed"),
        "inboxes" . to_string (), None ) ) . id ();
    for (member, home) in [ ("to-ed", "private"), ("elsewhere", "other") ] {
      let member_treeid : NodeId = tree . get_mut (recorder_treeid)
        . unwrap () . append ( mk_editable_viewnode (
          ID::new (member), SkgrepoName::from (home),
          member . to_string (), None ) ) . id ();
      assert_ne! ( config . default_relRepo (
                     &SkgrepoName::from ("ed"), &SkgrepoName::from (home) ),
                   SkgrepoName::from ("ed"),
                   "the default differs from the relRepo, so only the foreign rule suppresses the herald" );
      set_relRepo (
        &mut tree, member_treeid, true, None, &graph,
        &mut RelRepoLookup::over (&graph), &config );
      let ViewnodeKind::Vognode (Vognode::Occurrence (rendered_member)) =
        & tree . get (member_treeid) . unwrap () . value () . kind
      else { panic! ("member should be an occurrence"); };
      assert_eq! ( rendered_member . viewStats . relRepo, None,
        "a relationship recorded in the foreign skgrepo ed must not be marked ~ed ({})",
        member ); }
  }
}

/// Sets the attribution of the occurrence at treeid, from its node's.
fn set_attribution (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  node_pid : &ID,
  graph    : &InRustGraph,
  config   : &SkgConfig,
) {
  let attributed : Option<AttributedSkgrepo> = graph . pid_of (node_pid)
    . and_then ( |pid| graph . nodes . get (&pid) )
    . and_then ( |node| node . attribution . as_ref () )
    . map ( |attribution| attributed_skgrepo (config, attribution) );
  if let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . attribution = attributed; }}

/// Whose voice a node's home skgrepo herald names: its home skgrepo,
/// and, for an attributed fork, the skgrepo it was forked from. The
/// herald draws exactly this, "⌂:HOME" or "⌂:<HOME/ORIGINAL>".
type HomeVoice = (SkgrepoName, Option<AttributedSkgrepo>);

/// Sets repoBoundary on the occurrence at treeid: true where its home
/// voice ('HomeVoice') differs from that of the nearest occurrence
/// above it, or where none is above it (a root). So a child of
/// "⌂:<a/b>" in 'a' shows "⌂:a", and an attributed fork under a note
/// of its own skgrepo shows its "⌂:<a/b>"; only a child that would say
/// exactly what the node above it says says nothing. Where only one
/// skgrepo is configured, only a boundary an attribution makes, on
/// either side, is drawn.
fn set_skgrepo_at_boundary (
  tree          : &mut Tree<Viewnode>,
  treeid        : NodeId,
  multi_skgrepo : bool,
) {
  let node_voice : HomeVoice = {
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    ( t . home_skgrepo . clone (), t . viewStats . attribution . clone () ) };
  let ancestor_voice : Option<HomeVoice> =
    nearest_occurrence_ancestor_voice (tree, treeid);
  let attribution_involved : bool =
    node_voice . 1 . is_some ()
    || ancestor_voice . as_ref () . is_some_and ( |voice| voice . 1 . is_some () );
  let at_boundary : bool =
    ( multi_skgrepo || attribution_involved )
    && ancestor_voice . as_ref () != Some (&node_voice);
  if let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . repoBoundary = at_boundary; }}

/// Walk rootward from treeid (exclusive) to find
/// the nearest occurrence ancestor's home voice ('HomeVoice').
fn nearest_occurrence_ancestor_voice (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<HomeVoice> {
  let mut current : NodeId = treeid;
  while let Some (parent_ref)
    = tree . get (current) . unwrap () . parent ()
    { current = parent_ref . id ();
      if let ViewnodeKind::Vognode (Vognode::Occurrence (t))
        = & parent_ref . value () . kind
        { return Some (( t . home_skgrepo . clone (),
                         t . viewStats . attribution . clone () )); }}
  None }

/// The node's 'cycle' field becomes equal to
/// whether the 'ancestor_gnids' argument contains its ID.
fn detect_and_mark_cycle (
  tree            : &mut Tree<Viewnode>,
  treeid          : NodeId,
  node_pid        : &ID,
  ancestor_gnids : &HashSet<ID>,
) {
  if let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . cycle = ancestor_gnids . contains (node_pid); } }

/// The home skgrepo herald is drawn wherever what it would say differs
/// from what the node above says ('set_skgrepo_at_boundary').
#[cfg(test)]
mod home_voice_tests {
  use super::*;
  use crate::types::misc::Skgrepo;
  use crate::types::nodes::attribution::Attribution;
  use crate::types::nodes::complete::{empty_graphnode, Graphnode};
  use crate::types::viewnode::{
    mk_editable_viewnode, viewforest_root_viewnode, viewnode_of};
  use std::path::PathBuf;

  /// A config of the owned skgrepos 'a' and 'z' ('a' alone when not
  /// 'multi'); no attribution names a configured skgrepo, so each is
  /// unverified, which changes nothing here.
  fn config (
    multi : bool,
  ) -> SkgConfig {
    let names : Vec<&str> = if multi { vec! ["a", "z"] } else { vec! ["a"] };
    let skgrepos : HashMap<SkgrepoName, Skgrepo> = names . iter ()
      . map ( |name| ( SkgrepoName::from (*name),
                       Skgrepo { name         : SkgrepoName::from (*name),
                                 abbreviation : None,
                                 path         : PathBuf::from (name),
                                 owned        : true, } ))
      . collect ();
    SkgConfig::dummyFromSkgrepos (skgrepos) }

  /// A node of 'home', forked from a note of 'forked_from' if any.
  fn node (
    pid         : &str,
    home        : &str,
    forked_from : Option<&str>,
  ) -> Graphnode {
    let mut node : Graphnode = empty_graphnode ();
    node . pid = ID::new (pid);
    node . title = pid . to_string ();
    node . home_skgrepo = SkgrepoName::from (home);
    node . attribution = forked_from . map ( |skgrepo| Attribution {
      original : ID::new ("original"), skgrepo : SkgrepoName::from (skgrepo),
      remote_hash : None, commit : None, uncommitted : false } );
    node }

  /// Whether each node of the chain, drawn each under the one before
  /// (with a folder between the first two when 'folder_between'),
  /// stands at a home skgrepo boundary.
  fn boundaries (
    chain          : &[Graphnode],
    multi          : bool,
    folder_between : bool,
  ) -> Vec<bool> {
    let config : SkgConfig = config (multi);
    let graph : InRustGraph = InRustGraph::from_graphnodes (chain);
    let mut tree : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
    let mut parent : NodeId = tree . root () . id ();
    let mut treeids : Vec<NodeId> = Vec::new ();
    for (index, node) in chain . iter () . enumerate () {
      if index == 1 && folder_between {
        parent = tree . get_mut (parent) . unwrap ()
          . append ( viewnode_of ( ViewnodeKind::Folder (Folder::flags ()) )) . id (); }
      let treeid : NodeId = tree . get_mut (parent) . unwrap ()
        . append ( mk_editable_viewnode (
          node . pid . clone (), node . home_skgrepo . clone (),
          node . title . clone (), None )) . id ();
      set_attribution (&mut tree, treeid, &node . pid, &graph, &config);
      set_skgrepo_at_boundary (&mut tree, treeid, multi);
      treeids . push (treeid);
      parent = treeid; }
    treeids . iter () . map ( |treeid| {
      let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
        & tree . get (*treeid) . unwrap () . value () . kind
      else { panic! ("an occurrence") };
      t . viewStats . repoBoundary } ) . collect () }

  #[test]
  fn a_child_says_its_home_unless_the_node_above_says_the_same () {
    for (multi, folder_between) in
      [ (true, false), (true, true), (false, false), (false, true) ] {
      let case : String = format! ( "multi {}, folder between {}", multi, folder_between );
      assert_eq! ( boundaries ( &[ node ("p", "a", Some ("b")), node ("c", "a", None) ],
                                multi, folder_between ),
                   vec! [ true, true ],
                   "a plain child of <a/b> in a says ⌂:a ({})", case );
      assert_eq! ( boundaries ( &[ node ("p", "a", Some ("b")), node ("c", "a", Some ("b")) ],
                                multi, folder_between ),
                   vec! [ true, false ],
                   "a child of <a/b> that would say <a/b> says nothing ({})", case );
      assert_eq! ( boundaries ( &[ node ("p", "a", Some ("b")), node ("c", "a", Some ("c")) ],
                                multi, folder_between ),
                   vec! [ true, true ],
                   "a child <a/c> of <a/b> says <a/c> ({})", case );
      assert_eq! ( boundaries ( &[ node ("p", "a", None), node ("c", "a", Some ("b")) ],
                                multi, folder_between ),
                   vec! [ multi, true ],
                   "an attributed child of a plain note of its home says <a/b> ({})", case );
      assert_eq! ( boundaries ( &[ node ("p", "a", None), node ("c", "a", None) ],
                                multi, folder_between ),
                   vec! [ multi, false ],
                   "a plain child of a plain note of its home says nothing ({})", case ); }
    assert_eq! ( boundaries ( &[ node ("p", "a", None), node ("c", "z", None) ],
                              true, false ),
                 vec! [ true, true ],
                 "a child of another home says its home" ); }
}
