use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::{graphnode_by_skgid, opt_graphnode_by_skgid};
use crate::from_text::local_fieldintent_collection::lower::nodeMerge_pairs;
use crate::from_text::local_fieldintent_collection::traverse::collect_instructions_locally;
use crate::from_text::local_fieldintent_collection::types::CollectedFieldIntents;
use crate::types::save::{NodeMerge, SaveNode, DeleteNode};
use crate::types::misc::{MSV, RelPartner, SkgConfig, SkgrepoName, ID, members_of, rel_partners_at_relRepo};
use crate::types::nodes::complete::{
  Flag, Graphnode, flag_is_true, set_flag};
use crate::types::list::dedup_vector;
use crate::types::tree::forest::ViewForest;

use std::collections::HashSet;
use std::error::Error;

/// PURPOSE: For each nodeMerge request in the viewforest, this
/// creates a NodeMerge:
/// - acquiree_text_preserver: new node containing the acquiree's title and body
/// - updated_acquirer: acquirer node with modified contents and extra IDs
/// - acquiree_to_delete: acquiree marked for deletion
/// It is a convenience wrapper over local fieldIntent collection
/// plus 'nodeMerge_instructions_from_pairs'; the production save
/// pipeline collects once and calls the pair form directly.
#[allow(non_snake_case)]
pub fn nodeMerge_instructions_from_viewforest (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
) -> Result<Vec<NodeMerge>, Box<dyn Error>> {
  let collected : CollectedFieldIntents =
    collect_instructions_locally (viewforest)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  nodeMerge_instructions_from_pairs (
    &nodeMerge_pairs (&collected), graph, config ) }

/// This builds the NodeMerge triples for a batch of (acquirer,
/// acquiree) pairs, as collected from the buffer by local
/// instruction collection ('nodeMerge_pairs').
#[allow(non_snake_case)]
pub fn nodeMerge_instructions_from_pairs (
  pairs  : &[(ID, ID)],
  graph  : &InRustGraph,
  config : &SkgConfig,
) -> Result<Vec<NodeMerge>, Box<dyn Error>> {
  let mut merges : Vec<NodeMerge> =
    Vec::with_capacity (pairs . len());
  for (acquirer_skgid, acquiree_skgid) in pairs {
    merges . push (
      nodeMerge_from_acquirer_and_acquiree (
        acquirer_skgid, acquiree_skgid, graph, config ) ? ); }
  Ok (merges) }

fn nodeMerge_from_acquirer_and_acquiree (
  acquirer_skgid : &ID,
  acquiree_skgid : &ID,
  graph          : &InRustGraph,
  config         : &SkgConfig,
) -> Result<NodeMerge, Box<dyn Error>> {
  let acquirer_from_disk : Graphnode =
    graphnode_by_skgid (
      graph, config, acquirer_skgid )?;
  let acquiree_from_disk : Graphnode =
    graphnode_by_skgid (
      graph, config, &acquiree_skgid )?;
  let acquiree_text_preserver : Graphnode =
    create_acquiree_text_preserver (&acquiree_from_disk);
  let shown_pre_merge : HashSet<ID> = {
    // Whatever EITHER member showed as unintegrated subscribed
    // content before the merge, the merged node must keep showing
    // (TODO/more.org, "Take something like the intersection of hides
    // when merging"), so these ids are dropped from the combined
    // hides below.
    let mut shown : HashSet<ID> =
      skgids_shown_through_subscriptions (
        &acquirer_from_disk, graph, config ) ?;
    shown . extend (
      skgids_shown_through_subscriptions (
        &acquiree_from_disk, graph, config ) ? );
    shown };
  let updated_acquirer : Graphnode =
    three_nodeMerged_graphnodes( config,
                           &acquirer_from_disk,
                           &acquiree_from_disk,
                           &acquiree_text_preserver,
                           &shown_pre_merge)?;
  Ok(NodeMerge {
    acquiree_text_preserver :
      SaveNode (acquiree_text_preserver),
    updated_acquirer :
      SaveNode (updated_acquirer),
    acquiree_to_delete :
      DeleteNode {
        skgid           : acquiree_skgid . clone(),
        home_skgrepo : acquiree_from_disk . home_skgrepo . clone() }} ) }

/// Computes the updated acquirer node with all fields properly merged.
/// Returns a new Graphnode with:
/// - Combined IDs from both nodes
/// - contains: [acquiree_text_preserver] + acquirer's + acquiree's novel contents
///   - 'Novel' = not among the acquirer's contents
/// - Combined relationship fields (subscribesTo, overrides)
/// - Filtered hidesFromSubs: can't hide your own
///   content, and can't hide what either member SHOWED pre-merge
///   ('shown_pre_merge')
fn three_nodeMerged_graphnodes(
  config: &SkgConfig,
  acquirer_from_disk: &Graphnode,
  acquiree_from_disk: &Graphnode,
  acquiree_text_preserver: &Graphnode,
  shown_pre_merge: &HashSet<ID>,
) -> Result<Graphnode, String> {
  let mut updated_acquirer: Graphnode =
    acquirer_from_disk . clone();
  // Search exclusion is conservative across a merge: the surviving node is
  // excluded only when both inputs were excluded.  The acquiree's original
  // text is preserved separately below, with its own value unchanged.
  set_flag (
    &mut updated_acquirer . flags,
    Flag::NoSearchMatching,
    flag_is_true (
      &acquirer_from_disk . flags, Flag::NoSearchMatching)
    && flag_is_true (
      &acquiree_from_disk . flags, Flag::NoSearchMatching));
  { // Append acquiree's IDs (esp. its PID) to acquirer's extra_ids.
    let mut combined_extra_ids : Vec<ID> =
      acquirer_from_disk . extra_ids . clone();
    combined_extra_ids . push(
      acquiree_from_disk . pid . clone() );
    combined_extra_ids . extend(
      acquiree_from_disk . extra_ids . clone() );
    updated_acquirer . extra_ids =
      dedup_vector (combined_extra_ids); }
  // Combining lists of relation partners (5_plan.org, work item interactions;
  // "compose both, concatenate acquiree-after-acquirer, dedup,
  // decompose"): relRepos are PRESERVED, so a merge cannot silently
  // de-privatize a relationship. On a member both sides carry, the more
  // PRIVATE skgrepo wins (the safe tie-break); every skgrepo clamps at
  // the acquirer's home, since no section may be more public than
  // its home.
  let combine_rel_partners =
    |lists : &[&[RelPartner<ID>]]| -> Vec<RelPartner<ID>> {
      let home    : &SkgrepoName = & updated_acquirer . home_skgrepo;
      let mut out : Vec<RelPartner<ID>> = Vec::new ();
      for list in lists {
        for m in *list {
          let skgrepo : SkgrepoName = config . more_private_of (
            m . relRepo . clone (), home . clone () );
          match out . iter_mut ()
            . find ( |o| o . member == m . member ) {
            Some (existing) => {
              existing . relRepo = config . more_private_of (
                existing . relRepo . clone (), skgrepo ); }
            None => out . push ( RelPartner::at_relRepo (
              skgrepo, m . member . clone () )), }} }
      out };
  let new_contains : Vec<ID> = {
    // [preserver] + acquirer's old content + acquiree's old content
    let mut combined : Vec<ID> =
      vec![ acquiree_text_preserver . pid . clone() ];
    combined . extend (
      members_of (& acquirer_from_disk . contains) );
    combined . extend (
      members_of (& acquiree_from_disk . contains) );
    dedup_vector (combined) };
  updated_acquirer . contains = {
    let mut combined : Vec<RelPartner<ID>> = combine_rel_partners (
      & [ & rel_partners_at_relRepo ( & updated_acquirer . home_skgrepo,
                            vec! [ acquiree_text_preserver . pid . clone() ] ),
          & acquirer_from_disk . contains,
          & acquiree_from_disk . contains ] );
    let own_skgids : Vec<ID> =
      updated_acquirer . all_skgids() . cloned() . collect::<Vec<_>>();
    combined . retain ( // prevent acquirer from containing itself
      |m| ! own_skgids . contains ( &m . member ));
    combined };
  { // Union aliases (parallel to extra_ids): a merged node should
    // still be findable by the acquiree's old aliases.
    let mut combined : Vec<RelPartner<String>> = Vec::new ();
    for list in [ acquirer_from_disk . aliases . or_default (),
                  acquiree_from_disk . aliases . or_default () ] {
      for m in list {
        let skgrepo : SkgrepoName = config . more_private_of (
          m . relRepo . clone (),
          updated_acquirer . home_skgrepo . clone () );
        match combined . iter_mut ()
          . find ( |o| o . member == m . member ) {
          Some (existing) => {
            existing . relRepo = config . more_private_of (
              existing . relRepo . clone (), skgrepo ); }
          None => combined . push ( RelPartner::at_relRepo (
            skgrepo, m . member . clone () )), }} }
    updated_acquirer . aliases =
      MSV::Specified (combined); }
  { // Combine subscribesTo
    updated_acquirer . subscribesTo =
      MSV::Specified ( combine_rel_partners (
        & [ acquirer_from_disk . subscribesTo . or_default (),
            acquiree_from_disk . subscribesTo . or_default () ] )); }
  { // Combine hidesFromSubs, filtering to hide
    // nothing that the acquirer contains, and nothing either member
    // SHOWED through its subscriptions pre-merge: if it was
    // contained in one member's subscribee and unhidden by (and not
    // contained in) that member, the merge keeps showing it -- one
    // member's hide never silences the other's view. ("Something
    // like the intersection": exactly the intersection when both
    // members could see the id through some subscribee.)
    let mut combined : Vec<RelPartner<ID>> = combine_rel_partners (
      & [ acquirer_from_disk . hidesFromSubs
            . or_default (),
          acquiree_from_disk . hidesFromSubs
            . or_default () ] );
    combined . retain ( // if it's in 'new_contains', then it's not here
      |m| ! new_contains . contains ( &m . member ));
    combined . retain (
      |m| ! shown_pre_merge . contains ( &m . member ));
    updated_acquirer . hidesFromSubs =
      MSV::Specified (combined); }
  { // Combine overrides
    updated_acquirer . overrides =
      MSV::Specified ( combine_rel_partners (
        & [ acquirer_from_disk . overrides . or_default (),
            acquiree_from_disk . overrides . or_default () ] )); }
  Ok (updated_acquirer) }

/// The ids 'node' shows as unintegrated subscribed content: contained
/// by some node it subscribes to, and neither hidden by it nor among
/// its own contents (the subscribee-as-such display rule,
/// docs/sharing-model.org). A subscribee with no disk entry
/// contributes nothing.
fn skgids_shown_through_subscriptions (
  node   : &Graphnode,
  graph  : &InRustGraph,
  config : &SkgConfig,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let mut shown : HashSet<ID> = HashSet::new ();
  let hides : Vec<ID> =
    members_of ( node . hidesFromSubs . or_default () );
  let contains : Vec<ID> =
    members_of ( & node . contains );
  for subscribee_skgid in members_of ( node . subscribesTo . or_default () ) {
    let Some (subscribee) = opt_graphnode_by_skgid (
      graph, config, &subscribee_skgid ) ?
    else { continue; };
    for skgid in members_of ( & subscribee . contains ) {
      if ! hides . contains (&skgid)
        && ! contains . contains (&skgid)
      { shown . insert ( skgid ); }} }
  Ok (shown) }

/// Create an acquiree_text_preserver from the acquiree's data
fn create_acquiree_text_preserver(acquiree: &Graphnode) -> Graphnode {
  Graphnode {
    title: format!("MERGED: {}", acquiree . title),
    overPrivateText_telescope: false,
    aliases: MSV::Unspecified,
    home_skgrepo: acquiree . home_skgrepo . clone(),
    pid: ID(uuid::Uuid::new_v4() . to_string()),
    extra_ids: vec![],
    body: acquiree . body . clone(),
    contains                     : vec![],
    subscribesTo                 : MSV::Specified(vec![]),
    hidesFromSubs                : MSV::Specified(vec![]),
    overrides                    : MSV::Specified(vec![]),
    flags                        : if flag_is_true (
      &acquiree . flags, Flag::NoSearchMatching)
      { vec![Flag::NoSearchMatching] }
      else { Vec::new () },
  }}

#[cfg(test)]
mod flag_tests {
  use super::*;
  use crate::types::misc::Skgrepo;
  use crate::types::nodes::complete::{empty_graphnode, flag_is_true};
  use std::collections::HashMap;
  use std::path::PathBuf;

  fn config () -> SkgConfig {
    let skgrepo : SkgrepoName = SkgrepoName::from ("owned");
    SkgConfig::fromSkgreposAndTantivyFolder (
      HashMap::from ([(skgrepo . clone (), Skgrepo {
        name         : skgrepo,
        abbreviation : None,
        path         : PathBuf::from ("owned"),
        owned        : true, })]),
      "/tmp/none" )
  }

  #[test]
  fn merge_search_matching_is_and_while_preserver_keeps_acquiree_value () {
    for (acquirer_value, acquiree_value, expected) in [
      (false, false, false),
      (false, true,  false),
      (true,  false, false),
      (true,  true,  true),
    ] {
      let mut acquirer : Graphnode = Graphnode {
        pid          : ID::from ("A"),
        home_skgrepo : SkgrepoName::from ("owned"),
        .. empty_graphnode () };
      let mut acquiree : Graphnode = Graphnode {
        pid          : ID::from ("B"),
        home_skgrepo : SkgrepoName::from ("owned"),
        .. empty_graphnode () };
      acquirer . flags . extend ([
        Flag::Had_ID_Before_Import,
        Flag::Was_Overloaded]);
      acquiree . flags . push (Flag::Had_ID_Before_Import);
      if acquirer_value {
        acquirer . flags . push (Flag::NoSearchMatching); }
      if acquiree_value {
        acquiree . flags . push (Flag::NoSearchMatching); }
      let preserver : Graphnode = create_acquiree_text_preserver (&acquiree);
      let merged : Graphnode = three_nodeMerged_graphnodes (
        &config (), &acquirer, &acquiree, &preserver, &HashSet::new ())
        . unwrap ();
      assert_eq! ( flag_is_true (
        &merged . flags, Flag::NoSearchMatching), expected );
      assert! (flag_is_true (
        &merged . flags, Flag::Had_ID_Before_Import));
      assert! (flag_is_true (
        &merged . flags, Flag::Was_Overloaded));
      assert_eq! ( flag_is_true (
        &preserver . flags, Flag::NoSearchMatching), acquiree_value );
      assert_eq! (preserver . flags,
        if acquiree_value { vec![Flag::NoSearchMatching] }
        else { Vec::new () }); }
  }
}
