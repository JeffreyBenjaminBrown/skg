// cargo nextest run -E 'test(herald_tokens::)'

use skg::dbs::in_rust_graph::relation_accessors::NodeRelation;
use skg::herald_tokens::{
  assemble_active, assemble_counts_only, AncestorFlags };
use skg::types::viewnode::RelationCounts;

fn counts () -> RelationCounts { RelationCounts::default () }

#[test]
fn contains_birth_examples () {
  // aC : its parent contains it (1 container, the parent).
  let c = RelationCounts { containers : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Contains, true, 1); // parent on inbound C
  let h = assemble_active (
    &c, 0, 0, &f, &[NodeRelation::Contains] );
  assert_eq! ( h . birth . as_deref (), Some ("aC") );
  assert_eq! ( h . rels, None );
}

#[test]
fn contains_counts_and_omission () {
  // 3aC : 3 containers, one the parent.  aC5 : parent contains it, it
  // contains 5.  C4a : it contains 4 incl parent.  Ca : contains parent.
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Contains, true, 1);
  let h = assemble_active (
    &RelationCounts { containers : 3, .. counts () },
    0, 0, &f, &[NodeRelation::Contains] );
  assert_eq! ( h . birth . as_deref (), Some ("3aC") );

  let mut f2 = AncestorFlags::default ();
  f2 . record (NodeRelation::Contains, true, 1);
  let h2 = assemble_active (
    &RelationCounts { containers : 1, contents : 5, .. counts () },
    0, 0, &f2, &[NodeRelation::Contains] );
  assert_eq! ( h2 . birth . as_deref (), Some ("aC5") );

  let mut f3 = AncestorFlags::default ();
  f3 . record (NodeRelation::Contains, false, 1); // parent on outbound C
  let h3 = assemble_active (
    &RelationCounts { contents : 4, .. counts () },
    0, 0, &f3, &[NodeRelation::Contains] );
  assert_eq! ( h3 . birth . as_deref (), Some ("C4a") );

  let mut f4 = AncestorFlags::default ();
  f4 . record (NodeRelation::Contains, false, 1);
  let h4 = assemble_active (
    &RelationCounts { contents : 1, .. counts () },
    0, 0, &f4, &[NodeRelation::Contains] );
  assert_eq! ( h4 . birth . as_deref (), Some ("Ca") );
}

#[test]
fn three_containers_none_tracked () {
  // 3C : 3 containers, none an ancestor; shown blue (no birth here).
  let h = assemble_active (
    &RelationCounts { containers : 3, .. counts () },
    0, 0, &AncestorFlags::default (), &[] );
  assert_eq! ( h . birth, None );
  assert_eq! ( h . rels . as_deref (), Some ("3C") );
}

#[test]
fn subscribee_and_overrider_as_such () {
  // bS : grandparent subscribes to it (subscribee-as-such).
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Subscribes, true, 2);
  let h = assemble_active (
    &RelationCounts { subscribers : 1, .. counts () },
    0, 0, &f, &[NodeRelation::Subscribes] );
  assert_eq! ( h . birth . as_deref (), Some ("bS") );

  // Ob : it overrides its grandparent (overrider-as-such).
  let mut f2 = AncestorFlags::default ();
  f2 . record (NodeRelation::OverridesViewOf, false, 2);
  let h2 = assemble_active (
    &RelationCounts { overrides_out : 1, .. counts () },
    0, 0, &f2, &[NodeRelation::OverridesViewOf] );
  assert_eq! ( h2 . birth . as_deref (), Some ("Ob") );
}

#[test]
fn filter_col_two_birth_heralds () {
  // HiddenInSubscribee: hides herald (dH, subscriber at gen4) FIRST,
  // then contains (bC, subscribee at gen2).
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::HidesFromItsSubscriptions, true, 4);
  f . record (NodeRelation::Contains, true, 2);
  let h = assemble_active (
    &RelationCounts { hiders : 1, containers : 1, .. counts () },
    0, 0, &f,
    &[ NodeRelation::HidesFromItsSubscriptions, NodeRelation::Contains ] );
  assert_eq! ( h . birth . as_deref (), Some ("dH bC") );
  assert_eq! ( h . rels, None );
}

#[test]
fn surprising_links_collapse () {
  // a(b,c)L collapsing forms on the inbound (digit) side, no letters.
  let mk = |a, b, c| render_link (a, b, c);
  assert_eq! ( mk (3, 1, 1), Some ("3(1,1)L".to_string ()) );
  assert_eq! ( mk (3, 0, 0), Some ("3L".to_string ()) );
  assert_eq! ( mk (2, 1, 0), Some ("2(1)L".to_string ()) );
  assert_eq! ( mk (2, 0, 1), Some ("2(,1)L".to_string ()) );
  assert_eq! ( mk (0, 0, 0), None );
}

// Helper that routes through assemble_counts_only to read the L token.
fn render_link (a : usize, b : usize, c : usize) -> Option<String> {
  let c0 = RelationCounts {
    link_total : a, link_surprising : b, link_with_content : c,
    .. counts () };
  assemble_counts_only (&c0, 0, 0)
}

#[test]
fn linksource_birth_outbound_letter () {
  // La : it links to its parent (the one outbound-L case, as birth).
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::TextlinksTo, false, 1);
  let h = assemble_active (
    &counts (), 0, 0, &f, &[NodeRelation::TextlinksTo] );
  assert_eq! ( h . birth . as_deref (), Some ("La") );
}

#[test]
fn action_tokens_and_blue_order () {
  // A root that contains 5, is contained by 2, has 3 aliases & 1 extra id.
  let h = assemble_active (
    &RelationCounts { containers : 2, contents : 5, .. counts () },
    3, 1, &AncestorFlags::default (), &[] );
  assert_eq! ( h . birth, None );
  assert_eq! ( h . rels . as_deref (), Some ("2C5 A3 I1") );
}

#[test]
fn whole_token_omission_even_for_contains () {
  // Empty everything -> nothing.
  let h = assemble_active (
    &counts (), 0, 0, &AncestorFlags::default (), &[] );
  assert_eq! ( h . birth, None );
  assert_eq! ( h . rels, None );
}
