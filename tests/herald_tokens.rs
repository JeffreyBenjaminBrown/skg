// cargo nextest run -E 'test(herald_tokens::)'

use skg::dbs::in_rust_graph::relation_accessors::NodeRelation;
use skg::herald_tokens::{
  assemble_active, assemble_counts_only, AncestorFlags };
use skg::types::viewnode::{HeraldSpan, RelationCounts, SpanColor};

fn counts () -> RelationCounts { RelationCounts::default () }

fn sp (color : SpanColor, text : &str) -> HeraldSpan {
  HeraldSpan { color, text : text . to_string () } }

/// The visible text (all span texts concatenated) -- the token order and
/// characters a user would read, ignoring color.
fn text (spans : &[HeraldSpan]) -> String {
  spans . iter () . map ( |s| s . text . as_str () ) . collect () }

#[test]
fn contains_birth_examples () {
  // aC : its parent contains it (1 container, the parent). The birth
  // token is black-on-white; the ancestor letter 'a' is yellow.
  let c = RelationCounts { containers : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Contains, true, 1); // parent on inbound C
  let h = assemble_active (
    &c, 0, 0, &f, &[NodeRelation::Contains] );
  assert_eq! ( h, vec![ sp (SpanColor::Yellow, "a"),
                        sp (SpanColor::White,  "C") ] );
}

#[test]
fn contains_counts_and_multicontained () {
  // aC5 : parent contains it, it contains 5. (birth C, base white)
  let mut f2 = AncestorFlags::default ();
  f2 . record (NodeRelation::Contains, true, 1);
  let h2 = assemble_active (
    &RelationCounts { containers : 1, contents : 5, .. counts () },
    0, 0, &f2, &[NodeRelation::Contains] );
  assert_eq! ( h2, vec![ sp (SpanColor::Yellow, "a"),
                         sp (SpanColor::White,  "C5") ] );

  // 3aC : 3 containers, one the parent. The "3" before C is orange
  // (multi-contained), the "a" yellow, the "C" white (birth).
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Contains, true, 1);
  let h = assemble_active (
    &RelationCounts { containers : 3, .. counts () },
    0, 0, &f, &[NodeRelation::Contains] );
  assert_eq! ( h, vec![ sp (SpanColor::Orange, "3"),
                        sp (SpanColor::Yellow, "a"),
                        sp (SpanColor::White,  "C") ] );

  // C4a : it contains 4 incl parent (outbound). Ca : contains parent.
  let mut f3 = AncestorFlags::default ();
  f3 . record (NodeRelation::Contains, false, 1); // parent on outbound C
  let h3 = assemble_active (
    &RelationCounts { contents : 4, .. counts () },
    0, 0, &f3, &[NodeRelation::Contains] );
  assert_eq! ( text (&h3), "C4a" );

  let mut f4 = AncestorFlags::default ();
  f4 . record (NodeRelation::Contains, false, 1);
  let h4 = assemble_active (
    &RelationCounts { contents : 1, .. counts () },
    0, 0, &f4, &[NodeRelation::Contains] );
  assert_eq! ( h4, vec![ sp (SpanColor::White,  "C"),
                         sp (SpanColor::Yellow, "a") ] );
}

#[test]
fn multicontained_even_when_not_birth () {
  // 3C : 3 containers, none an ancestor, no birth. The C token base is
  // blue, but the multi-contains "3" is still orange.
  let h = assemble_active (
    &RelationCounts { containers : 3, .. counts () },
    0, 0, &AncestorFlags::default (), &[] );
  assert_eq! ( h, vec![ sp (SpanColor::Orange, "3"),
                        sp (SpanColor::Blue,   "C") ] );
}

#[test]
fn subscribee_and_overrider_as_such () {
  // bS : grandparent subscribes to it (subscribee-as-such). Purple group
  // but birth here, so white; the 'b' ancestor letter is yellow.
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Subscribes, true, 2);
  let h = assemble_active (
    &RelationCounts { subscribers : 1, .. counts () },
    0, 0, &f, &[NodeRelation::Subscribes] );
  assert_eq! ( h, vec![ sp (SpanColor::Yellow, "b"),
                        sp (SpanColor::White,  "S") ] );

  // Ob : it overrides its grandparent (overrider-as-such).
  let mut f2 = AncestorFlags::default ();
  f2 . record (NodeRelation::OverridesViewOf, false, 2);
  let h2 = assemble_active (
    &RelationCounts { overrides_out : 1, .. counts () },
    0, 0, &f2, &[NodeRelation::OverridesViewOf] );
  assert_eq! ( h2, vec![ sp (SpanColor::White,  "O"),
                         sp (SpanColor::Yellow, "b") ] );
}

#[test]
fn purple_group_when_not_birth () {
  // 2S : two subscribees, not birth -> purple base.
  let h = assemble_active (
    &RelationCounts { subscribees : 2, .. counts () },
    0, 0, &AncestorFlags::default (), &[] );
  assert_eq! ( h, vec![ sp (SpanColor::Purple, "S2") ] );
}

#[test]
fn filter_col_two_birth_tokens_in_fixed_order () {
  // HiddenInSubscribee: born of BOTH hides and contains. The tokens now
  // sit in the FIXED order C ... H (no more frontloading): 'bC' then
  // 'dH', both white (birth), the ancestor letters yellow.
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::HidesFromItsSubscriptions, true, 4);
  f . record (NodeRelation::Contains, true, 2);
  let h = assemble_active (
    &RelationCounts { hiders : 1, containers : 1, .. counts () },
    0, 0, &f,
    &[ NodeRelation::HidesFromItsSubscriptions, NodeRelation::Contains ] );
  assert_eq! ( text (&h), "bC dH" );
  assert_eq! ( h, vec![ sp (SpanColor::Yellow, "b"),
                        sp (SpanColor::White,  "C"),
                        sp (SpanColor::Sep,    " "),
                        sp (SpanColor::Yellow, "d"),
                        sp (SpanColor::White,  "H") ] );
}

#[test]
fn surprising_links_collapse () {
  // a(b,c)L collapsing forms on the inbound (digit) side, no letters.
  let mk = |a, b, c| render_link (a, b, c);
  assert_eq! ( mk (3, 1, 1), "3(1,1)L" );
  assert_eq! ( mk (3, 0, 0), "3L" );
  assert_eq! ( mk (2, 1, 0), "2(1)L" );
  assert_eq! ( mk (2, 0, 1), "2(,1)L" );
  assert_eq! ( mk (0, 0, 0), "" );
}

// Helper that routes through assemble_counts_only to read the L token.
fn render_link (a : usize, b : usize, c : usize) -> String {
  let c0 = RelationCounts {
    link_total : a, link_surprising : b, link_with_content : c,
    .. counts () };
  text ( &assemble_counts_only (&c0, 0, 0) ) }

#[test]
fn linksource_birth_outbound_letter () {
  // La : it links to its parent (the one outbound-L case, as birth). The
  // 'L' is white (birth), the 'a' yellow.
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::TextlinksTo, false, 1);
  let h = assemble_active (
    &counts (), 0, 0, &f, &[NodeRelation::TextlinksTo] );
  assert_eq! ( h, vec![ sp (SpanColor::White,  "L"),
                        sp (SpanColor::Yellow, "a") ] );
}

#[test]
fn action_tokens_and_fixed_order () {
  // A root that contains 5, is contained by 2, has 3 aliases & 1 extra
  // id. Fixed order C ... A I; the "2" before C is orange
  // (multi-contained), C5 blue, A3/I1 cyan.
  let h = assemble_active (
    &RelationCounts { containers : 2, contents : 5, .. counts () },
    3, 1, &AncestorFlags::default (), &[] );
  assert_eq! ( text (&h), "2C5 A3 I1" );
  assert_eq! ( h, vec![ sp (SpanColor::Orange, "2"),
                        sp (SpanColor::Blue,   "C5"),
                        sp (SpanColor::Sep,    " "),
                        sp (SpanColor::Cyan,   "A3"),
                        sp (SpanColor::Sep,    " "),
                        sp (SpanColor::Cyan,   "I1") ] );
}

#[test]
fn whole_token_omission_even_for_contains () {
  // Empty everything -> nothing.
  let h = assemble_active (
    &counts (), 0, 0, &AncestorFlags::default (), &[] );
  assert! ( h . is_empty () );
}
