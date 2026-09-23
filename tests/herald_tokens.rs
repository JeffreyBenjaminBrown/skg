// cargo nextest run -E 'test(herald_tokens::)'
//
// The server now emits SEMANTIC relationship facts; presentation lives
// in the clients. These pin the wire the emitter produces.

use skg::dbs::in_rust_graph::relation_accessors::NodeRelation;
use skg::herald_tokens::{relationship_heralds_sexp, AncestorFlags};
use skg::types::viewnode::RelationCounts;

fn counts () -> RelationCounts { RelationCounts::default () }

fn emit (
  c     : &RelationCounts,
  al    : usize,
  ex    : usize,
  f     : &AncestorFlags,
  birth : &[NodeRelation],
) -> Option<String> {
  relationship_heralds_sexp (c, al, ex, 0, f, birth, None) }

#[test]
fn contains_birth () {
  // aC: its parent contains it (1 container, gen 1), born of contains.
  let c = RelationCounts { containers : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Contains, true, 1);
  assert_eq! (
    emit (&c, 0, 0, &f, &[NodeRelation::Contains]) . as_deref (),
    Some ("(rels (contains (in 1 (ancestors 1))) (birth contains))") );
}

#[test]
fn contains_both_sides_and_multi () {
  // 2aC1: 2 containers (one the parent), contains 1, born of contains.
  let c = RelationCounts { containers : 2, contents : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Contains, true, 1);
  assert_eq! (
    emit (&c, 0, 0, &f, &[NodeRelation::Contains]) . as_deref (),
    Some ("(rels (contains (in 2 (ancestors 1)) (out 1)) (birth contains))") );
}

#[test]
fn contains_multi_not_birth () {
  // 3C: 3 containers, none tracked, not a birth. Just the count fact.
  let c = RelationCounts { containers : 3, .. counts () };
  assert_eq! (
    emit (&c, 0, 0, &AncestorFlags::default (), &[]) . as_deref (),
    Some ("(rels (contains (in 3)))") );
}

#[test]
fn subscribee_as_such () {
  // bS: grandparent (gen 2) subscribes to it, born of subscribes.
  let c = RelationCounts { subscribers : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Subscribes, true, 2);
  assert_eq! (
    emit (&c, 0, 0, &f, &[NodeRelation::Subscribes]) . as_deref (),
    Some ("(rels (subscribes (in 1 (ancestors 2))) (birth subscribes))") );
}

#[test]
fn contextual_content_distinguishes_applicable_zero_from_absent () {
  let c = RelationCounts { containers : 2, contents : 8, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::Contains, false, 2);
  f . contents_unintegrated_out . push (2);
  let normal = emit (&c, 0, 0, &f, &[]) . unwrap ();
  assert_eq! (normal, "(rels (contains (in 2) (out 8 (ancestors 2))))");
  let contextual = relationship_heralds_sexp (
    &c, 0, 0, 0, &f, &[], Some (2)) . unwrap ();
  assert_eq! (contextual,
    "(rels (contains (in 2) (out 8 (ancestors 2) (unintegrated 2 (ancestors 2)))))");
  let zero = relationship_heralds_sexp (
    &c, 0, 0, 0, &AncestorFlags::default (), &[], Some (0)) . unwrap ();
  assert_eq! (zero, "(rels (contains (in 2) (out 8 (unintegrated 0))))");
}

#[test]
fn overrider_as_such_out_side () {
  // Ob: it overrides its grandparent (out, gen 2), born of overrides.
  let c = RelationCounts { overrides_out : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::OverridesViewOf, false, 2);
  assert_eq! (
    emit (&c, 0, 0, &f, &[NodeRelation::OverridesViewOf]) . as_deref (),
    Some ("(rels (overrides (out 1 (ancestors 2))) (birth overrides))") );
}

#[test]
fn filter_folder_two_births_in_fixed_order () {
  // HiddenInSubscribee: born of BOTH hides (gen 4) and contains (gen 2).
  // Relations emitted contains-first; birth lists in the given order.
  let c = RelationCounts { hiders : 1, containers : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::HidesFromItsSubscriptions, true, 4);
  f . record (NodeRelation::Contains, true, 2);
  assert_eq! (
    emit (&c, 0, 0, &f,
          &[NodeRelation::HidesFromItsSubscriptions, NodeRelation::Contains])
      . as_deref (),
    Some ("(rels (contains (in 1 (ancestors 2))) \
           (hides (in 1 (ancestors 4))) (birth hides contains))") );
}

#[test]
fn link_counts_and_interesting_subset () {
  let mk = | total, interesting, targets | {
    let cc = RelationCounts {
      link_total : total, link_interesting : interesting,
      link_targets : targets,
      .. counts () };
    emit (&cc, 0, 0, &AncestorFlags::default (), &[]) };
  assert_eq! ( mk (3, 1, 2) . as_deref (),
    Some ("(rels (textlinksTo (in 3 (interesting 1)) (out 2)))") );
  assert_eq! ( mk (3, 0, 0) . as_deref (),
    Some ("(rels (textlinksTo (in 3 (interesting 0))))") );
  assert_eq! ( mk (0, 0, 2) . as_deref (),
    Some ("(rels (textlinksTo (out 2)))") );
  assert_eq! ( mk (0, 0, 0), None );
}

#[test]
fn linksource_birth_out_only () {
  // La: it links to its parent (out, gen 1), born of textlinks.
  let c = RelationCounts { link_targets : 1, .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::TextlinksTo, false, 1);
  assert_eq! (
    emit (&c, 0, 0, &f, &[NodeRelation::TextlinksTo]) . as_deref (),
    Some ("(rels (textlinksTo (out 1 (ancestors 1))) (birth textlinksTo))") );
}

#[test]
fn link_ancestor_membership_includes_interesting_subset () {
  let c = RelationCounts {
    link_total : 2, link_interesting : 1, link_targets : 3,
    .. counts () };
  let mut f = AncestorFlags::default ();
  f . record (NodeRelation::TextlinksTo, true, 1);
  f . links_interesting_in . push (1);
  f . record (NodeRelation::TextlinksTo, false, 2);
  assert_eq! (
    emit (&c, 0, 0, &f, &[]) . as_deref (),
    Some ("(rels (textlinksTo (in 2 (ancestors 1) (interesting 1 (ancestors 1))) (out 3 (ancestors 2))))") );
}

#[test]
fn aliases_and_extra_ids () {
  // A root: contains 5, is contained by 2, 3 aliases, 1 extra id.
  let c = RelationCounts { containers : 2, contents : 5, .. counts () };
  assert_eq! (
    emit (&c, 3, 1, &AncestorFlags::default (), &[]) . as_deref (),
    Some ("(rels (contains (in 2) (out 5)) (aliases 3) (extraIds 1))") );
}

#[test]
fn true_properties_are_counted_and_zero_is_omitted () {
  assert_eq! (
    relationship_heralds_sexp (
      &counts (), 0, 0, 2, &AncestorFlags::default (), &[], None)
      . as_deref (),
    Some ("(rels (properties 2))") );
  assert_eq! (emit (&counts (), 0, 0, &AncestorFlags::default (), &[]),
              None); }

#[test]
fn nothing_to_say_is_none () {
  assert_eq! ( emit (&counts (), 0, 0, &AncestorFlags::default (), &[]),
               None );
}
