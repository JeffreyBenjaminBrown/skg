//! The relationship-herald SEMANTIC FACTS, emitted for the client to
//! render however it likes. The server sends WHAT relates to what (per
//! relation, per side: a member count and which tracked ancestors are
//! members), plus the birth; the CLIENT decides letters,
//! colors, order, and layout (see elisp/heralds-minor-mode.el and
//! nvim/lua/skg/heralds.lua). No presentation lives here anymore.
//!
//! Wire shape (a relation/side/sub-list is omitted when it would be
//! empty apart from its key):
//!   (rels
//!     (contains          (in  COUNT (ancestors GEN...))
//!                        (out COUNT (ancestors GEN...)))
//!     (subscribesTo     (in ...) (out ...))
//!     (overrides (in ...) (out ...))
//!     (hidesFromSubs (in ...) (out ...))
//!     (linksTo          (in COUNT (ancestors GEN...)
//!                            (substantive COUNT (ancestors GEN...)))
//!                        (out COUNT (ancestors GEN...)))
//!     (aliases  K)
//!     (extraIds K)
//!     (flags K)
//!     (birth (RELNAME SIDE [GEN])...))
//! GEN is a generation distance: 1 = viewparent, 2 = grandparent,
//! ... `in` = "N nodes RELATION it"; `out` = "it RELATIONs N nodes".
//! Each birth fact names a relation explaining why this occurrence is
//! here, the side of this node it is on, and the generation of the
//! ancestor it accounts for -- omitted when no ancestor does, as for a
//! node drawn in place of a node it overrides.
//! See TODO/DONE/semantic-heralds-wire/plan.org.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;

/// Which side of a node a relation fact is on: 'In' = "N nodes
/// RELATION it"; 'Out' = "it RELATIONs N nodes".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side { In, Out }

impl Side {
  pub fn wire_name (self) -> &'static str {
    match self { Side::In => "in", Side::Out => "out" } } }

/// One reason an occurrence is drawn where it is: a relation, the side
/// of this node it is on, and the generation of the ancestor it
/// accounts for (None when it accounts for no ancestor).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BirthFact {
  pub relation   : NodeRelation,
  pub side       : Side,
  pub generation : Option<usize>,
}

impl BirthFact {
  pub fn new (
    relation   : NodeRelation,
    side       : Side,
    generation : Option<usize>,
  ) -> BirthFact {
    BirthFact { relation, side, generation } }

  /// '(RELNAME SIDE [GEN])'
  fn sexp (self) -> String {
    match self . generation {
      Some (generation) => format! ( "({} {} {})",
        self . relation . relation_name (), self . side . wire_name (),
        generation ),
      None => format! ( "({} {})",
        self . relation . relation_name (), self . side . wire_name () ), } } }
use crate::types::viewnode::RelationCounts;

/// Per-relation, per-side ancestor-flag generation distances (1 = the
/// viewparent, 2 = grandparent, ...). Transient: computed in the
/// viewnodestats pass and consumed immediately when emitting the
/// relationship heralds.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct AncestorFlags {
  pub contains_in    : Vec<usize>, pub contains_out    : Vec<usize>,
  pub contents_unintegrated_out : Vec<usize>,
  pub links_in       : Vec<usize>, pub links_substantive_in : Vec<usize>,
  pub links_out      : Vec<usize>,
  pub hides_in       : Vec<usize>, pub hides_out       : Vec<usize>,
  pub subscribes_in  : Vec<usize>, pub subscribes_out  : Vec<usize>,
  pub overrides_in   : Vec<usize>, pub overrides_out   : Vec<usize>,
}

impl AncestorFlags {
  /// Record a flag at generation 'generation' for relation 'rel' on the
  /// given side (inbound = "ancestor R's the node", outbound = "the node
  /// R's the ancestor").
  pub fn record (
    &mut self,
    rel        : NodeRelation,
    inbound    : bool,
    generation : usize,
  ) {
    let slot : Option<&mut Vec<usize>> = match (rel, inbound) {
      (NodeRelation::Contains,                  true ) => Some (&mut self . contains_in),
      (NodeRelation::Contains,                  false) => Some (&mut self . contains_out),
      (NodeRelation::LinksTo,               true ) => Some (&mut self . links_in),
      (NodeRelation::LinksTo,               false) => Some (&mut self . links_out),
      (NodeRelation::HidesFromSubs, true )             => Some (&mut self . hides_in),
      (NodeRelation::HidesFromSubs, false)             => Some (&mut self . hides_out),
      (NodeRelation::SubscribesTo,                true ) => Some (&mut self . subscribes_in),
      (NodeRelation::SubscribesTo,                false) => Some (&mut self . subscribes_out),
      (NodeRelation::Overrides,           true )       => Some (&mut self . overrides_in),
      (NodeRelation::Overrides,           false)       => Some (&mut self . overrides_out), };
    if let Some (v) = slot {
      if ! v . contains (&generation) { v . push (generation); } } }
}

/// `(ancestors GEN...)`, or None if empty. Generations are sorted and
/// de-duped for a stable wire.
fn ancestors_sexp (
  flags : &[usize],
) -> Option<String> {
  if flags . is_empty () { return None; }
  let mut g : Vec<usize> = flags . to_vec ();
  g . sort ();
  g . dedup ();
  let nums : Vec<String> = g . iter () . map ( |n| n . to_string () ) . collect ();
  Some ( format! ("(ancestors {})", nums . join (" ")) ) }

/// One ordinary (non-link) side: `(SIDE COUNT (ancestors ...))`, or None
/// when the side is empty (count 0 and no ancestors). COUNT is always
/// present when the side is present -- it is a fact; the client omits it
/// when it equals the ancestor count.
fn side_sexp (
  side  : &str,
  count : usize,
  flags : &[usize],
) -> Option<String> {
  if count == 0 && flags . is_empty () { return None; }
  let mut parts : Vec<String> = vec! [ count . to_string () ];
  if let Some (a) = ancestors_sexp (flags) { parts . push (a); }
  Some ( format! ("({} {})", side, parts . join (" ")) ) }

/// One ordinary relation `(KEY (in ...) (out ...))`, or None if both
/// sides are empty.
fn relation_sexp (
  key       : &str,
  in_count  : usize,
  in_flags  : &[usize],
  out_count : usize,
  out_flags : &[usize],
) -> Option<String> {
  let inb : Option<String> = side_sexp ("in",  in_count,  in_flags);
  let out : Option<String> = side_sexp ("out", out_count, out_flags);
  if inb . is_none () && out . is_none () { return None; }
  let mut inner : Vec<String> = Vec::new ();
  if let Some (s) = inb { inner . push (s); }
  if let Some (s) = out { inner . push (s); }
  Some ( format! ("({} {})", key, inner . join (" ")) ) }

fn contains_sexp (
  counts       : &RelationCounts,
  flags        : &AncestorFlags,
  unintegrated : Option<usize>,
) -> Option<String> {
  let inb : Option<String> = side_sexp (
    "in", counts . containers, &flags . contains_in);
  let out : Option<String> = if let Some (numerator) = unintegrated {
    assert! (numerator <= counts . contents);
    let mut facts : Vec<String> = vec![counts . contents . to_string ()];
    if let Some (a) = ancestors_sexp (&flags . contains_out) {
      facts . push (a); }
    let mut subset : Vec<String> = vec![numerator . to_string ()];
    if let Some (a) = ancestors_sexp (&flags . contents_unintegrated_out) {
      subset . push (a); }
    facts . push (format! ("(unintegrated {})", subset . join (" ")));
    Some (format! ("(out {})", facts . join (" ")))
  } else {
    side_sexp ("out", counts . contents, &flags . contains_out) };
  if inb . is_none () && out . is_none () { return None; }
  let mut sides : Vec<String> = Vec::new ();
  if let Some (s) = inb { sides . push (s); }
  if let Some (s) = out { sides . push (s); }
  Some (format! ("(contains {})", sides . join (" "))) }

/// The linksTo relation reports the substantive inbound subset and
/// distinct resolved outbound targets. All counts are complete facts.
fn links_sexp (
  total             : usize,
  substantive       : usize,
  in_flags          : &[usize],
  substantive_flags : &[usize],
  targets           : usize,
  out_flags         : &[usize],
) -> Option<String> {
  assert! (substantive <= total);
  let inb : Option<String> = if total == 0 && in_flags . is_empty () {
    None
  } else {
    let mut parts : Vec<String> = vec! [ total . to_string () ];
    if let Some (a) = ancestors_sexp (in_flags) { parts . push (a); }
    let mut subset : Vec<String> = vec! [substantive . to_string ()];
    if let Some (a) = ancestors_sexp (substantive_flags) {
      subset . push (a); }
    parts . push (format! ("(substantive {})", subset . join (" ")));
    Some ( format! ("(in {})", parts . join (" ")) ) };
  let out : Option<String> = side_sexp ("out", targets, out_flags);
  if inb . is_none () && out . is_none () { return None; }
  let mut inner : Vec<String> = Vec::new ();
  if let Some (s) = inb { inner . push (s); }
  if let Some (s) = out { inner . push (s); }
  Some ( format! ("(linksTo {})", inner . join (" ")) ) }

/// Emit the semantic `(rels ...)` form for a node from its member
/// counts, alias/extra-id/flag counts, ancestor flags, and birth relations.
/// None when there is nothing to say. Relations are emitted in a fixed
/// order for a stable wire, but order is not meaningful (the client
/// re-orders; the tests canonicalize).
pub fn relationship_heralds_sexp (
  counts    : &RelationCounts,
  aliases   : usize,
  extra_ids : usize,
  flag_count : usize,
  flags     : &AncestorFlags,
  birth     : &[BirthFact],
  unintegrated : Option<usize>,
) -> Option<String> {
  let mut parts : Vec<String> = Vec::new ();
  if let Some (s) = contains_sexp (
    counts, flags, unintegrated) { parts . push (s); }
  if let Some (s) = links_sexp (
    counts . link_total, counts . link_substantive,
    &flags . links_in, &flags . links_substantive_in,
    counts . link_targets, &flags . links_out) { parts . push (s); }
  if let Some (s) = relation_sexp (
    NodeRelation::SubscribesTo . relation_name (), counts . subscribers, &flags . subscribes_in,
    counts . subscribees, &flags . subscribes_out) { parts . push (s); }
  if let Some (s) = relation_sexp (
    NodeRelation::Overrides . relation_name (), counts . overriders, &flags . overrides_in,
    counts . overrides_out, &flags . overrides_out) { parts . push (s); }
  if let Some (s) = relation_sexp (
    NodeRelation::HidesFromSubs . relation_name (), counts . hiders, &flags . hides_in,
    counts . hides, &flags . hides_out) { parts . push (s); }
  if aliases   > 0 { parts . push ( format! ("(aliases {})",  aliases) ); }
  if extra_ids > 0 { parts . push ( format! ("(extraIds {})", extra_ids) ); }
  if flag_count > 0 {
    parts . push ( format! ("(flags {})", flag_count) ); }
  if ! birth . is_empty () {
    let facts : Vec<String> = birth . iter () . map ( |fact| fact . sexp () ) . collect ();
    parts . push ( format! ("(birth {})", facts . join (" ")) ); }
  if parts . is_empty () { None }
  else { Some ( format! ("(rels {})", parts . join (" ")) ) } }
