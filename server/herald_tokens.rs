//! The uniform-herald token grammar, assembled in Rust.
//!
//! Each graph relation gets at most one compact token of the shape
//!   [inNum][inLetters] X [outNum][outLetters]
//! where X is the relationship letter (C contains, L textlinks_to,
//! H hides, S subscribes, O overrides), numbers are member counts, and
//! the lowercase letters a/b/c/... flag a tracked ANCESTOR that is a
//! member on that side (a = visible parent, b = grandparent, ...,
//! counting all viewnodes incl. col scaffolds).
//!
//! The L token is special: its inbound side is the "surprising links"
//! digit form a(b,c) and NEVER carries ancestor letters; its outbound
//! side is omitted except for the one orange birth case (a linkSource
//! backpath -> =La=), where it shows letters only.
//!
//! The grammar lives here (not in the declarative rule table) because
//! it is more positional than the lens engine's INTERC can express; the
//! rule table only colors/orders/echoes the assembled strings. See
//! TODO/uniform-heralds/plan.org.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::viewnode::RelationCounts;

/// The five relationship letters, in canonical (blue) display order.
pub const RELATION_ORDER : [NodeRelation; 5] = [
  NodeRelation::Contains,
  NodeRelation::TextlinksTo,
  NodeRelation::HidesFromItsSubscriptions,
  NodeRelation::Subscribes,
  NodeRelation::OverridesViewOf, ];

pub fn relation_letter (
  r : NodeRelation,
) -> char {
  match r {
    NodeRelation::Contains                  => 'C',
    NodeRelation::TextlinksTo               => 'L',
    NodeRelation::HidesFromItsSubscriptions => 'H',
    NodeRelation::Subscribes                => 'S',
    NodeRelation::OverridesViewOf           => 'O', } }

/// Per-relation, per-side ancestor-flag generation distances (1 = a,
/// 2 = b, ...). Transient: computed in the viewnodestats pass and
/// consumed immediately by 'assemble_active'. The inbound L side is
/// absent on purpose -- it never carries ancestor letters.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct AncestorFlags {
  pub contains_in    : Vec<usize>, pub contains_out    : Vec<usize>,
  pub links_out      : Vec<usize>,
  pub hides_in       : Vec<usize>, pub hides_out       : Vec<usize>,
  pub subscribes_in  : Vec<usize>, pub subscribes_out  : Vec<usize>,
  pub overrides_in   : Vec<usize>, pub overrides_out   : Vec<usize>,
}

impl AncestorFlags {
  /// Record a flag at generation 'gen' for relation 'rel' on the given
  /// side (inbound = "ancestor R's the node", outbound = "the node R's
  /// the ancestor"). The inbound L side is dropped (never lettered).
  pub fn record (
    &mut self,
    rel        : NodeRelation,
    inbound    : bool,
    generation : usize,
  ) {
    let slot : Option<&mut Vec<usize>> = match (rel, inbound) {
      (NodeRelation::Contains,                  true ) => Some (&mut self . contains_in),
      (NodeRelation::Contains,                  false) => Some (&mut self . contains_out),
      (NodeRelation::TextlinksTo,               true ) => None, // no inbound L letters
      (NodeRelation::TextlinksTo,               false) => Some (&mut self . links_out),
      (NodeRelation::HidesFromItsSubscriptions, true ) => Some (&mut self . hides_in),
      (NodeRelation::HidesFromItsSubscriptions, false) => Some (&mut self . hides_out),
      (NodeRelation::Subscribes,                true ) => Some (&mut self . subscribes_in),
      (NodeRelation::Subscribes,                false) => Some (&mut self . subscribes_out),
      (NodeRelation::OverridesViewOf,           true ) => Some (&mut self . overrides_in),
      (NodeRelation::OverridesViewOf,           false) => Some (&mut self . overrides_out), };
    if let Some (v) = slot {
      if ! v . contains (&generation) { v . push (generation); } } }
}

/// The two assembled herald strings for an Active node.
#[derive(Debug, Clone, PartialEq)]
pub struct HeraldStrings {
  pub birth : Option<String>, // orange, leads (ABUT to the ☮)
  pub rels  : Option<String>, // blue: non-birth tokens + Ak + Ik
}

/// Render the ancestor letters for a side: distinct generations,
/// sorted, mapped 1->a, 2->b, ... A generation past 'z' renders as the
/// raw number in braces, which should never happen in practice.
fn gen_letters (
  flags : &[usize],
) -> String {
  let mut g : Vec<usize> = flags . to_vec ();
  g . sort ();
  g . dedup ();
  g . iter () . map ( |&n| {
    if (1 ..= 26) . contains (&n) {
      ((b'a' + (n - 1) as u8) as char) . to_string ()
    } else { format! ("{{{}}}", n) } } )
    . collect () }

/// One side of an ordinary (C/H/S/O) token: the count then the ancestor
/// letters, omitting the count when it equals the number of flagged
/// ancestors (and there is at least one). Empty when count 0 and no flags.
fn render_side (
  count : usize,
  flags : &[usize],
) -> String {
  let letters : String = gen_letters (flags);
  let n_flags : usize = letters . chars () . count ();
  if count == 0 && n_flags == 0 { String::new () }
  else if n_flags > 0 && count == n_flags { letters }
  else { format! ("{}{}", count, letters) } }

/// An ordinary (non-L) relationship token, or None if both sides empty.
fn render_token (
  letter    : char,
  in_count  : usize,
  out_count : usize,
  in_flags  : &[usize],
  out_flags : &[usize],
) -> Option<String> {
  let l : String = render_side (in_count,  in_flags);
  let r : String = render_side (out_count, out_flags);
  if l . is_empty () && r . is_empty () { None }
  else { Some ( format! ("{}{}{}", l, letter, r) ) } }

/// The inbound "surprising links" parenthetical: 'a' then a collapsing
/// (b,c) -- omit a zero, omit the comma when c=0, keep it when b=0, omit
/// the whole parenthetical when both are zero. Empty when a=0.
fn link_inbound (
  a : usize, b : usize, c : usize,
) -> String {
  if a == 0 { return String::new (); }
  let inner : String =
    if      b == 0 && c == 0 { String::new () }
    else if c == 0           { format! ("({})",  b) }
    else if b == 0           { format! ("(,{})", c) }
    else                     { format! ("({},{})", b, c) };
  format! ("{}{}", a, inner) }

/// The L token: inbound digit form a(b,c) (no ancestor letters) and an
/// outbound side that is letters-only (the linkSource-birth case). None
/// when both sides empty.
fn render_link_token (
  a : usize, b : usize, c : usize,
  out_flags : &[usize],
) -> Option<String> {
  let l : String = link_inbound (a, b, c);
  let r : String = gen_letters (out_flags);
  if l . is_empty () && r . is_empty () { None }
  else { Some ( format! ("{}L{}", l, r) ) } }

/// The token for one relation, given its counts and ancestor flags.
fn token_for_relation (
  rel    : NodeRelation,
  counts : &RelationCounts,
  flags  : &AncestorFlags,
) -> Option<String> {
  match rel {
    NodeRelation::Contains => render_token (
      'C', counts . containers, counts . contents,
      &flags . contains_in, &flags . contains_out),
    NodeRelation::TextlinksTo => render_link_token (
      counts . link_total, counts . link_surprising,
      counts . link_with_content, &flags . links_out),
    NodeRelation::HidesFromItsSubscriptions => render_token (
      'H', counts . hiders, counts . hides,
      &flags . hides_in, &flags . hides_out),
    NodeRelation::Subscribes => render_token (
      'S', counts . subscribers, counts . subscribees,
      &flags . subscribes_in, &flags . subscribes_out),
    NodeRelation::OverridesViewOf => render_token (
      'O', counts . overriders, counts . overrides_out,
      &flags . overrides_in, &flags . overrides_out), } }

/// The alias / extra-id count tokens (=Ak= / =Ik=), each present only
/// when its count is nonzero.
fn action_tokens (
  aliases : usize, extra_ids : usize,
) -> Vec<String> {
  let mut out : Vec<String> = Vec::new ();
  if aliases   > 0 { out . push ( format! ("A{}", aliases) ); }
  if extra_ids > 0 { out . push ( format! ("I{}", extra_ids) ); }
  out }

fn join_nonempty (
  tokens : Vec<String>,
) -> Option<String> {
  if tokens . is_empty () { None }
  else { Some ( tokens . join (" ") ) } }

/// Assemble the birth (orange) and rels (blue) strings for an Active
/// node from its counts, alias/extra-id counts, ancestor flags, and the
/// ordered list of birth relations (which token(s) lead, in orange).
pub fn assemble_active (
  counts    : &RelationCounts,
  aliases   : usize,
  extra_ids : usize,
  flags     : &AncestorFlags,
  birth     : &[NodeRelation],
) -> HeraldStrings {
  let birth_tokens : Vec<String> =
    birth . iter ()
    . filter_map ( |&rel| token_for_relation (rel, counts, flags) )
    . collect ();
  let blue_tokens : Vec<String> =
    RELATION_ORDER . iter ()
    . filter ( |rel| ! birth . contains (rel) )
    . filter_map ( |&rel| token_for_relation (rel, counts, flags) )
    . chain ( action_tokens (aliases, extra_ids) )
    . collect ();
  HeraldStrings {
    birth : join_nonempty (birth_tokens),
    rels  : join_nonempty (blue_tokens), } }

/// Assemble the counts-only blue rels string (no ancestor flags, no
/// birth) -- for phantoms, which carry graphStats but no view position.
pub fn assemble_counts_only (
  counts    : &RelationCounts,
  aliases   : usize,
  extra_ids : usize,
) -> Option<String> {
  let flags : AncestorFlags = AncestorFlags::default ();
  let blue_tokens : Vec<String> =
    RELATION_ORDER . iter ()
    . filter_map ( |&rel| token_for_relation (rel, counts, &flags) )
    . chain ( action_tokens (aliases, extra_ids) )
    . collect ();
  join_nonempty (blue_tokens) }
