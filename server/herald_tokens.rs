//! The uniform-herald token grammar, assembled in Rust as STYLED SPANS.
//!
//! Each graph relation gets at most one compact token of the shape
//!   [inNum][inLetters] X [outNum][outLetters]
//! where X is the relationship letter (C contains, L textlinks_to,
//! H hides, S subscribes, O overrides), numbers are member counts, and
//! the lowercase letters a/b/c/... flag a tracked ANCESTOR that is a
//! member on that side (a = visible parent, b = grandparent, ...).
//!
//! Tokens are emitted in the FIXED order C L S O H, then the count
//! tokens A (aliases) and I (extra ids). Each character carries a color;
//! the whole thing is a `Vec<HeraldSpan>` (runs of same-colored text),
//! with a `Sep` span between tokens. Coloring:
//!   - base per token group: C/L blue, S/O/H purple, A/I cyan (black);
//!   - the reason-for-being (birth) token's base is WHITE (black-on-white)
//!     instead of its group color -- it sits in its fixed slot, no longer
//!     frontloaded;
//!   - ancestor-flag letters (a/b/c...) are YELLOW (black-on-yellow),
//!     overriding the base on those characters;
//!   - the multi-contained containers count (the number before C, when
//!     > 1) is ORANGE, overriding the base on that number only.
//! The per-character coloring is why this lives in Rust rather than the
//! declarative rule table (the lens engine colors whole atoms). The rule
//! table only positions the assembled spans (via a sentinel) and the
//! client renders them; see server/heralds.rs and
//! elisp/heralds-minor-mode.el.
//!
//! The L token is special: its inbound side is the "surprising links"
//! digit form a(b,c) and NEVER carries ancestor letters; its outbound
//! side is omitted except for the one birth case (a linkSource backpath
//! -> =La=), where it shows letters only.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::viewnode::{HeraldSpan, RelationCounts, SpanColor};

/// The five relationship letters, in canonical display order C L S O H.
pub const RELATION_ORDER : [NodeRelation; 5] = [
  NodeRelation::Contains,
  NodeRelation::TextlinksTo,
  NodeRelation::Subscribes,
  NodeRelation::OverridesViewOf,
  NodeRelation::HidesFromItsSubscriptions, ];

pub fn relation_letter (
  r : NodeRelation,
) -> char {
  match r {
    NodeRelation::Contains                  => 'C',
    NodeRelation::TextlinksTo               => 'L',
    NodeRelation::HidesFromItsSubscriptions => 'H',
    NodeRelation::Subscribes                => 'S',
    NodeRelation::OverridesViewOf           => 'O', } }

/// The base (non-override) color for a relation's token group.
fn group_base (
  r : NodeRelation,
) -> SpanColor {
  match r {
    NodeRelation::Contains
    | NodeRelation::TextlinksTo => SpanColor::Blue,
    NodeRelation::HidesFromItsSubscriptions
    | NodeRelation::Subscribes
    | NodeRelation::OverridesViewOf => SpanColor::Purple, } }

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

/// One side of an ordinary (C/H/S/O) token, as spans: the count then the
/// ancestor letters, omitting the count when it equals the number of
/// flagged ancestors (and there is at least one). Empty when count 0 and
/// no flags. The count digits are YELLOW-neighboring but their own color
/// is `base`, except on the C inbound side, where a count > 1
/// (multi-contained) makes them ORANGE. Ancestor letters are always
/// YELLOW.
fn side_spans (
  count      : usize,
  flags      : &[usize],
  base       : SpanColor,
  c_inbound  : bool, // the Contains inbound side, where multi-contains applies
) -> Vec<HeraldSpan> {
  let letters : String = gen_letters (flags);
  let n_flags : usize = letters . chars () . count ();
  let mut out : Vec<HeraldSpan> = Vec::new ();
  if count == 0 && n_flags == 0 { return out; }
  let show_count : bool = ! (n_flags > 0 && count == n_flags);
  if show_count {
    let color : SpanColor =
      if c_inbound && count > 1 { SpanColor::Orange } else { base };
    out . push ( HeraldSpan { color, text : count . to_string () } ); }
  if n_flags > 0 {
    out . push ( HeraldSpan { color : SpanColor::Yellow, text : letters } ); }
  out }

fn span ( color : SpanColor, text : &str ) -> HeraldSpan {
  HeraldSpan { color, text : text . to_string () } }

/// The spans for one ordinary (non-L) relationship token, or empty if
/// both sides are empty. `base` is White for the birth token, else the
/// group color.
fn ordinary_token_spans (
  letter    : char,
  in_count  : usize,
  out_count : usize,
  in_flags  : &[usize],
  out_flags : &[usize],
  base      : SpanColor,
  c_inbound : bool,
) -> Vec<HeraldSpan> {
  let l : Vec<HeraldSpan> = side_spans (in_count,  in_flags,  base, c_inbound);
  let r : Vec<HeraldSpan> = side_spans (out_count, out_flags, base, false);
  if l . is_empty () && r . is_empty () { return Vec::new (); }
  let mut out : Vec<HeraldSpan> = l;
  out . push ( span (base, & letter . to_string ()) );
  out . extend (r);
  out }

/// The inbound "surprising links" parenthetical: 'a' then a collapsing
/// (b,c) -- omit a zero, omit the comma when c=0, keep it when b=0, omit
/// the whole parenthetical when both are zero. Empty when a=0. Digits
/// only (never ancestor letters), so it takes the token's base color.
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

/// The L token spans: inbound digit form a(b,c) (base color) and an
/// outbound side that is letters-only (the linkSource-birth case,
/// yellow). Empty when both sides empty.
fn link_token_spans (
  a : usize, b : usize, c : usize,
  out_flags : &[usize],
  base      : SpanColor,
) -> Vec<HeraldSpan> {
  let l : String = link_inbound (a, b, c);
  let r : String = gen_letters (out_flags);
  if l . is_empty () && r . is_empty () { return Vec::new (); }
  let mut out : Vec<HeraldSpan> = Vec::new ();
  if ! l . is_empty () { out . push ( span (base, &l) ); }
  out . push ( span (base, "L") );
  if ! r . is_empty () {
    out . push ( HeraldSpan { color : SpanColor::Yellow, text : r } ); }
  out }

/// The spans for one relation's token (empty if the token is omitted).
fn token_for_relation (
  rel      : NodeRelation,
  counts   : &RelationCounts,
  flags    : &AncestorFlags,
  is_birth : bool,
) -> Vec<HeraldSpan> {
  let base : SpanColor =
    if is_birth { SpanColor::White } else { group_base (rel) };
  match rel {
    NodeRelation::Contains => ordinary_token_spans (
      'C', counts . containers, counts . contents,
      &flags . contains_in, &flags . contains_out, base, true),
    NodeRelation::TextlinksTo => link_token_spans (
      counts . link_total, counts . link_surprising,
      counts . link_with_content, &flags . links_out, base),
    NodeRelation::HidesFromItsSubscriptions => ordinary_token_spans (
      'H', counts . hiders, counts . hides,
      &flags . hides_in, &flags . hides_out, base, false),
    NodeRelation::Subscribes => ordinary_token_spans (
      'S', counts . subscribers, counts . subscribees,
      &flags . subscribes_in, &flags . subscribes_out, base, false),
    NodeRelation::OverridesViewOf => ordinary_token_spans (
      'O', counts . overriders, counts . overrides_out,
      &flags . overrides_in, &flags . overrides_out, base, false), } }

/// The alias / extra-id count tokens (=Ak= / =Ik=), each present only
/// when its count is nonzero. Whole token is black-on-cyan.
fn action_tokens (
  aliases : usize, extra_ids : usize,
) -> Vec<Vec<HeraldSpan>> {
  let mut out : Vec<Vec<HeraldSpan>> = Vec::new ();
  if aliases   > 0 { out . push ( vec![ span (SpanColor::Cyan, & format! ("A{}", aliases)) ] ); }
  if extra_ids > 0 { out . push ( vec![ span (SpanColor::Cyan, & format! ("I{}", extra_ids)) ] ); }
  out }

/// Join per-token span lists into one flat list, inserting a `Sep` span
/// between tokens, then merging adjacent same-color runs.
fn join_tokens (
  tokens : Vec<Vec<HeraldSpan>>,
) -> Vec<HeraldSpan> {
  let mut out : Vec<HeraldSpan> = Vec::new ();
  for (i, tok) in tokens . into_iter () . enumerate () {
    if i > 0 { out . push ( span (SpanColor::Sep, " ") ); }
    out . extend (tok); }
  merge_adjacent (out) }

/// Merge consecutive spans of the same color (e.g. a token's letter and
/// its outbound count, both base). `Sep` spans never merge with colored
/// neighbors, so token boundaries survive.
fn merge_adjacent (
  spans : Vec<HeraldSpan>,
) -> Vec<HeraldSpan> {
  let mut out : Vec<HeraldSpan> = Vec::new ();
  for s in spans {
    match out . last_mut () {
      Some (last) if last . color == s . color =>
        last . text . push_str (& s . text),
      _ => out . push (s), } }
  out }

/// Assemble the styled relationship-herald spans for an Active node,
/// from its counts, alias/extra-id counts, ancestor flags, and the set
/// of birth relations (whose tokens take the black-on-white base).
/// Tokens are emitted in the fixed order C L S O H A I. Empty when there
/// is nothing to show.
pub fn assemble_active (
  counts    : &RelationCounts,
  aliases   : usize,
  extra_ids : usize,
  flags     : &AncestorFlags,
  birth     : &[NodeRelation],
) -> Vec<HeraldSpan> {
  let mut tokens : Vec<Vec<HeraldSpan>> = Vec::new ();
  for &rel in RELATION_ORDER . iter () {
    let ts : Vec<HeraldSpan> =
      token_for_relation (rel, counts, flags, birth . contains (&rel));
    if ! ts . is_empty () { tokens . push (ts); } }
  tokens . extend ( action_tokens (aliases, extra_ids) );
  join_tokens (tokens) }

/// Assemble the counts-only spans (no ancestor flags, no birth) -- for
/// phantoms, which carry graphStats but no view position. The
/// multi-contains orange still applies (it is a pure count fact).
pub fn assemble_counts_only (
  counts    : &RelationCounts,
  aliases   : usize,
  extra_ids : usize,
) -> Vec<HeraldSpan> {
  let flags : AncestorFlags = AncestorFlags::default ();
  let mut tokens : Vec<Vec<HeraldSpan>> = Vec::new ();
  for &rel in RELATION_ORDER . iter () {
    let ts : Vec<HeraldSpan> =
      token_for_relation (rel, counts, &flags, false);
    if ! ts . is_empty () { tokens . push (ts); } }
  tokens . extend ( action_tokens (aliases, extra_ids) );
  join_tokens (tokens) }
