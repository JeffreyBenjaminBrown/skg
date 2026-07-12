//! The FOLD: sections (per-level slices, most public first) -> the
//! node's effective leveled lists. Total and deterministic: junk
//! degrades to 'FoldWarning's, never errors (see types.rs).
//!
//! Semantics, per ordered relation: the fold THROUGH level k is
//! exactly what a level-k viewer sees. The most public section
//! mentioning the relation contributes the base list; each more
//! private section's prepend lands at the front, and each of its
//! runs lands immediately after its anchor -- an anchor being any
//! member of the strictly-more-public fold, matched through the
//! caller's 'resolve' (extra-ids: 'pid_of'; identity in tests).
//! Runs sharing an anchor concatenate in file order. A dangling run
//! attaches after the run that precedes it in the file, or joins the
//! prepend if it is first (Jeff's fallback, 4_discussion.org).
//!
//! Unordered relations (hides, overrides) and aliases: union in
//! level order; a member repeated across levels keeps its most
//! public occurrence, with a warning.

use crate::telescope::types::{FoldWarning, ListItem, SectionSlices};
use crate::types::misc::{ID, MSV, PrivaciedMember, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};

use std::collections::HashMap;

/// The fold of one node's sections, as effective leveled lists plus
/// scalars. Field names mirror 'NodeComplete'.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct FoldedNode {
  pub title                        : Option<String>,
  pub body                         : Option<String>,
  pub home                         : Option<SourceName>,
  // None = NO section mentioned the field (lowers to
  // MSV::Unspecified); contains has no such distinction, like
  // NodeComplete's.
  pub aliases                      : Option<Vec<PrivaciedMember<String>>>,
  pub contains                     : Vec<PrivaciedMember<ID>>,
  pub subscribes_to                : Option<Vec<PrivaciedMember<ID>>>,
  pub hides_from_its_subscriptions : Option<Vec<PrivaciedMember<ID>>>,
  pub overrides_view_of            : Option<Vec<PrivaciedMember<ID>>>,
}

/// The fold as a NodeComplete. None iff the telescope has no home
/// (no section carried a title) -- the caller decides whether that
/// is a hard load error (it is, at init) or a warning.
pub fn nodecomplete_from_fold (
  pid       : ID,
  extra_ids : Vec<ID>,
  misc      : Vec<FileProperty>,
  folded    : FoldedNode,
) -> Option<NodeComplete> {
  let home : SourceName = folded . home ?;
  let msv = |o : Option<Vec<PrivaciedMember<ID>>>|
  -> MSV<PrivaciedMember<ID>> {
    match o {
      None     => MSV::Unspecified,
      Some (v) => MSV::Specified (v), }};
  Some ( NodeComplete {
    title                        : folded . title ?,
    aliases                      : match folded . aliases {
      None     => MSV::Unspecified,
      Some (v) => MSV::Specified (v), },
    source                       : home,
    pid,
    extra_ids,
    body                         : folded . body,
    contains                     : folded . contains,
    subscribes_to                : msv ( folded . subscribes_to ),
    hides_from_its_subscriptions :
      msv ( folded . hides_from_its_subscriptions ),
    overrides_view_of            : msv ( folded . overrides_view_of ),
    misc, } ) }

/// Fold SECTIONS (already sorted most public first -- the caller
/// orders them via 'SkgConfig::ordered_sources') into effective
/// lists. 'resolve' maps any ID to its primary ID ('pid_of');
/// anchors and members are compared through it.
pub fn fold_sections (
  sections : &[(SourceName, SectionSlices)],
  resolve  : &dyn Fn (&ID) -> ID,
) -> (FoldedNode, Vec<FoldWarning>) {
  let mut warnings : Vec<FoldWarning> = Vec::new ();
  let mut folded : FoldedNode = FoldedNode::default ();
  { // scalars: the home is the most public section bearing a title.
    for (level, s) in sections {
      match (&folded . title, &s . title) {
        (None, Some (t)) => {
          folded . title = Some ( t . clone () );
          folded . home  = Some ( level . clone () );
          if let Some (b) = &s . body {
            folded . body = Some ( b . clone () ); }}
        (Some (_), Some (_)) => {
          warnings . push ( FoldWarning::NonHomeTitle {
            level : level . clone () } );
          if s . body . is_some ()
          && folded . body . is_none () {
            // A stray body rides its stray title's warning.
            warnings . push ( FoldWarning::NonHomeBody {
              level : level . clone () } ); }}
        _ => {
          if s . body . is_some () && folded . title . is_none () {
            // body in a titleless section, before any home was seen
            warnings . push ( FoldWarning::NonHomeBody {
              level : level . clone () } ); }} }}
    if folded . title . is_none () {
      warnings . push ( FoldWarning::MissingTitle ); }}
  folded . contains = fold_ordered (
    sections, |s| s . contains . as_deref (),
    resolve, &mut warnings );
  let mentioned = |proj : &dyn Fn (&SectionSlices) -> bool| -> bool {
    sections . iter () . any ( |(_, s)| proj (s) ) };
  folded . subscribes_to =
    if mentioned ( &|s| s . subscribes_to . is_some () ) {
      Some ( fold_ordered (
        sections, |s| s . subscribes_to . as_deref (),
        resolve, &mut warnings ) ) }
    else { None };
  folded . hides_from_its_subscriptions =
    if mentioned ( &|s| s . hides_from_its_subscriptions . is_some () ) {
      Some ( fold_unordered (
        sections, |s| s . hides_from_its_subscriptions . as_deref (),
        resolve, &mut warnings ) ) }
    else { None };
  folded . overrides_view_of =
    if mentioned ( &|s| s . overrides_view_of . is_some () ) {
      Some ( fold_unordered (
        sections, |s| s . overrides_view_of . as_deref (),
        resolve, &mut warnings ) ) }
    else { None };
  folded . aliases = {
    if ! sections . iter () . any ( |(_, s)| s . aliases . is_some () ) {
      None }
    else { // aliases: union like the unordered relations,
      // but members are strings, deduped verbatim.
      let mut seen : std::collections::HashSet<String> =
        std::collections::HashSet::new ();
      let mut out : Vec<PrivaciedMember<String>> = Vec::new ();
      for (level, s) in sections {
        if let Some (aliases) = &s . aliases {
          for a in aliases {
            if seen . insert ( a . clone () ) {
              out . push ( PrivaciedMember::at (
                level . clone (), a . clone () )); }
            else {
              // No per-alias id to report; reuse DuplicateMember with
              // a synthetic ID carrying the alias text.
              warnings . push ( FoldWarning::DuplicateMember {
                member : ID ( a . clone () ) } ); }}}}
      Some (out) }};
  (folded, warnings) }

/// One ordered relation's fold. 'slice_of' projects a section's
/// stored item sequence for this relation (None = no opinion).
fn fold_ordered (
  sections : &[(SourceName, SectionSlices)],
  slice_of : impl Fn (&SectionSlices) -> Option<&[ListItem]>,
  resolve  : &dyn Fn (&ID) -> ID,
  warnings : &mut Vec<FoldWarning>,
) -> Vec<PrivaciedMember<ID>> {
  let mut effective : Vec<PrivaciedMember<ID>> = Vec::new ();
  let mut any_section_yet : bool = false;
  for (level, s) in sections {
    let Some (items) = slice_of (s) else { continue; };
    let is_base : bool = ! any_section_yet;
    any_section_yet = true;
    // Parse items into prepend + per-anchor queues. Runs sharing an
    // anchor concatenate in file order; a dangling anchor's run
    // attaches after the preceding run (or joins the prepend).
    let mut prepend : Vec<ID> = Vec::new ();
    let mut queue_keys : Vec<ID> = Vec::new (); // resolved, insertion-ordered
    let mut queues : HashMap<ID, Vec<ID>> = HashMap::new ();
    { let known : std::collections::HashSet<ID> =
        effective . iter ()
        . map ( |m| resolve ( &m . member ) )
        . collect ();
      enum Dest { Prepend, Queue (ID) }
      let mut dest : Dest = Dest::Prepend;
      for item in items {
        match item {
          ListItem::Anchor { anchor : a } => {
            let key : ID = resolve (a);
            if is_base {
              warnings . push ( FoldWarning::AnchorInBase {
                anchor : a . clone () } );
              // fall through to the dangling handling below
            }
            if known . contains (&key) {
              if ! queues . contains_key (&key) {
                queue_keys . push ( key . clone () );
                queues . insert ( key . clone (), Vec::new () ); }
              dest = Dest::Queue (key);
            } else {
              if ! is_base { // base already warned above
                warnings . push ( FoldWarning::DanglingAnchor {
                  anchor : a . clone () } ); }
              // Fallback: keep writing into whatever preceded this
              // run -- the previous run's queue, or the prepend.
            }}
          ListItem::Member (id) => {
            match &dest {
              Dest::Prepend    => prepend . push ( id . clone () ),
              Dest::Queue (k)  =>
                queues . get_mut (k) . expect ("queue exists")
                . push ( id . clone () ), }} }} }
    // Dedup against the fold so far and within this section.
    let mut seen : std::collections::HashSet<ID> =
      effective . iter ()
      . map ( |m| resolve ( &m . member ) )
      . collect ();
    let mut keep = |id : &ID, warnings : &mut Vec<FoldWarning>|
    -> bool {
      if seen . insert ( resolve (id) ) { true }
      else {
        warnings . push ( FoldWarning::DuplicateMember {
          member : id . clone () } );
        false }};
    let mut next : Vec<PrivaciedMember<ID>> =
      Vec::with_capacity ( effective . len ()
                           + prepend . len () );
    for id in prepend {
      if keep (&id, warnings) {
        next . push ( PrivaciedMember::at (
          level . clone (), id )); }}
    for m in effective {
      let key : ID = resolve ( &m . member );
      next . push (m);
      if let Some (queue) = queues . remove (&key) {
        for id in queue {
          if keep (&id, warnings) {
            next . push ( PrivaciedMember::at (
              level . clone (), id )); }} }}
    effective = next; }
  effective }

/// One unordered relation's fold: union in level order, most public
/// occurrence winning.
fn fold_unordered (
  sections : &[(SourceName, SectionSlices)],
  slice_of : impl Fn (&SectionSlices) -> Option<&[ID]>,
  resolve  : &dyn Fn (&ID) -> ID,
  warnings : &mut Vec<FoldWarning>,
) -> Vec<PrivaciedMember<ID>> {
  let mut seen : std::collections::HashSet<ID> =
    std::collections::HashSet::new ();
  let mut out : Vec<PrivaciedMember<ID>> = Vec::new ();
  for (level, s) in sections {
    let Some (members) = slice_of (s) else { continue; };
    for id in members {
      if seen . insert ( resolve (id) ) {
        out . push ( PrivaciedMember::at (
          level . clone (), id . clone () ));
      } else {
        warnings . push ( FoldWarning::DuplicateMember {
          member : id . clone () } ); }}}
  out }
