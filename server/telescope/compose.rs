//! The COMPOSITION: sections (per-repo slices, most public first) -> the
//! node's effective lists of relation partners. Total and deterministic: junk
//! degrades to 'CompositionWarning's, never errors (see types.rs).
//!
//! Semantics, per ordered relation: the composition THROUGH skgrepo k is
//! exactly what a repo-k viewer sees. The most public section
//! mentioning the relation contributes the base list; each more
//! private section's prepend lands at the front, and each of its
//! runs lands immediately after its anchor -- an anchor being any
//! member of the strictly-more-public compose, matched through the
//! caller's 'resolve' (extra-ids: 'pid_of'; identity in tests).
//! Runs sharing an anchor concatenate in file order. A dangling run
//! attaches after the run that precedes it in the file, or joins the
//! prepend if it is first (Jeff's fallback, 4_discussion.org).
//!
//! Unordered relations (hides, overrides) and aliases: union in
//! Skgrepo order; a member repeated across skgrepos keeps its most
//! public occurrence, with a warning.

use crate::telescope::types::{CompositionWarning, ListItem, SectionSlices, Telescope};
use crate::types::misc::{ID, MSV, RelPartner, SkgRepoName};
use crate::types::nodes::complete::{Flag, Graphnode};

use std::collections::HashMap;
use std::io;

/// The compose of one node's sections, as effective lists of relation partners plus
/// title/body text. Field names mirror 'Graphnode'.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ComposedNode {
  pub title                        : Option<String>,
  pub title_skgrepo                : Option<SkgRepoName>,
  pub body                         : Option<String>,
  pub body_skgrepo                 : Option<SkgRepoName>,
  pub home                         : Option<SkgRepoName>,
  // None = NO section mentioned the field (lowers to
  // MSV::Unspecified); contains has no such distinction, like
  // Graphnode's.
  pub aliases                      : Option<Vec<RelPartner<String>>>,
  pub contains                     : Vec<RelPartner<ID>>,
  pub subscribesTo                 : Option<Vec<RelPartner<ID>>>,
  pub hidesFromSubs                : Option<Vec<RelPartner<ID>>>,
  pub overrides                    : Option<Vec<RelPartner<ID>>>,
}

/// THE compose entry point: one telescope on disk -> the effective
/// node, plus whatever the composition complained about. 'resolve' maps
/// extra ids to pids for anchor resolution and must be built from
/// the whole corpus, not just this telescope (else a nodeMerge can
/// dangle an anchor).
///
/// Errors only when no section anywhere carries a title. A title
/// present but BELOW the home composes fine, carrying a
/// 'TitleBelowHome' warning.
pub fn compose_telescope_collecting_warnings (
  telescope : Telescope,
  resolve   : &dyn Fn (&ID) -> ID,
) -> io::Result<(Graphnode, Vec<CompositionWarning>)> {
  let pid       : ID                = telescope . pid () . clone ();
  let extra_ids : Vec<ID>           = telescope . extra_ids ();
  let flags     : Vec<Flag> = telescope . flags ();
  let (composed, warnings) : (ComposedNode, Vec<CompositionWarning>) =
    compose_sections ( & telescope . into_slices (), resolve );
  let mut node : Graphnode = graphnode_from_composition (
    pid . clone (), extra_ids, flags, composed )
    . ok_or_else ( || io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("Telescope '{}' has no title in any section.",
               pid ))) ?;
  node . normalize_skgids ();
  Ok (( node, warnings )) }

/// 'fold_telescope_collecting_warnings', with the warnings logged
/// rather than returned -- for callers with no way to report them.
pub fn compose_telescope (
  telescope : Telescope,
  resolve   : &dyn Fn (&ID) -> ID,
) -> io::Result<Graphnode> {
  let pid : ID = telescope . pid () . clone ();
  let (node, warnings) : (Graphnode, Vec<CompositionWarning>) =
    compose_telescope_collecting_warnings ( telescope, resolve ) ?;
  for w in &warnings {
    tracing::warn! ( pid = %pid, warning = %w,
                     "telescope fold warning" ); }
  Ok (node) }

/// The compose as a Graphnode. None iff the telescope has no
/// sections at all, or no section carried a title anywhere -- the
/// caller decides whether that is a hard load error (it is, at
/// init) or a warning. A title present but BELOW the home is not
/// such a case: it composes, carrying a 'TitleBelowHome' warning.
pub fn graphnode_from_composition (
  pid       : ID,
  extra_ids : Vec<ID>,
  flags     : Vec<Flag>,
  composed    : ComposedNode,
) -> Option<Graphnode> {
  let home                      : SkgRepoName = composed . home ?;
  let overPrivateText_telescope : bool =
    composed . title_skgrepo . as_ref () != Some (&home)
    || composed . body_skgrepo . as_ref ()
       .map ( |skgrepo| skgrepo != &home )
       .unwrap_or (false);
  let msv = |o : Option<Vec<RelPartner<ID>>>|
  -> MSV<RelPartner<ID>> {
    match o {
      None     => MSV::Unspecified,
      Some (v) => MSV::Specified (v), }};
  Some ( Graphnode {
    title                        : composed . title ?,
    overPrivateText_telescope,
    aliases                      : match composed . aliases {
      None     => MSV::Unspecified,
      Some (v) => MSV::Specified (v), },
    home_skgrepo                       : home,
    pid,
    extra_ids,
    body                         : composed . body,
    contains                     : composed . contains,
    subscribesTo                 : msv ( composed . subscribesTo ),
    hidesFromSubs                :
      msv ( composed . hidesFromSubs ),
    overrides                    : msv ( composed . overrides ),
    flags, } ) }

/// Compose SECTIONS (already sorted most public first -- the caller
/// orders them via 'SkgConfig::ordered_repos') into effective
/// lists. 'resolve' maps any ID to its primary ID ('pid_of');
/// anchors and members are compared through it.
pub fn compose_sections (
  sections : &[(SkgRepoName, SectionSlices)],
  resolve  : &dyn Fn (&ID) -> ID,
) -> (ComposedNode, Vec<CompositionWarning>) {
  let mut warnings : Vec<CompositionWarning> = Vec::new ();
  let mut composed : ComposedNode = ComposedNode::default ();
  { // Title/body text select independently: the first title and first body
    // in privacy order win. The home remains the first section,
    // whether or not it carries either scalar.
    composed . home = sections . first ()
      . map ( |(skgrepo, _)| skgrepo . clone () );
    for (skgrepo, section) in sections {
      if let Some (title) = &section . title {
        match &composed . title_skgrepo {
          None => {
            composed . title = Some (title . clone ());
            composed . title_skgrepo = Some (skgrepo . clone ());
            if composed . home . as_ref () != Some (skgrepo) {
              warnings . push ( CompositionWarning::TitleBelowHome {
                home : composed . home . clone ()
                  . expect ("a section establishes the home"),
                title_at : skgrepo . clone (), } ); }}
          Some (selected_at) =>
            warnings . push ( CompositionWarning::NonHomeTitle {
              skgrepo     : skgrepo . clone (),
              selected_at : selected_at . clone (), } ), }}
      if let Some (body) = &section . body {
        match &composed . body_skgrepo {
          None => {
            composed . body = Some (body . clone ());
            composed . body_skgrepo = Some (skgrepo . clone ());
            if composed . home . as_ref () != Some (skgrepo) {
              warnings . push ( CompositionWarning::BodyBelowHome {
                home : composed . home . clone ()
                  . expect ("a section establishes the home"),
                body_at : skgrepo . clone (), } ); }}
          Some (selected_at) =>
            warnings . push ( CompositionWarning::NonHomeBody {
              skgrepo     : skgrepo . clone (),
              selected_at : selected_at . clone (), } ), }} }
    if composed . title . is_none () {
      warnings . push ( CompositionWarning::MissingTitle ); }}
  composed . contains = compose_ordered (
    sections, |s| s . contains . as_deref (),
    resolve, &mut warnings );
  let mentioned = |proj : &dyn Fn (&SectionSlices) -> bool| -> bool {
    sections . iter () . any ( |(_, s)| proj (s) ) };
  composed . subscribesTo =
    if mentioned ( &|s| s . subscribesTo . is_some () ) {
      Some ( compose_ordered (
        sections, |s| s . subscribesTo . as_deref (),
        resolve, &mut warnings ) ) }
    else { None };
  composed . hidesFromSubs =
    if mentioned ( &|s| s . hidesFromSubs . is_some () ) {
      Some ( compose_unordered (
        sections, |s| s . hidesFromSubs . as_deref (),
        resolve, &mut warnings ) ) }
    else { None };
  composed . overrides =
    if mentioned ( &|s| s . overrides . is_some () ) {
      Some ( compose_unordered (
        sections, |s| s . overrides . as_deref (),
        resolve, &mut warnings ) ) }
    else { None };
  composed . aliases = {
    if ! sections . iter () . any ( |(_, s)| s . aliases . is_some () ) {
      None }
    else { // aliases: union like the unordered relations,
      // but members are strings, deduped verbatim.
      let mut seen : std::collections::HashSet<String> =
        std::collections::HashSet::new ();
      let mut out : Vec<RelPartner<String>> = Vec::new ();
      for (skgrepo, s) in sections {
        if let Some (aliases) = &s . aliases {
          for a in aliases {
            if seen . insert ( a . clone () ) {
              out . push ( RelPartner::at_relRepo (
                skgrepo . clone (), a . clone () )); }
            else {
              // No per-alias id to report; reuse DuplicateMember with
              // a synthetic ID carrying the alias text.
              warnings . push ( CompositionWarning::DuplicateMember {
                member : ID ( a . clone () ) } ); }}}}
      Some (out) }};
  (composed, warnings) }

/// One ordered relation's compose. 'slice_of' projects a section's
/// stored item sequence for this relation (None = no opinion).
fn compose_ordered (
  sections : &[(SkgRepoName, SectionSlices)],
  slice_of : impl Fn (&SectionSlices) -> Option<&[ListItem]>,
  resolve  : &dyn Fn (&ID) -> ID,
  warnings : &mut Vec<CompositionWarning>,
) -> Vec<RelPartner<ID>> {
  let mut effective : Vec<RelPartner<ID>> = Vec::new ();
  let mut any_section_yet : bool = false;
  for (skgrepo, s) in sections {
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
              warnings . push ( CompositionWarning::AnchorInBase {
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
                warnings . push ( CompositionWarning::DanglingAnchor {
                  anchor : a . clone () } ); }
              // Fallback: keep writing into whatever preceded this
              // run -- the previous run's queue, or the prepend.
            }}
          ListItem::Member (skgid) => {
            match &dest {
              Dest::Prepend    => prepend . push ( skgid . clone () ),
              Dest::Queue (k)  =>
                queues . get_mut (k) . expect ("queue exists")
                . push ( skgid . clone () ), }} }} }
    // Dedup against the composition so far and within this section.
    let mut seen : std::collections::HashSet<ID> =
      effective . iter ()
      . map ( |m| resolve ( &m . member ) )
      . collect ();
    let mut keep = |skgid : &ID, warnings : &mut Vec<CompositionWarning>|
    -> bool {
      if seen . insert ( resolve (skgid) ) { true }
      else {
        warnings . push ( CompositionWarning::DuplicateMember {
          member : skgid . clone () } );
        false }};
    let mut next : Vec<RelPartner<ID>> =
      Vec::with_capacity ( effective . len ()
                           + prepend . len () );
    for skgid in prepend {
      if keep (&skgid, warnings) {
        next . push ( RelPartner::at_relRepo (
          skgrepo . clone (), skgid )); }}
    for m in effective {
      let key : ID = resolve ( &m . member );
      next . push (m);
      if let Some (queue) = queues . remove (&key) {
        for skgid in queue {
          if keep (&skgid, warnings) {
            next . push ( RelPartner::at_relRepo (
              skgrepo . clone (), skgid )); }} }}
    effective = next; }
  effective }

/// One unordered relation's compose: union in skgrepo order, most public
/// occurrence winning.
fn compose_unordered (
  sections : &[(SkgRepoName, SectionSlices)],
  slice_of : impl Fn (&SectionSlices) -> Option<&[ID]>,
  resolve  : &dyn Fn (&ID) -> ID,
  warnings : &mut Vec<CompositionWarning>,
) -> Vec<RelPartner<ID>> {
  let mut seen : std::collections::HashSet<ID> =
    std::collections::HashSet::new ();
  let mut out : Vec<RelPartner<ID>> = Vec::new ();
  for (skgrepo, s) in sections {
    let Some (members) = slice_of (s) else { continue; };
    for skgid in members {
      if seen . insert ( resolve (skgid) ) {
        out . push ( RelPartner::at_relRepo (
          skgrepo . clone (), skgid . clone () ));
      } else {
        warnings . push ( CompositionWarning::DuplicateMember {
          member : skgid . clone () } ); }}}
  out }
