//! What a refresh changed ('refresh_viewforest'): one
//! buffer correction per change, each naming its headline as a subtree is
//! named, by vnid and title. A buffer correction is a change the
//! refresh made to what a buffer said because the buffer was wrong
//! about the graph. The caller of a refresh says which situation it is
//! called in ('BufferCorrectionPolicy'), and so which changes are not
//! buffer corrections, the buffer not having been wrong:
//! - 'ReportAll': none; every change is reported;
//! - 'AfterSave': a save's rerender. A tombstone left where the save
//!   deleted the node; for the nodes its nodeMerge requests touched,
//!   the viewnode merge of their occurrences and the members drawn
//!   under them; and whatever left the buffer, pruned by the tail;
//! - 'AfterRestrictionChange': a skgrepo-set switch's rerender, after
//!   which what the refresh draws, entombstones, marks 'noClaim' or
//!   re-kinds across the skgrepo restriction, and what the tail pruned,
//!   is the skgrepo restriction's doing;
//! - 'Unread': a buffer nobody reads the buffer corrections of (a
//!   first drawing); it is corrected as always, and nobody is told.
//! The refresh reports buffer corrections;
//! it does not warn. Whoever calls it tags them with the view they
//! belong to ('ViewBufferCorrections'), and the save and rerender responses
//! carry them to the client, which shows each view's under a headline
//! of its own in its warnings buffer ('docs/api-and-formats.org',
//! "Save buffer").

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::{HeadlineName, with_org_subheadlines};
use crate::types::misc::ID;
use crate::types::viewnode::{Viewnode, ViewnodeId, VognodeKind};
use crate::types::views_state::ViewforestId;

use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::iter::once;

use ego_tree::{NodeId, NodeRef, Tree};

/// The kinds of buffer correction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BufferCorrectionKind {
  /// A goal item no child stood for was drawn.
  Reinstated,
  /// The headline became a tombstone, which keeps its children.
  Entombstoned,
  /// An unwarranted child that claimed membership was marked 'noClaim'.
  MarkedNoClaim,
  /// Duplicate viewnode siblings were merged into this one, the
  /// viewnode merge's result, which has a fresh vnid.
  ViewnodeMerged,
  /// By the ID rule the headline became this kind of vognode, where
  /// it stands.
  Rekinded (VognodeKind),
  /// An edit request the headline's new kind has no slot for was
  /// dropped.
  EditRequestDropped,
}

/// One buffer correction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BufferCorrection {
  pub kind  : BufferCorrectionKind,
  /// The headline's vnid: the one it carried, or the one the tail
  /// gave it. None for a headline that left the buffer before it had
  /// one.
  pub vnid  : Option<ViewnodeId>,
  /// Its title when the buffer correction was made.
  pub title : String,
}

impl BufferCorrection {
  /// The buffer correction as a message: what was done, then the headline
  /// it was done to, named as a subtree ('HeadlineName').
  pub fn message (
    &self,
  ) -> String {
    let what : String = match self . kind {
      BufferCorrectionKind::Reinstated =>
        "Drew what the graph says belongs here." . to_string (),
      BufferCorrectionKind::Entombstoned =>
        "Entombstoned a headline that can stand here for nothing." . to_string (),
      BufferCorrectionKind::MarkedNoClaim =>
        "Marked noClaim a child the graph does not relate to its parent." . to_string (),
      BufferCorrectionKind::ViewnodeMerged =>
        "Merged duplicate viewnode siblings into this one." . to_string (),
      BufferCorrectionKind::Rekinded (kind) =>
        format! ("Made a headline {}, as the graph has it.",
                 kind . what_a_headline_becomes ()),
      BufferCorrectionKind::EditRequestDropped =>
        "Dropped an edit request its headline has no slot for." . to_string (), };
    with_org_subheadlines (
      &what,
      &[ HeadlineName::of ( self . vnid . as_ref (), None, &self . title ) ] ) }
}

/// The situation a refresh is called in, which says which changes are
/// no buffer corrections (see the module comment). Each question below
/// is one match on it.
#[derive(Clone, Debug)]
pub enum BufferCorrectionPolicy {
  /// Every change is a buffer correction.
  ReportAll,
  /// The rerender of a save. What follows from the save's own
  /// gestures is no buffer correction: a headline entombstoned because
  /// it stood for a node the save deleted; for the nodes its nodeMerge
  /// requests touched, the viewnode merge of their occurrences and the
  /// members drawn under them; and what the tail pruned.
  AfterSave {
    /// Every ID of each node the save deleted.
    deleted    : HashSet<ID>,
    /// The acquirer and the acquiree of each nodeMerge request.
    nodeMerged : HashSet<ID>, },
  /// The rerender of a skgrepo-set switch that changed the
  /// skgrepo restriction. Every open view was up to date before the
  /// switch, so what its rerender draws (a skgrepo widening),
  /// entombstones or marks 'noClaim' (a skgrepo narrowing), and what
  /// it makes a restricted headline or an occurrence or a ghost
  /// again, is the skgrepo restriction's doing, not the buffer's.
  AfterRestrictionChange,
  /// The refresh of a buffer whose buffer corrections nobody reads:
  /// a first drawing (a fresh content view, a buffer of the diff
  /// view, the search buffer, the first refresh of the "integrate
  /// unshown changes" request). The word is coined here: the refresh
  /// corrects as always, and nobody is told.
  Unread,
}

impl BufferCorrectionPolicy {
  /// The policy of a save's rerender.
  pub fn after_a_save (
    deleted    : HashSet<ID>,
    nodeMerged : HashSet<ID>,
  ) -> BufferCorrectionPolicy {
    BufferCorrectionPolicy::AfterSave { deleted, nodeMerged } }

  /// The policy of the rerender of a skgrepo-set switch from the
  /// skgrepo restriction 'before' to the skgrepo restriction 'after'.
  pub fn after_a_switch (
    before : &SkgrepoRestriction,
    after  : &SkgrepoRestriction,
  ) -> BufferCorrectionPolicy {
    if before != after { BufferCorrectionPolicy::AfterRestrictionChange }
    else { BufferCorrectionPolicy::ReportAll } }

  /// Whether anyone reads the refresh's buffer corrections.
  pub(crate) fn is_read (
    &self,
  ) -> bool {
    match self {
      BufferCorrectionPolicy::Unread => false,
      _ => true, } }

  /// Whether drawing a goal item no child stood for, under a parent
  /// that stands for 'parent_gnid' (None for a parent that stands for
  /// no gnid), is a buffer correction.
  pub(crate) fn reports_reinstatement (
    &self,
    parent_gnid : Option<&ID>,
  ) -> bool {
    match self {
      BufferCorrectionPolicy::ReportAll => true,
      BufferCorrectionPolicy::AfterSave { nodeMerged, .. } =>
        ! parent_gnid . is_some_and ( |gnid| nodeMerged . contains (gnid) ),
      BufferCorrectionPolicy::AfterRestrictionChange
        | BufferCorrectionPolicy::Unread => false, } }

  /// Whether merging duplicate viewnode siblings standing for 'gnid'
  /// is a buffer correction.
  pub(crate) fn reports_viewnode_merge (
    &self,
    gnid : &ID,
  ) -> bool {
    match self {
      BufferCorrectionPolicy::ReportAll
        | BufferCorrectionPolicy::AfterRestrictionChange => true,
      BufferCorrectionPolicy::AfterSave { nodeMerged, .. } =>
        ! nodeMerged . contains (gnid),
      BufferCorrectionPolicy::Unread => false, } }

  /// Whether marking 'noClaim' a child that claimed membership and
  /// that the truth does not warrant is a buffer correction.
  pub(crate) fn reports_marking_noClaim (
    &self,
  ) -> bool {
    match self {
      BufferCorrectionPolicy::ReportAll
        | BufferCorrectionPolicy::AfterSave { .. } => true,
      BufferCorrectionPolicy::AfterRestrictionChange
        | BufferCorrectionPolicy::Unread => false, } }

  /// Whether re-kinding a headline to a restricted headline, or from
  /// one to an occurrence or a ghost, is a buffer correction.
  pub(crate) fn reports_rekind_across_the_restriction (
    &self,
  ) -> bool {
    match self {
      BufferCorrectionPolicy::ReportAll
        | BufferCorrectionPolicy::AfterSave { .. } => true,
      BufferCorrectionPolicy::AfterRestrictionChange
        | BufferCorrectionPolicy::Unread => false, } }

  /// Whether a buffer correction of a headline the tail then pruned
  /// is reported, named by the vnid it held. After a save, or a change
  /// of the skgrepo restriction, what left the buffer went by the user's own
  /// doing, and its buffer correction would name a headline the buffer
  /// no longer holds.
  pub(crate) fn reports_what_the_tail_pruned (
    &self,
  ) -> bool {
    match self {
      BufferCorrectionPolicy::ReportAll => true,
      BufferCorrectionPolicy::AfterSave { .. }
        | BufferCorrectionPolicy::AfterRestrictionChange
        | BufferCorrectionPolicy::Unread => false, } }

  /// Whether entombstoning a headline that stood for 'gnid' (None for
  /// a headline that stands for no gnid) is a buffer correction.
  pub(crate) fn reports_entombstoning (
    &self,
    gnid : Option<&ID>,
  ) -> bool {
    match self {
      BufferCorrectionPolicy::ReportAll => true,
      BufferCorrectionPolicy::AfterSave { deleted, .. } =>
        ! gnid . is_some_and ( |gnid| deleted . contains (gnid) ),
      BufferCorrectionPolicy::AfterRestrictionChange
        | BufferCorrectionPolicy::Unread => false, } }
}

/// The buffer corrections of one refreshed view, tagged with the view.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ViewBufferCorrections {
  pub viewforest_id      : ViewforestId,
  pub buffer_corrections : Vec<BufferCorrection>,
}

impl ViewBufferCorrections {
  /// The view's buffer corrections, if it has any.
  pub fn of (
    viewforest_id      : &ViewforestId,
    buffer_corrections : Vec<BufferCorrection>,
  ) -> Option<ViewBufferCorrections> {
    if buffer_corrections . is_empty () { return None; }
    Some ( ViewBufferCorrections { viewforest_id : viewforest_id . clone (),
                                   buffer_corrections } ) }
}

/// The 'corrections' entry of a save or rerender-done response:
/// '(corrections (("VIEW_ID" "MESSAGE" ...) ...))', one list per view
/// with buffer corrections, the view's ID first, then one message per
/// buffer correction ('BufferCorrection::message').
pub fn corrections_entry_sexp (
  views : &[ViewBufferCorrections],
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::Atom ( Atom::S ( "corrections" . to_string () )),
    Sexp::List (
      views . iter ()
      . map ( |view| Sexp::List (
        once (
          Sexp::Atom ( Atom::S ( view . viewforest_id . repr_in_client () )))
        . chain ( view . buffer_corrections . iter ()
                  . map ( |buffer_correction| Sexp::Atom ( Atom::S ( buffer_correction . message () )) ))
        . collect () ))
      . collect () ) ] ) }

/// The buffer corrections of one refresh, recorded during the walk. A
/// headline drawn by the walk has no vnid until the tail assigns one,
/// so each entry keeps where its headline stands.
#[derive(Default)]
pub struct BufferCorrectionLog {
  entries : Vec<(BufferCorrection, NodeId)>,
}

impl BufferCorrectionLog {
  /// Record a buffer correction of the headline at 'treeid', as it is now.
  pub(crate) fn record (
    &mut self,
    tree   : &Tree<Viewnode>,
    treeid : NodeId,
    kind   : BufferCorrectionKind,
  ) {
    let viewnode : &Viewnode = tree . get (treeid) . unwrap () . value ();
    self . entries . push ((
      BufferCorrection { kind,
                         vnid  : viewnode . viewnode_id . clone (),
                         title : viewnode . title () . to_string () },
      treeid )); }

  /// The buffer corrections, each headline named by the vnid it holds
  /// now: a headline drawn by the walk gets its vnid from the tail, and
  /// the result of a viewnode merge a fresh one. A headline no longer
  /// in the buffer (the tail pruned it, or a viewnode merge removed it)
  /// keeps the vnid it held, if 'keep_what_left_the_buffer'
  /// ('BufferCorrectionPolicy::reports_what_the_tail_pruned'); else
  /// its buffer corrections are dropped. Call after the tail has
  /// assigned the vnids.
  pub(crate) fn into_buffer_corrections (
    self,
    tree                      : &Tree<Viewnode>,
    keep_what_left_the_buffer : bool,
  ) -> Vec<BufferCorrection> {
    let root : NodeId = tree . root () . id ();
    self . entries . into_iter ()
      . filter_map ( |(buffer_correction, treeid)| {
        let node : NodeRef<Viewnode> = tree . get (treeid) ?;
        let in_the_buffer : bool =
          node . ancestors () . any ( |ancestor| ancestor . id () == root );
        ( in_the_buffer || keep_what_left_the_buffer ) . then ( || BufferCorrection {
          vnid : node . value () . viewnode_id . clone ()
                 . or (buffer_correction . vnid),
          .. buffer_correction } ) } )
      . collect () }
}
