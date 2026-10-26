//! What a refresh changed ('refresh_viewforest'): one buffer
//! correction per change, each naming its headline as a subtree is
//! named, by vnid and title. A buffer correction is a change the
//! refresh made to what a buffer said because the buffer was wrong
//! about the graph. Two changes are not buffer corrections, and are
//! not reported, since the buffer was not wrong: a tombstone the
//! refresh leaves where the saving user's own save deleted the node,
//! and a member the refresh draws because the skgrepo restriction was
//! widened. The refresh learns both from its caller
//! ('BufferCorrectionPolicy'). The refresh reports buffer corrections;
//! it does not warn. Whoever calls it tags them with the view they
//! belong to ('ViewCorrections'), and the save and rerender responses
//! carry them to the client, which shows each view's under a headline
//! of its own in its warnings buffer ('docs/api-and-formats.org',
//! "Save buffer").

use crate::types::errors::{HeadlineName, with_org_subheadlines};
use crate::types::misc::ID;
use crate::types::viewnode::{Viewnode, ViewnodeId};
use crate::types::views_state::ViewforestId;

use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::iter::once;

use ego_tree::{NodeId, Tree};

/// The kinds of correction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CorrectionKind {
  /// A goal item no child stood for was drawn.
  Reinstated,
  /// The headline became a tombstone, which keeps its children.
  Entombstoned,
  /// An unwarranted child that claimed membership was marked 'noClaim'.
  MarkedNoClaim,
  /// Duplicate viewnode siblings were merged into this one, the
  /// viewnode merge's result, which has a fresh vnid.
  SiblingsMerged,
  /// By the ID rule the headline became this kind of vognode, where
  /// it stands: "node", "ghost" or "unknown".
  Rekinded (&'static str),
  /// An edit request the headline's new kind has no slot for was
  /// dropped.
  EditRequestDropped,
}

/// One correction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Correction {
  pub kind  : CorrectionKind,
  /// The headline's vnid: the one it carried, or the one the tail
  /// gave it. None for a headline the tail pruned before it had one.
  pub vnid  : Option<ViewnodeId>,
  /// Its title when the correction was made.
  pub title : String,
}

impl Correction {
  /// The correction as a message: what was done, then the headline
  /// it was done to, named as a subtree ('HeadlineName').
  pub fn message (
    &self,
  ) -> String {
    let what : String = match self . kind {
      CorrectionKind::Reinstated =>
        "Drew what the graph says belongs here." . to_string (),
      CorrectionKind::Entombstoned =>
        "Entombstoned a headline that can stand here for nothing." . to_string (),
      CorrectionKind::MarkedNoClaim =>
        "Marked noClaim a child the graph does not relate to its parent." . to_string (),
      CorrectionKind::SiblingsMerged =>
        "Merged duplicate viewnode siblings into this one." . to_string (),
      CorrectionKind::Rekinded (kind) =>
        format! ("Made a headline a {}, as the graph has it.", kind),
      CorrectionKind::EditRequestDropped =>
        "Dropped an edit request its headline has no slot for." . to_string (), };
    with_org_subheadlines (
      &what,
      &[ HeadlineName::of ( self . vnid . as_ref (), None, &self . title ) ] ) }
}

/// What the caller of a refresh knows that makes a change no buffer
/// correction (see the module comment). The default, which every
/// caller but two passes, reports every change.
#[derive(Clone, Debug, Default)]
pub struct BufferCorrectionPolicy {
  /// The IDs of the nodes the save being rerendered deleted, set by
  /// the save's rerender. A headline standing for one of them that
  /// the refresh entombstones is the saving user's own doing.
  pub deleted_by_the_save          : HashSet<ID>,
  /// Set by the skgrepo-set switch when the restriction changed.
  /// Every open view was up to date before the switch, so what its
  /// rerender draws (a widening) or entombstones (a narrowing) is the
  /// restriction's doing, not the buffer's.
  pub follows_a_restriction_change : bool,
}

impl BufferCorrectionPolicy {
  /// The policy of a save's rerender.
  pub fn after_a_save_deleting (
    deleted_by_the_save : HashSet<ID>,
  ) -> BufferCorrectionPolicy {
    BufferCorrectionPolicy { deleted_by_the_save,
                             follows_a_restriction_change : false } }

  /// The policy of a skgrepo-set switch's rerender.
  pub fn after_a_switch (
    restriction_changed : bool,
  ) -> BufferCorrectionPolicy {
    BufferCorrectionPolicy {
      deleted_by_the_save          : HashSet::new (),
      follows_a_restriction_change : restriction_changed, } }

  pub(crate) fn reports_reinstatement (
    &self,
  ) -> bool {
    ! self . follows_a_restriction_change }

  /// Whether entombstoning a headline that stood for 'gnid' (None for
  /// a headline that stands for no gnid) is a buffer correction.
  pub(crate) fn reports_entombstoning (
    &self,
    gnid : Option<&ID>,
  ) -> bool {
    ! self . follows_a_restriction_change
    && ! gnid . is_some_and ( |gnid| self . deleted_by_the_save . contains (gnid) ) }
}

/// The corrections of one refreshed view, tagged with the view.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ViewCorrections {
  pub viewforest_id : ViewforestId,
  pub corrections   : Vec<Correction>,
}

impl ViewCorrections {
  /// The view's corrections, if it has any.
  pub fn of (
    viewforest_id : &ViewforestId,
    corrections   : Vec<Correction>,
  ) -> Option<ViewCorrections> {
    if corrections . is_empty () { return None; }
    Some ( ViewCorrections { viewforest_id : viewforest_id . clone (),
                             corrections } ) }
}

/// The 'corrections' entry of a save or rerender-done response:
/// '(corrections (("VIEW_ID" "MESSAGE" ...) ...))', one list per view
/// with corrections, the view's ID first, then one message per
/// correction ('Correction::message').
pub fn corrections_entry_sexp (
  views : &[ViewCorrections],
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::Atom ( Atom::S ( "corrections" . to_string () )),
    Sexp::List (
      views . iter ()
      . map ( |view| Sexp::List (
        once (
          Sexp::Atom ( Atom::S ( view . viewforest_id . repr_in_client () )))
        . chain ( view . corrections . iter ()
                  . map ( |correction| Sexp::Atom ( Atom::S ( correction . message () )) ))
        . collect () ))
      . collect () ) ] ) }

/// The corrections of one refresh, recorded during the walk. A
/// headline drawn by the walk has no vnid until the tail assigns one,
/// so each entry keeps where its headline stands.
#[derive(Default)]
pub struct CorrectionLog {
  entries : Vec<(Correction, NodeId)>,
}

impl CorrectionLog {
  /// Record a correction of the headline at 'treeid', as it is now.
  pub(crate) fn record (
    &mut self,
    tree   : &Tree<Viewnode>,
    treeid : NodeId,
    kind   : CorrectionKind,
  ) {
    let viewnode : &Viewnode = tree . get (treeid) . unwrap () . value ();
    self . entries . push ((
      Correction { kind,
                   vnid  : viewnode . viewnode_id . clone (),
                   title : viewnode . title () . to_string () },
      treeid )); }

  /// The corrections, each headline named by the vnid it holds now,
  /// or, if it holds none (the tail pruned it), by the one it held
  /// when the correction was made: a headline drawn by the walk gets
  /// its vnid from the tail, and the result of a viewnode merge a
  /// fresh one. Call after the tail has assigned the vnids.
  pub(crate) fn into_corrections (
    self,
    tree : &Tree<Viewnode>,
  ) -> Vec<Correction> {
    self . entries . into_iter ()
      . map ( |(correction, treeid)| {
        let vnid_now : Option<ViewnodeId> = tree . get (treeid)
          . and_then ( |node| node . value () . viewnode_id . clone () );
        Correction { vnid : vnid_now . or (correction . vnid),
                     .. correction } } )
      . collect () }
}
