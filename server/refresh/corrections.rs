//! What a refresh changed ('refresh_viewforest'): one correction per
//! change, each naming its headline as a subtree is named, by vnid
//! and title. The refresh reports corrections; it does not warn.
//! Whoever calls it tags them with the view they belong to.

use crate::types::viewnode::{Viewnode, ViewnodeId};

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
  /// Siblings standing for one gnid were unified into this one.
  Merged,
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

  /// The corrections, each headline that lacked a vnid given the one
  /// it holds now. Call after the tail has assigned the vnids.
  pub(crate) fn into_corrections (
    self,
    tree : &Tree<Viewnode>,
  ) -> Vec<Correction> {
    self . entries . into_iter ()
      . map ( |(correction, treeid)| match correction . vnid {
        Some (_) => correction,
        None => Correction {
          vnid : tree . get (treeid)
                 . and_then ( |node| node . value () . viewnode_id . clone () ),
          .. correction }, } )
      . collect () }
}
