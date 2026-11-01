//! The skgrepo restriction and its two questions
//! ('SkgrepoRestriction', 'record_is_visible', 'node_is_restricted').
//! .
//! The principle: the skgrepo restriction limits what can be edited
//! as well as what can be seen. What the user could not see the user
//! did not edit, so no write may change a record, a section or a node
//! file it keeps out, nor act on the buffer's silence about one.

use crate::types::misc::{RelPartner, SkgConfig, SkgrepoName};

use std::collections::BTreeSet;
use std::error::Error;

/// The skgrepo restriction: the skgrepos a connection currently shows
/// ('docs/glossary.org', "skgrepo, home skgrepo, skgrepo-set,
/// gitrepo"). It is two things: a rung of the privacy ladder (the
/// owned skgrepos, most public first), the most private owned skgrepo
/// shown; and whether the foreign skgrepos are shown, which they are
/// unless the connection hides them. It denotes the rungs through the
/// chosen one, plus every foreign skgrepo if they are shown. A
/// connection is unrestricted when it shows the most private rung and
/// the foreign skgrepos ('is_unrestricted'); the clients then show the
/// rung's name with " (no restriction)" appended, words that never
/// reach the wire or the config.
///
/// It limits what can be edited as well as what can be
/// seen: no save, nodeMerge, fork, deletion, commit or other write may
/// remove, move, rewrite or create a record, a section or a node file
/// it keeps out, nor act on the buffer's silence about such a record.
/// Every write asks 'record_is_visible' or 'node_is_restricted' first,
/// refuses what would land out of sight, or refuses unless the
/// connection is unrestricted ('require_unrestricted').
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SkgrepoRestriction {
  /// The most private owned skgrepo shown; the empty name when the
  /// config owns no skgrepo.
  pub most_private_shown : SkgrepoName,
  /// Whether the foreign skgrepos are shown.
  pub foreign_shown      : bool,
  /// The set it denotes: 'most_private_shown' and every more public
  /// owned skgrepo, and every foreign skgrepo if 'foreign_shown'.
  pub skgrepos           : BTreeSet<SkgrepoName>,
  /// The config's skgrepos it keeps out: the owned ones more private
  /// than 'most_private_shown', in privacy order, then the foreign
  /// ones if hidden. A skgrepo the config does not name is in neither
  /// set, and neither shown nor kept out.
  kept_out               : Vec<SkgrepoName>,
  /// The config's most private owned skgrepo, which an unrestricted
  /// connection shows.
  most_private_owned     : SkgrepoName,
}

impl SkgrepoRestriction {
  /// The skgrepo restriction a connection starts with: the config's
  /// 'default_repo_set' with foreign skgrepos shown, else no skgrepo
  /// restriction.
  pub fn default_from_config (
    config : &SkgConfig,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    match &config . default_skgrepo_set {
      Some (most_private_shown) =>
        SkgrepoRestriction::named (config, most_private_shown . clone ()),
      None => Ok ( SkgrepoRestriction::unrestricted (config) ), } }

  /// The skgrepo restriction whose most private skgrepo shown is
  /// 'most_private_shown', which must be an owned skgrepo of the
  /// config, with foreign skgrepos shown.
  pub fn named (
    config             : &SkgConfig,
    most_private_shown : SkgrepoName,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    SkgrepoRestriction::named_with_foreign_shown (
      config, most_private_shown, true ) }

  /// The skgrepo restriction whose most private skgrepo shown is
  /// 'most_private_shown', which must be an owned skgrepo of the
  /// config (or the empty name, when the config owns none), showing
  /// the foreign skgrepos iff 'foreign_shown'.
  pub fn named_with_foreign_shown (
    config             : &SkgConfig,
    most_private_shown : SkgrepoName,
    foreign_shown      : bool,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    let (shown_owned, more_private_owned)
      : (Vec<SkgrepoName>, Vec<SkgrepoName>) =
      if most_private_shown . 0 . is_empty ()
         && config . privacy_ladder () . is_empty ()
      { (Vec::new (), Vec::new ()) }
      else { config . privacy_ladder_split_after (&most_private_shown)
             . map_err ( |_| format! (
               "'{}' names no owned skgrepo of the config. The skgrepo restriction is the most private owned skgrepo to show; foreign skgrepos are shown or hidden apart from it.",
               most_private_shown )) ? };
    let foreign : Vec<SkgrepoName> =
      config . ordered_skgrepos () . into_iter ()
      . filter ( |name| config . skgrepos . contains_key (name)
                        && ! config . skgrepo_is_owned (name) )
      . collect ();
    let most_private_owned : SkgrepoName =
      more_private_owned . last () . or ( shown_owned . last () )
      . cloned ()
      . unwrap_or_else ( || SkgrepoName::from ("") );
    let mut skgrepos : BTreeSet<SkgrepoName> =
      shown_owned . into_iter () . collect ();
    let mut kept_out : Vec<SkgrepoName> = more_private_owned;
    if foreign_shown { skgrepos . extend (foreign); }
    else             { kept_out . extend (foreign); }
    Ok ( SkgrepoRestriction {
      most_private_shown,
      foreign_shown,
      skgrepos,
      kept_out,
      most_private_owned } ) }

  /// No skgrepo restriction: the most private owned skgrepo of the
  /// config shown, and with it every skgrepo, foreign ones included. A
  /// config that owns no skgrepo has no rung to show; its skgrepo
  /// restriction shows the empty name.
  pub fn unrestricted (
    config : &SkgConfig,
  ) -> SkgrepoRestriction {
    let most_private : SkgrepoName =
      config . privacy_ladder () . last () . cloned ()
      . unwrap_or_else ( || SkgrepoName::from ("") );
    SkgrepoRestriction::named (config, most_private)
      . expect ("the config names its own most private owned skgrepo") }

  /// The same rung with foreign skgrepos shown iff 'foreign_shown',
  /// under 'config'.
  pub fn with_foreign_shown (
    &self,
    config        : &SkgConfig,
    foreign_shown : bool,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    SkgrepoRestriction::named_with_foreign_shown (
      config, self . most_private_shown . clone (), foreign_shown ) }

  /// The same skgrepo restriction under 'config', which a config
  /// reload may have made differ from the one it was made under: no
  /// skgrepo restriction stays none, and a restriction stays at its
  /// most private skgrepo shown, foreign skgrepos shown or hidden as
  /// before. If 'config' no longer names that skgrepo as owned, it is
  /// the rung a connection starts with, as if the connection were new,
  /// foreign skgrepos still shown or hidden as before.
  pub fn under_config (
    &self,
    config : &SkgConfig,
  ) -> SkgrepoRestriction {
    if self . is_unrestricted () {
      return SkgrepoRestriction::unrestricted (config); }
    self . with_foreign_shown (config, self . foreign_shown)
      . unwrap_or_else ( |_| {
        tracing::warn! (
          skgrepo = %self . most_private_shown,
          "the reloaded config no longer names the skgrepo restriction's most private skgrepo shown as owned; starting over from its default");
        SkgrepoRestriction::default_from_config (config)
          . unwrap_or_else ( |_| SkgrepoRestriction::unrestricted (config) )
          . with_foreign_shown (config, self . foreign_shown)
          . unwrap_or_else ( |_| SkgrepoRestriction::unrestricted (config) ) } ) }

  /// The skgrepo restriction over the privacy ladder 'order' (most
  /// public first), with no foreign skgrepos, whose most private
  /// skgrepo shown is 'most_private_shown', for a test without a
  /// config.
  pub fn of_privacy_order_for_test (
    order              : &[&str],
    most_private_shown : &str,
  ) -> SkgrepoRestriction {
    let position : usize =
      order . iter () . position ( |s| *s == most_private_shown )
      . expect ("the order names the most private skgrepo shown");
    SkgrepoRestriction {
      most_private_shown : SkgrepoName::from (most_private_shown),
      foreign_shown      : true,
      skgrepos : order [..= position] . iter ()
        . map ( |s| SkgrepoName::from (*s) ) . collect (),
      kept_out : order [position + 1 ..] . iter ()
        . map ( |s| SkgrepoName::from (*s) ) . collect (),
      most_private_owned : SkgrepoName::from (
        *order . last () . expect ("the order names a skgrepo") ), } }

  /// Whether the set it denotes holds 'skgrepo', a skgrepo of the
  /// config.
  pub fn contains_skgrepo (
    &self,
    skgrepo : &SkgrepoName,
  ) -> bool {
    self . skgrepos . contains (skgrepo) }

  /// Whether it keeps nothing out: the most private skgrepo shown is
  /// the most private owned one, and foreign skgrepos are shown. No
  /// skgrepo restriction.
  pub fn is_unrestricted (
    &self,
  ) -> bool {
    self . kept_out . is_empty () }

  /// The most private owned skgrepo of the config, which an
  /// unrestricted connection shows.
  pub fn most_private_of_the_config (
    &self,
  ) -> &SkgrepoName {
    &self . most_private_owned }

  /// Whether a record (a relationship as one node's file states it,
  /// in the section of the skgrepo 'relRepo') is visible: the
  /// skgrepo restriction does not keep its skgrepo out. A record in a
  /// skgrepo the config does not name is visible under every skgrepo
  /// restriction.
  pub fn record_is_visible (
    restriction : &SkgrepoRestriction,
    relRepo     : &SkgrepoName,
  ) -> bool {
    ! restriction . keeps_out (relRepo) }

  /// Whether a node whose home skgrepo is 'home' is restricted: the
  /// skgrepo restriction keeps its home out. A node homed in a skgrepo
  /// the config does not name is restricted under no skgrepo
  /// restriction.
  pub fn node_is_restricted (
    restriction : &SkgrepoRestriction,
    home        : &SkgrepoName,
  ) -> bool {
    restriction . keeps_out (home) }

  /// The records among 'partners' the skgrepo restriction shows
  /// ('record_is_visible'), in order.
  pub fn visible_records<'a, T> (
    restriction : &'a SkgrepoRestriction,
    partners    : &'a [RelPartner<T>],
  ) -> impl Iterator<Item = &'a RelPartner<T>> + 'a {
    partners . iter ()
      . filter ( |partner| SkgrepoRestriction::record_is_visible (
        restriction, &partner . relRepo )) }

  /// Whether 'partners' records 'member' and the skgrepo restriction
  /// shows that record.
  pub fn member_record_is_visible<T : PartialEq> (
    restriction : &SkgrepoRestriction,
    partners    : &[RelPartner<T>],
    member      : &T,
  ) -> bool {
    partners . iter ()
      . find ( |partner| &partner . member == member )
      . is_some_and ( |partner| SkgrepoRestriction::record_is_visible (
        restriction, &partner . relRepo )) }

  /// The body of both questions: whether 'skgrepo' is among the
  /// config's skgrepos the skgrepo restriction keeps out.
  fn keeps_out (
    &self,
    skgrepo : &SkgrepoName,
  ) -> bool {
    self . kept_out . contains (skgrepo) }
}

/// Refuses unless the connection is unrestricted. 'what' is the
/// refused command, as the subject of the refusal ("The diff view").
pub fn require_unrestricted (
  what        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<(), String> {
  if restriction . is_unrestricted () { return Ok (( )); }
  let foreign_hidden : &str =
    if restriction . foreign_shown { "" }
    else { ", foreign repos hidden" };
  Err ( format! (
    "{} requires no skgrepo restriction; the skgrepo restriction is {}{}. Switch it to {}, the most private skgrepo, with foreign repos shown, first.",
    what, restriction . most_private_shown, foreign_hidden,
    restriction . most_private_of_the_config () )) }
