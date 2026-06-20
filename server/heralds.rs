/// PURPOSE: The herald rule table -- the single source of truth for
/// how the Emacs client displays '(skg ...)' metadata as short
/// colored "herald" tokens.
///
/// The DATA lives here; the code that USES the data (the lens
/// engine, 'elisp/skg-sexpr/skg-lens.el') lives entirely in Emacs.
/// Emacs fetches the table over the "herald rules" endpoint at
/// connect time and hands it, unchanged, to that engine. Rust never
/// interprets colors or labels; it only guarantees, via the
/// conformance test in 'tests/unit/heralds.rs', that the table and
/// the metadata vocabulary the server emits stay in sync.
///
/// GRAMMAR (mirrored one-to-one by 'HeraldRule'; full display
/// semantics are documented in skg-lens.el):
///   RULE        ::= [COLOR] (INTERC-BODY | SIMPLE-BODY)
///   INTERC-BODY ::= INTERC SEP [LABEL] CHILDREN...
///   SIMPLE-BODY ::= LABEL [ABUT] CHILDREN...
/// where each child is a literal string, the IT directive, or a
/// nested RULE. The special label ANY matches any leaf; IT echoes
/// the matched value(s).

use crate::dbs::in_rust_graph::relation_accessors::PARTNER_ROLE_VOCAB;
use crate::types::viewnode::{PartnerCol, Qual, QualCol, ViewRequest};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeraldColor { Red, Green, Blue, Yellow, Orange }

impl HeraldColor {
  pub fn repr_in_client (self) -> &'static str {
    match self {
      HeraldColor::Red    => "RED",
      HeraldColor::Green  => "GREEN",
      HeraldColor::Blue   => "BLUE",
      HeraldColor::Yellow => "YELLOW",
      HeraldColor::Orange => "ORANGE",
    }}}

/// One element of a rule's tail.
#[derive(Debug, Clone, PartialEq)]
pub enum RuleChild {
  Str  (&'static str), // serialized as a quoted string
  It,                  // the IT engine directive (echo the matched value)
  Rule (HeraldRule),   // a nested sub-rule
}

/// One rule, mirroring the sexp grammar above one-to-one.
#[derive(Debug, Clone, PartialEq)]
pub struct HeraldRule {
  pub color    : Option<HeraldColor>,
  pub interc   : Option<&'static str>, // Some(separator) makes this an INTERC rule
  pub label    : Option<&'static str>, // None only for unlabeled INTERC rules
  pub abut     : bool,                 // glue output to the preceding token
  pub children : Vec<RuleChild>,
}

//
// Constructor sugar, so the table below stays close to the sexp it serializes to.
//

use HeraldColor::{Red, Green, Blue, Yellow, Orange};

/// (label children...)
fn rule (
  label    : &'static str,
  children : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    color : None, interc : None, label : Some (label),
    abut : false, children } ) }

/// (COLOR label children...)
fn crule (
  color    : HeraldColor,
  label    : &'static str,
  children : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    color : Some (color), interc : None, label : Some (label),
    abut : false, children } ) }

/// (COLOR label "text") -- a one-token leaf
fn leaf (
  color : HeraldColor,
  label : &'static str,
  text  : &'static str,
) -> RuleChild {
  crule ( color, label, vec! [ s (text) ] ) }

/// (label) -- matches and emits nothing; consumes the atom so the
/// table documents it. (The engine ignores unmatched atoms anyway;
/// vacuous rules exist so the conformance test can see them.)
fn vac (
  label : &'static str,
) -> RuleChild {
  rule ( label, vec! [] ) }

/// (COLOR label ABUT "text")
fn leaf_abut (
  color : HeraldColor,
  label : &'static str,
  text  : &'static str,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    color : Some (color), interc : None, label : Some (label),
    abut : true, children : vec! [ s (text) ] } ) }

/// (ANY children...)
fn any (
  children : Vec<RuleChild>,
) -> RuleChild {
  rule ( "ANY", children ) }

/// ([COLOR] INTERC "sep" [label] children...)
fn interc (
  color    : Option<HeraldColor>,
  sep      : &'static str,
  label    : Option<&'static str>,
  children : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    color, interc : Some (sep), label,
    abut : false, children } ) }

fn s ( text : &'static str ) -> RuleChild { RuleChild::Str (text) }

/// Birth herald children: the literal 'backpath' tag (emits nothing)
/// plus one orange glyph leaf per partner role, generated from
/// PARTNER_ROLE_VOCAB so the role<->glyph table is the single source of
/// truth. Matches '(birth backpath ROLENAME)'.
fn birth_rule_children () -> Vec<RuleChild> {
  let mut children : Vec<RuleChild> = vec! [ vac ("backpath") ];
  for (rolename, _role, glyph) in PARTNER_ROLE_VOCAB {
    children . push ( leaf (Orange, rolename, glyph) ); }
  children }

//
// The table
//

/// The complete herald rule table, as a single root rule labeled
/// "skg". Rule ORDER is presentation order, independent of the raw
/// metadata order.
///
/// A few patterns (full engine semantics in skg-lens.el):
///
///   * Simple leaves -- e.g. the aliasCol rule matches the bare atom
///     'aliasCol' and emits the literal "aliases" in green. Children
///     are consumed positionally; 'any' in a child position matches
///     any leaf/atom.
///
///   * INTERC rules -- used when the server emits a parent whose
///     children should be glued together with a separator, preserving
///     each child's own color. Labelled INTERCs match a child of the
///     object bearing that label (e.g. the staged/unstaged forms);
///     unlabelled INTERCs run their sub-rules against the current
///     object's own children (used at the graphStats level to build
///     compact tokens like "N→M" and "N{M" from sibling atoms).
///
///   * 'leaf_abut' -- the emitted token glues onto the preceding
///     token with no space (used so the indefinitive marker "☮" sits
///     directly on its parentIs glyph).
///
/// WHY SOME RULES LOOK EMPTY OR REDUNDANT:
///
///   * 'vac' rules (focused, folded, node's source, deleted's id...)
///     match and emit nothing. The engine would ignore the atoms
///     anyway; the vacuous rules document that the atom is known, and
///     let the conformance test demand that every emittable atom
///     appear here.
///
///   * deletedScaffold vs deleted -- two shapes come in from the
///     server depending on whether the deletion is on a scaffold row
///     (like a deleted aliasCol) or on a file-level node. Each gets
///     its own matcher; both render as "DELETED ...".
///
///   * Two scaffold-level staged/unstaged INTERC rules and two
///     node-level ones -- the scaffold-level pair omits the X / -X
///     axes because existence-change markers only apply to ActiveNodes,
///     not to scaffolds.
///
///   * The 'parentIs' sub-rule 'container' names an atom the server
///     NEVER emits: the server leaves parentIs=affected implicit, and
///     Emacs ('heralds--inject-default-parentIs') inserts the
///     internal-only 'container' atom so omitted ordinary content
///     keeps its "{" herald while explicit '(parentIs affected)'
///     stays quiet. 'affected' itself is accepted on parse but never
///     emitted. Both are in ACCEPTED_NOT_EMITTED_ATOMS in the
///     conformance test.
pub fn herald_rule_table () -> HeraldRule {
  HeraldRule {
    color : None, interc : None, label : Some ("skg"), abut : false,
    children : vec! [
      vac ("focused"),
      vac ("folded"),
      vac ("bodyFolded"),
      leaf (Green, QualCol::Alias . repr_in_client (), "aliases"),
      leaf (Green, "alias", "alias"), // Qual::Alias
      leaf (Green, PartnerCol::HiddenInSubscribee . repr_in_client (),
            "hiddenIn"),
      leaf (Green, PartnerCol::HiddenOutsideOfSubscribee . repr_in_client (),
            "hiddenOut"),
      leaf (Green, PartnerCol::Subscribee . repr_in_client (),
            "it subscribes to these"),
      leaf (Green, PartnerCol::Subscriber . repr_in_client (),
            "these subscribe to it"),
      leaf (Green, PartnerCol::Hidden . repr_in_client (),
            "it hides these from its subscriptions"),
      leaf (Green, PartnerCol::Hider . repr_in_client (),
            "these hide it from their subscriptions"),
      leaf (Green, PartnerCol::Overridden . repr_in_client (),
            "it overrides the view of these"),
      leaf (Green, PartnerCol::Overrider . repr_in_client (),
            "these override the view of it"),
      leaf (Green, QualCol::ID . repr_in_client (), "IDs"),
      leaf (Green, "id", "ID"), // Qual::ID
      crule (Green, "textChanged", vec! [
        s ("text changed : "),
        leaf (Red, "staged",   "staged"),
        leaf (Red, "unstaged", "unstaged") ]),
      crule (Red, "deletedScaffold", vec! [
        any ( vec! [ s ("DELETED"), RuleChild::It ] ) ]),
      crule (Red, "deleted", vec! [
        s ("DELETED"),
        vac ("id"),
        vac ("source") ]),
      crule (Orange, "unknown", vec! [
        s ("Parent references unknown node."),
        vac ("id") ]),
      // An inactive placeholder is anonymous and dataless: the bare
      // atom 'inactiveNode' (see InactiveNode), like the other dataless
      // scaffold markers. Its id/source would leak hidden content, so
      // they are not emitted.
      crule (Blue, "inactiveNode", vec! [
        s ("node from inactive source") ]),
      interc (Some (Green), "", Some ("staged"), vec! [
        s ("staged:"),
        leaf (Green, "newM",     "M"),
        leaf (Red,   "removedM", "-M") ]),
      interc (Some (Green), "", Some ("unstaged"), vec! [
        s ("unstaged:"),
        leaf (Green, "newM",     "M"),
        leaf (Red,   "removedM", "-M") ]),
      rule ("node", vec! [
        vac ("id"),
        vac ("source"),
        rule ("parentIs", vec! [
          leaf (Blue, "container", "{"),
          vac ("absent"),
          vac ("affected"),
          leaf (Orange, "independent", "⊥") ]),
        rule ("birth", birth_rule_children ()),
        // The server emits the abbreviated atom 'indef'
        // (see org_to_text.rs); we match that here.
        leaf_abut (Green, "indef", "☮"),
        rule ("graphStats", vec! [
          interc (Some (Yellow), "→", None, vec! [
            rule ("linksInFromContainers", vec! [ any (vec! [RuleChild::It]) ]),
            rule ("linksInFromLeaves",     vec! [ any (vec! [RuleChild::It]) ]) ]),
          interc (Some (Blue), "{", None, vec! [
            crule (Yellow, "containers", vec! [ any (vec! [RuleChild::It]) ]),
            crule (Blue,   "contents",   vec! [ any (vec! [RuleChild::It]) ]) ]),
          leaf (Blue, "aliasing",    "A"),
          leaf (Blue, "extraIDs",    "I"),
          leaf (Blue, "overriding",  "O"),
          leaf (Blue, "subscribing", "S"),
          leaf (Blue, "hiding",      "H") ]),
        crule (Blue, "viewStats", vec! [
          leaf (Blue, "cycle", "⟳"),
          rule ("containsParent", vec! [ s ("}") ]),
          leaf (Red, "grandparentOverrides",  "gO"),
          leaf (Red, "grandparentSubscribes", "gS"),
          leaf (Red, "overridesParent",       "Op"),
          leaf (Red, "parentOverrides",       "pO"),
          crule (Red, "overridesHere", vec! [
            // ANY without IT: the marker's ID payload is matched
            // but not echoed; only "Oh" is displayed.
            any (vec! [ s ("Oh") ]) ]),
          crule (Green, "sourceHerald", vec! [ any (vec! [RuleChild::It]) ]) ]),
        rule ("editRequest", vec! [
          leaf (Red, "delete", "delete"),
          crule (Red, "merge", vec! [
            any ( vec! [ s ("merge:"), RuleChild::It ] ) ]) ]),
        crule (Green, "viewRequests", vec! [
          rule ("col",  vec! [ any (vec! [ s ("req:col:"),  RuleChild::It ]) ]),
          rule ("path", vec! [ any (vec! [ s ("req:path:"), RuleChild::It ]) ]),
          rule ("definitiveView", vec! [ s ("req:definitive") ]) ]),
        interc (Some (Green), "", Some ("staged"), vec! [
          s ("staged:"),
          leaf (Green, "newX",     "X"),
          leaf (Red,   "removedX", "-X"),
          leaf (Green, "newM",     "M"),
          leaf (Red,   "removedM", "-M") ]),
        interc (Some (Green), "", Some ("unstaged"), vec! [
          s ("unstaged:"),
          leaf (Green, "newX",     "X"),
          leaf (Red,   "removedX", "-X"),
          leaf (Green, "newM",     "M"),
          leaf (Red,   "removedM", "-M") ]),
        leaf (Red, "notInGit", "diff:not-in-git") ]),
      // A PhantomDiff (a moved/removed node in git-diff mode) emits its
      // own root atom 'diffPhantom', not 'node'. Its grammar is the
      // strict subset of node's that phantomDiff_metadata_to_string can
      // produce: id, source, indef, graphStats, the staged/unstaged diff
      // axes, and notInGit -- never parentIs/birth/viewStats/editRequest/
      // viewRequests.
      rule ("diffPhantom", vec! [
        vac ("id"),
        vac ("source"),
        leaf_abut (Green, "indef", "☮"),
        rule ("graphStats", vec! [
          interc (Some (Yellow), "→", None, vec! [
            rule ("linksInFromContainers", vec! [ any (vec! [RuleChild::It]) ]),
            rule ("linksInFromLeaves",     vec! [ any (vec! [RuleChild::It]) ]) ]),
          interc (Some (Blue), "{", None, vec! [
            crule (Yellow, "containers", vec! [ any (vec! [RuleChild::It]) ]),
            crule (Blue,   "contents",   vec! [ any (vec! [RuleChild::It]) ]) ]),
          leaf (Blue, "aliasing",    "A"),
          leaf (Blue, "extraIDs",    "I"),
          leaf (Blue, "overriding",  "O"),
          leaf (Blue, "subscribing", "S"),
          leaf (Blue, "hiding",      "H") ]),
        interc (Some (Green), "", Some ("staged"), vec! [
          s ("staged:"),
          leaf (Green, "newX",     "X"),
          leaf (Red,   "removedX", "-X"),
          leaf (Green, "newM",     "M"),
          leaf (Red,   "removedM", "-M") ]),
        interc (Some (Green), "", Some ("unstaged"), vec! [
          s ("unstaged:"),
          leaf (Green, "newX",     "X"),
          leaf (Red,   "removedX", "-X"),
          leaf (Green, "newM",     "M"),
          leaf (Red,   "removedM", "-M") ]),
        leaf (Red, "notInGit", "diff:not-in-git") ]) ],
  }}

//
// Serialization
//

/// Serialize the whole table to the sexp the lens engine interprets.
/// Strings are ALWAYS quoted (unlike the 'sexp' crate, which leaves
/// space-free strings bare): the engine distinguishes strings from
/// symbols -- e.g. an INTERC's separator may be the empty string, and
/// a bare prefix string would be misread as the INTERC's label.
pub fn herald_rules_sexp () -> String {
  serialize_rule ( &herald_rule_table () ) }

fn serialize_rule (
  rule : &HeraldRule,
) -> String {
  let mut parts : Vec<String> = Vec::new ();
  if let Some (color) = rule . color {
    parts . push ( color . repr_in_client () . to_string () ); }
  if let Some (sep) = rule . interc {
    parts . push ( "INTERC" . to_string () );
    parts . push ( quote_string (sep) ); }
  if let Some (label) = rule . label {
    parts . push ( label . to_string () ); }
  if rule . abut {
    parts . push ( "ABUT" . to_string () ); }
  for child in & rule . children {
    parts . push ( match child {
      RuleChild::Str (text) => quote_string (text),
      RuleChild::It         => "IT" . to_string (),
      RuleChild::Rule (r)   => serialize_rule (r), } ); }
  format! ( "({})", parts . join (" ")) }

fn quote_string (
  text : &str,
) -> String {
  format! ( "\"{}\"",
            text . replace ('\\', "\\\\") . replace ('"', "\\\"") ) }

//
// Vocabulary enumeration, for the conformance test
//

/// Every match atom in the rule table (labels of rules, recursively),
/// excluding the engine directives (ANY; IT is not a label) and the
/// root label "skg".
pub fn atoms_in_rule_table () -> std::collections::HashSet<&'static str> {
  fn collect (
    rule : &HeraldRule,
    out  : &mut std::collections::HashSet<&'static str>,
  ) {
    if let Some (label) = rule . label {
      if label != "ANY" && label != "skg" {
        out . insert (label); }}
    for child in & rule . children {
      if let RuleChild::Rule (r) = child {
        collect (r, out); }}}
  let mut out : std::collections::HashSet<&'static str> =
    std::collections::HashSet::new ();
  collect ( &herald_rule_table (), &mut out );
  out }

/// Every metadata atom the server can emit in a MATCH (label)
/// position. Value-position data (counts, IDs, source names,
/// 'deadScaffold', the sourceHerald payload) is consumed by ANY/IT
/// rules and so is deliberately absent.
///
/// Each component is derived from the type that owns it; the
/// exhaustiveness guards below make adding an enum variant or struct
/// field a compile error here until this list learns the new atom.
pub fn emittable_metadata_atoms () -> std::collections::HashSet<&'static str> {
  let mut atoms : Vec<&'static str> = vec! [
    // Bare buffer-position atoms, from org_to_text.rs:
    "focused", "folded", "bodyFolded",
    // Form heads, from org_to_text.rs:
    "node", "diffPhantom", "deleted", "unknown", "inactiveNode",
    "deletedScaffold",
    // Keys inside node / diffPhantom / deleted / unknown forms:
    "id", "source",
    "parentIs", "birth", "indef", "notInGit",
    "graphStats", "viewStats", "editRequest", "viewRequests",
    "staged", "unstaged",
    // EditRequest atoms:
    "delete", "merge",
  ];
  atoms . extend ( graphstats_atoms () );
  atoms . extend ( viewstats_atoms () );
  atoms . extend ( parentIs_emitted_atoms () );
  atoms . extend ( birth_emitted_atoms () );
  atoms . extend ( axis_atoms () );
  atoms . extend ( qual_and_col_atoms () );
  atoms . extend ( ViewRequest::EMITTABLE_MATCH_ATOMS );
  atoms . into_iter () . collect () }

/// GraphNodeStats atoms, from graphnodestats_to_sexp (org_to_text.rs).
/// The destructuring pattern is the exhaustiveness guard: a new field
/// on GraphNodeStats fails to compile here until this list (and the
/// rule table, via the conformance test) learns its atom.
fn graphstats_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::GraphNodeStats;
  fn guard ( g : GraphNodeStats ) {
    let GraphNodeStats {
      aliasing : _, extraIDs : _, overriding : _, subscribing : _,
      hiding : _,
      containRels : _,    // -> the containers / contents atoms
      linksourceRels : _, // -> the linksInFrom* atoms
    } = g; }
  let _ = guard;
  vec! [ "aliasing", "extraIDs", "overriding", "subscribing", "hiding",
         "containers", "contents",
         "linksInFromContainers", "linksInFromLeaves" ] }

/// ViewNodeStats atoms, from activeNode_metadata_to_string's
/// view_stats (org_to_text.rs). Guard as in graphstats_atoms.
fn viewstats_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::ViewNodeStats;
  fn guard ( v : ViewNodeStats ) {
    let ViewNodeStats {
      cycle : _,
      parentIsContainer : _, // never serialized (the default state makes no noise)
      parentIsContent : _,   // -> the containsParent atom
      sourceAtBoundary : _,  // -> the sourceHerald atom
      grandparentOverrides : _,
      grandparentSubscribes : _,
      overridesParent : _,
      parentOverrides : _,
      overridesHere : _, // keyed form (a viewStats sub-form)
    } = v; }
  let _ = guard;
  vec! [ "cycle", "containsParent", "sourceHerald",
         "grandparentOverrides", "grandparentSubscribes",
         "overridesParent", "parentOverrides", "overridesHere" ] }

/// ParentIs values the serializer can emit (Affected stays implicit).
fn parentIs_emitted_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::ParentIs;
  fn guard ( p : ParentIs ) { // compile error here = update the list below
    match p {
      ParentIs::Affected | ParentIs::Independent | ParentIs::Absent
        => () }}
  let _ = guard;
  vec! [ "independent", "absent" ] }

/// Birth values the serializer can emit (Unremarkable stays implicit).
/// A Backpath emits '(birth backpath ROLENAME)', so the atoms are the
/// 'backpath' tag plus every ROLENAME in PARTNER_ROLE_VOCAB.
fn birth_emitted_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::Birth;
  fn guard ( b : Birth ) { // compile error here = update the list below
    match b {
      Birth::Unremarkable | Birth::Backpath (_)
        => () }}
  let _ = guard;
  let mut atoms : Vec<&'static str> = vec! [ "backpath" ];
  atoms . extend ( PARTNER_ROLE_VOCAB . iter () . map ( |(name, _, _)| *name ) );
  atoms }

/// The staged/unstaged axis atoms, from types/git.rs.
fn axis_atoms () -> Vec<&'static str> {
  use crate::types::git::Sign;
  fn guard ( s : Sign ) { // compile error here = update the list below
    match s { Sign::Plus | Sign::Minus => () }}
  let _ = guard;
  vec! [ "newX", "removedX", "newM", "removedM" ] }

/// Col and Qual atoms, via the same repr_in_client constants the
/// serializer uses.
fn qual_and_col_atoms () -> Vec<&'static str> {
  fn partnerCol_guard ( c : PartnerCol ) { // compile error here = update all_partnerCols
    match c {
      PartnerCol::Subscribee | PartnerCol::Subscriber
      | PartnerCol::Overridden | PartnerCol::Overrider
      | PartnerCol::Hider | PartnerCol::Hidden
      | PartnerCol::HiddenInSubscribee
      | PartnerCol::HiddenOutsideOfSubscribee => () }}
  let _ = partnerCol_guard;
  let all_partnerCols : [PartnerCol; 8] = [
    PartnerCol::Subscribee, PartnerCol::Subscriber,
    PartnerCol::Overridden, PartnerCol::Overrider,
    PartnerCol::Hider, PartnerCol::Hidden,
    PartnerCol::HiddenInSubscribee,
    PartnerCol::HiddenOutsideOfSubscribee ];
  fn qualCol_guard ( c : QualCol ) { // ditto
    match c { QualCol::ID | QualCol::Alias => () }}
  let _ = qualCol_guard;
  let all_qualCols : [QualCol; 2] = [ QualCol::ID, QualCol::Alias ];
  let all_qual_atoms : [&'static str; 3] = {
    fn qual_guard ( q : &Qual ) { // ditto
      match q {
        Qual::Alias { .. } | Qual::ID { .. } | Qual::TextChanged { .. }
          => () }}
    let _ = qual_guard;
    [ "alias", "id", "textChanged" ] };
  let mut out : Vec<&'static str> = Vec::new ();
  out . extend ( all_partnerCols . iter ()
                 . map ( |c| c . repr_in_client () ) );
  out . extend ( all_qualCols . iter ()
                 . map ( |c| c . repr_in_client () ) );
  out . extend ( all_qual_atoms );
  out }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../tests/unit/heralds.rs"]
mod tests;
