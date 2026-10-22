/// PURPOSE: The herald rule table for non-relationship '(skg ...)'
/// metadata. Emacs and Neovim render semantic relationship facts and
/// their per-character styles themselves.
///
/// The DATA lives here; the code that USES the data (the lens
/// engine, 'elisp/skg-sexpr/skg-lens.el') lives entirely in Emacs.
/// Emacs fetches the table over the "herald rules" endpoint at
/// connect time and hands it, unchanged, to that engine. Rust never
/// interprets labels, and takes each rule's style by name from
/// 'shared/herald-styles.json'; the conformance test in
/// 'tests/unit/heralds.rs' guarantees that the table and the metadata
/// vocabulary the server emits stay in sync.
///
/// GRAMMAR (mirrored one-to-one by 'HeraldRule'; full display
/// semantics are documented in skg-lens.el):
///   RULE        ::= [STYLE] (INTERC-BODY | SIMPLE-BODY)
///   INTERC-BODY ::= INTERC SEP [LABEL] CHILDREN...
///   SIMPLE-BODY ::= LABEL [ABUT] CHILDREN...
/// where each child is a literal string, the IT directive, or a
/// nested RULE. The special label ANY matches any leaf; IT echoes
/// the matched value(s).

use crate::types::nodes::complete::Flag;
use crate::types::viewnode::{
  ChangesetMarkerKind, DiffPresence, PartnerFolder, Property, PropertyFolder,
  DiffObject, ViewRequest};

use serde_json::Value;
use std::collections::HashSet;
use std::sync::OnceLock;

/// 'shared/herald-styles.json', embedded at compile time. Its
/// 'node_heralds' table gives the style of each styled atom in the rule
/// table below; the clients define each style's look from its 'styles'.
/// See docs/heralds.org.
const HERALD_STYLES_JSON : &str =
  include_str! ("../shared/herald-styles.json");

static HERALD_STYLES : OnceLock<Value> = OnceLock::new ();

/// The parsed 'shared/herald-styles.json'.
pub fn herald_styles () -> &'static Value {
  HERALD_STYLES . get_or_init ( || {
    serde_json::from_str (HERALD_STYLES_JSON)
      . expect ("shared/herald-styles.json should parse as JSON") } ) }

/// The style named by KEY in 'node_heralds', if any.
fn node_herald_style (
  key : &str,
) -> Option<&'static str> {
  herald_styles () ["node_heralds"] [key] . as_str () }

/// The style directive in the served rule table: the style's name in
/// capitals, like the table's other directives (ANY, IT, ABUT, INTERC).
pub fn style_repr_in_client (
  style_name : &str,
) -> String {
  style_name . to_uppercase () }

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
  pub node_heralds_key : Option<&'static str>, // where 'node_heralds' gives the style
  pub interc   : Option<&'static str>, // Some(separator) makes this an INTERC rule
  pub label    : Option<&'static str>, // None only for unlabeled INTERC rules
  pub abut     : bool,                 // glue output to the preceding token
  pub children : Vec<RuleChild>,
}

impl HeraldRule {
  /// The rule's style name, from 'node_heralds'. Startup asserts that
  /// every key the table uses is present ('herald_styles_inconsistencies').
  pub fn style (&self) -> Option<&'static str> {
    self . node_heralds_key . map ( |key| {
      node_herald_style (key)
        . unwrap_or_else ( || panic! (
          "shared/herald-styles.json: node_heralds has no entry {}",
          key )) } ) }
}

//
// Constructor sugar, so the table below stays close to the sexp it serializes to.
//

/// (label children...)
fn rule (
  label    : &'static str,
  children : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : None, interc : None, label : Some (label),
    abut : false, children } ) }

/// (STYLE label children...), with STYLE from 'node_heralds' under the label
fn crule (
  label    : &'static str,
  children : Vec<RuleChild>,
) -> RuleChild {
  crule_styled_as ( label, label, children ) }

/// (STYLE label children...), with STYLE from 'node_heralds' under
/// NODE_HERALDS_KEY, for an atom styled differently in different places
fn crule_styled_as (
  node_heralds_key : &'static str,
  label            : &'static str,
  children         : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (node_heralds_key),
    interc : None, label : Some (label),
    abut : false, children } ) }

/// (STYLE label "text") -- a one-token leaf
fn leaf (
  label : &'static str,
  text  : &'static str,
) -> RuleChild {
  crule ( label, vec! [ s (text) ] ) }

/// (STYLE label "text"), with STYLE from 'node_heralds' under NODE_HERALDS_KEY
fn leaf_styled_as (
  node_heralds_key : &'static str,
  label            : &'static str,
  text             : &'static str,
) -> RuleChild {
  crule_styled_as ( node_heralds_key, label, vec! [ s (text) ] ) }

/// The two rules for a write-protected label: one emits ☮, styled
/// like 'writeProtected', meaning "this cannot be changed from here"
/// -- the sense ☮ carries on a node -- and the other the label's
/// text, styled under the label. Both match the same atom; the
/// renderer's ordinary space separates their tokens.
fn write_protected_leaves (
  label : &'static str,
  text  : &'static str,
) -> Vec<RuleChild> {
  vec! [ leaf_styled_as ("writeProtected", label, "☮"),
         leaf (label, text) ] }

/// (label) -- matches and emits nothing; consumes the atom so the
/// table documents it. (The engine ignores unmatched atoms anyway;
/// vacuous rules exist so the conformance test can see them.)
fn vac (
  label : &'static str,
) -> RuleChild {
  rule ( label, vec! [] ) }

/// (STYLE label ABUT "text")
fn leaf_abut (
  label : &'static str,
  text  : &'static str,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (label), interc : None, label : Some (label),
    abut : true, children : vec! [ s (text) ] } ) }

/// (STYLE label ABUT "text"), with STYLE from 'node_heralds' under
/// NODE_HERALDS_KEY
fn leaf_abut_styled_as (
  node_heralds_key : &'static str,
  label            : &'static str,
  text             : &'static str,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (node_heralds_key), interc : None,
    label : Some (label), abut : true, children : vec! [ s (text) ] } ) }

/// (STYLE ANY ABUT "text"), with STYLE from 'node_heralds' under
/// NODE_HERALDS_KEY
fn any_abut_styled_as (
  node_heralds_key : &'static str,
  text             : &'static str,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (node_heralds_key), interc : None,
    label : Some ("ANY"), abut : true, children : vec! [ s (text) ] } ) }

/// (STYLE ANY children...), with STYLE from 'node_heralds' under
/// NODE_HERALDS_KEY
fn any_styled_as (
  node_heralds_key : &'static str,
  children         : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (node_heralds_key), interc : None,
    label : Some ("ANY"), abut : false, children } ) }

/// The diff view's properties, each a label. A ready one's ✓
/// comes from the property-leaf rules after them in the table.
/// "⌂ -OLD +NEW" and "relationship moved from OLD to NEW" show the
/// skgrepo names in their own style.
fn diffView_property_rules (
) -> Vec<RuleChild> {
  vec! [
    crule ("oldText", vec! [
      rule ("titleAndBody", vec! [ s ("old title and body") ]),
      rule ("title",        vec! [ s ("old title") ]),
      rule ("body",         vec! [ s ("old body") ]) ]),
    leaf ("bodyDiff", "body diff"),
    crule ("homeSkgrepoMove", vec! [
      any (vec! [ s ("⌂") ]),
      // The sign is a prefix: an ANY rule would join it to the name
      // with a colon.
      crule_styled_as ("homeSkgrepoMove_skgrepo", "old", vec! [
        s ("-"), any (vec! [ RuleChild::It ]) ]),
      crule_styled_as ("homeSkgrepoMove_skgrepo", "new", vec! [
        s ("+"), any (vec! [ RuleChild::It ]) ]) ]),
    crule ("relRepoMove", vec! [
      any (vec! [ s ("relationship moved from") ]),
      rule ("old", vec! [ any_styled_as ("relRepoMove_skgrepo",
                                         vec! [ RuleChild::It ]) ]),
      rule ("new", vec! [ any (vec! [ s ("to") ]) ]),
      rule ("new", vec! [ any_styled_as ("relRepoMove_skgrepo",
                                         vec! [ RuleChild::It ]) ]) ]),
    crule ("changesetMarker", ChangesetMarkerKind::ALL . into_iter ()
      . map ( |kind| rule ( kind . wire_name (), vec! [ s (
        match kind {
          ChangesetMarkerKind::Content   => "its content changed",
          ChangesetMarkerKind::RelRepo   => "some content relationships changed skgrepo",
          ChangesetMarkerKind::OrderOnly => "order-only change", } ) ] ))
      . collect () ) ] }

/// The label of a folder the diff view drew because of a
/// change, '(KIND changed)', which takes the place of the folder's
/// ordinary label: "its X changed (this file)" for a folder of the
/// node's own lists, "(other files)" for one of its recorders. A
/// write-protected folder's label starts with ☮, as its ordinary one
/// does.
fn changed_folder_rules (
) -> Vec<RuleChild> {
  let changed = |atom : &'static str, write_protected : bool,
                 text : &'static str| -> RuleChild {
    rule ( atom, [
      if write_protected {
        vec! [ leaf_styled_as ("writeProtected", "changed", "☮") ]
      } else { vec! [] },
      vec! [ leaf ("changed", text) ] ] . concat () ) };
  vec! [
    changed (PropertyFolder::Alias . repr_in_client (), false,
             "its aliases changed (this file)"),
    changed (PropertyFolder::ID . repr_in_client (), false,
             "its IDs changed (this file)"),
    changed (PropertyFolder::flags () . repr_in_client (), true,
             "its flags changed (this file)"),
    changed (PartnerFolder::Container . repr_in_client (), true,
             "its containers changed (other files)"),
    changed (PartnerFolder::Mentioner . repr_in_client (), true,
             "its mentioners changed (other files)"),
    changed (PartnerFolder::Subscriber . repr_in_client (), true,
             "its subscribers changed (other files)"),
    changed (PartnerFolder::Overrider . repr_in_client (), true,
             "its overriders changed (other files)"),
    changed (PartnerFolder::Hider . repr_in_client (), true,
             "its hiders changed (other files)"),
    changed (PartnerFolder::Subscribee . repr_in_client (), false,
             "its subscribees changed (this file)"),
    changed (PartnerFolder::Overridden . repr_in_client (), false,
             "the nodes it overrides changed (this file)"),
    changed (PartnerFolder::Hidden . repr_in_client (), true,
             "the nodes it hides changed (this file)") ] }

/// The diff view's '(diff ...)' form on a vognode: its '(presence
/// ...)' as ∃+ ∃- M (the node itself: new, deleted or modified) and
/// ~+ ~- (its relationship to its viewparent), each followed by a ✓
/// when '(ready ...)' names that diff object ('all' for M), then the
/// readiness request. '(leaves ...)' is
/// for the clients' gestures and shows nothing. The same part is
/// matched once per object, so each ✓ lands after its own object
/// whatever the raw order.
fn change_fact_rules_for_two_objects (
) -> Vec<RuleChild> {
  vec! [
    rule ("diff", vec! [
      rule ("presence", vec! [
        leaf_styled_as ("presence", "node+", "∃+"),
        leaf_styled_as ("presence", "node-", "∃-"),
        leaf_styled_as ("presence", "node~", "M") ]),
      rule ("ready", vec! [ leaf_abut_styled_as ("ready", "node", "✓"),
                            leaf_abut_styled_as ("ready", "all",  "✓") ]),
      rule ("presence", vec! [
        leaf_styled_as ("presence", "rel+", "~+"),
        leaf_styled_as ("presence", "rel-", "~-") ]),
      rule ("ready", vec! [ leaf_abut_styled_as ("ready", "rel", "✓") ]),
      vac ("leaves"),
      readiness_request_rule ("readyRequest",   "request:ready"),
      readiness_request_rule ("unreadyRequest", "request:unready") ]) ] }

/// The same for a leaf with a single diff object (a property leaf, a deleted
/// or unknown phantom), whose '(ready)' names no object. 'plus' and
/// 'minus' render its presence atoms, whichever object they name.
fn change_fact_rules_for_one_object (
  plus  : &'static str,
  minus : &'static str,
) -> Vec<RuleChild> {
  vec! [
    rule ("diff", vec! [
      rule ("presence", vec! [
        leaf_styled_as ("presence", "node+", plus),
        leaf_styled_as ("presence", "node-", minus),
        leaf_styled_as ("presence", "rel+",  plus),
        leaf_styled_as ("presence", "rel-",  minus) ]),
      rule ("ready", vec! [ any_abut_styled_as ("ready", "✓") ]),
      vac ("leaves"),
      readiness_request_rule ("readyRequest",   "request:ready"),
      readiness_request_rule ("unreadyRequest", "request:unready") ]) ] }

/// "request:ready", then ":OBJECT" for each object the request names.
fn readiness_request_rule (
  label  : &'static str,
  prefix : &'static str,
) -> RuleChild {
  crule ( label,
          [ vec! [ s (prefix) ],
            DiffObject::ALL . into_iter ()
            . map ( |object| rule ( object . wire_name (), vec! [ s (
              match object {
                DiffObject::Node => ":node",
                DiffObject::Rel  => ":rel",
                DiffObject::All  => ":all", } ) ] ))
            . collect () ] . concat () ) }

/// (ANY children...)
fn any (
  children : Vec<RuleChild>,
) -> RuleChild {
  rule ( "ANY", children ) }

fn s ( text : &'static str ) -> RuleChild { RuleChild::Str (text) }

/// The placeholder token the `rels` rule emits. The relationship
/// heralds are per-character styled spans the lens engine cannot style,
/// so the rule only POSITIONS them: it emits this sentinel where the
/// relationship heralds belong, and the client
/// (`heralds-from-metadata`) replaces the sentinel token with the
/// spans it renders itself from the `(rels ...)` payload. Kept in sync
/// with `heralds--rels-sentinel` in elisp/heralds-minor-mode.el.
pub const RELS_SPANS_SENTINEL : &str = "__RELS_SPANS__";

//
// The table
//

/// The complete herald rule table, as a single root rule labeled
/// "skg". Rule ORDER is presentation order, independent of the raw
/// metadata order.
///
/// A few patterns (full engine semantics in skg-lens.el):
///
///   * Simple leaves -- e.g. the aliasFolder rule matches the bare atom
///     'aliasFolder' and emits the literal "aliases" as a message. Children
///     are consumed positionally; 'any' in a child position matches
///     any leaf/atom.
///
///   * 'leaf_abut' -- the emitted token glues onto the preceding
///     token with no space (used so the write-protected glyph "☮" sits
///     directly on its affectsParent glyph).
///
/// WHY SOME RULES LOOK EMPTY OR REDUNDANT:
///
///   * 'vac' rules (focused, folded, node's repo, deleted's id...)
///     match and emit nothing. The engine would ignore the atoms
///     anyway; the vacuous rules document that the atom is known, and
///     let the conformance test demand that every emittable atom
///     appear here.
///
///   * deadViewnode vs deleted -- two shapes come in from the
///     server depending on whether the deletion is on a non-vognode viewnode
///     (like a deleted aliasFolder) or on a file-level node. Each gets
///     its own matcher; both render as "DELETED".
///
///   * The 'affectsParent' sub-rule 'true' is vacuous: the server leaves
///     affectsParent=true implicit, and an explicit '(affectsParent
///     true)', which the parser accepts, stays quiet.
pub fn herald_rule_table () -> HeraldRule {
  HeraldRule {
    node_heralds_key : None, interc : None, label : Some ("skg"),
    abut : false,
    children : [ vec! [
      vac ("focused"),
      vac ("folded"),
      vac ("bodyFolded"),
      leaf (PropertyFolder::Alias . repr_in_client (), "aliases"),
      leaf ("alias", "alias"), // Property::Alias
      leaf ("removedAlias", "alias"), // Property::RemovedAlias
      // An alias's stored relRepo is a display fact.  A
      // requested replacement lives under editRequest below, so the
      // two values can be rendered side by side without conflation.
      crule ("relRepo", vec! [ any (vec! [ s ("~"), RuleChild::It ]) ]),
      rule ("editRequest", vec! [
        crule_styled_as ("editRequest_relRepo", "relRepo", vec! [
          any (vec! [ s ("request:~"), RuleChild::It ]) ]) ]) ],
      // The WRITE-PROTECTED folders carry ☮ ("cannot be changed
      // from here"); the editable folders (subscribeeFolder, overriddenFolder,
      // aliasFolder) do not.
      write_protected_leaves (PartnerFolder::HiddenInSubscribee . repr_in_client (),
            "It contains these, but the subscribing ancestor hides them."),
      vec! [
      leaf (PartnerFolder::HiddenOutsideOfSubscribee . repr_in_client (),
            "The subscriber ancestor hides these, but subscribes to nothing that contains them."),
      leaf (PartnerFolder::Subscribee . repr_in_client (),
            "It subscribes to these.") ],
      write_protected_leaves (PartnerFolder::Subscriber . repr_in_client (),
            "These subscribe to it."),
      write_protected_leaves (PartnerFolder::Hidden . repr_in_client (),
            "It hides these from its subscriptions."),
      write_protected_leaves (PartnerFolder::Hider . repr_in_client (),
            "These hide it from their subscriptions."),
      vec! [
      leaf (PartnerFolder::Overridden . repr_in_client (),
            "It overrides the view of these.") ],
      write_protected_leaves (PartnerFolder::Overrider . repr_in_client (),
            "These override the view of it."),
      write_protected_leaves (PartnerFolder::Container . repr_in_client (),
            "These contain it."),
      write_protected_leaves (PartnerFolder::Mentioner . repr_in_client (),
            "These link to it."),
      write_protected_leaves (PartnerFolder::AcquiredBy . repr_in_client (),
            "it was acquired by"),
      write_protected_leaves (PartnerFolder::Acquired . repr_in_client (),
            "it acquired"),
      vec! [
      leaf (PropertyFolder::ID . repr_in_client (), "IDs"),
      leaf ("id", "ID"), // Property::ID
      leaf ("removedID", "ID") ], // Property::RemovedID
      write_protected_leaves (PropertyFolder::flags () . repr_in_client (),
                "flags"),
      vec! [
      // A flag row is write-protected too: one rule emits its ☮, the
      // other its text.
      crule_styled_as ("writeProtected", "flag", vec! [ any (vec! [ s ("☮") ]) ]),
      crule ("flag", Flag::ALL . into_iter ()
        . map (|flag| rule (
          flag . wire_name (), vec! [s (flag . herald_text ())]))
        . collect ()) ],
      diffView_property_rules (),
      changed_folder_rules (),
      // The diff view's facts on a property leaf: + and - for
      // its presence, ✓ when ready.
      change_fact_rules_for_one_object ("+", "-"),
      vec! [
      // A changed folder's bare '(ready)', outside any '(diff ...)'.
      rule ("ready", vec! [ any_abut_styled_as ("ready", "✓") ]),
      crule ("deadViewnode", vec! [ s ("DELETED") ]),
      crule ("deleted", vec! [
        s ("DELETED"),
        vac ("id"),
        vac ("repo") ]),
      // A second rule over the same form, so its tokens do not take
      // the prefix "DELETED".
      rule ("deleted", change_fact_rules_for_one_object ("∃+", "∃-")),
      crule ("unknown", vec! [
        s ("Reference to unknown node."),
        vac ("id"),
        rule ("viewStats", vec! [
          crule ("relRepo", vec! [
            any (vec! [ s ("~"), RuleChild::It ]) ]) ]),
        rule ("editRequest", vec! [
          crule_styled_as ("editRequest_relRepo", "relRepo", vec! [
            any (vec! [ s ("request:~"), RuleChild::It ]) ]) ]) ]),
      rule ("unknown", change_fact_rules_for_one_object ("~+", "~-")),
      // A historical phantom is write-protected; its two objects are
      // its existence (∃-) and its relationship to its viewparent.
      crule_styled_as ("writeProtected", "historical", vec! [
        s ("☮"),
        vac ("id"),
        vac ("repo") ]),
      rule ("historical", change_fact_rules_for_two_objects ()),
      // A restricted vognode is anonymous and dataless: the bare
      // atom 'restrictedNode' (see RestrictedVognode), like the other dataless
      // non-vognode atoms. Its id/repo would leak hidden content, so
      // they are not emitted.
      crule ("restrictedNode", vec! [
        s ("node from restricted skgrepo") ]),
      rule ("node", [ vec! [
        vac ("id"),
        vac ("repo"),
        rule ("affectsParent", vec! [
          vac ("na"),
          vac ("true"),
          leaf_styled_as ("affectsParent_false", "false", "⊥") ]),
        // A role graft's birth: the (rels ...) facts already show it.
        vac ("birth"),
        // The server emits the atom 'writeProtected'
        // (see org_to_text.rs); we match that here.
        leaf_abut ("writeProtected", "☮"),
        // Emitted only on a write-protected node whose graphnode has a
        // body -- one the rendering hides. ABUT so the B rides the ☮.
        leaf_abut ("omittedBody", "B"),
        // The relationship heralds are per-CHARACTER styled spans that
        // the lens cannot style, so the server assembles semantic
        // (rels ...) facts and the CLIENT renders them. This rule only POSITIONS them: the
        // ANY child makes it match the (rels ...) list form and consumes
        // the span sub-forms, and it emits the sentinel token, which the
        // client swaps for the rendered spans.
        rule ("rels", vec! [ any (vec! [ s (RELS_SPANS_SENTINEL) ]) ]),
        rule ("viewStats", vec! [
          leaf ("cycle", "⟳"),
          // overridesHere is displayed as a crucial ĥ inside the O token
          // by the client relationship renderer. The atom's
          // ID payload is load-bearing save metadata, never displayed.
          vac ("overridesHere"),
          // relRepo is a display fact, not a save request.  It
          // echoes its own value directly:
          // "~" + the skgrepo name, ugly, immediately before the ⌂
          // homeRepoHerald below (table ORDER is presentation order,
          // per the module doc, so placing this rule first guarantees
          // that regardless of the atoms' order in the raw sexp).
          crule ("relRepo", vec! [ any (vec! [ s ("~"), RuleChild::It ]) ]),
          crule ("homeRepoHerald", vec! [ any (vec! [RuleChild::It]) ]),
          // The skgrepo-changes buffer of the diff view: the
          // node's home move, "-OLD +NEW", then "Δ⌂=a" when its
          // nearest moved vognode ancestor (here a) moved the same way.
          rule ("homeSkgrepoMove", vec! [
            crule_styled_as ("homeSkgrepoMove_skgrepo", "old", vec! [
              s ("-"), any (vec! [ RuleChild::It ]) ]),
            crule_styled_as ("homeSkgrepoMove_skgrepo", "new", vec! [
              s ("+"), any (vec! [ RuleChild::It ]) ]) ]),
          crule ("movedLikeAncestor", vec! [
            any (vec! [ s ("Δ⌂="), RuleChild::It ]) ]) ]),
        rule ("editRequest", vec! [
          leaf ("delete", "delete"),
          crule ("merge", vec! [
            any ( vec! [ s ("merge:"), RuleChild::It ] ) ]),
          crule_styled_as ("editRequest_relRepo", "relRepo", vec! [
            any (vec! [ s ("request:~"), RuleChild::It ]) ]),
          crule_styled_as ("editRequest_flag", "flag", vec! [
            // A flag request is flat metadata:
            //   (flag noSearchMatching true|false)
            // Matching the literal boolean child lets the existing rule
            // language render the desired user-facing state as one token.
            // noSearchMatching is currently the only mutable flag, and
            // the save parser rejects every other flag in this position.
            vac (Flag::NoSearchMatching . wire_name ()),
            rule ("true",  vec! [s ("request:no search matching")]),
            rule ("false", vec! [s ("request:search matching")]) ]) ]),
        crule ("viewRequests", vec! [
          rule ("folder",  vec! [ any (vec! [ s ("req:folder:"),  RuleChild::It ]) ]),
          rule ("roleTree", vec! [ any (vec! [ s ("req:roleTree:"), RuleChild::It ]) ]),
          rule ("flags", vec! [ s ("req:flags") ]),
          rule ("editableView", vec! [ s ("req:editable") ]),
          rule ("writeProtectedView", vec! [ s ("req:write-protected") ]),
          rule ("writeProtectedSubtreeView", vec! [ s ("req:write-protected-subtree") ]) ]) ],
        change_fact_rules_for_two_objects () ] . concat ()) ] ] . concat (),
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
  if let Some (style) = rule . style () {
    parts . push ( style_repr_in_client (style) ); }
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
// Consistency of shared/herald-styles.json
//

/// Every way 'shared/herald-styles.json' disagrees with itself or with
/// the rule table. Run as a unit test, and asserted at startup.
pub fn herald_styles_inconsistencies () -> Vec<String> {
  herald_styles_inconsistencies_of ( herald_styles () ) }

/// Every tier FILE names (in 'tier_order', 'node_heralds' or
/// 'relationship_heralds') that is not a key of its 'styles', and
/// every styled atom of the rule table that has no entry in its
/// 'node_heralds'.
pub fn herald_styles_inconsistencies_of (
  file : &Value,
) -> Vec<String> {
  let styles : HashSet<&str> =
    file ["styles"] . as_object ()
    . map ( |styles| styles . keys () . map (String::as_str) . collect () )
    . unwrap_or_default ();
  let undefined_tiers : Vec<String> =
    tiers_named_in_herald_styles (file) . into_iter ()
    . filter ( |(_, tier)| ! styles . contains (tier) )
    . map ( |(place, tier)| format! (
      "{} names {}, which is not a key of 'styles'", place, tier ))
    . collect ();
  let unstyled_atoms : Vec<String> =
    node_heralds_keys_in_rule_table () . into_iter ()
    . filter ( |key| file ["node_heralds"] [*key] . as_str () . is_none () )
    . map ( |key| format! (
      "the rule table styles {}, which has no entry in 'node_heralds'",
      key ))
    . collect ();
  [ undefined_tiers, unstyled_atoms ] . concat () }

/// Each tier FILE names, with the path where it does so.
fn tiers_named_in_herald_styles (
  file : &Value,
) -> Vec<(String, &str)> {
  fn collect<'a> (
    place : String,
    value : &'a Value,
    out   : &mut Vec<(String, &'a str)>,
  ) {
    match value {
      Value::String (tier) => out . push (( place, tier . as_str () )),
      Value::Array (tiers) =>
        for (i, tier) in tiers . iter () . enumerate () {
          collect ( format! ("{}[{}]", place, i), tier, out ); },
      Value::Object (entries) =>
        for (key, entry) in entries {
          if ! key . ends_with ("comment") {
            collect ( format! ("{}.{}", place, key), entry, out ); }},
      _ => () }}
  let mut out : Vec<(String, &str)> = Vec::new ();
  for field in [ "tier_order", "node_heralds", "relationship_heralds" ] {
    collect ( field . to_string (), &file [field], &mut out ); }
  out }

/// The 'node_heralds' key of every styled rule in the table.
pub fn node_heralds_keys_in_rule_table () -> HashSet<&'static str> {
  rules_in_rule_table () . into_iter ()
    . filter_map ( |rule| rule . node_heralds_key )
    . collect () }

/// The root rule and every rule nested in it.
fn rules_in_rule_table () -> Vec<HeraldRule> {
  fn collect (
    rule : &HeraldRule,
    out  : &mut Vec<HeraldRule>,
  ) {
    out . push ( rule . clone () );
    for child in & rule . children {
      if let RuleChild::Rule (r) = child {
        collect (r, out); }}}
  let mut out : Vec<HeraldRule> = Vec::new ();
  collect ( &herald_rule_table (), &mut out );
  out }

//
// Vocabulary enumeration, for the conformance test
//

/// Every match atom in the rule table (labels of rules, recursively),
/// excluding the engine directives (ANY; IT is not a label) and the
/// root label "skg".
pub fn atoms_in_rule_table () -> std::collections::HashSet<&'static str> {
  rules_in_rule_table () . into_iter ()
    . filter_map ( |rule| rule . label )
    . filter ( |label| *label != "ANY" && *label != "skg" )
    . collect () }

/// Every metadata atom the server can emit in a MATCH (label)
/// position. Value-position data (counts, IDs, skgrepo names,
/// the homeRepoHerald payload) is consumed by ANY/IT
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
    "node", "deleted", "unknown", "restrictedNode",
    "deadViewnode", "historical",
    // Keys inside node / deleted / unknown forms:
    "id", "repo",
    "affectsParent", "writeProtected", "omittedBody",
    // A role graft's '(birth roleGraft ROLENAME)'; its role is value
    // position.
    "birth",
    // The assembled relationship-herald atom, a payload of styled spans
    // (server/herald_tokens.rs); its span sub-forms are value position,
    // consumed by the client's renderer, so they are not match atoms.
    "rels",
    "viewStats", "editRequest", "viewRequests",
    // NodeEditRequest atoms. The flag form's name and desired value
    // are matched literally so its herald can describe the complete state
    // change rather than echoing two context-free arguments.
    "delete", "merge", "flag", "true", "false",
  ];
  // Flag-viewnode heralds match every public wire name.  Keeping this derived
  // from the registry makes adding a flag a conformance-checked change.
  atoms . extend (Flag::ALL . map (Flag::wire_name));
  atoms . extend ( graphstats_atoms () );
  atoms . extend ( viewstats_atoms () );
  atoms . extend ( affectsParent_emitted_atoms () );
  atoms . extend ( property_and_folder_atoms () );
  atoms . extend ( ViewRequest::EMITTABLE_MATCH_ATOMS );
  atoms . extend ( change_fact_atoms () );
  atoms . into_iter () . collect () }

/// The diff view's form, from org_to_text.rs's 'change_fact_forms':
/// its head, the heads of its parts, the presence atoms and the diff
/// objects.
fn change_fact_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::ChangeFacts;
  fn guard ( facts : ChangeFacts, object : DiffObject ) {
    let ChangeFacts {
      presence : _, // -> presence, with DiffPresence::EMITTABLE_ATOMS
      ready : _,    // -> ready, with the objects
      leaves : _,   // -> leaves, with the objects
    } = facts;
    match object { // compile error here = update DiffObject::ALL
      DiffObject::Node | DiffObject::Rel | DiffObject::All => () }}
  let _ = guard;
  [ vec! [ "diff", "presence", "ready", "leaves", "readyRequest",
           "unreadyRequest",
           "changed" ], // the head of a changed folder's form
    DiffPresence::EMITTABLE_ATOMS . to_vec (),
    DiffObject::ALL . into_iter () . map (DiffObject::wire_name)
      . collect () ] . concat () }

/// GraphnodeStats emits no match atoms: its counts feed semantic
/// '(rels ...)' facts. The
/// destructuring pattern is the exhaustiveness guard -- a new field
/// fails to compile here until it is accounted for.
fn graphstats_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::GraphnodeStats;
  fn guard ( g : GraphnodeStats ) {
    let GraphnodeStats {
      aliases : _,    // -> Ak, inside semantic rels metadata
      extra_ids : _,  // -> Ik, inside semantic rels metadata
      flags : _, // -> Fk, inside semantic rels metadata
      rels : _,       // -> relationship facts inside semantic rels metadata
    } = g; }
  let _ = guard;
  vec! [] }

/// ViewnodeStats match atoms, from unrestrictedVognode_metadata_to_string's
/// view_stats (org_to_text.rs). Birth and relationship facts are
/// node-level '(rels ...)' data, not viewStats sub-forms.
fn viewstats_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::ViewnodeStats;
  fn guard ( v : ViewnodeStats ) {
    let ViewnodeStats {
      cycle : _,
      homeSkgrepoAtBoundary : _, // -> the homeRepoHerald atom
      rel_heralds : _,      // -> the node-level rels atom (semantic sexp)
      overridesHere : _,    // keyed form (a viewStats sub-form)
      omitted_body : _,      // -> the node-level omittedBody atom
      relRepo : _,       // -> the relRepo display-fact atom and herald
      home_skgrepo_move : _,   // -> the homeSkgrepoMove atom
      moved_like_ancestor : _, // -> the movedLikeAncestor atom
    } = v; }
  let _ = guard;
  vec! [ "cycle", "homeRepoHerald", "overridesHere", "relRepo",
         "homeSkgrepoMove", "movedLikeAncestor" ] }

/// AffectsParent values the serializer can emit (True stays implicit).
fn affectsParent_emitted_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::AffectsParent;
  fn guard ( p : AffectsParent ) { // compile error here = update the list below
    match p {
      AffectsParent::True | AffectsParent::False | AffectsParent::NA
        => () }}
  let _ = guard;
  vec! [ "false", "na" ] }

/// Folder and Property atoms, via the same repr_in_client constants the
/// serializer uses.
fn property_and_folder_atoms () -> Vec<&'static str> {
  fn partnerFolder_guard ( c : PartnerFolder ) { // compile error here = update all_partnerFolders
    match c {
      PartnerFolder::Subscribee | PartnerFolder::Subscriber
      | PartnerFolder::Overridden | PartnerFolder::Overrider
      | PartnerFolder::Hider | PartnerFolder::Hidden
      | PartnerFolder::HiddenInSubscribee
      | PartnerFolder::HiddenOutsideOfSubscribee
      | PartnerFolder::Container | PartnerFolder::Mentioner
      | PartnerFolder::AcquiredBy | PartnerFolder::Acquired => () }}
  let _ = partnerFolder_guard;
  let all_partnerFolders : [PartnerFolder; 12] = [
    PartnerFolder::Subscribee, PartnerFolder::Subscriber,
    PartnerFolder::Overridden, PartnerFolder::Overrider,
    PartnerFolder::Hider, PartnerFolder::Hidden,
    PartnerFolder::HiddenInSubscribee,
    PartnerFolder::HiddenOutsideOfSubscribee,
    PartnerFolder::Container, PartnerFolder::Mentioner,
    PartnerFolder::AcquiredBy, PartnerFolder::Acquired ];
  fn propertyFolder_guard ( c : &PropertyFolder ) { // ditto
    match c {
      PropertyFolder::ID | PropertyFolder::Alias
        | PropertyFolder::Flags { .. } => () }}
  let _ = propertyFolder_guard;
  let all_propertyFolders : [PropertyFolder; 3] = [
    PropertyFolder::ID, PropertyFolder::Alias, PropertyFolder::flags () ];
  for c in &all_propertyFolders { propertyFolder_guard (c); }
  let all_property_atoms : [&'static str; 10] = {
    fn property_guard ( q : &Property ) { // ditto
      match q {
        Property::Alias { .. } | Property::ID { .. }
          | Property::Flag { .. }
          | Property::RemovedAlias { .. } | Property::RemovedID { .. }
          | Property::OldText { .. }
          | Property::BodyDiff { .. } | Property::HomeSkgrepoMove { .. }
          | Property::RelRepoMove { .. } | Property::ChangesetMarker { .. }
          => () }}
    let _ = property_guard;
    [ "alias", "id", "flag", "removedAlias", "removedID", "oldText", "bodyDiff",
      "homeSkgrepoMove", "relRepoMove", "changesetMarker" ] };
  // The arguments the diff view's properties take in match
  // position (org_to_text.rs's 'property_metadata_to_string').
  let property_argument_atoms : Vec<&'static str> =
    [ vec! [ "titleAndBody", "title", "body", "old", "new" ],
      ChangesetMarkerKind::ALL . into_iter ()
        . map (ChangesetMarkerKind::wire_name) . collect () ] . concat ();
  let mut out : Vec<&'static str> = Vec::new ();
  out . extend ( all_partnerFolders . iter ()
                 . map ( |c| c . repr_in_client () ) );
  out . extend ( all_propertyFolders . iter ()
                 . map ( |c| c . repr_in_client () ) );
  out . extend ( all_property_atoms );
  out . extend ( property_argument_atoms );
  out }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../tests/unit/heralds.rs"]
mod tests;
