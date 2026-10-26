/// PURPOSE: The herald rule table for non-relationship '(skg ...)'
/// metadata. Emacs and Neovim render semantic relationship facts and
/// their per-character styles themselves.
///
/// The data lives here; the code that uses the data (the lens
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
///   RULE        ::= [STYLE] (INTERC-BODY | WHEN-BODY | SIMPLE-BODY)
///   INTERC-BODY ::= INTERC SEP [LABEL] CHILDREN...
///   WHEN-BODY   ::= WHEN LABEL... CHILDREN...
///   SIMPLE-BODY ::= LABEL [ABUT] CHILDREN...
/// where each child is a literal string, the IT directive, or a
/// nested RULE. The special label ANY matches any leaf; IT echoes
/// the matched value(s). A WHEN rule applies its children to the
/// object it stands in when that object holds an element labeled
/// one of its LABELs (an atom, or a list headed by one): the table
/// keys a headline's parts on its kind atom, since every part
/// stands beside the kind at the top level of '(skg ...)'. The WHEN
/// directive stays because '(view ...)' and '(diff ...)' are the only
/// nested forms and a '(diff ...)' reads differently by kind: a node's
/// and a ghost's has two diff objects (∃+, ~+), an unknown's one
/// relationship (~+), a property's or a change viewnode's one object
/// (+). The conformance test checks that a WHEN names only kind atoms.
///
/// Three heralds need what one headline's metadata cannot say, so the
/// clients supply it before or instead of the lens: the label of a
/// node's home skgrepo ('repoBoundary'), from the table
/// 'skgrepo_labels_sexp' serves beside the rules; a pending
/// detachment, which depends on the parent; and a changed folder,
/// which depends on the members, whose labels the clients take from
/// 'folder_heralds' in 'shared/herald-styles.json'.

use crate::sexp_string::quote_sexp_string;
use crate::types::misc::SkgConfig;
use crate::types::nodes::complete::Flag;
use crate::types::viewnode::{
  ChangesetMarkerKind, DiffChange, Folder, PropertyMember, ChangeViewnode,
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
  pub when     : Vec<&'static str>,    // nonempty makes this a WHEN rule, on these kinds
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
    node_heralds_key : None, interc : None, when : vec! [], label : Some (label),
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
    interc : None, when : vec! [], label : Some (label),
    abut : false, children } ) }

/// (WHEN kinds... children...): 'children' applied to a headline
/// whose kind is one of 'kinds'.
fn when (
  kinds    : &[&'static str],
  children : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : None, interc : None, when : kinds . to_vec (),
    label : None, abut : false, children } ) }

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
    node_heralds_key : Some (label), interc : None, when : vec! [], label : Some (label),
    abut : true, children : vec! [ s (text) ] } ) }

/// (STYLE label ABUT "text"), with STYLE from 'node_heralds' under
/// NODE_HERALDS_KEY
fn leaf_abut_styled_as (
  node_heralds_key : &'static str,
  label            : &'static str,
  text             : &'static str,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (node_heralds_key), interc : None, when : vec! [],
    label : Some (label), abut : true, children : vec! [ s (text) ] } ) }

/// (STYLE ANY ABUT "text"), with STYLE from 'node_heralds' under
/// NODE_HERALDS_KEY
fn any_abut_styled_as (
  node_heralds_key : &'static str,
  text             : &'static str,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (node_heralds_key), interc : None, when : vec! [],
    label : Some ("ANY"), abut : true, children : vec! [ s (text) ] } ) }

/// (STYLE ANY children...), with STYLE from 'node_heralds' under
/// NODE_HERALDS_KEY
fn any_styled_as (
  node_heralds_key : &'static str,
  children         : Vec<RuleChild>,
) -> RuleChild {
  RuleChild::Rule ( HeraldRule {
    node_heralds_key : Some (node_heralds_key), interc : None, when : vec! [],
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

/// The diff view's '(diff ...)' form on a vognode: its node's
/// existence ('(node added)', '(node removed)') as ∃+ ∃-, its other
/// properties ('(properties changed)') as M, and the existence of its
/// relationship to its viewparent ('(rel added)', '(rel removed)') as
/// ~+ ~-, each followed by a ✓ when '(ready ...)' names that diff
/// object ('all' for M), then the readiness request. '(leaves ...)'
/// is for the clients' gestures and shows nothing. The ready part is
/// matched once per object, so each ✓ lands after its own object
/// whatever the raw order.
fn change_fact_rules_for_two_objects (
) -> Vec<RuleChild> {
  vec! [
    rule ("diff", vec! [
      rule ("node", vec! [
        leaf_styled_as ("existence", "added",   "∃+"),
        leaf_styled_as ("existence", "removed", "∃-") ]),
      rule ("properties", vec! [
        leaf_styled_as ("properties", "changed", "M") ]),
      rule ("ready", vec! [ leaf_abut_styled_as ("ready", "node", "✓"),
                            leaf_abut_styled_as ("ready", "all",  "✓") ]),
      rule ("rel", vec! [
        leaf_styled_as ("existence", "added",   "~+"),
        leaf_styled_as ("existence", "removed", "~-") ]),
      rule ("ready", vec! [ leaf_abut_styled_as ("ready", "rel", "✓") ]),
      vac ("leaves") ]) ] }

/// The readiness request inside '(editRequest ...)', rendered after
/// the change facts it acts on.
fn readiness_request_rules (
) -> Vec<RuleChild> {
  vec! [
    rule ("editRequest", vec! [
      readiness_request_rule ("readyRequest",   "ready",   "request:ready"),
      readiness_request_rule ("unreadyRequest", "unready", "request:unready") ]) ] }

/// The same for a leaf with a single diff object (a property leaf, an
/// unknown), whose '(ready)' names no object, or
/// 'all' on a changeset marker. 'added' and 'removed' render its
/// existence, whichever object it names.
fn change_fact_rules_for_one_object (
  added   : &'static str,
  removed : &'static str,
) -> Vec<RuleChild> {
  let existence = |object : &'static str| -> RuleChild {
    rule ( object, vec! [
      leaf_styled_as ("existence", "added",   added),
      leaf_styled_as ("existence", "removed", removed) ] ) };
  vec! [
    rule ("diff", vec! [
      existence ("node"),
      existence ("rel"),
      rule ("ready", vec! [ any_abut_styled_as ("ready", "✓") ]),
      vac ("leaves") ]) ] }

/// "request:ready", then ":OBJECT" for each object the request names,
/// styled under 'node_heralds_key'.
fn readiness_request_rule (
  node_heralds_key : &'static str,
  label            : &'static str,
  prefix           : &'static str,
) -> RuleChild {
  crule_styled_as ( node_heralds_key, label,
          [ vec! [ s (prefix) ],
            DiffObject::ALL . into_iter ()
            . map ( |object| rule ( object . wire_name (), vec! [ s (
              match object {
                DiffObject::Node => ":node",
                DiffObject::Rel  => ":rel",
                DiffObject::All  => ":all", } ) ] ))
            . collect () ] . concat () ) }

/// The position glyphs, first among a headline's heralds: ᗣ on a
/// ghost and on what HEAD alone warrants ('ghostWarrant'), ⊥ on what
/// the graph does not relate to its viewparent as its position
/// implies ('noWarrant'; see 'Warrant'). The ☮ of a write-protected
/// position abuts them, from its kind's rules below.
fn position_glyph_rules (
) -> Vec<RuleChild> {
  vec! [
    leaf ("ghost", "ᗣ"),
    rule ("view", vec! [
      leaf ("ghostWarrant", "ᗣ"),
      leaf ("noWarrant",    "⊥") ]) ] }

/// A child that is 'noClaim' and warranted under an editable parent:
/// the user detached it, and the next save takes it out of its
/// parent's list. Neither the warrant's absence nor the parent shows
/// in the child's own metadata, so the client adds the atom
/// 'pendingDetachment' before it hands the headline to the lens.
/// Rendered as a request.
fn pending_detachment_rule (
) -> RuleChild {
  leaf ("pendingDetachment", "detach") }

/// The view requests a headline carries, each "req:...".
fn view_request_rules (
) -> RuleChild {
  crule ("viewRequests", vec! [
    rule ("folder",  vec! [ any (vec! [ s ("req:folder:"),  RuleChild::It ]) ]),
    rule ("flags", vec! [ s ("req:flags") ]),
    rule ("editableView", vec! [ s ("req:editable") ]),
    rule ("writeProtectedView", vec! [ s ("req:write-protected") ]),
    rule ("writeProtectedSubtreeView", vec! [ s ("req:write-protected-subtree") ]),
    rule ("tombstone", vec! [ s ("req:tombstone") ]) ]) }

/// (ANY children...)
fn any (
  children : Vec<RuleChild>,
) -> RuleChild {
  rule ( "ANY", children ) }

fn s ( text : &'static str ) -> RuleChild { RuleChild::Str (text) }

/// The placeholder token the `rels` and `props` rules emit. The
/// relationship heralds are per-character styled spans the lens engine
/// cannot style, so the rules only position them: each emits this
/// sentinel where the relationship heralds belong, and the client
/// (`heralds-from-metadata`) replaces the first sentinel token with
/// the spans it renders itself from the `(rels ...)`, `(props ...)`
/// and `(birth ...)` payloads of `(view ...)`, dropping any other.
/// Kept in sync with `heralds--rels-sentinel` in
/// elisp/heralds-minor-mode.el.
pub const RELS_SPANS_SENTINEL : &str = "__RELS_SPANS__";

//
// The table
//

/// The complete herald rule table, as a single root rule labeled
/// "skg". Rule order is presentation order, independent of the raw
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
///     token with no space (used so the omitted-body glyph "B" sits
///     directly on the write-protected glyph "☮").
///
///   * 'when' -- a headline's parts stand beside its kind atom, so a
///     part that renders differently by kind ('(diff ...)' above all)
///     has its rules under a WHEN on the kinds it renders for.
///
/// WHY SOME RULES LOOK EMPTY OR REDUNDANT:
///
///   * 'vac' rules (focused, folded, a node's nodeRepo, a gnid...)
///     match and emit nothing. The engine would ignore the atoms
///     anyway; the vacuous rules document that the atom is known, and
///     let the conformance test demand that every emittable atom
///     appear here.
///
pub fn herald_rule_table () -> HeraldRule {
  HeraldRule {
    node_heralds_key : None, interc : None, when : vec! [], label : Some ("skg"),
    abut : false,
    children : [ vec! [
      vac ("focused"),
      vac ("folded"),
      vac ("bodyFolded"),
      vac ("vnid") ], // the viewnode ID starts every wrapper
      position_glyph_rules (),
      vec! [
      leaf (Folder::Alias . repr_in_client (), "aliases"),
      leaf ("alias", "alias"), // PropertyMember::Alias
      // An alias's stored relRepo is a display fact.  A
      // requested replacement lives under editRequest below, so the
      // two values can be rendered side by side without conflation.
      when ( &[ "alias" ], vec! [
        rule ("skgRepos", vec! [
          crule ("relRepo", vec! [ any (vec! [ s ("~"), RuleChild::It ]) ]) ]),
        rule ("editRequest", vec! [
          crule_styled_as ("editRequest_relRepo", "relRepo", vec! [
            any (vec! [ s ("request:~"), RuleChild::It ]) ]) ]) ]) ],
      // The write-protected folders carry ☮ ("cannot be changed
      // from here"); the editable folders (subscribeeFolder, overriddenFolder,
      // aliasFolder) do not.
      write_protected_leaves (Folder::HiddenInSubscribee . repr_in_client (),
            "It contains these, but the subscribing ancestor hides them."),
      vec! [
      leaf (Folder::HiddenOutsideOfSubscribee . repr_in_client (),
            "The subscriber ancestor hides these, but subscribes to nothing that contains them."),
      leaf (Folder::Subscribee . repr_in_client (),
            "It subscribes to these.") ],
      write_protected_leaves (Folder::Subscriber . repr_in_client (),
            "These subscribe to it."),
      write_protected_leaves (Folder::Hidden . repr_in_client (),
            "It hides these from its subscriptions."),
      write_protected_leaves (Folder::Hider . repr_in_client (),
            "These hide it from their subscriptions."),
      vec! [
      leaf (Folder::Overridden . repr_in_client (),
            "It overrides the view of these.") ],
      write_protected_leaves (Folder::Overrider . repr_in_client (),
            "These override the view of it."),
      write_protected_leaves (Folder::Container . repr_in_client (),
            "These contain it."),
      write_protected_leaves (Folder::Mentioner . repr_in_client (),
            "These link to it."),
      write_protected_leaves (Folder::AcquiredBy . repr_in_client (),
            "it was acquired by"),
      write_protected_leaves (Folder::Acquired . repr_in_client (),
            "it acquired"),
      vec! [
      leaf (Folder::ID . repr_in_client (), "IDs"),
      leaf ("id", "ID"), // PropertyMember::ID
      when ( &[ "alias", "id" ], vec! [ pending_detachment_rule () ]) ],
      write_protected_leaves (Folder::flags () . repr_in_client (),
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
      // The diff view's facts on a property leaf: + and - for
      // its existence in its folder, ✓ when ready, then its readiness
      // request.
      vec! [ when ( &kinds_whose_diff_has_one_object (),
                    [ change_fact_rules_for_one_object ("+", "-"),
                      readiness_request_rules () ] . concat () ) ],
      vec! [
      // One kind of tombstone, which says only that something stood
      // here: a node a save deleted, or one the skgrepo restriction
      // now hides.
      crule ("tombstone", vec! [ s ("something stood here") ]),
      when ( &[ "unknown" ], [
        vec! [
          leaf ("unknown", "Reference to unknown node."),
          vac ("gnid"),
          rule ("skgRepos", vec! [
            crule ("relRepo", vec! [
              any (vec! [ s ("~"), RuleChild::It ]) ]) ]),
          pending_detachment_rule (),
          rule ("editRequest", vec! [
            crule_styled_as ("editRequest_relRepo", "relRepo", vec! [
              any (vec! [ s ("request:~"), RuleChild::It ]) ]) ]) ],
        change_fact_rules_for_one_object ("~+", "~-"),
        readiness_request_rules () ] . concat () ),
      // A ghost is write-protected: its ☮ abuts its ᗣ. Its two
      // objects are its existence (∃-) and its relationship to its
      // viewparent.
      when ( &[ "ghost" ], [
        vec! [
          leaf_abut_styled_as ("writeProtected", "ghost", "☮"),
          vac ("gnid"),
          rule ("skgRepos", vec! [ vac ("nodeRepo") ]),
          // Its one edit request, beside its change facts.
          rule ("editRequest", vec! [
            leaf ("restoreNode", "restore node") ]),
          view_request_rules () ],
        change_fact_rules_for_two_objects (),
        readiness_request_rules () ] . concat () ),
      when ( &[ "node" ], [ vec! [
        vac ("node"),
        vac ("gnid"),
        // A child that does not claim membership carries the atom
        // 'noClaim'; it has no glyph of its own.
        vac ("noClaim"),
        // The server emits the atom 'writeProtected'
        // (see org_to_text.rs); we match that here. ABUT so the ☮
        // rides the position glyphs before it.
        leaf_abut ("writeProtected", "☮"),
        rule ("view", vec! [
          // Emitted only on a write-protected node whose graphnode has a
          // body -- one the rendering hides. ABUT so the B rides the ☮.
          leaf_abut ("omittedBody", "B"),
          // The relationship heralds are per-character styled spans
          // that the lens cannot style, so the server assembles
          // semantic (rels ...), (props ...) and (birth ...) facts and
          // the client renders them. These rules only position them:
          // the ANY child makes each match its list form and consume
          // the sub-forms, and emit the sentinel token, which the
          // client swaps for the rendered spans.
          rule ("rels",  vec! [ any (vec! [ s (RELS_SPANS_SENTINEL) ]) ]),
          rule ("props", vec! [ any (vec! [ s (RELS_SPANS_SENTINEL) ]) ]),
          vac ("birth"),
          leaf ("cycle", "⟳") ]),
        // overridesHere is displayed as a crucial ĥ inside the O token
        // by the client relationship renderer. The atom's
        // ID payload is load-bearing save metadata, never displayed.
        vac ("overridesHere"),
        rule ("skgRepos", vec! [
          vac ("nodeRepo"),
          // relRepo is a derived fact, not a save request.  It
          // echoes its own value directly:
          // "~" + the skgrepo name, ugly, immediately before the ⌂
          // of repoBoundary below (table order is presentation order,
          // per the module doc, so placing this rule first guarantees
          // that regardless of the atoms' order in the raw sexp).
          crule ("relRepo", vec! [ any (vec! [ s ("~"), RuleChild::It ]) ]) ]),
        rule ("view", vec! [
          // The node's home skgrepo differs from its nearest
          // occurrence ancestor's, or it is a root. The client hands
          // the lens '(repoBoundary LABEL)', LABEL from the skgrepo
          // label table ('skgrepo_labels_sexp'), for "⌂:LABEL"; the
          // bare atom, which it keeps when the node names no
          // nodeRepo, shows "⌂".
          crule ("repoBoundary", vec! [ any (vec! [ s ("⌂"), RuleChild::It ]) ]),
          leaf ("repoBoundary", "⌂") ]),
        rule ("diff", vec! [
          // The skgrepo-changes buffer of the diff view: the
          // node's home skgrepo move, "-OLD +NEW", then "Δ⌂=a" when its
          // nearest moved vognode ancestor (here a) moved the same way.
          rule ("homeSkgrepoMove", vec! [
            crule_styled_as ("homeSkgrepoMove_skgrepo", "old", vec! [
              s ("-"), any (vec! [ RuleChild::It ]) ]),
            crule_styled_as ("homeSkgrepoMove_skgrepo", "new", vec! [
              s ("+"), any (vec! [ RuleChild::It ]) ]) ]),
          crule ("movedLikeAncestor", vec! [
            any (vec! [ s ("Δ⌂="), RuleChild::It ]) ]) ]),
        pending_detachment_rule (),
        rule ("editRequest", vec! [
          leaf ("delete", "delete"),
          crule ("merge", vec! [
            any ( vec! [ s ("merge:"), RuleChild::It ] ) ]),
          leaf ("restoreRel", "restore membership"),
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
        view_request_rules () ],
        change_fact_rules_for_two_objects (),
        readiness_request_rules () ] . concat ()) ] ] . concat (),
  }}

/// The kinds whose '(diff ...)' renders a single diff object: the
/// property members and the diff view's change viewnodes but the body
/// diff, which carries none. A node's, a ghost's and an unknown's have
/// rules of their own; a folder carries none.
fn kinds_whose_diff_has_one_object (
) -> Vec<&'static str> {
  PROPERTY_AND_CHANGE_KIND_ATOMS . into_iter ()
    . filter ( |kind| *kind != "bodyDiff" )
    . collect () }

//
// Serialization
//

/// The skgrepo label table served beside the rules:
/// '(skgrepoLabels ("REPO" "LABEL")...)', one entry per configured
/// skgrepo in the config's order, LABEL its 'herald_label'. The
/// clients render a 'repoBoundary' as "⌂:LABEL", LABEL looked up by
/// the node's nodeRepo, so no rendered text rides in the metadata.
pub fn skgrepo_labels_sexp (
  config : &SkgConfig,
) -> String {
  let entries : Vec<String> =
    config . ordered_skgrepos () . into_iter ()
    . filter_map ( |name| config . skgrepos . get (&name) . map ( |skgrepo|
      format! ( "({} {})",
                quote_sexp_string (&name . 0),
                quote_sexp_string (skgrepo . herald_label ()) )))
    . collect ();
  format! ( "(skgrepoLabels{})",
            entries . iter () . map ( |entry| format! (" {}", entry) )
            . collect::<String> () ) }

/// Serialize the whole table to the sexp the lens engine interprets.
/// Strings are always quoted (unlike the 'sexp' crate, which leaves
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
    parts . push ( quote_sexp_string (sep) ); }
  if ! rule . when . is_empty () {
    parts . push ( "WHEN" . to_string () );
    parts . extend ( rule . when . iter () . map ( |kind| kind . to_string () )); }
  if let Some (label) = rule . label {
    parts . push ( label . to_string () ); }
  if rule . abut {
    parts . push ( "ABUT" . to_string () ); }
  for child in & rule . children {
    parts . push ( match child {
      RuleChild::Str (text) => quote_sexp_string (text),
      RuleChild::It         => "IT" . to_string (),
      RuleChild::Rule (r)   => serialize_rule (r), } ); }
  format! ( "({})", parts . join (" ")) }

//
// Consistency of shared/herald-styles.json
//

/// Every way 'shared/herald-styles.json' disagrees with itself or with
/// the rule table. Run as a unit test, and asserted at startup.
pub fn herald_styles_inconsistencies () -> Vec<String> {
  herald_styles_inconsistencies_of ( herald_styles () ) }

/// Every tier file names (in 'tier_order', 'node_heralds' or
/// 'relationship_heralds') that is not a key of its 'styles', every
/// styled atom of the rule table that has no entry in its
/// 'node_heralds', and every way its 'folder_heralds' disagrees with
/// the rule table ('folder_heralds_inconsistencies').
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
  [ undefined_tiers, unstyled_atoms,
    folder_heralds_inconsistencies (file) ] . concat () }

/// 'folder_heralds.write_protected' must name exactly the folders whose
/// label the rule table gives a ☮, and 'folder_heralds.changed_labels'
/// must give a string for folders only.
fn folder_heralds_inconsistencies (
  file : &Value,
) -> Vec<String> {
  let folder_atoms : HashSet<&str> =
    all_folders () . iter () . map ( |folder| folder . repr_in_client () ) . collect ();
  let listed : HashSet<&str> =
    file ["folder_heralds"] ["write_protected"] . as_array ()
    . map ( |atoms| atoms . iter () . filter_map (Value::as_str) . collect () )
    . unwrap_or_default ();
  let protected_in_table : HashSet<&str> =
    rules_in_rule_table () . into_iter ()
    . filter ( |rule| rule . node_heralds_key == Some ("writeProtected") )
    . filter_map ( |rule| rule . label )
    . filter ( |label| folder_atoms . contains (label) )
    . collect ();
  let mut problems : Vec<String> = Vec::new ();
  for atom in protected_in_table . difference (&listed) {
    problems . push ( format! (
      "the rule table gives {} a ☮, which 'folder_heralds.write_protected' omits",
      atom )); }
  for atom in listed . difference (&protected_in_table) {
    problems . push ( format! (
      "'folder_heralds.write_protected' names {}, whose label the rule table gives no ☮",
      atom )); }
  if let Some (labels) = file ["folder_heralds"] ["changed_labels"] . as_object () {
    for (atom, label) in labels {
      if atom . ends_with ("comment") { continue; }
      if ! folder_atoms . contains (atom . as_str ()) {
        problems . push ( format! (
          "'folder_heralds.changed_labels' names {}, which is no folder", atom )); }
      if ! label . is_string () {
        problems . push ( format! (
          "'folder_heralds.changed_labels' gives {} no string", atom )); }}}
  problems . sort ();
  problems }

/// Each tier file names, with the path where it does so.
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

/// Every match atom in the rule table (labels of rules, recursively,
/// and the kinds of WHEN rules), excluding the engine directives (ANY;
/// IT is not a label) and the root label "skg".
pub fn atoms_in_rule_table () -> std::collections::HashSet<&'static str> {
  rules_in_rule_table () . into_iter ()
    . flat_map ( |rule| rule . label . into_iter () . chain ( rule . when ))
    . filter ( |label| *label != "ANY" && *label != "skg" )
    . collect () }

/// Every metadata atom the server can emit in a match (label)
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
    // The viewnode ID that starts every wrapper; its value is value position.
    "vnid",
    // Kind atoms, from org_to_text.rs:
    "node", "unknown",
    "tombstone", "ghost",
    // The parts beside them:
    "gnid", "skgRepos", "nodeRepo",
    "writeProtected",
    // The two forms of derived facts.
    "view", "diff",
    // The assembled relationship-herald forms inside '(view ...)'
    // (server/herald_tokens.rs); their sub-forms are value position,
    // consumed by the client's renderer, so they are not match atoms.
    "rels", "props", "birth",
    "editRequest", "viewRequests",
    // The readiness requests inside '(editRequest ...)'.
    "ready", "unready",
    // NodeEditRequest atoms. The flag form's name and desired value
    // are matched literally so its herald can describe the complete state
    // change rather than echoing two context-free arguments.
    "delete", "merge", "flag", "true", "false",
    // The diff view's restore requests, on a ghost and on
    // a removed membership.
    "restoreNode", "restoreRel",
  ];
  // Flag-viewnode heralds match every public wire name.  Keeping this derived
  // from the registry makes adding a flag a conformance-checked change.
  atoms . extend (Flag::ALL . map (Flag::wire_name));
  atoms . extend ( graphstats_atoms () );
  atoms . extend ( viewstats_atoms () );
  atoms . extend ( claim_emitted_atoms () );
  atoms . extend ( property_and_folder_atoms () );
  atoms . extend ( ViewRequest::EMITTABLE_MATCH_ATOMS );
  atoms . extend ( change_fact_atoms () );
  atoms . extend ( client_computed_atoms () );
  atoms . into_iter () . collect () }

/// Atoms no wire carries, which the clients write into a headline's
/// metadata before they hand it to the lens: 'pendingDetachment'
/// ('pending_detachment_rule').
fn client_computed_atoms () -> Vec<&'static str> {
  vec! [ "pendingDetachment" ] }

/// Every kind atom the server writes: the kinds a WHEN may name.
pub fn emittable_kind_atoms () -> HashSet<&'static str> {
  [ "node", "ghost", "unknown", "tombstone" ] . into_iter ()
    . chain ( PROPERTY_AND_CHANGE_KIND_ATOMS )
    . chain ( all_folders () . iter () . map ( |folder| folder . repr_in_client () ))
    . collect () }

/// The kind atoms of the property members and the change viewnodes.
const PROPERTY_AND_CHANGE_KIND_ATOMS : [&'static str; 8] =
  [ "alias", "id", "flag", "oldText", "bodyDiff",
    "homeSkgrepoMove", "relRepoMove", "changesetMarker" ];

/// Every folder: the twelve partner folders, then the three property
/// folders.
fn all_folders () -> [Folder; 15] {
  [ Folder::Subscribee, Folder::Subscriber,
    Folder::Overridden, Folder::Overrider,
    Folder::Hider, Folder::Hidden,
    Folder::HiddenInSubscribee,
    Folder::HiddenOutsideOfSubscribee,
    Folder::Container, Folder::Mentioner,
    Folder::AcquiredBy, Folder::Acquired,
    Folder::ID, Folder::Alias, Folder::flags () ] }

/// The kinds every WHEN rule in the table names.
pub fn when_kinds_in_rule_table () -> HashSet<&'static str> {
  rules_in_rule_table () . into_iter ()
    . flat_map ( |rule| rule . when )
    . collect () }

/// The diff view's form, from org_to_text.rs's 'change_fact_forms':
/// its head, the heads of its parts and their atoms, and the diff
/// objects.
fn change_fact_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::ChangeFacts;
  fn guard ( facts : ChangeFacts, object : DiffObject ) {
    let ChangeFacts {
      change : _,   // -> node, rel, properties, with DiffChange::EMITTABLE_ATOMS
      ready : _,    // -> ready, with the objects
      leaves : _,   // -> leaves, with the objects
      home_skgrepo_move : _,   // -> the homeSkgrepoMove atom, below
      moved_like_ancestor : _, // -> the movedLikeAncestor atom, below
    } = facts;
    match object { // compile error here = update DiffObject::ALL
      DiffObject::Node | DiffObject::Rel | DiffObject::All => () }}
  let _ = guard;
  [ vec! [ "diff", "ready", "leaves",
           "homeSkgrepoMove", "movedLikeAncestor" ],
    DiffChange::EMITTABLE_ATOMS . to_vec (),
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

/// The match atoms of ViewnodeStats, from
/// occurrence_metadata_to_string (org_to_text.rs): the
/// facts of '(view ...)', the relRepo of '(skgRepos ...)', and the
/// occurrence's 'overridesHere'; and the property members' warrant
/// atoms. The relationship facts are the forms named in
/// 'emittable_metadata_atoms'.
fn viewstats_atoms () -> Vec<&'static str> {
  use crate::types::viewnode::ViewnodeStats;
  fn guard ( v : ViewnodeStats ) {
    let ViewnodeStats {
      cycle : _,
      repoBoundary : _,     // -> the repoBoundary atom
      isWarranted : _,      // -> the noWarrant atom
      rel_heralds : _,      // -> the rels, props and birth forms
      omitted_body : _,     // -> the omittedBody atom
      relRepo : _,          // -> the relRepo atom and herald
    } = v; }
  let _ = guard;
  vec! [ "cycle", "repoBoundary", "noWarrant", "omittedBody",
         "overridesHere", "relRepo",
         // A ghost warrant ('Warrant::ByHead').
         "ghostWarrant" ] }

/// The claim atom the serializer can emit: 'noClaim' on an occurrence,
/// an alias, an id or an unknown that does not claim membership (one
/// that claims carries nothing).
fn claim_emitted_atoms () -> Vec<&'static str> {
  vec! [ "noClaim" ] }

/// Folder and Property atoms, via the same repr_in_client constants the
/// serializer uses.
fn property_and_folder_atoms () -> Vec<&'static str> {
  fn folder_guard ( c : &Folder ) { // compile error here = update 'all_folders' 
    match c {
      Folder::Subscribee | Folder::Subscriber
      | Folder::Overridden | Folder::Overrider
      | Folder::Hider | Folder::Hidden
      | Folder::HiddenInSubscribee
      | Folder::HiddenOutsideOfSubscribee
      | Folder::Container | Folder::Mentioner
      | Folder::AcquiredBy | Folder::Acquired => (),
      Folder::ID | Folder::Alias | Folder::Flags { .. } => () }}
  let _ = folder_guard;
  let all_property_atoms : [&'static str; 8] = {
    fn propertyMember_guard ( q : &PropertyMember ) { // ditto
      match q {
        PropertyMember::Alias { .. } | PropertyMember::ID { .. }
          | PropertyMember::Flag { .. }
          => () }}
    let _ = propertyMember_guard;
    fn changeViewnode_guard ( q : &ChangeViewnode ) { // ditto
      match q {
        ChangeViewnode::OldText { .. }
          | ChangeViewnode::BodyDiff { .. } | ChangeViewnode::HomeSkgrepoMove { .. }
          | ChangeViewnode::RelRepoMove { .. } | ChangeViewnode::ChangesetMarker { .. }
          => () }}
    let _ = changeViewnode_guard;
    PROPERTY_AND_CHANGE_KIND_ATOMS };
  // The arguments the diff view's properties take in match
  // position (org_to_text.rs's 'property_metadata_to_string').
  let property_argument_atoms : Vec<&'static str> =
    [ vec! [ "titleAndBody", "title", "body", "old", "new" ],
      ChangesetMarkerKind::ALL . into_iter ()
        . map (ChangesetMarkerKind::wire_name) . collect () ] . concat ();
  let mut out : Vec<&'static str> = Vec::new ();
  out . extend ( all_folders () . iter ()
                 . map ( |c| c . repr_in_client () ) );
  out . extend ( all_property_atoms );
  out . extend ( property_argument_atoms );
  out }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../tests/unit/heralds.rs"]
mod tests;
