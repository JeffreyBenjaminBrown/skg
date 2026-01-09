/// OrgNode type hierarchy that separates TrueNode from Scaffold.
/// See thoughts/type-refactor/spec-questions.org for the design.
///
/// OrgNode
///   focused : bool
///   folded : bool
///   kind : OrgNodeKind
///     True(TrueNode)
///       title, body, id, source, effect_on_parent, indefinitive,
///       view_data, edit_request, view_requests
///     Scaff(Scaffold)
///       kind : ScaffoldKind
///         Alias(String) | AliasCol | ForestRoot | ...

use crate::types::misc::ID;
use crate::types::orgnode::{
    EditRequest, OrgnodeViewData, ViewRequest,
};
use std::collections::HashSet;

//
// EffectOnParent - how a TrueNode affects its parent's contains list
//

/// Describes how a TrueNode affects its parent when saved.
/// PITFALL: During the transition, we keep ParentIgnores and HiddenFromSubscribees
/// separate for lossless round-trips. They may be merged in Phase 7.
#[derive( Debug, Clone, PartialEq, Eq )]
pub enum EffectOnParent {
    Content,              // Normal content relationship
    Subscribee,           // Subscription relationship
    ParentIgnores,        // No effect on parent (containerward views)
    HiddenFromSubscribees, // No effect on parent (hidden from subscriptions)
}

//
// ScaffoldKind - types of scaffold nodes
//

/// The kind of scaffold node. Scaffolds are display-only structures
/// that don't correspond to real nodes in the graph.
#[derive( Debug, Clone, PartialEq, Eq )]
pub enum ScaffoldKind {
    Alias       ( String ), // The string is the alias text
    AliasCol,
    ForestRoot,
    HiddenInSubscribeeCol,
    HiddenOutsideOfSubscribeeCol,
    SubscribeeCol,
}

impl ScaffoldKind {
    /// Returns the title string for this scaffold kind.
    /// For Alias, returns the alias text itself.
    /// For others, returns a fixed descriptive string.
    pub fn title ( &self ) -> &str {
        match self {
            ScaffoldKind::Alias ( s ) => s,
            ScaffoldKind::AliasCol => "its aliases",
            ScaffoldKind::ForestRoot => "",
            ScaffoldKind::HiddenInSubscribeeCol => "hidden from this subscription",
            ScaffoldKind::HiddenOutsideOfSubscribeeCol => "hidden from all subscriptions",
            ScaffoldKind::SubscribeeCol => "it subscribes to these",
        } }

    /// Returns the interp string for serialization (matches old Interp::fmt).
    pub fn interp_str ( &self ) -> &str {
        match self {
            ScaffoldKind::Alias ( _ ) => "alias",
            ScaffoldKind::AliasCol => "aliasCol",
            ScaffoldKind::ForestRoot => "forestRoot",
            ScaffoldKind::HiddenInSubscribeeCol => "hiddenInSubscribeeCol",
            ScaffoldKind::HiddenOutsideOfSubscribeeCol => "hiddenOutsideOfSubscribeeCol",
            ScaffoldKind::SubscribeeCol => "subscribeeCol",
        } } }

//
// Scaffold - a display-only node
//

/// A scaffold node. These are synthetic nodes for display purposes only.
/// They don't correspond to real nodes in the graph.
#[derive( Debug, Clone, PartialEq )]
pub struct Scaffold {
    pub kind : ScaffoldKind,
}

//
// TrueNode - a node that corresponds to a real graph node
//

/// A node that corresponds to a real node in the graph.
/// It has an ID (once saved), a source file, and can be edited.
#[derive( Debug, Clone, PartialEq )]
pub struct TrueNode {
    pub title            : String,
    pub body             : Option < String >,
    pub id               : Option < ID >,
    pub source           : Option < String >,
    pub effect_on_parent : EffectOnParent,
    pub indefinitive     : bool,
    pub view_data        : OrgnodeViewData,
    pub edit_request     : Option < EditRequest >,
    pub view_requests    : HashSet < ViewRequest >,
}

//
// OrgNodeKind - the discriminated union
//

/// Either a TrueNode or a Scaffold.
#[derive( Debug, Clone, PartialEq )]
pub enum OrgNodeKind {
    True  ( TrueNode ),
    Scaff ( Scaffold ),
}

//
// OrgNode - the top-level wrapper
//

/// The OrgNode type. Wraps focused/folded state and the kind.
#[derive( Debug, Clone, PartialEq )]
pub struct OrgNode {
    pub focused : bool,
    pub folded  : bool,
    pub kind    : OrgNodeKind,
}

impl OrgNode {
    /// Returns the ID if this is a TrueNode with an ID, None otherwise.
    pub fn id ( &self ) -> Option < &ID > {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . id . as_ref (),
            OrgNodeKind::Scaff ( _ ) => None,
        } }

    /// Returns the title. For TrueNode, returns the title field.
    /// For Scaffold, returns the scaffold kind's title.
    pub fn title ( &self ) -> &str {
        match &self . kind {
            OrgNodeKind::True ( t ) => &t . title,
            OrgNodeKind::Scaff ( s ) => s . kind . title (),
        } }

    /// Returns true if this is a TrueNode with the given effect_on_parent.
    pub fn has_effect ( &self, effect : EffectOnParent ) -> bool {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . effect_on_parent == effect,
            OrgNodeKind::Scaff ( _ ) => false,
        } }

    /// Returns true if this is a Scaffold with the given kind.
    pub fn is_scaffold ( &self, kind : &ScaffoldKind ) -> bool {
        match &self . kind {
            OrgNodeKind::Scaff ( s ) => {
                // For Alias, we compare the variant, not the string content
                match ( &s . kind, kind ) {
                    ( ScaffoldKind::Alias ( _ ), ScaffoldKind::Alias ( _ ) ) => true,
                    _ => std::mem::discriminant ( &s . kind )
                         == std::mem::discriminant ( kind ),
                } }
            OrgNodeKind::True ( _ ) => false,
        } }

    /// Returns true if this is a TrueNode (not a Scaffold).
    pub fn is_true_node ( &self ) -> bool {
        matches! ( &self . kind, OrgNodeKind::True ( _ ) ) }

    /// Returns true if this is a Scaffold (not a TrueNode).
    pub fn is_scaffold_any ( &self ) -> bool {
        matches! ( &self . kind, OrgNodeKind::Scaff ( _ ) ) }

    /// Returns the ScaffoldKind if this is a Scaffold, None otherwise.
    pub fn scaffold_kind ( &self ) -> Option < &ScaffoldKind > {
        match &self . kind {
            OrgNodeKind::Scaff ( s ) => Some ( &s . kind ),
            OrgNodeKind::True ( _ ) => None,
        } }

    /// Returns true if this is a TrueNode and is indefinitive.
    pub fn is_indefinitive ( &self ) -> bool {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . indefinitive,
            OrgNodeKind::Scaff ( _ ) => false,
        } }

    /// Returns the Interp that this OrgNode corresponds to.
    /// Used during transition to bridge old and new type systems.
    pub fn interp ( &self ) -> Interp {
        match &self . kind {
            OrgNodeKind::Scaff ( s ) => match &s . kind {
                ScaffoldKind::ForestRoot => Interp::ForestRoot,
                ScaffoldKind::AliasCol => Interp::AliasCol,
                ScaffoldKind::Alias ( _ ) => Interp::Alias,
                ScaffoldKind::SubscribeeCol => Interp::SubscribeeCol,
                ScaffoldKind::HiddenInSubscribeeCol => Interp::HiddenInSubscribeeCol,
                ScaffoldKind::HiddenOutsideOfSubscribeeCol => Interp::HiddenOutsideOfSubscribeeCol,
            },
            OrgNodeKind::True ( t ) => match t . effect_on_parent {
                EffectOnParent::Content => Interp::Content,
                EffectOnParent::Subscribee => Interp::Subscribee,
                EffectOnParent::ParentIgnores => Interp::ParentIgnores,
                EffectOnParent::HiddenFromSubscribees => Interp::HiddenFromSubscribees,
            },
        } }

    /// Returns true if this OrgNode corresponds to the given old Interp.
    /// Used during transition to bridge old and new type systems.
    pub fn matches_interp ( &self, interp : &Interp ) -> bool {
        match ( &self . kind, interp ) {
            // Scaffolds
            ( OrgNodeKind::Scaff ( s ), Interp::ForestRoot ) =>
                matches! ( s . kind, ScaffoldKind::ForestRoot ),
            ( OrgNodeKind::Scaff ( s ), Interp::AliasCol ) =>
                matches! ( s . kind, ScaffoldKind::AliasCol ),
            ( OrgNodeKind::Scaff ( s ), Interp::Alias ) =>
                matches! ( s . kind, ScaffoldKind::Alias ( _ ) ),
            ( OrgNodeKind::Scaff ( s ), Interp::SubscribeeCol ) =>
                matches! ( s . kind, ScaffoldKind::SubscribeeCol ),
            ( OrgNodeKind::Scaff ( s ), Interp::HiddenInSubscribeeCol ) =>
                matches! ( s . kind, ScaffoldKind::HiddenInSubscribeeCol ),
            ( OrgNodeKind::Scaff ( s ), Interp::HiddenOutsideOfSubscribeeCol ) =>
                matches! ( s . kind, ScaffoldKind::HiddenOutsideOfSubscribeeCol ),
            // TrueNodes
            ( OrgNodeKind::True ( t ), Interp::Content ) =>
                t . effect_on_parent == EffectOnParent::Content,
            ( OrgNodeKind::True ( t ), Interp::Subscribee ) =>
                t . effect_on_parent == EffectOnParent::Subscribee,
            ( OrgNodeKind::True ( t ), Interp::ParentIgnores ) =>
                t . effect_on_parent == EffectOnParent::ParentIgnores,
            ( OrgNodeKind::True ( t ), Interp::HiddenFromSubscribees ) =>
                t . effect_on_parent == EffectOnParent::HiddenFromSubscribees,
            // Mismatched kinds
            _ => false,
        } }

    //
    // Mutation helpers (for TrueNodes only; no-op for Scaffolds)
    //

    /// Set the indefinitive flag. No-op for Scaffolds.
    pub fn set_indefinitive ( &mut self, value : bool ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . indefinitive = value;
        } }

    /// Clear the body (set to None). No-op for Scaffolds.
    pub fn clear_body ( &mut self ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . body = None;
        } }

    /// Set the title. No-op for Scaffolds.
    pub fn set_title ( &mut self, title : String ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . title = title;
        } }

    /// Set the source. No-op for Scaffolds.
    pub fn set_source ( &mut self, source : String ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . source = Some ( source );
        } }

    /// Set the cycle flag in view_data. No-op for Scaffolds.
    pub fn set_cycle ( &mut self, value : bool ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . view_data . cycle = value;
        } }

    /// Get mutable access to view_requests. Returns None for Scaffolds.
    pub fn view_requests_mut ( &mut self ) -> Option < &mut HashSet < ViewRequest > > {
        match &mut self . kind {
            OrgNodeKind::True ( t ) => Some ( &mut t . view_requests ),
            OrgNodeKind::Scaff ( _ ) => None,
        } }

    /// Check if source is set. Returns false for Scaffolds.
    pub fn has_source ( &self ) -> bool {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . source . is_some (),
            OrgNodeKind::Scaff ( _ ) => false,
        } }

    /// Get the source if this is a TrueNode. Returns None for Scaffolds.
    pub fn source ( &self ) -> Option < &String > {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . source . as_ref (),
            OrgNodeKind::Scaff ( _ ) => None,
        } }

    /// Get the focused flag.
    pub fn focused ( &self ) -> bool {
        self . focused }

    /// Get the folded flag.
    pub fn folded ( &self ) -> bool {
        self . folded }

    /// Get the cycle flag from view_data. Returns false for Scaffolds.
    pub fn cycle ( &self ) -> bool {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . view_data . cycle,
            OrgNodeKind::Scaff ( _ ) => false,
        } }

    /// Get the view_requests if this is a TrueNode. Returns None for Scaffolds.
    pub fn view_requests ( &self ) -> Option < &HashSet < ViewRequest > > {
        match &self . kind {
            OrgNodeKind::True ( t ) => Some ( &t . view_requests ),
            OrgNodeKind::Scaff ( _ ) => None,
        } }

    /// Get the edit_request if this is a TrueNode. Returns None for Scaffolds.
    pub fn edit_request ( &self ) -> Option < &EditRequest > {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . edit_request . as_ref (),
            OrgNodeKind::Scaff ( _ ) => None,
        } }

    /// Set the edit_request. No-op for Scaffolds.
    pub fn set_edit_request ( &mut self, edit_request : Option < EditRequest > ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . edit_request = edit_request;
        } }

    /// Set numContainers in view_data.relationships. No-op for Scaffolds.
    pub fn set_num_containers ( &mut self, value : Option < usize > ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . view_data . relationships . numContainers = value;
        } }

    /// Set numContents in view_data.relationships. No-op for Scaffolds.
    pub fn set_num_contents ( &mut self, value : Option < usize > ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . view_data . relationships . numContents = value;
        } }

    /// Set numLinksIn in view_data.relationships. No-op for Scaffolds.
    pub fn set_num_links_in ( &mut self, value : Option < usize > ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . view_data . relationships . numLinksIn = value;
        } }

    /// Set parentIsContainer in view_data.relationships. No-op for Scaffolds.
    pub fn set_parent_is_container ( &mut self, value : bool ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . view_data . relationships . parentIsContainer = value;
        } }

    /// Set parentIsContent in view_data.relationships. No-op for Scaffolds.
    pub fn set_parent_is_content ( &mut self, value : bool ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . view_data . relationships . parentIsContent = value;
        } }

    /// Set effect_on_parent. No-op for Scaffolds.
    pub fn set_effect_on_parent ( &mut self, effect : EffectOnParent ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . effect_on_parent = effect;
        } }

    /// Convert a TrueNode to an Alias scaffold.
    /// Uses the TrueNode's title as the alias text.
    /// No-op if already a Scaffold.
    pub fn convert_to_alias ( &mut self ) {
        if let OrgNodeKind::True ( true_node ) = &self . kind {
            self . kind = OrgNodeKind::Scaff ( Scaffold {
                kind : ScaffoldKind::Alias ( true_node . title . clone () ),
            });
        } }

    /// Set the id. No-op for Scaffolds.
    pub fn set_id ( &mut self, id : Option < ID > ) {
        if let OrgNodeKind::True ( t ) = &mut self . kind {
            t . id = id;
        } }

    /// Returns true if this node should have no source (is a Scaffold or Alias).
    /// Mirrors Interp::should_be_sourceless().
    pub fn should_be_sourceless ( &self ) -> bool {
        match &self . kind {
            OrgNodeKind::Scaff ( _ ) => true,
            OrgNodeKind::True ( _ ) => false,
        } }

    /// Get the body if this is a TrueNode. Returns None for Scaffolds.
    pub fn body ( &self ) -> Option < &String > {
        match &self . kind {
            OrgNodeKind::True ( t ) => t . body . as_ref (),
            OrgNodeKind::Scaff ( _ ) => None,
        } }
}

//
// Defaults
//

impl Default for TrueNode {
    fn default () -> Self {
        TrueNode {
            title            : String::new (),
            body             : None,
            id               : None,
            source           : None,
            effect_on_parent : EffectOnParent::Content,
            indefinitive     : false,
            view_data        : OrgnodeViewData::default (),
            edit_request     : None,
            view_requests    : HashSet::new (),
        } } }

impl Default for OrgNode {
    fn default () -> Self {
        OrgNode {
            focused : false,
            folded  : false,
            kind    : OrgNodeKind::True ( TrueNode::default () ),
        } } }

//
// Conversion functions
//

use crate::types::orgnode::{
    Interp, OrgnodeMetadata,
};

/// Create a OrgNode directly from parsed components.
/// This is the direct constructor for parsing, avoiding the intermediate OrgNode.
/// Takes the same inputs as parsing produces: metadata, title, and optional body.
pub fn from_parsed (
    metadata : &OrgnodeMetadata,
    title    : String,
    body     : Option < String >,
) -> OrgNode {
    let focused = metadata . viewData . focused;
    let folded  = metadata . viewData . folded;

    let kind = match metadata . code . interp {
        // Scaffold kinds
        Interp::ForestRoot => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::ForestRoot,
        }),
        Interp::AliasCol => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::AliasCol,
        }),
        Interp::Alias => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::Alias ( title.clone () ),
        }),
        Interp::SubscribeeCol => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::SubscribeeCol,
        }),
        Interp::HiddenOutsideOfSubscribeeCol => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::HiddenOutsideOfSubscribeeCol,
        }),
        Interp::HiddenInSubscribeeCol => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::HiddenInSubscribeeCol,
        }),

        // TrueNode kinds
        Interp::Content => OrgNodeKind::True ( TrueNode {
            title,
            body,
            id               : metadata . id . clone (),
            source           : metadata . source . clone (),
            effect_on_parent : EffectOnParent::Content,
            indefinitive     : metadata . code . indefinitive,
            view_data        : metadata . viewData . clone (),
            edit_request     : metadata . code . editRequest . clone (),
            view_requests    : metadata . code . viewRequests . clone (),
        }),
        Interp::Subscribee => OrgNodeKind::True ( TrueNode {
            title,
            body,
            id               : metadata . id . clone (),
            source           : metadata . source . clone (),
            effect_on_parent : EffectOnParent::Subscribee,
            indefinitive     : metadata . code . indefinitive,
            view_data        : metadata . viewData . clone (),
            edit_request     : metadata . code . editRequest . clone (),
            view_requests    : metadata . code . viewRequests . clone (),
        }),
        Interp::ParentIgnores => OrgNodeKind::True ( TrueNode {
            title,
            body,
            id               : metadata . id . clone (),
            source           : metadata . source . clone (),
            effect_on_parent : EffectOnParent::ParentIgnores,
            indefinitive     : metadata . code . indefinitive,
            view_data        : metadata . viewData . clone (),
            edit_request     : metadata . code . editRequest . clone (),
            view_requests    : metadata . code . viewRequests . clone (),
        }),
        Interp::HiddenFromSubscribees => OrgNodeKind::True ( TrueNode {
            title,
            body,
            id               : metadata . id . clone (),
            source           : metadata . source . clone (),
            effect_on_parent : EffectOnParent::HiddenFromSubscribees,
            indefinitive     : metadata . code . indefinitive,
            view_data        : metadata . viewData . clone (),
            edit_request     : metadata . code . editRequest . clone (),
            view_requests    : metadata . code . viewRequests . clone (),
        }),
    };

    OrgNode { focused, folded, kind } }

/// Create a Content OrgNode directly from disk data.
/// This is used when loading nodes from disk, bypassing the old OrgNode type.
pub fn orgnode_content_from_disk (
    id     : ID,
    source : String,
    title  : String,
    body   : Option < String >,
) -> OrgNode {
    OrgNode {
        focused : false,
        folded  : false,
        kind    : OrgNodeKind::True ( TrueNode {
            title,
            body,
            id               : Some ( id ),
            source           : Some ( source ),
            effect_on_parent : EffectOnParent::Content,
            indefinitive     : false,
            view_data        : OrgnodeViewData::default (),
            edit_request     : None,
            view_requests    : HashSet::new (),
        }),
    } }

/// Convert Interp to EffectOnParent for TrueNode cases.
/// Returns None for Scaffold interps.
pub fn effect_on_parent_from_interp ( interp : &Interp ) -> Option < EffectOnParent > {
    match interp {
        Interp::Content               => Some ( EffectOnParent::Content ),
        Interp::Subscribee            => Some ( EffectOnParent::Subscribee ),
        Interp::HiddenFromSubscribees => Some ( EffectOnParent::HiddenFromSubscribees ),
        Interp::ParentIgnores         => Some ( EffectOnParent::ParentIgnores ),
        _ => None, // Scaffold interps
    } }

/// Create a Scaffold OrgNode from an Interp and optional title.
/// For Alias, the title is required and becomes the alias text.
/// For other scaffolds, title is ignored.
/// Returns Err if the interp is not a scaffold type.
/// DEPRECATED: Use orgnode_from_scaffold_kind instead.
pub fn orgnode_scaffold_from_interp (
    interp : Interp,
    title  : &str,
) -> Result < OrgNode, String > {
    let kind = match interp {
        Interp::AliasCol                     => ScaffoldKind::AliasCol,
        Interp::Alias                        => ScaffoldKind::Alias ( title . to_string () ),
        Interp::SubscribeeCol                => ScaffoldKind::SubscribeeCol,
        Interp::HiddenInSubscribeeCol        => ScaffoldKind::HiddenInSubscribeeCol,
        Interp::HiddenOutsideOfSubscribeeCol => ScaffoldKind::HiddenOutsideOfSubscribeeCol,
        Interp::ForestRoot                   => ScaffoldKind::ForestRoot,
        _ => return Err ( format! ( "Interp {:?} is not a scaffold type", interp ) ),
    };
    Ok ( orgnode_from_scaffold_kind ( kind ) ) }

/// Create a Scaffold OrgNode from a ScaffoldKind.
pub fn orgnode_from_scaffold_kind ( kind : ScaffoldKind ) -> OrgNode {
    OrgNode {
        focused : false,
        folded  : false,
        kind    : OrgNodeKind::Scaff ( Scaffold { kind } ),
    } }

/// Create an indefinitive OrgNode from disk data with a specific effect.
/// This is used for subscription-related nodes (Subscribee, HiddenFromSubscribees).
/// Body is always None since indefinitive nodes don't have editable content.
pub fn orgnode_indefinitive_from_disk (
    id               : ID,
    source           : String,
    title            : String,
    effect_on_parent : EffectOnParent,
) -> OrgNode {
    OrgNode {
        focused : false,
        folded  : false,
        kind    : OrgNodeKind::True ( TrueNode {
            title,
            body             : None,
            id               : Some ( id ),
            source           : Some ( source ),
            effect_on_parent,
            indefinitive     : true,
            view_data        : OrgnodeViewData::default (),
            edit_request     : None,
            view_requests    : HashSet::new (),
        }),
    } }

/// Create a OrgNode from disk data with full metadata control.
/// This is used when rebuilding a node from disk while preserving metadata.
pub fn orgnode_with_metadata (
    id               : ID,
    source           : String,
    title            : String,
    body             : Option < String >,
    effect_on_parent : EffectOnParent,
    indefinitive     : bool,
    edit_request     : Option < EditRequest >,
    view_requests    : HashSet < ViewRequest >,
) -> OrgNode {
    OrgNode {
        focused : false,
        folded  : false,
        kind    : OrgNodeKind::True ( TrueNode {
            title,
            body,
            id               : Some ( id ),
            source           : Some ( source ),
            effect_on_parent,
            indefinitive,
            view_data        : OrgnodeViewData::default (),
            edit_request,
            view_requests,
        }),
    } }

//
// Helper to create a ForestRoot OrgNode
//

pub fn forest_root_orgnode () -> OrgNode {
    OrgNode {
        focused : false,
        folded  : false,
        kind    : OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::ForestRoot,
        }),
    } }

//
// Tests
//

#[cfg(test)]
mod tests {
    use super::*;

    // Test ScaffoldKind::title() returns correct strings
    #[test]
    fn test_scaffold_kind_title () {
        assert_eq! ( ScaffoldKind::ForestRoot . title (), "" );
        assert_eq! ( ScaffoldKind::AliasCol . title (), "its aliases" );
        assert_eq! ( ScaffoldKind::SubscribeeCol . title (),
                     "it subscribes to these" );
        assert_eq! ( ScaffoldKind::HiddenInSubscribeeCol . title (),
                     "hidden from this subscription" );
        assert_eq! ( ScaffoldKind::HiddenOutsideOfSubscribeeCol . title (),
                     "hidden from all subscriptions" );
        assert_eq! ( ScaffoldKind::Alias ( "foo" . to_string () ) . title (),
                     "foo" );
    }
}
