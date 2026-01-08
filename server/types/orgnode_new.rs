/// New OrgNode type hierarchy that separates TrueNode from Scaffold.
/// See thoughts/type-refactor/spec-questions.org for the design.
///
/// NewOrgNode
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
// NewOrgNode - the top-level wrapper
//

/// The new OrgNode type. Wraps focused/folded state and the kind.
#[derive( Debug, Clone, PartialEq )]
pub struct NewOrgNode {
    pub focused : bool,
    pub folded  : bool,
    pub kind    : OrgNodeKind,
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

impl Default for NewOrgNode {
    fn default () -> Self {
        NewOrgNode {
            focused : false,
            folded  : false,
            kind    : OrgNodeKind::True ( TrueNode::default () ),
        } } }

//
// Conversion functions
//

use crate::types::orgnode::{
    default_metadata, Interp, OrgNode, OrgnodeCode, OrgnodeMetadata,
};

/// Convert an old OrgNode to a NewOrgNode.
/// This should be lossless for valid OrgNodes.
pub fn from_old_orgnode ( old : &OrgNode ) -> NewOrgNode {
    let focused = old . metadata . viewData . focused;
    let folded  = old . metadata . viewData . folded;

    let kind = match old . metadata . code . interp {
        // Scaffold kinds
        Interp::ForestRoot => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::ForestRoot,
        }),
        Interp::AliasCol => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::AliasCol,
        }),
        Interp::Alias => OrgNodeKind::Scaff ( Scaffold {
            kind : ScaffoldKind::Alias ( old . title . clone () ),
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
            title            : old . title . clone (),
            body             : old . body . clone (),
            id               : old . metadata . id . clone (),
            source           : old . metadata . source . clone (),
            effect_on_parent : EffectOnParent::Content,
            indefinitive     : old . metadata . code . indefinitive,
            view_data        : old . metadata . viewData . clone (),
            edit_request     : old . metadata . code . editRequest . clone (),
            view_requests    : old . metadata . code . viewRequests . clone (),
        }),
        Interp::Subscribee => OrgNodeKind::True ( TrueNode {
            title            : old . title . clone (),
            body             : old . body . clone (),
            id               : old . metadata . id . clone (),
            source           : old . metadata . source . clone (),
            effect_on_parent : EffectOnParent::Subscribee,
            indefinitive     : old . metadata . code . indefinitive,
            view_data        : old . metadata . viewData . clone (),
            edit_request     : old . metadata . code . editRequest . clone (),
            view_requests    : old . metadata . code . viewRequests . clone (),
        }),
        Interp::ParentIgnores => OrgNodeKind::True ( TrueNode {
            title            : old . title . clone (),
            body             : old . body . clone (),
            id               : old . metadata . id . clone (),
            source           : old . metadata . source . clone (),
            effect_on_parent : EffectOnParent::ParentIgnores,
            indefinitive     : old . metadata . code . indefinitive,
            view_data        : old . metadata . viewData . clone (),
            edit_request     : old . metadata . code . editRequest . clone (),
            view_requests    : old . metadata . code . viewRequests . clone (),
        }),
        Interp::HiddenFromSubscribees => OrgNodeKind::True ( TrueNode {
            title            : old . title . clone (),
            body             : old . body . clone (),
            id               : old . metadata . id . clone (),
            source           : old . metadata . source . clone (),
            effect_on_parent : EffectOnParent::HiddenFromSubscribees,
            indefinitive     : old . metadata . code . indefinitive,
            view_data        : old . metadata . viewData . clone (),
            edit_request     : old . metadata . code . editRequest . clone (),
            view_requests    : old . metadata . code . viewRequests . clone (),
        }),
    };

    NewOrgNode { focused, folded, kind } }

/// Convert a NewOrgNode back to an old OrgNode.
/// This should be lossless.
pub fn to_old_orgnode ( new : &NewOrgNode ) -> OrgNode {
    match &new . kind {
        OrgNodeKind::Scaff ( scaffold ) => {
            let mut md = default_metadata ();
            md . viewData . focused = new . focused;
            md . viewData . folded  = new . folded;
            md . code . interp = match &scaffold . kind {
                ScaffoldKind::Alias ( _ )                  => Interp::Alias,
                ScaffoldKind::AliasCol                     => Interp::AliasCol,
                ScaffoldKind::ForestRoot                   => Interp::ForestRoot,
                ScaffoldKind::HiddenInSubscribeeCol        => Interp::HiddenInSubscribeeCol,
                ScaffoldKind::HiddenOutsideOfSubscribeeCol => Interp::HiddenOutsideOfSubscribeeCol,
                ScaffoldKind::SubscribeeCol                => Interp::SubscribeeCol,
            };
            // For Alias, title comes from the scaffold; others use empty or fixed strings
            let title = match &scaffold . kind {
                ScaffoldKind::Alias ( s ) => s . clone (),
                _ => String::new (), // Scaffolds other than Alias have empty title in old format
            };
            OrgNode {
                metadata : md,
                title,
                body : None,
            } }

        OrgNodeKind::True ( true_node ) => {
            let interp = match true_node . effect_on_parent {
                EffectOnParent::Content              => Interp::Content,
                EffectOnParent::Subscribee           => Interp::Subscribee,
                EffectOnParent::ParentIgnores        => Interp::ParentIgnores,
                EffectOnParent::HiddenFromSubscribees => Interp::HiddenFromSubscribees,
            };
            let mut view_data = true_node . view_data . clone ();
            view_data . focused = new . focused;
            view_data . folded  = new . folded;
            let md = OrgnodeMetadata {
                id       : true_node . id . clone (),
                source   : true_node . source . clone (),
                viewData : view_data,
                code     : OrgnodeCode {
                    interp,
                    indefinitive : true_node . indefinitive,
                    editRequest  : true_node . edit_request . clone (),
                    viewRequests : true_node . view_requests . clone (),
                },
            };
            OrgNode {
                metadata : md,
                title    : true_node . title . clone (),
                body     : true_node . body . clone (),
            } } } }

//
// Helper to create a ForestRoot NewOrgNode
//

pub fn forest_root_new_orgnode () -> NewOrgNode {
    NewOrgNode {
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
    use crate::types::misc::ID;
    use crate::types::orgnode::{
        default_metadata, forest_root_orgnode, EditRequest, Interp,
        OrgNode, ViewRequest,
    };

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

    // Test round-trip: old -> new -> old for each scaffold Interp
    #[test]
    fn test_roundtrip_scaffold_forest_root () {
        let old = forest_root_orgnode ();
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_scaffold_alias_col () {
        let mut md = default_metadata ();
        md . code . interp = Interp::AliasCol;
        let old = OrgNode { metadata: md, title: String::new (), body: None };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_scaffold_alias () {
        let mut md = default_metadata ();
        md . code . interp = Interp::Alias;
        let old = OrgNode {
            metadata : md,
            title    : "my alias" . to_string (),
            body     : None,
        };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_scaffold_subscribee_col () {
        let mut md = default_metadata ();
        md . code . interp = Interp::SubscribeeCol;
        let old = OrgNode { metadata: md, title: String::new (), body: None };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_scaffold_hidden_outside () {
        let mut md = default_metadata ();
        md . code . interp = Interp::HiddenOutsideOfSubscribeeCol;
        let old = OrgNode { metadata: md, title: String::new (), body: None };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_scaffold_hidden_in () {
        let mut md = default_metadata ();
        md . code . interp = Interp::HiddenInSubscribeeCol;
        let old = OrgNode { metadata: md, title: String::new (), body: None };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    // Test round-trip: old -> new -> old for each TrueNode Interp
    #[test]
    fn test_roundtrip_true_content () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "abc123" ) );
        md . source = Some ( "test.skg" . to_string () );
        md . code . interp = Interp::Content;
        md . code . indefinitive = true;
        md . viewData . focused = true;
        md . viewData . folded = true;
        let old = OrgNode {
            metadata : md,
            title    : "Test Node" . to_string (),
            body     : Some ( "Body text" . to_string () ),
        };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_true_subscribee () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "sub123" ) );
        md . code . interp = Interp::Subscribee;
        let old = OrgNode {
            metadata : md,
            title    : "Subscribee" . to_string (),
            body     : None,
        };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_true_parent_ignores () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "pi123" ) );
        md . code . interp = Interp::ParentIgnores;
        let old = OrgNode {
            metadata : md,
            title    : "Parent Ignores" . to_string (),
            body     : None,
        };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    #[test]
    fn test_roundtrip_true_hidden_from_subscribees () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "hfs123" ) );
        md . code . interp = Interp::HiddenFromSubscribees;
        let old = OrgNode {
            metadata : md,
            title    : "Hidden From Subscribees" . to_string (),
            body     : None,
        };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    // Test round-trip with edit request
    #[test]
    fn test_roundtrip_with_edit_request () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "del123" ) );
        md . code . interp = Interp::Content;
        md . code . editRequest = Some ( EditRequest::Delete );
        let old = OrgNode {
            metadata : md,
            title    : "To Delete" . to_string (),
            body     : None,
        };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    // Test round-trip with view requests
    #[test]
    fn test_roundtrip_with_view_requests () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "vr123" ) );
        md . code . interp = Interp::Content;
        md . code . viewRequests . insert ( ViewRequest::Aliases );
        md . code . viewRequests . insert ( ViewRequest::Containerward );
        let old = OrgNode {
            metadata : md,
            title    : "With Views" . to_string (),
            body     : None,
        };
        let new = from_old_orgnode ( &old );
        let back = to_old_orgnode ( &new );
        assert_eq! ( old, back );
    }

    // Test conversion preserves effect_on_parent correctly
    #[test]
    fn test_effect_on_parent_mapping () {
        // Content
        let mut md = default_metadata ();
        md . code . interp = Interp::Content;
        let new = from_old_orgnode ( &OrgNode {
            metadata: md, title: String::new (), body: None });
        match new . kind {
            OrgNodeKind::True ( t ) =>
                assert_eq! ( t . effect_on_parent, EffectOnParent::Content ),
            _ => panic! ( "Expected TrueNode" ),
        }

        // Subscribee
        let mut md = default_metadata ();
        md . code . interp = Interp::Subscribee;
        let new = from_old_orgnode ( &OrgNode {
            metadata: md, title: String::new (), body: None });
        match new . kind {
            OrgNodeKind::True ( t ) =>
                assert_eq! ( t . effect_on_parent, EffectOnParent::Subscribee ),
            _ => panic! ( "Expected TrueNode" ),
        }

        // ParentIgnores
        let mut md = default_metadata ();
        md . code . interp = Interp::ParentIgnores;
        let new = from_old_orgnode ( &OrgNode {
            metadata: md, title: String::new (), body: None });
        match new . kind {
            OrgNodeKind::True ( t ) =>
                assert_eq! ( t . effect_on_parent, EffectOnParent::ParentIgnores ),
            _ => panic! ( "Expected TrueNode" ),
        }

        // HiddenFromSubscribees
        let mut md = default_metadata ();
        md . code . interp = Interp::HiddenFromSubscribees;
        let new = from_old_orgnode ( &OrgNode {
            metadata: md, title: String::new (), body: None });
        match new . kind {
            OrgNodeKind::True ( t ) =>
                assert_eq! ( t . effect_on_parent,
                             EffectOnParent::HiddenFromSubscribees ),
            _ => panic! ( "Expected TrueNode" ),
        }
    }

    // Test that scaffolds are correctly identified
    #[test]
    fn test_scaffold_kind_mapping () {
        let scaffolds = vec![
            ( Interp::ForestRoot, ScaffoldKind::ForestRoot ),
            ( Interp::AliasCol, ScaffoldKind::AliasCol ),
            ( Interp::SubscribeeCol, ScaffoldKind::SubscribeeCol ),
            ( Interp::HiddenOutsideOfSubscribeeCol,
              ScaffoldKind::HiddenOutsideOfSubscribeeCol ),
            ( Interp::HiddenInSubscribeeCol,
              ScaffoldKind::HiddenInSubscribeeCol ),
        ];
        for ( interp, expected_kind ) in scaffolds {
            let mut md = default_metadata ();
            md . code . interp = interp;
            let new = from_old_orgnode ( &OrgNode {
                metadata: md, title: String::new (), body: None });
            match new . kind {
                OrgNodeKind::Scaff ( s ) => {
                    // Can't directly compare ScaffoldKind because Alias has data
                    // Just check it's a scaffold of the right variant
                    assert_eq! ( std::mem::discriminant ( &s . kind ),
                                 std::mem::discriminant ( &expected_kind ));
                }
                _ => panic! ( "Expected Scaffold" ),
            }
        }
    }

    // Test Alias scaffold preserves title
    #[test]
    fn test_alias_preserves_title () {
        let mut md = default_metadata ();
        md . code . interp = Interp::Alias;
        let old = OrgNode {
            metadata : md,
            title    : "My Alias Title" . to_string (),
            body     : None,
        };
        let new = from_old_orgnode ( &old );
        match &new . kind {
            OrgNodeKind::Scaff ( s ) => match &s . kind {
                ScaffoldKind::Alias ( t ) =>
                    assert_eq! ( t, "My Alias Title" ),
                _ => panic! ( "Expected Alias" ),
            }
            _ => panic! ( "Expected Scaffold" ),
        }
    }
}
