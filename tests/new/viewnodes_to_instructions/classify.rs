use ego_tree::{NodeId, Tree};
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::viewnodes_to_instructions::classify::{
  classify_save_roles, SaveRole, SaveRoleMap,
};
use skg::types::git::MembershipAxes;
use skg::types::misc::{ID, SourceName};
use skg::types::unchecked_viewnode::{unchecked_to_checked_tree, UncheckedViewNode};
use skg::types::viewnode::{
  mk_definitive_viewnode,
  mk_unknown_viewnode,
  viewforest_root_viewnode,
  viewnode_from_scaffold,
  DeletedNode,
  Scaffold,
  ScaffoldKind,
  ViewNode,
  ViewNodeKind,
};

fn checked_viewforest_from_org (
  input : &str,
) -> Tree<ViewNode> {
  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  unchecked_to_checked_tree (unchecked_viewforest) . unwrap() }

fn role_for_title (
  tree  : &Tree<ViewNode>,
  roles : &SaveRoleMap,
  title : &str,
) -> SaveRole {
  for node_ref in tree . nodes() {
    if node_ref . value() . title() == title {
      return roles . get (&node_ref . id()) . unwrap() . clone(); }}
  panic! ("node not found: {}", title) }

fn first_node_id_with_title (
  tree  : &Tree<ViewNode>,
  title : &str,
) -> NodeId {
  for node_ref in tree . nodes() {
    if node_ref . value() . title() == title {
      return node_ref . id(); }}
  panic! ("node not found: {}", title) }

#[test]
fn classifies_buffer_root_and_ordinary_truenodes (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg (node (id ordinary) (source main))) ordinary
            ** (skg (node (id independent) (source main) (birth independent))) independent
            "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let roles : SaveRoleMap =
    classify_save_roles (&viewforest) . unwrap();

  assert_eq!(
    roles . get (&viewforest . root() . id()),
    Some (&SaveRole::BufferRoot));
  assert_eq!(
    role_for_title (&viewforest, &roles, "root"),
    SaveRole::OrdinaryTrueNode);
  assert_eq!(
    role_for_title (&viewforest, &roles, "ordinary"),
    SaveRole::OrdinaryTrueNode);
  assert_eq!(
    role_for_title (&viewforest, &roles, "independent"),
    SaveRole::OrdinaryTrueNode); }

#[test]
fn classifies_alias_and_id_display_scaffolds (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg aliasCol) aliases
            *** (skg alias) first alias
            ** (skg idCol) ids
            *** (skg id) extra-id
            "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let roles : SaveRoleMap =
    classify_save_roles (&viewforest) . unwrap();

  assert_eq!(
    role_for_title (&viewforest, &roles, "its aliases"),
    SaveRole::DisplayOnly);
  assert_eq!(
    role_for_title (&viewforest, &roles, "first alias"),
    SaveRole::AliasText);
  assert_eq!(
    role_for_title (&viewforest, &roles, "its IDs"),
    SaveRole::DisplayOnly);
  assert_eq!(
    role_for_title (&viewforest, &roles, "extra-id"),
    SaveRole::IdDisplay); }

#[test]
fn classifies_subscribee_col_positions (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol) subscribees
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg hiddenInSubscribeeCol) hidden in
            ***** (skg (node (id hidden-in) (source main))) hidden in child
            *** (skg hiddenOutsideOfSubscribeeCol) hidden outside
            **** (skg (node (id hidden-outside) (source main))) hidden outside child
            "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let roles : SaveRoleMap =
    classify_save_roles (&viewforest) . unwrap();

  assert_eq!(
    role_for_title (&viewforest, &roles, "it subscribes to these"),
    SaveRole::DisplayOnly);
  assert_eq!(
    role_for_title (&viewforest, &roles, "subscribee"),
    SaveRole::AsSubscribee {
      subscriber : ID::from ("subscriber"),
    });
  assert_eq!(
    role_for_title (&viewforest, &roles, "hidden from this subscription"),
    SaveRole::DisplayOnly);
  assert_eq!(
    role_for_title (&viewforest, &roles, "hidden in child"),
    SaveRole::HiddenInSubscribeeCol {
      subscriber : ID::from ("subscriber"),
      subscribee : ID::from ("subscribee"),
    });
  assert_eq!(
    role_for_title (&viewforest, &roles, "hidden from all subscriptions"),
    SaveRole::DisplayOnly);
  assert_eq!(
    role_for_title (&viewforest, &roles, "hidden outside child"),
    SaveRole::HiddenOutsideOfSubscribeeCol {
      subscriber : ID::from ("subscriber"),
    }); }

#[test]
fn classifies_deleted_unknown_and_deleted_scaffolds_as_display_only (
) {
  let mut viewforest : Tree<ViewNode> =
    Tree::new (viewforest_root_viewnode ());
  let root_id : NodeId =
    viewforest . root() . id();
  let deleted_id : NodeId =
    viewforest . get_mut (root_id) . unwrap() . append (ViewNode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewNodeKind::Deleted (DeletedNode {
        id     : ID::from ("deleted"),
        source : SourceName::from ("main"),
        title  : "deleted" . to_string(),
        body   : None,
      }),
    }) . id();
  let deleted_scaff_id : NodeId =
    viewforest . get_mut (root_id) . unwrap() . append (ViewNode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewNodeKind::DeletedScaff (
        ScaffoldKind::SubscribeeCol),
    }) . id();
  let unknown_id : NodeId =
    viewforest . get_mut (root_id) . unwrap() . append (
      mk_unknown_viewnode (ID::from ("unknown"))) . id();
  let root_node_id : NodeId =
    viewforest . get_mut (root_id) . unwrap() . append (
      mk_definitive_viewnode (
        ID::from ("root"),
        SourceName::from ("main"),
        "root" . to_string(),
        None)) . id();
  let text_changed_id : NodeId =
    viewforest . get_mut (root_node_id) . unwrap() . append (
      viewnode_from_scaffold (Scaffold::TextChanged {
        staged   : true,
        unstaged : false,
      })) . id();
  let idcol_id : NodeId =
    viewforest . get_mut (root_node_id) . unwrap() . append (
      viewnode_from_scaffold (Scaffold::IDCol)) . id();
  let id_id : NodeId =
    viewforest . get_mut (idcol_id) . unwrap() . append (
      viewnode_from_scaffold (Scaffold::ID {
        id         : ID::from ("extra"),
        membership : MembershipAxes::default(),
      })) . id();

  let roles : SaveRoleMap =
    classify_save_roles (&viewforest) . unwrap();

  assert_eq!(roles . get (&deleted_id), Some (&SaveRole::DisplayOnly));
  assert_eq!(roles . get (&deleted_scaff_id), Some (&SaveRole::DisplayOnly));
  assert_eq!(roles . get (&unknown_id), Some (&SaveRole::DisplayOnly));
  assert_eq!(roles . get (&text_changed_id), Some (&SaveRole::DisplayOnly));
  assert_eq!(roles . get (&idcol_id), Some (&SaveRole::DisplayOnly));
  assert_eq!(roles . get (&id_id), Some (&SaveRole::IdDisplay)); }

#[test]
fn errors_on_hidden_in_without_subscribeecol_context (
) {
  let mut viewforest : Tree<ViewNode> =
    Tree::new (viewforest_root_viewnode ());
  let root_id : NodeId =
    viewforest . root() . id();
  let root_node_id : NodeId =
    viewforest . get_mut (root_id) . unwrap() . append (
      mk_definitive_viewnode (
        ID::from ("root"),
        SourceName::from ("main"),
        "root" . to_string(),
        None)) . id();
  let hidden_id : NodeId =
    viewforest . get_mut (root_node_id) . unwrap() . append (
      viewnode_from_scaffold (Scaffold::HiddenInSubscribeeCol)) . id();
  viewforest . get_mut (hidden_id) . unwrap() . append (
    mk_definitive_viewnode (
      ID::from ("hidden"),
      SourceName::from ("main"),
      "hidden" . to_string(),
      None));

  let error : String =
    classify_save_roles (&viewforest) . unwrap_err();
  assert!(
    error . contains (
      "HiddenInSubscribeeCol subscribee must be under SubscribeeCol"),
    "unexpected error: {}",
    error); }

#[test]
fn every_node_gets_a_role (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg aliasCol) aliases
            *** (skg alias) alias
            ** (skg (node (id child) (source main))) child
            "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let roles : SaveRoleMap =
    classify_save_roles (&viewforest) . unwrap();
  let node_count : usize =
    viewforest . nodes() . count();

  assert_eq!(roles . len(), node_count);
  assert!(
    roles . contains_key (
      &first_node_id_with_title (&viewforest, "child"))); }
