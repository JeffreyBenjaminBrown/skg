use skg::types::tree::from_org_text::tree_from_org_text;

#[test]
fn test_simple_tree() {
    let text = "* Root\n** Child\n*** Grandchild";
    let tree = tree_from_org_text(text).unwrap();
    assert_eq!(tree.root().value(), "Root");
    let child = tree.root().children().next().unwrap();
    assert_eq!(child.value(), "Child");
    let grandchild = child.children().next().unwrap();
    assert_eq!(grandchild.value(), "Grandchild");
}

#[test]
fn test_multiple_children() {
    let text = "* Root\n** Child1\n** Child2\n** Child3";
    let tree = tree_from_org_text(text).unwrap();
    let children: Vec<_> = tree.root().children().map(|c| c.value()).collect();
    assert_eq!(children, vec!["Child1", "Child2", "Child3"]);
}

#[test]
fn test_empty_input() {
    assert!(tree_from_org_text("").is_err());
}

#[test]
fn test_invalid_first_level() {
    assert!(tree_from_org_text("** NotRoot").is_err());
}
