#[allow(unused_imports)]
use std::collections::HashSet;

use skg::render::orgnode::{render_org_node, render_org_node_from_text, render_metadata_header};
use skg::types::{OrgNode, MetadataItem, RelToOrgParent};

#[test]
fn test_render_org_node_from_text_no_metadata() {
    let metadata = HashSet::new();
    let result = render_org_node_from_text(
        1,
        "Test Title",
        None,
        &metadata
    );
    assert_eq!(result, "* Test Title\n");
}

#[test]
fn test_render_org_node_from_text_with_body() {
    let metadata = HashSet::new();
    let result = render_org_node_from_text(
        2,
        "Test Title",
        Some("Test body content"),
        &metadata
    );
    assert_eq!(result, "** Test Title\nTest body content\n");
}

#[test]
fn test_render_org_node_from_text_with_metadata() {
    let mut metadata = HashSet::new();
    metadata.insert(MetadataItem::RelToOrgParent(RelToOrgParent::Aliases));
    metadata.insert(MetadataItem::Folded);

    let result = render_org_node_from_text(
        1,
        "Test Title",
        None,
        &metadata
    );
    assert_eq!(result, "* <skg<relToOrgParent:aliases,folded>> Test Title\n");
}

#[test]
fn test_render_org_node_from_text_with_id_metadata() {
    let mut metadata = HashSet::new();
    metadata.insert(MetadataItem::ID("test123".to_string()));
    metadata.insert(MetadataItem::Repeated);

    let result = render_org_node_from_text(
        3,
        "Test Title",
        None,
        &metadata
    );
    assert_eq!(result, "*** <skg<id:test123,repeated>> Test Title\n");
}

#[test]
fn test_render_org_node() {
    let node = OrgNode {
        title: "Test Node Title".to_string(),
        body: Some("Test node body".to_string()),
        branches: vec![],
    };

    let mut metadata = HashSet::new();
    metadata.insert(MetadataItem::Focused);

    let result = render_org_node(
        &node,
        2,
        &metadata,
        true
    );
    assert_eq!(result, "** <skg<focused>> Test Node Title\nTest node body\n");
}

#[test]
fn test_render_org_node_without_body() {
    let node = OrgNode {
        title: "Test Node Title".to_string(),
        body: Some("Test node body".to_string()),
        branches: vec![],
    };

    let metadata = HashSet::new();

    let result = render_org_node(
        &node,
        1,
        &metadata,
        false
    );
    assert_eq!(result, "* Test Node Title\n");
}

#[test]
fn test_metadata_ordering() {
    let mut metadata = HashSet::new();
    metadata.insert(MetadataItem::RelToOrgParent(RelToOrgParent::Content));
    metadata.insert(MetadataItem::ID("xyz".to_string()));
    metadata.insert(MetadataItem::Cycle);
    metadata.insert(MetadataItem::Repeated);

    let result = render_org_node_from_text(
        1,
        "Test",
        None,
        &metadata
    );
    assert_eq!(result, "* <skg<id:xyz,relToOrgParent:content,cycle,repeated>> Test\n");
}

#[test]
#[should_panic(expected = "render_metadata_header called with empty metadata - caller should check first")]
fn test_render_metadata_header_empty() {
    let metadata = HashSet::new();
    render_metadata_header(&metadata);
}

#[test]
fn test_render_metadata_header_single_item() {
    let mut metadata = HashSet::new();
    metadata.insert(MetadataItem::Folded);

    let result = render_metadata_header(&metadata);
    assert_eq!(result, "<skg<folded>>");
}

#[test]
fn test_render_metadata_header_id_first() {
    let mut metadata = HashSet::new();
    metadata.insert(MetadataItem::RelToOrgParent(RelToOrgParent::Content));
    metadata.insert(MetadataItem::ID("abc123".to_string()));
    metadata.insert(MetadataItem::Repeated);

    let result = render_metadata_header(&metadata);
    assert_eq!(result, "<skg<id:abc123,relToOrgParent:content,repeated>>");
}

#[test]
fn test_render_metadata_header_alphabetical_order() {
    let mut metadata = HashSet::new();
    metadata.insert(MetadataItem::Repeated);
    metadata.insert(MetadataItem::Cycle);
    metadata.insert(MetadataItem::Folded);

    let result = render_metadata_header(&metadata);
    assert_eq!(result, "<skg<cycle,folded,repeated>>");
}

#[test]
#[should_panic(expected = "render_org_node_from_text called with both empty metadata and empty title")]
fn test_render_org_node_from_text_empty_metadata_and_title() {
    let metadata = HashSet::new();
    render_org_node_from_text(1, "", None, &metadata);
}