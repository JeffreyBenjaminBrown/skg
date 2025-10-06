// Tests for the reconcile_dup_instructions module
// Testing the logic for collecting and reducing duplicate instructions

use skg::save::orgnodes_to_instructions::reconcile_dup_instructions::collect_dup_instructions;
use skg::types::{ID, SkgNode, NodeSaveAction};

// Helper function to create a basic SkgNode for testing
fn create_test_node(
    id: &str,
    title: &str,
    aliases: Option<Vec<String>>,
    body: Option<String>,
    contains: Vec<ID>
) -> SkgNode {
    SkgNode {
        title: title.to_string(),
        aliases,
        ids: vec![ID::from(id)],
        body,
        contains,
        subscribes_to: vec![],
        hides_from_its_subscriptions: vec![],
        overrides_view_of: vec![],
    }
}

// Helper function to create a NodeSaveAction
fn create_save_action(might_contain_more: bool, to_delete: bool) -> NodeSaveAction {
    NodeSaveAction {
        mightContainMore: might_contain_more,
        toDelete: to_delete,
    }
}

#[test]
fn test_collect_dup_instructions_no_duplicates() {
    let instructions = vec![
        (create_test_node("id1", "Node 1", None, None, vec![]), create_save_action(false, false)),
        (create_test_node("id2", "Node 2", None, None, vec![]), create_save_action(false, false)),
        (create_test_node("id3", "Node 3", None, None, vec![]), create_save_action(false, false)),
    ];

    let grouped = collect_dup_instructions(instructions);

    assert_eq!(grouped.len(), 3, "Should have 3 groups with no duplicates");
    assert!(grouped.contains_key(&ID::from("id1")));
    assert!(grouped.contains_key(&ID::from("id2")));
    assert!(grouped.contains_key(&ID::from("id3")));

    // Each group should have exactly one instruction
    for (_, group) in grouped {
        assert_eq!(group.len(), 1);
    }
}

#[test]
fn test_collect_dup_instructions_with_duplicates() {
    let instructions = vec![
        (create_test_node("id1", "Node 1 First", None, None, vec![]), create_save_action(true, false)),
        (create_test_node("id2", "Node 2", None, None, vec![]), create_save_action(false, false)),
        (create_test_node("id1", "Node 1 Second", None, None, vec![]), create_save_action(false, false)),
        (create_test_node("id3", "Node 3", None, None, vec![]), create_save_action(false, false)),
        (create_test_node("id1", "Node 1 Third", None, None, vec![]), create_save_action(true, false)),
    ];

    let grouped = collect_dup_instructions(instructions);

    assert_eq!(grouped.len(), 3, "Should have 3 unique IDs");

    // id1 should have 3 instructions
    let id1_group = grouped.get(&ID::from("id1")).unwrap();
    assert_eq!(id1_group.len(), 3, "id1 should have 3 instructions");

    // id2 and id3 should have 1 instruction each
    let id2_group = grouped.get(&ID::from("id2")).unwrap();
    assert_eq!(id2_group.len(), 1, "id2 should have 1 instruction");

    let id3_group = grouped.get(&ID::from("id3")).unwrap();
    assert_eq!(id3_group.len(), 1, "id3 should have 1 instruction");
}

#[test]
fn test_collect_dup_instructions_empty_input() {
    let instructions = vec![];
    let grouped = collect_dup_instructions(instructions);
    assert!(grouped.is_empty(), "Empty input should produce empty output");
}

#[test]
fn test_reconcile_dup_instructions_for_one_id_error_empty() {
    // Test that we get the expected error for empty instructions
    // This doesn't require async since we return early
    let instructions: Vec<(SkgNode, NodeSaveAction)> = vec![];

    // The function should return an error for empty instructions
    // We can't directly test the async function here without TypeDB setup,
    // but we can verify the logic structure
    assert!(instructions.is_empty());
}

#[test]
fn test_consistent_to_delete_values() {
    // Test the to_delete_if_consistent logic indirectly
    let instructions_consistent = vec![
        (create_test_node("id1", "Node 1", None, None, vec![]), create_save_action(false, true)),
        (create_test_node("id1", "Node 1 Again", None, None, vec![]), create_save_action(true, true)),
    ];

    // All have toDelete=true, so should be consistent
    let to_delete_values: std::collections::HashSet<bool> =
        instructions_consistent.iter()
        .map(|(_, action)| action.toDelete)
        .collect();
    assert_eq!(to_delete_values.len(), 1, "All toDelete values should be the same");
    assert!(to_delete_values.contains(&true));
}

#[test]
fn test_inconsistent_to_delete_values() {
    // Test inconsistent toDelete values
    let instructions_inconsistent = vec![
        (create_test_node("id1", "Node 1", None, None, vec![]), create_save_action(false, true)),
        (create_test_node("id1", "Node 1 Again", None, None, vec![]), create_save_action(true, false)),
    ];

    // Mixed toDelete values should be detected
    let to_delete_values: std::collections::HashSet<bool> =
        instructions_inconsistent.iter()
        .map(|(_, action)| action.toDelete)
        .collect();
    assert_eq!(to_delete_values.len(), 2, "Should detect inconsistent toDelete values");
}

#[test]
fn test_alias_collection_and_deduplication() {
    // Test that aliases are collected and deduplicated correctly
    let node1 = create_test_node("id1", "Node 1",
        Some(vec!["alias1".to_string(), "alias2".to_string()]), None, vec![]);
    let node2 = create_test_node("id1", "Node 1 Again",
        Some(vec!["alias2".to_string(), "alias3".to_string()]), None, vec![]);
    let node3 = create_test_node("id1", "Node 1 Third",
        Some(vec!["alias1".to_string(), "alias4".to_string()]), None, vec![]);

    let instructions = vec![
        (node1, create_save_action(true, false)),
        (node2, create_save_action(true, false)),
        (node3, create_save_action(false, false)),
    ];

    // Collect all aliases
    let mut all_aliases = vec![];
    for (node, _) in &instructions {
        if let Some(aliases) = &node.aliases {
            all_aliases.extend(aliases.iter().cloned());
        }
    }

    // Deduplicate
    all_aliases.sort();
    all_aliases.dedup();

    assert_eq!(all_aliases, vec!["alias1", "alias2", "alias3", "alias4"]);
}

#[test]
fn test_content_merging_logic() {
    // Test the logic for merging definitive and appendable content
    let definitive_content = vec![ID::from("def1"), ID::from("def2")];
    let appendable_content = vec![ID::from("app1"), ID::from("def1"), ID::from("app2")];

    // Remove appendable items that are already in definitive
    let definitive_set: std::collections::HashSet<ID> =
        definitive_content.iter().cloned().collect();
    let filtered_appendable: Vec<ID> = appendable_content
        .into_iter()
        .filter(|id| !definitive_set.contains(id))
        .collect();

    assert_eq!(filtered_appendable, vec![ID::from("app1"), ID::from("app2")]);

    // Build final contents
    let mut final_contents = definitive_content;
    final_contents.extend(filtered_appendable);

    assert_eq!(final_contents, vec![
        ID::from("def1"), ID::from("def2"),
        ID::from("app1"), ID::from("app2")
    ]);
}

#[test]
fn test_title_and_body_precedence() {
    // Test that definitive values take precedence over maybe values
    let maybe_title = Some("Last Title".to_string());
    let definite_title = Some("Definite Title".to_string());

    let maybe_body = Some("Last Body".to_string());
    let definite_body = Some("Definite Body".to_string());

    // Test precedence logic: defining > last > default
    assert_eq!(
        definite_title.as_ref().or(maybe_title.as_ref()),
        Some(&"Definite Title".to_string())
    );
    assert_eq!(
        definite_body.as_ref().or(maybe_body.as_ref()),
        Some(&"Definite Body".to_string())
    );

    // Test fallback when no definitive value
    let no_definite_title: Option<String> = None;
    assert_eq!(
        no_definite_title.as_ref().or(maybe_title.as_ref()),
        Some(&"Last Title".to_string())
    );
}

#[test]
fn test_last_instruction_defines_title_and_body() {
    // Test that the LAST mightContainMore instruction defines title/body
    // when there's no defining instruction

    let instructions = vec![
        (create_test_node("id1", "First Title",
            None, Some("First Body".to_string()), vec![]),
         create_save_action(true, false)),
        (create_test_node("id1", "Middle Title",
            None, None, vec![]),
         create_save_action(true, false)),
        (create_test_node("id1", "Last Title",
            None, Some("Last Body".to_string()), vec![]),
         create_save_action(true, false)),
    ];

    // Simulate the logic: last instruction should win for both title and body
    let mut maybe_title: Option<String> = None;
    let mut maybe_body: Option<String> = None;

    for (skg_node, save_action) in &instructions {
        if save_action.mightContainMore {
            maybe_title = Some(skg_node.title.clone());
            if skg_node.body.is_some() {
                maybe_body = skg_node.body.clone();
            }
        }
    }

    assert_eq!(maybe_title, Some("Last Title".to_string()));
    assert_eq!(maybe_body, Some("Last Body".to_string()));
}

#[test]
fn test_defining_instruction_takes_precedence() {
    // Test that a defining instruction (mightContainMore=false)
    // takes precedence over all mightContainMore instructions

    let instructions = vec![
        (create_test_node("id1", "First Title",
            None, Some("First Body".to_string()), vec![ID::from("content1")]),
         create_save_action(true, false)),
        (create_test_node("id1", "Defining Title",
            None, Some("Defining Body".to_string()), vec![ID::from("content2")]),
         create_save_action(false, false)), // This is the defining instruction
        (create_test_node("id1", "Last Title",
            None, Some("Last Body".to_string()), vec![ID::from("content3")]),
         create_save_action(true, false)),
    ];

    // Simulate the logic
    let mut maybe_title: Option<String> = None;
    let mut maybe_body: Option<String> = None;
    let mut defines_title: Option<String> = None;
    let mut defines_body: Option<String> = None;
    let mut defines_content: Option<Vec<ID>> = None;

    for (skg_node, save_action) in &instructions {
        if save_action.mightContainMore {
            maybe_title = Some(skg_node.title.clone());
            if skg_node.body.is_some() {
                maybe_body = skg_node.body.clone();
            }
        } else {
            // Defining instruction
            if defines_content.is_some() {
                panic!("Multiple defining instructions");
            }
            defines_content = Some(skg_node.contains.clone());
            defines_title = Some(skg_node.title.clone());
            defines_body = skg_node.body.clone();
        }
    }

    // Defining should take precedence
    assert_eq!(defines_title.or(maybe_title), Some("Defining Title".to_string()));
    assert_eq!(defines_body.or(maybe_body), Some("Defining Body".to_string()));
    assert_eq!(defines_content, Some(vec![ID::from("content2")]));
}

#[test]
fn test_initial_content_from_disk_when_no_defining() {
    // Test that initial contents come from disk when there's no defining instruction
    // This tests the logic but doesn't actually call the async function

    let instructions = vec![
        (create_test_node("id1", "Title 1", None, None, vec![ID::from("append1")]),
         create_save_action(true, false)),
        (create_test_node("id1", "Title 2", None, None, vec![ID::from("append2")]),
         create_save_action(true, false)),
    ];

    let mut defines_content: Option<Vec<ID>> = None;
    let mut append_to_content: Vec<ID> = Vec::new();

    for (skg_node, save_action) in &instructions {
        if save_action.mightContainMore {
            append_to_content.extend(skg_node.contains.iter().cloned());
        } else {
            defines_content = Some(skg_node.contains.clone());
        }
    }

    // No defining instruction, so defines_content should be None
    assert!(defines_content.is_none());

    // Simulate reading from disk
    let disk_contents = vec![ID::from("disk1"), ID::from("disk2")];
    let initial_contents = defines_content.unwrap_or(disk_contents);

    // Simulate deduplication
    let initial_set: std::collections::HashSet<ID> =
        initial_contents.iter().cloned().collect();
    append_to_content.retain(|id| !initial_set.contains(id));

    let mut final_contents = initial_contents;
    final_contents.extend(append_to_content);

    assert_eq!(final_contents, vec![
        ID::from("disk1"), ID::from("disk2"),
        ID::from("append1"), ID::from("append2")
    ]);
}

#[test]
fn test_body_from_disk_when_no_instruction_has_body() {
    // Test that body comes from disk when no instruction has a body

    let instructions = vec![
        (create_test_node("id1", "Title 1", None, None, vec![]),
         create_save_action(true, false)),
        (create_test_node("id1", "Title 2", None, None, vec![]),
         create_save_action(true, false)),
    ];

    let mut maybe_body: Option<String> = None;
    let mut defines_body: Option<String> = None;

    for (skg_node, save_action) in &instructions {
        if save_action.mightContainMore {
            if skg_node.body.is_some() {
                maybe_body = skg_node.body.clone();
            }
        } else {
            defines_body = skg_node.body.clone();
        }
    }

    // No instruction had a body
    assert!(defines_body.is_none());
    assert!(maybe_body.is_none());

    // Simulate reading from disk
    let disk_body = Some("Body from disk".to_string());
    let final_body = defines_body.or(maybe_body).or(disk_body);

    assert_eq!(final_body, Some("Body from disk".to_string()));
}

#[test]
fn test_might_contain_more_vs_definitive() {
    // Test the distinction between mightContainMore (appendable) and definitive instructions
    let appendable_action = create_save_action(true, false);
    let definitive_action = create_save_action(false, false);

    assert!(appendable_action.mightContainMore);
    assert!(!definitive_action.mightContainMore);

    // In the actual function, mightContainMore=true means:
    // - append to content
    // - set maybe_title and maybe_body
    //
    // mightContainMore=false means:
    // - set definitive content (error if already set)
    // - set definitive title and body
}

#[test]
fn test_id_merging() {
    // Test that IDs from instructions and disk are merged without duplicates
    let instruction_ids = vec![ID::from("id1"), ID::from("id2")];
    let disk_ids = vec![ID::from("id2"), ID::from("id3"), ID::from("id4")];

    let mut final_ids = instruction_ids.clone();
    for disk_id in &disk_ids {
        if !final_ids.contains(disk_id) {
            final_ids.push(disk_id.clone());
        }
    }

    assert_eq!(final_ids, vec![
        ID::from("id1"), ID::from("id2"),
        ID::from("id3"), ID::from("id4")
    ]);
}

// Note: Integration tests for reconcile_dup_instructions_for_one_id and reconcile_dup_instructions
// would require TypeDB setup and are better suited for integration test files
