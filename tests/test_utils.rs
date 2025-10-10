// cargo test --test test_utils

use skg::test_utils::compare_headlines_modulo_id;

#[test]
fn test_compare_headlines_modulo_id() {
    // Test identical headlines
    assert!(compare_headlines_modulo_id(
        "* Title",
        "* Title"
    ));

    // Test headlines that differ only by ID
    assert!(compare_headlines_modulo_id(
        "* (skg (id abc123)) Title",
        "* (skg (id xyz789)) Title"
    ));

    // Test headlines where one has ID and other doesn't - should be unequal
    assert!(!compare_headlines_modulo_id(
        "* (skg (id abc123)) Title",
        "* Title"
    ));

    // Test headlines with same other metadata but different IDs
    assert!(compare_headlines_modulo_id(
        "* (skg (id abc) (relToOrgParent content)) Title",
        "* (skg (id xyz) (relToOrgParent content)) Title"
    ));

    // Test headlines that differ by title
    assert!(!compare_headlines_modulo_id(
        "* (skg (id abc)) Title One",
        "* (skg (id xyz)) Title Two"
    ));

    // Test headlines that differ by level
    assert!(!compare_headlines_modulo_id(
        "* (skg (id abc)) Title",
        "** (skg (id xyz)) Title"
    ));

    // Test headlines that differ by other metadata
    assert!(!compare_headlines_modulo_id(
        "* (skg (id abc) (relToOrgParent content)) Title",
        "* (skg (id xyz) (relToOrgParent alias)) Title"
    ));

    // Test non-headlines
    assert!(compare_headlines_modulo_id(
        "This is body text",
        "This is body text"
    ));

    assert!(!compare_headlines_modulo_id(
        "This is body text",
        "This is different text"
    ));

    // Test mixed (one headline, one not)
    assert!(!compare_headlines_modulo_id(
        "* Title",
        "Body text"
    ));
}