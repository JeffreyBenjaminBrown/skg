use skg::serve::containerward_view::extract_containerward_view_params;
use skg::types::ID;

use std::error::Error;

#[test]
fn test_sexp_parsing_generative() -> Result<(), Box<dyn Error>> {
    // Characters that could befuddle S-expression parsing from Emacs
    let problematic_chars = [' ', '"', '(', ')', '\'', '`'];

    // Generate 50 random test cases
    for i in 0..50 {
        // Create a random test ID
        let test_id = format!("test{}", i);

        // Pick two random problematic characters for this test
        let char1 = problematic_chars[i % problematic_chars.len()];
        let char2 = problematic_chars[(i * 3) % problematic_chars.len()];

        // Create a title with the problematic characters
        let title = format!("Title{}with{}chars", char1, char2);

        // Create headline with random level (1-4 asterisks)
        let level = (i % 4) + 1;
        let asterisks = "*".repeat(level);
        let headline = format!("{} <skg<id:{}>> {}", asterisks, test_id, title);

        // Create the S-expression request
        let request = format!(
            "((request . \"containerward view\") (headline . \"{}\"))",
            headline.replace("\"", "\\\"") // Escape quotes for the S-expression
        );

        // Test the parsing
        match extract_containerward_view_params(&request) {
            Ok((node_id, parsed_level)) => {
                assert_eq!(node_id, ID(test_id.clone()),
                    "Failed to parse ID correctly for title: '{}' (headline: '{}')",
                    title, headline);
                assert_eq!(parsed_level, level,
                    "Failed to parse level correctly for title: '{}' (headline: '{}')",
                    title, headline);
            },
            Err(e) => {
                panic!("Parsing failed for title: '{}' (headline: '{}') with error: {}",
                    title, headline, e);
            }
        }
    }

    Ok(())
}

#[test]
fn test_sexp_parsing_edge_cases() -> Result<(), Box<dyn Error>> {
    // Test some specific edge cases that are likely to cause problems
    let test_cases = vec![
        ("quote_test", "*** <skg<id:quote_test>> Text with \"quoted\" content"),
        ("paren_test", "** <skg<id:paren_test>> Text with (parentheses) here"),
        ("tick_test", "* <skg<id:tick_test>> Text with `backticks` here"),
        ("mixed_test", "** <skg<id:mixed_test>> Text with \"quotes\" and (parens) and `ticks`"),
        ("space_test", "* <skg<id:space_test>> Text   with   multiple   spaces"),
    ];

    for (test_id, headline) in test_cases {
        let request = format!(
            "((request . \"containerward view\") (headline . \"{}\"))",
            headline.replace("\"", "\\\"") // Escape quotes for the S-expression
        );

        match extract_containerward_view_params(&request) {
            Ok((node_id, level)) => {
                assert_eq!(node_id, ID(test_id.to_string()),
                    "Failed to parse ID correctly for edge case: '{}'", headline);
                // Level should match the number of asterisks at start
                let expected_level = headline.chars().take_while(|&c| c == '*').count();
                assert_eq!(level, expected_level,
                    "Failed to parse level correctly for edge case: '{}'", headline);
            },
            Err(e) => {
                panic!("Parsing failed for edge case headline: '{}' with error: {}",
                    headline, e);
            }
        }
    }

    Ok(())
}
