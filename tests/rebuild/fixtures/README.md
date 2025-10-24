# Test Fixtures for integrate_backpath tests

These are minimal fixtures used for testing the `integrate_path_that_might_fork_or_cycle` function.

Each file (0.skg through 4.skg) contains only:
- An ID matching its filename
- A title matching its ID

No other fields (body, contains, subscribes_to, etc.) are included.

These simple fixtures are sufficient because the tests focus on how the integration logic constructs tree structures from path data, not on loading complex node relationships from disk.
