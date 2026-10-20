// cargo nextest run --test grouped_repos -E 'test(diff_view::roundtrip::)'
//
// The round-trip invariant for the diff view's own viewnodes:
// each renders and parses back to itself, and a buffer holding them
// saves back unchanged with no graph change and no validation error.

mod metadata;
mod view;
