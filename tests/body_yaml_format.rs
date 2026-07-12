// cargo nextest run --test grouped_unit -E 'test(body_yaml_format::)'
//
// Tests that NodeFS::to_yaml emits the 'body' field as a YAML block
// literal (readable in diffs) even when the body contains tabs, and
// that the output round-trips exactly via serde_yaml::from_str.
// Also verifies field order is unchanged from the default serde_yaml
// rendering.

use skg::types::nodes::fs::NodeFS;
use skg::telescope::types::ListItem;
use skg::types::misc::ID;

fn node_with_body (
  body : &str,
) -> NodeFS {
  NodeFS {
    title                        : Some ("t" . to_string ()),
    aliases                      : vec! [],
    pid                          : ID::new ("p"),
    extra_ids                    : vec! [],
    body                         : Some (body . to_string ()),
    contains                     : vec! [],
    subscribes_to                : vec! [],
    hides_from_its_subscriptions : vec! [],
    overrides_view_of            : vec! [],
    misc                         : vec! [],
  } }

fn assert_roundtrip (
  label : &str,
  body  : &str,
) {
  let node : NodeFS = node_with_body (body);
  let yaml : String = node . to_yaml () . expect ("serializes");
  let back : NodeFS =
    serde_yaml::from_str (&yaml)
    . unwrap_or_else (|e| panic! (
      "{}: re-parse failed: {}\n----\n{}\n----", label, e, yaml));
  assert_eq! (
    back . body . as_deref (),
    Some (body),
    "{}: body did not round-trip. YAML:\n{}", label, yaml);
}

#[test]
fn block_scalar_for_body_without_tabs () {
  let body : &str = "line one\nline two\nline three";
  let node : NodeFS = node_with_body (body);
  let yaml : String = node . to_yaml () . unwrap ();
  assert! (
    yaml . contains ("body: |"),
    "expected block-literal body indicator, got:\n{}", yaml);
  assert! (
    ! yaml . contains (r"\n"),
    "unexpected backslash-n escape in output:\n{}", yaml); }

#[test]
fn block_scalar_for_body_with_tabs () {
  // Regression guard: serde_yaml's default emitter falls back to a
  // double-quoted scalar (with \n escapes) whenever the body contains
  // a tab character. NodeFS::to_yaml must avoid that fallback.
  let body : &str =
    "  prose line one\n\t(elisp-code)\n  prose line two";
  let node : NodeFS = node_with_body (body);
  let yaml : String = node . to_yaml () . unwrap ();
  assert! (
    ! yaml . contains (r"\n"),
    "body with tabs was emitted as a quoted scalar with \\n escapes:\n{}", yaml);
  assert! (
    yaml . contains ("body: |"),
    "expected block-literal body indicator, got:\n{}", yaml); }

#[test]
fn body_round_trips_across_many_shapes () {
  assert_roundtrip ("single line",       "just one line");
  assert_roundtrip ("trailing newline",  "a\nb\n");
  assert_roundtrip ("blank line inside", "a\n\nb");
  assert_roundtrip ("tabs mid-line",     "line1\n\tindented\nline3");
  assert_roundtrip ("leading space",     "  prefix two spaces\nsecond");
  assert_roundtrip ("mixed",             "  https://x/y\n\t(elisp)\n  more");
  assert_roundtrip ("literal backslash", r"contains \n as two chars"); }

#[test]
fn field_order_places_body_between_extra_ids_and_contains () {
  // 'body' sits between 'extra_ids' and 'contains' in NodeFS. The
  // to_yaml helper must preserve that ordering even though it takes
  // a custom path for 'body'.
  let node : NodeFS = NodeFS {
    title                        : Some ("t" . to_string ()),
    aliases                      : vec! [],
    pid                          : ID::new ("p"),
    extra_ids                    : vec! [ ID::new ("x1"),
                                          ID::new ("x2") ],
    body                         : Some (
      "multi\nline\n\tbody" . to_string ()),
    contains                     : vec! [
      ListItem::Member ( ID::new ("c1") ) ],
    subscribes_to                : vec! [],
    hides_from_its_subscriptions : vec! [],
    overrides_view_of            : vec! [],
    misc                         : vec! [],
  };
  let yaml : String = node . to_yaml () . unwrap ();
  let extra_ids_pos : usize =
    yaml . find ("extra_ids:")
    . expect ("extra_ids field present");
  let body_pos : usize =
    yaml . find ("body:")
    . expect ("body field present");
  let contains_pos : usize =
    yaml . find ("contains:")
    . expect ("contains field present");
  assert! (
    extra_ids_pos < body_pos && body_pos < contains_pos,
    "field order is wrong:\n{}", yaml); }

#[test]
fn body_with_only_literal_backslash_n_not_newlines () {
  // WYSIWYG check: a body that literally contains backslash + 'n'
  // (two chars) must come back exactly — block scalar must not
  // interpret it as a newline (that would be YAML double-quoted
  // behavior, which we're avoiding).
  let body : &str = "foo\\nbar\\nbaz";
  let node : NodeFS = node_with_body (body);
  let yaml : String = node . to_yaml () . unwrap ();
  let back : NodeFS = serde_yaml::from_str (&yaml) . unwrap ();
  assert_eq! (back . body . as_deref (), Some (body));
  // The file should contain the literal bytes on disk.
  assert! (
    yaml . contains (r"foo\nbar\nbaz"),
    "expected literal backslash-n bytes in YAML:\n{}", yaml); }
