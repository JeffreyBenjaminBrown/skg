use skg::serve::handlers::save_buffer::parse_save_request_envelope;
use skg::types::views_state::ViewUri;

#[test]
fn accepts_exact_dirty_snapshots_and_explicit_empty_views () {
  let saved_uri : ViewUri = ViewUri::ContentView ("saved" . into ());
  let empty = parse_save_request_envelope (
    "((saved-buffer \"* root\n\") (other-views ()))",
    Some (&saved_uri)) . unwrap ();
  assert_eq! (empty . saved_buffer, "* root\n");
  assert! (empty . other_views . is_empty ());

  let parsed = parse_save_request_envelope (
    "((saved-buffer \"λ\n\") (other-views (((view-uri \"other\") (dirty true) (baseline (present \"before\n\")) (current \"after\tλ\")) ((view-uri \"newer\") (dirty true) (baseline unavailable) (current \"new\")) ((view-uri \"clean\") (dirty false)))))",
    Some (&saved_uri)) . unwrap ();
  assert_eq! (parsed . saved_buffer, "λ\n");
  assert_eq! (parsed . other_views . len (), 3);
  assert_eq! (parsed . other_views [0] . baseline . as_deref (),
              Some ("before\n"));
  assert_eq! (parsed . other_views [0] . current . as_deref (),
              Some ("after\tλ"));
  assert_eq! (parsed . other_views [1] . baseline, None);
  assert! (! parsed . other_views [2] . dirty);
}

#[test]
fn rejects_legacy_and_ambiguous_envelopes () {
  let saved_uri : ViewUri = ViewUri::ContentView ("saved" . into ());
  for malformed in [
    "* legacy raw Org",
    "((saved-buffer \"text\"))",
    "((saved-buffer \"text\") (other-views ()) (other-views ()))",
    "((saved-buffer \"text\") (other-views ()) (surprise true))",
    "((saved-buffer \"text\") (other-views (((view-uri \"saved\") (dirty false)))))",
    "((saved-buffer \"text\") (other-views (((view-uri \"other\") (dirty false)) ((view-uri \"other\") (dirty false)))))",
    "((saved-buffer \"text\") (other-views (((view-uri \"other\") (dirty perhaps)))))",
    "((saved-buffer \"text\") (other-views (((view-uri \"other\") (dirty true) (baseline unavailable)))))",
    "((saved-buffer \"text\") (other-views (((view-uri \"other\") (dirty false) (current \"stale\")))))",
    "((saved-buffer \"text\") (other-views (((view-uri \"other\") (dirty true) (baseline (bogus \"old\")) (current \"new\")))))",
  ] {
    assert! (parse_save_request_envelope (
      malformed, Some (&saved_uri)) . is_err (),
      "should reject: {}", malformed); }
}
