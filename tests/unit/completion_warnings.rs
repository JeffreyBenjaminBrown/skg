use super::*;

#[test]
fn warnings_batch_per_col_and_owner () {
  let warnings : Vec<CompletionWarning> = vec! [
    CompletionWarning {
      col      : PartnerCol::Subscriber,
      owner    : ID::from ("n"),
      repair   : RepairKind::RestoredMember,
      children : vec! [ ID::from ("r"), ID::from ("t") ] },
    CompletionWarning {
      col      : PartnerCol::Subscriber,
      owner    : ID::from ("n"),
      repair   : RepairKind::DemotedNonMember,
      children : vec! [ ID::from ("x") ] },
    CompletionWarning {
      col      : PartnerCol::Hider,
      owner    : ID::from ("m"),
      repair   : RepairKind::RemovedDuplicate,
      children : vec! [ ID::from ("d") ] },
    CompletionWarning { // empty children: contributes nothing
      col      : PartnerCol::Hider,
      owner    : ID::from ("m"),
      repair   : RepairKind::RestoredMember,
      children : vec! [] },
  ];
  let rendered : Vec<String> =
    render_completion_warnings (&warnings);
  assert_eq! ( rendered . len (), 2,
    "one string per (col, owner) pair: {:?}", rendered );
  assert! ( rendered [0] . contains ("subscriberCol") );
  assert! ( rendered [0] . contains ("under node n") );
  assert! ( rendered [0] . contains ("restored 2 member(s): r, t") );
  assert! ( rendered [0] . contains ("demoted 1 non-member(s)") );
  assert! ( rendered [0] . contains ("edited from the other side"),
    "a restoration explains where membership is edited: {}",
    rendered [0] );
  assert! ( rendered [1] . contains ("hiderCol") );
  assert! ( rendered [1] . contains ("removed 1 duplicate member(s): d") );
  assert! ( ! rendered [1] . contains ("edited from the other side"),
    "no restoration, no explainer: {}", rendered [1] ); }
