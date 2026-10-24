use super::*;

#[test]
fn warnings_batch_per_folder_and_recorder () {
  let warnings : Vec<CompletionWarning> = vec! [
    CompletionWarning::FolderRepair {
      folder      : PartnerFolder::Subscriber,
      recorder : ID::from ("n"),
      folder_viewnode_id : Some (ViewnodeId ("0a" . to_string ())),
      repair   : RepairKind::ReinstatedMember,
      children : vec! [ ID::from ("r"), ID::from ("t") ] },
    CompletionWarning::FolderRepair {
      folder      : PartnerFolder::Subscriber,
      recorder : ID::from ("n"),
      folder_viewnode_id : Some (ViewnodeId ("0a" . to_string ())),
      repair   : RepairKind::Retained,
      children : vec! [ ID::from ("x") ] },
    CompletionWarning::FolderRepair {
      folder      : PartnerFolder::Hider,
      recorder : ID::from ("m"),
      folder_viewnode_id : None,
      repair   : RepairKind::EntombstonedDuplicate,
      children : vec! [ ID::from ("d") ] },
    CompletionWarning::FolderRepair { // empty children: contributes nothing
      folder      : PartnerFolder::Hider,
      recorder : ID::from ("m"),
      folder_viewnode_id : None,
      repair   : RepairKind::ReinstatedMember,
      children : vec! [] },
  ];
  let rendered : Vec<String> =
    render_completion_warnings (&warnings);
  assert_eq! ( rendered . len (), 2,
    "one string per (folder, recorder) pair: {:?}", rendered );
  assert! ( rendered [0] . contains ("subscriberFolder (viewnode ID 0a) under node n") );
  assert! ( rendered [0] . contains ("under node n") );
  assert! ( rendered [0] . contains ("reinstated 2 member(s): r, t") );
  assert! ( rendered [0] . contains ("retained 1 child(ren) as non-members") );
  assert! ( rendered [0] . contains ("edited from the other side"),
    "a restoration explains where membership is edited: {}",
    rendered [0] );
  assert! ( rendered [1] . contains ("hiderFolder under node m"),
    "a folder without a viewnode ID is named by its kind alone: {}",
    rendered [1] );
  assert! ( rendered [1] . contains ("removed 1 duplicate member(s): d") );
  assert! ( ! rendered [1] . contains ("edited from the other side"),
    "no restoration, no explainer: {}", rendered [1] ); }
