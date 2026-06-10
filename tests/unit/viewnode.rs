use super::*;

// 'PartnerCol::policy' is the single source of truth for how each
// col's membership relates to user edits; this pins the mapping.
#[test]
fn partnerCol_policy_mapping () {
  assert_eq! ( PartnerCol::Subscribee . policy (),
               ColPolicy::WritableSet );
  assert_eq! ( PartnerCol::Overridden . policy (),
               ColPolicy::WritableSet );
  assert_eq! ( PartnerCol::Subscriber . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::Overrider . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::Hider . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::Hidden . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::HiddenInSubscribee . policy (),
               ColPolicy::ReadOnlyFilter );
  assert_eq! ( PartnerCol::HiddenOutsideOfSubscribee . policy (),
               ColPolicy::ReadOnlyFilter ); }
