// The post-save and skgrepo-set-switch constructors of
// 'RerenderOptions' give the values those rerenders passed to
// 'rerender_view' as positional bools: the saved view substitutes
// existing content overrides and creates no partner folders, a
// collateral view does neither, and the switch creates partner
// folders. None of them integrates unshown changes, and each keeps
// the buffer kind it is given.

use super::*;

#[test]
fn rerender_options_for_post_save_and_skgrepo_switch_carry_the_old_values () {
  for diffView_buffer in [ None, Some (DiffViewBufferKind::AllChanges) ] {
    assert_eq! (
      RerenderOptions::for_saved_view (diffView_buffer),
      RerenderOptions {
        create_partnerFolders                 : false,
        substitute_existing_content_overrides : true,
        diffView_buffer,
        integrate_unshown_changes             : false, } );
    assert_eq! (
      RerenderOptions::ordinary (diffView_buffer),
      RerenderOptions {
        create_partnerFolders                 : false,
        substitute_existing_content_overrides : false,
        diffView_buffer,
        integrate_unshown_changes             : false, },
      "a collateral view of a save" );
    assert_eq! (
      RerenderOptions::for_skgrepo_switch (diffView_buffer),
      RerenderOptions {
        create_partnerFolders                 : true,
        substitute_existing_content_overrides : false,
        diffView_buffer,
        integrate_unshown_changes             : false, } ); }
}
