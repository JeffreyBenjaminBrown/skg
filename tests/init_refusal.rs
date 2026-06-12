// cargo nextest run --test grouped_unit -E 'test(init_refusal::)'
//
// The init/rebuild refusal wiring
// (TODO/full-schema/13_test-rel-matrix.org): override invariants are
// validated not only at their own seam (tests/dbs/in_rust_graph/
// override_invariants.rs) but actually BLOCK initialization. This
// drives the cheapest real entry point that server start and "rebuild
// dbs" share -- 'wipe_then_init_typedb_db' -- which calls
// 'error_unless_override_invariants_hold' before it touches TypeDB.
// Bad disk data (one node with two user-owned overriders) is refused
// with an error naming the collision; no database is created.

use std::error::Error;

use futures::executor::block_on;
use skg::consts::TYPEDB_ADDRESS;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::init::wipe_then_init_typedb_db;
use skg::types::misc::SkgConfig;
use skg::types::nodes::complete::NodeComplete;
use typedb_driver::{
  TypeDBDriver, Addresses, Credentials, DriverOptions, DriverTlsConfig};

#[test]
fn init_refuses_two_user_owned_overriders
  () -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let config : SkgConfig =
      load_config_with_overrides (
        "tests/init_refusal/fixtures/skgconfig.toml",
        Some ("skg-test-init-refusal"),
        &[] ) ?;
    let nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources (&config) ?;
    let driver : TypeDBDriver = TypeDBDriver::new (
      Addresses::try_from_address_str (TYPEDB_ADDRESS) ?,
      Credentials::new ("admin", "password"),
      DriverOptions::new (DriverTlsConfig::disabled ()) ) . await ?;
    // The invariant check runs before any TypeDB mutation, so this
    // refuses without creating the database.
    let result : Result<(), Box<dyn Error>> =
      wipe_then_init_typedb_db (&config, &driver, &nodes) . await;
    let err : Box<dyn Error> = match result {
      Ok (())  => panic! (
        "init must refuse a node with two user-owned overriders"),
      Err (e) => e, };
    let msg : String = err . to_string ();
    assert! (
      msg . contains ("overridden by user-owned")
        && msg . contains ("N")
        && msg . contains ("R1")
        && msg . contains ("R2"),
      "the refusal must name the collision (node N, overriders R1/R2): {}",
      msg );
    Ok (( )) } ) }
