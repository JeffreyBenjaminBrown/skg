use crate::dbs::typedb::util::delete_database;
use crate::types::SkgConfig;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

/// Performs cleanup before server shutdown.
/// Deletes the database if delete_on_quit is configured, then exits.
pub fn cleanup_and_shutdown (
  typedb_driver : &Arc<TypeDBDriver>,
  config        : &SkgConfig,
) {
  if config . delete_on_quit {
    println! (
      "Deleting database '{}' before shutdown...",
      config . db_name );

    // Wait briefly to allow any pending operations to complete.
    // This helps ensure the database isn't marked as "in use".
    std::thread::sleep (
      std::time::Duration::from_millis ( 100 ) );

    futures::executor::block_on ( async {
      if let Err ( e ) =
        delete_database (
          typedb_driver, & config . db_name )
        . await {
          eprintln! ( "Failed to delete database: {}", e );
        }} ); }
  println! ( "Shutdown complete." );
  std::process::exit (0); }
