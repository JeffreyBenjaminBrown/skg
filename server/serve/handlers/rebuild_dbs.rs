use crate::maintenance::QueuedObservationReason;
use crate::runtime::ServerRuntime;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};

use std::net::TcpStream;

/// Retain the old wire spelling as a safe migration aid, but never let an
/// unqualified control connection cross the archive/session boundary.  The
/// observation makes newly imported bytes visible as pending work while the
/// interactive command supplies the required census and recovery archive.
pub fn handle_rebuild_dbs_request (
  stream  : &mut TcpStream,
  runtime : &ServerRuntime,
) {
  let observation = runtime . schedule_full_observation (
    QueuedObservationReason::ClientHint);
  let message = match observation {
    Ok (( )) if runtime . interactive_slot . attached () =>
      "Direct rebuild requests cannot bypass interactive recovery. Disk observation was queued; run M-x skg-rebuild-dbs or :SkgRebuildDbs in the attached client.",
    Ok (( )) =>
      "Direct rebuild requests cannot bypass recovery maintenance. Disk observation was queued; attach a client and run M-x skg-rebuild-dbs or :SkgRebuildDbs.",
    Err (_) if runtime . interactive_slot . attached () =>
      "Direct rebuild requests cannot bypass interactive recovery. Run M-x skg-rebuild-dbs or :SkgRebuildDbs in the attached client.",
    Err (_) =>
      "Direct rebuild requests cannot bypass recovery maintenance. Attach a client and run M-x skg-rebuild-dbs or :SkgRebuildDbs.",
  };
  let _ = send_response_with_length_prefix (
    stream, &tag_text_response (TcpToClient::RebuildDbs, message));
}
