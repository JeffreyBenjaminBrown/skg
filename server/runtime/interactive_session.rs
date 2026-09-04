use crate::serve::ViewsState;
use crate::serve::handlers::collateral_scheduler::CollateralScheduler;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::SkgConfig;
use crate::types::views_state::ViewUri;

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ClientKind {
  Emacs,
  Neovim,
}

impl ClientKind {
  pub fn label (&self) -> &'static str {
    match self {
      Self::Emacs => "emacs",
      Self::Neovim => "neovim",
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClientCapabilities {
  pub archive_format_version : u32,
  pub native_undo_kind       : String,
  pub native_undo_version    : String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AttachedClient {
  pub kind          : ClientKind,
  pub version       : String,
  pub session_id    : String,
  pub capabilities  : ClientCapabilities,
  pub census_complete : bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueuedServerEvent {
  pub frame_kind   : String,
  pub operation_id : String,
  pub payload      : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CensusDescriptor {
  pub buffer_id            : String,
  pub kind                 : String,
  pub view_uri             : Option<ViewUri>,
  pub graph_generation     : u64,
  pub presentation_generation : u64,
  pub server_revision      : u64,
  pub application_token    : u64,
  pub dirty                : bool,
  pub undo_required        : bool,
  pub last_fetched_sha256  : String,
  pub current_sha256       : String,
}

impl CensusDescriptor {
  pub fn frozen_record (&self) -> Result<crate::maintenance::FrozenBufferRecord, String> {
    Ok (crate::maintenance::FrozenBufferRecord {
      buffer_id: self . buffer_id . clone (),
      kind: crate::maintenance::BufferKind::parse (&self . kind)?,
      view_uri: self . view_uri . as_ref ()
        . map (ViewUri::repr_in_client),
      graph_generation: self . graph_generation,
      presentation_generation: self . presentation_generation,
      server_revision: self . server_revision,
      application_token: self . application_token,
      dirty: self . dirty,
      undo_required: self . undo_required,
      last_fetched_sha256: self . last_fetched_sha256 . clone (),
      current_sha256: self . current_sha256 . clone (),
    })
  }
}

pub struct InteractiveSession {
  pub views                  : ViewsState,
  pub active_source_set      : ActiveSourceSet,
  pub collateral_scheduler   : CollateralScheduler,
  pub attached_client        : Option<AttachedClient>,
  pub queued_server_events   : VecDeque<QueuedServerEvent>,
  pub pending_census_texts   : HashMap<String, CensusDescriptor>,
  /// The last editor-side registry census, including records without a live
  /// view URI. Maintenance uses this as its exact lock/archive inventory.
  pub live_census            : HashMap<String, CensusDescriptor>,
}

impl InteractiveSession {
  pub fn new (config : &SkgConfig) -> Result<Self, String> {
    let active_source_set = ActiveSourceSet::default_from_config (config)
      .or_else (|_| ActiveSourceSet::named (
        config, crate::types::misc::SourceSetName::from ("all")))
      .map_err (|error| error . to_string ())?;
    Ok (Self {
      views: ViewsState {
        diff_mode_enabled: false,
        open_views: crate::types::views_state::OpenViews::new (),
      },
      active_source_set,
      collateral_scheduler: CollateralScheduler::new (),
      attached_client: None,
      queued_server_events: VecDeque::new (),
      pending_census_texts: HashMap::new (),
      live_census: HashMap::new (),
    })
  }
}
