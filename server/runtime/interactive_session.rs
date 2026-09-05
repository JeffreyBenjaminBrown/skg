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
  pub lifecycle            : String,
  pub disposable           : bool,
  pub continuation_id      : Option<String>,
  pub origin_buffer_id     : Option<String>,
  pub origin_view_uri      : Option<String>,
  pub origin_application_token : Option<u64>,
  pub origin_location      : Option<String>,
  pub view_uri             : Option<ViewUri>,
  pub recipe               : String,
  pub root_ids             : Vec<String>,
  pub source_set           : String,
  pub graph_generation     : u64,
  pub presentation_generation : u64,
  pub server_revision      : u64,
  pub application_token    : u64,
  pub dirty                : bool,
  pub logical_dirty        : bool,
  pub undo_required        : bool,
  pub maintenance_epoch    : Option<u64>,
  pub modification_tick    : u64,
  pub presentation_stale   : bool,
  pub search_stale         : bool,
  pub herald_bearing       : bool,
  pub last_fetched_sha256  : String,
  pub current_sha256       : String,
}

impl CensusDescriptor {
  pub fn frozen_record (&self) -> Result<crate::maintenance::FrozenBufferRecord, String> {
    if self . lifecycle . is_empty () {
      return Err ("census buffer lifecycle is empty" . into ()); }
    if self . source_set . is_empty () {
      return Err ("census buffer source-set is empty" . into ()); }
    if self . logical_dirty && !self . dirty {
      return Err ("logically dirty census buffer is not marked dirty" . into ()); }
    match &self . origin_buffer_id {
      Some (origin_id) => {
        if origin_id == &self . buffer_id {
          return Err ("census workflow names itself as its origin" . into ()); }
        if self . origin_application_token . is_none () {
          return Err ("census workflow has no origin application token" . into ()); }
        if self . origin_location . as_deref () . unwrap_or ("") . is_empty () {
          return Err ("census workflow has no origin location" . into ()); }
      }
      None if self . origin_view_uri . is_some ()
           || self . origin_application_token . is_some ()
           || self . origin_location . is_some () =>
      {
        return Err ("census buffer has partial origin authority" . into ());
      }
      None => {}
    }
    Ok (crate::maintenance::FrozenBufferRecord {
      buffer_id: self . buffer_id . clone (),
      kind: crate::maintenance::BufferKind::parse (&self . kind)?,
      lifecycle: self . lifecycle . clone (),
      disposable: self . disposable,
      continuation_id: self . continuation_id . clone (),
      origin_buffer_id: self . origin_buffer_id . clone (),
      origin_view_uri: self . origin_view_uri . clone (),
      origin_application_token: self . origin_application_token,
      origin_location: self . origin_location . clone (),
      view_uri: self . view_uri . as_ref ()
        . map (ViewUri::repr_in_client),
      recipe: self . recipe . clone (),
      root_ids: self . root_ids . clone (),
      source_set: self . source_set . clone (),
      graph_generation: self . graph_generation,
      presentation_generation: self . presentation_generation,
      server_revision: self . server_revision,
      application_token: self . application_token,
      dirty: self . dirty,
      logical_dirty: self . logical_dirty,
      undo_required: self . undo_required,
      maintenance_epoch: self . maintenance_epoch,
      presentation_stale: self . presentation_stale,
      search_stale: self . search_stale,
      herald_bearing: self . herald_bearing,
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
