/// See api-and-formats.org for details.
/// Enum of all request types the server recognizes.
/// The client format is a string in the s-exp
/// ((request . "single root content view") ...).
/// Emacs sends these strings; the server parses them here.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RequestType {
  SingleRootContentView,
  SaveBuffer,
  CloseView,
  TextSearch,
  SnapshotResponse,
  VerifyConnection,
  Shutdown,
  GetFilePath,
  GitDiffModeToggle,
  HeraldRules,
  RebuildEphemeralDataStores,
  RerenderAllViews,
  StripBodyWhitespace,
  TitlesByIds,
  DiffAnalysis,
  StageMoves,
  EdgeSourceInfo,
  ListSourceSets,
  ActiveSourceSet,
  SetActiveSourceSet,
  ExportToOrg,
  DeleteReferencesToAbsentNode,
}

impl RequestType {
  pub fn from_client_string (
    s : &str,
  ) -> Result<RequestType, String> {
    match s {
      "single root content view" => Ok (RequestType::SingleRootContentView),
      "save buffer"              => Ok (RequestType::SaveBuffer),
      "close view"               => Ok (RequestType::CloseView),
      "text search"              => Ok (RequestType::TextSearch),
      "snapshot response"        => Ok (RequestType::SnapshotResponse),
      "verify connection"        => Ok (RequestType::VerifyConnection),
      "shutdown"                 => Ok (RequestType::Shutdown),
      "get file path"            => Ok (RequestType::GetFilePath),
      "git diff mode toggle"     => Ok (RequestType::GitDiffModeToggle),
      "herald rules"             => Ok (RequestType::HeraldRules),
      "rebuild ephemeral data stores"              => Ok (RequestType::RebuildEphemeralDataStores),
      "rerender all views"       => Ok (RequestType::RerenderAllViews),
      "strip body whitespace"    => Ok (RequestType::StripBodyWhitespace),
      "titles by ids"            => Ok (RequestType::TitlesByIds),
      "diff analysis"            => Ok (RequestType::DiffAnalysis),
      "stage moves"              => Ok (RequestType::StageMoves),
      "edge source info"          => Ok (RequestType::EdgeSourceInfo),
      "list source sets"         => Ok (RequestType::ListSourceSets),
      "active source set"        => Ok (RequestType::ActiveSourceSet),
      "set active source set"    => Ok (RequestType::SetActiveSourceSet),
      "export to org"            => Ok (RequestType::ExportToOrg),
      "delete references to absent node" => Ok (RequestType::DeleteReferencesToAbsentNode),
      other => Err (format! ("Unsupported request type: {}", other)), }} }

/// IN DETAIL: See api-and-formats.md
///
/// IN BRIEF: Enum of all message types the server sends to the client.
/// The client format is a string in the s-exp
/// (("response-type" "save-result") ...).
/// Emacs dispatches on these strings.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TcpToClient {
  ContentView,
  SaveLock, // Sent before the expensive save pipeline. Lists collateral view URIs so Emacs can lock those buffers against edits while the save is in progress.
  SaveRelaxLock, // Sent after the SavePlan is computed and the graph updated, before the collateral-view stream. Lists the now-narrowed still-locked set (the EXACT collateral set), symmetric with SaveLock, so Emacs unlocks every buffer it locked early that turned out not to be collateral. Lets the user edit those during the rest of the pipeline (TODO/DONE/local-view-update/plan_v2.org §8.1).
  SaveResult,
  ForkConfirmation, // Terminal message of a save that found fork candidates and was not pre-approved: a read-only buffer listing the foreign nodes about to be forked, for the user to approve (re-issue the save with (fork-approved . "true")) or decline. Sent after SaveLock, in place of SaveResult; nothing is committed.
  TelescopeHoistConfirmation, // Terminal message of a save whose current disk inputs select title/body below home. Carries only pid/home pairs and a publication warning; an approved retry carries the exact pids. Nothing is committed.
  OverPrivateTextTelescopeConfirmation, // A textual response would expose title/body selected below home under a restricted source-set. Carries only the operation, affected pids, and a prompt; the client may retry with an explicit per-pid approval.
  CollateralView, // One streamed collateral-view update during save. Sent per-view between SaveLock and SaveResult.
  CloseView,
  SearchResults, // computed fast
  SearchEnrichment, // replaces SearchResults, once it's computed. Includes containerward ancestries and graphnodestats.
  RequestSnapshot, // Asks Emacs for a snapshot of one buffer (the search buffer matching the given terms), so that Rust can integrate enrichment data into the user's current edits.
  GetFilePath,
  HeraldRules,
  VerifyConnection,
  Shutdown,
  GitDiffMode,
  RebuildEphemeralDataStores,
  RerenderLock,
  RerenderView,
  RerenderDone,
  StripBodyWhitespace,
  TitlesByIds,
  DiffAnalysis,
  StageMoves,
  EdgeSourceInfo,
  SourceSets,
  ActiveSourceSet,
  ExportToOrg,
  DeleteReferencesConfirmation,
  DeleteReferencesResult,
  Error,
}

impl TcpToClient {
  pub fn repr_in_client (
    &self,
  ) -> &'static str {
    match self {
      TcpToClient::ContentView      => "content-view",
      TcpToClient::SaveLock         => "save-lock",
      TcpToClient::SaveRelaxLock    => "save-relax-lock",
      TcpToClient::SaveResult       => "save-result",
      TcpToClient::ForkConfirmation => "fork-confirmation",
      TcpToClient::TelescopeHoistConfirmation =>
        "telescope-hoist-confirmation",
      TcpToClient::OverPrivateTextTelescopeConfirmation =>
        "overPrivateText-telescope-confirmation",
      TcpToClient::CollateralView   => "collateral-view",
      TcpToClient::CloseView        => "close-view",
      TcpToClient::SearchResults    => "search-results",
      TcpToClient::SearchEnrichment => "search-enrichment",
      TcpToClient::RequestSnapshot  => "request-snapshot",
      TcpToClient::GetFilePath      => "get-file-path",
      TcpToClient::HeraldRules      => "herald-rules",
      TcpToClient::VerifyConnection => "verify-connection",
      TcpToClient::Shutdown         => "shutdown",
      TcpToClient::GitDiffMode      => "git-diff-mode",
      TcpToClient::RebuildEphemeralDataStores       => "rebuild-ephemeral-data-stores",
      TcpToClient::RerenderLock     => "rerender-lock",
      TcpToClient::RerenderView     => "rerender-view",
      TcpToClient::RerenderDone     => "rerender-done",
      TcpToClient::StripBodyWhitespace => "strip-body-whitespace",
      TcpToClient::TitlesByIds      => "titles-by-ids",
      TcpToClient::DiffAnalysis     => "diff-analysis",
      TcpToClient::StageMoves       => "stage-moves",
      TcpToClient::EdgeSourceInfo    => "edge-source-info",
      TcpToClient::SourceSets       => "source-sets",
      TcpToClient::ActiveSourceSet  => "active-source-set",
      TcpToClient::ExportToOrg      => "export-to-org",
      TcpToClient::DeleteReferencesConfirmation => "delete-references-confirmation",
      TcpToClient::DeleteReferencesResult => "delete-references-result",
      TcpToClient::Error            => "error", }} }
