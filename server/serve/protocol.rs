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
  RebuildDbs,
  RerenderAllViews,
  TitlesByIds,
  DiffAnalysis,
  ListSourceSets,
  ActiveSourceSet,
  SetActiveSourceSet,
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
      "rebuild dbs"              => Ok (RequestType::RebuildDbs),
      "rerender all views"       => Ok (RequestType::RerenderAllViews),
      "titles by ids"            => Ok (RequestType::TitlesByIds),
      "diff analysis"            => Ok (RequestType::DiffAnalysis),
      "list source sets"         => Ok (RequestType::ListSourceSets),
      "active source set"        => Ok (RequestType::ActiveSourceSet),
      "set active source set"    => Ok (RequestType::SetActiveSourceSet),
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
  SaveResult,
  CollateralView, // One streamed collateral-view update during save. Sent per-view between SaveLock and SaveResult.
  CloseView,
  SearchResults, // computed fast
  SearchEnrichment, // replaces SearchResults, once it's computed. Includes containerward ancestries and graphnodestats.
  RequestSnapshot, // Asks Emacs for a snapshot of one buffer (the search buffer matching the given terms), so that Rust can integrate enrichment data into the user's current edits.
  GetFilePath,
  VerifyConnection,
  Shutdown,
  GitDiffMode,
  RebuildDbs,
  RerenderLock,
  RerenderView,
  RerenderDone,
  TitlesByIds,
  DiffAnalysis,
  SourceSets,
  ActiveSourceSet,
  Error,
}

impl TcpToClient {
  pub fn repr_in_client (
    &self,
  ) -> &'static str {
    match self {
      TcpToClient::ContentView      => "content-view",
      TcpToClient::SaveLock         => "save-lock",
      TcpToClient::SaveResult       => "save-result",
      TcpToClient::CollateralView   => "collateral-view",
      TcpToClient::CloseView        => "close-view",
      TcpToClient::SearchResults    => "search-results",
      TcpToClient::SearchEnrichment => "search-enrichment",
      TcpToClient::RequestSnapshot  => "request-snapshot",
      TcpToClient::GetFilePath      => "get-file-path",
      TcpToClient::VerifyConnection => "verify-connection",
      TcpToClient::Shutdown         => "shutdown",
      TcpToClient::GitDiffMode      => "git-diff-mode",
      TcpToClient::RebuildDbs       => "rebuild-dbs",
      TcpToClient::RerenderLock     => "rerender-lock",
      TcpToClient::RerenderView     => "rerender-view",
      TcpToClient::RerenderDone     => "rerender-done",
      TcpToClient::TitlesByIds      => "titles-by-ids",
      TcpToClient::DiffAnalysis     => "diff-analysis",
      TcpToClient::SourceSets       => "source-sets",
      TcpToClient::ActiveSourceSet  => "active-source-set",
      TcpToClient::Error            => "error", }} }
