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
  HeraldRules,
  RebuildSessionLifetimeStores,
  RerenderAllViews,
  StripBodyWhitespace,
  TitlesByIds,
  LinkStatuses,
  RelRepoInfo,
  FlagState,
  ListSkgrepoSets,
  SkgrepoRestriction,
  SetSkgrepoRestriction,
  ExportToOrg,
  ImportMdAndOrg,
  DeleteReferencesToAbsentNode,
  TotalDiffView,
  StageAndCommit,
  ShowUnshownChanges,
  InvestigateUnknownNode,
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
      "herald rules"             => Ok (RequestType::HeraldRules),
      "rebuild session-lifetime stores"              => Ok (RequestType::RebuildSessionLifetimeStores),
      "rerender all views"       => Ok (RequestType::RerenderAllViews),
      "strip body whitespace"    => Ok (RequestType::StripBodyWhitespace),
      "titles by ids"            => Ok (RequestType::TitlesByIds),
      "link statuses"            => Ok (RequestType::LinkStatuses),
      "relRepo info"          => Ok (RequestType::RelRepoInfo),
      "flag state"            => Ok (RequestType::FlagState),
      "list repo sets"         => Ok (RequestType::ListSkgrepoSets),
      "skgrepo restriction"    => Ok (RequestType::SkgrepoRestriction),
      "set skgrepo restriction" => Ok (RequestType::SetSkgrepoRestriction),
      "export to org"            => Ok (RequestType::ExportToOrg),
      "import md and org"         => Ok (RequestType::ImportMdAndOrg),
      "delete references to absent node" => Ok (RequestType::DeleteReferencesToAbsentNode),
      "total diff view"          => Ok (RequestType::TotalDiffView),
      "stage and commit"         => Ok (RequestType::StageAndCommit),
      "show unshown changes"     => Ok (RequestType::ShowUnshownChanges),
      "investigate unknown node" => Ok (RequestType::InvestigateUnknownNode),
      other => Err (format! ("Unsupported request type: {}", other)), }} }

/// IN DETAIL: See docs/api-and-formats.org
///
/// IN BRIEF: Enum of all message types the server sends to the client.
/// The client format is a string in the s-exp
/// (("response-type" "save-result") ...).
/// Emacs dispatches on these strings.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TcpToClient {
  ContentView,
  SaveLock, // Sent before the expensive save pipeline. Acknowledges the clients' broad lock of all local views; its server-known view ID list does not authorize narrowing.
  SaveRelaxLock, // Sent after preparation and the dirty-view conflict check, before mutation. Lists collateral targets plus every dirty conflict-check input. The client also retains the saved view, and may unlock unrelated clean views.
  SaveResult,
  ForkConfirmation, // Terminal message of a save that found fork candidates and was not pre-approved: a write-protected buffer listing the foreign nodes about to be forked, for the user to approve (re-issue the save with (approved-forks . "true")) or decline. Sent after SaveLock, in place of SaveResult; nothing is committed.
  StrandConfirmation, // Terminal message of a save whose home moves would strand relationships at their old, more private skgrepos, when the request carried no review of them. Lists each one; the client re-saves with (strands-reviewed . "true") and the chosen (relevel-strands (RELATION RECORDER MEMBER) ...). Nothing is committed.
  TelescopeHoistConfirmation, // Terminal message of a save whose current disk inputs select title/body below home. Carries only pid/home pairs and a publication warning; an approved retry carries the exact pids. Nothing is committed.
  OverPrivateTextTelescopeConfirmation, // A textual response would expose title/body selected below home under a restricted skgrepo-set. Carries only the operation, affected pids, and a prompt; the client may retry with an explicit per-pid approval.
  CollateralView, // One streamed collateral-view update during save. Sent per-view between SaveLock and SaveResult.
  CloseView,
  SearchResults, // computed fast
  SearchEnrichment, // replaces SearchResults, once it's computed. Includes containerward role trees and graphnodestats.
  RequestSnapshot, // Asks Emacs for a snapshot of one buffer (the search buffer matching the given terms), so that Rust can integrate enrichment data into the user's current edits.
  GetFilePath,
  HeraldRules,
  VerifyConnection,
  Shutdown,
  RebuildSessionLifetimeStores,
  RerenderLock,
  RerenderView,
  RerenderDone,
  StripBodyWhitespace,
  TitlesByIds,
  LinkStatuses,
  RelRepoInfo,
  FlagState,
  SkgrepoSets,
  SkgrepoRestriction,
  ExportToOrg,
  ImportMdAndOrgHostMappingNeeded,
  ImportMdAndOrgPreview,
  ImportMdAndOrgResult,
  DeleteReferencesConfirmation,
  DeleteReferencesResult,
  TotalDiffView, // One buffer of the total diff view, or '(empty . t)' when that buffer would show nothing.
  StageAndCommit, // The per-skgrepo git-commits, or the readiness summary with '(needs-confirmation . t)'. A rerender stream follows.
  ShowUnshownChanges, // The readiness summary of the buffer of the total diff view redrawn with every change shown, or a refusal. A rerender stream follows.
  InvestigateUnknownNode, // What git history says an unknown node used to be, or the node an ID resolves to, as Org text.
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
      TcpToClient::StrandConfirmation =>
        "strand-confirmation",
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
      TcpToClient::RebuildSessionLifetimeStores       => "rebuild-session-lifetime-stores",
      TcpToClient::RerenderLock     => "rerender-lock",
      TcpToClient::RerenderView     => "rerender-view",
      TcpToClient::RerenderDone     => "rerender-done",
      TcpToClient::StripBodyWhitespace => "strip-body-whitespace",
      TcpToClient::TitlesByIds      => "titles-by-ids",
      TcpToClient::LinkStatuses     => "link-statuses",
      TcpToClient::RelRepoInfo    => "relRepo-info",
      TcpToClient::FlagState     => "flag-state",
      TcpToClient::SkgrepoSets       => "repo-sets",
      TcpToClient::SkgrepoRestriction => "skgrepo-restriction",
      TcpToClient::ExportToOrg      => "export-to-org",
      TcpToClient::ImportMdAndOrgHostMappingNeeded => "import-md-and-org-host-mapping-needed",
      TcpToClient::ImportMdAndOrgPreview => "import-md-and-org-preview",
      TcpToClient::ImportMdAndOrgResult => "import-md-and-org-result",
      TcpToClient::DeleteReferencesConfirmation => "delete-references-confirmation",
      TcpToClient::DeleteReferencesResult => "delete-references-result",
      TcpToClient::TotalDiffView    => "total-diff-view",
      TcpToClient::StageAndCommit   => "stage-and-commit",
      TcpToClient::ShowUnshownChanges => "show-unshown-changes",
      TcpToClient::InvestigateUnknownNode => "investigate-unknown-node",
      TcpToClient::Error            => "error", }} }
