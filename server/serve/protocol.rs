/// Enum of all request types the server recognizes.
/// The client format is a string in the s-exp
/// ((request . "single root content view") ...).
/// Emacs sends these strings; the server parses them here.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RequestType {
  SingleRootContentView,
  SaveBuffer,
  CloseView,
  TitleMatches,
  VerifyConnection,
  Shutdown,
  GetFilePath,
  GitDiffModeToggle,
}

impl RequestType {
  pub fn from_client_string (
    s : &str,
  ) -> Result<RequestType, String> {
    match s {
      "single root content view" => Ok (RequestType::SingleRootContentView),
      "save buffer"              => Ok (RequestType::SaveBuffer),
      "close view"               => Ok (RequestType::CloseView),
      "title matches"            => Ok (RequestType::TitleMatches),
      "verify connection"        => Ok (RequestType::VerifyConnection),
      "shutdown"                 => Ok (RequestType::Shutdown),
      "get file path"            => Ok (RequestType::GetFilePath),
      "git diff mode toggle"     => Ok (RequestType::GitDiffModeToggle),
      other => Err (format! ("Unsupported request type: {}", other)), }} }

/// IN DETAIL: See api-and-formats.md
///
/// IN BRIEF: Enum of all response types the server sends.
/// The client format is a string in the s-exp
/// (("response-type" "save-result") ...).
/// Emacs dispatches on these strings.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResponseType {
  ContentView,
  SaveLock,
  SaveResult,
  CloseView,
  SearchResults,
  SearchEnrichment,
  GetFilePath,
  VerifyConnection,
  Shutdown,
  GitDiffMode,
  Error,
}

impl ResponseType {
  pub fn repr_in_client (
    &self,
  ) -> &'static str {
    match self {
      ResponseType::ContentView      => "content-view",
      ResponseType::SaveLock         => "save-lock",
      ResponseType::SaveResult       => "save-result",
      ResponseType::CloseView        => "close-view",
      ResponseType::SearchResults    => "search-results",
      ResponseType::SearchEnrichment => "search-enrichment",
      ResponseType::GetFilePath      => "get-file-path",
      ResponseType::VerifyConnection => "verify-connection",
      ResponseType::Shutdown         => "shutdown",
      ResponseType::GitDiffMode      => "git-diff-mode",
      ResponseType::Error            => "error", }} }
