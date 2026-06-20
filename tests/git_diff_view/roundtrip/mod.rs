/// Git diff view round-trip tests: a buffer the server RENDERS in diff mode
/// must PASS save-validation, since the user can save to refresh the view.
///
/// Two scenarios that used to render a buffer the save then rejected:
/// - a contains member REORDERED within one parent (its old slot used to come
///   out as a second live vognode -- 'newM' -- tripping the content-child
///   uniqueness check; it must be a 'removedM' phantom);
/// - a contains member referenced at HEAD whose .skg file exists in no source
///   (deleted by an earlier commit), which renders with the NOT_FOUND source
///   sentinel (validate_phantom used to reject that).

mod common;
mod view;
