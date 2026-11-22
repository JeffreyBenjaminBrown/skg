//! The errors a save reports to the user: 'SaveError',
//! 'BufferValidationError' (what makes a buffer unsavable), stale
//! graphnodes ('StaleGraphnodeReport'), and 'LinkParseError'. Also how
//! a message names a headline ('HeadlineName', 'org_subheadline').
//! Each error has two texts: a line ('Display') and the Org message
//! the client shows ('format_save_error_as_org', at the end of this
//! file). Finding buffer errors is 'from_text/validate.rs' and the
//! rest of 'from_text/'.

use super::misc::{ID, SkgrepoName};
use crate::diff_view::named_leaf_set::NamedLeafSet;
use super::viewnode::ViewnodeId;
use std::error::Error;
use std::fmt;
use std::io;

/// What a message says of one headline: its title, the graphnode ID it
/// carries, and its viewnode ID. A message names it as an org
/// subheadline ('org_subheadline'). No message invents a viewnode ID.
#[derive (Debug, Clone, PartialEq, Eq, Hash)]
pub struct HeadlineName {
  pub title        : String, // empty when the headline has none
  pub graphnode_id : Option<ID>, // None for a folder, a property, a tombstone, or a typed headline without one
  pub viewnode_id  : Option<ViewnodeId>, // None for a headline typed, or copied, since the last render
}

/// Which merge node of a merge pair a graphnode is.
#[derive (Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeMergeRole {
  Acquirer,
  Acquiree,
}

/// How a graphnode the save would write changed after its buffer was
/// rendered: what the graph holds now, against the buffer's baseline
/// copy of it.
#[derive (Debug, Clone, PartialEq)]
pub enum StaleGraphnodeReport {
  Deleted, // the graph no longer has the graphnode
  Changed {
    title      : String, // the graph's title of it now
    aspects    : Vec<StaleAspect>, // only those that differ
    body_lines : Vec<String>, // the body's changed lines, baseline against graph, each prefixed "- " or "+ "; empty when the body did not change
  }, }

/// One aspect of a stale graphnode that differs between a buffer's
/// baseline and the graph: title, body, content, aliases, extra IDs,
/// flags, forks, hides or home skgrepo. These are
/// the stale error's own grain, not the diff aspects of change-forest
/// leaves ('NodeAspect') nor the diff view's 'ChangedAspects': this
/// says "body" where those say "text", and names a relationship list
/// whole where those name each relationship.
#[derive (Debug, Clone, PartialEq)]
pub struct StaleAspect {
  pub name   : &'static str, // as the one-line message names it, e.g. "title"
  pub detail : Option<String>, // what the org message adds in parentheses, e.g. the old and new values
}

#[derive (Debug)]
pub enum LinkParseError {
  InvalidFormat,
  MissingDivider,
}

#[derive (Debug)]
pub enum SaveError {
  ParseError (String),
  DatabaseError(Box<dyn Error>),
  IoError (io::Error),
  // A failed save carries, alongside its errors, the nonfatal
  // warnings collected before the abort (e.g. discarded folder-headline
  // text). Warnings always accompany errors.
  BufferValidationErrors {
    errors   : Vec<BufferValidationError>,
    warnings : Vec<String>, }, }

/// If the user attempts to save a buffer
/// with any of these flags, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
pub enum BufferValidationError {
  Body_of_NonVognode               (String,   // Title from buffer
                                  String,   // Non-vognode kind (e.g. "aliasFolder", "alias")
                                  HeadlineName),
  Multiple_Defining_Viewnodes     (ID, Vec<HeadlineName>), // For any given ID, at most one occurrence can be editable. (Its contents are intended to define those of the node.) The names are the editable occurrences'.
  AmbiguousDeletion              (ID, Vec<HeadlineName>), // The names are the editable occurrences', with and without the delete request.
  ContradictoryProcessedRequests (NamedLeafSet, Vec<HeadlineName>), // The change leaves that one headline requests processed and another unprocessed. The names are those headlines'.
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  ModifiedForeignNode            (ID, SkgrepoName), // Attempted to modify a node from a foreign skgrepo - (gnid, repo_name)
  CreatedForeignNode             (ID, SkgrepoName), // Attempted to create a node in a foreign skgrepo - (gnid, repo_name)
  CannotMoveToOrFromForeignSkgrepo (ID,
                                   SkgrepoName, // disk skgrepo
                                   SkgrepoName), // buffer skgrepo
  CannotMoveAndMergeSimultaneously (ID),
  CannotDeleteAndMergeSimultaneously {
    pid   : ID,            // the pid of the merge node the buffer deletes
    title : String,        // its title in the graph
    role  : NodeMergeRole, // which merge node of its merge pair it is
  }, // The buffer deletes the acquirer or the acquiree of one of its own merge requests.
  SkgrepoNotInConfig              (ID, SkgrepoName),
  // Fork errors. Editing a foreign node N is read as a request to
  // clone it (the clone C lives in an owned skgrepo and forks N).
  // These are the ways that request can be refused.
  ForkSkgrepoUnresolved           (ID), // N's pid: no owned vognode ancestor in the view to inherit C's skgrepo from, and the user set none in the confirmation buffer.
  ForkAlreadyExists              (ID,   // N's pid
                                  ID),  // a note the user owns that already forks N: the fork operation's guard ('validate_fork_specs_in_graph')
  ForkSkgrepoRestricted             (ID,           // N's pid
                                  SkgrepoName),  // C's resolved owned skgrepo, which is restricted under the skgrepo restriction
  ForkSkgrepoNotOwned             (ID,           // N's pid
                                  SkgrepoName),  // C's chosen skgrepo, which the user does not own (a typed or hand-edited skgrepo the rotation would never offer)
  RecordSkgrepoRestricted        (ID,           // the recorder's pid
                                  String,       // the relation, as the file names it ("contains")
                                  String,       // the member: an ID, or an alias's text
                                  SkgrepoName), // the record's relRepo, which the skgrepo restriction keeps out
  RecordMovedOutOfRestrictedSkgrepo (ID,        // the recorder's pid
                                  String,       // the relation, as the file names it ("contains")
                                  String,       // the member's ID
                                  SkgrepoName), // the relRepo the graph holds the record at, which the skgrepo restriction keeps out
  ForkRequestOnUnknownNode       (ID, HeadlineName),  // An explicit 'skg-fork-node' request on a node whose id is not in the graph (an unsaved headline): nothing exists to clone.
  ForkRequestMultiple            (ID, Vec<HeadlineName>),  // Two headlines for the same id both carry an explicit fork request; at most one is allowed.
  ForkRuleViolation              (String),
  ChangedSinceRendered {
    gnid      : ID, // the node's pid
    change    : StaleGraphnodeReport, // how the graph's graphnode differs from the buffer's baseline copy of it
    headlines : Vec<HeadlineName>, // the buffer's occurrences of the node
  }, // The save would write a node that changed after its buffer was rendered: writing the buffer's text would silently undo that change.
  RenderedUnderAnotherSkgrepoRestriction {
    skgrepos_then : Vec<SkgrepoName>, // the skgrepos the restriction the held view was last rendered under shows, in the config's order
    skgrepos_now  : Vec<SkgrepoName>, // the skgrepos the connection's current restriction shows, in the config's order
  }, // The save is of a held view rendered under a skgrepo restriction that showed other skgrepos than the connection's current one.
  MultipleEditableViewRequestsForSameId    (ID, Vec<HeadlineName>), // Multiple editable view requests for the same ID
  WriteProtectAndEditableViewRequestsOnOneOccurrence (ID, HeadlineName), // One occurrence asks to become both write-protected and the editable one. (On different occurrences of a node, the two requests move editability, and are allowed.)
  EmptyTitle                             (ID),
  LocalStructureViolation        (String, ID, HeadlineName), // (error message, nearest ancestor ID, the headline violating)
  EditRequestOnWriteProtectedOccurrence      (ID, HeadlineName), // Write-protected nodes -- ghosts and unknowns in particular -- cannot carry write instructions like (editRequest delete) or (editRequest (merge X)). The user must visit an editable view of the node first.
  EditedWriteProtectedOccurrence {
    gnid    : ID,
    title    : String,
    changes  : Vec<String>,
    headline : HeadlineName,
  }, // This occurrence differs from the graph, but a write-protected occurrence emits no fieldIntent for the differing data.
  FlagsSurfaceEdited {
    recorder_gnid : ID,
    recorder_title : String,
    changes        : Vec<String>,
    headline       : HeadlineName, // the recorder's occurrence
  },
  FlagEditOnForeignNode                 (ID, SkgrepoName),
  FlagEditOnUnknownNode                 (ID),
  IDFolder_Edited                   (ID,       // recorder of the idFolder
                                  Vec<ID>,  // ids the buffer's idFolder claims
                                  Vec<ID>,  // the recorder's real ids (pid + extra_ids); empty if the recorder is not in the graph
                                  HeadlineName), // the recorder's occurrence
  InPlaceOf_Mismatch         (Option<ID>, // the own ID of the headline bearing the fact
                                  ID,         // the original the fact claims
                                  Option<ID>, // the mirror the graph says stands for that original; None if the graph was unavailable
                                  HeadlineName), // the headline bearing the fact
  Other                          (String),
}


//
// Implementations
//

impl NodeMergeRole {
  /// "the acquirer" or "the acquiree".
  pub fn with_article (
    &self,
  ) -> &'static str {
    match self {
      NodeMergeRole::Acquirer => "the acquirer",
      NodeMergeRole::Acquiree => "the acquiree", }}
}

impl HeadlineName {
  pub fn of (
    viewnode_id  : Option<&ViewnodeId>,
    graphnode_id : Option<&ID>,
    title        : &str,
  ) -> HeadlineName {
    HeadlineName {
      title        : title . to_string (),
      graphnode_id : graphnode_id . cloned (),
      viewnode_id  : viewnode_id . cloned (), }}

  /// The org subheadline naming this headline under a message's
  /// headline, which the clients put at level 2 (see
  /// 'docs/api-and-formats.org', "How a message names a headline"):
  /// the title (or '(untitled)'), then 'graphnode ID: X' when it
  /// carries one, then 'viewnode ID: X' with the command that finds
  /// it, or 'not yet saved'.
  pub fn org_subheadline (
    &self
  ) -> String {
    let viewnode_line : String = match &self . viewnode_id {
      Some (viewnode_id) => format! (
        "- viewnode ID: {}. To find it: C-c g v {}\n",
        viewnode_id, viewnode_id ),
      None => "- not yet saved\n" . to_string (), };
    format! ( "{}{}", self . org_subheadline_of_a_node_this_save_created (),
              viewnode_line ) }

  /// The org subheadline naming a headline from which the save that
  /// sends the message created a node: the title (or '(untitled)')
  /// and 'graphnode ID: X', with no viewnode line, since the headline
  /// is saved and its viewnode ID is the rerender's to give.
  pub fn org_subheadline_of_a_node_this_save_created (
    &self
  ) -> String {
    let heading : &str =
      if self . title . is_empty () { "(untitled)" }
      else { &self . title };
    let graphnode_line : String =
      self . graphnode_id . as_ref ()
      . map ( |gnid| format! ("- graphnode ID: {}\n", gnid . 0) )
      . unwrap_or_default ();
    format! ( "*** {}\n{}", heading, graphnode_line ) }
}

impl fmt::Display for HeadlineName {
  /// 'viewnode ID X', or 'headline "T"': for a one-line description,
  /// as in a log. A message shown to the user names its headlines as
  /// subheadlines instead ('org_subheadline').
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match &self . viewnode_id {
      Some (viewnode_id) =>
        write! (f, "viewnode ID {}", viewnode_id),
      None =>
        write! (f, "headline {:?}", self . title), }}}

/// The subheadlines naming 'names', one after another.
pub fn org_subheadlines (
  names : &[HeadlineName],
) -> String {
  names . iter () . map ( HeadlineName::org_subheadline )
    . collect::<Vec<String>> () . concat () }

/// A message's text followed by the subheadlines naming 'names': the
/// text ends its line, and the subheadlines come after it, so that
/// nothing of the message falls under one of them.
pub fn with_org_subheadlines (
  text  : &str,
  names : &[HeadlineName],
) -> String {
  format! ( "{}\n{}", text . trim_end_matches ('\n'),
            org_subheadlines (names) ) }

/// Several occurrences' names on one line: 'viewnode IDs X, Y' when
/// every one has a viewnode ID, else each named as 'HeadlineName'
/// displays it.
pub fn name_headlines (
  names : &[HeadlineName],
) -> String {
  match viewnode_ids_of_all (names) {
    Some (viewnode_ids) if names . len () > 1 =>
      format! ("viewnode IDs {}", viewnode_ids),
    _ =>
      names . iter () . map ( |name| name . to_string () )
      . collect::<Vec<String>> () . join (", "), }}

/// The viewnode IDs, comma-separated, if every name has one.
fn viewnode_ids_of_all (
  names : &[HeadlineName],
) -> Option<String> {
  let viewnode_ids : Vec<&str> =
    names . iter ()
    . map ( |name| name . viewnode_id . as_ref ()
            . map ( |viewnode_id| viewnode_id . 0 . as_str () ))
    . collect::<Option<Vec<&str>>> () ?;
  Some ( viewnode_ids . join (", ") ) }

impl std::fmt::Display for LinkParseError {
  fn fmt (
    &self,
    f: &mut std::fmt::Formatter <'_>
  ) -> std::fmt::Result {
    match self {
      LinkParseError::InvalidFormat =>
        write! (
          f, "Invalid link format. Expected [[id:ID][LABEL]]" ),
      LinkParseError::MissingDivider =>
        write! (
          f, "Missing divider between ID and label. Expected ][" ),
    } } }

impl Error for LinkParseError {}

impl std::fmt::Display for BufferValidationError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>
  ) -> std::fmt::Result {
    match self {
      BufferValidationError::Body_of_NonVognode(title, kind, headline) =>
        write!(f, "{} node ({}) should not have a body. Node title: '{}'",
               kind, headline, title),
      BufferValidationError::Multiple_Defining_Viewnodes (gnid, headlines) =>
        write!(f, "Multiple occurrences of node {:?} are editable ({})",
               gnid, name_headlines (headlines)),
      BufferValidationError::AmbiguousDeletion (gnid, headlines) =>
        write!(f, "Ambiguous deletion request for ID {:?} ({})",
               gnid, name_headlines (headlines)),
      BufferValidationError::ContradictoryProcessedRequests (leaf, headlines) =>
        write!(f, "Contradictory processed requests for {} ({})",
               leaf . describe (), name_headlines (headlines)),
      BufferValidationError::DuplicatedContent (gnid) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", gnid),
      BufferValidationError::ModifiedForeignNode(gnid, skgrepo) =>
        write!(f, "Cannot modify node {:?} from foreign repo '{}'", gnid, skgrepo),
      BufferValidationError::CreatedForeignNode(gnid, skgrepo) =>
        write!(f, "Cannot create node {:?} in foreign repo '{}'", gnid, skgrepo),
      BufferValidationError::CannotMoveToOrFromForeignSkgrepo(gnid, disk_skgrepo, buffer_skgrepo) =>
        write!(f, "Cannot move node {:?} between repos '{}' and '{}': one or both are foreign", gnid, disk_skgrepo, buffer_skgrepo),
      BufferValidationError::CannotMoveAndMergeSimultaneously(gnid) =>
        write!(f, "Cannot move and merge node {:?} in the same save", gnid),
      BufferValidationError::CannotDeleteAndMergeSimultaneously { pid, title, role } =>
        write!(f, "Cannot delete and merge node {:?} ('{}', {}) in the same save",
               pid, title, role . with_article ()),
      BufferValidationError::SkgrepoNotInConfig(gnid, skgrepo) =>
        write!(f, "Node {:?} references repo '{}' which does not exist in config", gnid, skgrepo),
      BufferValidationError::ForkSkgrepoUnresolved(gnid) =>
        write!(f, "Cannot fork node {:?}: no owned repo to put the clone in. It has no owned ancestor in the view to inherit a repo from; set the clone's repo in the confirmation buffer (C-c s s).", gnid),
      BufferValidationError::ForkAlreadyExists(original, existing) =>
        write!(f, "Cannot fork node {:?}: you already fork it; your fork is {:?}. Edit that fork instead. To fork it a second time deliberately, make another note of yours fork it.", original, existing),
      BufferValidationError::ForkSkgrepoRestricted(gnid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is kept out by the skgrepo restriction. Activate it first; an invisible clone is never created silently.", gnid, skgrepo),
      BufferValidationError::RecordSkgrepoRestricted(recorder, relation, member, skgrepo) =>
        write!(f, "Cannot save node {:?}: its {} record of {} would land in repo '{}', which the skgrepo restriction keeps out. Activate it first; an invisible record is never created silently.", recorder, relation, member, skgrepo),
      BufferValidationError::RecordMovedOutOfRestrictedSkgrepo(recorder, relation, member, skgrepo) =>
        write!(f, "Cannot save node {:?}: its {} record of {} would leave repo '{}', which the skgrepo restriction keeps out. Activate it first; a record you cannot see is never moved silently.", recorder, relation, member, skgrepo),
      BufferValidationError::ForkSkgrepoNotOwned(gnid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is not one you own. Choose an owned repo for the clone (C-c s s in the confirmation buffer).", gnid, skgrepo),
      BufferValidationError::ForkRequestOnUnknownNode(gnid, headline) =>
        write!(f, "Cannot fork node {:?} ({}): it is not in the graph. Only a saved node can be forked; save it first, then fork.", gnid, headline),
      BufferValidationError::ForkRequestMultiple(gnid, headlines) =>
        write!(f, "Multiple fork requests for the same node {:?} ({}). At most one fork request per node is allowed.", gnid, name_headlines (headlines)),
      BufferValidationError::ForkRuleViolation(msg) =>
        write!(f, "{}", msg),
      BufferValidationError::MultipleEditableViewRequestsForSameId (gnid, headlines) =>
        write!(f, "Multiple editable view requests for the same ID {:?} ({}). At most one request per ID is allowed.", gnid, name_headlines (headlines)),
      BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (gnid, headline) =>
        write!(f, "An occurrence of node {:?} ({}) requests both write-protection and an editable view. Request one; to move editability, request write-protection on one occurrence (C-c s w) and an editable view on another (C-c s e).", gnid, headline),
      BufferValidationError::EmptyTitle(gnid) =>
        write!(f, "Node {:?} has an empty title. Every editable node must have a non-empty title.", gnid),
      BufferValidationError::LocalStructureViolation(msg, gnid, headline) =>
        write!(f, "Local structure violation at ID {:?} ({}): {}", gnid, headline, msg),
      BufferValidationError::IDFolder_Edited(recorder, buffer_gnids, real_gnids, headline) =>
        write!(f, "The idFolder under node {:?} ({}) was edited (buffer claims {:?}; real ids are {:?}). Reordering is fine, but IDs cannot be added, removed or edited through the buffer; edit the .skg file directly.", recorder, headline, buffer_gnids, real_gnids),
      BufferValidationError::EditRequestOnWriteProtectedOccurrence (gnid, headline) =>
        write!(f, "Edit request on write-protected node {:?} ({}). Ghosts and unknown viewnodes are write-protected; write-protected nodes cannot carry write instructions. Visit an editable view of the node first (C-c g RET).", gnid, headline),
      BufferValidationError::EditedWriteProtectedOccurrence {
        gnid, title, changes, headline } =>
        write!(f, "The write-protected occurrence of node {:?} ({:?}, {}) differs from the graph ({}). A write-protected occurrence saves nothing of its own; edit an editable occurrence instead, or request this one editable (C-c s e).",
               gnid, title, headline, changes . join ("; ")),
      BufferValidationError::FlagsSurfaceEdited {
        recorder_gnid, recorder_title, changes, headline } =>
        write!(f, "The flags surface under node {:?} ({:?}, {}) was edited: {}. It is server-owned and no changes were saved. Use skg-set-flag-search-matching for noSearchMatching; provenance flags have no setter.",
               recorder_gnid, recorder_title, headline, changes . join ("; ")),
      BufferValidationError::FlagEditOnForeignNode (gnid, skgrepo) =>
        write! (f, "Cannot change flags of node {:?} from foreign repo '{}'; this gesture never creates an implicit fork.", gnid, skgrepo),
      BufferValidationError::FlagEditOnUnknownNode (gnid) =>
        write! (f, "Cannot change flags of unsaved or unknown graphnode {:?}; save the node first.", gnid),
      BufferValidationError::InPlaceOf_Mismatch(bearer, original, effective, headline) =>
        write!(f, "Node {:?} ({}) carries the fact (inPlaceOf {:?}), but it is not a mirror of that original (whose mirror shown here is {:?}). The fact looks hand-edited or stale; saving it would rewrite a contains list. Re-render the view and retry.", bearer, headline, original, effective),
      BufferValidationError::ChangedSinceRendered {
        gnid, change : StaleGraphnodeReport::Deleted, headlines } =>
        write!(f, "Node {:?} ({}) was deleted after this buffer was rendered. Nothing was saved; reopen a fresh view and reapply the edit.",
               gnid, name_headlines (headlines)),
      BufferValidationError::ChangedSinceRendered {
        gnid, change : StaleGraphnodeReport::Changed { title, aspects, .. }, headlines } =>
        write!(f, "Node {:?} \"{}\" ({}) changed after this buffer was rendered: {}. Nothing was saved; reopen a fresh view and reapply the edit.",
               gnid, title, name_headlines (headlines),
               aspects . iter () . map ( |aspect| aspect . name )
               . collect::<Vec<&str>> () . join (", ")),
      BufferValidationError::RenderedUnderAnotherSkgrepoRestriction {
        skgrepos_then, skgrepos_now } =>
        write!(f, "{}", rendered_under_another_skgrepo_restriction_text (
          skgrepos_then, skgrepos_now )),
      BufferValidationError::Other (msg) =>
        write!(f, "{}", msg), }} }

impl BufferValidationError {
  /// The headlines the error concerns, which its org form names as
  /// subheadlines.
  pub fn named_headlines (
    &self
  ) -> Vec<&HeadlineName> {
    match self {
      BufferValidationError::Body_of_NonVognode (_, _, headline)
        | BufferValidationError::ForkRequestOnUnknownNode (_, headline)
        | BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (_, headline)
        | BufferValidationError::LocalStructureViolation (_, _, headline)
        | BufferValidationError::EditRequestOnWriteProtectedOccurrence (_, headline)
        | BufferValidationError::EditedWriteProtectedOccurrence { headline, .. }
        | BufferValidationError::FlagsSurfaceEdited { headline, .. }
        | BufferValidationError::IDFolder_Edited (_, _, _, headline)
        | BufferValidationError::InPlaceOf_Mismatch (_, _, _, headline) =>
        vec! [ headline ],
      BufferValidationError::Multiple_Defining_Viewnodes (_, headlines)
        | BufferValidationError::AmbiguousDeletion (_, headlines)
        | BufferValidationError::ContradictoryProcessedRequests (_, headlines)
        | BufferValidationError::ForkRequestMultiple (_, headlines)
        | BufferValidationError::MultipleEditableViewRequestsForSameId (_, headlines)
        | BufferValidationError::ChangedSinceRendered { headlines, .. } =>
        headlines . iter () . collect (),
      BufferValidationError::DuplicatedContent (_)
        | BufferValidationError::ModifiedForeignNode (..)
        | BufferValidationError::CreatedForeignNode (..)
        | BufferValidationError::CannotMoveToOrFromForeignSkgrepo (..)
        | BufferValidationError::CannotMoveAndMergeSimultaneously (_)
        | BufferValidationError::CannotDeleteAndMergeSimultaneously { .. }
        | BufferValidationError::SkgrepoNotInConfig (..)
        | BufferValidationError::ForkSkgrepoUnresolved (_)
        | BufferValidationError::ForkAlreadyExists (..)
        | BufferValidationError::ForkSkgrepoRestricted (..)
        | BufferValidationError::RecordSkgrepoRestricted (..)
        | BufferValidationError::RecordMovedOutOfRestrictedSkgrepo (..)
        | BufferValidationError::ForkSkgrepoNotOwned (..)
        | BufferValidationError::ForkRuleViolation (_)
        | BufferValidationError::EmptyTitle (_)
        | BufferValidationError::FlagEditOnForeignNode (..)
        | BufferValidationError::FlagEditOnUnknownNode (_)
        | BufferValidationError::RenderedUnderAnotherSkgrepoRestriction { .. }
        | BufferValidationError::Other (_) =>
        Vec::new (), }}
}

/// The refusal of a save of a view rendered under another skgrepo
/// restriction, as both of its forms say it.
pub fn rendered_under_another_skgrepo_restriction_text (
  skgrepos_then : &[SkgrepoName],
  skgrepos_now  : &[SkgrepoName],
) -> String {
  let names = |skgrepos : &[SkgrepoName]| -> String {
    if skgrepos . is_empty () { "no skgrepo" . to_string () }
    else { skgrepos . iter () . map ( |skgrepo| skgrepo . to_string () )
           . collect::<Vec<String>> () . join (", ") }};
  format! ( "This view was rendered under a different skgrepo restriction (then: {}; now: {}). Rerender it, or archive its edits and reopen it.",
            names (skgrepos_then), names (skgrepos_now) ) }

impl Error for BufferValidationError {}

impl std::fmt::Display for SaveError {
  fn fmt (
    &self,
    f : &mut std::fmt::Formatter<'_>
  ) -> std::fmt::Result {
    match self {
      SaveError::ParseError (msg) =>
        write!(f, "Parse error: {}", msg),
      SaveError::DatabaseError (err) =>
        write!(f, "Database error: {}", err),
      SaveError::IoError (err) =>
        write!(f, "IO error: {}", err),
      SaveError::BufferValidationErrors { errors, .. } => {
        write!(f, "Buffer validation errors: {} error(s) found",
               errors . len()) }} }}

impl std::error::Error for SaveError {
  fn source (
    &self
  ) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      SaveError::DatabaseError (err) => Some(err . as_ref()),
      SaveError::IoError (err) => Some (err),
      _ => None, }} }

/// Formats a SaveError as one message, in org text, for the client:
/// its first line is the message's headline, which the client puts at
/// level 2 ('** '); each error follows as a '** Error N' of its own,
/// any headline it names a level-3 subheadline of that error.
pub fn format_save_error_as_org (
  error : &SaveError
) -> String {
  match error {
    SaveError::ParseError (msg) => {
      format!("NOTHING WAS SAVED\n\nParse error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              msg) },
    SaveError::DatabaseError (err) => {
      format!("NOTHING WAS SAVED\n\nDatabase error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::IoError (err) => {
      format!("NOTHING WAS SAVED\n\nI/O error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::BufferValidationErrors { errors, .. } => {
      let mut content : String =
        String::from ("NOTHING WAS SAVED\n\nValidation errors found in buffer.\n\n");
      for (i, error) in errors . iter() . enumerate() {
        content . push_str(&format!("** Error {}\n", i + 1));
        content . push_str(&format_buffer_validation_error (error));
        content . push ('\n'); }
      content }} }

/// One validation error's text, then the subheadlines naming the
/// headlines it concerns, which nest under its '** Error N'.
fn format_buffer_validation_error (
  error : &BufferValidationError
) -> String {
  let named : Vec<HeadlineName> =
    error . named_headlines () . into_iter () . cloned () . collect ();
  let text : String = buffer_validation_error_text (error);
  if named . is_empty () { text }
  else { with_org_subheadlines (&text, &named) }}

/// One validation error's text, without the headlines it names.
fn buffer_validation_error_text (
  error : &BufferValidationError
) -> String {
  match error {
    BufferValidationError::Body_of_NonVognode(title, kind, _) => {
      format!("{} node has a body (not allowed):\n- Title: {}\n",
              kind, title) },
    BufferValidationError::IDFolder_Edited(recorder, buffer_gnids, real_gnids, _) => {
      let fmt_gnids = |gnids : &Vec<ID>| -> String {
        gnids . iter() . map(|i| i . 0 . as_str())
          . collect::<Vec<&str>>() . join(", ") };
      if real_gnids . is_empty() {
        format!("Node {} is not in the graph, so it cannot carry an idFolder:\n- ids claimed by the buffer: {}\n- IDs cannot be created through the buffer. To edit a node's ID list, edit its .skg file directly.\n",
                recorder . 0, fmt_gnids(buffer_gnids))
      } else {
        format!("The idFolder under node {} was edited; saving would not honor that, so the save was aborted:\n- ids claimed by the buffer: {}\n- the node's real ids: {}\n- Reordering is fine, but IDs cannot be added, removed or edited through the buffer. To edit a node's ID list, edit its .skg file directly.\n",
                recorder . 0, fmt_gnids(buffer_gnids), fmt_gnids(real_gnids)) }},
    BufferValidationError::InPlaceOf_Mismatch(bearer, original, effective, _) => {
      let fmt_opt = |gnid : &Option<ID>| -> String {
        gnid . as_ref () . map ( |i| i . 0 . clone () )
          . unwrap_or_else ( || "<none>" . to_string () ) };
      format!("Invalid (inPlaceOf ...) fact; saving it would rewrite a contains list, so the save was aborted:\n- the node carrying the fact: {}\n- the original the fact claims it stands for: {}\n- what the server would show in place of that original: {}\n- The fact looks hand-edited or stale. Re-render the view (close and reopen, or C-c g RET) and retry.\n",
              fmt_opt (bearer), original . 0, fmt_opt (effective)) },
    BufferValidationError::Multiple_Defining_Viewnodes (gnid, _) => {
      format!("ID has multiple defining containers:\n- ID: {}\n",
              gnid . 0) },
    BufferValidationError::AmbiguousDeletion (gnid, _) => {
      format!("ID has ambiguous deletion instructions:\n- ID: {}\n",
              gnid . 0) },
    BufferValidationError::ContradictoryProcessedRequests (leaf, _) => {
      format!("Contradictory processed requests: one headline asks that a change be marked processed, another that it be marked unprocessed:\n- the change: {}\n- Remove one of the two requests.\n",
              leaf . describe ()) },
    BufferValidationError::DuplicatedContent (gnid) => {
      format!("Node has multiple Content children with the same ID:\n- ID: {}\n",
              gnid . 0) },
    BufferValidationError::ModifiedForeignNode(gnid, skgrepo) => {
      format!("Cannot modify node from foreign repo:\n- ID: {}\n- Repo: {}\n- Foreign repos can only be viewed, not modified.\n",
              gnid . 0, skgrepo) },
    BufferValidationError::CreatedForeignNode(gnid, skgrepo) => {
      format!("Cannot create node in foreign repo:\n- ID: {}\n- Repo: {}\n- Foreign repos can only be viewed, not modified.\n",
              gnid . 0, skgrepo) },
    BufferValidationError::CannotMoveToOrFromForeignSkgrepo(gnid, disk_skgrepo, buffer_skgrepo) => {
      format!("Cannot move node between repos:\n- ID: {}\n- Repo on disk: {}\n- Repo from buffer: {}\n- One or both repos are foreign.\n",
              gnid . 0, disk_skgrepo, buffer_skgrepo) },
    BufferValidationError::CannotMoveAndMergeSimultaneously(gnid) => {
      format!("Cannot move and merge a node simultaneously:\n- ID: {}\n- Please save the move and merge in separate operations.\n",
              gnid . 0) },
    BufferValidationError::CannotDeleteAndMergeSimultaneously { pid, title, role } => {
      format!("A node this save would merge is also deleted by it:\n- Title: {}\n- ID: {}\n- It is {} of a merge request in this buffer.\n- Remove either the deletion or the merge request, then save.\n",
              title, pid . 0, role . with_article ()) },
    BufferValidationError::SkgrepoNotInConfig(gnid, skgrepo) => {
      format!("Node references a repo that does not exist in config:\n- ID: {}\n- Repo: {}\n- Please check your config file and ensure this repo is defined.\n",
              gnid . 0, skgrepo) },
    BufferValidationError::ForkSkgrepoUnresolved(gnid) => {
      format!("Cannot fork a foreign node -- no owned repo for the clone:\n- Foreign node: {}\n- It has no owned ancestor in the view to inherit a repo from.\n- Set the clone's repo in the fork-confirmation buffer (C-c s s), then approve.\n",
              gnid . 0) },
    BufferValidationError::ForkAlreadyExists(original, existing) => {
      format!("Cannot fork a node you already fork:\n- Foreign node: {}\n- Your fork of it: {}\n- Edit that fork instead. To fork it a second time deliberately, make another note of yours fork it.\n",
              original . 0, existing . 0) },
    BufferValidationError::ForkSkgrepoRestricted(gnid, skgrepo) => {
      format!("Cannot fork into a restricted repo:\n- Foreign node: {}\n- Clone's resolved repo: {}\n- That repo is not in the skgrepo restriction. Activate it first; an invisible clone is never created silently.\n",
              gnid . 0, skgrepo) },
    BufferValidationError::RecordSkgrepoRestricted(recorder, relation, member, skgrepo) => {
      format!("Cannot save a record into a restricted repo:\n- Recorder: {}\n- Relation: {}\n- Member: {}\n- The record's repo: {}\n- That repo is not in the skgrepo restriction. Activate it first; an invisible record is never created silently.\n",
              recorder . 0, relation, member, skgrepo) },
    BufferValidationError::RecordMovedOutOfRestrictedSkgrepo(recorder, relation, member, skgrepo) => {
      format!("Cannot move a record out of a restricted repo:\n- Recorder: {}\n- Relation: {}\n- Member: {}\n- The record's repo: {}\n- That repo is not in the skgrepo restriction. Activate it first; a record you cannot see is never moved silently.\n",
              recorder . 0, relation, member, skgrepo) },
    BufferValidationError::ForkSkgrepoNotOwned(gnid, skgrepo) => {
      format!("Cannot fork into a repo you do not own:\n- Foreign node: {}\n- Clone's chosen repo: {}\n- Pick an owned repo for the clone (C-c s s in the confirmation buffer).\n",
              gnid . 0, skgrepo) },
    BufferValidationError::ForkRequestOnUnknownNode(gnid, _) => {
      format!("Cannot fork an unsaved node:\n- Node: {}\n- It is not in the graph. Only a saved node can be forked; save it first, then fork.\n",
              gnid . 0) },
    BufferValidationError::ForkRequestMultiple(gnid, _) => {
      format!("Multiple fork requests for the same node:\n- Node: {}\n- At most one fork request per node is allowed.\n",
              gnid . 0) },
    BufferValidationError::ForkRuleViolation(msg) => {
      format!("{}\n", msg) },
    BufferValidationError::MultipleEditableViewRequestsForSameId (gnid, _) => {
      format!("Multiple editable view requests for the same ID:\n- ID: {}\n- At most one editable view request per ID is allowed.\n",
              gnid . 0) },
    BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (gnid, _) => {
      format!("Contradictory view requests on one occurrence:\n- ID: {}\n- It requests both write-protection and an editable view.\n- To move editability, request write-protection on one occurrence (C-c s w) and an editable view on another (C-c s e).\n",
              gnid . 0) },
    BufferValidationError::EmptyTitle(gnid) => {
      format!("Node has an empty title:\n- ID: {}\n- Every editable node must have a non-empty title.\n",
              gnid . 0) },
    BufferValidationError::LocalStructureViolation(msg, gnid, _) => {
      format!("Local structure violation:\n- ID: {}\n- {}\n",
              gnid . 0, msg) },
    BufferValidationError::EditRequestOnWriteProtectedOccurrence (gnid, _) => {
      format!("Edit request on a write-protected node (possibly a ghost or an unknown viewnode):\n- ID: {}\n- Write-protected nodes cannot carry write instructions.\n- To delete or merge this node, visit an editable view of it first (C-c g RET).\n",
              gnid . 0) },
    BufferValidationError::EditedWriteProtectedOccurrence {
      gnid, title, changes, .. } => {
      format!("Write-protected occurrence differs from the graph:\n- ID: {}\n- Title: {}\n- Differences: {}\n- A write-protected occurrence saves nothing of its own; no changes were saved.\n- Edit an editable occurrence instead, or request this one editable (C-c s e).\n",
              gnid . 0, title, changes . join ("; ")) },
    BufferValidationError::FlagsSurfaceEdited {
      recorder_gnid, recorder_title, changes, .. } => {
      format!("Edited server-owned flags surface:\n- Recorder ID: {}\n- Recorder title: {}\n- Changes: {}\n- No changes were saved. Use skg-set-flag-search-matching for noSearchMatching. HadId and WasOverloaded are provenance and have no setter.\n",
              recorder_gnid . 0, recorder_title, changes . join ("; ")) },
    BufferValidationError::FlagEditOnForeignNode (gnid, skgrepo) => {
      format!("Cannot change a flag on a foreign node:\n- ID: {}\n- Repo: {}\n- Flag changes never create an implicit fork. Visit an owned node instead.\n",
              gnid . 0, skgrepo) },
    BufferValidationError::FlagEditOnUnknownNode (gnid) => {
      format!("Cannot change a flag on an unsaved or unknown graphnode:\n- ID: {}\n- Save the node first, then run the flag setter.\n",
              gnid . 0) },
    BufferValidationError::ChangedSinceRendered { gnid, change, .. } => {
      format!("A node this save would write changed after the buffer was rendered:\n{}- Saving would undo that change. Archive your edits if you need them (M-x skg-show-unsaved-changes in Emacs, :SkgShowUnsavedChanges in Neovim), reopen a fresh view, and reapply them there.\n",
              change_since_rendered_lines (gnid, change)) },
    BufferValidationError::RenderedUnderAnotherSkgrepoRestriction {
      skgrepos_then, skgrepos_now } => {
      format!("{}\n", rendered_under_another_skgrepo_restriction_text (
        skgrepos_then, skgrepos_now )) },
    BufferValidationError::Other (msg) => {
      format!("{}\n", msg) }, }}

/// The lines of a stale-write error between its first line and its
/// advice: the graphnode, and what changed in it.
fn change_since_rendered_lines (
  gnid   : &ID,
  change : &StaleGraphnodeReport,
) -> String {
  match change {
    StaleGraphnodeReport::Deleted =>
      format! ("- ID: {}\n- It was deleted after this buffer was rendered.\n",
               gnid . 0),
    StaleGraphnodeReport::Changed { title, aspects, body_lines } => {
      let aspects_text : String =
        aspects . iter ()
        . map ( |aspect| match &aspect . detail {
          Some (detail) => format! ("the {} ({})", aspect . name, detail),
          None          => format! ("the {}", aspect . name), } )
        . collect::<Vec<String>> () . join ("; ");
      let body_text : String =
        if body_lines . is_empty () { String::new () }
        else { format! (
          "- The body's changed lines, then against now:\n{}",
          body_lines . iter ()
          . map ( |line| format! ("  {}\n", line) )
          . collect::<String> () ) };
      format! ("- Title: {}\n- ID: {}\n- Changed since this buffer was rendered: {}.\n{}",
               title, gnid . 0, aspects_text, body_text) }, }}
