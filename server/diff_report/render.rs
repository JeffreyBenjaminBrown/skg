use crate::diff_report::types::{
  CommitStamp, DiffReport, DuplicateIDReport, ListDiffItem,
  NodeDiffReport, RelationshipDiff, RepoForReport, TextDiffLine,
  ValueSetDiff, VanishedNodeReport};
use crate::types::misc::{ID, SkgRepoName};

use std::collections::{BTreeMap, BTreeSet, HashMap};

pub fn render_report (
  report : &DiffReport,
) -> String {
  let abbreviations : HashMap<ID, String> =
    abbreviations_for_report (report);
  let mut out : String =
    String::new ();
  out . push_str ("* affected nodes\n");
  render_duplicate_skgids (
    &mut out, &report . duplicate_ids, &abbreviations );
  let mut any_nodes : bool =
    ! report . duplicate_ids . is_empty ();
  for bucket in &report . buckets {
    // Empty categories are rendered too (as bare headings), so a
    // reader can see what the categories are, and thus (e.g.)
    // that nothing was orphaned.
    out . push_str (&format! ("** {}\n", bucket . name));
    if bucket . nodes . is_empty () {
      continue; }
    any_nodes = true;
    for node in &bucket . nodes {
      render_node_report (&mut out, node, report, &abbreviations); }}
  if ! any_nodes {
    out . push_str ("** no affected nodes\n"); }
  render_vanished_nodes (&mut out, &report . vanished);
  out
}

/// The vanished-nodes section (TODO/more.org): each id the worktree
/// references though it exists in no skgrepo, with what git history
/// says it used to be. Like the buckets, the (empty) heading renders
/// even with nothing to report, so the reader knows it was checked.
fn render_vanished_nodes (
  out      : &mut String,
  vanished : &[VanishedNodeReport],
) {
  out . push_str ("* vanished nodes (referenced, but existing in no repo)\n");
  if vanished . is_empty () {
    out . push_str ("** none\n");
    return; }
  let stamp = |c : &CommitStamp| -> String {
    format! ("{} ({}, {:?})", c . short_sha, c . date, c . summary) };
  for report in vanished {
    out . push_str (&format! ("** {}\n", report . skgid));
    if report . sightings . is_empty () {
      out . push_str (
        "*** never present in the git history of any repo\n");
      continue; }
    for sighting in & report . sightings {
      out . push_str (&format! (
        "*** in repo {}\n", sighting . home_skgrepo ));
      out . push_str (&format! (
        "**** title when last present: {}\n", sighting . title ));
      out . push_str (&format! (
        "**** last present at commit {}\n",
        stamp (& sighting . last_present) ));
      match & sighting . vanished_at {
        Some (c) => out . push_str (&format! (
          "**** vanished at commit {}\n", stamp (c) )),
        None => out . push_str (
          "**** still present at HEAD (only the worktree lacks it)\n" ), }
      if sighting . outbound . is_empty () {
        out . push_str ("**** its own relationship lists then: all empty\n");
      } else {
        out . push_str ("**** its own relationship lists then\n");
        for (relation, members) in & sighting . outbound {
          out . push_str (&format! ("***** {}\n", relation));
          for member in members {
            out . push_str (&format! ("****** {}\n", member)); }} }
      if sighting . inbound . is_empty () {
        out . push_str ("**** nothing else referred to it then\n");
      } else {
        out . push_str ("**** referred to then by\n");
        for (referrer, relation) in & sighting . inbound {
          out . push_str (&format! (
            "***** {} (via {})\n", referrer, relation )); }} }}
}

fn render_duplicate_skgids (
  out           : &mut String,
  duplicates    : &[DuplicateIDReport],
  abbreviations : &HashMap<ID, String>,
) {
  if duplicates . is_empty () {
    return; }
  out . push_str ("** IDs claimed by more than one node\n");
  for duplicate in duplicates {
    out . push_str (&format! (
      "*** {}\n",
      abbreviation_for (&duplicate . skgid, abbreviations) ));
    out . push_str (&format! ("**** {}\n", duplicate . skgid));
    out . push_str ("**** repo(s) before these changes\n");
    render_skgrepos (out, &duplicate . before_skgrepos);
    out . push_str ("**** repo(s) after these changes\n");
    render_skgrepos (out, &duplicate . after_skgrepos); }
}

fn render_skgrepos (
  out      : &mut String,
  skgrepos : &BTreeSet<SkgRepoName>,
) {
  if skgrepos . is_empty () {
    out . push_str ("***** none\n");
    return; }
  for skgrepo in skgrepos {
    out . push_str (&format! ("***** {}\n", skgrepo)); }
}

fn render_node_report (
  out           : &mut String,
  node          : &NodeDiffReport,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  out . push_str (&format! (
    "*** {}\n",
    abbreviation_for (&node . pid, abbreviations) ));
  out . push_str ("**** identifiers\n");
  out . push_str (&format! ("***** {}\n", skgrepo_text (&node . home_skgrepo)));
  out . push_str (&format! ("***** {}\n", node . pid));
  out . push_str (&format! ("***** {}\n", node . title));
  if let Some ((before, after)) = &node . skgrepo_change {
    out . push_str ("**** repo\n");
    out . push_str (&format! ("***** was: {}\n", before));
    out . push_str (&format! ("***** is: {}\n", after)); }
  if let Some (diff) = &node . title_diff {
    render_text_diff (out, "title", diff); }
  if let Some (diff) = &node . body_diff {
    render_text_diff (out, "body", diff); }
  for value_diff in &node . value_set_diffs {
    render_value_set_diff (out, value_diff); }
  for relationship_diff in &node . relationship_diffs {
    render_relationship_diff (
      out, relationship_diff, report, abbreviations ); }
  if let Some (diff) = &node . content_list_diff {
    render_content_list_diff (out, diff, abbreviations); }
}

fn skgrepo_text (
  skgrepo : &RepoForReport,
) -> String {
  match skgrepo {
    RepoForReport::Before (s) => s . to_string (),
    RepoForReport::After  (s) => s . to_string (), }
}

fn render_text_diff (
  out   : &mut String,
  name  : &str,
  lines : &[TextDiffLine],
) {
  out . push_str (&format! ("**** {}\n", name));
  for line in lines {
    match line {
      TextDiffLine::Unchanged (s) =>
        out . push_str (&format! (" {}\n", s)),
      TextDiffLine::Removed (s) =>
        out . push_str (&format! (" -{}\n", s)),
      TextDiffLine::Added (s) =>
        out . push_str (&format! (" +{}\n", s)), } }
}

fn render_value_set_diff (
  out  : &mut String,
  diff : &ValueSetDiff,
) {
  out . push_str (&format! ("**** {}\n", diff . name));
  if ! diff . lost . is_empty () {
    out . push_str ("***** lost\n");
    for value in &diff . lost {
      out . push_str (&format! ("****** {}\n", value)); }}
  if ! diff . gained . is_empty () {
    out . push_str ("***** gained\n");
    for value in &diff . gained {
      out . push_str (&format! ("****** {}\n", value)); }}
}

fn render_relationship_diff (
  out           : &mut String,
  diff          : &RelationshipDiff,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  if is_backward_relationship_role (diff . role) {
    render_backward_relationship_diff (
      out, diff, report, abbreviations );
    return; }
  out . push_str (&format! ("**** {}\n", diff . role));
  if ! diff . lost . is_empty () {
    out . push_str ("***** lost\n");
    for skgid in &diff . lost {
      render_related_node (out, skgid, report, abbreviations); }}
  if ! diff . gained . is_empty () {
    out . push_str ("***** gained\n");
    for skgid in &diff . gained {
      render_related_node (out, skgid, report, abbreviations); }}
}

fn render_backward_relationship_diff (
  out           : &mut String,
  diff          : &RelationshipDiff,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  out . push_str (&format! (
    "**** {}\n", backward_relationship_heading (diff) ));
  for skgid in &diff . lost {
    render_related_node_with_marker (
      out, "-", skgid, report, abbreviations ); }
  for skgid in &diff . gained {
    render_related_node_with_marker (
      out, "+", skgid, report, abbreviations ); }
  for skgid in &diff . unchanged {
    render_related_node_with_marker (
      out, " ", skgid, report, abbreviations ); }
}

fn backward_relationship_heading (
  diff : &RelationshipDiff,
) -> String {
  let base : &str =
    backward_relationship_heading_base (diff . role);
  match (diff . gained . is_empty (), diff . lost . is_empty ()) {
    (false, false) => format! ("{} (with gains and losses)", base),
    (false, true)  => format! ("{} (with gains)", base),
    (true, false)  => format! ("{} (with losses)", base),
    (true, true)   => format! ("{} (unchanged)", base), }
}

fn backward_relationship_heading_base (
  role : &str,
) -> &str {
  match role {
    "container" => "containers",
    _           => role, }
}

fn is_backward_relationship_role (
  role : &str,
) -> bool {
  matches! (
    role,
    "container" | "subscribee" | "hidden" | "overridden" | "mentioned" )
}

fn render_content_list_diff (
  out           : &mut String,
  diff          : &[ListDiffItem],
  abbreviations : &HashMap<ID, String>,
) {
  out . push_str ("**** contained diff\n");
  for item in diff {
    match item {
      ListDiffItem::Unchanged (skgid) =>
        out . push_str (&format! (
          "  {}\n", abbreviation_for (skgid, abbreviations) )),
      ListDiffItem::Removed (skgid) =>
        out . push_str (&format! (
          " -{}\n", abbreviation_for (skgid, abbreviations) )),
      ListDiffItem::Added (skgid) =>
        out . push_str (&format! (
          " +{}\n", abbreviation_for (skgid, abbreviations) )), } }
}

fn render_related_node (
  out           : &mut String,
  skgid         : &ID,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  render_related_node_with_marker (
    out, "", skgid, report, abbreviations );
}

fn render_related_node_with_marker (
  out           : &mut String,
  marker        : &str,
  skgid         : &ID,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  out . push_str (&format! (
    "****** {}{}\n",
    marker, abbreviation_for (skgid, abbreviations) ));
  out . push_str (&format! ("******* {}\n", skgid));
  out . push_str (&format! (
    "******* {}\n",
    report . titles . get (skgid)
      . map ( |s| s . as_str () )
      . unwrap_or ("[unknown title]") ));
}

fn abbreviation_for (
  skgid            : &ID,
  abbreviations : &HashMap<ID, String>,
) -> String {
  abbreviations . get (skgid) . cloned ()
    . unwrap_or_else ( || format! ("{}..[unknown]", skgid) )
}

fn abbreviations_for_report (
  report : &DiffReport,
) -> HashMap<ID, String> {
  let mut titles : BTreeMap<ID, String> =
    BTreeMap::new ();
  for duplicate in &report . duplicate_ids {
    titles . insert (
      duplicate . skgid . clone (), duplicate . title . clone () ); }
  for bucket in &report . buckets {
    for node in &bucket . nodes {
      titles . insert (node . pid . clone (), node . title . clone ());
      for relationship in &node . relationship_diffs {
        for skgid in relationship . lost . iter ()
          . chain (relationship . gained . iter ())
          . chain (relationship . unchanged . iter ()) {
          titles . entry (skgid . clone ())
            . or_insert_with ( || report . titles . get (skgid)
              . cloned () . unwrap_or_else (
                || "[unknown title]" . to_string () ) ); }}
      if let Some (diff) = &node . content_list_diff {
        for item in diff {
          let skgid : &ID = match item {
            ListDiffItem::Unchanged (skgid)
            | ListDiffItem::Removed (skgid)
            | ListDiffItem::Added (skgid) => skgid, };
          titles . entry (skgid . clone ())
            . or_insert_with ( || report . titles . get (skgid)
              . cloned () . unwrap_or_else (
                || "[unknown title]" . to_string () ) ); }} } }
  let prefix_len : usize =
    distinguishing_prefix_len (titles . keys ());
  titles . into_iter ()
    . map ( |(skgid, title)| {
      let prefix : String =
        skgid . 0 . chars () . take (prefix_len) . collect ();
      let title_head : String =
        title . chars () . take (40) . collect ();
      (skgid, format! ("{}..{}", prefix, title_head)) } )
    . collect ()
}

fn distinguishing_prefix_len<'a> (
  skgids : impl Iterator<Item = &'a ID>,
) -> usize {
  let id_strings : Vec<&'a str> =
    skgids . map ( |skgid| skgid . 0 . as_str () ) . collect ();
  let max_len : usize =
    id_strings . iter () . map ( |s| s . chars () . count () )
      . max () . unwrap_or (8);
  for len in 1..=max_len {
    let mut seen : BTreeSet<String> =
      BTreeSet::new ();
    let all_unique : bool =
      id_strings . iter () . all ( |skgid| {
        let prefix : String =
          skgid . chars () . take (len) . collect ();
        seen . insert (prefix)
      });
    if all_unique {
      return len . max (1); }}
  max_len
}
