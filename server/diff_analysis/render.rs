use crate::diff_analysis::types::{
  DiffReport, DuplicateIDReport, ListDiffItem, NodeDiffReport,
  RelationshipDiff, SourceForReport, TextDiffLine, ValueSetDiff};
use crate::types::misc::{ID, SourceName};

use std::collections::{BTreeMap, BTreeSet, HashMap};

pub fn render_report (
  report : &DiffReport,
) -> String {
  let abbreviations : HashMap<ID, String> =
    abbreviations_for_report (report);
  let mut out : String =
    String::new ();
  out . push_str ("* affected nodes\n");
  render_duplicate_ids (
    &mut out, &report . duplicate_ids, &abbreviations );
  let mut any_nodes : bool =
    ! report . duplicate_ids . is_empty ();
  for bucket in &report . buckets {
    if bucket . nodes . is_empty () {
      continue; }
    any_nodes = true;
    out . push_str (&format! ("** {}\n", bucket . name));
    for node in &bucket . nodes {
      render_node_report (&mut out, node, report, &abbreviations); }}
  if ! any_nodes {
    out . push_str ("** no affected nodes\n"); }
  out
}

fn render_duplicate_ids (
  out           : &mut String,
  duplicates    : &[DuplicateIDReport],
  abbreviations : &HashMap<ID, String>,
) {
  if duplicates . is_empty () {
    return; }
  out . push_str ("** IDs duplicated across sources\n");
  for duplicate in duplicates {
    out . push_str (&format! (
      "*** {}\n",
      abbreviation_for (&duplicate . id, abbreviations) ));
    out . push_str (&format! ("**** {}\n", duplicate . id));
    out . push_str ("**** source(s) before these changes\n");
    render_sources (out, &duplicate . before_sources);
    out . push_str ("**** source(s) after these changes\n");
    render_sources (out, &duplicate . after_sources); }
}

fn render_sources (
  out     : &mut String,
  sources : &BTreeSet<SourceName>,
) {
  if sources . is_empty () {
    out . push_str ("***** none\n");
    return; }
  for source in sources {
    out . push_str (&format! ("***** {}\n", source)); }
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
  out . push_str (&format! ("***** {}\n", source_text (&node . source)));
  out . push_str (&format! ("***** {}\n", node . pid));
  out . push_str (&format! ("***** {}\n", node . title));
  if let Some ((before, after)) = &node . source_change {
    out . push_str ("**** source\n");
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
  if let Some (diff) = &node . contained_list_diff {
    render_contained_list_diff (out, diff, abbreviations); }
}

fn source_text (
  source : &SourceForReport,
) -> String {
  match source {
    SourceForReport::Before (s) => s . to_string (),
    SourceForReport::After  (s) => s . to_string (), }
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
    for id in &diff . lost {
      render_related_node (out, id, report, abbreviations); }}
  if ! diff . gained . is_empty () {
    out . push_str ("***** gained\n");
    for id in &diff . gained {
      render_related_node (out, id, report, abbreviations); }}
}

fn render_backward_relationship_diff (
  out           : &mut String,
  diff          : &RelationshipDiff,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  out . push_str (&format! (
    "**** {}\n", backward_relationship_heading (diff) ));
  for id in &diff . lost {
    render_related_node_with_marker (
      out, "-", id, report, abbreviations ); }
  for id in &diff . gained {
    render_related_node_with_marker (
      out, "+", id, report, abbreviations ); }
  for id in &diff . unchanged {
    render_related_node_with_marker (
      out, " ", id, report, abbreviations ); }
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
    "container" | "subscribee" | "hidden" | "overridden" | "dest" )
}

fn render_contained_list_diff (
  out           : &mut String,
  diff          : &[ListDiffItem],
  abbreviations : &HashMap<ID, String>,
) {
  out . push_str ("**** contained diff\n");
  for item in diff {
    match item {
      ListDiffItem::Unchanged (id) =>
        out . push_str (&format! (
          "  {}\n", abbreviation_for (id, abbreviations) )),
      ListDiffItem::Removed (id) =>
        out . push_str (&format! (
          " -{}\n", abbreviation_for (id, abbreviations) )),
      ListDiffItem::Added (id) =>
        out . push_str (&format! (
          " +{}\n", abbreviation_for (id, abbreviations) )), } }
}

fn render_related_node (
  out           : &mut String,
  id            : &ID,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  render_related_node_with_marker (
    out, "", id, report, abbreviations );
}

fn render_related_node_with_marker (
  out           : &mut String,
  marker        : &str,
  id            : &ID,
  report        : &DiffReport,
  abbreviations : &HashMap<ID, String>,
) {
  out . push_str (&format! (
    "****** {}{}\n",
    marker, abbreviation_for (id, abbreviations) ));
  out . push_str (&format! ("******* {}\n", id));
  out . push_str (&format! (
    "******* {}\n",
    report . titles . get (id)
      . map ( |s| s . as_str () )
      . unwrap_or ("[unknown title]") ));
}

fn abbreviation_for (
  id            : &ID,
  abbreviations : &HashMap<ID, String>,
) -> String {
  abbreviations . get (id) . cloned ()
    . unwrap_or_else ( || format! ("{}..[unknown]", id) )
}

fn abbreviations_for_report (
  report : &DiffReport,
) -> HashMap<ID, String> {
  let mut titles : BTreeMap<ID, String> =
    BTreeMap::new ();
  for duplicate in &report . duplicate_ids {
    titles . insert (
      duplicate . id . clone (), duplicate . title . clone () ); }
  for bucket in &report . buckets {
    for node in &bucket . nodes {
      titles . insert (node . pid . clone (), node . title . clone ());
      for relationship in &node . relationship_diffs {
        for id in relationship . lost . iter ()
          . chain (relationship . gained . iter ())
          . chain (relationship . unchanged . iter ()) {
          titles . entry (id . clone ())
            . or_insert_with ( || report . titles . get (id)
              . cloned () . unwrap_or_else (
                || "[unknown title]" . to_string () ) ); }}
      if let Some (diff) = &node . contained_list_diff {
        for item in diff {
          let id : &ID = match item {
            ListDiffItem::Unchanged (id)
            | ListDiffItem::Removed (id)
            | ListDiffItem::Added (id) => id, };
          titles . entry (id . clone ())
            . or_insert_with ( || report . titles . get (id)
              . cloned () . unwrap_or_else (
                || "[unknown title]" . to_string () ) ); }} } }
  let prefix_len : usize =
    distinguishing_prefix_len (titles . keys ());
  titles . into_iter ()
    . map ( |(id, title)| {
      let prefix : String =
        id . 0 . chars () . take (prefix_len) . collect ();
      let title_head : String =
        title . chars () . take (40) . collect ();
      (id, format! ("{}..{}", prefix, title_head)) } )
    . collect ()
}

fn distinguishing_prefix_len<'a> (
  ids : impl Iterator<Item = &'a ID>,
) -> usize {
  let id_strings : Vec<&'a str> =
    ids . map ( |id| id . 0 . as_str () ) . collect ();
  let max_len : usize =
    id_strings . iter () . map ( |s| s . chars () . count () )
      . max () . unwrap_or (8);
  for len in 1..=max_len {
    let mut seen : BTreeSet<String> =
      BTreeSet::new ();
    let all_unique : bool =
      id_strings . iter () . all ( |id| {
        let prefix : String =
          id . chars () . take (len) . collect ();
        seen . insert (prefix)
      });
    if all_unique {
      return len . max (1); }}
  max_len
}
