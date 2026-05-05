use git2::{Commit, ObjectType, Oid, Repository, Tree, TreeWalkMode, TreeWalkResult};
use serde::Deserialize;
use serde_yaml::Value;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::env;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_REPORT_PATH: &str = "compare-org-roam-import-report.org";

const SPECIAL_TITLE: &str = "These are special!";
const SPECIAL_BODY: &str = "They were super-indented in org-roam.";
const NORMAL_TITLE: &str = "These are normal.";
const NORMAL_BODY: &str = "They have been buried to encourage reading the node(s)\n\
   that were super-indented in org-roam.";

fn main() -> Result<(), Box<dyn Error>> {
    let report_path: PathBuf = env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(DEFAULT_REPORT_PATH));
    let root: PathBuf = env::current_dir()?;
    let collections: Vec<CollectionPair> = default_collection_pairs(&root);
    let mut report: Report = Report::new();
    for collection in collections {
        compare_collection(&collection, &mut report);
    }
    let text: String = report.to_org();
    fs::write(&report_path, text)?;
    println!("wrote {}", report_path.display());
    Ok(())
}

fn compare_collection(collection: &CollectionPair, report: &mut Report) {
    let mut collection_report: CollectionReport = CollectionReport::new(collection);
    let org_commit: Option<Oid> = read_head_commit_id(&collection.org_path, &mut collection_report);
    let data_commit: Option<Oid> =
        read_first_commit_id(&collection.data_path, &mut collection_report);
    if let (Some(org_commit), Some(data_commit)) = (org_commit, data_commit) {
        compare_collection_at_commits(collection, org_commit, data_commit, &mut collection_report);
    }
    report.collections.push(collection_report);
}

fn compare_collection_at_commits(
    collection: &CollectionPair,
    org_commit: Oid,
    data_commit: Oid,
    collection_report: &mut CollectionReport,
) {
    let org_blobs: Result<Vec<GitBlob>, String> =
        read_committed_blobs_with_extension(&collection.org_path, org_commit, "org");
    let skg_blobs: Result<Vec<GitBlob>, String> =
        read_committed_blobs_with_extension(&collection.data_path, data_commit, "skg");
    let org_blobs: Vec<GitBlob> = match org_blobs {
        Ok(blobs) => blobs,
        Err(e) => {
            collection_report.problem("git read failure", e);
            return;
        }
    };
    let skg_blobs: Vec<GitBlob> = match skg_blobs {
        Ok(blobs) => blobs,
        Err(e) => {
            collection_report.problem("git read failure", e);
            return;
        }
    };
    collection_report.org_commit = Some(org_commit.to_string());
    collection_report.data_commit = Some(data_commit.to_string());
    collection_report.org_files = org_blobs.len();
    collection_report.skg_files = skg_blobs.len();
    let org_parse_result: OrgParseResult = parse_org_blobs(&org_blobs, collection_report);
    let actual_nodes: HashMap<String, SkgNode> = parse_skg_blobs(&skg_blobs, collection_report);
    collection_report.expected_nodes = count_expected_nodes(&org_parse_result.roots);
    collection_report.actual_nodes = actual_nodes.len();
    compare_expected_roots_to_skg(
        &org_parse_result.roots,
        &actual_nodes,
        &org_parse_result.duplicate_ids,
        collection_report,
    );
}

fn read_head_commit_id(repo_path: &Path, collection_report: &mut CollectionReport) -> Option<Oid> {
    let repo: Repository = match Repository::open(repo_path) {
        Ok(repo) => repo,
        Err(e) => {
            collection_report.problem(
                "git repository missing",
                format!("{}: {}", repo_path.display(), e),
            );
            return None;
        }
    };
    match repo.head().and_then(|h| h.peel_to_commit()) {
        Ok(commit) => Some(commit.id()),
        Err(e) => {
            collection_report.problem(
                "git HEAD unreadable",
                format!("{}: {}", repo_path.display(), e),
            );
            None
        }
    }
}

fn read_first_commit_id(repo_path: &Path, collection_report: &mut CollectionReport) -> Option<Oid> {
    let repo: Repository = match Repository::open(repo_path) {
        Ok(repo) => repo,
        Err(e) => {
            collection_report.problem(
                "git repository missing",
                format!("{}: {}", repo_path.display(), e),
            );
            return None;
        }
    };
    let mut revwalk: git2::Revwalk = match repo.revwalk() {
        Ok(revwalk) => revwalk,
        Err(e) => {
            collection_report.problem(
                "git history unreadable",
                format!("{}: {}", repo_path.display(), e),
            );
            return None;
        }
    };
    if let Err(e) = revwalk.push_head() {
        collection_report.problem(
            "git history unreadable",
            format!("{}: {}", repo_path.display(), e),
        );
        return None;
    }
    let mut root_ids: Vec<Oid> = Vec::new();
    for id_result in revwalk {
        match id_result {
            Ok(id) => {
                if is_root_commit(&repo, id) {
                    root_ids.push(id);
                }
            }
            Err(e) => {
                collection_report.problem(
                    "git history unreadable",
                    format!("{}: {}", repo_path.display(), e),
                );
            }
        }
    }
    root_ids.sort();
    if root_ids.len() > 1 {
        collection_report.problem(
            "multiple root commits",
            format!(
                "{} has {} root commits; using {}",
                repo_path.display(),
                root_ids.len(),
                root_ids[0]
            ),
        );
    }
    root_ids.first().copied()
}

fn is_root_commit(repo: &Repository, id: Oid) -> bool {
    match repo.find_commit(id) {
        Ok(commit) => commit.parent_count() == 0,
        Err(_) => false,
    }
}

fn read_committed_blobs_with_extension(
    repo_path: &Path,
    commit_id: Oid,
    extension: &str,
) -> Result<Vec<GitBlob>, String> {
    let repo: Repository =
        Repository::open(repo_path).map_err(|e| format!("{}: {}", repo_path.display(), e))?;
    let commit: Commit = repo
        .find_commit(commit_id)
        .map_err(|e| format!("{} {}: {}", repo_path.display(), commit_id, e))?;
    let tree: Tree = commit
        .tree()
        .map_err(|e| format!("{} {}: {}", repo_path.display(), commit_id, e))?;
    let mut blobs: Vec<GitBlob> = Vec::new();
    let mut errors: Vec<String> = Vec::new();
    tree.walk(TreeWalkMode::PreOrder, |dir, entry| {
        if entry.kind() != Some(ObjectType::Blob) {
            return TreeWalkResult::Ok;
        }
        let name: &str = match entry.name() {
            Some(name) => name,
            None => return TreeWalkResult::Ok,
        };
        if !name.ends_with(&format!(".{}", extension)) {
            return TreeWalkResult::Ok;
        }
        let path: String = format!("{}{}", dir, name);
        match repo.find_blob(entry.id()) {
            Ok(blob) => match std::str::from_utf8(blob.content()) {
                Ok(content) => {
                    blobs.push(GitBlob {
                        path,
                        content: content.to_string(),
                    });
                }
                Err(e) => {
                    errors.push(format!("{} is not UTF-8: {}", path, e));
                }
            },
            Err(e) => {
                errors.push(format!("could not read {}: {}", path, e));
            }
        }
        TreeWalkResult::Ok
    })
    .map_err(|e| e.to_string())?;
    if errors.is_empty() {
        blobs.sort_by(|a, b| a.path.cmp(&b.path));
        Ok(blobs)
    } else {
        Err(errors.join("\n"))
    }
}

fn parse_org_blobs(blobs: &[GitBlob], collection_report: &mut CollectionReport) -> OrgParseResult {
    let mut roots: Vec<ExpectedNode> = Vec::new();
    let mut ids_seen: HashMap<String, Vec<String>> = HashMap::new();
    for blob in blobs {
        match parse_org_file(&blob.path, &blob.content) {
            Ok(node) => {
                record_expected_ids(&node, &mut ids_seen);
                roots.push(node);
            }
            Err(e) => {
                collection_report.problem("org parse failure", format!("{}: {}", blob.path, e));
            }
        }
    }
    let mut duplicate_ids: HashSet<String> = HashSet::new();
    for (id, origins) in ids_seen {
        if origins.len() > 1 {
            duplicate_ids.insert(id.clone());
            collection_report.problem(
                "duplicate org ID",
                format!("{} appears at {}", id, origins.join("; ")),
            );
        }
    }
    OrgParseResult {
        roots,
        duplicate_ids,
    }
}

fn parse_skg_blobs(
    blobs: &[GitBlob],
    collection_report: &mut CollectionReport,
) -> HashMap<String, SkgNode> {
    let mut nodes: HashMap<String, SkgNode> = HashMap::new();
    for blob in blobs {
        match parse_skg_blob(blob) {
            Ok(node) => {
                let filename_pid: Option<String> = Path::new(&blob.path)
                    .file_stem()
                    .map(|s| s.to_string_lossy().to_string());
                if filename_pid.as_ref() != Some(&node.pid) {
                    collection_report.problem(
                        "skg filename/pid mismatch",
                        format!("{} has pid {}", blob.path, node.pid),
                    );
                }
                if nodes.insert(node.pid.clone(), node).is_some() {
                    collection_report.problem(
                        "duplicate skg pid",
                        format!("{} defines a pid already seen", blob.path),
                    );
                }
            }
            Err(e) => {
                collection_report.problem("skg parse failure", format!("{}: {}", blob.path, e));
            }
        }
    }
    nodes
}

fn compare_expected_roots_to_skg(
    expected_roots: &[ExpectedNode],
    actual_nodes: &HashMap<String, SkgNode>,
    duplicate_ids: &HashSet<String>,
    collection_report: &mut CollectionReport,
) {
    let mut visited_actual_ids: HashSet<String> = HashSet::new();
    for node in expected_roots {
        match &node.id {
            Some(id) => {
                compare_expected_node_to_actual_id(
                    node,
                    id,
                    actual_nodes,
                    duplicate_ids,
                    &mut visited_actual_ids,
                    collection_report,
                );
            }
            None => {
                collection_report.problem("org root lacks ID", node.origin.clone());
            }
        }
    }
    let unvisited: Vec<&String> = actual_nodes
        .keys()
        .filter(|id| !visited_actual_ids.contains(*id))
        .collect();
    for id in unvisited.iter().take(200) {
        if let Some(node) = actual_nodes.get(*id) {
            collection_report.problem(
                "unmatched skg node",
                format!("{} title={}", id, org_escape_inline(&node.title)),
            );
        }
    }
    if unvisited.len() > 200 {
        collection_report.problem(
            "unmatched skg node",
            format!(
                "{} more unmatched skg nodes not listed",
                unvisited.len() - 200
            ),
        );
    }
}

fn compare_expected_node_to_actual_id(
    node: &ExpectedNode,
    actual_id: &str,
    actual_nodes: &HashMap<String, SkgNode>,
    duplicate_ids: &HashSet<String>,
    visited_actual_ids: &mut HashSet<String>,
    collection_report: &mut CollectionReport,
) {
    if node
        .id
        .as_ref()
        .map_or(false, |id| duplicate_ids.contains(id))
    {
        mark_actual_subtree_visited(actual_id, actual_nodes, visited_actual_ids);
        return;
    }
    if !visited_actual_ids.insert(actual_id.to_string()) {
        collection_report.problem(
            "skg node reached twice",
            format!("{} expected at {}", actual_id, node.origin),
        );
    }
    let actual: &SkgNode = match actual_nodes.get(actual_id) {
        Some(actual) => actual,
        None => {
            collection_report.problem(
                "missing skg node",
                format!("expected {} for {}", actual_id, node.origin),
            );
            return;
        }
    };
    compare_scalar_field(
        "title",
        &node.title,
        &actual.title,
        actual_id,
        &node.origin,
        collection_report,
    );
    compare_optional_field(
        "body",
        node.body.as_deref(),
        actual.body.as_deref(),
        actual_id,
        &node.origin,
        collection_report,
    );
    compare_aliases(
        &node.aliases,
        &actual.aliases,
        actual_id,
        &node.origin,
        collection_report,
    );
    compare_children(
        node,
        actual,
        actual_nodes,
        duplicate_ids,
        visited_actual_ids,
        collection_report,
    );
}

fn compare_children(
    node: &ExpectedNode,
    actual: &SkgNode,
    actual_nodes: &HashMap<String, SkgNode>,
    duplicate_ids: &HashSet<String>,
    visited_actual_ids: &mut HashSet<String>,
    collection_report: &mut CollectionReport,
) {
    if node.children.len() != actual.contains.len() {
        collection_report.problem(
            "contains length mismatch",
            format!(
                "{} at {}: expected {}, got {}",
                actual.pid,
                node.origin,
                node.children.len(),
                actual.contains.len()
            ),
        );
    }
    let shared_len: usize = node.children.len().min(actual.contains.len());
    for i in 0..shared_len {
        let expected_child: &ExpectedNode = &node.children[i];
        let actual_child_id: &str = &actual.contains[i];
        if let Some(expected_id) = &expected_child.id {
            if expected_id != actual_child_id {
                collection_report.problem(
                    "contains order/id mismatch",
                    format!(
                        "{} child {}: expected {}, got {} ({})",
                        actual.pid, i, expected_id, actual_child_id, expected_child.origin
                    ),
                );
            }
        }
        compare_expected_node_to_actual_id(
            expected_child,
            actual_child_id,
            actual_nodes,
            duplicate_ids,
            visited_actual_ids,
            collection_report,
        );
    }
}

fn mark_actual_subtree_visited(
    actual_id: &str,
    actual_nodes: &HashMap<String, SkgNode>,
    visited_actual_ids: &mut HashSet<String>,
) {
    if !visited_actual_ids.insert(actual_id.to_string()) {
        return;
    }
    if let Some(actual) = actual_nodes.get(actual_id) {
        for child_id in &actual.contains {
            mark_actual_subtree_visited(child_id, actual_nodes, visited_actual_ids);
        }
    }
}

fn compare_scalar_field(
    field: &str,
    expected: &str,
    actual: &str,
    actual_id: &str,
    origin: &str,
    collection_report: &mut CollectionReport,
) {
    if expected != actual {
        collection_report.problem(
            format!("{} mismatch", field),
            format!(
                "{} at {}\nexpected: {}\nactual: {}",
                actual_id,
                origin,
                org_escape_inline(expected),
                org_escape_inline(actual)
            ),
        );
    }
}

fn compare_optional_field(
    field: &str,
    expected: Option<&str>,
    actual: Option<&str>,
    actual_id: &str,
    origin: &str,
    collection_report: &mut CollectionReport,
) {
    if expected != actual {
        collection_report.problem(
            format!("{} mismatch", field),
            format!(
                "{} at {}\nexpected: {}\nactual: {}",
                actual_id,
                origin,
                org_escape_multiline(expected.unwrap_or("<none>")),
                org_escape_multiline(actual.unwrap_or("<none>"))
            ),
        );
    }
}

fn compare_aliases(
    expected: &[String],
    actual: &[String],
    actual_id: &str,
    origin: &str,
    collection_report: &mut CollectionReport,
) {
    if expected != actual {
        collection_report.problem(
            "aliases mismatch",
            format!(
                "{} at {}\nexpected: {:?}\nactual: {:?}",
                actual_id, origin, expected, actual
            ),
        );
    }
}

fn parse_org_file(path: &str, content: &str) -> Result<ExpectedNode, String> {
    let lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();
    if lines.is_empty() {
        return Err("empty org file".to_string());
    }
    let sections: Vec<OrgSection> = extract_sections(&lines, path);
    if sections.is_empty() {
        return Err("no org sections".to_string());
    }
    if sections[0].id.is_none() {
        return Err("file-level section has no :ID:".to_string());
    }
    let mut roots: Vec<SectionTree> = build_section_viewforest(sections, lines.len());
    if roots.len() != 1 {
        return Err(format!("expected one root, got {}", roots.len()));
    }
    let mut root: SectionTree = roots.remove(0);
    insert_super_indentation_groups(&mut root);
    Ok(expected_node_from_section_tree(&root, &lines, path))
}

fn extract_sections(lines: &[String], path: &str) -> Vec<OrgSection> {
    let mut sections: Vec<OrgSection> = Vec::new();
    let (file_id, file_aliases, props_end): (Option<String>, Vec<String>, usize) =
        parse_properties_block(lines, 0);
    let (file_title, title_line_end): (String, usize) = find_title(lines, props_end, path);
    sections.push(OrgSection {
        level: 0,
        headline_line: 0,
        headline: file_title,
        id: file_id,
        aliases: file_aliases,
        body_start: title_line_end,
        override_body: None,
        origin: path.to_string(),
    });
    for (i, line) in lines.iter().enumerate() {
        if !is_headline(line) {
            continue;
        }
        let level: usize = headline_level(line);
        let headline: String = headline_title(line);
        let (id, aliases, props_end): (Option<String>, Vec<String>, usize) =
            parse_properties_block(lines, i + 1);
        let body_start: usize = if props_end > i + 1 { props_end } else { i + 1 };
        sections.push(OrgSection {
            level,
            headline_line: i,
            headline,
            id,
            aliases,
            body_start,
            override_body: None,
            origin: format!("{}:{}", path, i + 1),
        });
    }
    sections
}

fn build_section_viewforest(sections: Vec<OrgSection>, num_lines: usize) -> Vec<SectionTree> {
    let mut viewforest: Vec<SectionTree> = Vec::new();
    let mut stack: Vec<SectionTree> = Vec::new();
    for section in sections {
        let level: usize = section.level;
        loop {
            let should_pop: bool = match stack.last() {
                Some(top) => top.section.level >= level,
                None => false,
            };
            if !should_pop {
                break;
            }
            let mut popped: SectionTree = stack.pop().unwrap();
            popped.extent_end = section.headline_line;
            if let Some(node) = stack.last_mut() {
                node.children.push(popped);
            } else {
                viewforest.push(popped);
            }
        }
        stack.push(SectionTree {
            section,
            extent_end: num_lines,
            children: Vec::new(),
        });
    }
    while let Some(popped) = stack.pop() {
        if let Some(node) = stack.last_mut() {
            node.children.push(popped);
        } else {
            viewforest.push(popped);
        }
    }
    viewforest
}

fn insert_super_indentation_groups(tree: &mut SectionTree) {
    for child in &mut tree.children {
        insert_super_indentation_groups(child);
    }
    let mut levels: Vec<usize> = tree.children.iter().map(|c| c.section.level).collect();
    levels.sort();
    levels.dedup();
    if levels.len() <= 1 {
        return;
    }
    let old_children: Vec<SectionTree> = tree.children.drain(..).collect();
    tree.children = group_children_by_level(old_children);
}

fn group_children_by_level(children: Vec<SectionTree>) -> Vec<SectionTree> {
    let mut levels: Vec<usize> = children.iter().map(|c| c.section.level).collect();
    levels.sort();
    levels.dedup();
    if levels.len() <= 1 {
        return children;
    }
    let highest_level: usize = *levels.last().unwrap();
    let (special, rest): (Vec<SectionTree>, Vec<SectionTree>) = children
        .into_iter()
        .partition(|c| c.section.level == highest_level);
    let normal_children: Vec<SectionTree> = group_children_by_level(rest);
    vec![
        synthetic_section_tree(SPECIAL_TITLE, SPECIAL_BODY, special),
        synthetic_section_tree(NORMAL_TITLE, NORMAL_BODY, normal_children),
    ]
}

fn synthetic_section_tree(title: &str, body: &str, children: Vec<SectionTree>) -> SectionTree {
    SectionTree {
        section: OrgSection {
            level: 0,
            headline_line: 0,
            headline: title.to_string(),
            id: None,
            aliases: Vec::new(),
            body_start: 0,
            override_body: Some(body.to_string()),
            origin: format!("synthetic: {}", title),
        },
        extent_end: 0,
        children,
    }
}

fn expected_node_from_section_tree(
    tree: &SectionTree,
    lines: &[String],
    path: &str,
) -> ExpectedNode {
    let body_end: usize = tree
        .children
        .first()
        .map(|c| c.section.headline_line)
        .unwrap_or(tree.extent_end);
    let body: Option<String> = tree
        .section
        .override_body
        .clone()
        .or_else(|| collect_body(lines, tree.section.body_start, body_end));
    let children: Vec<ExpectedNode> = tree
        .children
        .iter()
        .map(|child| expected_node_from_section_tree(child, lines, path))
        .collect();
    let origin: String = if tree.section.origin.starts_with("synthetic:") {
        format!("{} under {}", tree.section.origin, path)
    } else {
        tree.section.origin.clone()
    };
    ExpectedNode {
        id: tree.section.id.clone(),
        title: tree.section.headline.clone(),
        body,
        aliases: tree.section.aliases.clone(),
        origin,
        children,
    }
}

fn collect_body(lines: &[String], start: usize, end: usize) -> Option<String> {
    let mut body_lines: Vec<&str> = Vec::new();
    for i in start..end.min(lines.len()) {
        body_lines.push(&lines[i]);
    }
    while body_lines.last().map_or(false, |l| l.trim().is_empty()) {
        body_lines.pop();
    }
    if body_lines.is_empty() {
        None
    } else {
        Some(normalize_body_whitespace(&body_lines))
    }
}

fn normalize_body_whitespace(body_lines: &[&str]) -> String {
    let min_indent: usize = body_lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            l.chars()
                .take_while(|c| c.is_whitespace())
                .map(|c| c.len_utf8())
                .sum::<usize>()
        })
        .min()
        .unwrap_or(0);
    let stripped: Vec<&str> = body_lines
        .iter()
        .map(|l| {
            if l.len() >= min_indent {
                &l[min_indent..]
            } else {
                ""
            }
        })
        .collect();
    let has_false_headline: bool = stripped.iter().any(|l| is_headline(l));
    if has_false_headline {
        stripped
            .iter()
            .map(|l| {
                if l.is_empty() {
                    l.to_string()
                } else {
                    format!("  {}", l)
                }
            })
            .collect::<Vec<String>>()
            .join("\n")
    } else {
        stripped.join("\n")
    }
}

fn parse_properties_block(lines: &[String], start: usize) -> (Option<String>, Vec<String>, usize) {
    if start >= lines.len() {
        return (None, Vec::new(), start);
    }
    if !lines[start].trim().eq_ignore_ascii_case(":PROPERTIES:") {
        return (None, Vec::new(), start);
    }
    let mut id: Option<String> = None;
    let mut aliases: Vec<String> = Vec::new();
    let mut i: usize = start + 1;
    while i < lines.len() {
        let trimmed: &str = lines[i].trim();
        if trimmed.eq_ignore_ascii_case(":END:") {
            return (id, aliases, i + 1);
        }
        if let Some(value) = extract_property(trimmed, ":ID:") {
            id = Some(value.trim().to_string());
        }
        if let Some(value) = extract_property(trimmed, ":ROAM_ALIASES:") {
            aliases = parse_roam_aliases(value.trim());
        }
        i += 1;
    }
    (id, aliases, i)
}

fn extract_property<'a>(line: &'a str, prefix: &str) -> Option<&'a str> {
    if line.len() < prefix.len() {
        return None;
    }
    if line[..prefix.len()].eq_ignore_ascii_case(prefix) {
        Some(&line[prefix.len()..])
    } else {
        None
    }
}

fn parse_roam_aliases(value: &str) -> Vec<String> {
    let mut aliases: Vec<String> = Vec::new();
    let mut chars: std::iter::Peekable<std::str::Chars> = value.chars().peekable();
    loop {
        while matches!(chars.peek(), Some(' ' | '\t')) {
            chars.next();
        }
        if chars.peek().is_none() {
            break;
        }
        if chars.peek() == Some(&'"') {
            chars.next();
            let mut alias: String = String::new();
            loop {
                match chars.next() {
                    Some('"') => break,
                    Some(c) => alias.push(c),
                    None => break,
                }
            }
            if !alias.is_empty() {
                aliases.push(alias);
            }
        } else {
            let mut alias: String = String::new();
            loop {
                match chars.peek() {
                    Some(' ' | '\t') | None => break,
                    Some(_) => alias.push(chars.next().unwrap()),
                }
            }
            if !alias.is_empty() {
                aliases.push(alias);
            }
        }
    }
    aliases
}

fn find_title(lines: &[String], props_end: usize, path: &str) -> (String, usize) {
    for i in props_end..lines.len() {
        let trimmed: &str = lines[i].trim();
        if trimmed.to_lowercase().starts_with("#+title:") {
            return (trimmed["#+title:".len()..].trim().to_string(), i + 1);
        }
        if is_headline(&lines[i]) {
            break;
        }
    }
    let title: String = Path::new(path)
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "untitled".to_string());
    (title, props_end)
}

fn is_headline(line: &str) -> bool {
    if !line.starts_with('*') {
        return false;
    }
    let stars: usize = line.chars().take_while(|c| *c == '*').count();
    line.len() > stars && line.as_bytes()[stars] == b' '
}

fn headline_level(line: &str) -> usize {
    line.chars().take_while(|c| *c == '*').count()
}

fn headline_title(line: &str) -> String {
    let stars: usize = headline_level(line);
    line[stars..].trim_start().to_string()
}

fn parse_skg_blob(blob: &GitBlob) -> Result<SkgNode, String> {
    let raw: RawSkgNode = serde_yaml::from_str(&blob.content).map_err(|e| e.to_string())?;
    let pid: String = raw
        .pid
        .or_else(|| first_id_from_value(raw.ids))
        .ok_or_else(|| "missing pid/ids".to_string())?;
    let title: String = raw.title.ok_or_else(|| "missing title".to_string())?;
    Ok(SkgNode {
        pid,
        title,
        body: raw.body,
        aliases: raw.aliases.unwrap_or_default(),
        contains: raw.contains.unwrap_or_default(),
    })
}

fn first_id_from_value(value: Option<Value>) -> Option<String> {
    match value {
        Some(Value::Sequence(items)) => items
            .first()
            .and_then(|item| item.as_str())
            .map(|s| s.to_string()),
        Some(Value::String(s)) => Some(s),
        _ => None,
    }
}

fn record_expected_ids(node: &ExpectedNode, ids_seen: &mut HashMap<String, Vec<String>>) {
    if let Some(id) = &node.id {
        ids_seen
            .entry(id.clone())
            .or_default()
            .push(node.origin.clone());
    }
    for child in &node.children {
        record_expected_ids(child, ids_seen);
    }
}

fn count_expected_nodes(roots: &[ExpectedNode]) -> usize {
    roots.iter().map(count_expected_node).sum()
}

fn count_expected_node(node: &ExpectedNode) -> usize {
    1 + node.children.iter().map(count_expected_node).sum::<usize>()
}

fn default_collection_pairs(root: &Path) -> Vec<CollectionPair> {
    vec![
        CollectionPair::new(root, "deciduous", "deciduous"),
        CollectionPair::new(root, "mincit", "mincit"),
        CollectionPair::new(root, "ofiscal", "ofiscal"),
        CollectionPair::new(root, "personal-deprecated", "personal"),
        CollectionPair::new(root, "personal-ish", "personal-ish"),
        CollectionPair::new(root, "personal-most", "personal-most"),
        CollectionPair::new(root, "personal-proc", "personal-proc"),
        CollectionPair::new(root, "public", "public"),
    ]
}

fn org_escape_inline(s: &str) -> String {
    s.replace('\n', "\\n")
}

fn org_escape_multiline(s: &str) -> String {
    if s.contains('\n') {
        format!("\n#+begin_example\n{}\n#+end_example", s)
    } else {
        org_escape_inline(s)
    }
}

#[derive(Clone)]
struct CollectionPair {
    name: String,
    data_path: PathBuf,
    org_path: PathBuf,
}

impl CollectionPair {
    fn new(root: &Path, data_name: &str, org_name: &str) -> CollectionPair {
        CollectionPair {
            name: format!("{} / {}", data_name, org_name),
            data_path: root.join("data").join(data_name),
            org_path: root.join("org-roam").join(org_name),
        }
    }
}

struct GitBlob {
    path: String,
    content: String,
}

struct OrgSection {
    level: usize,
    headline_line: usize,
    headline: String,
    id: Option<String>,
    aliases: Vec<String>,
    body_start: usize,
    override_body: Option<String>,
    origin: String,
}

struct SectionTree {
    section: OrgSection,
    extent_end: usize,
    children: Vec<SectionTree>,
}

struct ExpectedNode {
    id: Option<String>,
    title: String,
    body: Option<String>,
    aliases: Vec<String>,
    origin: String,
    children: Vec<ExpectedNode>,
}

struct OrgParseResult {
    roots: Vec<ExpectedNode>,
    duplicate_ids: HashSet<String>,
}

#[derive(Deserialize)]
struct RawSkgNode {
    title: Option<String>,
    pid: Option<String>,
    ids: Option<Value>,
    body: Option<String>,
    aliases: Option<Vec<String>>,
    contains: Option<Vec<String>>,
}

struct SkgNode {
    pid: String,
    title: String,
    body: Option<String>,
    aliases: Vec<String>,
    contains: Vec<String>,
}

struct Report {
    collections: Vec<CollectionReport>,
}

impl Report {
    fn new() -> Report {
        Report {
            collections: Vec::new(),
        }
    }

    fn to_org(&self) -> String {
        let mut out: String = String::new();
        out.push_str("* org-roam import correspondence report\n");
        out.push_str("Generated by tools/compare-org-roam-import.\n");
        let total_problems: usize = self.collections.iter().map(|c| c.problem_count()).sum();
        out.push_str(&format!("\nTotal problems: {}\n", total_problems));
        for collection in &self.collections {
            collection.push_org(&mut out);
        }
        out
    }
}

struct CollectionReport {
    name: String,
    org_commit: Option<String>,
    data_commit: Option<String>,
    org_files: usize,
    skg_files: usize,
    expected_nodes: usize,
    actual_nodes: usize,
    problems: BTreeMap<String, Vec<String>>,
}

impl CollectionReport {
    fn new(collection: &CollectionPair) -> CollectionReport {
        CollectionReport {
            name: collection.name.clone(),
            org_commit: None,
            data_commit: None,
            org_files: 0,
            skg_files: 0,
            expected_nodes: 0,
            actual_nodes: 0,
            problems: BTreeMap::new(),
        }
    }

    fn problem<K, M>(&mut self, kind: K, message: M)
    where
        K: Into<String>,
        M: Into<String>,
    {
        self.problems
            .entry(kind.into())
            .or_default()
            .push(message.into());
    }

    fn problem_count(&self) -> usize {
        self.problems.values().map(Vec::len).sum()
    }

    fn push_org(&self, out: &mut String) {
        out.push_str(&format!("\n** {}\n", self.name));
        out.push_str(&format!(
            "- org commit: {}\n",
            self.org_commit.as_deref().unwrap_or("<unreadable>")
        ));
        out.push_str(&format!(
            "- data commit: {}\n",
            self.data_commit.as_deref().unwrap_or("<unreadable>")
        ));
        out.push_str(&format!(
            "- org files: {}, skg files: {}\n",
            self.org_files, self.skg_files
        ));
        out.push_str(&format!(
            "- expected nodes: {}, actual skg nodes: {}\n",
            self.expected_nodes, self.actual_nodes
        ));
        out.push_str(&format!("- problems: {}\n", self.problem_count()));
        if self.problems.is_empty() {
            out.push_str("\nNo correspondence problems found.\n");
            return;
        }
        for (kind, messages) in &self.problems {
            out.push_str(&format!("\n*** {} ({})\n", kind, messages.len()));
            let mut seen: BTreeSet<&String> = BTreeSet::new();
            for message in messages {
                if seen.insert(message) {
                    out.push_str("- ");
                    out.push_str(message);
                    out.push('\n');
                }
            }
        }
    }
}
