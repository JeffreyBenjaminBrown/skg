#!/usr/bin/env python3
"""
Step 5 of 5 in the mangled→public port pipeline. See README.org.

Produces mangle.diff — a unified-diff patch that carries every
post-BASE_COMMIT edit from MANGLED_DIR over to PUBLIC_DIR:

    cd PUBLIC_DIR && git apply /path/to/mangle.diff

Added files (status 'A' in `git diff BASE HEAD` on MANGLED_DIR):
  - Emit as a new file in public/ under the same UUID.
  - Parse the mangled YAML, rewrite UUID references in list-valued
    fields through uuid_map (added UUIDs stay as themselves), then
    re-emit using skg's preferred format (body as block scalar).

Modified files (status 'M'):
  - Look up the public/ counterpart via uuid_map.
  - Read public/'s current text.
  - Apply the semantic delta (mangled BASE → mangled HEAD) as
    minimal text-level edits:
      title / body / misc / extra_ids: replace that field's span.
      list fields (contains etc): replace the whole list with
        map(mangled HEAD list). This exploits the step-04
        invariant — positions carry through faithfully.
  - Emit a unified diff of public/'s old vs new text.

Output (absolute, not under WORK):
  mangle.diff           — unified diff ready for `git apply`
  mangle-unresolved.org — per-file notes: no-op edits, missing
                          target files, parse warnings, etc.

Implementation notes:

- Rendering goes through yaml.safe_dump({key: value}, ...) rather
  than yaml.safe_dump(value, ...) to avoid PyYAML emitting `...`
  document-end markers on scalar-only dumps.
- Body is emitted by skg_fmt.emit_body, which mirrors skg's
  NodeFS::to_yaml — block scalar `body: |2-` when round-trippable,
  double-quoted single-line otherwise (for CR/NUL).
- For an existing public/ file, edits are text-level so that the
  diff is minimal (one field's span replaced per changed field).
  The span parser lives in skg_fmt.parse_fields.
"""

import difflib
import json
import subprocess
from pathlib import Path

import yaml

import skg_fmt as S

WORK = Path("/home/ubuntu/mangle-work")
MANGLED_DIR = Path("/home/ubuntu/data/public-1-mangled")
PUBLIC_DIR = Path("/home/ubuntu/data/public")
BASE_COMMIT = "181e2264"
OUT_DIFF = Path("/home/ubuntu/mangle.diff")
OUT_UNRES = Path("/home/ubuntu/mangle-unresolved.org")


def git_show(commit: str, relpath: str) -> str | None:
    r = subprocess.run(
        ["git", "-C", str(MANGLED_DIR), "show", f"{commit}:./{relpath}"],
        capture_output=True,
    )
    if r.returncode != 0:
        return None
    return r.stdout.decode("utf-8", errors="replace")


def normalize_list(v) -> list[str]:
    if not v:
        return []
    if isinstance(v, str):
        return [v]
    if isinstance(v, list):
        return [x for x in v if isinstance(x, str)]
    return []


def map_uuid(uuid_map: dict, added: set[str], u: str) -> str:
    """Translate a mangled UUID to its public counterpart.

    - If u is one of the newly-added files, keep it (added files enter
      public/ with their original UUID).
    - Else, use uuid_map. If not in the map, leave as-is (caller should
      warn).
    """
    if u in added:
        return u
    return uuid_map.get(u, u)


# --- rendering ---

def render_field(key: str, value) -> str:
    """Emit a single YAML field as '<key>: <value>\\n' (possibly multi-line for
    lists), matching what serde_yaml produces in skg."""
    return yaml.safe_dump(
        {key: value},
        default_flow_style=False,
        width=100000,
        allow_unicode=True,
        sort_keys=False,
    )


def render_list_field(key: str, items: list) -> str:
    if not items:
        return ""
    return render_field(key, items)


def render_title(title: str) -> str:
    return render_field("title", title)


def render_pid(pid: str) -> str:
    return render_field("pid", pid)


def render_added_file(mangled_text: str, mangled_node: dict,
                      uuid_map: dict, added: set[str]) -> str:
    """Produce the new file content for an added .skg.

    We mostly preserve the mangled text but:
    - rewrite UUIDs in list-items that need mapping,
    - re-emit the body field as a block scalar if it was in a form that
      won't round-trip cleanly (single-line `"...\n..."`).
    """
    # Work at the semantic level: parse, rewrite, re-emit, to be safe.
    node = mangled_node

    # Rewrite list/str UUID fields.
    for field in (
        "contains",
        "extra_ids",
        "subscribes_to",
        "hides_from_its_subscriptions",
        "overrides_view_of",
        "textlinks_to",
    ):
        v = node.get(field)
        if isinstance(v, list):
            node[field] = [map_uuid(uuid_map, added, x) if isinstance(x, str) else x for x in v]
        elif isinstance(v, str):
            node[field] = map_uuid(uuid_map, added, v)

    return render_node(node)


def render_node(node: dict) -> str:
    """Render a node dict back to YAML in skg's preferred format."""
    out = ""
    out += render_title(node.get("title", ""))
    if node.get("aliases"):
        aliases = node["aliases"]
        if isinstance(aliases, str):
            aliases = [aliases]
        out += render_list_field("aliases", aliases)
    out += render_pid(node["pid"])
    if node.get("extra_ids"):
        out += render_list_field("extra_ids", node["extra_ids"])
    if node.get("body") is not None:
        out += S.emit_body(node["body"])
    if node.get("contains"):
        out += render_list_field("contains", node["contains"])
    if node.get("subscribes_to"):
        v = node["subscribes_to"]
        if isinstance(v, str):
            v = [v]
        out += render_list_field("subscribes_to", v)
    if node.get("hides_from_its_subscriptions"):
        v = node["hides_from_its_subscriptions"]
        if isinstance(v, str):
            v = [v]
        out += render_list_field("hides_from_its_subscriptions", v)
    if node.get("overrides_view_of"):
        v = node["overrides_view_of"]
        if isinstance(v, str):
            v = [v]
        out += render_list_field("overrides_view_of", v)
    if node.get("misc"):
        out += render_list_field("misc", node["misc"])
    return out


# --- editing modified files ---

def apply_modifications(public_text: str, mangled_base: dict,
                        mangled_head: dict, uuid_map: dict,
                        added: set[str]) -> tuple[str, list[str]]:
    """Apply the semantic delta mangled_base -> mangled_head to public_text.

    Returns (new_text, warnings).
    """
    warnings: list[str] = []
    spans = S.parse_fields(public_text)

    # Title change.
    if mangled_base.get("title") != mangled_head.get("title"):
        new_title = mangled_head.get("title", "")
        span = S.find_span(spans, "title")
        if span is None:
            warnings.append("no 'title' span in public file")
        else:
            new_text = render_title(new_title)
            public_text = S.replace_span(public_text, span, new_text)
            spans = S.parse_fields(public_text)

    # Body change.
    if mangled_base.get("body") != mangled_head.get("body"):
        head_body = mangled_head.get("body")
        span = S.find_span(spans, "body")
        if head_body is None:
            # Remove the body field.
            if span is not None:
                public_text = S.replace_span(public_text, span, "")
                spans = S.parse_fields(public_text)
        else:
            new_text = S.emit_body(head_body)
            if span is None:
                # Insert body field after pid (or after extra_ids if present).
                # Per FIELD_ORDER, body goes right after extra_ids; else after pid.
                for anchor in ("extra_ids", "pid"):
                    anchor_span = S.find_span(spans, anchor)
                    if anchor_span is not None:
                        public_text = S.insert_span_after(public_text, spans, anchor, new_text)
                        break
                spans = S.parse_fields(public_text)
            else:
                public_text = S.replace_span(public_text, span, new_text)
                spans = S.parse_fields(public_text)

    # Misc change.
    if mangled_base.get("misc") != mangled_head.get("misc"):
        head_misc = mangled_head.get("misc") or []
        span = S.find_span(spans, "misc")
        new_text = render_list_field("misc", head_misc) if head_misc else ""
        if span is None:
            if new_text:
                public_text = public_text if public_text.endswith("\n") else public_text + "\n"
                public_text += new_text
                spans = S.parse_fields(public_text)
        else:
            public_text = S.replace_span(public_text, span, new_text)
            spans = S.parse_fields(public_text)

    # List-field changes (contains, subscribes_to, etc.).
    #
    # Relies on step 04's invariant: mapping the mangled BASE list
    # through uuid_map yields the public/ list element-for-element. So
    # the target public/ list is just map(mangled HEAD list) — no need
    # to compute individual insert/delete positions. Replace in one
    # shot. If the invariant is violated for a file, the result is
    # "mangled HEAD's order and content, translated to public UUIDs" —
    # which may reorder public/'s list, but the content is correct.
    for field in ("contains", "subscribes_to",
                  "hides_from_its_subscriptions", "overrides_view_of",
                  "extra_ids"):
        base_list = normalize_list(mangled_base.get(field))
        head_list = normalize_list(mangled_head.get(field))
        if base_list == head_list:
            continue
        target = [map_uuid(uuid_map, added, x) for x in head_list]
        span = S.find_span(spans, field)
        new_text = render_list_field(field, target) if target else ""
        if span is None:
            if new_text:
                # Find anchor: field should appear at its position in
                # FIELD_ORDER. Insert before the next present field.
                i = S.FIELD_ORDER.index(field)
                inserted = False
                for later in S.FIELD_ORDER[i + 1:]:
                    if S.find_span(spans, later) is not None:
                        public_text = S.insert_span_before(public_text, spans, later, new_text)
                        inserted = True
                        break
                if not inserted:
                    public_text = public_text if public_text.endswith("\n") else public_text + "\n"
                    public_text += new_text
                spans = S.parse_fields(public_text)
        else:
            public_text = S.replace_span(public_text, span, new_text)
            spans = S.parse_fields(public_text)

    return public_text, warnings


# --- diff output ---

def unified_diff(old_text: str, new_text: str, old_path: str, new_path: str) -> str:
    """Produce a git-apply-compatible unified diff. Empty strings are OK for
    new/deleted files.

    difflib.unified_diff alone doesn't produce the `diff --git a/ b/` header
    line or the `new file mode` / `deleted file mode` lines that git apply
    requires for creations and deletions. We synthesize that header manually
    and then splice in difflib's hunks (minus its own `---` / `+++` lines,
    which we replace with the git-style forms)."""
    # git apply expects:
    #   diff --git a/path b/path
    #   new file mode 100644
    #   --- /dev/null
    #   +++ b/path
    #   @@ ...
    is_new = old_text == "" and new_text != ""
    is_del = new_text == "" and old_text != ""
    old_lines = old_text.splitlines(keepends=True)
    new_lines = new_text.splitlines(keepends=True)
    hunks = list(difflib.unified_diff(
        old_lines, new_lines,
        fromfile=("a/" + old_path) if not is_new else "/dev/null",
        tofile=("b/" + new_path) if not is_del else "/dev/null",
        lineterm="",
    ))
    if not hunks:
        return ""
    body = []
    # difflib's '---' and '+++' lines end without newline (lineterm=""). Add
    # newlines ourselves.
    header = [
        f"diff --git a/{new_path} b/{new_path}\n",
    ]
    if is_new:
        header.append("new file mode 100644\n")
        header.append(f"--- /dev/null\n")
        header.append(f"+++ b/{new_path}\n")
    elif is_del:
        header.append("deleted file mode 100644\n")
        header.append(f"--- a/{old_path}\n")
        header.append(f"+++ /dev/null\n")
    else:
        header.append(f"--- a/{old_path}\n")
        header.append(f"+++ b/{new_path}\n")
    # Skip difflib's own '---' and '+++' lines. First two lines of output
    # from unified_diff are the from/to headers.
    hunks = hunks[2:]
    # Ensure every hunk line ends with '\n' (difflib preserves original
    # terminators, but for safety).
    body_lines = []
    for h in hunks:
        if not h.endswith("\n"):
            body_lines.append(h + "\n")
        else:
            body_lines.append(h)
    return "".join(header) + "".join(body_lines)


def main():
    uuid_map = json.load(open(WORK / "uuid_map.json"))
    targets = json.load(open(WORK / "targets.json"))
    added = set(targets["added_files"])
    modified = set(targets["modified_files"])

    diffs: list[str] = []
    unresolved: list[str] = []

    # Added files: sorted for stable output.
    for u in sorted(added):
        p = MANGLED_DIR / f"{u}.skg"
        mangled_text = p.read_text(encoding="utf-8")
        mangled_node = yaml.safe_load(mangled_text)
        if mangled_node is None:
            unresolved.append(f"- Added file {u}: could not parse mangled YAML")
            continue
        new_text = render_added_file(mangled_text, mangled_node, uuid_map, added)
        rel = f"{u}.skg"
        diffs.append(unified_diff("", new_text, rel, rel))

    # Modified files.
    for u in sorted(modified):
        pub_uuid = uuid_map.get(u, u)
        pub_path = PUBLIC_DIR / f"{pub_uuid}.skg"
        if not pub_path.exists():
            unresolved.append(
                f"- Modified {u} -> public {pub_uuid}: TARGET FILE MISSING in public/"
            )
            continue
        public_text = pub_path.read_text(encoding="utf-8")
        # Load base and head.
        base_txt = git_show(BASE_COMMIT, f"{u}.skg")
        base_node = yaml.safe_load(base_txt) if base_txt else {}
        head_node = yaml.safe_load((MANGLED_DIR / f"{u}.skg").read_text()) or {}
        new_text, warns = apply_modifications(public_text, base_node, head_node,
                                              uuid_map, added)
        for w in warns:
            unresolved.append(f"- Modified {u} -> {pub_uuid}: {w}")
        rel = f"{pub_uuid}.skg"
        d = unified_diff(public_text, new_text, rel, rel)
        if d:
            diffs.append(d)
        else:
            unresolved.append(
                f"- Modified {u} -> {pub_uuid}: no effective change — "
                "public/ already matches the edit (no action needed)."
            )

    OUT_DIFF.write_text("".join(diffs), encoding="utf-8")
    if unresolved:
        preamble = (
            "* Unresolved / manual-review items for mangle.diff\n\n"
            f"Generated for BASE={BASE_COMMIT} in data/public-1-mangled/\n\n"
        )
        OUT_UNRES.write_text(preamble + "\n".join(unresolved) + "\n",
                              encoding="utf-8")
    else:
        OUT_UNRES.write_text("* No unresolved items.\n", encoding="utf-8")

    print(f"Wrote {OUT_DIFF}  ({sum(1 for d in diffs if d)} file diffs)")
    print(f"Wrote {OUT_UNRES}  ({len(unresolved)} items)")


if __name__ == "__main__":
    main()
