#!/usr/bin/env python3
"""
Step 2 of 5 in the mangled→public port pipeline. See README.org.

Enumerates the post-BASE_COMMIT edits in MANGLED_DIR and determines
exactly which mangled UUIDs need translation via uuid_map. Uses
`git diff --name-status BASE HEAD` to partition changed files into
Added vs Modified, then for each Modified file computes the semantic
delta (title/body/misc + per-field inserts/deletes on list fields).

Outputs (under WORK):
  targets.json — full summary. Shape:
    {
      "added_files":    [uuid, ...],   # files new in mangled since BASE
      "modified_files": [uuid, ...],   # files edited in mangled since BASE
      "refs_in_added_needing_map":
                        [uuid, ...],   # UUIDs referenced by added files
                                       # that don't exist in public/ by own
                                       # UUID and aren't themselves added
      "contains_inserts": {file_uuid: {field: [uuid, ...]}},
      "contains_deletes": {file_uuid: {field: [uuid, ...]}},
      "title_changes":    {file_uuid: {from: .., to: ..}},
      "body_changes":     {file_uuid: {from: .., to: ..}},
      "misc_changes":     {file_uuid: {from: .., to: ..}},
      "needed_map_keys":  [uuid, ...]  # UUIDs step 05 will dereference
                                       # through uuid_map.
    }

The `needed_map_keys` set is the union of:
  - refs_in_added_needing_map
  - every modified_file whose own UUID isn't present in public/
  - every UUID deleted from a list-field that isn't in public/
  - every UUID inserted to a list-field that isn't in public/ and
    isn't itself an added file (added files enter public/ with their
    own UUIDs, so they don't need mapping)
"""

import json
import subprocess
from pathlib import Path

import yaml

MANGLED_DIR = Path("/home/ubuntu/data/public-1-mangled")
PUBLIC_DIR = Path("/home/ubuntu/data/public")
BASE_COMMIT = "181e2264"
WORK = Path(__file__).resolve().parent / "output"


def git_show(commit: str, relpath: str) -> str | None:
    """Return file contents at commit, or None if path didn't exist."""
    r = subprocess.run(
        ["git", "-C", str(MANGLED_DIR), "show", f"{commit}:./{relpath}"],
        capture_output=True,
    )
    if r.returncode != 0:
        return None
    return r.stdout.decode("utf-8", errors="replace")


def git_diff_names(base: str) -> list[tuple[str, str]]:
    """Return [(status, path), ...] between base and HEAD in MANGLED_DIR."""
    r = subprocess.run(
        ["git", "-C", str(MANGLED_DIR), "diff", "--name-status", base, "HEAD"],
        capture_output=True,
        check=True,
    )
    out = []
    for line in r.stdout.decode().splitlines():
        parts = line.split(maxsplit=1)
        if len(parts) != 2:
            continue
        out.append((parts[0], parts[1]))
    return out


def load_file(p: Path) -> dict | None:
    try:
        with open(p, "r", encoding="utf-8") as f:
            return yaml.safe_load(f)
    except Exception:
        return None


def load_yaml_text(txt: str) -> dict | None:
    try:
        return yaml.safe_load(txt)
    except Exception:
        return None


def all_refs(node: dict) -> list[str]:
    """All UUID-valued fields we care about."""
    out = []
    for k in (
        "contains",
        "subscribes_to",
        "hides_from_its_subscriptions",
        "overrides_view_of",
        "extra_ids",
        "textlinks_to",
    ):
        v = node.get(k)
        if not v:
            continue
        # MSV: may be str or list. Normalize to list.
        if isinstance(v, str):
            out.append(v)
        elif isinstance(v, list):
            for item in v:
                if isinstance(item, str):
                    out.append(item)
    return out


def main():
    changed = git_diff_names(BASE_COMMIT)
    added = [p[:-4] for st, p in changed if st == "A" and p.endswith(".skg")]
    modified = [p[:-4] for st, p in changed if st == "M" and p.endswith(".skg")]

    print(f"Added: {len(added)}, Modified: {len(modified)}")

    # Load current mangled nodes for each.
    mangled_nodes = {}
    for uu in added + modified:
        mangled_nodes[uu] = load_file(MANGLED_DIR / f"{uu}.skg")

    # Load base (pre-mod) nodes for modified.
    base_nodes = {}
    for uu in modified:
        txt = git_show(BASE_COMMIT, f"{uu}.skg")
        base_nodes[uu] = load_yaml_text(txt) if txt is not None else None

    # Load public nodes for modified (where they exist).
    public_nodes = {}
    for uu in modified + added:
        p = PUBLIC_DIR / f"{uu}.skg"
        public_nodes[uu] = load_file(p) if p.exists() else None

    # --- refs_in_added ---
    # All UUIDs referenced by the 22 added files, minus those that exist in
    # public/ by same UUID, minus the 22 themselves.
    refs_in_added: set[str] = set()
    for uu in added:
        for r in all_refs(mangled_nodes[uu]):
            refs_in_added.add(r)
    refs_in_added -= set(added)
    # Only keep those NOT already present in public.
    still_missing = {
        r for r in refs_in_added if not (PUBLIC_DIR / f"{r}.skg").exists()
    }

    # --- contains inserts / deletes per modified file ---
    list_fields = [
        "contains",
        "subscribes_to",
        "hides_from_its_subscriptions",
        "overrides_view_of",
        "extra_ids",
    ]

    def normalize_list(v) -> list[str]:
        if not v:
            return []
        if isinstance(v, str):
            return [v]
        if isinstance(v, list):
            return [x for x in v if isinstance(x, str)]
        return []

    inserts: dict = {}
    deletes: dict = {}
    title_changes: dict = {}
    body_changes: dict = {}
    misc_changes: dict = {}

    for uu in modified:
        base = base_nodes[uu] or {}
        head = mangled_nodes[uu] or {}
        if base.get("title") != head.get("title"):
            title_changes[uu] = {"from": base.get("title"), "to": head.get("title")}
        if base.get("body") != head.get("body"):
            body_changes[uu] = {"from": base.get("body"), "to": head.get("body")}
        if base.get("misc") != head.get("misc"):
            misc_changes[uu] = {"from": base.get("misc"), "to": head.get("misc")}
        for k in list_fields:
            b = normalize_list(base.get(k))
            h = normalize_list(head.get(k))
            if b == h:
                continue
            bset, hset = set(b), set(h)
            ins = [x for x in h if x not in bset]
            dele = [x for x in b if x not in hset]
            if ins:
                inserts.setdefault(uu, {})[k] = ins
            if dele:
                deletes.setdefault(uu, {})[k] = dele

    # Collect UUIDs that need mapping:
    needed: set[str] = set()
    needed |= still_missing
    # Modified files that don't exist in public/ by UUID: need to map.
    for uu in modified:
        if public_nodes.get(uu) is None:
            needed.add(uu)
    # UUIDs removed in contains lists — if they don't exist in public/, need
    # a map to find what to delete.
    for uu, d in deletes.items():
        for k, lst in d.items():
            for x in lst:
                if not (PUBLIC_DIR / f"{x}.skg").exists():
                    needed.add(x)
    # UUIDs inserted in contains lists — if they don't exist in public/ AND
    # aren't added, we need to find the public equivalent. If they ARE
    # added, we insert by their own UUID (no map needed).
    for uu, d in inserts.items():
        for k, lst in d.items():
            for x in lst:
                if x in added:
                    continue
                if not (PUBLIC_DIR / f"{x}.skg").exists():
                    needed.add(x)

    targets = {
        "added_files": sorted(added),
        "modified_files": sorted(modified),
        "refs_in_added_needing_map": sorted(still_missing),
        "contains_inserts": inserts,
        "contains_deletes": deletes,
        "title_changes": title_changes,
        "body_changes": body_changes,
        "misc_changes": misc_changes,
        "needed_map_keys": sorted(needed),
    }

    WORK.mkdir(parents=True, exist_ok=True)
    with open(WORK / "targets.json", "w") as f:
        json.dump(targets, f, indent=2, sort_keys=True)

    print(f"needed_map_keys: {len(needed)}")
    print(f"  -> {sorted(needed)}")
    print(f"title_changes: {len(title_changes)}")
    print(f"body_changes: {len(body_changes)}")
    print(f"misc_changes: {len(misc_changes)}")
    print(f"contains_inserts: {sum(len(d) for d in inserts.values())} keys across {len(inserts)} files")
    print(f"contains_deletes: {sum(len(d) for d in deletes.values())} keys across {len(deletes)} files")


if __name__ == "__main__":
    main()
