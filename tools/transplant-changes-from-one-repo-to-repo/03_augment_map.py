#!/usr/bin/env python3
"""
Step 3 of 5 in the mangled→public port pipeline. See README.org.

Recovers a common failure case from step 01: a mangled file whose
title was itself edited post-BASE_COMMIT. Its HEAD title won't match
anything in public/ (public/ was re-imported from the same source, so
it still has the pre-edit title). But its BASE title — fetched via
`git show BASE:./uuid.skg` — will match.

For every needed_map_key that's still unmapped AND is a modified
file, retry title-matching with the BASE title.

This step only looks at modified files (not added files — they're
new, so they shouldn't map to anything) and only at UUIDs step 05
will actually dereference (targets.json:needed_map_keys). It doesn't
try to rescue every unmapped UUID in the repo.

Input (under WORK):
  uuid_map.json, targets.json, public_index.json
Output:
  uuid_map.json         — updated in place with the new mappings
  augmented.json        — {uuid: {source, target|candidates|base_title}}
                          diagnostic of what this step did for each
                          attempted lookup
  still_unresolved.json — any needed_map_keys not mapped after this
                          step. In a well-behaved dataset this list
                          is empty.
"""

import json
import subprocess
from pathlib import Path

import yaml

WORK = Path("/home/ubuntu/mangle-work")
MANGLED_DIR = Path("/home/ubuntu/data/public-1-mangled")
BASE_COMMIT = "181e2264"


def git_show(commit: str, relpath: str) -> str | None:
    r = subprocess.run(
        ["git", "-C", str(MANGLED_DIR), "show", f"{commit}:./{relpath}"],
        capture_output=True,
    )
    if r.returncode != 0:
        return None
    return r.stdout.decode("utf-8", errors="replace")


def main():
    uuid_map = json.load(open(WORK / "uuid_map.json"))
    targets = json.load(open(WORK / "targets.json"))
    public = json.load(open(WORK / "public_index.json"))

    # Index public/ by title for fast lookup.
    pub_by_title: dict = {}
    for u, node in public.items():
        pub_by_title.setdefault(node.get("title", ""), []).append(u)

    needed = targets["needed_map_keys"]
    modified = set(targets["modified_files"])

    augmented: dict[str, dict] = {}  # uuid -> {source, target}

    for u in needed:
        if u in uuid_map:
            continue
        if u not in modified:
            # Not a modified file — skip. (Could also try base title for
            # other UUIDs if needed, but these were pre-existing nodes and
            # would have been matched by HEAD title already.)
            continue
        txt = git_show(BASE_COMMIT, f"{u}.skg")
        if txt is None:
            continue
        base_node = yaml.safe_load(txt)
        if not base_node:
            continue
        bt = base_node.get("title")
        if bt is None:
            continue
        matches = pub_by_title.get(bt, [])
        if len(matches) == 1:
            uuid_map[u] = matches[0]
            augmented[u] = {"source": "base_title", "target": matches[0]}
        elif len(matches) > 1:
            augmented[u] = {"source": "base_title_ambiguous", "candidates": matches}
        else:
            augmented[u] = {"source": "base_title_no_match", "base_title": bt}

    # Check for UUIDs referenced by added/removed contains, not modified files
    # themselves. E.g. 2a8b3d4a is in mangled (not modified, may be deleted
    # from a container). It should have been matched via HEAD title, but we
    # can also fall back to base if that failed. Since I didn't see any
    # such case above, just flag these if any remain.
    still: list[str] = []
    for u in needed:
        if u not in uuid_map:
            still.append(u)

    json.dump(uuid_map, open(WORK / "uuid_map.json", "w"), indent=2, sort_keys=True)
    json.dump(augmented, open(WORK / "augmented.json", "w"), indent=2, sort_keys=True)
    json.dump(still, open(WORK / "still_unresolved.json", "w"), indent=2, sort_keys=True)

    print(f"Augmented (newly mapped via base title): "
          f"{sum(1 for v in augmented.values() if v['source']=='base_title')}")
    print(f"Still unresolved: {len(still)}")
    for u in still:
        print(f"  {u}")


if __name__ == "__main__":
    main()
