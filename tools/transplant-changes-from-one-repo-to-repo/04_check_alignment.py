#!/usr/bin/env python3
"""
Step 4 of 5 in the mangled→public port pipeline. See README.org.

Sanity check on the load-bearing invariant step 05 relies on:

    for every modified mangled file F with counterpart F' in public/,
    and for every list-valued field (contains, subscribes_to, ...):
      map_uuid(mangled_base[F][field])  ==  public[F'][field]

If this holds, step 05 can translate mangled's list-edit semantics
to public/ by simply replacing public's list with
map(mangled_HEAD's list) — positions carry over faithfully because
the two lists aligned element-for-element before the edits.

If it fails for some (file, field), the mangled edit can't be
position-translated to public/ via the map alone. Step 05 still
produces a patch for that file, but the result is just
map(mangled_HEAD_list) clobbering public/'s list — correct UUIDs,
but relative ordering is whatever mangled HEAD had, not whatever
public/ HEAD had.

This script only prints; no output files. Scan for 'BAD' in its
stdout before trusting step 05's output for a given file.
"""

import json
import subprocess
from pathlib import Path

import yaml

WORK = Path(__file__).resolve().parent / "output"
MANGLED_DIR = Path("/home/ubuntu/data/public-1-mangled")
PUBLIC_DIR = Path("/home/ubuntu/data/public")
BASE_COMMIT = "181e2264"


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


def main():
    uuid_map = json.load(open(WORK / "uuid_map.json"))
    targets = json.load(open(WORK / "targets.json"))

    modified = targets["modified_files"]

    for u in modified:
        pub_uuid = uuid_map.get(u, u)
        # Base
        base_txt = git_show(BASE_COMMIT, f"{u}.skg")
        base_node = yaml.safe_load(base_txt) if base_txt else {}
        # Public
        p = PUBLIC_DIR / f"{pub_uuid}.skg"
        public_node = yaml.safe_load(open(p)) if p.exists() else None
        if public_node is None:
            print(f"{u} -> {pub_uuid}: public/ FILE MISSING (target)")
            continue
        for field in ("contains", "subscribes_to",
                      "hides_from_its_subscriptions", "overrides_view_of"):
            base_list = normalize_list(base_node.get(field))
            public_list = normalize_list(public_node.get(field))
            if not base_list and not public_list:
                continue
            mapped = [uuid_map.get(x, f"?{x}") for x in base_list]
            ok = mapped == public_list
            tag = "OK " if ok else "BAD"
            print(f"{tag} {u} -> {pub_uuid}  [{field}]")
            if not ok:
                print(f"     base        : {base_list}")
                print(f"     map(base)   : {mapped}")
                print(f"     public      : {public_list}")


if __name__ == "__main__":
    main()
