#!/usr/bin/env python3
"""
Step 1 of 5 in the mangled→public port pipeline. See README.org.

Builds a UUID map mangled → public, keyed primarily by title and
disambiguated by body and by contains-relation overlap. The map lets
later steps translate "edit to UUID X in mangled" into "edit to UUID
X' in public/" when idless org headlines have been assigned different
random UUIDs by two separate imports of the same source.

Input:
  MANGLED_DIR/*.skg  — every .skg in the old import repo (HEAD)
  PUBLIC_DIR/*.skg   — every .skg in the new import directory

Output (under WORK):
  uuid_map.json       — {mangled_uuid: public_uuid}
  unmapped.json       — {mangled_uuid: {reason, title, [candidates]}}
  mangled_index.json  — {uuid: node_dict}   (mangled HEAD, JSON-coerced)
  public_index.json   — {uuid: node_dict}   (public HEAD, JSON-coerced)

The indices aren't required by later steps' logic but are convenient
for ad-hoc inspection.

Strategy, in escalating passes:
  1. Pass-through: UUIDs present in both repos map to themselves. This
     covers all nodes whose org headline had an explicit :ID: when
     imported — both imports assigned the same UUID.
  2. Title match: for each remaining mangled UUID, find public/ files
     with the same title. 0 → unmapped (reason: no title match); 1 →
     mapped; ≥2 → escalate.
  3. Body match: among title-tied candidates, keep those whose body
     also equals the mangled body. 1 → mapped; ≥2 → keep for step 4;
     0 → also keep the original set for step 4 (many nodes are
     bodyless so body doesn't discriminate).
  4. Contains-relation overlap, iterated: for each remaining
     ambiguous mapping, score each candidate by how many of the
     mangled node's already-mapped parents and children also appear
     in the candidate's parent/child set. Highest strictly-unique
     score wins. Loop until no progress (bounded at 5 iterations).
"""

import json
import os
from collections import defaultdict
from pathlib import Path

import yaml

MANGLED_DIR = Path("/home/ubuntu/data/public-1-mangled")
PUBLIC_DIR  = Path("/home/ubuntu/data/public")
WORK = Path(__file__).resolve().parent / "output"


def load_dir(p: Path) -> dict:
    """Return {uuid: node_dict} for all .skg in dir."""
    out = {}
    for f in p.glob("*.skg"):
        try:
            with open(f, "r", encoding="utf-8") as fh:
                data = yaml.safe_load(fh)
        except Exception as e:
            print(f"ERR parsing {f}: {e}")
            continue
        if not isinstance(data, dict):
            continue
        pid = data.get("pid")
        if not pid:
            continue
        out[pid] = data
    return out


def build_map(mangled: dict, public: dict) -> tuple[dict, dict]:
    """Return (uuid_map, unmapped).

    uuid_map: mangled_uuid → public_uuid
    unmapped: {mangled_uuid: reason} for cases we couldn't resolve.
    """
    uuid_map: dict[str, str] = {}
    unmapped: dict[str, dict] = {}

    # 1) Pass-through for UUIDs present in both repos.
    for u in mangled:
        if u in public:
            uuid_map[u] = u

    # Index public files by title.
    pub_by_title: dict[str, list[str]] = defaultdict(list)
    for u, node in public.items():
        pub_by_title[node.get("title", "")].append(u)

    # Candidates: mangled UUIDs still unmapped (not in public by UUID).
    unresolved: dict[str, list[str]] = {}
    for u, node in mangled.items():
        if u in uuid_map:
            continue
        title = node.get("title", "")
        cands = list(pub_by_title.get(title, []))
        # Don't let u (which is unmapped = not in public by same UUID) steal
        # a public UUID that's already the pass-through counterpart of some
        # other mangled UUID. Since pass-through is same-UUID, filtering out
        # any public candidate that also appears in mangled achieves that.
        cands = [c for c in cands if c not in mangled]
        if len(cands) == 0:
            unmapped[u] = {"reason": "no title match", "title": title}
        elif len(cands) == 1:
            uuid_map[u] = cands[0]
        else:
            unresolved[u] = cands

    # 2) Body disambiguation.
    still: dict[str, list[str]] = {}
    for u, cands in unresolved.items():
        mbody = mangled[u].get("body")
        matching = [c for c in cands if public[c].get("body") == mbody]
        if len(matching) == 1:
            uuid_map[u] = matching[0]
        elif len(matching) > 1:
            still[u] = matching
        else:
            # No body match — keep all title cands for contains step.
            still[u] = cands

    # 3) Contains-relation disambiguation, iterated.
    #
    # For each remaining mangled UUID U with candidates [C1, C2, ...]:
    #   - Look at U's children (contains list) in mangled; map them through
    #     uuid_map. The candidate whose children best overlap wins.
    #   - Also look at U's parents (files that contain U) in mangled; map
    #     them. The candidate most often contained by those mapped parents
    #     wins.
    #
    # Repeat until no progress or all resolved.

    # Pre-compute: parents[v] = list of files that contain v (in each repo).
    def parents(idx: dict) -> dict[str, list[str]]:
        out = defaultdict(list)
        for u, node in idx.items():
            for c in node.get("contains") or []:
                out[c].append(u)
        return out

    mangled_parents = parents(mangled)
    public_parents = parents(public)

    def score(u_mangled: str, c_public: str) -> int:
        s = 0
        # Child overlap.
        mchildren = mangled[u_mangled].get("contains") or []
        pchildren = set(public[c_public].get("contains") or [])
        for mc in mchildren:
            pc = uuid_map.get(mc)
            if pc and pc in pchildren:
                s += 1
        # Parent overlap.
        mps = mangled_parents.get(u_mangled, [])
        pps = set(public_parents.get(c_public, []))
        for mp in mps:
            pp = uuid_map.get(mp)
            if pp and pp in pps:
                s += 1
        return s

    for _ in range(5):
        progress = False
        next_still: dict[str, list[str]] = {}
        for u, cands in still.items():
            scores = [(score(u, c), c) for c in cands]
            scores.sort(reverse=True)
            top, top_c = scores[0]
            # Require strictly-best score AND nonzero signal.
            if top > 0 and (len(scores) == 1 or scores[1][0] < top):
                uuid_map[u] = top_c
                progress = True
            else:
                next_still[u] = cands
        still = next_still
        if not progress:
            break

    for u, cands in still.items():
        unmapped[u] = {
            "reason": "ambiguous after title+body+contains",
            "title": mangled[u].get("title", ""),
            "candidates": cands,
        }

    return uuid_map, unmapped


def main():
    print("Loading mangled...")
    mangled = load_dir(MANGLED_DIR)
    print(f"  {len(mangled)} nodes")
    print("Loading public...")
    public = load_dir(PUBLIC_DIR)
    print(f"  {len(public)} nodes")

    print("Building map...")
    uuid_map, unmapped = build_map(mangled, public)
    print(f"  mapped: {len(uuid_map)} / {len(mangled)}")
    print(f"  unmapped: {len(unmapped)}")

    WORK.mkdir(parents=True, exist_ok=True)
    with open(WORK / "uuid_map.json", "w") as f:
        json.dump(uuid_map, f, indent=2, sort_keys=True)
    with open(WORK / "unmapped.json", "w") as f:
        json.dump(unmapped, f, indent=2, sort_keys=True)

    # Save indices, coercing non-JSON-native values (e.g. date) to strings.
    def coerce(v):
        if isinstance(v, (str, int, float, bool)) or v is None:
            return v
        if isinstance(v, list):
            return [coerce(x) for x in v]
        if isinstance(v, dict):
            return {str(k): coerce(x) for k, x in v.items()}
        return str(v)

    def to_serializable(idx):
        return {u: coerce(node) for u, node in idx.items()}

    with open(WORK / "mangled_index.json", "w") as f:
        json.dump(to_serializable(mangled), f, indent=2, sort_keys=True)
    with open(WORK / "public_index.json", "w") as f:
        json.dump(to_serializable(public), f, indent=2, sort_keys=True)

    print("Done.")


if __name__ == "__main__":
    main()
