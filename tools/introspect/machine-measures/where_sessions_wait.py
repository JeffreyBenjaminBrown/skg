"""Split each transcript's time into model time and tool time by kind.

Usage, from the repo root:
  python3 -I tools/introspect/machine-measures/where_sessions_wait.py \
    my-dot-claude/projects/-home-ubuntu-host data/claude/spending/account.tsv 2026-11-10
Env: ALLCMDS=1 lists every Bash call, not only tests and builds;
TOP=N prints the N longest.

Each gap between consecutive assistant/user records is attributed to
what ended it: an assistant record -> 'model'; a tool_result -> that
tool's category (Bash commands are sorted by words in the command:
all-tests, integration, nextest/cargo test, client tests, cargo build,
skg_for_ai); a task notification -> 'bg-wait'; a human prompt ->
'idle' (excluded).  Gaps over 30 minutes are counted as idle.
Groups are the account's kind:origin:mission_kind.  A session's
'subagent-wait' and 'bg-wait' are mostly time its subagents spent,
which their own rows count again, so subtract them before summing.
Known misses: a command that runs tests under another name lands in
'bash:other'; a loop that waits for another session's cargo (pgrep)
is counted as the test it waits for, and printed separately.
"""
import json, os, re, sys, glob, collections
from datetime import datetime

root, account, since = sys.argv[1], sys.argv[2], sys.argv[3]

kinds = {}
with open(account) as f:
    hdr = f.readline().rstrip("\n").split("\t")
    for line in f:
        r = dict(zip(hdr, line.rstrip("\n").split("\t")))
        kinds[r["id"]] = (r["kind"], r["origin"], r["mission_kind"], r["specialist"])

def cat_bash(cmd):
    c = cmd
    if "all-tests.sh" in c: return "test:all-tests"
    if "integration-tests.sh" in c or "run-all-tests.sh" in c or "tests/integration/" in c and ".sh" in c:
        return "test:integration"
    if "nextest" in c or re.search(r"cargo\s+test", c): return "test:rust"
    if "emacs-tests.sh" in c or "nvim-tests.sh" in c or "ert-run" in c or "--batch" in c and "emacs" in c:
        return "test:client"
    if re.search(r"cargo\s+(build|check|clippy|run|b\b)", c): return "build"
    if "skg_for_ai" in c: return "graph-tool"
    if "pytest" in c or "test_" in c and "python" in c: return "test:python"
    return "bash:other"

def ts(s):
    return datetime.fromisoformat(s.replace("Z", "+00:00")).timestamp()

tot = collections.defaultdict(lambda: collections.Counter())
longest = []
files = glob.glob(os.path.join(root, "*.jsonl")) + glob.glob(os.path.join(root, "*/subagents/*.jsonl"))
for fn in files:
    if datetime.fromtimestamp(os.path.getmtime(fn)).strftime("%Y-%m-%d") < since:
        continue
    stem = os.path.basename(fn)[:-6]
    rid = stem[len("agent-"):] if stem.startswith("agent-") else stem
    k = kinds.get(rid)
    if not k:
        group = "unaccounted"
    else:
        kind, origin, mk, sp = k
        group = f"{kind}:{origin}" + (f":{mk}" if mk else "")
    uses = {}
    prev = None
    recs = []
    with open(fn) as f:
        for line in f:
            try: r = json.loads(line)
            except Exception: continue
            if r.get("type") not in ("assistant", "user") or "timestamp" not in r: continue
            recs.append(r)
    for r in recs:
        t = ts(r["timestamp"])
        content = (r.get("message") or {}).get("content")
        label = None
        if r["type"] == "assistant":
            label = "model"
            if isinstance(content, list):
                for b in content:
                    if b.get("type") == "tool_use":
                        name = b.get("name")
                        inp = b.get("input") or {}
                        if name == "Bash":
                            c = cat_bash(inp.get("command", ""))
                            if inp.get("run_in_background"): c = "bg-launch"
                        elif name in ("Agent", "Task"): c = "subagent-wait"
                        elif name in ("Read", "Edit", "Write", "Grep", "Glob"): c = "file-tools"
                        else: c = "tool:" + str(name)
                        uses[b.get("id")] = (c, inp.get("command", "")[:120] if name == "Bash" else name)
        else:
            if isinstance(content, list) and any(b.get("type") == "tool_result" for b in content if isinstance(b, dict)):
                tid = next(b.get("tool_use_id") for b in content if isinstance(b, dict) and b.get("type") == "tool_result")
                label, cmd = uses.get(tid, ("tool:?", ""))
                if prev is not None and (label.startswith(("test", "build")) or os.environ.get("ALLCMDS")):
                    longest.append((t - prev, group, label, cmd))
            else:
                text = json.dumps(content)[:400] if content else ""
                label = "bg-wait" if "task-notification" in text or "task_notification" in text else "idle"
        if prev is not None and label:
            gap = t - prev
            if gap < 0: gap = 0
            if gap > 1800: label = "idle"
            tot[group][label] += gap
            tot["ALL"][label] += gap
        prev = t

def show(g, c):
    busy = sum(v for k2, v in c.items() if k2 not in ("idle",))
    if busy == 0: return
    parts = ", ".join(f"{k2} {v/60:.1f} ({100*v/busy:.0f}%)" for k2, v in sorted(c.items(), key=lambda x: -x[1]) if k2 != "idle")
    print(f"{g}: busy {busy/60:.0f} min: {parts}")

for g in sorted(tot):
    show(g, tot[g])
print()
pg = sum(d for d, g, l, c in longest if "pgrep" in c and l.startswith(("test", "build")))
print(f"test/build calls that wait on pgrep: {pg/60:.1f} min")
oth = collections.Counter()
for d, g, l, c in longest:
    if l == "bash:other": oth[c.split()[0] if c.split() else ""] += d
print("bash:other by first word:", [(w, round(v/60,1)) for w, v in oth.most_common(12)])
for d, g, l, c in sorted(longest, reverse=True)[:int(os.environ.get("TOP", "15"))]:
    print(f"{d/60:6.1f} min  {g}  {l}  {c}")
