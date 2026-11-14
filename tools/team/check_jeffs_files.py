#!/usr/bin/env python3
"""check_jeffs_files.py -- place each change under data/jeff/ in a window with its writer.

  python3 -I tools/team/check_jeffs_files.py SINCE [UNTIL]
          [--before LISTING --after LISTING] [--log FILE --label TEXT]
  python3 -I tools/team/check_jeffs_files.py --list LISTING

SINCE and UNTIL are UTC, e.g. 2026-11-13T00:00; UNTIL defaults to now.
The repository is found from this file's path, two folders up, so a
copy in a scratch folder checks that folder's data/jeff/.

WHY. Jeff asked, 2026-11-14, that no specialist's work change a file of
his unnoticed. Libby wrote this check, and the procedure for reading
what it prints is in her graph ("I check that nothing of ours changed a
file of Jeff's ..."). tools/team/pacer.sh runs it after every session it
starts, over that session, and once a day over the day.

It only reads: file times, Jeff's server logs, 'git reflog', 'git
status' and 'git log' (all with --no-optional-locks, so they write no
index), our Claude transcripts and the check's own log. It never reads
the contents of his notes.

Writers it knows:
  his server  a save ("Deleted N file(s), wrote M file(s)" in
              data/jeff/skgconfig.toml.running/start-servers.*.log),
              a server start, or a "stage and commit" request.
  his git     a reflog entry ('pull', or a commit his server made).
  our script  tools/team/specialist.sh writing data/jeff/skgconfig.toml,
              which Jeff allowed on 2026-11-12. It is seen either in a
              transcript or by the line specialist.sh appends to the log.
  our session a Claude tool call that names data/jeff and writes.
Anything else is UNPLACED, and an unplaced change is his.

A deletion leaves no file to time. Given two listings of his files
(--list writes one: path, size and time of every file, gzipped), the
check names each file in the first and not the second as GONE, so a
deletion is placed by its path; without them it can only count.

With --log, it appends to FILE one line of counts headed by TEXT, and
under it every line to read: OURS?, UNPLACED, STAGED, GONE, a deletion
count his server does not explain, and our tool calls that may write.
"""
import argparse, datetime as dt, glob, gzip, json, os, re, subprocess, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
JEFF = ROOT + "/data/jeff"
TRANSCRIPTS = os.path.join(
    os.environ.get("CLAUDE_CONFIG_DIR", os.path.expanduser("~/.claude")),
    "projects", ROOT.replace("/", "-"))
DEFAULT_LOG = os.path.join(
    os.environ.get("PACER_DIR", ROOT + "/data/claude/pacer"), "jeff-check.log")
SKIP_TOP = {".index.tantivy", "skgconfig.toml.running"}
TOL = {"save": (-0.5, 2.0), "start": (-0.5, 15.0), "commit": (-0.5, 6.0)}
LOG_MOST = 300  # lines to read, per window, that the log takes; a pull or a mass deletion can make thousands

def utc(s):
    s = s.strip().replace("Z", "+00:00")
    t = dt.datetime.fromisoformat(s)
    return t if t.tzinfo else t.replace(tzinfo=dt.timezone.utc)

def git(repo, *args):
    return subprocess.run(["git", "--no-optional-locks", "-C", repo, *args],
                          capture_output=True, text=True).stdout

def skgrepos():
    out = []
    for line in open(JEFF + "/skgconfig.toml"):
        m = re.match(r'\s*path\s*=\s*"([^"]+)"', line)
        if m and not m.group(1).startswith(".."):
            out.append(os.path.normpath(os.path.join(JEFF, m.group(1))))
    return out

def jeffs_files():
    """Every file in his skgrepos (outside .git) and in data/jeff/ itself."""
    files = []
    for r in skgrepos():
        for root, dirs, names in os.walk(r):
            dirs[:] = [d for d in dirs if d != ".git"]
            files += [os.path.join(root, n) for n in names]
    for n in os.listdir(JEFF):
        p = os.path.join(JEFF, n)
        if n not in SKIP_TOP and os.path.isfile(p): files.append(p)
    return files

def write_listing(path):
    lines = []
    for p in jeffs_files():
        try: s = os.stat(p)
        except FileNotFoundError: continue  # deleted while listing
        t = dt.datetime.fromtimestamp(s.st_mtime, dt.timezone.utc)
        lines.append(f"{os.path.relpath(p, ROOT)}\t{s.st_size}\t{t:%Y-%m-%dT%H:%M:%SZ}\n")
    with gzip.open(path + ".tmp", "wt") as f:
        f.write(f"# Jeff's files at {dt.datetime.now(dt.timezone.utc):%Y-%m-%dT%H:%M:%SZ}: path, size, time\n")
        f.writelines(sorted(lines))
    os.replace(path + ".tmp", path)

def read_listing(path):
    out = {}
    with gzip.open(path, "rt") as f:
        for line in f:
            if line.startswith("#"): continue
            p, size, t = line.rstrip("\n").split("\t")
            out[p] = (int(size), t)
    return out

# Our sessions: tool calls that name data/jeff and write.
JEFFPATH = re.compile(r"data/jeff|(^|[\s'\"=(])(\.\./)*jeff/")
VERB = re.compile(r"^\s*(sudo\s+)?(mv|cp|rm|touch|tee|truncate|rsync|ln|install|sed\s+-i|perl\s+-p?i)\b"
                  r"|\bgit\b.*\b(add|commit|checkout|restore|reset|stash|clean|rm|mv|pull|merge|rebase|apply)\b"
                  r"|(^|[^0-9&>])>>?\s*\S"
                  r"|open\([^)]*['\"][wa]|write_text|\bspecialist\.sh\s+(born|up|rename)\b")
def writes_jeff(cmd):
    """Simple commands that write and name, or stand in, data/jeff."""
    hits, in_jeff = [], False
    for part in re.split(r"&&|\|\||;|\n|\|", cmd):
        m = re.match(r"\s*cd\s+(\S+)", part)
        if m: in_jeff = bool(re.search(r"data/jeff|^[\'\"]?(\.\./)*jeff(/|$)", m.group(1))); continue
        if re.search(r"specialist\.sh\s+(born|up|rename)\b", part):
            continue  # its writes to his config are placed through 'scripted' below
        if VERB.search(part) and (in_jeff or JEFFPATH.search(part)):
            # a redirect only counts when its target is in data/jeff
            r = re.search(r"(^|[^0-9&>])>>?\s*(\S+)", part)
            if r and not re.search(r"\bgit\b|sed\s+-i|\b(mv|cp|rm|touch|tee)\b", part):
                tgt = r.group(2)
                if not (JEFFPATH.search(tgt) or (in_jeff and not tgt.startswith("/"))): continue
            hits.append(part.strip())
    return hits

def check(since, until, before, after, log_path):
    """Returns (report lines, counts line, lines to read)."""
    inwin = lambda t: since <= t <= until
    out, flagged = [], []

    # 1. Jeff's server: saves, starts, commit requests.
    server = []  # (time, kind, deleted, wrote)
    for f in sorted(glob.glob(JEFF + "/skgconfig.toml.running/start-servers.*.log")):
        for line in open(f, errors="replace"):
            m = re.match(r"(\S+Z)\s+\w+\s+(.*)", line)
            if not m: continue
            t, rest = utc(m.group(1)), m.group(2)
            s = re.search(r"Deleted (\d+) file\(s\), wrote (\d+) file\(s\)", rest)
            if s: server.append((t, "save", int(s.group(1)), int(s.group(2))))
            elif "Received request" in rest and "save buffer" in rest:
                server.append((t, "save", 0, 0))  # a save that writes only marks logs no count
            elif "Listening for Emacs connections" in rest: server.append((t, "start", 0, 0))
            elif "Received request" in rest and "stage and commit" in rest:
                server.append((t, "commit", 0, 0))

    # 2. His git: reflog entries in the window.
    reflog = []  # (time, repo, text)
    for r in skgrepos():
        for line in git(r, "reflog", "--date=iso-strict", "--format=%gd %gs").splitlines():
            m = re.match(r"HEAD@\{([^}]+)\} (.*)", line)
            if m and inwin(utc(m.group(1))): reflog.append((utc(m.group(1)), r, m.group(2)))

    # 3. Our sessions, and specialist.sh as the log records it.
    ours = []  # (time, transcript, tool, text)
    scripted = []  # specialist.sh born, up, rename: may write his config, as he allowed
    for f in glob.glob(TRANSCRIPTS + "/**/*.jsonl", recursive=True):
        for line in open(f, errors="replace"):
            if '"tool_use"' not in line: continue
            try: d = json.loads(line)
            except ValueError: continue
            try: t = utc(d.get("timestamp", ""))
            except ValueError: continue
            if not inwin(t): continue
            content = (d.get("message") or {}).get("content")
            if not isinstance(content, list): continue
            for b in content:
                if b.get("type") != "tool_use": continue
                inp, name = b.get("input", {}), b.get("name")
                if name in ("Edit", "Write", "NotebookEdit"):
                    p = inp.get("file_path", "")
                    if p.startswith(JEFF): ours.append((t, f, name, p))
                elif name == "Bash":
                    c = inp.get("command", "")
                    for h in writes_jeff(c):
                        ours.append((t, f, "Bash", h))
                    for m in re.finditer(r"specialist\.sh\s+(born|up|rename)\b[^;&|\n]*", c):
                        scripted.append((t, f, m.group(0).strip()[:60]))
    if os.path.exists(log_path):
        for line in open(log_path, errors="replace"):
            m = re.match(r"(\S+Z) (specialist\.sh [^:]*): wrote \d+ lines? of data/jeff/", line)
            if m and inwin(utc(m.group(1))): scripted.append((utc(m.group(1)), log_path, m.group(2)))

    def placed(path, t):
        for (e, kind, _, _) in server:
            lo, hi = TOL[kind]
            if lo <= (t - e).total_seconds() <= hi: return f"his server ({kind} at {e:%H:%M:%S})"
        for (e, r, text) in reflog:
            if -0.5 <= (t - e).total_seconds() <= 5 and path.startswith(r): return f"his git ({text})"
        for (e, f, text) in scripted:
            if -0.5 <= (t - e).total_seconds() <= 5 and os.path.basename(path).startswith("skgconfig.toml"):
                return f"ours by his leave ({text} at {e:%F %T})"
        for (e, f, tool, text) in ours:
            if -0.5 <= (t - e).total_seconds() <= 5: return f"OURS? {tool} at {e:%F %T} in {os.path.basename(f)}"
        return None

    # Files under data/jeff written in the window.
    out.append(f"Window {since:%F %T} to {until:%F %T} UTC")
    counts, unplaced, oursish = {}, [], []
    for p in jeffs_files():
        try: t = dt.datetime.fromtimestamp(os.stat(p).st_mtime, dt.timezone.utc)
        except FileNotFoundError: continue
        if not inwin(t): continue
        who = placed(p, t)
        key = "OURS?" if (who or "").startswith("OURS?") else (who or "UNPLACED").split(" (")[0]
        counts[key] = counts.get(key, 0) + 1
        if who is None: unplaced.append((t, p))
        elif who.startswith("OURS") or who.startswith("ours"): oursish.append((t, p, who))
    out.append("\nFiles written in the window, by writer: " + str(counts))
    for t, p, who in sorted(oursish):
        line = f"{t:%F %T}  {os.path.relpath(p, ROOT)}  <- {who}"
        out.append("  " + line)
        if who.startswith("OURS"): flagged.append(line)
    for t, p in sorted(unplaced):
        line = f"UNPLACED {t:%F %T}  {os.path.relpath(p, ROOT)}"
        out.append("  " + line); flagged.append(line)

    # Commits and pulls.
    out.append("\nReflog entries in the window:")
    reflog_unplaced = 0
    for (t, r, text) in sorted(reflog):
        hit = any(k == "commit" and -0.5 <= (t - e).total_seconds() <= 6 for (e, k, _, _) in server)
        who = "his pull" if text.startswith("pull") else ("his server's commit" if hit else "UNPLACED")
        line = f"{t.astimezone(dt.timezone.utc):%F %T}  {os.path.basename(r):20} {text[:40]:40} {who}"
        out.append("  " + line)
        if who == "UNPLACED": reflog_unplaced += 1; flagged.append("reflog " + line)

    # Deletions: by path if there are listings, else only counted.
    out.append("\nDeletions: files gone (uncommitted D, and D in commits in the window)")
    gone = 0
    for r in skgrepos():
        st = git(r, "status", "--porcelain")
        d = sum(1 for l in st.splitlines() if l[:2].strip() == "D")
        staged = [l for l in st.splitlines() if l[0] not in " ?"]
        if staged:  # his server stages and commits in one step, so a staged path is not its
            line = f"{os.path.basename(r):20} STAGED, NOT COMMITTED: {len(staged)}, e.g. {staged[0]}"
            out.append("  " + line); flagged.append(line)
        log = git(r, "log", "--no-merges", f"--since={since.isoformat()}",
                  f"--until={until.isoformat()}", "--name-status", "--format=")
        c = sum(1 for l in log.splitlines() if l.startswith("D\t"))
        gone += d + c
        if d or c: out.append(f"  {os.path.basename(r):20} uncommitted {d}, committed {c}")
    logged = sum(x for (e, k, x, _) in server if k == "save" and inwin(e))
    listed_gone = None
    if before is not None and after is not None:
        listed_gone = sorted(set(before) - set(after))
        for p in listed_gone:
            size, t = before[p]
            line = f"GONE {p}  (was {size} bytes, written {t})"
            out.append("  " + line); flagged.append(line)
        gone_said = f"gone {len(listed_gone)} by the listings, {gone} by git"
        worst = len(listed_gone)
    else:
        gone_said, worst = f"gone {gone} by git", gone
    more = f"MORE GONE THAN HE DELETED: {gone_said}, his server logged {logged} deletions"
    out.append(f"  {gone_said}; his server logged {logged} deletions in the window"
               + ("" if logged >= worst else "  <- MORE GONE THAN HE DELETED: look"))
    if logged < worst: flagged.append(more)

    out.append("\nOur tool calls that name data/jeff and may write (read each):")
    for (t, f, tool, text) in sorted(ours):
        line = (f"{t:%F %T}  {os.path.relpath(f, TRANSCRIPTS)[:60]}  {tool}: "
                f"{text.replace(chr(10), ' ')[:200]}")
        out.append("  " + line); flagged.append("tool call " + line)

    summary = ("written " + (", ".join(f"{k} {v}" for k, v in sorted(counts.items())) or "nothing")
               + f"; reflog {len(reflog)}, unplaced {reflog_unplaced}"
               + f"; {gone_said}, his server deleted {logged}"
               + f"; our tool calls naming data/jeff {len(ours)}")
    return out, summary, flagged

def main():
    a = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    a.add_argument("since", nargs="?")
    a.add_argument("until", nargs="?")
    a.add_argument("--list", metavar="LISTING", help="write a listing of his files, and do nothing else")
    a.add_argument("--before", metavar="LISTING", help="his files as they were at SINCE")
    a.add_argument("--after", metavar="LISTING", help="his files as they were at UNTIL")
    a.add_argument("--log", metavar="FILE", help="append the counts and the lines to read to FILE")
    a.add_argument("--label", default="by hand", help="what the log line says the window was")
    o = a.parse_args()
    if o.list:
        write_listing(o.list); return
    if not o.since: a.error("SINCE is needed, unless --list")
    if bool(o.before) != bool(o.after): a.error("--before and --after go together")
    since = utc(o.since)
    until = utc(o.until) if o.until else dt.datetime.now(dt.timezone.utc)
    before = read_listing(o.before) if o.before else None
    after = read_listing(o.after) if o.after else None
    out, summary, flagged = check(since, until, before, after, o.log or DEFAULT_LOG)
    print("\n".join(out))
    if o.log:
        with open(o.log, "a") as f:
            f.write(f"{dt.datetime.now(dt.timezone.utc):%Y-%m-%dT%H:%M:%SZ} {o.label}, "
                    f"{since:%F %T} to {until:%F %T}: {summary}\n")
            f.writelines(f"  {l}\n" for l in flagged[:LOG_MOST])
            if len(flagged) > LOG_MOST:
                f.write(f"  ... and {len(flagged) - LOG_MOST} more: run the check by hand over"
                        " this window, with the same listings, to see them all\n")

if __name__ == "__main__":
    main()
