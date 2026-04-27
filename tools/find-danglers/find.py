#!/usr/bin/env python3
"""Audit data/public/ for dangling references.

A "dangling reference" here means: a current .skg file mentions an ID
(via 'contains', 'subscribes_to', 'hides_from_its_subscriptions',
'overrides_view_of', or a body/title '[[id:X]]' textlink) that was at
some point checked into git as a 'pid' or 'extra_ids' entry, but is
no longer owned by any current file (neither as 'pid' nor 'extra_ids').

Outputs:
- ~danglers/find.log~  -- timestamped progress log per phase.
- ~danglers/found.org~ -- the report.

The phases are independent and individually timed so the log makes
clear which step is slow if anything ever changes.
"""

import os
import re
import subprocess
import sys
import time
import tempfile
from collections import defaultdict

REPO_DIR = "/home/ubuntu/data/public"
LOG_PATH = "/home/ubuntu/danglers/find.log"
OUT_PATH = "/home/ubuntu/danglers/found.org"

TEXTLINK_RE = re . compile (r'\[\[id:([^\]]+)\]')

OWNERSHIP_FIELDS = ('extra_ids',)
REFERENCE_FIELDS = (
    'contains',
    'subscribes_to',
    'hides_from_its_subscriptions',
    'overrides_view_of',
)
LIST_FIELDS = OWNERSHIP_FIELDS + REFERENCE_FIELDS + ('aliases', 'misc')


# ============================================================
# Logging — every phase emits its own timestamped line so you
# can see exactly where time goes.
# ============================================================

_log_fh = None
_t0 = None

def log_open ():
  global _log_fh, _t0
  _log_fh = open (LOG_PATH, 'w', encoding = 'utf-8', buffering = 1)
  _t0 = time . time ()
  log ("logging started")

def log (msg):
  dt = time . time () - _t0
  line = f"[{dt:7.2f}s] {msg}"
  print (line, file = sys . stderr, flush = True)
  _log_fh . write (line + "\n")


# ============================================================
# .skg parsing — line-based, no YAML library needed.
# ============================================================

def parse_skg (text):
  out = { 'pid'    : None,
          'title'  : '',
          'body'   : '',
          'extra_ids'                    : [],
          'contains'                     : [],
          'subscribes_to'                : [],
          'hides_from_its_subscriptions' : [],
          'overrides_view_of'            : [],
          'aliases'                      : [],
          'misc'                         : [], }
  lines = text . split ('\n')
  i = 0
  n = len (lines)
  while i < n:
    line = lines [i]
    if line . startswith ('pid: '):
      out ['pid'] = line [5:] . strip ()
      i += 1
    elif line . startswith ('title: '):
      raw = line [7:]
      if raw . startswith ("'") and raw . endswith ("'"):
        raw = raw [1:-1] . replace ("''", "'")
      elif raw . startswith ('"') and raw . endswith ('"'):
        raw = raw [1:-1]
      out ['title'] = raw
      i += 1
    elif line . startswith ('body: |'):
      i += 1
      body_lines = []
      while i < n:
        l = lines [i]
        if l . startswith ('  '):
          body_lines . append (l [2:])
          i += 1
        elif l == '':
          body_lines . append ('')
          i += 1
        else:
          break
      out ['body'] = '\n' . join (body_lines)
    else:
      matched = False
      for fld in LIST_FIELDS:
        if line == f"{fld}:":
          i += 1
          items = []
          while i < n and lines [i] . startswith ('- '):
            items . append (lines [i] [2:] . strip ())
            i += 1
          out [fld] = items
          matched = True
          break
      if not matched:
        i += 1
  return out


def textlinked_ids (text):
  return TEXTLINK_RE . findall (text)


# ============================================================
# Phase 1: pids + extra_ids of every CURRENT file.
# ============================================================

def collect_current_owned_ids ():
  log ("phase 1: scan current .skg files")
  files = [f for f in os . listdir (REPO_DIR) if f . endswith ('.skg')]
  files . sort ()
  log (f"phase 1: {len (files)} current files")
  owned = set ()
  pid_to_path = {}
  for idx, fname in enumerate (files):
    if idx and idx % 5000 == 0:
      log (f"phase 1: parsed {idx} / {len (files)}")
    with open (os . path . join (REPO_DIR, fname), 'r', encoding = 'utf-8') as fh:
      parsed = parse_skg (fh . read ())
    if parsed ['pid']:
      owned . add (parsed ['pid'])
      pid_to_path [parsed ['pid']] = fname
    for x in parsed ['extra_ids']:
      owned . add (x)
  log (f"phase 1: done; {len (owned)} currently-owned ids")
  return owned, pid_to_path


# ============================================================
# Phase 2: pids + extra_ids in EVERY historical version.
#
# Earlier attempt deadlocked: I wrote 29k hashes to git's stdin
# in one go, then read stdout, but git's stdout pipe filled up
# before stdin reached EOF, so neither side could make progress.
#
# Fix: stage stdin to a temp file, then point git's stdin at it
# via shell redirection. git's stdin is satisfied immediately;
# we stream stdout incrementally and parse it as it arrives.
# ============================================================

def list_historical_blob_hashes ():
  log ("phase 2a: rev-list all .skg blob hashes")
  proc = subprocess . run (
      ['git', 'rev-list', '--all', '--objects'],
      cwd = REPO_DIR, capture_output = True, text = True, check = True)
  blobs = set ()
  for line in proc . stdout . splitlines ():
    parts = line . split (' ', 1)
    if len (parts) == 2 and parts [1] . endswith ('.skg'):
      blobs . add (parts [0])
  log (f"phase 2a: done; {len (blobs)} unique historical blobs")
  return sorted (blobs)


def stream_cat_file_batch (hashes):
  """Yield (hash, body-text) for each blob, parsing the
  cat-file --batch stream incrementally.

  The cat-file output is binary; each blob is preceded by a
  '<sha> blob <size>\\n' header, then exactly <size> bytes, then
  a single '\\n'. Streaming saves us from buffering 100+ MB of
  blob text in memory all at once.
  """
  with tempfile . NamedTemporaryFile (
      mode = 'w', suffix = '.txt', delete = False,
      encoding = 'utf-8') as inp:
    inp_path = inp . name
    inp . write ('\n' . join (hashes))
    inp . write ('\n')
  try:
    with open (inp_path, 'rb') as inp_bin:
      proc = subprocess . Popen (
          ['git', 'cat-file', '--batch'],
          cwd = REPO_DIR, stdin = inp_bin,
          stdout = subprocess . PIPE,
          bufsize = 0)
      buf = b''
      while True:
        # Read header.
        nl = buf . find (b'\n')
        while nl < 0:
          chunk = proc . stdout . read (65536)
          if not chunk:
            proc . wait ()
            return
          buf += chunk
          nl = buf . find (b'\n')
        header = buf [:nl] . decode ('utf-8', errors = 'replace')
        buf = buf [nl + 1 :]
        parts = header . split (' ')
        if len (parts) < 3 or parts [1] != 'blob':
          continue
        h, size = parts [0], int (parts [2])
        # Read 'size' bytes plus trailing newline.
        need = size + 1
        while len (buf) < need:
          chunk = proc . stdout . read (max (65536, need - len (buf)))
          if not chunk:
            break
          buf += chunk
        body_bytes = buf [:size]
        buf = buf [need:]
        yield h, body_bytes . decode ('utf-8', errors = 'replace')
  finally:
    try: os . unlink (inp_path)
    except OSError: pass


def collect_ever_owned_ids ():
  hashes = list_historical_blob_hashes ()
  owned = set ()
  # pid -> last-seen title across history. Used to give the report
  # human-readable context for IDs that no longer exist on disk.
  last_title : dict = {}
  log ("phase 2b: stream cat-file --batch and parse")
  count = 0
  for h, body in stream_cat_file_batch (hashes):
    parsed = parse_skg (body)
    if parsed ['pid']:
      owned . add (parsed ['pid'])
      if parsed ['title']:
        last_title [parsed ['pid']] = parsed ['title']
    for x in parsed ['extra_ids']:
      owned . add (x)
    count += 1
    if count % 5000 == 0:
      log (f"phase 2b: parsed {count} / {len (hashes)} blobs; {len (owned)} ids so far")
  log (f"phase 2b: done; {count} blobs parsed; {len (owned)} ever-owned ids")
  return owned, last_title


# ============================================================
# Phase 3: scan current files for references to lost IDs.
# ============================================================

def find_dangling_refs (lost_ids):
  log ("phase 3: scan current files for danglers")
  files = sorted (f for f in os . listdir (REPO_DIR) if f . endswith ('.skg'))
  result = defaultdict (list)
  for idx, fname in enumerate (files):
    if idx and idx % 5000 == 0:
      log (f"phase 3: scanned {idx} / {len (files)}")
    with open (os . path . join (REPO_DIR, fname), 'r', encoding = 'utf-8') as fh:
      text = fh . read ()
    parsed = parse_skg (text)
    for fld in REFERENCE_FIELDS:
      for ref in parsed [fld]:
        if ref in lost_ids:
          result [fname] . append ((fld, ref))
    for ref in textlinked_ids (parsed ['title']):
      if ref in lost_ids:
        result [fname] . append (('title-textlink', ref))
    for ref in textlinked_ids (parsed ['body']):
      if ref in lost_ids:
        result [fname] . append (('body-textlink', ref))
  log (f"phase 3: done; {len (result)} files with danglers")
  return result


def title_lookup (paths_to_check):
  out = {}
  for fname in paths_to_check:
    p = os . path . join (REPO_DIR, fname)
    if not os . path . exists (p):
      continue
    with open (p, 'r', encoding = 'utf-8') as fh:
      out [fname] = parse_skg (fh . read ()) ['title']
  return out


# ============================================================
# Phase 4: render report.
# ============================================================

def per_file_sentence (field, lost_phrase):
  """Tells what *this current file* does that points at the lost ID.
  Subject is implicit ("it" / "its X"), so the reader can chain
  multiple of these sentences under one file heading."""
  return {
      'contains'                     : f"Its ~contains~ field still lists {lost_phrase}.",
      'subscribes_to'                : f"Its ~subscribes_to~ field still lists {lost_phrase}.",
      'hides_from_its_subscriptions' : f"Its ~hides_from_its_subscriptions~ field still lists {lost_phrase}.",
      'overrides_view_of'            : f"Its ~overrides_view_of~ field still lists {lost_phrase}.",
      'title-textlink'               : f"Its title embeds a textlink to {lost_phrase}.",
      'body-textlink'                : f"Its body embeds a textlink to {lost_phrase}.",
  } . get (field, f"It references {lost_phrase} via {field}.")


def per_lost_id_sentence (field, fname, ref_title):
  """Tells what one current file does to point at the lost ID.
  Subject is the file (named explicitly) so the sentence stands alone
  under a 'this lost ID' heading."""
  who = f'~{fname}~ (titled "{ref_title}")'
  return {
      'contains'                     : f"{who} lists it in its ~contains~ field.",
      'subscribes_to'                : f"{who} lists it in its ~subscribes_to~ field.",
      'hides_from_its_subscriptions' : f"{who} lists it in its ~hides_from_its_subscriptions~ field.",
      'overrides_view_of'            : f"{who} lists it in its ~overrides_view_of~ field.",
      'title-textlink'               : f"{who} embeds a textlink to it in its title.",
      'body-textlink'                : f"{who} embeds a textlink to it in its body.",
  } . get (field, f"{who} references it via {field}.")


def lost_id_phrase (lost_id, last_title):
  """Render '~ID~ (formerly titled "T")' or just '~ID~' if no title is known."""
  t = last_title . get (lost_id)
  if t:
    return f'~{lost_id}~ (formerly titled "{t}")'
  return f'~{lost_id}~'


def render_report (current_owned, ever_owned, lost, danglers, last_title):
  log ("phase 4: render report")
  total_refs = sum (len (v) for v in danglers . values ())
  by_lost_id = defaultdict (list)
  for fname, refs in danglers . items ():
    for fld, lid in refs:
      by_lost_id [lid] . append ((fname, fld))
  titles = title_lookup (set (danglers . keys ()))
  with open (OUT_PATH, 'w', encoding = 'utf-8') as fh:
    fh . write ("* Dangling references in data/public/\n\n")
    fh . write (f"Generated by ~danglers/find.py~. Repo: ~{REPO_DIR}~.\n\n")
    fh . write ("A /dangling reference/ here is a current ~.skg~ file that still mentions a node ID which was once committed to git as the ~pid~ or an ~extra_ids~ entry of some node, but is no longer owned by any current file. The referent has vanished; the pointer to it has not.\n\n")
    fh . write ("** Summary\n\n")
    n_files = len ([f for f in os . listdir (REPO_DIR) if f . endswith ('.skg')])
    fh . write (f"- Current .skg files:        {n_files}\n")
    fh . write (f"- Currently-owned IDs:       {len (current_owned)}\n")
    fh . write (f"- Ever-owned IDs (history):  {len (ever_owned)}\n")
    fh . write (f"- Lost IDs (history − now):  {len (lost)}\n")
    fh . write (f"- Files with danglers:       {len (danglers)}\n")
    fh . write (f"- Total dangling references: {total_refs}\n")
    fh . write (f"- Distinct lost IDs cited:   {len (by_lost_id)}\n\n")
    if not danglers:
      fh . write ("No dangling references found. The graph's outbound IDs are all live.\n")
      log ("phase 4: done (no danglers)")
      return
    fh . write ("** Dangling references, grouped by current file\n\n")
    fh . write ("Each entry below names a current file and explains exactly how it still points at a vanished node.\n\n")
    for fname in sorted (danglers . keys ()):
      title = titles . get (fname, '') or '(untitled)'
      fh . write (f"*** ~{fname}~\n")
      fh . write (f'This file is currently titled "{title}".\n\n')
      for (fld, lid) in danglers [fname]:
        sentence = per_file_sentence (fld, lost_id_phrase (lid, last_title))
        fh . write (f"- {sentence}\n")
      fh . write ("\n")
    fh . write ("** Lost IDs, ranked by citation count\n\n")
    fh . write ("Each entry below names a vanished node and lists every current file that still cites it. A node with many citations is a more urgent fix: every reader who follows one of those pointers will hit a wall.\n\n")
    ranked = sorted (
        by_lost_id . items (),
        key = lambda kv : (-len (kv [1]), kv [0]))
    for lid, refs in ranked:
      n = len (refs)
      verb = "files reference" if n != 1 else "file references"
      fh . write (f"*** {lost_id_phrase (lid, last_title)}\n")
      fh . write (f"{n} current {verb} this lost ID:\n\n")
      for (fname, fld) in refs [:20]:
        ref_title = titles . get (fname, '') or '(untitled)'
        sentence = per_lost_id_sentence (fld, fname, ref_title)
        fh . write (f"- {sentence}\n")
      if len (refs) > 20:
        fh . write (f"- ...and {len (refs) - 20} more.\n")
      fh . write ("\n")
  log (f"phase 4: done; wrote {OUT_PATH}")


def main ():
  log_open ()
  current_owned, _pid_to_path = collect_current_owned_ids ()
  ever_owned, last_title = collect_ever_owned_ids ()
  lost = ever_owned - current_owned
  log (f"lost ids: {len (lost)}")
  danglers = find_dangling_refs (lost)
  render_report (current_owned, ever_owned, lost, danglers, last_title)
  log ("all phases complete")


if __name__ == '__main__':
  main ()
