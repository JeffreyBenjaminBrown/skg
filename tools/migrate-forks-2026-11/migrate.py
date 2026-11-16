#!/usr/bin/env python3
"""One-time migration (November 2026): the relations 'subscribesTo'
and 'overrides' become one relation, 'forks', and the key
'hidesFromSubs' becomes 'hides'. README.org, beside this file, says
what it does to each file and why.

Usage: migrate.py [--dry-run] [--marks FILE]... DIR...

Reads every *.skg file under each DIR (skipping .git directories),
grouped by pid across all the DIRs, so that every section of a
telescope is seen together: pass all of one owner's skgrepos at once.
Each --marks FILE is a processed marks file, migrated too.
'--dry-run' writes nothing and reports the same counts.
Exits 1 if any file was refused (and left unchanged), 2 on bad usage.
"""

import argparse
import json
import os
import re
import subprocess
import sys
from collections import defaultdict
from dataclasses import dataclass, field

FORKS           = "forks"
SUBSCRIBES_TO   = "subscribesTo"
OVERRIDES       = "overrides"
HIDES_FROM_SUBS = "hidesFromSubs"
HIDES           = "hides"
EXTRA_IDS       = "extra_ids"
PID             = "pid"

# The keys tools/migrate-skg-keys-2026-10/ renamed. A file holding one
# is refused: that tool must run first.
OCTOBER_2026_KEYS = (
  "subscribes_to", "overrides_view_of",
  "hides_from_its_subscriptions", "misc" )

# A file holding one of these needs this tool.
KEYS_TO_MIGRATE = ( SUBSCRIBES_TO, OVERRIDES, HIDES_FROM_SUBS )

# The keys the server's serializer writes before 'forks'
# ('GraphnodeOnDisk', server/types/nodes/fs.rs). A 'forks' list made
# from 'overrides' alone goes after the last of these in the file.
KEYS_THE_SERIALIZER_WRITES_BEFORE_FORKS = (
  "title", "aliases", PID, EXTRA_IDS, "body", "contains" )

# In a processed marks file, a mark of a relationship of one of these
# relations is dropped; one of a renamed relation is renamed.
RETIRED_MARK_RELATIONS = ( "SubscribesTo", "Overrides" )
RENAMED_MARK_RELATIONS = { "HidesFromSubs" : "Hides" }

COMMIT_MESSAGE = "Migrate to forks"

TOP_LEVEL_KEY_LINE = re.compile (
  r"^([A-Za-z_][A-Za-z0-9_]*):(?=[ \t]|\r?\n|$)" )
LIST_ITEM_LINE = re.compile (
  r"^( *)- (.*?)[ \t]*(\r?\n)?$" )
NOTHING_BUT_A_COMMENT = re.compile (
  r"^[ \t]*(#.*)?(\r?\n)?$" )

class RefusedFile (Exception):
  def __init__ (self, path, reason):
    super () . __init__ (reason)
    self . path   = path
    self . reason = reason

@dataclass
class ListEntry:
  text      : str  # what follows "- " on its line, as written
  gnid      : str  # the ID it names
  is_anchor : bool

@dataclass
class Segment:
  key   : object  # str, or None for the lines before the first key
  lines : list

@dataclass
class Section:
  root     : str
  path     : str
  segments : list

  def keys (self):
    return [ s . key for s in self . segments if s . key is not None ]

  def segment (self, key):
    return next ( ( s for s in self . segments if s . key == key ),
                  None )

@dataclass
class ListOfSection:
  entries : list
  indent  : str  # the indentation of its item lines

@dataclass
class Counts:
  files_changed                       : int  = 0
  paired                              : int  = 0
  subscribe_only                      : int  = 0
  override_only                       : int  = 0
  overrides_recorded_in_other_section : int  = 0
  duplicates                          : int  = 0
  hides_keys_renamed                  : int  = 0
  self_forks                          : list = field (default_factory = list)
  changed_roots                       : set  = field (default_factory = set)

  def add (self, other):
    for name in ( "files_changed", "paired", "subscribe_only",
                  "override_only", "overrides_recorded_in_other_section",
                  "duplicates", "hides_keys_renamed" ):
      setattr ( self, name,
                getattr (self, name) + getattr (other, name) )
    self . self_forks    += other . self_forks
    self . changed_roots |= other . changed_roots

@dataclass
class Report:
  counts                    : Counts = field (default_factory = Counts)
  refused_files             : list   = field (default_factory = list)
  unchanged_beside_refusals : list   = field (default_factory = list)
  marks_dropped             : int    = 0
  marks_renamed             : int    = 0

def main (argv):
  arguments = parse_arguments (argv)
  report = Report ()
  located_paths_by_pid, pids_to_migrate, pids_refused = \
    survey_skg_files (arguments . dirs, report)
  for pid in sorted (pids_to_migrate):
    if pid in pids_refused:
      report . unchanged_beside_refusals += [
        path for (_, path) in located_paths_by_pid [pid]
        if path not in pids_refused [pid] ]
      continue
    migrate_telescope ( pid, located_paths_by_pid [pid],
                        arguments . dry_run, report )
  for marks_file in arguments . marks:
    migrate_marks_file (marks_file, arguments . dry_run, report)
  print_report (report, arguments)
  return 1 if report . refused_files else 0

def parse_arguments (argv):
  parser = argparse.ArgumentParser (
    description = "Migrate .skg files to the relation 'forks'." )
  parser . add_argument ( "--dry-run", action = "store_true",
                          help = "write nothing; report the same counts" )
  parser . add_argument ( "--marks", action = "append", default = [],
                          metavar = "FILE",
                          help = "a processed marks file to migrate" )
  parser . add_argument ( "dirs", nargs = "+", metavar = "DIR" )
  return parser . parse_args (argv)

def survey_skg_files (roots, report):
  """Read every file once, for its pid and its top-level keys.
Returns the (root, path) pairs of each pid, the pids some section of
which needs migrating, and, for each pid with a refused section, the
paths refused."""
  located_paths_by_pid = defaultdict (list)
  pids_to_migrate      = set ()
  pids_refused         = defaultdict (set)
  real_paths_seen      = set () # a DIR named twice, or inside another
  for root in roots:
    for path in skg_files_under (root):
      if os . path . realpath (path) in real_paths_seen:
        continue
      real_paths_seen . add ( os . path . realpath (path) )
      text = read_text (path)
      keys = top_level_keys (text)
      pid  = pid_of_text (text)
      october_keys = [ k for k in keys if k in OCTOBER_2026_KEYS ]
      needs_migrating = any ( k in KEYS_TO_MIGRATE for k in keys )
      if october_keys:
        report . refused_files . append ( (
          path,
          "holds the October 2026 key(s) {}; run "
          "tools/migrate-skg-keys-2026-10/migrate.py first" . format (
            ", " . join (october_keys) ) ) )
        if pid is not None:
          pids_refused [pid] . add (path)
      if pid is None:
        if needs_migrating and not october_keys:
          report . refused_files . append (
            (path, "has no readable top-level 'pid'") )
        continue
      located_paths_by_pid [pid] . append ( (root, path) )
      if needs_migrating:
        pids_to_migrate . add (pid)
  return located_paths_by_pid, pids_to_migrate, pids_refused

def migrate_telescope (pid, located_paths, dry_run, report):
  """Migrate every section of one pid, or, if any is refused, none."""
  sections = [ read_section (root, path)
               for (root, path) in located_paths ]
  try:
    new_texts, counts = migration_of_telescope (pid, sections)
  except RefusedFile as refusal:
    report . refused_files . append ( (refusal . path, refusal . reason) )
    report . unchanged_beside_refusals += [
      s . path for s in sections if s . path != refusal . path ]
    return
  report . counts . add (counts)
  if not dry_run:
    for path, text in new_texts . items ():
      write_text (path, text)

def migration_of_telescope (pid, sections):
  """The new text of each section of 'pid' that changes, and the
counts. Raises RefusedFile."""
  counts = Counts ()
  for section in sections:
    refuse_conflicting_keys (section)
  own_gnids = { pid } | {
    entry . gnid
    for section in sections
    for entry in members_of ( list_of_section (section, EXTRA_IDS) ) }
  forks_lists_before = {
    # A section already migrated (a rerun after a refusal) has a
    # 'forks' list; it pairs with an 'overrides' list elsewhere as a
    # 'subscribesTo' list does.
    section . path : (
      list_of_section (section, SUBSCRIBES_TO)
      or list_of_section (section, FORKS) )
    for section in sections }
  overrides_lists = {
    section . path : refuse_anchors (
      section, list_of_section (section, OVERRIDES) )
    for section in sections }
  subscribed = { entry . gnid
                 for section in sections
                 for entry in members_of (
                     list_of_section (section, SUBSCRIBES_TO) ) }
  forked_before = { entry . gnid
                    for lst in forks_lists_before . values ()
                    for entry in members_of (lst) }
  overridden = { entry . gnid
                 for lst in overrides_lists . values ()
                 for entry in members_of (lst) }
  counts . paired         = len ( (overridden & forked_before) - own_gnids )
  counts . subscribe_only = len ( (subscribed - overridden) - own_gnids )
  counts . override_only  = len ( (overridden - forked_before) - own_gnids )
  counts . self_forks     = [
    (pid, gnid)
    for gnid in sorted ( (forked_before | overridden) & own_gnids ) ]
  new_texts = {}
  for section in sections:
    new_text = migrated_section_text (
      section,
      forks_lists_before [section . path],
      overrides_lists [section . path],
      own_gnids, forked_before, counts )
    if new_text is not None:
      new_texts [section . path] = new_text
      counts . files_changed += 1
      counts . changed_roots . add (section . root)
  return new_texts, counts

def migrated_section_text (
    section, forks_list_before, overrides_list,
    own_gnids, forked_anywhere, counts ):
  """The section's new text, or None if it does not change."""
  has_hides_key = section . segment (HIDES_FROM_SUBS) is not None
  if ( section . segment (SUBSCRIBES_TO) is None
       and overrides_list is None
       and not has_hides_key ):
    return None
  forks_entries = forks_list_entries (
    forks_list_before, overrides_list,
    own_gnids, forked_anywhere, counts )
  forks_indent = ( forks_list_before or overrides_list
                   or ListOfSection ([], "") ) . indent
  forks_segment = Segment (
    FORKS,
    forks_segment_lines ( forks_entries, forks_indent,
                          newline_of (section) ) ) \
    if forks_entries else None
  if has_hides_key:
    counts . hides_keys_renamed += 1
  return "" . join (
    line
    for segment in segments_after_migration (section, forks_segment)
    for line in segment . lines )

def forks_list_entries (
    forks_list_before, overrides_list,
    own_gnids, forked_anywhere, counts ):
  """The section's subscriptions (or existing forks), in order, less
self-references and repeats, then each target of its overrides that
no section of the telescope subscribes to."""
  entries = []
  members_seen = set ()
  for entry in ( forks_list_before . entries
                 if forks_list_before else [] ):
    if not entry . is_anchor:
      if entry . gnid in own_gnids:
        continue
      if entry . gnid in members_seen:
        counts . duplicates += 1
        continue
      members_seen . add (entry . gnid)
    entries . append (entry)
  members_before = set (members_seen)
  for entry in ( overrides_list . entries
                 if overrides_list else [] ):
    if entry . gnid in own_gnids:
      continue
    if entry . gnid in forked_anywhere:
      if entry . gnid not in members_before:
        counts . overrides_recorded_in_other_section += 1
      continue
    if entry . gnid in members_seen:
      counts . duplicates += 1
      continue
    members_seen . add (entry . gnid)
    entries . append (entry)
  return entries

def segments_after_migration (section, forks_segment):
  """The section's segments with 'subscribesTo' (or 'forks') replaced
by 'forks_segment', 'overrides' gone, and 'hidesFromSubs' renamed.
A 'forks' segment with nowhere to stand goes after the last key the
serializer writes before it."""
  segments = []
  forks_placed = False
  for segment in section . segments:
    if segment . key in (SUBSCRIBES_TO, FORKS):
      if forks_segment is not None:
        segments . append ( Segment (
          FORKS,
          forks_segment . lines
          + trailing_blank_lines_of (segment . lines [1:]) ) )
      forks_placed = True
    elif segment . key == OVERRIDES:
      continue
    elif segment . key == HIDES_FROM_SUBS:
      segments . append ( Segment (
        HIDES,
        [ HIDES + segment . lines [0] [ len (HIDES_FROM_SUBS) : ] ]
        + segment . lines [1:] ) )
    else:
      segments . append (segment)
  if forks_segment is not None and not forks_placed:
    index_after_preceding_keys = 1 + max (
      ( i for (i, s) in enumerate (segments)
        if s . key in KEYS_THE_SERIALIZER_WRITES_BEFORE_FORKS
        or s . key is None ),
      default = -1 )
    ensure_last_line_ends ( segments, index_after_preceding_keys,
                            newline_of (section) )
    segments . insert (index_after_preceding_keys, forks_segment)
  return segments

def ensure_last_line_ends (segments, index, newline):
  """If the segment before 'index' ends the file without a newline,
give it one, since something now follows it."""
  if index > 0 and segments [index - 1] . lines:
    last = segments [index - 1] . lines [-1]
    if not last . endswith ("\n"):
      segments [index - 1] . lines [-1] = last + newline

def forks_segment_lines (entries, indent, newline):
  return ( [ FORKS + ":" + newline ]
           + [ indent + "- " + entry . text + newline
               for entry in entries ] )

def refuse_conflicting_keys (section):
  keys = section . keys ()
  for key in set (keys):
    if keys . count (key) > 1:
      raise RefusedFile (
        section . path, "holds the key '{}' twice" . format (key) )
  for (old, new) in ( (SUBSCRIBES_TO, FORKS), (HIDES_FROM_SUBS, HIDES) ):
    if old in keys and new in keys:
      raise RefusedFile (
        section . path,
        "holds both '{}' and '{}'" . format (old, new) )

def refuse_anchors (section, lst):
  if lst is not None and any ( e . is_anchor for e in lst . entries ):
    raise RefusedFile (
      section . path, "holds an anchor in 'overrides', an unordered list" )
  return lst

def members_of (lst):
  return [] if lst is None else [
    entry for entry in lst . entries if not entry . is_anchor ]

def list_of_section (section, key):
  """The block-style list under 'key', or None if the key is absent.
Raises RefusedFile if it is not a block-style list of IDs."""
  segment = section . segment (key)
  if segment is None:
    return None
  key_line_rest = segment . lines [0] [ len (key) + 1 : ]
  if not NOTHING_BUT_A_COMMENT . match (key_line_rest):
    raise RefusedFile (
      section . path,
      "its '{}' list is not in block style" . format (key) )
  entries = []
  indent = None
  item_lines = trailing_blank_lines_removed ( segment . lines [1:] )
  for line in item_lines:
    match = LIST_ITEM_LINE . match (line)
    if match is None or ( indent is not None
                          and match . group (1) != indent ):
      raise RefusedFile (
        section . path,
        "unexpected line under '{}': {!r}" . format (key, line) )
    indent = match . group (1)
    entries . append (
      list_entry_of_item_text ( section . path, key, match . group (2) ) )
  return ListOfSection (entries, indent or "")

def trailing_blank_lines_removed (lines):
  return lines [ : len (lines) - len ( trailing_blank_lines_of (lines) ) ]

def trailing_blank_lines_of (lines):
  end = len (lines)
  while end > 0 and lines [end - 1] . strip () == "":
    end -= 1
  return lines [end:]

def list_entry_of_item_text (path, key, text):
  if text . startswith ("anchor:"):
    return ListEntry (
      text, gnid_of_scalar ( path, key, text [ len ("anchor:") : ] . strip () ),
      is_anchor = True )
  return ListEntry ( text, gnid_of_scalar (path, key, text),
                     is_anchor = False )

def gnid_of_scalar (path, key, text):
  """The string a one-line YAML scalar holds. Raises RefusedFile for
anything else (a flow collection, a mapping, an alias)."""
  def refuse ():
    raise RefusedFile (
      path,
      "a member of '{}' is not a one-line scalar: {!r}" . format (key, text) )
  if text . startswith ("'"):
    if len (text) < 2 or not text . endswith ("'"):
      refuse ()
    return text [1:-1] . replace ("''", "'")
  if text . startswith ('"'):
    try:
      return json . loads (text)
    except ValueError:
      refuse ()
  plain = re . sub (r"[ \t]+#.*$", "", text)
  if ( plain == "" or plain [0] in "[]{}&*!|>%@`#,"
       or plain . startswith ("? ")
       or ": " in plain or plain . endswith (":") ):
    refuse ()
  return plain

def read_section (root, path):
  return Section ( root, path,
                   segments_of_text ( read_text (path) ) )

def segments_of_text (text):
  """Split a file's lines at each top-level key line. Only a key at
the start of a line is top-level, so titles and bodies (whose lines
the YAML indents) never start a segment."""
  segments = [ Segment (None, []) ]
  for line in text . splitlines (keepends = True):
    match = TOP_LEVEL_KEY_LINE . match (line)
    if match:
      segments . append ( Segment (match . group (1), [line]) )
    else:
      segments [-1] . lines . append (line)
  return segments

def top_level_keys (text):
  return [ m . group (1)
           for m in re . finditer ( TOP_LEVEL_KEY_LINE . pattern,
                                    text, re . MULTILINE ) ]

def pid_of_text (text):
  match = re . search ( r"^pid:[ \t]*(.*?)[ \t]*\r?$", text, re . MULTILINE )
  if match is None:
    return None
  try:
    return gnid_of_scalar (None, PID, match . group (1))
  except RefusedFile:
    return None

def newline_of (section):
  for segment in section . segments:
    for line in segment . lines:
      if line . endswith ("\r\n"):
        return "\r\n"
      if line . endswith ("\n"):
        return "\n"
  return "\n"

def migrate_marks_file (marks_file, dry_run, report):
  """Drop each mark of a relationship of a retired relation, and
rename each of a renamed relation. A file that is not a marks file is
refused and left unchanged."""
  try:
    store = json . loads ( read_text (marks_file) )
    marks = store ["marks"]
  except (OSError, ValueError, KeyError, TypeError) as error:
    report . refused_files . append (
      (marks_file, "not a readable processed marks file: {}" . format (error)) )
    return
  kept = []
  marks_changed = 0
  for mark in marks:
    relationship = relationship_of_mark (mark)
    relation = relationship . get ("relation") if relationship else None
    if relation in RETIRED_MARK_RELATIONS:
      report . marks_dropped += 1
      marks_changed += 1
      continue
    if relation in RENAMED_MARK_RELATIONS:
      relationship ["relation"] = RENAMED_MARK_RELATIONS [relation]
      report . marks_renamed += 1
      marks_changed += 1
    kept . append (mark)
  if marks_changed and not dry_run:
    store ["marks"] = kept
    write_text ( marks_file,
                 json . dumps ( # as serde_json's pretty printer writes it
                   store, indent = 2, ensure_ascii = False ) )

def relationship_of_mark (mark):
  leaf = mark . get ("leaf") if isinstance (mark, dict) else None
  if isinstance (leaf, dict) and isinstance (leaf . get ("Relationship"), dict):
    return leaf ["Relationship"]
  return None

def print_report (report, arguments):
  counts = report . counts
  print ( "{}{} files changed" . format (
    "(dry run: nothing written) " if arguments . dry_run else "",
    counts . files_changed ) )
  print ( "relationships: {} paired, {} subscribe-only, {} override-only"
          . format ( counts . paired, counts . subscribe_only,
                     counts . override_only ) )
  print ( "overrides dropped as recorded in another section: {}"
          . format (counts . overrides_recorded_in_other_section) )
  print ( "duplicates dropped: {}" . format (counts . duplicates) )
  print ( "self-forks dropped: {}" . format ( len (counts . self_forks) ) )
  for (pid, gnid) in counts . self_forks:
    print ( "  {} named its own ID {}" . format (pid, gnid) )
  print ( "'hidesFromSubs' keys renamed 'hides': {}"
          . format (counts . hides_keys_renamed) )
  print ( "marks dropped: {}" . format (report . marks_dropped) )
  print ( "marks renamed HidesFromSubs to Hides: {}"
          . format (report . marks_renamed) )
  if report . refused_files:
    print ( "refused, left unchanged: {}" . format (
      len (report . refused_files) ) )
    for (path, reason) in report . refused_files:
      print ( "  {}: {}" . format (path, reason) )
  if report . unchanged_beside_refusals:
    print ( "left unchanged because another section of their pid was refused: {}"
            . format ( len (report . unchanged_beside_refusals) ) )
    for path in report . unchanged_beside_refusals:
      print ( "  " + path )
  commit_lines = [
    "git -C {} commit -am '{}'" . format (root, COMMIT_MESSAGE)
    for root in arguments . dirs
    if root in counts . changed_roots and is_in_a_gitrepo (root) ]
  if commit_lines:
    print ( "{}commit each changed gitrepo at once (the diff view refuses "
            "a HEAD in the old format):" . format (
              "after a run without --dry-run, " if arguments . dry_run
              else "" ) )
    for line in commit_lines:
      print ( "  " + line )

def is_in_a_gitrepo (root):
  result = subprocess . run (
    [ "git", "-C", root, "rev-parse", "--is-inside-work-tree" ],
    capture_output = True, text = True )
  return result . returncode == 0 and result . stdout . strip () == "true"

def skg_files_under (root):
  for dirpath, dirnames, filenames in os . walk (root):
    dirnames [:] = sorted ( d for d in dirnames if d != ".git" )
    for name in sorted (filenames):
      if name . endswith (".skg"):
        yield os . path . join (dirpath, name)

def read_text (path):
  with open (path, encoding = "utf-8", newline = "") as f:
    return f . read ()

def write_text (path, text):
  with open (path, "w", encoding = "utf-8", newline = "") as f:
    f . write (text)

if __name__ == "__main__":
  sys . exit ( main ( sys . argv [1:] ) )
