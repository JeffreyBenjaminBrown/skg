#!/usr/bin/env python3
"""One-time migration (November 2026): a fork's home section now
records an 'attribution' saying which note it was forked from, and
where that note lives. The server writes it at fork time; this tool
backfills it for forks made before. README.org, beside this file,
says what it writes and why.

Usage: migrate.py [--dry-run] CONFIG

CONFIG is a skgconfig.toml. Every skgrepo it lists is read, owned or
foreign, to find each original's home; only owned skgrepos are
written. '--dry-run' writes nothing and reports the same counts.
Exits 1 if any file was refused (and left unchanged), 2 on bad usage.
"""

import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
import tomllib
from collections import defaultdict
from dataclasses import dataclass, field

TITLE       = "title"
PID         = "pid"
EXTRA_IDS   = "extra_ids"
FORKS       = "forks"
ATTRIBUTION = "attribution"

COMMIT_MESSAGE = "Record fork attributions"

TOP_LEVEL_KEY_LINE = re . compile (
  r"^([A-Za-z_][A-Za-z0-9_]*):(?=[ \t]|\r?\n|$)" )
LIST_ITEM_LINE = re . compile (
  r"^( *)- (.*?)[ \t]*(\r?\n)?$" )
NOTHING_BUT_A_COMMENT = re . compile (
  r"^[ \t]*(#.*)?(\r?\n)?$" )

# A value written bare must be one of these, and not a YAML special,
# an int or a float. Anything else would need quoting, which the tool
# does not guess at: it refuses the file.
SAFE_PLAIN_SCALAR = re . compile ( r"^[A-Za-z0-9_./][A-Za-z0-9_./-]*$" )
YAML_SPECIALS = {
  "true", "false", "yes", "no", "on", "off", "null", "~", "y", "n",
  # YAML's own spellings of infinity and not-a-number.
  ".inf", ".nan" }

SKIP_NOT_LOADED  = "original not in any configured skgrepo"
SKIP_SAME_HOME   = "original's home is the fork's own home skgrepo"
SKIP_NO_HOME     = "original has no single home section"
SKIP_REASONS = ( SKIP_NOT_LOADED, SKIP_SAME_HOME, SKIP_NO_HOME )

class RefusedFile (Exception):
  def __init__ (self, path, reason):
    super () . __init__ (reason)
    self . path   = path
    self . reason = reason

@dataclass
class Skgrepo:
  name    : str
  path    : str   # as resolved against the config's folder
  foreign : bool

@dataclass
class SkgFile:
  skgrepo   : Skgrepo
  path      : str
  text      : str
  pid       : str
  is_home   : bool  # it carries 'title'
  keys      : list

@dataclass
class Report:
  attributed_by_skgrepo : dict = field (default_factory = dict)
  already_attributed    : int  = 0
  skipped               : dict = field (
    default_factory = lambda : { reason : 0 for reason in SKIP_REASONS } )
  refused_files         : list = field (default_factory = list)
  missing_skgrepos      : list = field (default_factory = list)
  changed_skgrepos      : list = field (default_factory = list)

def main (argv):
  arguments = parse_arguments (argv)
  try:
    skgrepos = skgrepos_of_config (arguments . config)
  except (OSError, ValueError, TypeError, KeyError) as error:
    print ( "migrate.py: cannot read {}: {}" . format (
      arguments . config, error ), file = sys . stderr )
    return 2
  report = Report ()
  files = read_skg_files (skgrepos, report)
  pid_of_id, homes_of_pid = index_of_files (files)
  skgrepo_by_name = {}
  for skgrepo in skgrepos:
    skgrepo_by_name . setdefault (skgrepo . name, skgrepo)
  remote_hashes = {}
  for skgrepo in skgrepos:
    if not skgrepo . foreign:
      report . attributed_by_skgrepo . setdefault (skgrepo . name, 0)
  for f in files:
    if f . skgrepo . foreign or not f . is_home:
      continue
    try:
      new_text = attributed_text (
        f, pid_of_id, homes_of_pid, skgrepo_by_name, remote_hashes, report )
    except RefusedFile as refusal:
      report . refused_files . append ( (refusal . path, refusal . reason) )
      continue
    if new_text is None:
      continue
    report . attributed_by_skgrepo [f . skgrepo . name] += 1
    if f . skgrepo not in report . changed_skgrepos:
      report . changed_skgrepos . append (f . skgrepo)
    if not arguments . dry_run:
      write_text (f . path, new_text)
  print_report (report, arguments)
  return 1 if report . refused_files else 0

def parse_arguments (argv):
  parser = argparse.ArgumentParser (
    description = "Record an 'attribution' in each fork made before "
                  "the server recorded one." )
  parser . add_argument ( "--dry-run", action = "store_true",
                          help = "write nothing; report the same counts" )
  parser . add_argument ( "config", metavar = "CONFIG",
                          help = "a skgconfig.toml" )
  return parser . parse_args (argv)

def skgrepos_of_config (config_path):
  """The skgrepos the config lists, in its order. A relative path
resolves against the config's folder; a name defaults to the path as
written; 'foreign' defaults to false."""
  with open (config_path, "rb") as f:
    config = tomllib . load (f)
  entries = config . get ("repos")
  if not isinstance (entries, list) or not entries:
    raise ValueError ("it lists no [[repos]]")
  config_folder = os . path . dirname (config_path)
  skgrepos = []
  for entry in entries:
    path = entry ["path"]
    if not isinstance (path, str):
      raise ValueError ( "a 'path' is not a string: {!r}" . format (path) )
    skgrepos . append ( Skgrepo (
      name    = entry . get ("name", path),
      path    = os . path . normpath (
                  os . path . join (config_folder, path) ),
      foreign = bool ( entry . get ("foreign", False) ) ) )
  return skgrepos

def read_skg_files (skgrepos, report):
  """Every '*.skg' file directly in each skgrepo (not in its
subfolders). A folder listed twice is read once."""
  files = []
  real_paths_seen = set ()
  for skgrepo in skgrepos:
    if not os . path . isdir (skgrepo . path):
      report . missing_skgrepos . append (skgrepo)
      continue
    real = os . path . realpath (skgrepo . path)
    if real in real_paths_seen:
      continue
    real_paths_seen . add (real)
    for name in sorted ( os . listdir (skgrepo . path) ):
      path = os . path . join (skgrepo . path, name)
      if not ( name . endswith (".skg") and os . path . isfile (path) ):
        continue
      text = read_text (path)
      pid  = pid_of_text (text)
      if pid is None:
        continue
      keys = top_level_keys (text)
      files . append ( SkgFile ( skgrepo, path, text, pid,
                                 TITLE in keys, keys ) )
  return files

def index_of_files (files):
  """Each ID (a pid, or an extra ID in any section) to its pid, and
each pid to the names of the skgrepos whose section of it carries
'title'. A pid is taken over an extra ID that says otherwise."""
  pid_of_id    = {}
  homes_of_pid = defaultdict (list)
  for f in files:
    pid_of_id [f . pid] = f . pid
    if f . is_home and f . skgrepo . name not in homes_of_pid [f . pid]:
      homes_of_pid [f . pid] . append (f . skgrepo . name)
  for f in files:
    try:
      extra_ids = list_of_text (f . path, f . text, EXTRA_IDS)
    except RefusedFile:
      continue # only a fork's own list matters enough to refuse
    for entry in extra_ids or []:
      if not entry [1]:
        pid_of_id . setdefault (entry [0], f . pid)
  return pid_of_id, homes_of_pid

def attributed_text ( f, pid_of_id, homes_of_pid,
                      skgrepo_by_name, remote_hashes, report ):
  """The home section's text with an attribution appended, or None if
it gets none. Raises RefusedFile."""
  if ATTRIBUTION in f . keys:
    report . already_attributed += 1
    return None
  forks = list_of_text (f . path, f . text, FORKS)
  members = [ gnid for (gnid, is_anchor) in forks or []
              if not is_anchor ]
  if not members:
    return None
  if f . keys . count (TITLE) > 1 or f . keys . count (FORKS) > 1:
    raise RefusedFile ( f . path, "holds a key twice" )
  original = pid_of_id . get (members [0])
  if original is None:
    report . skipped [SKIP_NOT_LOADED] += 1
    return None
  homes = homes_of_pid . get (original, [])
  if len (homes) != 1:
    report . skipped [SKIP_NO_HOME] += 1
    return None
  original_home = homes [0]
  if original_home == f . skgrepo . name:
    report . skipped [SKIP_SAME_HOME] += 1
    return None
  remote_hash = remote_hash_of_skgrepo_named (
    original_home, skgrepo_by_name, remote_hashes )
  values = [ ("original", original), ("skgrepo", original_home) ]
  if remote_hash is not None:
    values . append ( ("remote_hash", remote_hash) )
  for (key, value) in values:
    if not is_safe_plain_scalar (value):
      raise RefusedFile (
        f . path,
        "its attribution's {} {!r} would need YAML quoting" . format (
          key, value ) )
  newline = newline_of (f . text)
  text = f . text
  if text and not text . endswith ("\n"):
    text += newline
  return text + "" . join (
    [ ATTRIBUTION + ":" + newline ]
    + [ "  {}: {}{}" . format (key, value, newline)
        for (key, value) in values ] )

def remote_hash_of_skgrepo_named (name, skgrepo_by_name, remote_hashes):
  """The remote hash of the configured skgrepo 'name', looked up once."""
  if name not in remote_hashes:
    remote_hashes [name] = remote_hash_of_skgrepo_path (
      skgrepo_by_name [name] . path )
  return remote_hashes [name]

def is_safe_plain_scalar (value):
  if not SAFE_PLAIN_SCALAR . match (value):
    return False
  if value . lower () in YAML_SPECIALS:
    return False
  for parse in ( int, lambda v : int (v, 0), float ):
    try:
      parse (value)
      return False
    except ValueError:
      pass
  return True

# The remote

def remote_hash_of_skgrepo_path (path):
  """The remote hash of the skgrepo at 'path', or None when it has no
remote of its own: when it is not itself a gitrepo's top level (a
folder inside an enclosing gitrepo has none), or has no remote. The
remote named 'origin', else the alphabetically first."""
  if not os . path . exists ( os . path . join (path, ".git") ):
    return None
  names = git_output ( path, "remote" )
  if names is None:
    return None
  names = sorted ( n for n in names . split () if n )
  if not names:
    return None
  name = "origin" if "origin" in names else names [0]
  url = git_output ( path, "config", "--get",
                     "remote.{}.url" . format (name) )
  if url is None or not url . strip ():
    return None
  return remote_hash_of_url (url)

def git_output (path, *arguments):
  try:
    result = subprocess . run (
      [ "git", "-C", path ] + list (arguments),
      capture_output = True, text = True )
  except OSError:
    return None
  return result . stdout if result . returncode == 0 else None

SCHEME_CHARS = re . compile ( r"^[A-Za-z0-9+.-]+$" )

def normalised_remote_url (url):
  """A remote URL as the repository it names, whatever the transport,
as 'normalised_remote_url' in server/attribution.rs computes it. The
cases in tests/shared/remote-hash-cases.txt hold for both."""
  url = url . strip ()
  def host_and_path (host, path):
    return host . lower () + "/" + path . lstrip ("/")
  scheme, separator, rest = url . partition ("://")
  if separator and SCHEME_CHARS . match (scheme):
    if scheme . lower () == "file":
      joined = rest
    else:
      slash = rest . find ("/")
      authority, path = ( (rest, "") if slash < 0
                          else (rest [:slash], rest [slash:]) )
      host_and_port = authority . rpartition ("@") [2]
      host, colon, port = host_and_port . rpartition (":")
      if not ( colon and all ( c in "0123456789" for c in port ) ):
        host = host_and_port
      joined = host_and_path (host, path)
  elif ":" in url and url . split (":", 1) [0] \
       and "/" not in url . split (":", 1) [0]:
    before, path = url . split (":", 1)
    joined = host_and_path ( before . rpartition ("@") [2], path )
  else:
    joined = url
  trimmed = joined . rstrip ("/")
  if trimmed . endswith (".git"):
    trimmed = trimmed [ : -len (".git") ]
  return trimmed . rstrip ("/")

def remote_hash_of_url (url):
  """The git blob hash (SHA-1) of the normalised URL: what
'printf %s NORMALISED | git hash-object --stdin' prints."""
  data = normalised_remote_url (url) . encode ("utf-8")
  return hashlib . sha1 ( b"blob %d\0" % len (data) + data ) . hexdigest ()

# Reading .skg files, as tools/migrate-forks-2026-11/ does: only
# top-level keys at the start of a line, so titles and bodies (whose
# lines YAML indents) are never read as keys.

def top_level_keys (text):
  return [ m . group (1)
           for m in re . finditer ( TOP_LEVEL_KEY_LINE . pattern,
                                    text, re . MULTILINE ) ]

def lines_under_key (text, key):
  """The key's line and the lines after it, up to the next top-level
key; None if the key is absent."""
  lines = None
  for line in text . splitlines (keepends = True):
    match = TOP_LEVEL_KEY_LINE . match (line)
    if match:
      if lines is not None:
        break
      if match . group (1) == key:
        lines = [line]
    elif lines is not None:
      lines . append (line)
  return lines

def list_of_text (path, text, key):
  """The block-style list under 'key', as (ID, is_anchor) pairs, or
None if the key is absent. Raises RefusedFile if it is not a
block-style list of IDs."""
  lines = lines_under_key (text, key)
  if lines is None:
    return None
  if not NOTHING_BUT_A_COMMENT . match ( lines [0] [ len (key) + 1 : ] ):
    raise RefusedFile (
      path, "its '{}' list is not in block style" . format (key) )
  entries = []
  indent = None
  for line in lines [1:]:
    if line . strip () == "":
      continue
    match = LIST_ITEM_LINE . match (line)
    if match is None or ( indent is not None
                          and match . group (1) != indent ):
      raise RefusedFile (
        path, "unexpected line under '{}': {!r}" . format (key, line) )
    indent = match . group (1)
    item = match . group (2)
    if item . startswith ("anchor:"):
      entries . append ( ( gnid_of_scalar (
        path, key, item [ len ("anchor:") : ] . strip () ), True ) )
    else:
      entries . append ( ( gnid_of_scalar (path, key, item), False ) )
  return entries

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

def pid_of_text (text):
  match = re . search ( r"^pid:[ \t]*(.*?)[ \t]*\r?$", text, re . MULTILINE )
  if match is None:
    return None
  try:
    return gnid_of_scalar (None, PID, match . group (1))
  except RefusedFile:
    return None

def newline_of (text):
  index = text . find ("\n")
  if index > 0 and text [index - 1] == "\r":
    return "\r\n"
  return "\n"

def print_report (report, arguments):
  total = sum ( report . attributed_by_skgrepo . values () )
  print ( "{}{} files attributed" . format (
    "(dry run: nothing written) " if arguments . dry_run else "",
    total ) )
  for (name, count) in report . attributed_by_skgrepo . items ():
    print ( "  {}: {}" . format (name, count) )
  print ( "already attributed: {}" . format (report . already_attributed) )
  for reason in SKIP_REASONS:
    print ( "skipped, {}: {}" . format (reason, report . skipped [reason]) )
  for skgrepo in report . missing_skgrepos:
    print ( "not found, not read: skgrepo {} at {}" . format (
      skgrepo . name, skgrepo . path ) )
  if report . refused_files:
    print ( "refused, left unchanged: {}" . format (
      len (report . refused_files) ) )
    for (path, reason) in report . refused_files:
      print ( "  {}: {}" . format (path, reason) )
  commit_lines = [
    "git -C {} commit -am '{}'" . format (skgrepo . path, COMMIT_MESSAGE)
    for skgrepo in report . changed_skgrepos
    if is_a_gitrepos_top_level (skgrepo . path) ]
  if commit_lines:
    print ( "{}commit each changed gitrepo:" . format (
      "after a run without --dry-run, " if arguments . dry_run else "" ) )
    for line in commit_lines:
      print ( "  " + line )

# Whether 'root' is the top level of a gitrepo's work tree. A
# folder deeper inside one gets no commit line: 'git -C root commit
# -am' would commit every tracked change of the enclosing gitrepo.
def is_a_gitrepos_top_level (root):
  top = git_output ( root, "rev-parse", "--show-toplevel" )
  return ( top is not None
           and os . path . realpath ( top . strip () )
               == os . path . realpath (root) )

def read_text (path):
  with open (path, encoding = "utf-8", newline = "") as f:
    return f . read ()

def write_text (path, text):
  with open (path, "w", encoding = "utf-8", newline = "") as f:
    f . write (text)

if __name__ == "__main__":
  sys . exit ( main ( sys . argv [1:] ) )
