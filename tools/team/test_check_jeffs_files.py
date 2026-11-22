#!/usr/bin/env python3
"""PURPOSE: test that 'check_jeffs_files.py' places each write under
data/jeff/ with its writer: his server's save, a tool call of one of
our sessions, a line specialist.sh left in the check's log; that it
names a write nobody explains UNPLACED, a file the listings lost GONE,
and a staged path STAGED; and that '--log' appends the counts and
exactly those lines.

Run from anywhere: 'python3 tools/team/test_check_jeffs_files.py'. It
copies the check into a temporary folder beside a made-up 'data/jeff/'
(one skgrepo, 'public', a gitrepo) and points CLAUDE_CONFIG_DIR at a
made-up transcript; the check finds both from its own path.
"""
import calendar
import json
import os
import shutil
import subprocess
import tempfile
import time
import unittest

TEAM_FOLDER = os . path . dirname ( os . path . abspath (__file__) )

SINCE, UNTIL = "2026-11-20T10:00:00Z", "2026-11-20T11:00:00Z"
SAVE_TIME    = "2026-11-20T10:10:00Z" # his server's save
OURS_TIME    = "2026-11-20T10:20:00Z" # a tool call of ours
SCRIPT_TIME  = "2026-11-20T10:30:00Z" # specialist.sh writes his config
LONELY_TIME  = "2026-11-20T10:40:00Z" # nothing explains this one
BEFORE_TIME  = "2026-11-20T09:00:00Z" # outside the window

def epoch (utc_text):
  return calendar . timegm ( time . strptime (utc_text, "%Y-%m-%dT%H:%M:%SZ") )

def write (path, text, utc_text = None):
  os . makedirs ( os . path . dirname (path), exist_ok = True )
  with open (path, "w", encoding = "utf-8") as f:
    f . write (text)
  if utc_text:
    os . utime ( path, (epoch (utc_text), epoch (utc_text)) )

def git (repo, *args):
  subprocess . run ( [ "git", "-C", repo, "-c", "user.name=t", "-c", "user.email=t@t",
                       *args ], check = True, capture_output = True )

class CheckJeffsFilesTest (unittest . TestCase):

  def setUp (self):
    self . root = tempfile . mkdtemp ()
    self . check = os . path . join (self . root, "tools", "team", "check_jeffs_files.py")
    os . makedirs ( os . path . dirname (self . check) )
    shutil . copy ( os . path . join (TEAM_FOLDER, "check_jeffs_files.py"), self . check )
    jeff = os . path . join (self . root, "data", "jeff")
    self . public = os . path . join (jeff, "public")
    write ( os . path . join (jeff, "skgconfig.toml"),
            '[[repos]]\nname = "public"\npath = "public"\n', SCRIPT_TIME )
    os . makedirs (self . public)
    git (self . public, "init", "-q", "-b", "main")
    for name in [ "saved", "ours", "lonely", "old", "doomed", "staged" ]:
      write ( os . path . join (self . public, name + ".skg"), name + "\n" )
    git (self . public, "add", "-A")
    git (self . public, "commit", "-q", "-m", "first")
    write ( os . path . join (self . public, "staged.skg"), "changed\n", BEFORE_TIME )
    git (self . public, "add", "staged.skg")
    for name, when in [ ("saved", SAVE_TIME), ("ours", OURS_TIME),
                        ("lonely", LONELY_TIME), ("old", BEFORE_TIME),
                        ("doomed", BEFORE_TIME) ]:
      write ( os . path . join (self . public, name + ".skg"), name + "\n", when )
    write ( os . path . join (jeff, "skgconfig.toml.running", "start-servers.1.log"),
            SAVE_TIME + " INFO Deleted 0 file(s), wrote 1 file(s)\n" )
    self . config_dir = os . path . join (self . root, "claude-config")
    transcript = os . path . join (
      self . config_dir, "projects", self . root . replace ("/", "-"), "s.jsonl" )
    write ( transcript, json . dumps ( {
      "timestamp" : OURS_TIME, "type" : "assistant",
      "message" : { "content" : [ { "type" : "tool_use", "name" : "Bash",
                                    "input" : { "command" : "touch data/jeff/public/ours.skg" } } ] }
    } ) + "\n" )
    self . log = os . path . join (self . root, "jeff-check.log")
    write ( self . log, SCRIPT_TIME + " specialist.sh up x: wrote 5 lines of data/jeff/skgconfig.toml\n" )

  def tearDown (self):
    shutil . rmtree (self . root, ignore_errors = True)

  def run_check (self, *args):
    environment = dict ( os . environ, CLAUDE_CONFIG_DIR = self . config_dir )
    environment . pop ("PACER_DIR", None)
    result = subprocess . run ( [ "python3", "-I", self . check, *args ], env = environment,
                                capture_output = True, text = True )
    self . assertEqual (result . returncode, 0, result . stderr)
    return result . stdout

  def test_each_write_is_placed_with_its_writer_and_the_log_gets_the_lines_to_read (self):
    before = os . path . join (self . root, "before.gz")
    after = os . path . join (self . root, "after.gz")
    self . run_check ("--list", before)
    os . remove ( os . path . join (self . public, "doomed.skg") )
    self . run_check ("--list", after)
    report = self . run_check ( SINCE, UNTIL, "--before", before, "--after", after,
                                "--log", self . log, "--label", "a test" )
    self . assertIn ("data/jeff/skgconfig.toml  <- ours by his leave (specialist.sh up x at 2026-11-20 10:30:00)",
                     report)
    with open (self . log, encoding = "utf-8") as log_file:
      log_lines = log_file . read () . splitlines ()
    self . assertEqual (len (log_lines), 8, "\n" . join (log_lines))
    self . assertIn (" a test, 2026-11-20 10:00:00 to 2026-11-20 11:00:00: written OURS? 1, "
                     "UNPLACED 1, his server 1, ours by his leave 1; reflog 0, unplaced 0; "
                     "gone 1 by the listings, 1 by git, his server deleted 0; "
                     "our tool calls naming data/jeff 1", log_lines [1])
    self . assertEqual ( log_lines [2:], [
      "  2026-11-20 10:20:00  data/jeff/public/ours.skg  <- OURS? Bash at 2026-11-20 10:20:00 in s.jsonl",
      "  UNPLACED 2026-11-20 10:40:00  data/jeff/public/lonely.skg",
      "  public               STAGED, NOT COMMITTED: 1, e.g. M  staged.skg",
      "  GONE data/jeff/public/doomed.skg  (was 7 bytes, written 2026-11-20T09:00:00Z)",
      "  MORE GONE THAN HE DELETED: gone 1 by the listings, 1 by git, his server logged 0 deletions",
      "  tool call 2026-11-20 10:20:00  s.jsonl  Bash: touch data/jeff/public/ours.skg" ] )

if __name__ == "__main__":
  unittest . main ()
