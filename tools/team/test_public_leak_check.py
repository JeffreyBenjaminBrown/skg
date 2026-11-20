#!/usr/bin/env python3
"""PURPOSE: test that 'public-leak-check.sh' flags a note of Claude's
public skgrepo that contains, forks or links to a note of one of
Jeff's private skgrepos, and passes one that links to a note of his
'deciduous' skgrepo, a public note (by its pid or an extra ID), a note
of 'skg-system/', or that is a fork whose original no longer exists.

Run from anywhere: 'python3 tools/team/test_public_leak_check.py'. It
copies the check into a temporary folder beside a made-up 'data/' and
'skg-system/'; the check finds both from its own path.
"""
import os
import shutil
import subprocess
import tempfile
import unittest

TEAM_FOLDER = os . path . dirname ( os . path . abspath (__file__) )

CONFIG = """
[[repos]]
name = "jeff-public"
path = "../jeff/public"
foreign = true

[[repos]]
name = "jeff-decid"
path = "../jeff/deciduous"
foreign = true

[[repos]]
name = "jeff-pers-i"
path = "../jeff/personal-ish"
foreign = true

[[repos]]
name = "skg-system"
path = "../../skg-system"
foreign = true

[[repos]]
name = "claude-public"
path = "public"

[[repos]]
name = "claude-private"
path = "private"
"""

def uuid (n):
  return "%08x-0000-4000-8000-000000000000" % n

JEFF_PUBLIC   = uuid (1)
JEFF_EXTRA    = uuid (2) # an extra ID of JEFF_PUBLIC
DECID         = uuid (3)
PERSONAL      = uuid (4)
SYSTEM        = uuid (5)
VANISHED      = uuid (6)

def note (pid, title, body = None, contains = (), forks = (),
          extra_ids = (), attribution = None):
  lines = [ "title: " + title, "pid: " + pid ]
  if extra_ids:
    lines += [ "extra_ids:" ] + [ "- " + i for i in extra_ids ]
  if body is not None:
    lines += [ "body: |2-", "  " + body ]
  if contains:
    lines += [ "contains:" ] + [ "- " + i for i in contains ]
  if forks:
    lines += [ "forks:" ] + [ "- " + i for i in forks ]
  if attribution:
    original, skgrepo = attribution
    lines += [ "attribution:", "  original: " + original,
               "  skgrepo: " + skgrepo,
               "  remote_hash: " + "a" * 40, "  commit: " + "b" * 40 ]
  return "\n" . join (lines) + "\n"

# The notes of Claude's public skgrepo, by pid: whether each should be
# flagged, and the note.
CLAUDE_PUBLIC = {
  uuid (100): (False, note (uuid (100), "links decid",
                            body = "see [[id:%s][that]]" % DECID)),
  uuid (101): (True,  note (uuid (101), "links personal",
                            body = "see [[id:%s][that]]" % PERSONAL)),
  uuid (102): (True,  note (uuid (102), "contains decid",
                            contains = [ DECID ])),
  uuid (103): (True,  note (uuid (103), "forks decid", forks = [ DECID ],
                            attribution = (DECID, "jeff-decid"))),
  uuid (104): (False, note (uuid (104), "names own extra ID",
                            extra_ids = [ uuid (204) ],
                            body = "was " + uuid (204))),
  uuid (105): (False, note (uuid (105), "forks a vanished original",
                            forks = [ VANISHED ],
                            attribution = (VANISHED, "jeff-public"))),
  uuid (106): (False, note (uuid (106), "links system and extra",
                            body = "[[id:%s][a]] [[id:%s][b]]"
                                   % (SYSTEM, JEFF_EXTRA))),
  uuid (107): (True,  note (uuid (107), "forks a vanished personal note",
                            forks = [ VANISHED ],
                            attribution = (VANISHED, "jeff-pers-i"))),
  uuid (108): (False, note (uuid (108), "contains public",
                            contains = [ JEFF_PUBLIC, uuid (100) ])),
}

def write (folder, pid, text):
  os . makedirs (folder, exist_ok = True)
  with open (os . path . join (folder, pid + ".skg"), "w") as f:
    f . write (text)

class PublicLeakCheckTest (unittest . TestCase):
  def setUp (self):
    self . root = tempfile . mkdtemp ()
    team = os . path . join (self . root, "tools", "team")
    os . makedirs (team)
    shutil . copy (os . path . join (TEAM_FOLDER, "public-leak-check.sh"),
                   team)
    data = os . path . join (self . root, "data")
    write (os . path . join (data, "jeff", "public"), JEFF_PUBLIC,
           note (JEFF_PUBLIC, "public", extra_ids = [ JEFF_EXTRA ]))
    write (os . path . join (data, "jeff", "deciduous"), DECID,
           note (DECID, "decid"))
    write (os . path . join (data, "jeff", "personal-ish"), PERSONAL,
           note (PERSONAL, "personal"))
    write (os . path . join (self . root, "skg-system"), SYSTEM,
           note (SYSTEM, "system"))
    os . makedirs (os . path . join (data, "claude", "private"))
    with open (os . path . join (data, "claude", "skgconfig.toml"),
               "w") as f:
      f . write (CONFIG)
    for pid, (_, text) in CLAUDE_PUBLIC . items ():
      write (os . path . join (data, "claude", "public"), pid, text)

  def tearDown (self):
    shutil . rmtree (self . root)

  def test_flags_exactly_the_leaks (self):
    result = subprocess . run (
      [ "bash", os . path . join (self . root, "tools", "team",
                                  "public-leak-check.sh") ],
      capture_output = True, text = True)
    self . assertEqual (result . stderr, "")
    flagged = { line . split (".skg") [0]
                for line in result . stdout . splitlines () }
    expected = { pid for pid, (leaks, _) in CLAUDE_PUBLIC . items ()
                 if leaks }
    self . assertEqual (flagged, expected, result . stdout)
    self . assertEqual (result . returncode, 1)

if __name__ == "__main__":
  unittest . main ()
