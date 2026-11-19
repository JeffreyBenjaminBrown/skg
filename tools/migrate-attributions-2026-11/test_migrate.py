#!/usr/bin/env python3
"""Tests of migrate.py, over temporary directories. One-time, like the
tool: run by hand
('python3 tools/migrate-attributions-2026-11/test_migrate.py'), not by
any suite."""

import os
import shutil
import subprocess
import sys
import tempfile
import textwrap
import unittest

HERE      = os . path . dirname ( os . path . abspath (__file__) )
MIGRATE_PY = os . path . join (HERE, "migrate.py")
HASH_CASES = os . path . join (
  HERE, "..", "..", "tests", "shared", "remote-hash-cases.txt" )

sys . dont_write_bytecode = True
sys . path . insert (0, HERE)
import migrate

def skg (text):
  return textwrap . dedent (text) . lstrip ("\n")

# The hash of 'git@github.com:Jeff/notes.git' (remote-hash-cases.txt).
JEFF_NOTES_HASH = "d1b916a3aef4c6f971a1a0306e9a03529823e5e7"

class SharedHashCasesTest (unittest.TestCase):

  # Every case of the file the Rust side is tested against too.
  def test_every_shared_case (self):
    cases = 0
    with open (HASH_CASES, encoding = "utf-8") as f:
      for line in f:
        line = line . rstrip ("\n")
        if line . startswith ("#") or not line . strip ():
          continue
        url, normalised, hash_ = line . split ("\t")
        with self . subTest (url = url):
          self . assertEqual ( migrate . normalised_remote_url (url),
                               normalised )
          self . assertEqual ( migrate . remote_hash_of_url (url), hash_ )
        cases += 1
    self . assertGreater (cases, 0)

  # The hash is what git itself computes.
  def test_the_hash_is_gits_blob_hash (self):
    result = subprocess . run (
      [ "git", "hash-object", "--stdin" ],
      input = "github.com/Jeff/notes", capture_output = True, text = True,
      check = True )
    self . assertEqual ( result . stdout . strip (), JEFF_NOTES_HASH )

class SafePlainScalarTest (unittest.TestCase):

  def test_safe_and_unsafe_values (self):
    for value in ( "abc", "jeff-public", "d0229507-f4b5-475c-a3c7",
                   "team/x/notes", "a.b_c", JEFF_NOTES_HASH ):
      self . assertTrue ( migrate . is_safe_plain_scalar (value), value )
    for value in ( "true", "On", "NO", "y", "null", "~", "55", "1.5",
                   "1e5", "0x1F", ".inf", "a b", "-a", "a:b", "'a'", "" ):
      self . assertFalse ( migrate . is_safe_plain_scalar (value), value )

class MigrationTest (unittest.TestCase):

  def setUp (self):
    self . tmp = tempfile . mkdtemp (prefix = "migrate-attributions-test-")

  def tearDown (self):
    shutil . rmtree (self . tmp)

  def path (self, relative_path):
    return os . path . join (self . tmp, relative_path)

  def write (self, relative_path, text):
    path = self . path (relative_path)
    os . makedirs ( os . path . dirname (path), exist_ok = True )
    with open (path, "w", encoding = "utf-8", newline = "") as f:
      f . write (text)
    return path

  def read (self, relative_path):
    with open ( self . path (relative_path),
                encoding = "utf-8", newline = "" ) as f:
      return f . read ()

  def config (self, *repos):
    """Writes 'skgconfig.toml' listing each (name, path, foreign); a
name of None is left out."""
    text = "port = 1730\n"
    for (name, path, foreign) in repos:
      os . makedirs ( self . path (path), exist_ok = True )
      text += "\n[[repos]]\n"
      if name is not None:
        text += "name = \"{}\"\n" . format (name)
      text += "path = \"{}\"\n" . format (path)
      if foreign:
        text += "foreign = true\n"
    self . write ("skgconfig.toml", text)

  def git (self, relative_path, *arguments):
    subprocess . run (
      [ "git", "-C", self . path (relative_path) ] + list (arguments),
      check = True, capture_output = True )

  def run_tool (self, *flags):
    result = subprocess . run (
      [ sys . executable, MIGRATE_PY ] + list (flags)
      + [ self . path ("skgconfig.toml") ],
      capture_output = True, text = True )
    return result . returncode, result . stdout

  def assertReports (self, stdout, *lines):
    for line in lines:
      self . assertIn (line, stdout . splitlines ())

  # The usual setting: Jeff's fork in his owned 'mine' of a note in
  # Claude's foreign 'theirs'.
  def a_fork_of_a_foreign_note (self):
    self . config ( ("mine", "mine", False), ("theirs", "theirs", True) )
    self . write ( "theirs/o.skg", "title: O\npid: o\n" )
    self . fork_text = skg ("""
      title: F
      pid: f
      body: |2-
        forks:
        - not-a-key
      contains:
      - c
      forks:
      - anchor: x
      - o
      - p
      """ )
    self . write ("mine/f.skg", self . fork_text)

  # A foreign original whose skgrepo is its own gitrepo with a remote:
  # all three fields, appended as the last key.
  def test_a_foreign_original_with_a_remote (self):
    self . a_fork_of_a_foreign_note ()
    self . git ("theirs", "init", "-q")
    self . git ( "theirs", "remote", "add", "origin",
                 "git@github.com:Jeff/notes.git" )
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual (
      self . read ("mine/f.skg"),
      self . fork_text
      + "attribution:\n  original: o\n  skgrepo: theirs\n"
      + "  remote_hash: " + JEFF_NOTES_HASH + "\n" )
    self . assertReports ( out, "1 files attributed", "  mine: 1" )

  # With no remote, no 'remote_hash'.
  def test_a_foreign_original_with_no_remote (self):
    self . a_fork_of_a_foreign_note ()
    self . git ("theirs", "init", "-q")
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual (
      self . read ("mine/f.skg"),
      self . fork_text + "attribution:\n  original: o\n  skgrepo: theirs\n" )

  # A skgrepo folder inside an enclosing gitrepo has no remote of its
  # own, though the enclosing one has.
  def test_an_original_inside_an_enclosing_gitrepo_has_no_remote (self):
    self . config ( ("mine", "mine", False),
                    ("theirs", "outer/theirs", True) )
    self . write ( "outer/theirs/o.skg", "title: O\npid: o\n" )
    self . write ( "mine/f.skg", "title: F\npid: f\nforks:\n- o\n" )
    self . git ("outer", "init", "-q")
    self . git ( "outer", "remote", "add", "origin",
                 "git@github.com:Jeff/notes.git" )
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual (
      self . read ("mine/f.skg"),
      "title: F\npid: f\nforks:\n- o\n"
      "attribution:\n  original: o\n  skgrepo: theirs\n" )

  # 'origin' is chosen over a remote named before it.
  def test_origin_is_preferred_over_other_remotes (self):
    self . a_fork_of_a_foreign_note ()
    self . git ("theirs", "init", "-q")
    self . git ( "theirs", "remote", "add", "aaa",
                 "https://example.org/other.git" )
    self . git ( "theirs", "remote", "add", "origin",
                 "https://github.com/Jeff/notes" )
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertTrue ( self . read ("mine/f.skg") . endswith (
      "  remote_hash: " + JEFF_NOTES_HASH + "\n" ) )

  # Without 'origin', the alphabetically first remote.
  def test_without_origin_the_first_remote (self):
    self . a_fork_of_a_foreign_note ()
    self . git ("theirs", "init", "-q")
    self . git ( "theirs", "remote", "add", "zzz",
                 "https://example.org/other.git" )
    self . git ( "theirs", "remote", "add", "bbb",
                 "https://github.com/Jeff/notes" )
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertTrue ( self . read ("mine/f.skg") . endswith (
      "  remote_hash: " + JEFF_NOTES_HASH + "\n" ) )

  # A fork of a note in its own home skgrepo is skipped.
  def test_an_original_in_the_forks_own_home_is_skipped (self):
    self . config ( ("mine", "mine", False) )
    self . write ( "mine/o.skg", "title: O\npid: o\n" )
    fork = "title: F\npid: f\nforks:\n- o\n"
    self . write ("mine/f.skg", fork)
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("mine/f.skg"), fork )
    self . assertReports (
      out, "0 files attributed",
      "skipped, original's home is the fork's own home skgrepo: 1" )

  # An original named by an extra ID (recorded in a non-home section)
  # is recorded by its pid; its home is where its title is.
  def test_an_original_named_by_an_extra_id_records_the_pid (self):
    self . config ( ("mine", "mine", False), ("theirs", "theirs", True),
                    ("theirs2", "theirs2", True) )
    self . write ( "theirs/o.skg", "title: O\npid: o\n" )
    self . write ( "theirs2/o.skg", "pid: o\nextra_ids:\n- o-old\n" )
    self . write ( "mine/f.skg", "title: F\npid: f\nforks:\n- o-old\n" )
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual (
      self . read ("mine/f.skg"),
      "title: F\npid: f\nforks:\n- o-old\n"
      "attribution:\n  original: o\n  skgrepo: theirs\n" )

  # An original in no configured skgrepo is skipped.
  def test_an_original_not_loaded_is_skipped (self):
    self . config ( ("mine", "mine", False) )
    fork = "title: F\npid: f\nforks:\n- elsewhere\n"
    self . write ("mine/f.skg", fork)
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("mine/f.skg"), fork )
    self . assertReports (
      out, "skipped, original not in any configured skgrepo: 1" )

  # A file already attributed is untouched, even if its attribution
  # disagrees with what the tool would write.
  def test_an_already_attributed_file_is_untouched (self):
    self . config ( ("mine", "mine", False), ("theirs", "theirs", True) )
    self . write ( "theirs/o.skg", "title: O\npid: o\n" )
    fork = ( "title: F\npid: f\nforks:\n- o\n"
             "attribution:\n  original: q\n  skgrepo: else\n" )
    self . write ("mine/f.skg", fork)
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("mine/f.skg"), fork )
    self . assertReports ( out, "0 files attributed", "already attributed: 1" )

  # Only owned skgrepos are written: a foreign skgrepo's fork of an
  # owned note stays as it was.
  def test_only_owned_skgrepos_are_modified (self):
    self . config ( ("mine", "mine", False), ("theirs", "theirs", True) )
    self . write ( "mine/o.skg", "title: O\npid: o\n" )
    theirs = "title: T\npid: t\nforks:\n- o\n"
    self . write ("theirs/t.skg", theirs)
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("theirs/t.skg"), theirs )
    self . assertReports ( out, "0 files attributed", "  mine: 0" )
    self . assertNotIn ( "  theirs: 0", out . splitlines () )

  # A section without a title is not a home section: untouched.
  def test_a_non_home_section_is_untouched (self):
    self . config ( ("mine", "mine", False), ("mine2", "mine2", False),
                    ("theirs", "theirs", True) )
    self . write ( "theirs/o.skg", "title: O\npid: o\n" )
    self . write ( "mine/f.skg", "title: F\npid: f\n" )
    private = "pid: f\nforks:\n- o\n"
    self . write ("mine2/f.skg", private)
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("mine2/f.skg"), private )
    self . assertEqual ( self . read ("mine/f.skg"), "title: F\npid: f\n" )
    self . assertReports ( out, "0 files attributed" )

  # A skgrepo with no 'name' is named by its path as written.
  def test_an_unnamed_skgrepo_is_named_by_its_path (self):
    self . config ( ("mine", "mine", False), (None, "sub/theirs", True) )
    self . write ( "sub/theirs/o.skg", "title: O\npid: o\n" )
    self . write ( "mine/f.skg", "title: F\npid: f\nforks:\n- o\n" )
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertTrue ( self . read ("mine/f.skg") . endswith (
      "attribution:\n  original: o\n  skgrepo: sub/theirs\n" ) )

  # '--dry-run' writes nothing, and reports what a run would.
  def test_dry_run_writes_nothing_and_reports_the_same_counts (self):
    self . a_fork_of_a_foreign_note ()
    self . git ("mine", "init", "-q")
    code, dry_out = self . run_tool ("--dry-run")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("mine/f.skg"), self . fork_text )
    code, real_out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertNotEqual ( self . read ("mine/f.skg"), self . fork_text )
    self . assertEqual (
      dry_out . replace ("(dry run: nothing written) ", "")
              . replace ("after a run without --dry-run, ", ""),
      real_out )

  # A second run changes nothing.
  def test_a_second_run_changes_nothing (self):
    self . a_fork_of_a_foreign_note ()
    self . run_tool ()
    after_first = self . read ("mine/f.skg")
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("mine/f.skg"), after_first )
    self . assertReports ( out, "0 files attributed", "already attributed: 1" )

  # A file not ending in a newline gets one before the block; a file
  # of CRLF lines gets CRLF lines.
  def test_a_missing_final_newline_is_added (self):
    self . config ( ("mine", "mine", False), ("theirs", "theirs", True) )
    self . write ( "theirs/o.skg", "title: O\npid: o\n" )
    self . write ( "mine/f.skg", "title: F\npid: f\nforks:\n- o" )
    self . write ( "mine/g.skg", "title: G\r\npid: g\r\nforks:\r\n- o\r\n" )
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual (
      self . read ("mine/f.skg"),
      "title: F\npid: f\nforks:\n- o\n"
      "attribution:\n  original: o\n  skgrepo: theirs\n" )
    self . assertEqual (
      self . read ("mine/g.skg"),
      "title: G\r\npid: g\r\nforks:\r\n- o\r\n"
      "attribution:\r\n  original: o\r\n  skgrepo: theirs\r\n" )

  # A value that would need YAML quoting is refused: the file is left
  # unchanged, the tool exits 1, and other files are still written.
  def test_a_value_needing_quoting_is_refused (self):
    self . config ( ("mine", "mine", False), ("true", "t", True),
                    ("on", "u", True), ("theirs", "theirs", True) )
    self . write ( "t/o.skg", "title: O\npid: o\n" )
    self . write ( "u/p.skg", "title: P\npid: p\n" )
    self . write ( "theirs/q.skg", "title: Q\npid: q\n" )
    self . write ( "theirs/n.skg", "title: N\npid: '55'\n" )
    for (name, original) in ( ("f", "o"), ("g", "p"), ("h", "q"),
                              ("i", "'55'") ):
      self . write ( "mine/{}.skg" . format (name),
                     "title: X\npid: {}\nforks:\n- {}\n" . format (
                       name, original ) )
    code, out = self . run_tool ()
    self . assertEqual (code, 1)
    for name in ("f", "g", "i"):
      self . assertNotIn ( "attribution", self . read (
        "mine/{}.skg" . format (name) ) )
    self . assertIn ( "attribution", self . read ("mine/h.skg") )
    self . assertReports ( out, "1 files attributed", "refused, left unchanged: 3" )
    self . assertIn ( "its attribution's skgrepo 'true' would need YAML quoting", out )
    self . assertIn ( "its attribution's skgrepo 'on' would need YAML quoting", out )
    self . assertIn ( "its attribution's original '55' would need YAML quoting", out )

  # An owned skgrepo that is a gitrepo's top level gets its commit
  # line; one inside an enclosing gitrepo gets none.
  def test_commit_lines (self):
    self . config ( ("mine", "mine", False), ("inner", "outer/inner", False),
                    ("theirs", "theirs", True) )
    self . write ( "theirs/o.skg", "title: O\npid: o\n" )
    self . write ( "mine/f.skg", "title: F\npid: f\nforks:\n- o\n" )
    self . write ( "outer/inner/g.skg", "title: G\npid: g\nforks:\n- o\n" )
    self . git ("mine", "init", "-q")
    self . git ("outer", "init", "-q")
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertReports (
      out, "2 files attributed",
      "  git -C {} commit -am 'Record fork attributions'" . format (
        self . path ("mine") ) )
    self . assertNotIn ( "inner commit", out )
    self . assertEqual ( out . count ("commit -am"), 1 )

  # Files in a skgrepo's subfolders are not read.
  def test_subfolders_are_not_read (self):
    self . config ( ("mine", "mine", False), ("theirs", "theirs", True) )
    self . write ( "theirs/o.skg", "title: O\npid: o\n" )
    nested = "title: F\npid: f\nforks:\n- o\n"
    self . write ("mine/sub/f.skg", nested)
    code, out = self . run_tool ()
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("mine/sub/f.skg"), nested )

  # Bad usage exits 2.
  def test_bad_usage (self):
    result = subprocess . run ( [ sys . executable, MIGRATE_PY ],
                                capture_output = True, text = True )
    self . assertEqual (result . returncode, 2)
    result = subprocess . run (
      [ sys . executable, MIGRATE_PY, self . path ("absent.toml") ],
      capture_output = True, text = True )
    self . assertEqual (result . returncode, 2)

if __name__ == "__main__":
  unittest . main ()
