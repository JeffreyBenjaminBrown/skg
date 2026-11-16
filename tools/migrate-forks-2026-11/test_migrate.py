#!/usr/bin/env python3
"""Tests of migrate.py, over temporary directories. One-time, like the
tool: run by hand ('python3 tools/migrate-forks-2026-11/test_migrate.py'),
not by any suite."""

import json
import os
import shutil
import subprocess
import sys
import tempfile
import textwrap
import unittest

MIGRATE_PY = os . path . join (
  os . path . dirname ( os . path . abspath (__file__) ), "migrate.py" )

def skg (text):
  return textwrap . dedent (text) . lstrip ("\n")

class MigrationTest (unittest.TestCase):

  def setUp (self):
    self . tmp = tempfile . mkdtemp (prefix = "migrate-forks-test-")

  def tearDown (self):
    shutil . rmtree (self . tmp)

  def write (self, relative_path, text):
    path = os . path . join (self . tmp, relative_path)
    os . makedirs ( os . path . dirname (path), exist_ok = True )
    with open (path, "w", encoding = "utf-8", newline = "") as f:
      f . write (text)
    return path

  def read (self, relative_path):
    with open ( os . path . join (self . tmp, relative_path),
                encoding = "utf-8", newline = "" ) as f:
      return f . read ()

  def run_tool (self, *arguments):
    """Runs migrate.py with each DIR or FILE relative to the temporary
directory; returns (exit code, stdout)."""
    absolute = [ a if a . startswith ("--")
                 else os . path . join (self . tmp, a)
                 for a in arguments ]
    result = subprocess . run (
      [ sys . executable, MIGRATE_PY ] + absolute,
      capture_output = True, text = True )
    return result . returncode, result . stdout

  def assertReports (self, stdout, *lines):
    for line in lines:
      self . assertIn (line, stdout . splitlines ())

  # A pair: 'subscribesTo T' with 'overrides T' becomes one 'forks T'.
  def test_a_pair_becomes_one_fork (self):
    self . write ( "repo/a.skg", skg ("""
      title: A
      pid: a
      contains:
      - c
      subscribesTo:
      - t
      overrides:
      - t
      """ ) )
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"), skg ("""
      title: A
      pid: a
      contains:
      - c
      forks:
      - t
      """ ) )
    self . assertReports (
      out, "1 files changed",
      "relationships: 1 paired, 0 subscribe-only, 0 override-only" )

  # A subscription with no like override becomes a fork. A blank line
  # after the list stays.
  def test_a_subscribe_only_becomes_a_fork (self):
    self . write ( "repo/a.skg", skg ("""
      pid: a
      subscribesTo:
      - t

      flags:
      - SomeFlag
      """ ) )
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"), skg ("""
      pid: a
      forks:
      - t

      flags:
      - SomeFlag
      """ ) )
    self . assertReports (
      out, "relationships: 0 paired, 1 subscribe-only, 0 override-only" )

  # An override with no like subscription is appended after the
  # section's subscriptions, at their indentation.
  def test_an_override_only_is_appended_after_the_subscriptions (self):
    self . write ( "repo/a.skg", skg ("""
      pid: a
      subscribesTo:
      - s1
      - s2
      overrides:
        - o
      """ ) )
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"), skg ("""
      pid: a
      forks:
      - s1
      - s2
      - o
      """ ) )
    self . assertReports (
      out, "relationships: 0 paired, 2 subscribe-only, 1 override-only" )

  # With no subscriptions, the 'forks' list goes after 'contains', as
  # the server's serializer writes it.
  def test_an_override_only_without_subscriptions_goes_after_contains (self):
    self . write ( "repo/a.skg", skg ("""
      title: A
      pid: a
      body: |2-
        a body
      contains:
      - c
      hidesFromSubs:
      - h
      overrides:
      - o
      """ ) )
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"), skg ("""
      title: A
      pid: a
      body: |2-
        a body
      contains:
      - c
      forks:
      - o
      hides:
      - h
      """ ) )

  # Anchors stay where they were, among the members, in order.
  def test_anchors_are_kept_in_order (self):
    self . write ( "public/a.skg", skg ("""
      title: A
      pid: a
      subscribesTo:
      - p1
      - p2
      """ ) )
    self . write ( "private/a.skg", skg ("""
      pid: a
      subscribesTo:
      - q0
      - anchor: p1
      - q1
      - anchor: 'p2'
      - q2
      overrides:
      - q1
      - o
      """ ) )
    code, out = self . run_tool ("public", "private")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("private/a.skg"), skg ("""
      pid: a
      forks:
      - q0
      - anchor: p1
      - q1
      - anchor: 'p2'
      - q2
      - o
      """ ) )

  # A pair split across two sections of one pid is recorded where the
  # subscription stood; the override is dropped and counted.
  def test_a_pair_split_across_sections_is_recorded_at_the_subscription (self):
    self . write ( "public/a.skg", skg ("""
      title: A
      pid: a
      subscribesTo:
      - t
      """ ) )
    self . write ( "private/a.skg", skg ("""
      pid: a
      contains:
      - c
      overrides:
      - t
      """ ) )
    code, out = self . run_tool ("public", "private")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("public/a.skg"), skg ("""
      title: A
      pid: a
      forks:
      - t
      """ ) )
    self . assertEqual ( self . read ("private/a.skg"), skg ("""
      pid: a
      contains:
      - c
      """ ) )
    self . assertReports (
      out, "2 files changed",
      "relationships: 1 paired, 0 subscribe-only, 0 override-only",
      "overrides dropped as recorded in another section: 1" )

  # A target that is one of the pid's own IDs (its pid, or an extra ID
  # recorded in another section) is dropped and reported.
  def test_a_self_reference_is_dropped (self):
    self . write ( "public/a.skg", skg ("""
      title: A
      pid: a
      extra_ids:
      - a2
      """ ) )
    self . write ( "private/a.skg", skg ("""
      pid: a
      subscribesTo:
      - a
      - t
      overrides:
      - a2
      """ ) )
    code, out = self . run_tool ("public", "private")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("private/a.skg"), skg ("""
      pid: a
      forks:
      - t
      """ ) )
    self . assertReports (
      out, "self-forks dropped: 2",
      "  a named its own ID a", "  a named its own ID a2",
      "relationships: 0 paired, 1 subscribe-only, 0 override-only" )

  # A target listed twice in one section's 'forks' is kept once.
  def test_a_duplicate_is_kept_once (self):
    self . write ( "repo/a.skg", skg ("""
      pid: a
      subscribesTo:
      - t
      - u
      - t
      overrides:
      - o
      - o
      """ ) )
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"), skg ("""
      pid: a
      forks:
      - t
      - u
      - o
      """ ) )
    self . assertReports ( out, "duplicates dropped: 2" )

  # A flow-style list is refused: the file is left unchanged, and so
  # is every other section of its pid, and the tool exits nonzero.
  # Other pids still migrate.
  def test_a_flow_style_list_is_refused (self):
    flow = skg ("""
      pid: a
      overrides: [t]
      """ )
    partner = skg ("""
      title: A
      pid: a
      subscribesTo:
      - t
      """ )
    self . write ("private/a.skg", flow)
    self . write ("public/a.skg", partner)
    self . write ( "public/b.skg", "pid: b\nsubscribesTo:\n- t\n" )
    code, out = self . run_tool ("public", "private")
    self . assertEqual (code, 1)
    self . assertEqual ( self . read ("private/a.skg"), flow )
    self . assertEqual ( self . read ("public/a.skg"), partner )
    self . assertEqual ( self . read ("public/b.skg"), "pid: b\nforks:\n- t\n" )
    self . assertIn ( "its 'overrides' list is not in block style", out )
    self . assertIn ( "left unchanged because another section of their pid was refused: 1", out )

  # A file holding an October 2026 key is refused, unchanged, nonzero.
  def test_an_october_2026_key_is_refused (self):
    old = skg ("""
      pid: a
      subscribes_to:
      - t
      """ )
    self . write ("repo/a.skg", old)
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 1)
    self . assertEqual ( self . read ("repo/a.skg"), old )
    self . assertIn ( "holds the October 2026 key(s) subscribes_to", out )

  # '--dry-run' writes nothing, and reports what a run would.
  def test_dry_run_writes_nothing_and_reports_the_same_counts (self):
    original = "pid: a\nsubscribesTo:\n- t\noverrides:\n- t\n- o\n"
    self . write ("repo/a.skg", original)
    marks = self . write ( "marks.json", json . dumps ( {
      "marks" : [ relationship_mark ("SubscribesTo") ] }, indent = 2 ) )
    with open (marks) as f: marks_before = f . read ()
    code, dry_out = self . run_tool ("--dry-run", "--marks", "marks.json", "repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"), original )
    self . assertEqual ( self . read ("marks.json"), marks_before )
    code, real_out = self . run_tool ("--marks", "marks.json", "repo")
    self . assertEqual (code, 0)
    self . assertEqual (
      dry_out . replace ("(dry run: nothing written) ", ""),
      real_out )

  # A second run changes nothing.
  def test_a_second_run_changes_nothing (self):
    self . write ( "public/a.skg",
                   "title: A\npid: a\nsubscribesTo:\n- t\nhidesFromSubs:\n- h\n" )
    self . write ( "private/a.skg", "pid: a\noverrides:\n- o\n" )
    self . run_tool ("public", "private")
    after_first = ( self . read ("public/a.skg"), self . read ("private/a.skg") )
    code, out = self . run_tool ("public", "private")
    self . assertEqual (code, 0)
    self . assertEqual (
      ( self . read ("public/a.skg"), self . read ("private/a.skg") ),
      after_first )
    self . assertReports ( out, "0 files changed" )

  # A title or body line that reads like a key is not a key.
  def test_a_title_or_body_that_reads_like_a_key_is_untouched (self):
    original = skg ("""
      title: 'overrides: t'
      pid: a
      body: |2-
        subscribesTo:
        - x
        hidesFromSubs:
        overrides:
        - y
      subscribesTo:
      - t
      """ )
    self . write ("repo/a.skg", original)
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"),
                         original . replace ("\nsubscribesTo:\n- t",
                                             "\nforks:\n- t") )
    self . assertReports (
      out, "relationships: 0 paired, 1 subscribe-only, 0 override-only",
      "'hidesFromSubs' keys renamed 'hides': 0" )

  # In a marks file, a mark of a 'SubscribesTo' or an 'Overrides'
  # relationship is dropped; a 'Contains' mark is kept, and a
  # 'HidesFromSubs' mark becomes a 'Hides' mark.
  def test_marks_of_retired_relations_are_dropped (self):
    self . write ( "repo/a.skg", "pid: a\n" )
    self . write ( "marks.json", json . dumps ( {
      "marks" : [ relationship_mark ("SubscribesTo"),
                  relationship_mark ("Overrides"),
                  relationship_mark ("Contains"),
                  relationship_mark ("HidesFromSubs"),
                  { "leaf" : { "Node" : { "pid" : "a", "aspect" : "Text" } },
                    "fingerprint" : 7 } ] }, indent = 2 ) )
    code, out = self . run_tool ("--marks", "marks.json", "repo")
    self . assertEqual (code, 0)
    self . assertEqual (
      self . read ("marks.json"),
      json . dumps ( {
        "marks" : [ relationship_mark ("Contains"),
                    relationship_mark ("Hides"),
                    { "leaf" : { "Node" : { "pid" : "a", "aspect" : "Text" } },
                      "fingerprint" : 7 } ] }, indent = 2 ) )
    self . assertReports ( out, "marks dropped: 2",
                           "marks renamed HidesFromSubs to Hides: 1" )

  # 'hidesFromSubs' becomes 'hides'; its members are unchanged.
  def test_hidesFromSubs_is_renamed_hides (self):
    self . write ( "repo/a.skg", skg ("""
      pid: a
      hidesFromSubs:
        - '55'
        - h2
      flags:
      - F
      """ ) )
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertEqual ( self . read ("repo/a.skg"), skg ("""
      pid: a
      hides:
        - '55'
        - h2
      flags:
      - F
      """ ) )
    self . assertReports ( out, "1 files changed",
                           "'hidesFromSubs' keys renamed 'hides': 1" )

  # A DIR that is a gitrepo's top level gets the line to commit it.
  def test_a_changed_gitrepo_gets_its_commit_line (self):
    self . write ( "repo/a.skg", "pid: a\nsubscribesTo:\n- t\n" )
    subprocess . run ( [ "git", "init", "-q", os . path . join (self . tmp, "repo") ],
                       check = True )
    code, out = self . run_tool ("repo")
    self . assertEqual (code, 0)
    self . assertIn (
      "  git -C {} commit -am 'Migrate to forks'" . format (
        os . path . join (self . tmp, "repo") ),
      out . splitlines () )

  # A DIR deeper inside a gitrepo gets no commit line: committing
  # it with '-am' would commit the enclosing gitrepo's every change.
  def test_a_changed_folder_inside_a_gitrepo_gets_no_commit_line (self):
    self . write ( "outer/repo/a.skg", "pid: a\nsubscribesTo:\n- t\n" )
    subprocess . run ( [ "git", "init", "-q", os . path . join (self . tmp, "outer") ],
                       check = True )
    code, out = self . run_tool ("outer/repo")
    self . assertEqual (code, 0)
    self . assertNotIn ( "commit -am", out )

def relationship_mark (relation):
  return { "leaf" : { "Relationship" : {
             "relation" : relation, "recorder" : "a",
             "member"   : "b",      "aspect"   : "Existence" } },
           "fingerprint" : 42 }

if __name__ == "__main__":
  unittest . main ()
