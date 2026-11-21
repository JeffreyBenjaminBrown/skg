#!/usr/bin/env python3
"""PURPOSE: test that 'bash/skg' runs the release binary if there is
one, else the debug one, passing its arguments and its working
directory through unchanged, from any directory and through a symlink,
and that with neither it says how to build one and fails.

Run from anywhere: 'python3 bash/test_skg_wrapper.py'. It builds
nothing: it copies the wrapper into a temporary repository whose
'target/release/skg' and 'target/debug/skg' are stand-ins, each of
which prints its profile, its working directory and its arguments.
"""
import os
import shutil
import subprocess
import tempfile
import unittest

BASH_FOLDER = os . path . dirname ( os . path . abspath (__file__) )

STAND_IN = """#!/bin/bash
echo "{profile}"
pwd
for argument in "$@"; do echo "[$argument]"; done
"""

def write_executable (path, text):
  os . makedirs ( os . path . dirname (path), exist_ok = True )
  with open (path, "w", encoding = "utf-8") as f:
    f . write (text)
  os . chmod (path, 0o755)

class SkgWrapperTest (unittest . TestCase):

  def setUp (self):
    self . root = os . path . realpath ( tempfile . mkdtemp () )
    self . elsewhere = os . path . realpath ( tempfile . mkdtemp () )
    self . wrapper = os . path . join (self . root, "bash", "skg")
    os . makedirs ( os . path . dirname (self . wrapper) )
    shutil . copy ( os . path . join (BASH_FOLDER, "skg"), self . wrapper )

  def tearDown (self):
    shutil . rmtree (self . root)
    shutil . rmtree (self . elsewhere)

  def stand_in (self, profile):
    write_executable (
      os . path . join (self . root, "target", profile, "skg"),
      STAND_IN . format (profile = profile) )

  def run_wrapper (self, wrapper, *arguments):
    return subprocess . run ( [wrapper, *arguments], cwd = self . elsewhere,
                              capture_output = True, text = True )

  def test_release_is_run_when_both_exist (self):
    self . stand_in ("release")
    self . stand_in ("debug")
    result = self . run_wrapper (self . wrapper, "fix-attributions", "a b", "--help")
    self . assertEqual (result . returncode, 0, result . stderr)
    self . assertEqual ( result . stdout . splitlines (),
                         [ "release", self . elsewhere,
                           "[fix-attributions]", "[a b]", "[--help]" ] )

  def test_debug_is_run_when_there_is_no_release (self):
    self . stand_in ("debug")
    result = self . run_wrapper (self . wrapper, "check-config")
    self . assertEqual ( result . stdout . splitlines (),
                         [ "debug", self . elsewhere, "[check-config]" ] )

  def test_a_symlink_to_the_wrapper_finds_the_repository (self):
    self . stand_in ("debug")
    link = os . path . join (self . elsewhere, "skg")
    os . symlink (self . wrapper, link)
    result = self . run_wrapper (link)
    self . assertEqual ( result . stdout . splitlines (), [ "debug", self . elsewhere ] )

  def test_with_no_binary_it_says_how_to_build_one (self):
    result = self . run_wrapper (self . wrapper, "--help")
    self . assertEqual (result . returncode, 1)
    self . assertEqual (result . stdout, "")
    self . assertIn ( "cargo build --release --bin skg", result . stderr )
    self . assertIn ( self . root, result . stderr )

if __name__ == "__main__":
  unittest . main ()
