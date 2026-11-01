#!/usr/bin/env python3
"""PURPOSE: test 'skg_for_ai.py'.

Run from anywhere: 'python3 tools/skg-for-ai/test_skg_for_ai.py'.
It needs a built server, 'target/debug/skg' ('cargo build --bin skg'),
or the binary SKG_BINARY names.

In order, it:
- checks the relationship heralds against the rendering cases both
  clients' tests share ('tests/shared/herald-rendering-cases.txt'),
  those whose text is nothing but relationship and property tokens;
- copies 'test-fixture/' to a scratch data root, starts a server
  there on port SKG_FOR_AI_TEST_PORT (default 1793), and runs each
  command of the tool against it;
- compares each compact view with its file in 'test-expected/', and
  checks that every handle names a headline line of the full view;
- sends a 'repos' request through 'raw', and imports a Markdown file
  through 'import'.
'--update-expected' rewrites the expected files instead of comparing.
"""

import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time

TOOL_FOLDER = os . path . dirname ( os . path . abspath (__file__) )
REPO_ROOT = os . path . dirname ( os . path . dirname (TOOL_FOLDER) )
sys . dont_write_bytecode = True
sys . path . insert (0, TOOL_FOLDER)
import skg_for_ai

FIXTURE_FOLDER = os . path . join (TOOL_FOLDER, "test-fixture")
EXPECTED_FOLDER = os . path . join (TOOL_FOLDER, "test-expected")
SHARED_HERALD_CASES = os . path . join (
  REPO_ROOT, "tests", "shared", "herald-rendering-cases.txt" )
SERVER_BINARY = os . environ . get (
  "SKG_BINARY", os . path . join (REPO_ROOT, "target", "debug", "skg") )
PORT = int ( os . environ . get ( "SKG_FOR_AI_TEST_PORT", "1793" ))
SERVER_READY_TIMEOUT_SECONDS = 60
HANDLE_PATTERN = re . compile (r'⟨([0-9]+)[ ⟩]')

# Each run: the files' base name, then the tool's arguments.
TOOL_RUNS = [
  ( "view_root-id",          [ "view", "root-id" ] ),
  ( "search_Fred",           [ "search", "Fred" ] ),
  ( "links-here_child-b-id", [ "links-here", "child-b-id" ] ),
  ( "contains_shared-id",    [ "contains", "shared-id", "--no-summary" ] ) ]

def main (argv):
  update_expected = "--update-expected" in argv
  failures = []
  failures += failures_of_shared_herald_cases ()
  failures += failures_against_a_scratch_server (update_expected)
  for failure in failures:
    print ( "FAIL: " + failure )
  if failures:
    sys . exit (1)
  print ( "PASS" + ( " (expected files rewritten)" if update_expected else "" ))

# ============================================================
# Relationship heralds
# ============================================================

def failures_of_shared_herald_cases ():
  failures = []
  checked = 0
  for name, metadata_text, text in shared_herald_cases ():
    if not relationship_tokens_only (metadata_text, text):
      continue
    checked += 1
    rendered = skg_for_ai . relationship_heralds_text (
      skg_for_ai . parse_sexp (metadata_text) )
    if rendered != text:
      failures . append (
        f"herald case '{name}': rendered '{rendered}', expected '{text}'" )
  if checked < 30:
    failures . append (
      f"only {checked} shared herald cases were relationship-only" )
  return failures

def shared_herald_cases ():
  """[(NAME, METADATA, TEXT)] for each case with a text line."""
  cases = []
  name, metadata = None, None
  with open (SHARED_HERALD_CASES, encoding = "utf-8") as cases_file:
    for line in cases_file . read () . split ("\n"):
      if line . startswith ("==== "):
        name, metadata = line [ 5 : ], None
      elif line . startswith ("---- text: ") and metadata:
        cases . append (( name, metadata, line [ 11 : ] ))
      elif name and metadata is None and line . startswith ("(skg"):
        metadata = line
  return cases

def relationship_tokens_only (metadata_text, text):
  """Whether the case's whole herald text is relationship and property
  tokens: its metadata has relationship facts, and its text no other
  glyph."""
  return ( ( "(rels" in metadata_text or "(props" in metadata_text )
           and all ( re . fullmatch (
                       r'[0-9a-z{}/]*[CLSOHAIF]ĥ?[0-9a-z{}/]*', token )
                     for token in text . split (" ") ))

# ============================================================
# The scratch server
# ============================================================

def failures_against_a_scratch_server (update_expected):
  if not os . path . exists (SERVER_BINARY):
    return [ f"no server binary at {SERVER_BINARY}; run 'cargo build --bin skg'" ]
  if port_is_listening (PORT):
    return [ f"port {PORT} is taken; set SKG_FOR_AI_TEST_PORT" ]
  scratch = tempfile . mkdtemp ( prefix = "skg-for-ai-test-" )
  data_root = os . path . join (scratch, "data")
  output_folder = os . path . join (scratch, "out")
  shutil . copytree (FIXTURE_FOLDER, data_root)
  set_port_in_config ( os . path . join (data_root, "skgconfig.toml"), PORT )
  server_log_path = os . path . join (scratch, "server.log")
  with open (server_log_path, "w") as server_log:
    server = subprocess . Popen (
      [ SERVER_BINARY, "skgconfig.toml" ], cwd = data_root,
      stdout = server_log, stderr = subprocess . STDOUT )
  try:
    if not wait_for_server_ready (server_log_path, server):
      return [ f"the scratch server did not become ready; see {server_log_path}" ]
    failures = []
    for base_name, tool_arguments in TOOL_RUNS:
      failures += failures_of_one_run (
        base_name, tool_arguments, output_folder, update_expected )
    failures += failures_of_raw_and_import (scratch)
    failures += failures_of_changed_skg_files (data_root)
  finally:
    server . terminate ()
    server . wait (timeout = 30)
  if failures:
    print ( f"Scratch files kept in {scratch}" )
  else:
    shutil . rmtree (scratch)
  return failures

def failures_of_one_run (base_name, tool_arguments, output_folder,
                         update_expected):
  completed = run_tool ( tool_arguments + [ "--out-dir", output_folder ] )
  if completed . returncode != 0:
    return [ f"'{' ' . join (tool_arguments)}' failed: {completed . stderr}" ]
  written = completed . stdout . split ()
  expected_written = [ os . path . join (output_folder, base_name + suffix)
                       for suffix in ( ".full.org", ".compact.org", ".view.json" ) ]
  if written != expected_written:
    return [ f"'{' ' . join (tool_arguments)}' printed {written}" ]
  sidecar = json . loads ( read_file ( expected_written [2] ))
  expected_view_id = ( "search:" + tool_arguments [1]
                       if tool_arguments [0] == "search"
                       else "content-view:" + tool_arguments [1] )
  if sidecar [ "view_id" ] != expected_view_id:
    return [ f"{base_name}: the sidecar's view ID is {sidecar [ 'view_id' ]}" ]
  full_lines = read_file ( expected_written [0] ) . split ("\n")
  compact = read_file ( expected_written [1] )
  failures = failures_of_handles (base_name, full_lines, compact)
  expected_path = os . path . join (EXPECTED_FOLDER, base_name + ".compact.org")
  if update_expected:
    os . makedirs (EXPECTED_FOLDER, exist_ok = True)
    shutil . copyfile ( expected_written [1], expected_path )
  elif compact != read_file (expected_path):
    failures . append (
      f"{base_name}: the compact view differs from {expected_path}; "
      f"got {expected_written [1]}" )
  return failures

def failures_of_raw_and_import (scratch):
  failures = []
  raw = run_tool ([ "raw", '((request . "repos"))' ])
  expected_repos = ( '((response-type repos) (restriction "main") (repos ('
                     '((name "fred") (path "fred/public") (owned "false")) '
                     '((name "main") (path "me/main") (owned "true")))))' )
  if raw . returncode != 0 or raw . stdout . split ("\n") [1] != expected_repos:
    failures . append ( f"'raw' printed {raw . stdout!r}, {raw . stderr!r}" )
  import_folder = os . path . join (scratch, "import")
  os . makedirs (import_folder)
  with open ( os . path . join (import_folder, "imported.md"), "w",
              encoding = "utf-8" ) as markdown_file:
    markdown_file . write ( "# An imported heading\n\nIts text.\n" )
  imported = run_tool ([ "import", import_folder, "main" ])
  if ( imported . returncode != 0
       or "(response-type import-md-and-org-result) (content \"Imported "
          not in imported . stdout ):
    failures . append (
      f"'import' printed {imported . stdout!r}, {imported . stderr!r}" )
  return failures

def run_tool (tool_arguments):
  return subprocess . run (
    [ sys . executable, os . path . join (TOOL_FOLDER, "skg_for_ai.py") ]
    + tool_arguments + [ "--port", str (PORT) ],
    capture_output = True, text = True )

def failures_of_handles (base_name, full_lines, compact):
  """Each handle names a headline of the full view, every headline is
  named once, and the compact tree has one line per full-view line."""
  tree_lines = [ line for line in compact . split ("\n") [ : -1 ]
                 if not line . startswith ("# ") ]
  full_lines = full_lines [ : -1 ] if full_lines [-1] == "" else full_lines
  failures = []
  if len (tree_lines) != len (full_lines):
    failures . append (
      f"{base_name}: {len (tree_lines)} tree lines for {len (full_lines)} full lines" )
  for index, line in enumerate (tree_lines):
    match = HANDLE_PATTERN . search (line)
    if line . startswith ("*"):
      if not match or int ( match . group (1) ) != index + 1:
        failures . append ( f"{base_name}: line {index + 1} has handle {line!r}" )
      elif not full_lines [index] . startswith ("*"):
        failures . append ( f"{base_name}: handle {index + 1} is no headline" )
  return failures

def failures_of_changed_skg_files (data_root):
  """'links-here' sends a save, and 'import' adds files; neither may
  change a '.skg' file the fixture has."""
  failures = []
  for folder, _, file_names in os . walk (FIXTURE_FOLDER):
    for file_name in file_names:
      if not file_name . endswith (".skg"):
        continue
      fixture_path = os . path . join (folder, file_name)
      scratch_path = os . path . join (
        data_root, os . path . relpath (fixture_path, FIXTURE_FOLDER) )
      if read_file (fixture_path) != read_file (scratch_path):
        failures . append ( f"the runs changed {scratch_path}" )
  return failures

def set_port_in_config (config_path, port):
  text = read_file (config_path)
  with open (config_path, "w", encoding = "utf-8") as config_file:
    config_file . write (
      re . sub ( r'(?m)^port = [0-9]+', f"port = {port}", text ))

def wait_for_server_ready (server_log_path, server):
  deadline = time . time () + SERVER_READY_TIMEOUT_SECONDS
  while time . time () < deadline:
    if server . poll () is not None:
      return False
    if "Server ready." in read_file (server_log_path):
      return True
    time . sleep (0.2)
  return False

def port_is_listening (port):
  with socket . socket () as probe:
    return probe . connect_ex (( "127.0.0.1", port )) == 0

def read_file (path):
  with open (path, encoding = "utf-8") as input_file:
    return input_file . read ()

if __name__ == "__main__":
  main ( sys . argv [ 1 : ] )
