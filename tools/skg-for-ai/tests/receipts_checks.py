"""The checks of 'test_skg_for_ai.py' on the ninth scratch server, over
mail: 'receipt'."""

import os

from harness import (
  CaseFailure, require_equal, run_tool, skg_list )

# ============================================================
# Receipts: fred's mailboxes 'box' (holding 'letter' and 'aside') and
# 'box2' (holding 'aside'), which the reader's 'boxfork' and
# 'boxfork2', in the owned 'mine', fork; 'loner' is in no mailbox.
# The reader also owns 'private'.
# ============================================================

RECEIPT_FIXTURE_FILES = {
  "skgconfig.toml" :
    'tantivy_folder = ".index.tantivy"\nport = 0\ntiming_log = false\n'
    'beep_when_server_becomes_available = false\n\n'
    '[[repos]]\nname = "fred"\npath = "fred/public"\nforeign = true\n\n'
    '[[repos]]\nname = "mine"\npath = "me/mine"\n\n'
    '[[repos]]\nname = "private"\npath = "me/private"\n',
  "fred/public/box.skg" :
    "pid: box\ntitle: the box\ncontains:\n- letter\n- aside\n",
  "fred/public/box2.skg" : "pid: box2\ntitle: the other box\ncontains:\n- aside\n",
  "fred/public/letter.skg" : "pid: letter\ntitle: a letter\n",
  "fred/public/aside.skg" : "pid: aside\ntitle: an aside\n",
  "fred/public/loner.skg" : "pid: loner\ntitle: a loner\n",
  "me/mine/boxfork.skg" : "pid: boxfork\ntitle: my box\nforks:\n- box\n",
  "me/mine/boxfork2.skg" : "pid: boxfork2\ntitle: my other box\nforks:\n- box2\n",
  "me/private/secret.skg" : "pid: secret\ntitle: a secret\n" }

def write_receipt_fixture (data_root):
  for relative_path, text in RECEIPT_FIXTURE_FILES . items ():
    path = os . path . join (data_root, relative_path)
    os . makedirs ( os . path . dirname (path), exist_ok = True )
    with open (path, "w", encoding = "utf-8") as fixture_file:
      fixture_file . write (text)

def failures_of_receipts (scratch, data_root):
  """'receipt ITEM TEXT' without '--yes' prints what it would do and
  writes nothing; with it, forks the item, writes the receipt under
  the fork and hides the item in the fork of its mailbox. A second
  receipt for it goes under the same fork, hiding nothing more. An
  item in no mailbox asks for '--fork-into'; one in two asks for
  '--mailbox'."""
  context = { "scratch" : scratch, "data_root" : data_root,
              "main_repo" : os . path . join (data_root, "me", "mine") }
  try:
    check_receipts (context)
  except CaseFailure as failure:
    return [ "check_receipts: " + str (failure) ]
  return []

def require_lines_in (completed, lines, returncode = 0, stderr_text = ""):
  """COMPLETED exited RETURNCODE, printing each of LINES, and STDERR_TEXT
  on standard error."""
  printed = completed . stdout . split ("\n")
  if ( completed . returncode != returncode
       or stderr_text not in completed . stderr
       or any ( line not in printed for line in lines ) ):
    raise CaseFailure (
      f"exit {completed . returncode}, printed:\n{completed . stdout}stderr: "
      f"{completed . stderr}\nexpected exit {returncode}, the lines {lines!r} "
      f"and on stderr {stderr_text!r}" )

def check_receipts (context):
  forks = ( "Forks 'a letter' (letter, of 'fred') into 'mine', and writes the "
            "receipt 'Read.' as the first child of the fork." )
  hides = "Hides 'a letter' from 'my box', as content of 'the box'."
  require_lines_in ( run_tool ([ "receipt", "letter", "Done." ]),
    [ "Not written. The receipt would do this; run again with '--yes':",
      forks, hides ] )
  require_equal ( "my box's hides after the preview",
                  skg_list (context, "boxfork", "hides"), [] )
  completed = run_tool ([ "receipt", "letter", "Done.", "--yes" ])
  require_lines_in ( completed, [ forks, hides ] )
  if "Server: wrote the receipt " not in completed . stdout:
    raise CaseFailure ( f"it printed {completed . stdout!r}" )
  require_equal ( "my box's hides", skg_list (context, "boxfork", "hides"),
                  [ "letter" ] )
  again = run_tool ([ "receipt", "letter", "Answered.", "--title", "Answered.",
                      "--yes" ])
  require_lines_in ( again, [ "Hides nothing: 'my box' already hides 'a letter'." ] )
  if "your fork of it, in 'mine'." not in again . stdout:
    raise CaseFailure ( f"the second receipt printed {again . stdout!r}" )
  require_lines_in (
    run_tool ([ "receipt", "loner", "Read.", "--yes" ]), [], 1,
    "Run again with '--fork-into' one of: mine, private." )
  require_lines_in (
    run_tool ([ "receipt", "loner", "Read.", "--fork-into", "private", "--yes" ]),
    [ "Forks 'a loner' (loner, of 'fred') into 'private', and writes the "
      "receipt 'Read.' as the first child of the fork.",
      "Hides nothing: no note of another's that holds 'a loner' is forked by "
      "a note of yours, so it is in no mailbox you read." ] )
  require_lines_in (
    run_tool ([ "receipt", "aside", "", "--yes" ]), [], 1,
    "Run again with '--mailbox' one of: box ('the box'), box2 ('the other box')." )
  require_lines_in (
    run_tool ([ "receipt", "aside", "", "--mailbox", "boxfork2", "--yes" ]),
    [ "Hides 'an aside' from 'my other box', as content of 'the other box'." ] )
  require_equal ( "my other box's hides", skg_list (context, "boxfork2", "hides"),
                  [ "aside" ] )
