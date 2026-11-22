"""The checks of 'test_skg_for_ai.py' that need no server: the
perspective's names, the forks folder's claims, chains, what heralds
mean, the save envelope, a commit's result text, the preview, no
default port, 'python3 -I', link annotations, the title matching of
retyped headlines, and the shared herald rendering cases."""

import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile

import sexp
import server_connection
import full_view
import compact_view
import heralds
import views
import edited_compact_view
import outline_words
import save_dialogue
import retyped_headlines
import view_commands
import diff_and_commit
import headline_commands
import skg_for_ai

from harness import (
  SHARED_HERALD_CASES, SHARED_HERALD_RULES, SHARED_SKGREPO_LABELS,
  TOOL_FOLDER, read_file, run_tool )

# ============================================================
# The perspective's names
# ============================================================

def failures_of_perspective_names ():
  """A sidecar's perspective is read by the server's names: 'skgrepo
  NAME' as it is; none, 'root homes', and the older forms ('user',
  'owner of NAME', 'root authors'), as the root homes. The command
  line's words are joined, and none means 'next'."""
  failures = []
  for recorded, read in [ ( "skgrepo main", "skgrepo main" ),
                          ( "root homes", "root homes" ),
                          ( "user", "root homes" ),
                          ( "owner of tracy", "root homes" ),
                          ( "root authors", "root homes" ),
                          ( None, "root homes" ) ]:
    sidecar = {} if recorded is None else { "perspective" : recorded }
    if views . perspective_of_sidecar (sidecar) != read:
      failures . append ( f"a sidecar recording {recorded!r} is read as "
                          f"{views . perspective_of_sidecar (sidecar)!r}, not {read!r}" )
  for words, name in [ ( [], "next" ), ( [ "next" ], "next" ),
                       ( [ "root", "homes" ], "root homes" ),
                       ( [ "skgrepo main" ], "skgrepo main" ),
                       ( [ "skgrepo", "main" ], "skgrepo main" ) ]:
    if view_commands . perspective_of_argument (words) != name:
      failures . append ( f"the words {words!r} name "
                          f"{view_commands . perspective_of_argument (words)!r}, not {name!r}" )
  return failures

# ============================================================
# The forks folder's claims
# ============================================================

def failures_of_forks_folder_claims ():
  """In a forks folder a member claims exactly when the folder's
  viewparent forks it. One claiming nothing is a pending detachment
  only when the viewparent still forks it (a detached forkee or
  mutual, whose '(rels (forks (in ...)))' names ancestor generation
  2), not a forker the viewparent does not fork; and 'relRepo:NAME' is
  refused on it, since such a forker records its fork in its own
  file, while a claiming member takes it."""
  full = ( "* (skg (vnid 0a) node (gnid n) (skgRepos (nodeRepo main))) n\n"
           "** (skg (vnid 0b) forksFolder)\n"
           "*** (skg (vnid 0c) node (gnid f) (skgRepos (nodeRepo main)) noClaim writeProtected "
           "(view (rels (forks (out 1 (ancestors 2)))) (birth (forks out 2)))) f\n"
           "*** (skg (vnid 0d) node (gnid e) (skgRepos (nodeRepo main)) noClaim writeProtected "
           "(view (rels (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) e\n"
           "*** (skg (vnid 0e) node (gnid m) (skgRepos (nodeRepo main)) noClaim writeProtected "
           "(view (rels (forks (in 1 (ancestors 2)) (out 1 (ancestors 2)))) "
           "(birth (forks in 2) (forks out 2)))) m\n"
           "*** (skg (vnid 0g) node (gnid k) (skgRepos (nodeRepo main)) writeProtected "
           "(view (rels (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) k\n" )
  compact = ( "* n ⟨0a⟩\n** ⟨0b⟩\n*** f ⟨0c relRepo:main⟩\n"
              "*** e ⟨0d⟩\n*** m ⟨0e⟩\n*** k ⟨0g relRepo:main⟩\n" )
  headline_of_handle = views . headlines_by_viewnode_id (
    full_view . parse_full_view (full) )
  edited = edited_compact_view . parse_edited_compact_view (compact)
  def metadata (handle):
    return headline_of_handle [handle] . metadata
  failures = []
  for handle, pending in [ ( "0c", False ), ( "0d", True ), ( "0e", True ) ]:
    if heralds . is_pending_detachment (
        metadata (handle), metadata ("0b"), metadata ("0a") ) != pending:
      failures . append ( f"the forks folder's member {handle} should "
                          f"{'' if pending else 'not '}be a pending detachment" )
  refusal = outline_words . relRepo_problems (
    edited, 2, headline_of_handle, { "main" : True } )
  if not ( len (refusal) == 1 and "claims nothing" in refusal [0] ):
    failures . append ( f"'relRepo:main' on a forker claiming nothing gave {refusal!r}" )
  allowed = outline_words . relRepo_problems (
    edited, 5, headline_of_handle, { "main" : True } )
  if allowed:
    failures . append ( f"'relRepo:main' on a claiming forkee gave {allowed!r}" )
  # The folder's label asks for nothing: of its handle's words, only
  # those typed after it do.
  for text, asked in [
      ( "** ⟨0b Its mirrors are these.⟩", ( [], [], [], False ) ),
      ( "** ⟨0b Its mirrors are these. relRepo:main subtree⟩",
        ( [], [], [ "relRepo:main" ], True ) ) ]:
    folder = edited_compact_view . parse_edited_compact_view (text) [0]
    found = ( folder . repo_words (), folder . view_request_words (),
              folder . edit_request_words (), folder . asks_subtree () )
    if found != asked:
      failures . append ( f"the handle of {text!r} asks {found!r}, not {asked!r}" )
  return failures

# ============================================================
# Chains ('chain VIEW HANDLE')
# ============================================================

# Jeff's example: a contains b, whose forks folder holds c, which b
# forks; a mentioner folder's member, born of its link though it also
# contains the note; and a mirror joined to its folder's note by no
# edge.
CHAIN_VIEW = """* (skg (vnid 0a) node (gnid a)) a
** (skg (vnid 0b) node (gnid b) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
*** (skg (vnid 0c) forksFolder)
**** (skg (vnid 0d) node (gnid c) (view (rels (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) c
**** (skg (vnid 0e) node (gnid m) (view (rels (forks (in 1))))) m
*** (skg (vnid 0f) mentionerFolder)
**** (skg (vnid 0g) node (gnid r) (view (rels (contains (out 1 (ancestors 2))) (linksTo (out 1 (ancestors 2)))) (birth (linksTo out 2)))) r
***** (skg (vnid 0h) node (gnid h) (view (rels (hides (out 1 (ancestors 1)))))) h
"""

def failures_of_chains ():
  """A chain joins each note to the one above it by the edge its
  metadata records, folders skipped, and refuses a step no edge
  joins; 'chain' prints it."""
  headlines = [ line for line in full_view . parse_full_view (CHAIN_VIEW)
                if line . is_headline ]
  def chain_of (handle):
    index = next ( i for i, line in enumerate (headlines)
                   if full_view . headline_viewnode_id (line . metadata) == handle )
    try:
      return headline_commands . chain_text (headlines, index)
    except server_connection . SkgToolError as error:
      return "refused: " + str (error)
  failures = []
  for handle, expected in [
      ( "0a", "[[id:a][a]]" ),
      ( "0d", "[[id:a][a]] / [[id:b][b]] ~ [[id:c][c]]" ),
      ( "0g", "[[id:a][a]] / [[id:b][b]] <- [[id:r][r]]" ),
      ( "0h", "[[id:a][a]] / [[id:b][b]] <- [[id:r][r]] is hidden by [[id:h][h]]" ),
      ( "0e", "refused: No edge of the graph joins 'm' to 'b', the note above "
              "it in this view, so no chain runs through them." ) ]:
    if chain_of (handle) != expected:
      failures . append ( f"the chain of {handle} is {chain_of (handle)!r}, not {expected!r}" )
  folder = tempfile . mkdtemp ( prefix = "skg-for-ai-chain-" )
  for name, text in [ ( "v.full.org", CHAIN_VIEW ), ( "v.compact.org", "" ),
                      ( "v.view.json", json . dumps ({ "full_view" : "v.full.org",
                                                       "compact_view" : "v.compact.org" }) ) ]:
    with open ( os . path . join (folder, name), "w", encoding = "utf-8" ) as file:
      file . write (text)
  printed = run_tool ([ "chain", os . path . join (folder, "v.full.org"), "0d" ])
  if printed . stdout . strip () != "[[id:a][a]] / [[id:b][b]] ~ [[id:c][c]]":
    failures . append ( f"'chain' printed {printed . stdout!r} {printed . stderr!r}" )
  shutil . rmtree (folder, ignore_errors = True)
  return failures

# ============================================================
# What heralds mean ('describe VIEW HANDLE')
# ============================================================

def failures_of_describe_requests ():
  """'describe' sends the headline's metadata as the full view holds
  it, with 'pendingDetachment' on a pending detachment."""
  view = full_view . parse_full_view (
    CHAIN_VIEW + "** (skg (vnid 0i) node (gnid d) noClaim) d\n" )
  failures = []
  for handle, expected in [
      ( "0d", '((request . "describe heralds") (metadata . "(skg (vnid 0d) '
              'node (gnid c) (view (rels (forks (in 1 (ancestors 2)))) '
              '(birth (forks in 2))))"))' ),
      ( "0i", '((request . "describe heralds") (metadata . "(skg (vnid 0i) '
              'node (gnid d) noClaim pendingDetachment)"))' ) ]:
    actual = headline_commands . describe_request_line (view, handle)
    if actual != expected:
      failures . append ( f"the describe request of {handle} is {actual!r}, not {expected!r}" )
  return failures

# ============================================================
# The save envelope
# ============================================================

def failures_of_save_envelope ():
  """A save's envelope carries the saved buffer, its baseline (the
  text the server last drew for it), and the other views, each string
  quoted."""
  envelope = server_connection . save_envelope_text (
    '* edited "λ"\n', '* drawn "λ"\n', "()" )
  expected = ( '((saved-buffer "* edited \\"λ\\"\n") '
               '(baseline "* drawn \\"λ\\"\n") (other-views ()))' )
  return ( [] if envelope == expected
           else [ f"the save envelope is {envelope!r}, not {expected!r}" ] )

def failures_of_commit_result_text ():
  """A commit's line names each node left because it changed in a way
  no mark covers, with how, and says what can commit it."""
  response = sexp . parse_sexp (
    '((commits (("main" "9c1e40d2"))) (committed-nodes 1) (left-nodes 2)'
    ' (waiting ()) (unmarkable (("h" "H" ("its file changed in a way the diff view does not show (formatting, say)"))'
    ' ("t" "T" ("its file changed in a way the diff view does not show (formatting, say)"))))'
    ' (errors ()) (warnings ()))' )
  text = diff_and_commit . commit_result_text (response)
  expected = ( "Committed 1 node, in main (9c1e40d2). 2 changed nodes are left;"
               " 2 of them changed in ways no mark covers, which 'commit --all'"
               " or Git can commit: 'H' (its file changed in a way the diff view does not show (formatting, say));"
               " 'T' (its file changed in a way the diff view does not show (formatting, say))." )
  return ( [] if text == expected
           else [ f"the commit's line is {text!r}, not {expected!r}" ] )

def failures_of_preview_support ():
  """A dry run previews a save only on a server whose answer to "verify
  connection" names "save preview": an older one ignores the preview
  field, and would save."""
  class Answering:
    def __init__ (self, answer):
      self . answer = answer
    def request (self, line):
      return sexp . parse_sexp (self . answer)
  old = Answering ( '((response-type verify-connection) (content "This is '
                    'the skg server verifying the connection."))' )
  new = Answering ( '((response-type verify-connection) (content "x") '
                    '(supports ("save preview")))' )
  failures = []
  if save_dialogue . server_supports (old, "save preview"):
    failures . append ( "an older server was taken to support the preview" )
  if not save_dialogue . server_supports (new, "save preview"):
    failures . append ( "a server naming the preview was taken not to support it" )
  return failures

# Commands that need a server, each run with no port, no sidecar and
# no daemon: 'commit --all --yes' wrote the user's unfinished changes
# to his own server when a default port named it.
PORTLESS_SERVER_COMMANDS = [
  [ "commit", "A message", "--all", "--yes" ],
  [ "view", "root-id" ],
  [ "raw", "(repos)" ],
  [ "daemon" ],
  [ "linkstack", "push", "root-id" ] ]

def failures_of_no_default_port ():
  """There is no default port. A command that needs a server, given no
  port by '--port' or SKG_PORT, no sidecar and no daemon in its
  folder, is refused, exit 1, with one line on stderr, before it
  connects to anything or writes anything; 'linkstack show', which
  needs no server, still runs."""
  failures = []
  expected_stderr = "skg_for_ai: " + server_connection . NO_PORT_MESSAGE + "\n"
  connections = []
  def refuse_to_connect (*arguments, **keywords):
    connections . append (arguments)
    raise ConnectionRefusedError ( "the test forbids connecting" )
  original_create_connection = socket . create_connection
  original_connect = socket . socket . connect
  socket . create_connection = refuse_to_connect
  socket . socket . connect = refuse_to_connect
  folder = tempfile . mkdtemp ( prefix = "skg-for-ai-test-no-port-" )
  try:
    for command in PORTLESS_SERVER_COMMANDS:
      argv = command + ( [] if command [0] in ( "commit", "raw" )
                         else [ "--out-dir", folder ] )
      described = "'" + " " . join (argv) + "'"
      del connections [ : ]
      try:
        skg_for_ai . main (argv)
        failures . append ( f"{described} without a port was not refused" )
      except server_connection . SkgToolError as error:
        if str (error) != server_connection . NO_PORT_MESSAGE:
          failures . append ( f"{described} without a port said {error}" )
      if connections:
        failures . append ( f"{described} without a port tried to connect" )
      completed = run_tool (argv, port = None, bypassing_daemons = True)
      if ( completed . returncode != 1 or completed . stdout != ""
           or completed . stderr != expected_stderr ):
        failures . append (
          f"{described} without a port: exit {completed . returncode}, "
          f"stdout {completed . stdout!r}, stderr {completed . stderr!r}" )
    if os . listdir (folder):
      failures . append ( f"commands without a port wrote {os . listdir (folder)}" )
    completed = run_tool ( [ "linkstack", "show", "--out-dir", folder ],
                           port = None, bypassing_daemons = True )
    if ( completed . returncode != 0
         or completed . stdout != "The linkstack is empty.\n" ):
      failures . append (
        f"'linkstack show' without a port: exit {completed . returncode}, "
        f"stdout {completed . stdout!r}, stderr {completed . stderr!r}" )
  finally:
    socket . create_connection = original_create_connection
    socket . socket . connect = original_connect
    shutil . rmtree (folder, ignore_errors = True)
  return failures

def failures_of_isolated_run ():
  """Under 'python3 -I', which leaves the script's folder off
  'sys.path', and from another folder, the tool still finds its
  modules: '--help' exits 0 and prints its usage."""
  folder = tempfile . mkdtemp ( prefix = "skg-for-ai-test-isolated-" )
  try:
    completed = subprocess . run (
      [ sys . executable, "-I", os . path . join (TOOL_FOLDER, "skg_for_ai.py"),
        "--help" ],
      capture_output = True, text = True, cwd = folder )
  finally:
    shutil . rmtree (folder, ignore_errors = True)
  if completed . returncode != 0 or not completed . stdout . startswith ("usage:"):
    return [ f"'python3 -I skg_for_ai.py --help': exit {completed . returncode}, "
             f"stdout {completed . stdout!r}, stderr {completed . stderr!r}" ]
  return []

def failures_of_link_annotation_round_trip ():
  """A save strips exactly the link annotations a view wrote, even one
  quoting a target title that holds '⟦', '⟧', '→', '✗' and '⌂', so an
  unedited body or title saves unchanged."""
  title = ( 'The AI reader writes "⟦not held here⟧" where it now writes '
            '"⟦✗ missing⟧", for a link whose target this server holds no '
            'node of. ⟦→ x ⌂:y⟧' )
  statuses = {
    "odd-id"  : { "status" : "resolved", "pid" : "odd-id",
                  "label" : "cl⟧eo", "title" : title },
    "gone-id" : { "status" : "missing" } }
  failures = []
  for text in [ "See [[id:odd-id][the note]] and [[id:gone-id][gone]].",
                "[[id:odd-id]] then [[id:odd-id][again ⟦→ typed⟧]]⟦→ typed⟧" ]:
    annotated = compact_view . annotate_links (text, statuses)
    stripped = edited_compact_view . strip_link_annotations (annotated)
    if stripped != text:
      failures . append (
        f"annotating then stripping {text!r} gives {stripped!r}" )
  return failures

# (the server's entry for a link's target, the annotation it gets)
LINK_ANNOTATION_CASES = [
  ( { "status" : "resolved", "pid" : "t", "label" : "main",
      "title" : "The target" }, "⟦→ The target ⌂:main⟧" ),
  ( { "status" : "missing" }, "⟦not held here⟧" ),
  ( { "status" : "restricted" }, "⟦✗ restricted⟧" ),
  ( None, "⟦✗ lookup failed⟧" ) ]

def failures_of_link_annotations ():
  """Each status gets its annotation, and a save strips each of them,
  and the '⟦✗ missing⟧' of views written before '⟦not held here⟧'."""
  failures = []
  link = "[[id:t][a label]]"
  for entry, annotation in LINK_ANNOTATION_CASES:
    if compact_view . link_annotation (entry) != annotation:
      failures . append (
        f"the link annotation of {entry} is "
        f"{compact_view . link_annotation (entry)!r}, not {annotation!r}" )
  for annotation in ( [ annotation for _, annotation in LINK_ANNOTATION_CASES ]
                      + [ "⟦✗ missing⟧" ] ):
    text = "see " + link + annotation + ", then more"
    stripped = edited_compact_view . strip_link_annotations (text)
    if stripped != "see " + link + ", then more":
      failures . append ( f"{annotation!r} after a link strips to "
                          f"{stripped!r}" )
  if edited_compact_view . strip_link_annotations ("⟦not held here⟧ alone") \
     != "⟦not held here⟧ alone":
    failures . append ( "'⟦not held here⟧' after no link is stripped" )
  return failures

# ============================================================
# Retyped headlines: the title normalizer and the fuzzy match
# ============================================================

# (TITLE, its normalized words)
NORMALIZED_TITLES = [
  ( "A save writes the skg files", [ "sav", "writ", "skg", "fil" ] ),
  ( "Saving: it's what the server does!", [ "sav", "server" ] ),
  ( "Fixing the stale-save check", [ "fix", "stal", "sav", "check" ] ),
  ( "Boxes, classes and the class", [ "box", "class", "class" ] ),
  ( "Really quickly used", [ "real", "quick", "used" ] ),
  ( "Is it a thing of theirs?", [ "thing" ] ),
  ( "", [] ) ]

# (NEW TITLE, REMOVED TITLE, whether the fuzzy match asks about them)
TITLE_MATCH_CASES = [
  # Reworded: the same statement
  ( "A save writes the skg files", "Saving writes skg files", True ),
  ( "Foreign repos are removed from the privacy ladder",
    "Foreign repos leave the privacy ladder", True ),
  ( "Fixing the stale save check", "Fix the stale-save check", True ),
  ( "Retyping a headline loses the node ID",
    "A retyped headline loses its ID", True ),
  ( "The handle is the viewnode ID", "Handles are viewnode IDs", True ),
  ( "Watching each repo directory for changes",
    "Watch every repo directory for changes", True ),
  ( "The background worker commits Tantivy",
    "Tantivy commits in the background", True ),
  ( "Ask Jeff about the port", "Ask Jeff about ports", True ),
  # Unrelated
  ( "The diff view shows every change",
    "Foreign repos leave the privacy ladder", False ),
  ( "Sell the car", "Buy milk", False ),
  ( "Tests must not share fixture directories", "Tests run in parallel", False ),
  ( "Tantivy updates after a save", "Tantivy commits in the background", False ),
  ( "Annual budget audit", "Quarterly budget review", False ),
  # Equal titles are the exact case, not the fuzzy one
  ( "Fred likes apples", "Fred likes apples", False ),
  # At the thresholds: Dice 0.6 (ratio 0.70) asks, Dice 0.57 (ratio
  # 0.69) does not; ratio 0.75 (Dice 0) asks, ratio 0.69 does not;
  # Dice 0.5 with ratio 0.61 does not
  ( "one two three six seven", "one two three four five", True ),
  ( "red green yellow purple", "red green blue", False ),
  ( "abcdefxy kmn pqr", "abcdefgh kmo pqs", True ),
  ( "abcdexyz kmn pqr", "abcdefgh kmo pqs", False ),
  ( "Second child", "First child", False ),
  # The ratio alone counts only when each title has three normalized
  # words: misspellings in three words ask (Dice 0.5 and 0.33, ratio
  # 0.93 and 0.9), in two do not (ratio 0.92), nor two words beside
  # three (ratio 0.87), nor the short titles that prompted the rule
  # (Dice 0.5, ratio 0.76), nor one word (ratio 0.75)
  ( "Recieve the quarterly budjet report",
    "Receive the quarterly budget report", True ),
  ( "Recieve the budjet report", "Receive the budget report", True ),
  ( "budjet report", "budget reports", False ),
  ( "Recieve the budjet report", "Receive the budgetreport", False ),
  ( "Budget meeting", "Budget notes", False ),
  ( "abcdefxy", "abcdefgh", False ) ]

def failures_of_title_matching ():
  failures = []
  for title, words in NORMALIZED_TITLES:
    if retyped_headlines . normalized_title_words (title) != words:
      failures . append (
        f"{title!r} normalizes to "
        f"{retyped_headlines . normalized_title_words (title)}, not {words}" )
  for new_title, removed_title, asks in TITLE_MATCH_CASES:
    if retyped_headlines . titles_probably_reworded (new_title, removed_title) != asks:
      failures . append (
        f"{new_title!r} / {removed_title!r}: the fuzzy match "
        + ( "does not ask" if asks else "asks" ) + ", with (Dice, ratio) "
        f"{retyped_headlines . title_similarity (new_title, removed_title)}" )
  dice, ratio = retyped_headlines . title_similarity (
    "one two three six seven", "one two three four five" )
  if dice != 0.6 or ratio >= retyped_headlines . REWORDED_RATIO_THRESHOLD:
    failures . append ( f"the Dice threshold case gives {( dice, ratio )}" )
  dice, ratio = retyped_headlines . title_similarity (
    "abcdefxy kmn pqr", "abcdefgh kmo pqs" )
  if dice != 0 or ratio != 0.75:
    failures . append ( f"the ratio threshold case gives {( dice, ratio )}" )
  return failures + failures_of_reworded_pairing ()

def failures_of_reworded_pairing ():
  """Pairs are one to one, best first: each new headline gets its best
  removed one still unpaired, and names the others it resembles. A
  headline marked '⟨new⟩' is never paired, and carries no other word."""
  original = [ edited_compact_view . OutlineHeadline ( handle, 1, title, [], None )
               for handle, title in [ ( "0a", "Fred likes apples and pears" ),
                                      ( "0b", "Fred likes apples" ),
                                      ( "0c", "Unrelated" ) ] ]
  edited = edited_compact_view . parse_edited_compact_view (
    "* Unrelated ⟨0c⟩\n"
    "* Fred likes apples and plums\n"
    "* Fred likes apples, pears\n"
    "* Fred likes pears and apples ⟨new⟩\n"
    "* Fred likes plums ⟨new delete⟩\n" )
  failures = []
  pairs = [ ( pair . new_index, pair . removed_handle,
              pair . other_removed_handles )
            for pair in retyped_headlines . reworded_headline_pairs (edited, original) ]
  if pairs != [ ( 1, "0b", [ "0a" ] ), ( 2, "0a", [ "0b" ] ) ]:
    failures . append ( f"the reworded pairs are {pairs}" )
  errors = edited_compact_view . handle_error_lines (
    edited [ 1 : ], { "0c" : original [2] } )
  if errors != [ "Error: ⟨new⟩ on line 5 of the compact view carries "
                 "'delete'; it takes no other word but one 'repo:NAME'." ]:
    failures . append ( f"the '⟨new⟩' errors are {errors}" )
  return failures

# ============================================================
# Heralds: every rendering case both clients' tests share, rendered
# as they render it, with the rule table and label table pinned in
# 'tests/shared/', and each case's context as they supply it.
# ============================================================

def failures_of_shared_herald_cases ():
  herald_tables = heralds . HeraldTables (
    sexp . parse_sexp ( read_file (SHARED_HERALD_RULES) ),
    { sexp . sexp_to_text ( entry [0] ) :
        sexp . sexp_to_text ( entry [1] )
      for entry in sexp . parse_sexp (
        read_file (SHARED_SKGREPO_LABELS) ) [ 1 : ] } )
  failures = []
  cases = shared_herald_cases ()
  for name, metadata_text, text, context in cases:
    rendered = heralds . herald_text (
      sexp . parse_sexp (metadata_text), herald_tables,
      heralds . HeraldContext (
        { "folderChange processed" : True,
          "folderChange unprocessed" : False } . get (context),
        context == "pendingDetachment" ))
    if rendered != text:
      failures . append (
        f"herald case '{name}': rendered '{rendered}', expected '{text}'" )
  if len (cases) <= 40:
    failures . append ( f"only {len (cases)} shared herald cases were read" )
  return failures

def shared_herald_cases ():
  """[(NAME, METADATA, TEXT, CONTEXT)] for each case, read as the
  clients' tests read it: a case ends at its styles line, and CONTEXT
  is the text of its '---- context: ' line, or None."""
  cases = []
  name, metadata, text, context = None, None, None, None
  with open (SHARED_HERALD_CASES, encoding = "utf-8") as cases_file:
    for line in cases_file . read () . split ("\n"):
      if line . startswith ("==== "):
        name, metadata, text, context = line [ 5 : ], None, None, None
      elif line . startswith ("---- context: "):
        context = line [ 14 : ]
      elif line . startswith ("---- text: "):
        text = line [ 11 : ]
      elif line . startswith ("---- styles: "):
        cases . append (( name, metadata, text, context ))
        name = None
      elif name and metadata is None:
        metadata = line
  return cases
