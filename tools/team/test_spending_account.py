#!/usr/bin/env python3
"""PURPOSE: test 'spending_account.py' on small transcripts made here,
never on the real ones.

Run from anywhere: 'python3 tools/team/test_spending_account.py'.
It writes a project folder of transcripts into a temporary folder: a
session the pacer started, which lent a mind to a specialist through a
subagent, which started a subagent of its own; and an interactive
session that began with a slash command. It checks the account's rows,
that a response streamed into two records is counted once, the
dollars at list price and weighted, the mission's kind, the requests
file and how it joins the account, and that running again neither
doubles a row, nor loses one whose transcript is gone, nor keeps a row
from going out of date. It also runs a copy of
'tools/team/specialist.sh prompt' in the temporary folder, to check
that the account reads back the kind the prompt prints.
"""
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

sys . path . insert ( 0, os . path . dirname ( os . path . abspath (__file__) ))
import spending_account

PACER_SESSION_ID = "11111111-1111-4111-8111-111111111111"
JEFF_SESSION_ID = "22222222-2222-4222-8222-222222222222"
SPECIALIST_AGENT_ID = "a1"
HELPER_AGENT_ID = "a2"
ODD_SESSION_ID = "33333333-3333-4333-8333-333333333333"
CUT_SESSION_ID = "44444444-4444-4444-8444-444444444444"
ODD_AGENT_ID = "a3"
MISSION_ID = "c37b2a71-3c37-48b0-83b0-9ec489fc653b"
SPECIALIST_PROMPT = (
  "You are being lent to a specialist named Mario, on the team that"
  " builds skg.\n\n3. Read your mission: view " + MISSION_ID +
  ". Then do it. It is a build mission." )
TEAM_FOLDER = os . path . dirname ( os . path . abspath (__file__) )

def user_record (timestamp, text, entrypoint = "sdk-cli"):
  return { "type" : "user", "timestamp" : timestamp,
           "entrypoint" : entrypoint,
           "message" : { "role" : "user", "content" : text } }

def assistant_record (timestamp, request_id, model, usage):
  return { "type" : "assistant", "timestamp" : timestamp,
           "requestId" : request_id,
           "message" : { "model" : model, "id" : "msg_" + request_id,
                         "usage" : usage } }

def usage (input_tokens, output_tokens, cache_read, cache_write_5m,
           cache_write_1h = 0):
  return { "input_tokens" : input_tokens, "output_tokens" : output_tokens,
           "cache_read_input_tokens" : cache_read,
           "cache_creation_input_tokens" : cache_write_5m + cache_write_1h,
           "cache_creation" : { "ephemeral_5m_input_tokens" : cache_write_5m,
                                "ephemeral_1h_input_tokens" : cache_write_1h } }

def write_transcript (path, records, broken_last_line = False):
  os . makedirs ( os . path . dirname (path), exist_ok = True )
  with open (path, "w", encoding = "utf-8") as transcript_file:
    for record in records:
      transcript_file . write ( json . dumps (record) + "\n" )
    if broken_last_line: # a transcript being written can end mid-line
      transcript_file . write ( '{"type": "assistant", "timest' )

def write_metadata (path, metadata):
  with open (path, "w", encoding = "utf-8") as metadata_file:
    json . dump (metadata, metadata_file)

class SpendingAccountTest (unittest . TestCase):

  def setUp (self):
    self . folder = tempfile . mkdtemp ()
    self . transcripts = os . path . join (self . folder, "projects")
    self . project = os . path . join (self . transcripts, "-home-someone")
    self . account = os . path . join (self . folder, "spending", "account.tsv")
    self . requests = os . path . join (self . folder, "spending", "requests.tsv")
    self . write_pacer_session ()
    self . write_jeff_session ()

  def tearDown (self):
    shutil . rmtree (self . folder)

  def write_pacer_session (self):
    write_transcript (
      os . path . join (self . project, PACER_SESSION_ID + ".jsonl"),
      [ { "type" : "session-owner", "sessionId" : PACER_SESSION_ID },
        user_record ( "2026-11-13T10:00:00.500Z",
                      spending_account . PACER_PROMPT_START + "." ),
        # one response, streamed into two records that repeat its usage
        assistant_record ( "2026-11-13T10:00:05.000Z", "r1",
                           "claude-fable-5-2", usage (10, 100, 1000, 0, 2000) ),
        assistant_record ( "2026-11-13T10:00:06.000Z", "r1",
                           "claude-fable-5-2", usage (10, 100, 1000, 0, 2000) ),
        assistant_record ( "2026-11-13T10:01:00.000Z", "r2",
                           "claude-fable-5-2", usage (5, 50, 3000, 0, 0) ) ],
      broken_last_line = True )
    subagents = os . path . join (self . project, PACER_SESSION_ID, "subagents")
    write_transcript (
      os . path . join (subagents, "agent-" + SPECIALIST_AGENT_ID + ".jsonl"),
      [ user_record ( "2026-11-13T10:00:10.000Z", SPECIALIST_PROMPT ),
        assistant_record ( "2026-11-13T10:00:20.000Z", "r3",
                           "claude-opus-5-5", usage (1, 10, 100, 1000) ) ] )
    write_metadata (
      os . path . join (subagents, "agent-" + SPECIALIST_AGENT_ID + ".meta.json"),
      { "agentType" : "general-purpose", "description" : "Mario builds it",
        "spawnDepth" : 1 } )
    write_transcript (
      os . path . join (subagents, "agent-" + HELPER_AGENT_ID + ".jsonl"),
      [ user_record ( "2026-11-13T10:00:30.000Z", "\n\nCount the files.\nThen stop." ),
        assistant_record ( "2026-11-13T10:30:00.000Z", "r4",
                           "a-model-with-no-price", usage (1, 1, 0, 0) ) ] )
    write_metadata (
      os . path . join (subagents, "agent-" + HELPER_AGENT_ID + ".meta.json"),
      { "agentType" : "Explore", "description" : "Count files",
        "spawnDepth" : 2, "parentAgentId" : SPECIALIST_AGENT_ID } )

  def write_jeff_session (self):
    write_transcript (
      os . path . join (self . project, JEFF_SESSION_ID + ".jsonl"),
      [ user_record ( "2026-11-13T09:00:00.000Z",
                      "<command-name>/clear</command-name>", "cli" ),
        user_record ( "2026-11-13T09:00:10.000Z",
                      "Welcome, Claude!\nHere is the plan.", "cli" ),
        assistant_record ( "2026-11-13T09:00:20.000Z", "r5",
                           "claude-sonnet-5-5", usage (2, 20, 0, 400) ),
        { "type" : "ai-title", "aiTitle" : "A generated title" } ] )

  def run_script (self):
    with open (os . devnull, "w") as quiet:
      standard_output = sys . stdout
      sys . stdout = quiet
      try:
        spending_account . main ( [ "--transcripts", self . transcripts,
                                    "--account", self . account ] )
      finally:
        sys . stdout = standard_output
    rows = spending_account . read_account (self . account)
    with open (self . account, encoding = "utf-8") as account_file:
      lines = account_file . read () . splitlines ()
    self . assertEqual ( lines [0] . split ("\t"),
                         spending_account . ACCOUNT_COLUMNS )
    self . assertEqual ( len (lines) - 1, len (rows) ) # no ID twice
    with open (self . requests, encoding = "utf-8") as requests_file:
      self . assertEqual ( requests_file . readline () . rstrip ("\n")
                           . split ("\t"),
                           spending_account . REQUEST_COLUMNS,
                           "the requests file is beside the account" )
    return rows

  def request_rows (self):
    """The requests file's rows, by (id, request), each once."""
    request_rows = spending_account . read_table (self . requests)
    keys = [ (row ["id"], row ["request"]) for row in request_rows ]
    self . assertEqual ( len (keys), len ( set (keys) ), "no request twice" )
    return { key : row for key, row in zip (keys, request_rows) }

  def test_rows (self):
    rows = self . run_script ()
    self . assertEqual ( set (rows), { PACER_SESSION_ID, JEFF_SESSION_ID,
                                       SPECIALIST_AGENT_ID, HELPER_AGENT_ID } )
    pacer = rows [PACER_SESSION_ID]
    self . assertEqual ( (pacer ["kind"], pacer ["origin"], pacer ["session"],
                          pacer ["parent"]),
                         ("session", "pacer", PACER_SESSION_ID, "") )
    self . assertEqual ( (pacer ["requests"], pacer ["input"], pacer ["output"],
                          pacer ["cache_write"], pacer ["cache_read"]),
                         ("2", "15", "150", "2000", "4000") )
    self . assertEqual ( pacer ["began"], "2026-11-13T10:00:00Z" )
    self . assertEqual ( pacer ["ended"], "2026-11-13T10:30:00Z",
                         "a session lasts until its last subagent ends" )
    self . assertEqual ( pacer ["minutes"], "30.0" )
    self . assertEqual ( pacer ["model"], "claude-fable-5-2" )
    self . assertEqual ( pacer ["usd"], "%.4f" % (
      ( 15 * 8 + 150 * 40 + 4000 * 0.2 + 2000 * 16 ) / 1e6 ))
    self . assertEqual ( pacer ["weighted_usd"], "%.4f" % (
      2.5 * ( 15 * 8 + 150 * 40 + 4000 * 0.2 + 2000 * 16 ) / 1e6 ),
      "a Fable dollar weighs 2.5" )
    self . assertEqual ( pacer ["mission_kind"], "" )
    specialist = rows [SPECIALIST_AGENT_ID]
    self . assertEqual ( (specialist ["kind"], specialist ["origin"],
                          specialist ["session"], specialist ["parent"],
                          specialist ["specialist"], specialist ["mission"],
                          specialist ["title"]),
                         ("subagent", "subagent", PACER_SESSION_ID,
                          PACER_SESSION_ID, "mario", MISSION_ID,
                          "Mario builds it") )
    self . assertEqual ( specialist ["usd"], "%.4f" % (
      ( 1 * 4 + 10 * 20 + 100 * 0.2 + 1000 * 5 ) / 1e6 ))
    self . assertEqual ( specialist ["weighted_usd"], specialist ["usd"],
                         "an Opus dollar weighs 1" )
    self . assertEqual ( specialist ["mission_kind"], "build" )
    helper = rows [HELPER_AGENT_ID]
    self . assertEqual ( (helper ["parent"], helper ["session"],
                          helper ["specialist"], helper ["mission"],
                          helper ["mission_kind"], helper ["asked"],
                          helper ["usd"], helper ["weighted_usd"]),
                         (SPECIALIST_AGENT_ID, PACER_SESSION_ID, "mario",
                          MISSION_ID, "build", "Count the files.", "", "") )
    jeff = rows [JEFF_SESSION_ID]
    self . assertEqual ( (jeff ["origin"], jeff ["asked"], jeff ["title"],
                          jeff ["specialist"], jeff ["mission_kind"]),
                         ("jeff", "Welcome, Claude!", "A generated title",
                          "", "") )

  def test_requests (self):
    rows = self . run_script ()
    request_rows = self . request_rows ()
    self . assertEqual ( set (request_rows), {
      (PACER_SESSION_ID, "r1"), (PACER_SESSION_ID, "r2"),
      (SPECIALIST_AGENT_ID, "r3"), (HELPER_AGENT_ID, "r4"),
      (JEFF_SESSION_ID, "r5") } )
    streamed = request_rows [ (PACER_SESSION_ID, "r1") ]
    self . assertEqual (
      ( streamed ["time"], streamed ["model"], streamed ["input"],
        streamed ["output"], streamed ["cache_write"],
        streamed ["cache_read"], streamed ["usd"], streamed ["weighted_usd"] ),
      ( "2026-11-13T10:00:05Z", "claude-fable-5-2", "10", "100", "2000",
        "1000", "%.6f" % ( ( 10 * 8 + 100 * 40 + 1000 * 0.2 + 2000 * 16 ) / 1e6 ),
        "%.6f" % ( 2.5 * ( 10 * 8 + 100 * 40 + 1000 * 0.2 + 2000 * 16 ) / 1e6 )),
      "a streamed request is one row, at its first record's time" )
    unpriced = request_rows [ (HELPER_AGENT_ID, "r4") ]
    self . assertEqual ( (unpriced ["usd"], unpriced ["weighted_usd"]), ("", "") )
    for account_id, row in rows . items (): # the join on 'id'
      its_requests = [ request_row for key, request_row in request_rows . items ()
                       if key [0] == account_id ]
      self . assertEqual ( str ( len (its_requests) ), row ["requests"] )
      if row ["weighted_usd"]:
        self . assertAlmostEqual (
          sum ( float ( request_row ["weighted_usd"] )
                for request_row in its_requests ),
          float ( row ["weighted_usd"] ), places = 4 )

  def test_changed_weight_reaches_rows_of_deleted_transcripts (self):
    self . run_script ()
    os . remove ( os . path . join (self . project, JEFF_SESSION_ID + ".jsonl") )
    weights = spending_account . SUBSCRIPTION_WEIGHT_PER_LIST_DOLLAR
    original_weight = weights ["claude-sonnet-5-5"]
    weights ["claude-sonnet-5-5"] = 3
    try:
      rows = self . run_script ()
    finally:
      weights ["claude-sonnet-5-5"] = original_weight
    jeff = rows [JEFF_SESSION_ID]
    self . assertEqual ( jeff ["weighted_usd"],
                         "%.4f" % ( 3 * float ( jeff ["usd"] )) )
    self . assertEqual ( self . request_rows () [ (JEFF_SESSION_ID, "r5") ]
                         ["weighted_usd"],
                         "%.6f" % ( 3 * ( 2 * 2 + 20 * 10 + 400 * 2.5 ) / 1e6 ),
                         "a deleted transcript's requests are kept" )

  def test_row_older_than_the_requests_file_keeps_its_weight (self):
    os . makedirs ( os . path . dirname (self . account) )
    with open (self . account, "w", encoding = "utf-8") as account_file:
      account_file . write ( "id\tbegan\trequests\tusd\tweighted_usd\n"
                             "old\t2026-10-01T00:00:00Z\t2\t1.0000\t9.0000\n" )
    old = self . run_script () ["old"]
    self . assertEqual ( (old ["usd"], old ["weighted_usd"], old ["mission_kind"]),
                         ("1.0000", "9.0000", "") )

  def test_the_account_reads_the_kind_specialist_prompt_prints (self):
    root = os . path . join (self . folder, "root")
    os . makedirs ( os . path . join (root, "tools", "team") )
    os . makedirs ( os . path . join (root, "tools", "team-processing") )
    script = os . path . join (root, "tools", "team", "specialist.sh")
    shutil . copy ( os . path . join (TEAM_FOLDER, "specialist.sh"), script )
    with open ( os . path . join (root, "tools", "team-processing", "ports"),
                "w", encoding = "utf-8" ) as ports_file:
      ports_file . write ( "mario 1774 " + MISSION_ID . replace ("c", "d") + "\n" )
    for kind in spending_account . MISSION_KINDS:
      prompt = subprocess . run ( [ "bash", script, "prompt", "mario",
                                    MISSION_ID, kind ],
                                  capture_output = True, text = True,
                                  check = True ) . stdout
      self . assertEqual (
        spending_account . MISSION_KIND_IN_PROMPT . search (prompt) . group (1),
        kind )
      self . assertEqual (
        spending_account . MISSION_IN_PROMPT . search (prompt) . group (1),
        MISSION_ID )
    refused = subprocess . run ( [ "bash", script, "prompt", "mario",
                                   MISSION_ID, "fixing" ],
                                 capture_output = True, text = True )
    self . assertEqual ( (refused . returncode, refused . stdout), (2, "") )
    self . assertIn ( "not a kind of mission", refused . stderr )
    without_kind = subprocess . run ( [ "bash", script, "prompt", "mario",
                                        MISSION_ID ],
                                      capture_output = True, text = True )
    self . assertEqual ( (without_kind . returncode, without_kind . stdout),
                         (2, "") )

  def test_running_again_updates_without_doubling_or_losing (self):
    self . run_script ()
    self . run_script ()
    os . remove ( os . path . join (self . project, JEFF_SESSION_ID + ".jsonl") )
    with open ( os . path . join (self . project, PACER_SESSION_ID + ".jsonl"),
                "a", encoding = "utf-8" ) as transcript_file:
      transcript_file . write ( "\n" + json . dumps ( assistant_record (
        "2026-11-13T11:00:00.000Z", "r6", "claude-fable-5-2",
        usage (1, 1, 0, 0) )) + "\n" )
    rows = self . run_script ()
    self . assertEqual ( len (rows), 4 )
    self . assertIn ( JEFF_SESSION_ID, rows,
                      "a row outlives its deleted transcript" )
    self . assertEqual ( rows [PACER_SESSION_ID] ["requests"], "3" )
    self . assertEqual ( len ( self . request_rows () ), 6 )
    self . assertEqual ( rows [PACER_SESSION_ID] ["ended"],
                         "2026-11-13T11:00:00Z" )

  def test_odd_records_are_skipped (self):
    write_transcript (
      os . path . join (self . project, ODD_SESSION_ID + ".jsonl"),
      [ None, [],
        { "type" : "user", "timestamp" : "2026-11-13T08:00:00.000Z",
          "message" : None },
        { "type" : "assistant", "timestamp" : "2026-11-13T08:00:01.000Z",
          "requestId" : "r7", "message" : None },
        user_record ( "2026-11-13T08:00:02.000Z",
                      [ "a bare string",
                        { "type" : "text", "text" : "Real request" } ] ),
        assistant_record ( "2026-11-13T08:00:03.000Z", "r8",
                           "claude-sonnet-5-5", usage (1, 2, 0, 0) ) ] )
    odd = self . run_script () [ODD_SESSION_ID]
    self . assertEqual ( (odd ["asked"], odd ["requests"], odd ["began"]),
                         ("Real request", "1", "2026-11-13T08:00:00Z") )

  def test_zero_byte_account_is_empty (self):
    os . makedirs ( os . path . dirname (self . account) )
    open (self . account, "w") . close ()
    self . assertEqual ( spending_account . read_account (self . account), {} )
    self . assertEqual ( len ( self . run_script () ), 4 )

  def test_unreadable_pieces_are_skipped (self):
    cut_path = os . path . join (self . project, CUT_SESSION_ID + ".jsonl")
    write_transcript (
      cut_path,
      [ user_record ( "yesterday", "Ask me" ),
        assistant_record ( "2026-11-13T07:00:00.000Z", "r9",
                           "claude-sonnet-5-5", usage (1, 2, 0, 0) ) ] )
    with open (cut_path, "ab") as transcript_file:
      # a line cut in the middle of the three bytes of a euro sign
      transcript_file . write ( '{"type": "user", "x": "\u20ac'
                                . encode ("utf-8") [:-1]
                                + "€" . encode ("utf-8") [:2] )
    subagents = os . path . join (self . project, PACER_SESSION_ID, "subagents")
    write_transcript (
      os . path . join (subagents, "agent-" + ODD_AGENT_ID + ".jsonl"),
      [ user_record ( "2026-11-13T10:00:40.000Z", "Do it." ) ] )
    with open ( os . path . join (subagents, "agent-" + ODD_AGENT_ID
                                  + ".meta.json"),
                "w", encoding = "utf-8" ) as metadata_file:
      metadata_file . write ( '{"description": "cut off' )
    rows = self . run_script ()
    cut = rows [CUT_SESSION_ID]
    self . assertEqual ( (cut ["asked"], cut ["requests"], cut ["began"],
                          cut ["ended"]),
                         ("Ask me", "1", "2026-11-13T07:00:00Z",
                          "2026-11-13T07:00:00Z") )
    odd_agent = rows [ODD_AGENT_ID]
    self . assertEqual ( (odd_agent ["parent"], odd_agent ["title"]),
                         (PACER_SESSION_ID, "") )

if __name__ == "__main__":
  unittest . main ()
