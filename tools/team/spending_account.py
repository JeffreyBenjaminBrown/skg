#!/usr/bin/env python3
"""PURPOSE: keep the account of what each Claude Code session and each
subagent cost, read from Claude Code's own transcripts.

  python3 tools/team/spending_account.py [--transcripts DIR] [--account FILE]

It reads every transcript under DIR (default: the 'projects/' folder of
$CLAUDE_CONFIG_DIR, else of ~/.claude): a session is
'PROJECT/SESSION_ID.jsonl', and each subagent it started, at any depth,
is 'PROJECT/SESSION_ID/subagents/agent-AGENT_ID.jsonl' beside an
'agent-AGENT_ID.meta.json'. It writes FILE (default
'data/claude/spending/account.tsv'), one row per session and one per
subagent, tab-separated, with a header line.

Running it again brings FILE up to date: each transcript's row is
replaced by a fresh one, and a row whose transcript is gone is kept.
Claude Code deletes transcripts older than 'cleanupPeriodDays' (30 by
default), so after a month this file is the only record of them.
A session still running is counted as far as it has gone.

THE COLUMNS
  id           the session's ID, or the subagent's agent ID
  kind         'session' or 'subagent'
  session      the session the row belongs to (its own ID, for a session)
  parent       for a subagent, the session or subagent that started it
  origin       'pacer' (began with tools/team/pacer.sh's prompt),
               'jeff' (an interactive session: someone sat at it),
               'other' (non-interactive, not the pacer), or 'subagent'
  specialist   whom the row worked for, read from a first request
               that 'tools/team/specialist.sh prompt' printed; a
               subagent a specialist started inherits it, so summing
               by specialist counts the helpers too
  mission      the mission's node ID, read and inherited the same way
  began, ended the first and last timestamps in the transcript (UTC);
               a session ends no earlier than its last subagent, since
               its own transcript is silent while it waits for one
  minutes      ended minus began: wall time, idle time included
  model        the model that answered most of its requests
  requests     API requests (one response, streamed into several
               transcript records, is counted once)
  input, output, cache_write, cache_read
               tokens, summed over its requests; a subagent's tokens
               are in its own row only, not in its parent's
  usd          what the requests would cost at API list prices, in
               dollars; see below
  title        a subagent's description, or a session's title
               (Jeff's, else the one Claude Code generated)
  asked        the first line of its first request

THE ONE COMPARABLE NUMBER, 'usd'. Tokens of different kinds cost
different amounts, and differently per model, so the column that can
be summed and compared is a price. Each request's tokens are priced
with PRICES_PER_MILLION_TOKENS, which are the prices Claude Code itself
applies (its 'cost-state' transcript records give a run's cost; these
prices reproduce it to the cent, except an hour's cache write for
Sonnet and Haiku, which is assumed, as none was seen). A cache write
costs more when it is kept an hour than five minutes, and the
transcript says which. It is a list price: Jeff pays a subscription,
whose limits are not published per token, so 'usd' measures how much
of the subscription a row used only as far as those limits follow list
prices. A model missing from the table leaves 'usd' empty; add it
there. Two costs no transcript records are missing from every row:
Claude Code's own side requests (a session's title; the model behind
WebSearch and WebFetch, which can reach 10 cents a session), and
anything before the transcript's start.

QUERYING IT. A shell reads it with awk ('-F\\t'), or with SQLite, which
takes the header for column names. What each session cost, its
subagents included, and what each specialist's work cost:
  sqlite3 :memory: -cmd '.mode tabs' -cmd '.import FILE account' \\
    'select session, round(sum(usd),2) from account group by session'
  ... 'select specialist, round(sum(usd),2) from account
       where length(specialist) group by specialist'
"""
import argparse
import datetime
import glob
import json
import os
import re
import sys

PRICES_PER_MILLION_TOKENS = {
  # model: (input, output, cache read, cache write kept 5 minutes,
  #         cache write kept 1 hour), in dollars
  "claude-fable-5-2"          : (8,   40,  0.2,  10,    16),
  "claude-opus-5-5"           : (4,   20,  0.2,  5,     8),
  "claude-sonnet-5-5"         : (2,   10,  0.1,  2.5,   4),
  "claude-haiku-5-5"          : (0.1, 0.5, 0.01, 0.125, 0.2),
  "claude-haiku-4-5-20251001" : (1,   5,   0.1,  1.25,  2),
}
DOLLARS_PER_WEB_SEARCH = 0.01

ACCOUNT_COLUMNS = [
  "id", "kind", "session", "parent", "origin", "specialist", "mission",
  "began", "ended", "minutes", "model", "requests",
  "input", "output", "cache_write", "cache_read", "usd",
  "title", "asked" ]

PACER_PROMPT_START = ( # the start of WAKE_PROMPT in tools/team/pacer.sh
  "You are Claude, waking with no memory of earlier sessions. Nobody is"
  " here: tools/team/pacer.sh started this session" )
NODE_ID = r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
SPECIALIST_IN_PROMPT = re . compile (
  r"You are being lent to a specialist named (\w+)" )
MISSION_IN_PROMPT = re . compile (
  r"Read your mission: `?view (" + NODE_ID + ")" )
ASKED_MAX_CHARACTERS = 200

def main (argv):
  arguments = parse_command_line (argv)
  rows_by_id = read_account (arguments . account)
  rows_found = rows_of_every_transcript (arguments . transcripts)
  rows_by_id . update ( { row ["id"] : row for row in rows_found } )
  write_account (arguments . account, rows_by_id . values () )
  print ( f"{len (rows_found)} transcripts read; "
          f"{len (rows_by_id)} rows in {arguments . account}" )
  return 0

def parse_command_line (argv):
  repository_root = os . path . dirname ( os . path . dirname (
    os . path . dirname ( os . path . abspath (__file__) )) )
  claude_config_folder = os . environ . get (
    "CLAUDE_CONFIG_DIR", os . path . expanduser ("~/.claude") )
  parser = argparse . ArgumentParser (
    description = __doc__,
    formatter_class = argparse . RawDescriptionHelpFormatter )
  parser . add_argument (
    "--transcripts",
    default = os . path . join (claude_config_folder, "projects"),
    help = "the folder of project folders (default: %(default)s)" )
  parser . add_argument (
    "--account",
    default = os . path . join (repository_root,
                                "data/claude/spending/account.tsv"),
    help = "the table to bring up to date (default: %(default)s)" )
  return parser . parse_args (argv)

def read_account (account_path):
  """The rows already in the account, by ID."""
  if not os . path . exists (account_path):
    return {}
  with open (account_path, encoding = "utf-8") as account_file:
    lines = account_file . read () . splitlines ()
  if not lines: # a zero-byte file, e.g. from an interrupted first run
    return {}
  header = lines [0] . split ("\t")
  rows = [ dict ( zip (header, line . split ("\t") ))
           for line in lines [1:] if line ]
  return { row ["id"] : row for row in rows }

def write_account (account_path, rows):
  """Write ROWS, oldest first, replacing the file only when complete."""
  os . makedirs ( os . path . dirname ( os . path . abspath (account_path) ),
                  exist_ok = True )
  temporary_path = account_path + ".tmp"
  with open (temporary_path, "w", encoding = "utf-8") as account_file:
    account_file . write ( "\t" . join (ACCOUNT_COLUMNS) + "\n" )
    for row in sorted ( rows, key = lambda row: (row ["began"], row ["id"]) ):
      account_file . write ( "\t" . join (
        cell_text ( row . get (column, "") ) for column in ACCOUNT_COLUMNS )
                             + "\n" )
  os . replace (temporary_path, account_path)

def cell_text (value):
  """VALUE as one TSV cell: no tabs, no line breaks."""
  return re . sub ( r"[\t\r\n]+", " ", str (value) )

def rows_of_every_transcript (transcripts_folder):
  """A row for each session and each subagent under TRANSCRIPTS_FOLDER,
  subagents after the sessions and subagents that started them."""
  rows = []
  for session_path in sorted ( glob . glob (
      os . path . join (transcripts_folder, "*", "*.jsonl") )):
    session_row = row_of_session (session_path)
    if session_row is None:
      continue
    subagent_rows = rows_of_subagents_of_session (session_row, session_path)
    extend_session_to_its_last_subagent (session_row, subagent_rows)
    rows . append (session_row)
    rows . extend (subagent_rows)
  return rows

def extend_session_to_its_last_subagent (session_row, subagent_rows):
  """A session runs until its last subagent ends, though its own
  transcript is silent while it waits for one."""
  session_row ["ended"] = max ( [ session_row ["ended"] ] +
                                [ row ["ended"] for row in subagent_rows ] )
  session_row ["minutes"] = minutes_between (session_row ["began"],
                                             session_row ["ended"])

def row_of_session (session_path):
  """The row of the session transcript SESSION_PATH, or None if it
  holds no conversation (a session that only ran a local command)."""
  records = read_transcript_records (session_path)
  row = row_of_records (records)
  if row is None:
    return None
  session_id = os . path . basename (session_path) [: - len (".jsonl")]
  row . update ( { "id" : session_id, "kind" : "session",
                   "session" : session_id, "parent" : "" } )
  row ["origin"] = origin_of_session (records, row ["asked_in_full"])
  row ["title"] = title_of_session (records)
  return row

def rows_of_subagents_of_session (session_row, session_path):
  """Rows for every subagent of the session. Each inherits its parent's
  specialist and mission unless its own first request names them; a
  parent begins before its subagents, so taking them in the order they
  began meets each parent first."""
  subagent_folder = os . path . join ( session_path [: - len (".jsonl")],
                                       "subagents" )
  rows = []
  for transcript_path in glob . glob (
      os . path . join (subagent_folder, "agent-*.jsonl") ):
    row = row_of_subagent (transcript_path, session_row ["id"])
    if row is not None:
      rows . append (row)
  rows . sort ( key = lambda row: (row ["began"], row ["id"]) )
  rows_by_id = { session_row ["id"] : session_row }
  for row in rows:
    parent_row = rows_by_id . get (row ["parent"])
    if parent_row is not None and not row ["specialist"]:
      row ["specialist"] = parent_row ["specialist"]
      row ["mission"] = parent_row ["mission"]
    rows_by_id [row ["id"]] = row
  return rows

def row_of_subagent (transcript_path, session_id):
  records = read_transcript_records (transcript_path)
  row = row_of_records (records)
  if row is None:
    return None
  metadata_path = transcript_path [: - len (".jsonl")] + ".meta.json"
  metadata = {}
  if os . path . exists (metadata_path):
    with open (metadata_path, "rb") as metadata_file:
      try:
        metadata = json . loads ( metadata_file . read () . decode ("utf-8") )
      except (json . JSONDecodeError, UnicodeDecodeError):
        pass # no metadata is better than no row
    if not isinstance (metadata, dict): # e.g. 'null' or '[]'
      metadata = {}
  agent_id = os . path . basename (transcript_path) [
    len ("agent-") : - len (".jsonl") ]
  row . update ( {
    "id" : agent_id, "kind" : "subagent", "session" : session_id,
    "parent" : metadata . get ("parentAgentId") or session_id,
    "origin" : "subagent",
    "title" : metadata . get ("description", "") } )
  return row

def read_transcript_records (transcript_path):
  """The records of a transcript, skipping a line that is not a JSON
  object (the last line of a transcript being written can be incomplete,
  even cut in the middle of a multibyte character)."""
  records = []
  with open (transcript_path, "rb") as transcript_file:
    for line in transcript_file:
      try:
        record = json . loads ( line . decode ("utf-8") )
      except (json . JSONDecodeError, UnicodeDecodeError):
        continue
      if isinstance (record, dict): # not 'null' or '[]'
        records . append (record)
  return records

def row_of_records (records):
  """The columns that sessions and subagents compute alike, or None if
  RECORDS hold no timestamp. 'asked_in_full' is the whole first
  request, for the caller; it is not a column."""
  timestamps = [ record ["timestamp"] for record in records
                 if is_iso_timestamp ( record . get ("timestamp") ) ]
  if not timestamps:
    return None
  began, ended = min (timestamps), max (timestamps)
  asked_in_full = first_request_text (records)
  specialist_match = SPECIALIST_IN_PROMPT . search (asked_in_full)
  mission_match = MISSION_IN_PROMPT . search (asked_in_full)
  row = {
    "specialist" : specialist_match . group (1) . lower ()
                   if specialist_match else "",
    "mission" : mission_match . group (1) if mission_match else "",
    "began" : seconds_precision (began),
    "ended" : seconds_precision (ended),
    "minutes" : minutes_between (began, ended),
    "asked" : first_line (asked_in_full),
    "asked_in_full" : asked_in_full }
  row . update ( usage_columns ( usage_of_each_request (records) ))
  return row

def first_request_text (records):
  """The text of the first thing the transcript was asked: the first
  user message that is not a tool's result, a note Claude Code added,
  or a slash command (an interactive session can begin with '/clear')."""
  for record in records:
    if record . get ("type") != "user" or record . get ("isMeta"):
      continue
    content = message_of_record (record) . get ("content")
    if isinstance (content, list):
      content = "\n" . join ( block ["text"] for block in content
                              if isinstance (block, dict)
                              and block . get ("type") == "text"
                              and isinstance ( block . get ("text"), str ) )
    if not isinstance (content, str) or not content or content . lstrip () . startswith (
        ("<command-", "<local-command-") ):
      continue
    return content
  return ""

def first_line (text):
  """The first line of TEXT that says something: not blank, and not
  only a markup tag such as '<pasted_content id=...>'."""
  for line in text . splitlines ():
    line = line . strip ()
    if line and not re . fullmatch ( r"<[^>]*>", line ):
      return line [:ASKED_MAX_CHARACTERS]
  return ""

def usage_of_each_request (records):
  """(model, usage) per API request. A response is streamed into one
  transcript record per content block, each repeating the response's
  usage, so records are deduplicated by their request ID."""
  usage_by_request = {}
  for record in records:
    if record . get ("type") != "assistant":
      continue
    message = message_of_record (record)
    request_key = record . get ("requestId") or message . get ("id")
    usage = message . get ("usage")
    if request_key is None or not isinstance (usage, dict):
      continue
    usage_by_request [request_key] = ( message . get ("model"), usage )
  return list ( usage_by_request . values () )

def message_of_record (record):
  """RECORD's 'message', or {} if it has none."""
  message = record . get ("message")
  return message if isinstance (message, dict) else {}

def usage_columns (model_and_usage_of_each_request):
  totals = { "input" : 0, "output" : 0, "cache_write" : 0, "cache_read" : 0 }
  requests_by_model = {}
  dollars = 0.0
  every_model_priced = True
  for model, usage in model_and_usage_of_each_request:
    totals ["input"] += usage . get ("input_tokens") or 0
    totals ["output"] += usage . get ("output_tokens") or 0
    totals ["cache_write"] += usage . get ("cache_creation_input_tokens") or 0
    totals ["cache_read"] += usage . get ("cache_read_input_tokens") or 0
    requests_by_model [model] = requests_by_model . get (model, 0) + 1
    request_dollars = dollars_of_request (model, usage)
    if request_dollars is None:
      every_model_priced = False
    else:
      dollars += request_dollars
  columns = { key : str (value) for key, value in totals . items () }
  columns ["requests"] = str ( len (model_and_usage_of_each_request) )
  columns ["model"] = max ( requests_by_model,
                            key = requests_by_model . get,
                            default = "" ) or ""
  columns ["usd"] = "%.4f" % dollars if every_model_priced else ""
  return columns

def dollars_of_request (model, usage):
  """What one request costs at list prices, or None if MODEL is not
  in the price table. A request that used no tokens (Claude Code's
  '<synthetic>' messages) costs nothing, whatever its model."""
  token_counts = [ usage . get (key) or 0 for key in (
    "input_tokens", "output_tokens",
    "cache_read_input_tokens", "cache_creation_input_tokens" ) ]
  if not any (token_counts):
    return 0.0
  prices = PRICES_PER_MILLION_TOKENS . get (model)
  if prices is None:
    return None
  input_price, output_price, read_price, write_5m_price, write_1h_price = (
    prices )
  input_tokens, output_tokens, read_tokens, write_tokens = token_counts
  write_1h_tokens = ( usage . get ("cache_creation") or {} ) . get (
    "ephemeral_1h_input_tokens") or 0
  write_5m_tokens = write_tokens - write_1h_tokens # without a breakdown, the API's default, 5 minutes
  web_searches = ( usage . get ("server_tool_use") or {} ) . get (
    "web_search_requests") or 0
  return ( ( input_tokens * input_price
             + output_tokens * output_price
             + read_tokens * read_price
             + write_5m_tokens * write_5m_price
             + write_1h_tokens * write_1h_price ) / 1e6
           + web_searches * DOLLARS_PER_WEB_SEARCH )

def origin_of_session (records, asked_in_full):
  if asked_in_full . startswith (PACER_PROMPT_START):
    return "pacer"
  entrypoints = { record . get ("entrypoint") for record in records
                  if record . get ("type") == "user" }
  if "cli" in entrypoints: # the interactive terminal; 'claude -p' is 'sdk-cli'
    return "jeff"
  return "other"

def title_of_session (records):
  """Jeff's title for the session if he gave one, else the last one
  Claude Code generated."""
  for record_type, title_key in ( ("custom-title", "customTitle"),
                                  ("ai-title", "aiTitle") ):
    titles = [ record . get (title_key) for record in records
               if record . get ("type") == record_type
               and record . get (title_key) ]
    if titles:
      return titles [-1]
  return ""

def minutes_between (began, ended):
  return "%.1f" % ( ( parse_timestamp (ended) - parse_timestamp (began) )
                    . total_seconds () / 60 )

def seconds_precision (timestamp):
  """'2026-11-13T15:23:04.684Z' as '2026-11-13T15:23:04Z'."""
  return parse_timestamp (timestamp) . strftime ("%Y-%m-%dT%H:%M:%SZ")

def parse_timestamp (timestamp):
  return datetime . datetime . fromisoformat (
    timestamp . replace ("Z", "+00:00") )

def is_iso_timestamp (value):
  """Whether VALUE is a string that parse_timestamp can read."""
  if not isinstance (value, str):
    return False
  try:
    return parse_timestamp (value) . tzinfo is not None
  except ValueError:
    return False

if __name__ == "__main__":
  sys . exit ( main ( sys . argv [1:] ))
