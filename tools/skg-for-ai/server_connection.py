"""The connection to a skg server: 'SkgConnection' and
'connect_to_server'; the tool's errors ('SkgToolError' and its
subclasses); and the helpers that build requests and read responses
('request_line', 'response_type', 'response_field', ...).
'RUNNING_DAEMON' is the daemon this process runs, if any: the daemon
sets it here, as 'server_connection . RUNNING_DAEMON', and
'connect_to_server' hands its connection to the commands it runs.
Also the overPrivateText confirmation a request may meet under a
skgrepo restriction ('overPrivateText_answer',
'request_releasing_overPrivateText')."""

import socket
import sys

from sexp import (
  metadata_form, parse_sexp, quote_string, render_sexp, sexp_to_text )

HOST = "127.0.0.1"
# There is no default port: the one there was belonged to a live
# user's server, and a command whose shell had lost its SKG_PORT
# wrote to it.
NO_PORT_MESSAGE = (
  "No server is named: name it with '--port PORT' or SKG_PORT; "
  "there is no default port. Nothing was sent." )
FIRST_MESSAGE_TIMEOUT_SECONDS = 120
IMPORT_TIMEOUT_SECONDS = 600
# How long 'raw' waits after a response for another before it stops.
RAW_QUIET_SECONDS = 3
# A view's perspective ('docs/api-and-formats.org', "Set
# perspective"), by the server's name: "root homes", as every fresh
# view follows, or "skgrepo NAME".
DEFAULT_PERSPECTIVE = "root homes"
SAVE_TERMINAL_RESPONSE_TYPES = {
  "save-result", "save-preview", "delete-from-confirmation", "strandeds-confirmation",
  "telescope-hoist-confirmation", "fork-confirmation",
  "overPrivateText-telescope-confirmation", "error" }

# ============================================================
# The connection
# ============================================================

UNPROMPTED_REPO_CHANGE_MESSAGE_START = "((response-type repo-files-reloaded)"

def repo_files_reloaded_count_clauses (message):
  """'reloaded N files from repo NAME' for each '(NAME N)' of a
  repo-files-reloaded MESSAGE's '(reloaded ...)'."""
  clauses = []
  for skgrepo, count in response_field (message, "reloaded") or []:
    clauses . append ( f"reloaded {count} "
                       f"{'file' if count == 1 else 'files'} "
                       f"from repo {sexp_to_text (skgrepo)}" )
  return clauses

class SkgToolError (Exception):
  pass

class SkgServerQuietError (SkgToolError):
  pass

class SkgViewAlreadyOpen (SkgToolError):
  """The server answered a view of NODE_ID by naming VIEW_ID, a view
  the connection holds whose root it already is, as it lands a
  client's link in that view's buffer. A view switched to another's
  perspective has the mirror it shows as its root
  ('docs/api-and-formats.org', "Set perspective")."""
  def __init__ (self, node_id, view_id):
    super () . __init__ (
      f"{node_id} is already a root of the view {view_id}, which this "
      f"connection holds, so the server sent no view of it. If that "
      f"view's perspective put it there, switch the view back with "
      f"'perspective SIDECAR root homes' to view {node_id} "
      f"by itself." )
    self . node_id = node_id
    self . view_id = view_id

class SkgConnectionClosedError (SkgToolError):
  pass

# The daemon this process runs, while it runs. The commands it runs
# talk to its server on its connection ('connect_to_server').
RUNNING_DAEMON = None

def connect_to_server (port):
  """The connection a command talks to the server on: the daemon's,
  when this process is a daemon running the command and PORT is its
  server's; otherwise a new one, which the command closes."""
  if RUNNING_DAEMON is not None and RUNNING_DAEMON . port == port:
    return RUNNING_DAEMON . connection
  return SkgConnection (port)

class SkgConnection:
  """One TCP connection to the server. The server keeps a
  connection's views until it ends. This one is a command's own: it
  holds none of the views the command writes."""

  def __init__ (self, port):
    if port is None:
      raise SkgToolError (NO_PORT_MESSAGE)
    self . port = port
    try:
      self . socket = socket . create_connection (( HOST, port ))
    except OSError as error:
      raise SkgToolError (
        f"No skg server answers on port {port}: {error}" )
    self . buffered = b""
    self . herald_tables = None # 'fetch_herald_tables' fills it

  def close (self):
    self . socket . close ()

  def holds_views_it_writes (self):
    return False

  def holds_view (self, view_id):
    return False

  def holds_view_for_other_files (self, view_id, directory, base_name):
    return False

  def files_stem_of_view (self, view_id):
    return None

  def note_view_written (self, view_id, out_dir, base_name):
    pass

  def forget_view (self, view_id):
    pass

  def log_event (self, text):
    pass

  def log_view_event (self, verb, directory, base_name, suffix = ""):
    pass

  def handle_unprompted_message (self, payload):
    """A message the server sent unprompted, that it reloaded files
    changed on disk, is no answer: its counts and warnings go to
    stderr."""
    message = parse_sexp (payload)
    for line in ( repo_files_reloaded_count_clauses (message) +
                  [ sexp_to_text (warning)
                    for warning in response_field (message, "warnings") or [] ] ):
      print ( "skg server: " + line, file = sys . stderr )

  def observe_message (self, payload):
    """Every message read as part of a response passes here."""
    pass

  def request (self, line):
    """Send one request line; return its one response, parsed."""
    return parse_sexp (
      self . request_text (line, FIRST_MESSAGE_TIMEOUT_SECONDS) )

  def request_text (self, line, timeout_seconds):
    """Send one request line; return its one response's text."""
    self . send_line (line)
    return self . read_message (timeout_seconds)

  def raw_request (self, line):
    """Send one request line; return the text of every response that
    arrives before the server is quiet for RAW_QUIET_SECONDS."""
    self . send_line (line)
    messages = [ self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) ]
    while True:
      try:
        messages . append ( self . read_message (RAW_QUIET_SECONDS) )
      except SkgServerQuietError:
        if self . buffered:
          raise SkgToolError (
            "The server stopped partway through a response." )
        return messages
      except SkgConnectionClosedError:
        return messages

  def save_buffer (self, view_id, buffer_text, baseline,
                   diffView_buffer = None, answers = None,
                   perspective = DEFAULT_PERSPECTIVE,
                   preview = False):
    """Send a save of BUFFER_TEXT, whose baseline (the text the server
    last drew for it) is BASELINE; return every response up to and including the
    terminal one, parsed. DIFFVIEW_BUFFER names the buffer of the diff
    view it is ('all changes', say), if it is one: the server holds no
    view of this connection's, and re-renders the save as that buffer,
    with its processed summary, only when the request names it.
    ANSWERS, a SaveAnswers, carries the answers to the questions
    earlier attempts of the same save asked, as the clients' retries
    carry them ('skg--save-request-sexp' in
    'elisp/skg-request-save.el'). PERSPECTIVE, the perspective
    the buffer was drawn from, is named
    unless it is the root homes, as the clients name it: the server
    checks and rerenders the buffer following it, and holds the view
    with it, which a save that names none resets to the root
    homes. With PREVIEW, the server writes nothing, holds nothing,
    asks nothing, and answers 'save-preview': what the save would hide
    and stop hiding, and the errors that would refuse it."""
    line = request_line (
      "save buffer", ( "view-id", view_id ),
      * ( [ ( "preview", "true" ) ] if preview else [] ),
      * ( [ ( "diff-view-buffer", diffView_buffer ) ]
          if diffView_buffer is not None else [] ),
      * ( [ ( "perspective", perspective ) ]
          if perspective != DEFAULT_PERSPECTIVE else [] ),
      ( "point-lines-below-focused-headline", "0" ),
      ( "point-column", "0" ),
      ( "point-screen-lines-below-window-start", "0" ))
    if answers is not None:
      line = line [ : -1 ] + answers . request_fields_text () + ")"
    return [ parse_sexp (message)
             for message in self . raw_save (line, buffer_text, baseline, view_id) ]

  def stage_and_commit (self, message, confirmed, processed_only = False):
    """Send "stage and commit" with MESSAGE, confirmed when CONFIRMED,
    asking for the processed nodes only when PROCESSED_ONLY; return its
    response, parsed, after reading the rerender stream that follows
    it up to its 'rerender-done'. The message goes one string per
    line, as '(message-lines "LINE" ...)', since the server reads one
    request per line."""
    self . send_line (
      request_line ( "stage and commit" ) [ : -1 ] +
      " (message-lines " +
      " " . join ( quote_string (line) for line in message . split ("\n") ) +
      ")" + ( " (processed-only . t)" if processed_only else "" ) +
      ( " (confirmed . t))" if confirmed else ")" ))
    response = parse_sexp ( self . read_message (IMPORT_TIMEOUT_SECONDS) )
    require_response_type (response, "stage-and-commit")
    while response_type ( parse_sexp (
        self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) )) != "rerender-done":
      pass
    return response

  def rerender_views (self, view_ids, approved_pids):
    """Send "rerender views" for VIEW_IDS, approving the text of
    APPROVED_PIDS; return every message of its answer, parsed, up to
    and including the 'rerender-done' that ends it: a rerender stream,
    perhaps after an overPrivateText confirmation. A daemon's
    connection keeps the rerendered views for the daemon to write."""
    self . send_line (
      "((request . \"rerender views\") (view-ids " +
      " " . join ( map ( quote_string, view_ids )) + ")" +
      approved_overPrivateText_field (approved_pids) + ")" )
    messages = []
    while not messages or response_type ( messages [-1] ) != "rerender-done":
      messages . append ( parse_sexp (
        self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) ))
    return messages

  def request_then_rerender_stream (self, line):
    """Send the request LINE, whose response a rerender stream follows
    ('rerender-lock', 'rerender-view'..., 'rerender-done'); return the
    response, parsed, after reading the stream. A daemon's connection
    keeps the rerendered views for the daemon to write."""
    self . send_line (line)
    response = parse_sexp ( self . read_message (IMPORT_TIMEOUT_SECONDS) )
    while response_type ( parse_sexp (
        self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) )) != "rerender-done":
      pass
    return response

  def held_views (self):
    """[(VIEW ID, FILES STEM)] of the views the connection holds."""
    return []

  def extend_diff_view (self, view_id):
    """Send "extend diff view" for VIEW_ID; return its
    response, parsed, and the 'rerender-view' message for VIEW_ID in
    the rerender stream that follows it (None when there is none),
    after reading that stream up to its 'rerender-done'."""
    response, rerendered, _ = self . request_with_rerender_of_view (
      request_line ( "extend diff view", ( "view-id", view_id )), view_id )
    require_response_type (response, "extend-diff-view")
    return response, rerendered

  def request_with_rerender_of_view (self, line, view_id):
    """Send the request LINE, whose response a rerender stream follows;
    return the response, parsed, the 'rerender-view' message for
    VIEW_ID in the stream (None when there is none) and the
    'rerender-done' that ends it, after reading the stream. A daemon's
    connection also keeps the rerendered views for the daemon, which
    writes those the command did not."""
    self . send_line (line)
    response = parse_sexp ( self . read_message (IMPORT_TIMEOUT_SECONDS) )
    rerendered = None
    while True:
      message = parse_sexp ( self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) )
      kind = response_type (message)
      if kind == "rerender-done":
        return response, rerendered, message
      if ( kind == "rerender-view"
           and sexp_to_text ( response_field (message, "view-id") ) == view_id ):
        rerendered = message

  def raw_save (self, line, buffer_text, baseline, saved_view_id = None):
    """Send the save request LINE with BUFFER_TEXT as its saved buffer
    and BASELINE as that buffer's baseline; return the text of every
    response up to and including the terminal one. The envelope's
    'other-views' lists the connection's other views
    ('other_views_text'), those but SAVED_VIEW_ID."""
    other_views = self . other_views_text (saved_view_id)
    self . send_line (line)
    envelope = save_envelope_text (
      buffer_text, baseline, other_views ) . encode ("utf-8")
    self . socket . sendall (
      f"Content-Length: {len (envelope)}\r\n\r\n" . encode () + envelope )
    messages = []
    while True:
      message = self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS)
      messages . append (message)
      if ( response_type ( parse_sexp (message) )
           in SAVE_TERMINAL_RESPONSE_TYPES ):
        return messages

  def other_views_text (self, saved_view_id):
    """The 'other-views' of a save's envelope: every other view the
    client holds live, a command's own connection none."""
    return "()"

  def send_line (self, line):
    self . socket . sendall ( line . encode ("utf-8") + b"\n" )

  def read_message (self, timeout_seconds):
    """One 'Content-Length: N\\r\\n\\r\\nPAYLOAD' message's payload.
    A server still initializing answers with a bare
    '((busy-initializing . ...))' line instead. A message the server
    sends unprompted, between responses, that it reloaded files
    changed on disk is no answer: 'handle_unprompted_message' takes
    it, and the next message is read."""
    while True:
      payload = self . read_one_message (timeout_seconds)
      if payload . startswith (UNPROMPTED_REPO_CHANGE_MESSAGE_START):
        self . handle_unprompted_message (payload)
        continue
      self . observe_message (payload)
      return payload

  def read_one_message (self, timeout_seconds):
    """The payload of the next message, whatever it is."""
    self . socket . settimeout (timeout_seconds)
    while b"\r\n\r\n" not in self . buffered:
      if self . buffered . startswith (b"((busy-initializing") and b"\n" in self . buffered:
        raise SkgToolError ( "The server is still initializing: " +
                             self . buffered . decode ("utf-8", "replace") . strip () )
      self . receive_more ()
    header, self . buffered = self . buffered . split (b"\r\n\r\n", 1)
    length = int ( header . decode () . split (":") [1] . strip () )
    while len (self . buffered) < length:
      self . receive_more ()
    payload = self . buffered [ : length ]
    self . buffered = self . buffered [ length : ]
    return payload . decode ("utf-8")

  def receive_more (self):
    try:
      chunk = self . socket . recv (65536)
    except socket . timeout:
      raise SkgServerQuietError ( "The server did not answer in time." )
    if not chunk:
      raise SkgConnectionClosedError ( "The server closed the connection." )
    self . buffered += chunk

def request_line (request_name, *fields):
  return ( "((request . " + quote_string (request_name) + ")" +
           "" . join ( " (" + key + " . " + quote_string (value) + ")"
                       for key, value in fields ) + ")" )

def response_type (response):
  form = metadata_form ( [ None ] + response, "response-type" )
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def response_field (response, field_name):
  """The value of a '(FIELD VALUE)' entry in a response alist: VALUE
  itself, or for '(FIELD V1 V2 ...)' the list of values. A field
  whose value is '()' gives []."""
  form = metadata_form ( [ None ] + response, field_name )
  if form is None:
    return None
  values = form [ 1 : ]
  if len (values) == 1:
    return values [0]
  return values

def response_text_field (response, field_name):
  value = response_field (response, field_name)
  return "" if value is None else sexp_to_text (value)

def save_envelope_text (buffer_text, baseline, other_views_text):
  """The envelope a 'save buffer' request carries: BUFFER_TEXT as the
  saved buffer, BASELINE as its baseline, and OTHER_VIEWS_TEXT, already
  a sexp list, as its 'other-views'."""
  return ( "((saved-buffer " + quote_string (buffer_text) +
           ") (baseline " + quote_string (baseline) +
           ") (other-views " + other_views_text + "))" )

def overPrivateText_answer (question, show, situation, refusal,
                            to_stderr = False,
                            approval = "the request is sent again"):
  """The PIDs an overPrivateText confirmation lists, approved when SHOW
  ('--show-overPrivateText'); else raise REFUSAL. SITUATION opens what
  the confirmation says happened, APPROVAL what approving it does.
  TO_STDERR: printed on standard error, for a command whose standard
  output is the paths it wrote."""
  output = sys . stderr if to_stderr else sys . stdout
  pids = [ sexp_to_text (pid) for pid in ( response_field (question, "pids") or [] ) ]
  print ( "Server: overPrivateText confirmation: " + situation +
          "the skgrepo restriction withholds text from overPrivateText "
          "telescopes" + ( " (" + ", " . join (pids) + ")" if pids else "" ) +
          " until approved. " + response_text_field (question, "prompt"),
          file = output )
  if not show:
    raise SkgToolError (refusal)
  print ( "Approved: " + approval + ", releasing that text.",
          file = output )
  return pids

def approved_overPrivateText_field (pids):
  return ( " (approved-overPrivateText-pids " +
           " " . join ( map ( quote_string, pids )) + ")" )

def request_releasing_overPrivateText (connection, line, show, what):
  """Send the request LINE and return its response; when the server
  instead asks to release text from overPrivateText telescopes (under a
  skgrepo restriction), print the question on standard error and, when
  SHOW, send LINE again approving it; else raise, saying nothing was
  WHAT."""
  response = connection . request (line)
  if response_type (response) != "overPrivateText-telescope-confirmation":
    return response
  pids = overPrivateText_answer (
    response, show, "",
    f"Nothing was {what}: the server withholds text from overPrivateText "
    "telescopes under the skgrepo restriction; run again with "
    "'--show-overPrivateText' to release it.", to_stderr = True )
  return connection . request (
    line [ : -1 ] + approved_overPrivateText_field (pids) + ")" )

def require_response_type (response, expected):
  actual = response_type (response)
  if actual == expected:
    return
  if actual == "overPrivateText-telescope-confirmation":
    raise SkgToolError (
      "The server asks to approve releasing over-private text, which "
      "this tool does not do: " + sexp_to_text (
        response_field (response, "prompt") or "" ))
  raise SkgToolError (
    f"Expected a {expected} response, got: " + render_sexp (response) [ : 2000 ] )
