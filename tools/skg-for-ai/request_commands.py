"""The commands that send the server a request of their own instead of
writing a view: 'raw', 'import', 'delete-references' and
'restrict'."""

from sexp import parse_sexp, quote_string, sexp_to_text
from server_connection import (
  IMPORT_TIMEOUT_SECONDS, SkgConnection, SkgToolError,
  approved_overPrivateText_field, connect_to_server,
  overPrivateText_answer, request_line, require_response_type,
  response_field, response_text_field, response_type )
from save import refuse_while_held_views_have_edits

def command_raw (arguments):
  """Send ARGUMENTS.request, and with '--saved-buffer' its save body,
  whose baseline is the text of '--baseline' (by default empty);
  print each response as the server sent it. Without a body, every
  response arriving until the server is quiet for RAW_QUIET_SECONDS."""
  connection = SkgConnection (arguments . port)
  try:
    if arguments . saved_buffer is None:
      messages = connection . raw_request (arguments . request)
    else:
      with open (arguments . saved_buffer, encoding = "utf-8") as buffer_file:
        buffer_text = buffer_file . read ()
      baseline = ""
      if arguments . baseline is not None:
        with open (arguments . baseline, encoding = "utf-8") as baseline_file:
          baseline = baseline_file . read ()
      messages = connection . raw_save (
        arguments . request, buffer_text, baseline )
  finally:
    connection . close ()
  print_messages (messages)

def command_import (arguments):
  """Preview the import, then apply it, or with '--preview-only'
  cancel it, on one connection: the server binds the approval token
  to the connection that previewed. A request for a host root, sent
  when the input links to files by absolute path, is answered with
  '--host-root', the input directory's absolute path on the host; by
  default with the empty one, which leaves those links unresolved."""
  preview_fields = [ ( "action", "preview" ),
                     ( "input-directory", arguments . input_dir ),
                     ( "destination-repo", arguments . repo ) ]
  messages = []
  connection = SkgConnection (arguments . port)
  try:
    def send (fields):
      message = connection . request_text (
        request_line ( "import md and org", *fields ),
        IMPORT_TIMEOUT_SECONDS )
      messages . append (message)
      return parse_sexp (message)
    preview = send (preview_fields)
    if response_type (preview) == "import-md-and-org-host-mapping-needed":
      preview = send ( preview_fields + [ ( "host-root", arguments . host_root ) ] )
    token = response_field (preview, "approval-token")
    if token is not None:
      send ( [ ( "action", "cancel" ) ] if arguments . preview_only
             else [ ( "action", "apply" ),
                    ( "approval-token", sexp_to_text (token) ) ] )
  finally:
    connection . close ()
  print_messages (messages)

def command_delete_references (arguments):
  """Send "delete references to absent node" for an ID the graph lacks,
  as the clients' 'skg-delete-references-to-absent-node' ('C-c d a')
  does: the server removes it from every owned 'contains', 'forks'
  and 'hides' list, and rerenders
  the views that showed one. Without '--yes' nothing is sent, since the
  server asks first only when titles or bodies link to the ID; when it
  asks, its warning is printed, and '--leave-links' approves, sending
  the request again with the preview it names. Like the clients, it
  refuses while a view the daemon holds has unsaved edits."""
  if not arguments . yes:
    print ( f"Not sent. This removes every owned structured reference to the "
            f"absent node {arguments . id} (in 'contains', 'forks' and "
            "'hides' lists); run again with '--yes'." )
    return
  connection = connect_to_server (arguments . port)
  try:
    refuse_while_held_views_have_edits (
      connection, "removing the references to an absent node" )
    line = request_line ( "delete references to absent node",
                          ( "id", arguments . id ))
    response = connection . request_then_rerender_stream (line)
    if response_type (response) == "delete-references-confirmation":
      print ( "Server: delete-references confirmation: titles or bodies link "
              f"to {arguments . id}; the cleanup leaves those links, dangling. "
              + response_text_field (response, "prompt") )
      print ( response_text_field (response, "content") . rstrip ("\n") )
      if not arguments . leave_links:
        raise SkgToolError (
          "Nothing was removed: run again with '--yes --leave-links' to "
          "remove the structured references and leave the links." )
      print ( "Approved: the request is sent again." )
      response = connection . request_then_rerender_stream (
        line [ : -1 ] + " (approved-preview . " + quote_string (
          response_text_field (response, "approved-preview") ) + "))" )
    if response_type (response) != "delete-references-result":
      raise SkgToolError (
        "The server refused: " + response_text_field (response, "content") )
    connection . log_event ( "removed the references to " + arguments . id )
  finally:
    connection . close ()
  print ( "Server: removed the references to " + arguments . id + ": " +
          response_text_field (response, "changed-memberships") +
          " membership(s) in " + response_text_field (response, "changed-nodes") +
          " node(s)." )
  print ( response_text_field (response, "content") . rstrip ("\n") )

def command_restrict (arguments):
  """With neither NAME nor '--foreign', print the skgrepo restriction
  and its rungs, as 'skg-list-repo-sets' does. Otherwise set it, as
  the clients' 'skg-restrict-repo-set' ('C-c v l') does: NAME is the
  most private owned skgrepo to show, '--foreign' whether the foreign
  skgrepos are shown. The restriction belongs to the connection, so
  setting it needs a daemon, whose connection keeps it for every
  command run through it; the server rerenders the views it holds,
  which it rewrites. Refused while a held view has unsaved edits. When
  the new restriction would release text from overPrivateText
  telescopes, the server asks first; '--show-overPrivateText'
  approves."""
  connection = connect_to_server (arguments . port)
  try:
    if arguments . name is None and arguments . foreign is None:
      response = connection . request ( request_line ("list repo sets") )
      require_response_type (response, "repo-sets")
      rungs = [ sexp_to_text (rung) for rung in response_field (response, "sets") or [] ]
      print ( skgrepo_restriction_text (response, rungs) + "; available: " +
              ", " . join ( skgrepo_rung_label (rung, rungs) for rung in rungs ) + "." )
      return
    if not connection . holds_views_it_writes ():
      raise SkgToolError (
        "The skgrepo restriction belongs to a connection, and this command's "
        "own ends with it; start a daemon ('daemon --out-dir DIR &') and run "
        "'restrict' through it, so that the commands run through it see the "
        "restriction." )
    refuse_while_held_views_have_edits (
      connection, "switching the skgrepo restriction" )
    line = request_line (
      "set skgrepo restriction",
      * ( [ ( "name", arguments . name ) ] if arguments . name is not None else [] ),
      * ( [ ( "foreign-repos", arguments . foreign ) ]
          if arguments . foreign is not None else [] ))
    response = connection . request_then_rerender_stream (line)
    if response_type (response) == "overPrivateText-telescope-confirmation":
      pids = overPrivateText_answer (
        response, arguments . show_overPrivateText, "",
        "The skgrepo restriction is as it was: run again with "
        "'--show-overPrivateText' to release that text." )
      response = connection . request_then_rerender_stream (
        line [ : -1 ] + approved_overPrivateText_field (pids) + ")" )
    require_response_type (response, "skgrepo-restriction")
    refusal = response_field (response, "content")
    if refusal is not None:
      raise SkgToolError ( "The server refused: " + sexp_to_text (refusal) )
    rungs = [ sexp_to_text (rung) for rung in response_field (
      connection . request ( request_line ("list repo sets") ), "sets" ) or [] ]
    described = skgrepo_restriction_text (response, rungs)
    connection . log_event (described)
  finally:
    connection . close ()
  print (described)

def skgrepo_rung_label (rung, rungs):
  """How the clients show a rung ('skg--skgrepo-restriction-label'):
  the most private, which with foreign skgrepos shown is no
  restriction, says so."""
  return rung + ( " (no restriction)" if rungs and rung == rungs [-1] else "" )

def skgrepo_restriction_text (response, rungs):
  """What the clients say of a skgrepo restriction RESPONSE
  ('skg--skgrepo-restriction-message')."""
  rung = response_text_field (response, "restriction")
  if response_text_field (response, "foreign-repos") == "hidden":
    return f"Skgrepo restriction: {rung}; foreign repos hidden"
  return "Skgrepo restriction: " + skgrepo_rung_label (rung, rungs)

def print_messages (messages):
  for index, message in enumerate (messages):
    print ( f"--- message {index} ({len (message)} chars) ---" )
    print (message)
