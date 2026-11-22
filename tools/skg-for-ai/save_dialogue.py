"""'save_dialogue': what 'save --yes' does once the save is sent:
'send_save_and_write_its_rerender' sends it, answers the server's
questions by retries ('answer_save_question': the fork confirmation,
'--delete-from', '--strandeds', '--hoist') and writes the view the
server rerenders ('rerendered_view_of_save'). Also what a preview
reads of the server's answer: 'server_supports', 'print_save_preview'."""

import re

from sexp import Symbol, quote_string, render_sexp, sexp_to_text
from server_connection import (
  SkgToolError, overPrivateText_answer, request_line, require_response_type,
  response_field, response_text_field, response_type )
from full_view import headline_gnid, headline_node_repo, parse_full_view
from compact_view import processed_summary_text
from views import (
  RenderedView, perspective_fields_of_sidecar, perspective_of_sidecar,
  release_view, release_withheld_save_rerender, write_view_files )

def send_save_and_write_its_rerender (connection, saved_view, buffer_text,
                                      arguments, forks):
  """Send the save of BUFFER_TEXT, answering the server's questions as
  ARGUMENTS say and approving only the forks FORKS predicts
  ('answer_save_question'); release a withheld rerender when
  '--show-overPrivateText' allows; print the server's answer, and
  write its re-rendered view over SAVED_VIEW's three files. Return
  their paths."""
  answers = SaveAnswers ()
  for attempt in range (SAVE_ATTEMPTS_LIMIT):
    messages = connection . save_buffer (
      saved_view . sidecar [ "view_id" ], buffer_text,
      saved_view . full_text,
      saved_view . sidecar [ "request" ] . get ("buffer"), answers,
      perspective_of_sidecar (saved_view . sidecar) )
    if response_type ( messages [-1] ) not in SAVE_QUESTION_TYPES:
      break
    answer_save_question ( messages [-1], answers, arguments, forks )
  terminal = messages [-1]
  if response_type (terminal) == "overPrivateText-telescope-confirmation":
    overPrivateText_answer (
      terminal, arguments . show_overPrivateText, "the save succeeded, but ",
      "The save succeeded, but its re-rendered view is withheld; the files "
      "are out of date. View the node again with '--show-overPrivateText'.",
      approval = "the withheld rerender is asked for" )
    terminal = release_withheld_save_rerender (
      connection, terminal, saved_view . sidecar [ "view_id" ] )
  rerendered = rerendered_view_of_save ( terminal, saved_view . sidecar )
  connection . log_view_event (
    "saved", saved_view . directory, saved_view . base_name )
  written_paths = write_view_files (
    connection, rerendered, saved_view . directory, saved_view . base_name,
    saved_view . sidecar [ "summary" ] )
  release_view (connection, rerendered . view_id)
  return written_paths

# ------------------------------------------------------------
# The fork confirmation
# ------------------------------------------------------------

# What the server's fork confirmation shows as the skgrepo of a clone
# whose skgrepo the edit did not settle, which the client must replace
# ('FORK_SKGREPO_PLACEHOLDER' in 'server/from_text/fork.rs').
FORK_SKGREPO_PLACEHOLDER = "PICK-A-REPO"
# The comment the server writes above such a clone.
SUGGESTED_FORK_SKGREPO_PATTERN = re . compile (
  r'^# Suggested repo for the clone below: (.+)$' )

class ForkConfirmationClone:
  """A clone-to-be of the fork confirmation: its title, the skgrepo
  shown for it (perhaps the placeholder), the server's suggestion
  above it (or None), and 'originals', [(ID, SKGREPO, TITLE)], the
  nodes it forks."""
  def __init__ (self, title, skgrepo, suggested_skgrepo):
    self . title = title
    self . skgrepo = skgrepo
    self . suggested_skgrepo = suggested_skgrepo
    self . originals = []

def fork_confirmation_clones (content):
  """The clones-to-be of the fork confirmation's buffer CONTENT, read
  as the clients read it ('skg--fork-repos-from-confirmation-buffer'
  in 'elisp/skg-request-save/skg-save-confirmations.el'): each top-level headline with
  metadata is a clone-to-be, and each headline with an ID under one
  that names its skgrepo is an original it forks. The instructions
  headline has no metadata."""
  clones = []
  clone = None
  previous_text = ""
  for view_line in parse_full_view (content):
    if view_line . is_headline and len (view_line . stars) == 1:
      clone = None
      if view_line . metadata is not None:
        suggestion = SUGGESTED_FORK_SKGREPO_PATTERN . match (previous_text)
        clone = ForkConfirmationClone (
          view_line . title, headline_node_repo (view_line . metadata),
          suggestion . group (1) . strip () if suggestion else None )
        clones . append (clone)
    elif ( view_line . is_headline and len (view_line . stars) == 2
           and clone is not None and clone . skgrepo is not None
           and view_line . metadata is not None
           and headline_gnid (view_line . metadata) is not None ):
      clone . originals . append ((
        headline_gnid (view_line . metadata),
        headline_node_repo (view_line . metadata), view_line . title ))
    previous_text = view_line . text
  return clones

def approved_fork_skgrepos_of_confirmation (confirmation, forks):
  """Print the fork confirmation, as the clients' confirmation buffer
  shows it: each clone-to-be, its skgrepo, and the original it forks.
  Return [(ID, SKGREPO)], each original's ID with its clone's skgrepo,
  which approves it: a placeholder is replaced by FORKS'
  'clone_skgrepo', as a client's prompt replaces it. Raise, saving
  nothing, when the server forks a node the report did not list
  (FORKS' 'gnids'): '--yes' approved only what the report showed."""
  clones = fork_confirmation_clones (
    sexp_to_text ( response_field (confirmation, "content") or "" ))
  originals = [ original for clone in clones for original in clone . originals ]
  print ( f"Server: fork confirmation: the save forks {len (originals)} "
          f"node{'' if len (originals) == 1 else 's'}, and writes nothing "
          "until that is approved." )
  approved = []
  for clone in clones:
    placeholder = clone . skgrepo == FORK_SKGREPO_PLACEHOLDER
    skgrepo = forks . clone_skgrepo if placeholder else clone . skgrepo
    print ( f"Clone-to-be: '{clone . title}', in " +
            ( f"'{skgrepo}'" if skgrepo is not None else "no skgrepo yet" ) +
            ( "" if placeholder else ", as the edit settles it" ) +
            ( f" (the server suggests '{clone . suggested_skgrepo}')"
              if clone . suggested_skgrepo else "" ) + "." )
    for gnid, original_skgrepo, title in clone . originals:
      print ( f"  It forks '{title}' ({gnid}), of the skgrepo "
              f"'{original_skgrepo}'." )
      approved . append (( gnid, skgrepo ))
  unlisted = [ gnid for gnid, _, _ in originals if gnid not in forks . gnids ]
  if unlisted or not approved:
    raise SkgToolError (
      "Nothing was saved: the server asks to fork " +
      ( ", " . join (unlisted) if unlisted else "nothing it names" ) +
      ", which the report did not list as a fork; '--yes' approves only "
      "what the report showed. A client can review it." )
  if any ( skgrepo is None for _, skgrepo in approved ):
    raise SkgToolError (
      "Nothing was saved: a clone needs an owned skgrepo; name one with "
      "'--fork-into REPO'." )
  print ( "Approved: the save is sent again with the forks approved." )
  return approved

# ------------------------------------------------------------
# The save's other questions, and the answers its retries carry
# ------------------------------------------------------------

# The questions a save may ask in place of its result, each answered by
# a retry of the same save that carries the answer.
SAVE_QUESTION_TYPES = (
  "delete-from-confirmation", "strandeds-confirmation",
  "telescope-hoist-confirmation", "fork-confirmation" )
# How many times one save may be sent: once, and once per question
# (the Hoist question can come again when a telescope became dirty
# meanwhile).
SAVE_ATTEMPTS_LIMIT = 8

class SaveAnswers:
  """The answers a save carries on its retries, each retry carrying
  every earlier one, as the clients' do ('skg--save-request-sexp' in
  'elisp/skg-request-save.el'): 'fork_skgrepos', [(ID, SKGREPO)], the
  approved forks; 'hoist_pids', the telescopes approved for Hoist;
  'strandeds_reset', None until the strandeds are reviewed, then the
  [(RELATION, RECORDER, MEMBER)] to reset to default; 'delete_from',
  'visible' or 'all'."""
  def __init__ (self):
    self . fork_skgrepos = None
    self . hoist_pids = None
    self . strandeds_reset = None
    self . delete_from = None

  def request_fields_text (self):
    """The fields the save request carries for these answers, each
    after a space."""
    fields = []
    if self . fork_skgrepos is not None:
      fields += [ '(approved-forks . "true")',
                  "(fork-repos (" + " " . join (
                    "(" + quote_string (gnid) + " . " + quote_string (skgrepo) + ")"
                    for gnid, skgrepo in self . fork_skgrepos ) + "))" ]
    if self . hoist_pids:
      fields . append ( "(approved-hoist-pids " +
                        " " . join ( map ( quote_string, self . hoist_pids )) + ")" )
    if self . strandeds_reset is not None:
      fields . append ( '(strandeds-reviewed . "true")' )
      if self . strandeds_reset:
        fields . append ( "(reset-strandeds " + " " . join (
          "(" + " " . join ( map ( quote_string, triple )) + ")"
          for triple in self . strandeds_reset ) + ")" )
    if self . delete_from is not None:
      fields . append ( "(delete-from . " + quote_string (self . delete_from) + ")" )
    return "" . join ( " " + field for field in fields )

def answer_save_question (question, answers, arguments, forks):
  """Print the save's QUESTION, and record in ANSWERS the answer the
  command line gives it, so that the save is sent again with it; raise,
  sending nothing more, when it gives none. '--yes' alone takes the
  answer that writes nothing more: Abort, cancel, decline. FORKS are
  what the report predicted, which approves a fork confirmation."""
  kind = response_type (question)
  if kind == "fork-confirmation":
    answers . fork_skgrepos = approved_fork_skgrepos_of_confirmation (question, forks)
  elif kind == "delete-from-confirmation":
    answers . delete_from = delete_from_answer (question, arguments . delete_from)
  elif kind == "strandeds-confirmation":
    answers . strandeds_reset = strandeds_answer (question, arguments . strandeds)
  elif kind == "telescope-hoist-confirmation":
    answers . hoist_pids = hoist_answer (question, arguments . hoist)

def delete_from_answer (question, delete_from):
  """'visible' or 'all', as DELETE_FROM ('--delete-from') says, for a
  delete-from confirmation ('skg--delete-from-confirmation-handler'):
  records the skgrepo restriction keeps out name a node the save
  deletes."""
  print ( f"Server: delete-from confirmation: "
          f"{response_text_field (question, 'count')} record(s) the skgrepo "
          "restriction keeps out name a node this save deletes; nothing was "
          "written. " + response_text_field (question, "prompt") )
  if delete_from is None:
    raise SkgToolError (
      "Nothing was saved (cancelled): save again with '--delete-from "
      "visible' to strip the deleted IDs from the records the restriction "
      "shows only, or '--delete-from all' to strip them from every owned "
      "record." )
  print ( "Delete from: " + ( "the visible records only"
                              if delete_from == "visible"
                              else "every owned record" ) +
          "; the save is sent again." )
  return delete_from

def strandeds_answer (question, strandeds):
  """The relationships to reset to default, as STRANDEDS
  ('--strandeds') says, for a strandeds confirmation
  ('skg--strandeds-confirmation-handler'): 'reset' every listed one,
  'keep' none. Each is printed as the clients list it."""
  listed = [
    tuple ( response_text_field (entry, field)
            for field in ( "relation", "recorder", "member", "relRepo", "default" ))
    for entry in ( response_field (question, "strandeds") or [] ) ]
  print ( f"Server: strandeds confirmation: the save's home skgrepo moves "
          f"would leave {len (listed)} relationship"
          f"{'' if len (listed) == 1 else 's'} in their old skgrepos; nothing "
          "was written." )
  for relation, recorder, member, relRepo, default in listed:
    print ( f"  {recorder} {relation} {member}: stays at {relRepo}; its default "
            f"is now {default}." )
  if strandeds is None:
    raise SkgToolError (
      "Nothing was saved: save again with '--strandeds reset' to move every "
      "listed relationship to its new default, or '--strandeds keep' to "
      "leave them where they are." )
  print ( "Strandeds: " + ( "each is reset to its default"
                            if strandeds == "reset" else "each stays where it is" )
          + "; the save is sent again." )
  return ( [ ( relation, recorder, member )
             for relation, recorder, member, _, _ in listed ]
           if strandeds == "reset" else [] )

def hoist_answer (question, hoist):
  """The telescopes a Hoist confirmation lists, approved when HOIST
  ('--hoist'); else Abort, which writes nothing
  ('skg--telescope-hoist-confirmation-handler')."""
  telescopes = [ ( response_text_field (entry, "pid"),
                   response_text_field (entry, "home") )
                 for entry in ( response_field (question, "telescopes") or [] ) ]
  print ( "Server: Hoist confirmation: " + response_text_field (question, "prompt") )
  for pid, home in telescopes:
    print ( f"  {pid}, whose home is '{home}'." )
  if not hoist:
    raise SkgToolError (
      "Nothing was saved (Abort). Save again with '--hoist' to Hoist, or "
      "repair the '.skg' files by hand." )
  print ( "Hoist: approved; the save is sent again." )
  return [ pid for pid, _ in telescopes ]

# ------------------------------------------------------------
# The server's answer
# ------------------------------------------------------------

def server_supports (connection, feature):
  """Whether CONNECTION's server names FEATURE among those its answer
  to "verify connection" says it supports. An older server names none,
  and ignores a request field it does not know: a save sent with
  '(preview . "true")' would be saved for real."""
  response = connection . request ( request_line ( "verify connection" ))
  require_response_type (response, "verify-connection")
  return feature in ( response_field (response, "supports") or [] )

def print_save_preview (terminal):
  """Print what the server's answer to a preview of the save, TERMINAL,
  says the save would hide and stop hiding, one line each, as the
  server words them ('docs/api-and-formats.org', "Save buffer"); and,
  when the server would refuse the save, its errors."""
  if response_type (terminal) != "save-preview":
    raise SkgToolError (
      "The server answered the preview of the save with "
      f"'{response_type (terminal)}', not 'save-preview': "
      + render_sexp (terminal) )
  for line in response_field (terminal, "hides") or []:
    print (line)
  errors = response_field (terminal, "errors") or []
  if errors:
    print ( "The server would refuse this save:" )
    for error in errors:
      print ( "Server error: " + error )

def rerendered_view_of_save (terminal, sidecar):
  """Print the server's answer to the save. Return its re-rendered
  view, a 'RenderedView'; raise if there is none."""
  kind = response_type (terminal)
  if kind != "save-result":
    print ( f"Server: {kind}." )
    for field_name in ( "prompt", "to-minibuffer" ):
      text = response_field (terminal, field_name)
      if text:
        print ( sexp_to_text (text) )
    raise SkgToolError (
      "Nothing was saved: the server asks a question this tool does not "
      "answer. A client can answer it." )
  content = response_field (terminal, "content")
  errors = response_field (terminal, "errors") or []
  warnings = response_field (terminal, "warnings") or []
  saved = isinstance (content, str) and not isinstance (content, Symbol)
  print ( "Server: " + ( "saved" if saved else "nothing was saved" ) +
          f", {len (errors)} error{'' if len (errors) == 1 else 's'}, "
          f"{len (warnings)} warning{'' if len (warnings) == 1 else 's'}." )
  for error in errors:
    print ( "Server error: " + error )
  for warning in warnings:
    print ( "Server warning: " + warning )
  processed_summary = response_field (terminal, "processed-summary")
  if saved and processed_summary:
    print ( "Processed: " + processed_summary_text (processed_summary) )
  for line in response_field (terminal, "hides") or []:
    print (line)
  if not saved:
    raise SkgToolError (
      "The server refused the save; the files are as they were." )
  return RenderedView (
    view_id  = sidecar [ "view_id" ],
    command  = sidecar [ "command" ],
    request  = sidecar [ "request" ],
    content  = content,
    errors   = errors,
    warnings = warnings,
    processed_summary = processed_summary,
    ** perspective_fields_of_sidecar (sidecar) )
