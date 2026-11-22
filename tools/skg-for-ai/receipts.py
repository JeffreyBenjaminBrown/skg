"""'receipt': writes a read receipt under your fork of a mailed item
and hides the item from your fork of its mailbox, printing first what
it would do."""

from sexp import sexp_to_text
from server_connection import (
  SkgToolError, connect_to_server, request_line, response_field,
  response_text_field, response_type )
from save import refuse_while_held_views_have_edits

def command_receipt (arguments):
  """Send "receipt" for ITEM, as the Emacs command 'skg-receipt' does:
  the server writes the note TEXT (titled '--title') as the first
  child of your fork of ITEM, forking it first if you hold none, and
  hides ITEM from your fork of the mailbox it is in, all in one write,
  then rerenders the views that show them. Without '--yes' it asks the
  server what it would do, and writes nothing. A refusal names what
  to choose: '--mailbox' or '--fork-into'. Like the clients, it
  refuses while a view the daemon holds has unsaved edits."""
  if "\n" in arguments . text or "\n" in arguments . title:
    raise SkgToolError ( "Nothing was sent: a receipt's text and title are "
                         "one line each." )
  fields = [ ( "item", arguments . item ), ( "text", arguments . text ),
             ( "title", arguments . title ) ]
  if arguments . mailbox is not None:
    fields . append (( "mailbox", arguments . mailbox ))
  if arguments . fork_into is not None:
    fields . append (( "fork-into", arguments . fork_into ))
  if arguments . no_hide:
    fields . append (( "hide", "false" ))
  if not arguments . yes:
    fields . append (( "preview", "true" ))
  connection = connect_to_server (arguments . port)
  try:
    if arguments . yes:
      refuse_while_held_views_have_edits ( connection, "the receipt" )
    response = connection . request_then_rerender_stream (
      request_line ( "receipt", *fields ))
    if response_type (response) != "receipt":
      raise SkgToolError (
        "The server refused: " + response_text_field (response, "content") )
    refusal = response_field (response, "refusal")
    if refusal is not None:
      raise SkgToolError ( "Nothing was written: " + sexp_to_text (refusal) +
                           receipt_choices_text (response) )
    if arguments . yes:
      connection . log_event ( "wrote a receipt for " + arguments . item )
  finally:
    connection . close ()
  if arguments . yes:
    print ( "Server: wrote the receipt " +
            response_text_field (response, "receipt-id") + " under " +
            response_text_field (response, "holder-id") + "." )
  else:
    print ( "Not written. The receipt would do this; run again with '--yes':" )
  for line in response_field (response, "lines") or []:
    print ( sexp_to_text (line) )

def receipt_choices_text (response):
  """The choices a receipt's refusal offers, as a sentence, or ""."""
  skgrepos = response_field (response, "choose-skgrepo")
  if skgrepos:
    return ( " Run again with '--fork-into' one of: " +
             ", " . join ( sexp_to_text (s) for s in skgrepos ) + "." )
  mailboxes = response_field (response, "choose-mailbox")
  if mailboxes:
    titles = response_field (response, "choose-mailbox-titles") or []
    return ( " Run again with '--mailbox' one of: " + ", " . join (
      f"{sexp_to_text (pid)} ('{sexp_to_text (title)}')"
      for pid, title in zip (mailboxes, titles) ) + "." )
  return ""
