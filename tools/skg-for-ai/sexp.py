"""S-expressions as the server writes them: 'parse_sexp' reads one,
'sexp_to_text' and 'render_sexp' write one back, and 'metadata_form'
finds a list inside one."""

import re

# ============================================================
# S-expressions as the server writes them: lists, double-quoted
# strings (only backslash and double quote escaped), bare symbols
# and integers.
# ============================================================

class Symbol (str):
  """A bare atom, as distinct from a double-quoted string."""
  pass

def parse_sexp (text):
  value, end = read_sexp_value (text, skip_whitespace (text, 0))
  return value

def read_sexp_value (text, position):
  character = text [position]
  if character == "(":
    elements = []
    position = skip_whitespace (text, position + 1)
    while text [position] != ")":
      element, position = read_sexp_value (text, position)
      elements . append (element)
      position = skip_whitespace (text, position)
    return elements, position + 1
  if character == '"':
    return read_sexp_string (text, position)
  end = position
  while end < len (text) and text [end] not in ' \t\n\r()"':
    end += 1
  atom = text [ position : end ]
  if re . fullmatch ( r'-?[0-9]+', atom ):
    return int (atom), end
  return Symbol (atom), end

def read_sexp_string (text, position):
  pieces = []
  position += 1
  while text [position] != '"':
    if text [position] == "\\":
      position += 1
    pieces . append ( text [position] )
    position += 1
  return "" . join (pieces), position + 1

def skip_whitespace (text, position):
  while position < len (text) and text [position] in " \t\n\r":
    position += 1
  return position

def end_of_balanced_sexp (text, start):
  """The offset just past the sexp starting at START, or None if the
  text ends first."""
  try:
    value, end = read_sexp_value (text, start)
  except IndexError:
    return None
  return end

def top_level_elements_with_offsets (list_text):
  """[(OFFSET, ELEMENT)] for each element of the list LIST_TEXT."""
  results = []
  position = skip_whitespace (list_text, 1)
  while list_text [position] != ")":
    element, end = read_sexp_value (list_text, position)
    results . append (( position, element ))
    position = skip_whitespace (list_text, end)
  return results

def sexp_to_text (value):
  """An atom's text; a list rendered as a sexp."""
  if isinstance (value, list):
    return render_sexp (value)
  return str (value)

def render_sexp (value):
  if isinstance (value, list):
    return "(" + " " . join ( render_sexp (element) for element in value ) + ")"
  if isinstance (value, Symbol) or isinstance (value, int):
    return str (value)
  return quote_string (value)

def quote_string (text):
  return '"' + text . replace ("\\", "\\\\") . replace ('"', '\\"') + '"'

def metadata_form (sexp, head_name):
  """The first element of SEXP (after its head) that is a list headed
  by HEAD_NAME, or None."""
  if not isinstance (sexp, list):
    return None
  for element in sexp [ 1 : ]:
    if ( isinstance (element, list) and element
         and isinstance (element [0], Symbol) and element [0] == head_name ):
      return element
  return None
