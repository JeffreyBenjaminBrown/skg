"""'retyped_headlines': a new headline of a save whose title is, or
resembles, a removed one's. An exact retype is an error
('retyped_headline_error_lines') until the headline is marked
'⟨new⟩' or its handle restored; a reworded one is a question
('reworded_headline_pairs', 'reworded_question_lines'), which
'--keep-ids' answers by 'give_new_headlines_their_removed_handles'.
The fuzzy match: 'titles_probably_reworded', on the words
'normalized_title_words' keeps."""

import difflib
import re

from edited_compact_view import NEW_NODE_HANDLE_WORD
from outline_words import described_handle

# ------------------------------------------------------------
# Retyped headlines: a new headline whose title is, or resembles, a
# removed one's
# ------------------------------------------------------------
# Words too common to say what a title is about: articles,
# prepositions, conjunctions, pronouns and the commonest verbs. The
# fuzzy match ignores them.
TITLE_STOPWORDS = frozenset ( """
  a an the
  of in on at to for from by with about into onto over under up down
  out off as than via per after before between through during without
  within against among around behind below above across along toward
  towards upon
  and or but nor so if then
  i me my mine we us our ours you your yours he him his she her hers
  it its they them their theirs this that these those who whom whose
  which what
  is are was were be been being am has have had having do does did
  done will would can could shall should may might must
  """ . split () )
# The suffixes the crude stemmer strips, tried in this order; the
# first one a word ends in that leaves a stem this long is stripped.
STEMMED_SUFFIXES = [ "ing", "ed", "es", "ly", "s" ]
MINIMUM_STEM_LENGTH = 3
# A new headline and a removed one probably state the same thing
# reworded when either signal reaches its threshold; the ratio counts
# only when both titles have at least REWORDED_RATIO_MINIMUM_WORDS
# normalized words, since on shorter titles it fires on one shared
# word ("budget meeting" / "budget notes").
REWORDED_DICE_THRESHOLD = 0.6
REWORDED_RATIO_THRESHOLD = 0.75
REWORDED_RATIO_MINIMUM_WORDS = 3

def retyped_headline_error_lines (edited, original, headline_of_handle):
  """An error per new headline, not marked '⟨new⟩', whose title equals
  the title of a headline the edit removed: retyping a headline
  instead of keeping its handle would lose its node's ID."""
  removed = removed_titled_headlines (edited, original)
  errors = []
  for index in unmarked_new_titled_indices (edited):
    headline = edited [index]
    handles = [ handle for handle, title in removed
                if title == headline . title ]
    if not handles:
      continue
    restored = ( f"end it with ⟨{handles [0]}⟩" if len (handles) == 1
                 else "end it with one of those handles" )
    errors . append (
      f"Error: the new headline '{headline . title}' on line "
      f"{headline . compact_line_number} of the compact view has the title "
      "of the removed " + ", " . join (
        described_handle (handle, headline_of_handle) for handle in handles )
      + f". If it is that node, moved or reworded, {restored}; if it is "
      f"another node, end it with '⟨{NEW_NODE_HANDLE_WORD}⟩'." )
  return errors

class RewordedHeadlinePair:
  """A new headline, by its index in the edited outline, and the
  removed headline, by handle, whose title it probably rewords; and
  the other removed headlines its title resembles, best first."""
  def __init__ (self, new_index, removed_handle, other_removed_handles):
    self . new_index = new_index
    self . removed_handle = removed_handle
    self . other_removed_handles = other_removed_handles

def reworded_headline_pairs (edited, original):
  """Each new headline not marked '⟨new⟩' paired with the removed
  headline whose title it probably rewords, one to one, in the edited
  outline's order. Candidate pairs are taken best first, by the larger
  of their two signals, and a pair either of whose headlines is
  already paired is skipped."""
  removed = removed_titled_headlines (edited, original)
  candidates_of_new_index = {}
  for index in unmarked_new_titled_indices (edited):
    for removed_order, ( handle, title ) in enumerate (removed):
      if titles_probably_reworded ( edited [index] . title, title ):
        candidates_of_new_index . setdefault ( index, [] ) . append (
          ( - max ( title_similarity ( edited [index] . title, title )),
            removed_order, handle ))
  ranked = sorted (
    ( candidate + ( index, )
      for index, candidates in candidates_of_new_index . items ()
      for candidate in candidates ),
    key = lambda candidate : ( candidate [0], candidate [3], candidate [1] ))
  pairs = []
  paired_indices, paired_handles = set (), set ()
  for _, _, handle, index in ranked:
    if index in paired_indices or handle in paired_handles:
      continue
    paired_indices . add (index)
    paired_handles . add (handle)
    pairs . append ( RewordedHeadlinePair (
      index, handle,
      [ other for _, _, other in sorted ( candidates_of_new_index [index] )
        if other != handle ] ))
  return sorted ( pairs, key = lambda pair : pair . new_index )

def reworded_question_lines (edited, pairs, headline_of_handle):
  """A question per pair: is the new headline the removed one, reworded?"""
  lines = []
  for pair in pairs:
    headline = edited [ pair . new_index ]
    lines . append (
      "Question: removed " +
      described_handle ( pair . removed_handle, headline_of_handle ) +
      f" / new '{headline . title}' (line {headline . compact_line_number}): "
      "probably the same statement reworded; keep its ID?" +
      ( " Its title also resembles " + ", " . join (
          described_handle (handle, headline_of_handle)
          for handle in pair . other_removed_handles ) + "."
        if pair . other_removed_handles else "" ))
  return lines

def give_new_headlines_their_removed_handles (edited, pairs):
  """The answer '--keep-ids': each pair's new headline takes the removed
  headline's handle, and so its metadata and node, with its new title."""
  for pair in pairs:
    edited [ pair . new_index ] . handle = pair . removed_handle

def removed_titled_headlines (edited, original):
  """[(HANDLE, TITLE)] of each titled headline of the full view whose
  handle the edited compact view no longer has, in the full view's
  order."""
  kept_handles = { headline . handle for headline in edited
                   if headline . handle is not None }
  return [ ( headline . handle, headline . title ) for headline in original
           if headline . handle not in kept_handles and headline . title ]

def unmarked_new_titled_indices (edited):
  return [ index for index, headline in enumerate (edited)
           if headline . handle is None and not headline . marked_new
           and not headline . is_placed () and headline . title ]

def titles_probably_reworded (left_title, right_title):
  """Whether two different titles probably state the same thing
  reworded: their normalized words' Dice coefficient reaches its
  threshold, or difflib's ratio of their normalized texts reaches its
  own and each title has REWORDED_RATIO_MINIMUM_WORDS normalized
  words."""
  if left_title == right_title:
    return False
  dice, ratio = title_similarity (left_title, right_title)
  return ( dice >= REWORDED_DICE_THRESHOLD
           or ( ratio >= REWORDED_RATIO_THRESHOLD
                and min ( len ( normalized_title_words (left_title) ),
                          len ( normalized_title_words (right_title) ))
                    >= REWORDED_RATIO_MINIMUM_WORDS ))

def title_similarity (left_title, right_title):
  """(DICE, RATIO): the Dice coefficient of the two titles' sets of
  normalized words, 2 |A ∩ B| / (|A| + |B|), and
  'difflib.SequenceMatcher''s ratio of their normalized words joined
  by spaces. Both are 0 when either title has no word left."""
  left_words = normalized_title_words (left_title)
  right_words = normalized_title_words (right_title)
  if not left_words or not right_words:
    return ( 0.0, 0.0 )
  left_set, right_set = set (left_words), set (right_words)
  return ( 2 * len ( left_set & right_set ) / ( len (left_set) + len (right_set) ),
           difflib . SequenceMatcher (
             None, " " . join (left_words), " " . join (right_words)
           ) . ratio () )

def normalized_title_words (title):
  """TITLE's words as the fuzzy match compares them: lowercased,
  apostrophes dropped ("it's" is 'its'), every other run of
  punctuation a word break, stopwords dropped, each word crudely
  stemmed."""
  words = re . sub ( r'[\W_]+', " ",
                     re . sub ( "['’]", "", title . lower () )) . split ()
  return [ crude_stem (word) for word in words
           if word not in TITLE_STOPWORDS ]

def crude_stem (word):
  """WORD without the first of STEMMED_SUFFIXES it ends in that leaves
  a stem of MINIMUM_STEM_LENGTH letters, and then without a final 'e'
  (on a stem longer than that), so that 'save', 'saves', 'saved' and
  'saving' are all 'sav'. '-es' is stripped only after s, x, z, ch
  or sh ('boxes', not 'makes'), and '-s' never after another s
  ('class')."""
  for suffix in STEMMED_SUFFIXES:
    stem = word [ : - len (suffix) ]
    if ( not word . endswith (suffix)
         or len (stem) < MINIMUM_STEM_LENGTH
         or ( suffix == "es"
              and not stem . endswith (( "s", "x", "z", "ch", "sh" )) )
         or ( suffix == "s" and stem . endswith ("s") )):
      continue
    word = stem
    break
  return ( word [ : -1 ]
           if word . endswith ("e") and len (word) > MINIMUM_STEM_LENGTH
           else word )
