#!/usr/bin/env python3
"""Remove a Rust function (by file + name) including its leading
doc-comments / attributes, character-precisely: if the function's
closing brace shares its line with enclosing braces (the cuddled
`) } }` idiom), those enclosing braces are preserved.

Brace/string/char/comment aware so delimiters inside literals or
comments don't fool the scanner.

Usage:
  python3 tools/remove_fn.py --from-list listfile
    list lines: FILE<TAB>FN_NAME            (must be unique in file)
                FILE<TAB>FN_NAME<TAB>ALL    (remove every match)
                FILE<TAB>FN_NAME<TAB>3      (remove the 3rd, 1-based)

Prints each removal's first/last source line for eyeballing.
"""
import re, sys

SIG = lambda fn: re.compile(
  r'^(\s*)(pub(\s*\([^)]*\))?\s+)?(async\s+)?(unsafe\s+)?(extern\s+"[^"]*"\s+)?fn\s+'
  + re.escape(fn) + r'\b')

def scan_close(text, start):
  """From index `start` (at/just before the signature), find the index
  of the function body's matching closing brace. Returns that index."""
  i = start; n = len(text)
  depth = 0; opened = False
  in_line = in_block = in_str = in_char = False
  while i < n:
    c = text[i]; two = text[i:i+2]
    if in_line:
      if c == '\n': in_line = False
    elif in_block:
      if two == '*/': in_block = False; i += 2; continue
    elif in_str:
      if c == '\\': i += 2; continue
      if c == '"': in_str = False
    elif in_char:
      if c == '\\': i += 2; continue
      if c == "'": in_char = False
    else:
      if two == '//': in_line = True; i += 2; continue
      if two == '/*': in_block = True; i += 2; continue
      if c == '"': in_str = True
      elif c == "'":
        if re.match(r"'(\\.|[^'])'", text[i:i+4]) or text[i:i+2] == "'\\":
          in_char = True
      elif c == '{':
        depth += 1; opened = True
      elif c == '}':
        depth -= 1
        if opened and depth == 0:
          return i
    i += 1
  return None

def line_starts(text):
  offs = [0]
  for ch in text:
    offs.append(offs[-1] + 1)
  return offs

def find_spans(text, fn):
  lines = text.split('\n')
  # offset of the start of each line
  loff = []; acc = 0
  for l in lines:
    loff.append(acc); acc += len(l) + 1
  sig = SIG(fn)
  spans = []
  for s, l in enumerate(lines):
    if not sig.match(l): continue
    b = s
    while b-1 >= 0 and lines[b-1].lstrip()[:3] and \
          lines[b-1].lstrip().startswith(('#[', '#![', '///', '//!', '//')):
      b -= 1
    start_char = loff[b]
    close = scan_close(text, loff[s])
    if close is None: continue
    spans.append((start_char, close + 1))  # end is just past the fn's own '}'
  return spans

def remove_spans(text, spans):
  for start, end in sorted(spans, reverse=True):
    left = text[:start]; right = text[end:]
    # Strip leading spaces/tabs before a cuddled enclosing brace so it
    # lands at column 0 (top-level impl close) rather than ` }`.
    m = re.match(r'[ \t]+(?=\})', right)
    if m: right = right[m.end():]
    # Collapse the blank line the deletion would otherwise insert.
    if right.startswith('\n') and left.endswith('\n'):
      right = right[1:]
    text = left + right
  return text

def main():
  args = sys.argv[1:]
  assert args[0] == '--from-list'
  targets = []
  for ln in open(args[1]):
    ln = ln.rstrip('\n')
    if not ln.strip() or ln.lstrip().startswith('#'): continue
    parts = ln.split('\t')
    f, fn = parts[0], parts[1].strip()
    which = parts[2].strip() if len(parts) > 2 else None
    targets.append((f, fn, which))
  byfile = {}
  for f, fn, which in targets:
    byfile.setdefault(f, []).append((fn, which))
  rc = 0
  for f, fns in byfile.items():
    text = open(f).read()
    chosen = []
    for fn, which in fns:
      spans = find_spans(text, fn)
      if not spans:
        print(f"ERROR {f}: no def of {fn!r}", file=sys.stderr); rc = 1; continue
      if which == 'ALL':
        pick = spans
      elif which is None:
        if len(spans) != 1:
          print(f"ERROR {f}: {fn!r} has {len(spans)} defs; specify ALL or index", file=sys.stderr); rc = 1; continue
        pick = spans
      else:
        idx = int(which) - 1
        pick = [spans[idx]]
      for st, en in pick:
        snippet = text[st:en]
        first = snippet.split('\n', 1)[0]
        last = snippet.rsplit('\n', 1)[-1]
        print(f"{f}: remove {fn!r}: |{first.strip()[:60]} ... {last.strip()[-40:]}|")
        chosen.append((st, en))
    if chosen:
      text = remove_spans(text, chosen)
      open(f, 'w').write(text)
  sys.exit(rc)

if __name__ == '__main__':
  main()
