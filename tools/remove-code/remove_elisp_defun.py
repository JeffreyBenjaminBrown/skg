#!/usr/bin/env python3
"""Remove a top-level elisp defun/defsubst/defmacro/cl-defun by name,
paren-matched, with string/char/comment awareness, including its
leading contiguous comment block and one trailing blank line.

Usage: python3 tools/remove_elisp_defun.py --from-list listfile
       list lines: FILE<TAB>SYMBOL
"""
import re, sys

def scan_close(text, start):
  """From the '(' at `start`, return index of the matching ')'. """
  i = start; n = len(text); depth = 0
  in_str = in_comment = False
  while i < n:
    c = text[i]
    if in_comment:
      if c == '\n': in_comment = False
    elif in_str:
      if c == '\\': i += 2; continue
      if c == '"': in_str = False
    else:
      if c == ';': in_comment = True
      elif c == '"': in_str = True
      elif c == '?':            # char literal: ?x or ?\x
        i += 3 if text[i+1:i+2] == '\\' else 2; continue
      elif c == '(': depth += 1
      elif c == ')':
        depth -= 1
        if depth == 0: return i
    i += 1
  return None

def main():
  assert sys.argv[1] == '--from-list'
  targets = []
  for ln in open(sys.argv[2]):
    ln = ln.rstrip('\n')
    if not ln.strip() or ln.lstrip().startswith('#'): continue
    f, sym = ln.split('\t'); targets.append((f, sym.strip()))
  byfile = {}
  for f, sym in targets: byfile.setdefault(f, []).append(sym)
  rc = 0
  for f, syms in byfile.items():
    text = open(f).read()
    lines = text.split('\n')
    loff = []; acc = 0
    for l in lines: loff.append(acc); acc += len(l) + 1
    spans = []
    for sym in syms:
      pat = re.compile(r'^\((?:defun|defsubst|defmacro|cl-defun|cl-defmacro)\s+'
                       + re.escape(sym) + r'(?![A-Za-z0-9_/:+<>=*!?-])')
      hits = [i for i, l in enumerate(lines) if pat.match(l)]
      if len(hits) != 1:
        print(f"ERROR {f}: {sym!r} matched {len(hits)} defs", file=sys.stderr); rc = 1; continue
      s = hits[0]
      start_char = loff[s]
      close = scan_close(text, start_char)
      if close is None:
        print(f"ERROR {f}: unbalanced parens for {sym!r}", file=sys.stderr); rc = 1; continue
      # back up over leading contiguous comment lines
      b = s
      while b-1 >= 0 and lines[b-1].lstrip().startswith(';'):
        b -= 1
      start_char = loff[b]
      end_char = close + 1
      # swallow trailing newline(s): drop through end of the close line
      while end_char < len(text) and text[end_char] != '\n': end_char += 1
      end_char += 1  # the newline
      if end_char < len(text) and text[end_char] == '\n': end_char += 1  # one blank line
      spans.append((start_char, end_char, sym))
    for st, en, sym in sorted(spans, reverse=True):
      snip = text[st:en]
      print(f"{f}: remove {sym!r}: |{snip.strip().splitlines()[0][:60]} ...|")
      text = text[:st] + text[en:]
    open(f, 'w').write(text)
  sys.exit(rc)

if __name__ == '__main__':
  main()
