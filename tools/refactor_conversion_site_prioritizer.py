#!/usr/bin/env python3
"""
PURPOSE:
This code should be useful for any refactor in which
lots of version-1 functions and version-2 functions are copresent.
Viewing thoes version-1 functions
that need replacement in the call graph,
it is likely that either the
most rootward or the most leafward ones
are easiest to convert next.

HOW IT WORKS:
For each unconverted v2 function, this counts:
- Unconverted "ancestors" - functions that call it
- Unconverted "descendants" - functions it calls

HOW TO USE IT:
Interpreting the results something like this:
- 0 ancestors, 0 descendants = Isolated. Safe to convert.
- 0 ancestors, N descendants = Root-level. Maybe easy.
- N ancestors, 0 descendants = Leaf.       Maybe easy.

DATA REQUIREMENTS:
v2 functions must end in the string literal '_v2'.
  TODO: Currently the code also accepts _in_orgtree,
  but that should go away.

Needs an up-to-date call graph.
  This can be made from the root of the project by calling
  cargo run -p call-graph --bin call-graph-regen
  It should show both the v1 and the v2 functions.

Needs a 'functions.org' file.
For an examplem, see refactor-ongoing/functions.org from commit
1ba033135209edf778107666f56052936e2d70b1
It needs to be kept up to date --
anything converted should be marked DONE.
functions.org should follow the convention:
  ** (level 2 headline)  for v2/new functions
  *** (level 3 headline) for v1/old functions
PITFALL: So far, creating and maintaining functions.org is manual work.

Q: What if there's {name}_v2 and no corresponding {name}?
A: This is fine! Some functions were created new for v2 architecture with no v1
   counterpart (like many _in_orgtree functions). These should be marked DONE
   immediately in functions.org since there's no cleanup needed - just document them
   under the "new-functions-without-correspondences" section.
"""

import re
from collections import defaultdict
from pathlib import Path


def parse_functions_org(path):
  """Parse functions.org to get unconverted v2 functions and their v1 names."""
  unconverted_v2 = {}  # v2_name -> v1_name
  converted = set()

  with open(path) as f:
    lines = f.readlines()

  i = 0
  while i < len(lines):
    line = lines[i].rstrip()

    # Check for v2 function entry
    if line.startswith('** '):
      v2_done = 'DONE' in line
      v2_name = line.replace('** ', '').replace('DONE ', '').strip()

      # Look ahead for the v1 name (*** line)
      # Search until next ** entry
      v1_name = None
      v1_done = False
      j = i + 1
      while j < len(lines):
        if lines[j].startswith('** '):  # Next entry
          break
        if lines[j].startswith('*** '):
          v1_line = lines[j]
          v1_done = 'DONE' in v1_line
          v1_name = v1_line.replace('*** ', '').replace('DONE ', '').strip()
          break
        j += 1

      # Function is converted if EITHER v2 or v1 has DONE
      is_converted = v2_done or v1_done

      if is_converted:
        # Mark both names as converted
        converted.add(v2_name)
        if v1_name:
          converted.add(v1_name)
      else:
        # Not converted yet
        if v1_name:
          unconverted_v2[v2_name] = v1_name

    i += 1

  return unconverted_v2, converted


def parse_call_graph(path):
  """Parse call graph to build caller/callee relationships."""
  callers = defaultdict(set)  # function -> set of functions that call it
  callees = defaultdict(set)  # function -> set of functions it calls

  with open(path) as f:
    stack = []  # Stack of (depth, function_name)

    for line in f:
      line = line.rstrip()
      if not line:
        continue

      # Count stars to get depth
      depth = 0
      while depth < len(line) and line[depth] == '*':
        depth += 1

      # Extract function name (skip "DUP " prefix)
      func_name = line[depth:].strip()
      if func_name.startswith('DUP '):
        func_name = func_name[4:]

      # Pop stack to current depth
      while stack and stack[-1][0] >= depth:
        stack.pop()

      # If there's a parent, record the relationship
      if stack:
        parent_func = stack[-1][1]
        callers[func_name].add(parent_func)
        callees[parent_func].add(func_name)

      # Push current function
      stack.append((depth, func_name))

  return callers, callees


def normalize_name(name):
  """Remove _v2 or _in_orgtree suffix to get base name."""
  name = name.replace('_v2', '')
  name = name.replace('_in_orgtree', '')
  return name


def count_unconverted_relatives(func_name, relatives, unconverted_v2, converted):
  """Count how many relatives (callers or callees) are unconverted."""
  count = 0
  for rel in relatives.get(func_name, set()):
    base_rel = normalize_name(rel)

    # Check if this relative is unconverted
    # It's unconverted if:
    # - It's in unconverted_v2 (as v2 name)
    # - OR it's a v1 name that hasn't been converted yet

    if rel in unconverted_v2:  # It's an unconverted v2 function
      count += 1
    elif base_rel not in converted and rel.endswith(('_v2', '_in_orgtree')):
      # Has v2 suffix but not in our list - might be unconverted
      count += 1
    elif not rel.endswith(('_v2', '_in_orgtree')) and base_rel not in converted:
      # v1 function that hasn't been converted
      count += 1

  return count


def main():
  functions_org = Path('/home/ubuntu/refactor-ongoing/functions.org')
  call_graph = Path('/home/ubuntu/tools/introspect/call-graph/output/main.org')

  unconverted_v2, converted = parse_functions_org(functions_org)
  callers, callees = parse_call_graph(call_graph)

  print(f"Found {len(unconverted_v2)} unconverted v2 functions")
  print(f"Found {len(converted)} converted functions")
  print()

  # Analyze each unconverted function
  results = []
  for v2_name, v1_name in unconverted_v2.items():
    # Check both v1 and v2 names in the call graph
    ancestors_v1 = count_unconverted_relatives(v1_name, callers, unconverted_v2, converted)
    ancestors_v2 = count_unconverted_relatives(v2_name, callers, unconverted_v2, converted)

    descendants_v1 = count_unconverted_relatives(v1_name, callees, unconverted_v2, converted)
    descendants_v2 = count_unconverted_relatives(v2_name, callees, unconverted_v2, converted)

    ancestors = max(ancestors_v1, ancestors_v2)
    descendants = max(descendants_v1, descendants_v2)

    results.append((v2_name, v1_name, ancestors, descendants))

  # Sort by priority:
  # 1. Functions with 0 ancestors (can delete v1, rename v2)
  # 2. Within that, prefer fewer descendants
  results.sort(key=lambda x: (x[2], x[3]))

  # Print results
  print("=" * 100)
  print("PRIORITY LIST: Unconverted v2 Functions")
  print("=" * 100)
  print(f"{'v2 Function':<50} {'Ancestors':<12} {'Descendants':<12} {'Strategy'}")
  print("-" * 100)

  for v2_name, v1_name, ancestors, descendants in results:
    if ancestors == 0 and descendants == 0:
      strategy = "✓ READY: Isolated"
    elif ancestors == 0:
      strategy = f"✓ READY: Delete v1, rename v2"
    elif descendants == 0:
      strategy = "Leaf (safe)"
    else:
      strategy = "Blocked"
    print(f"{v2_name:<50} {ancestors:<12} {descendants:<12} {strategy}")


if __name__ == '__main__':
  main()
