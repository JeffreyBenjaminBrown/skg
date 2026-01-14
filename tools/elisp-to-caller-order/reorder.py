#!/usr/bin/env python3
"""
Reorder elisp definitions so that callers precede callees.

Usage:
    python reorder.py <file.el>           # Print reordered content to stdout
    python reorder.py <file.el> --inplace # Modify file in place
    python reorder.py <directory> --inplace # Process all .el files in directory
    python reorder.py <file.el> --debug   # Show debug info about definitions and calls
"""

import re
import sys
import os
from dataclasses import dataclass, field
from typing import Optional
from collections import defaultdict


@dataclass
class Definition:
    """A single elisp definition (defun, defvar, defconst, etc.)"""
    name: str
    kind: str  # 'defun', 'defmacro', 'defvar', 'defconst', 'defface', 'define-minor-mode', etc.
    sexp_text: str  # Just the sexp itself
    prefix_text: str  # Comments/whitespace before this definition
    calls: set = field(default_factory=set)  # Names of functions/macros this definition calls


def find_matching_paren(text: str, start: int) -> int:
    """
    Find the position of the closing paren that matches the opening paren at `start`.
    Handles nested parens, strings, and comments.
    Returns the index of the closing paren, or -1 if not found.
    """
    if start >= len(text) or text[start] != '(':
        return -1

    depth = 0
    i = start
    in_string = False
    in_comment = False

    while i < len(text):
        c = text[i]

        if in_comment:
            if c == '\n':
                in_comment = False
            i += 1
            continue

        if in_string:
            if c == '\\' and i + 1 < len(text):
                i += 2  # Skip escaped character
                continue
            if c == '"':
                in_string = False
            i += 1
            continue

        if c == ';':
            in_comment = True
            i += 1
            continue

        if c == '"':
            in_string = True
            i += 1
            continue

        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
            if depth == 0:
                return i

        i += 1

    return -1


def extract_definitions(content: str) -> tuple[str, list[Definition], str]:
    """
    Parse elisp content and extract:
    - header: everything before the first definition
    - definitions: list of Definition objects
    - footer: everything after the last definition (typically the provide statement)

    Returns (header, definitions, footer)
    """
    # Pattern to match the start of a top-level definition
    # Only match definitions at column 0 (no leading whitespace)
    # to avoid matching nested definitions inside other sexps
    def_pattern = re.compile(
        r'^\((?:defun|defmacro|defvar|defconst|defface|'
        r'define-minor-mode|define-derived-mode|cl-defun|cl-defmacro)\s+',
        re.MULTILINE
    )

    # Find all definition starts
    matches = list(def_pattern.finditer(content))

    if not matches:
        return content, [], ""

    # First, find the sexp boundaries for each definition
    sexp_boundaries = []  # List of (sexp_start, sexp_end) tuples

    for match in matches:
        # Find the opening paren
        sexp_start = match.start()
        while sexp_start < len(content) and content[sexp_start] != '(':
            sexp_start += 1

        if sexp_start >= len(content):
            continue

        sexp_end = find_matching_paren(content, sexp_start)
        if sexp_end == -1:
            continue

        sexp_boundaries.append((sexp_start, sexp_end + 1))

    if not sexp_boundaries:
        return content, [], ""

    # Now extract definitions with their prefixes
    definitions = []

    for idx, (sexp_start, sexp_end) in enumerate(sexp_boundaries):
        # Prefix is everything from end of previous sexp (or start of content) to start of this sexp
        if idx == 0:
            prefix_start = 0
        else:
            prefix_start = sexp_boundaries[idx - 1][1]

        prefix_text = content[prefix_start:sexp_start]
        sexp_text = content[sexp_start:sexp_end]

        # Extract definition info
        kind, name = extract_def_info(sexp_text)

        if name:
            defn = Definition(
                name=name,
                kind=kind,
                sexp_text=sexp_text,
                prefix_text=prefix_text
            )
            defn.calls = extract_calls(sexp_text)
            definitions.append(defn)

    if not definitions:
        return content, [], ""

    # Header is the prefix of the first definition
    header = definitions[0].prefix_text
    definitions[0].prefix_text = ""

    # Footer is everything after the last sexp
    footer = content[sexp_boundaries[-1][1]:]

    return header, definitions, footer


def extract_def_info(sexp: str) -> tuple[str, str]:
    """
    Extract the kind and name from a definition sexp.
    Returns (kind, name) tuple.
    """
    match = re.match(
        r'\s*\((defun|defmacro|defvar|defconst|defface|'
        r'define-minor-mode|define-derived-mode|cl-defun|cl-defmacro)\s+'
        r'([^\s()]+)',
        sexp
    )
    if match:
        return match.group(1), match.group(2)
    return "", ""


def extract_calls(sexp: str) -> set[str]:
    """
    Extract all function/macro calls from a definition body.
    Returns a set of called names.
    """
    calls = set()

    # Remove strings and comments first to avoid false positives
    cleaned = remove_strings_and_comments(sexp)

    # Find all (symbol patterns
    for match in re.finditer(r'\(([a-zA-Z][a-zA-Z0-9_*/-]*)', cleaned):
        symbol = match.group(1)
        # Filter out common non-function forms
        if symbol not in ('defun', 'defmacro', 'defvar', 'defconst', 'defface',
                          'define-minor-mode', 'define-derived-mode',
                          'lambda', 'let', 'let*', 'if', 'when', 'unless',
                          'cond', 'case', 'pcase', 'cl-case', 'cl-loop',
                          'progn', 'prog1', 'prog2', 'save-excursion',
                          'save-restriction', 'save-match-data',
                          'with-current-buffer', 'with-temp-buffer',
                          'condition-case', 'unwind-protect', 'ignore-errors',
                          'catch', 'throw', 'while', 'dolist', 'dotimes',
                          'cl-defun', 'cl-defmacro', 'cl-labels',
                          'cl-flet', 'cl-flet*', 'cl-letf', 'cl-letf*',
                          'and', 'or', 'not', 'null', 'quote', 'function',
                          'setq', 'setf', 'push', 'pop', 'incf', 'decf',
                          'interactive', 'declare'):
            calls.add(symbol)

    return calls


def remove_strings_and_comments(text: str) -> str:
    """Remove string literals and comments from elisp text."""
    result = []
    i = 0
    in_string = False

    while i < len(text):
        c = text[i]

        if in_string:
            if c == '\\' and i + 1 < len(text):
                i += 2
                continue
            if c == '"':
                in_string = False
                result.append(' ')  # Replace string with space
            i += 1
            continue

        if c == ';':
            # Skip to end of line
            while i < len(text) and text[i] != '\n':
                i += 1
            continue

        if c == '"':
            in_string = True
            i += 1
            continue

        result.append(c)
        i += 1

    return ''.join(result)


def build_call_graph(definitions: list[Definition]) -> dict[str, set[str]]:
    """
    Build a call graph from definitions.
    Returns a dict mapping each definition name to the set of definitions it calls.
    Only includes calls to other definitions in the same file.
    """
    defined_names = {d.name for d in definitions}
    graph = {}

    for defn in definitions:
        # Filter calls to only include names defined in this file
        local_calls = defn.calls & defined_names
        graph[defn.name] = local_calls

    return graph


def topological_sort_callers_first(definitions: list[Definition],
                                    call_graph: dict[str, set[str]]) -> list[Definition]:
    """
    Sort definitions so that callers come before callees.

    If A calls B, then A should appear before B.

    Uses Kahn's algorithm with stability (preserving original order for ties).
    """
    name_to_def = {d.name: d for d in definitions}
    name_to_idx = {d.name: i for i, d in enumerate(definitions)}

    # Build in-degree (how many definitions call this one)
    in_degree = {d.name: 0 for d in definitions}
    for caller, callees in call_graph.items():
        for callee in callees:
            if callee in in_degree:
                in_degree[callee] += 1

    # Start with definitions that no one calls (in-degree 0)
    # Sort by original position for stability
    queue = sorted(
        [name for name, deg in in_degree.items() if deg == 0],
        key=lambda n: name_to_idx[n]
    )

    result_names = []
    visited = set()

    while queue:
        name = queue.pop(0)
        if name in visited:
            continue
        visited.add(name)
        result_names.append(name)

        # Decrease in-degree of callees
        for callee in call_graph.get(name, set()):
            if callee in in_degree:
                in_degree[callee] -= 1
                if in_degree[callee] == 0 and callee not in visited:
                    # Insert in sorted order by original position
                    inserted = False
                    for i, q_name in enumerate(queue):
                        if name_to_idx[callee] < name_to_idx[q_name]:
                            queue.insert(i, callee)
                            inserted = True
                            break
                    if not inserted:
                        queue.append(callee)

    # Handle cycles - any remaining definitions
    for defn in definitions:
        if defn.name not in visited:
            result_names.append(defn.name)

    return [name_to_def[name] for name in result_names]


def reorder_file(content: str) -> str:
    """
    Reorder an elisp file so callers precede callees.
    Returns the reordered content.
    """
    header, definitions, footer = extract_definitions(content)

    if not definitions:
        return content

    # Build call graph
    call_graph = build_call_graph(definitions)

    # Sort definitions
    sorted_defs = topological_sort_callers_first(definitions, call_graph)

    # Reconstruct file
    # The first definition loses its prefix (which becomes the header)
    # Other definitions keep their prefixes (comments/whitespace between defs)

    result = header

    for i, defn in enumerate(sorted_defs):
        if i == 0:
            # First definition - no prefix needed (header already added)
            result += defn.sexp_text
        else:
            # Use the prefix from this definition
            # But we need to ensure there's at least a newline separator
            if defn.prefix_text:
                result += defn.prefix_text
            else:
                result += "\n\n"
            result += defn.sexp_text

    result += footer

    return result


def debug_file(filepath: str):
    """Show debug info about definitions and their calls."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    header, definitions, footer = extract_definitions(content)

    print(f"=== {filepath} ===")
    print(f"Header length: {len(header)}")
    print(f"Footer length: {len(footer)}")
    print(f"Definitions: {len(definitions)}")
    print()

    for defn in definitions:
        print(f"  {defn.kind} {defn.name}")
        local_calls = defn.calls & {d.name for d in definitions}
        if local_calls:
            print(f"    calls (local): {sorted(local_calls)}")
        print()

    # Build and display call graph
    call_graph = build_call_graph(definitions)
    print("Call graph (local calls only):")
    for caller, callees in call_graph.items():
        if callees:
            print(f"  {caller} -> {sorted(callees)}")

    print()
    print("Sorted order (callers first):")
    sorted_defs = topological_sort_callers_first(definitions, call_graph)
    for i, defn in enumerate(sorted_defs, 1):
        print(f"  {i}. {defn.name}")


def process_file(filepath: str, inplace: bool = False) -> Optional[str]:
    """
    Process a single file.
    If inplace is True, modifies the file.
    Otherwise, returns the reordered content.
    """
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    reordered = reorder_file(content)

    if inplace:
        if reordered != content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(reordered)
            print(f"Reordered: {filepath}")
        else:
            print(f"No changes: {filepath}")
        return None
    else:
        return reordered


def find_el_files(path: str) -> list[str]:
    """Find all .el files in path (recursively if directory)."""
    if os.path.isfile(path):
        return [path] if path.endswith('.el') else []

    result = []
    for root, dirs, files in os.walk(path):
        for filename in sorted(files):
            if filename.endswith('.el'):
                result.append(os.path.join(root, filename))
    return sorted(result)


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    path = sys.argv[1]
    inplace = '--inplace' in sys.argv
    debug = '--debug' in sys.argv

    el_files = find_el_files(path)

    if debug:
        for filepath in el_files:
            debug_file(filepath)
        return

    if os.path.isdir(path):
        # Process all .el files in directory (recursively)
        for filepath in el_files:
            process_file(filepath, inplace=True)
    else:
        result = process_file(path, inplace=inplace)
        if result is not None:
            print(result)


if __name__ == '__main__':
    main()
