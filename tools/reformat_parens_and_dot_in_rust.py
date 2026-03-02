#!/usr/bin/env python3
"""
Reformat single-token parenthetical expressions in Rust files.

USAGE:
python3 tools/reformat_parens_and_dot_in_rust.py [folder]

RULES:
- No space inside parens: ( x ) -> (x)
- At least one whitespace outside parens on each side: f(x)g -> f (x) g
- Idempotent.
- Only considers innermost parens.
- Skips string literals, char literals, and comments.
"""

import re
import sys
import os
import glob


def lex_positions(src):
    """
    Yield (kind, start, end) for each region that should be
    skipped (strings, comments, char literals).
    Everything not yielded is "normal code".
    """
    i = 0
    n = len(src)
    while i < n:
        # Raw string: r#"..."#, r##"..."##, etc.
        if src[i] == 'r' and i + 1 < n:
            j = i + 1
            hashes = 0
            while j < n and src[j] == '#':
                hashes += 1
                j += 1
            if j < n and src[j] == '"':
                # This is a raw string
                j += 1  # skip opening "
                closing = '"' + '#' * hashes
                end = src.find(closing, j)
                if end == -1:
                    end = n
                else:
                    end += len(closing)
                yield ('raw_string', i, end)
                i = end
                continue
        # Regular string literal
        if src[i] == '"':
            j = i + 1
            while j < n:
                if src[j] == '\\':
                    j += 2
                elif src[j] == '"':
                    j += 1
                    break
                else:
                    j += 1
            yield ('string', i, j)
            i = j
            continue
        # Character literal
        if src[i] == "'" and i + 1 < n:
            # Distinguish char literals from lifetimes.
            # Char literal: 'x', '\n', '\x41', '\u{1234}'
            # Lifetime: 'a (followed by ident chars, no closing ')
            j = i + 1
            if j < n and src[j] == '\\':
                # Escaped char literal
                j += 1
                while j < n and src[j] != "'":
                    j += 1
                if j < n:
                    j += 1
                yield ('char', i, j)
                i = j
                continue
            elif j < n and j + 1 < n and src[j + 1] == "'":
                # Simple char literal like 'x'
                yield ('char', i, j + 2)
                i = j + 2
                continue
            else:
                # Probably a lifetime, skip the '
                i += 1
                continue
        # Line comment
        if src[i:i+2] == '//':
            end = src.find('\n', i)
            if end == -1:
                end = n
            else:
                end += 1
            yield ('line_comment', i, end)
            i = end
            continue
        # Block comment
        if src[i:i+2] == '/*':
            depth = 1
            j = i + 2
            while j < n and depth > 0:
                if src[j:j+2] == '/*':
                    depth += 1
                    j += 2
                elif src[j:j+2] == '*/':
                    depth -= 1
                    j += 2
                else:
                    j += 1
            yield ('block_comment', i, j)
            i = j
            continue
        i += 1


def build_skip_set(src):
    """Return a set of character positions that are inside
    strings/comments/char literals."""
    skip = set()
    for (kind, start, end) in lex_positions(src):
        for p in range(start, end):
            skip.add(p)
    return skip


def find_innermost_parens(src, skip):
    """
    Find all innermost paren pairs in code (not in strings/comments).
    Returns list of (open_pos, close_pos).
    "Innermost" means: no other code-parens inside.
    """
    # Stack-based approach: push open parens,
    # on close paren, check if contents have any code-parens.
    stack = []
    pairs = []
    for i, ch in enumerate(src):
        if i in skip:
            continue
        if ch == '(':
            stack.append(i)
        elif ch == ')':
            if stack:
                open_pos = stack.pop()
                # Only consider parens on the same line.
                if '\n' not in src[open_pos:i]:
                    pairs.append((open_pos, i))
    # Filter to innermost: a pair is innermost if no other pair
    # is strictly inside it.
    pair_set = set(pairs)
    innermost = []
    for (o, c) in pairs:
        # Check if any other pair is strictly contained
        has_inner = False
        for (o2, c2) in pairs:
            if o2 > o and c2 < c:
                has_inner = True
                break
        if not has_inner:
            innermost.append((o, c))
    return innermost


# What counts as a single token (after trimming whitespace):
# - identifier: [a-zA-Z_][a-zA-Z0-9_:]*  (:: for paths like std::foo)
# - prefixed identifier: (& ?)(mut )? ident  or  (mut )(& ?)ident
# - numeric literal: [0-9][0-9a-fA-Fx_.]*
# - string literal: "..."
# - bool/self/Self
# - a single char literal
# - negative numeric: -[0-9]...
SINGLE_TOKEN_RE = re.compile(
    r'^'
    r'(?:'
    r'&\s*mut\s+&?\s*[a-zA-Z_][a-zA-Z0-9_:]*'  # &mut x, &mut &x
    r'|mut\s+&?\s*[a-zA-Z_][a-zA-Z0-9_:]*'      # mut x, mut &x
    r'|&\s*[a-zA-Z_][a-zA-Z0-9_:]*'              # &x
    r'|\*\s*mut\s+[a-zA-Z_][a-zA-Z0-9_:]*'       # *mut x
    r'|\*\s*const\s+[a-zA-Z_][a-zA-Z0-9_:]*'     # *const x
    r'|\*\s*[a-zA-Z_][a-zA-Z0-9_:]*'             # *x
    r'|[a-zA-Z_][a-zA-Z0-9_:]*'                  # plain identifier
    r'|-?[0-9][0-9a-fA-Fx_.]*'                   # numeric literal
    r'|"(?:[^"\\]|\\.)*"'                         # string literal
    r"|'(?:[^'\\]|\\.)'?"                         # char literal
    r')'
    r'$'
)


def is_single_token(contents):
    """Check if the (trimmed) contents of a paren pair is a single token."""
    trimmed = contents.strip()
    if not trimmed:
        return False
    return SINGLE_TOKEN_RE.match(trimmed) is not None


def reformat_dots(src):
    """Ensure every single dot (not ..) has a space on each side, in code only."""
    skip = build_skip_set(src)
    n = len(src)
    # Collect edit positions right-to-left.
    edits = []
    for i in range(n):
        if src[i] != '.' or i in skip:
            continue
        # Skip if part of .. or ... or ..=
        if (i > 0 and src[i-1] == '.') or (i + 1 < n and src[i+1] == '.'):
            continue
        # Skip float literals: digit.digit
        if (i > 0 and src[i-1].isdigit()) and (i + 1 < n and src[i+1].isdigit()):
            continue
        # Characters that count as "already spaced" on the left:
        # whitespace, open brackets, ?
        prev = src[i-1] if i > 0 else '\n'
        need_before = prev not in (' ', '\t', '\n', '\r', '(', '[', '{', '?')
        # Characters that count as "already spaced" on the right:
        # whitespace, close brackets, ? and )
        nxt = src[i+1] if i + 1 < n else '\n'
        need_after = nxt not in (' ', '\t', '\n', '\r', ')', ']', '}', '?')
        if need_before or need_after:
            edits.append((i, need_before, need_after))
    result = src
    for (pos, before, after) in reversed(edits):
        if after:
            result = result[:pos+1] + ' ' + result[pos+1:]
        if before:
            result = result[:pos] + ' ' + result[pos:]
    return result


def reformat_file(filepath, dry_run=False):
    """Reformat singleton parens and dot spacing in one file."""
    with open(filepath, 'r') as f:
        src = f.read()

    # Pass 1: paren reformatting.
    skip = build_skip_set(src)
    innermost = find_innermost_parens(src, skip)

    # Sort by position descending so replacements don't shift indices.
    innermost.sort(key=lambda p: p[0], reverse=True)

    result = src
    changed = False

    for (o, c) in innermost:
        # Skip visibility modifiers: pub(crate), pub(super), pub(self)
        before_paren = result[:o].rstrip()
        if before_paren.endswith('pub'):
            contents_trimmed = result[o+1:c].strip()
            if contents_trimmed in ('crate', 'super', 'self', 'in'):
                continue

        contents = result[o+1:c]
        if not is_single_token(contents):
            continue

        trimmed = contents.strip()
        new_paren = '(' + trimmed + ')'

        # Determine what to replace: the paren group itself,
        # plus we may need to add spaces outside.
        # Look at char before ( and char after ).
        before_pos = o - 1
        after_pos = c + 1

        need_space_before = False
        if before_pos >= 0:
            ch = result[before_pos]
            # Need space if previous char is not already whitespace
            # and not an open bracket/paren/operator that shouldn't
            # have space after it.
            if ch not in (' ', '\t', '\n', '\r', '(', '[', '{', ',',
                          ';', ':', '!', '#'):
                need_space_before = True

        need_space_after = False
        if after_pos < len(result):
            ch = result[after_pos]
            if ch not in (' ', '\t', '\n', '\r', ')', ']', '}', ',',
                          ';', ':', '.', '?', '!'):
                need_space_after = True

        replacement = ''
        if need_space_before:
            replacement += ' '
        replacement += new_paren
        if need_space_after:
            replacement += ' '

        old = result[o:c+1]
        if need_space_before:
            # Check we're not doubling a space
            pass
        if need_space_after:
            pass

        new_fragment = replacement
        old_start = o
        old_end = c + 1

        if result[old_start:old_end] != old:
            continue  # safety check

        new_result = result[:old_start] + new_fragment + result[old_end:]
        if new_result != result:
            changed = True
            result = new_result

    # Pass 2: dot spacing (rebuild skip set since positions shifted).
    result = reformat_dots(result)

    changed = result != src
    if changed and not dry_run:
        with open(filepath, 'w') as f:
            f.write(result)

    return changed, src, result


def main():
    import argparse
    parser = argparse.ArgumentParser(
        description='Reformat single-token parentheticals in Rust files.')
    parser.add_argument('paths', nargs='*', default=['.'],
                        help='Files or directories to process')
    parser.add_argument('--dry-run', action='store_true',
                        help='Show what would change without writing')
    parser.add_argument('--diff', action='store_true',
                        help='Show unified diff of changes')
    args = parser.parse_args()

    files = []
    for path in args.paths:
        if os.path.isfile(path):
            files.append(path)
        elif os.path.isdir(path):
            for f in glob.glob(os.path.join(path, '**', '*.rs'),
                               recursive=True):
                files.append(f)

    total_changed = 0
    for filepath in sorted(files):
        changed, old, new = reformat_file(filepath, dry_run=args.dry_run)
        if changed:
            total_changed += 1
            print(f"{'Would change' if args.dry_run else 'Changed'}: {filepath}")
            if args.diff:
                import difflib
                diff = difflib.unified_diff(
                    old.splitlines(keepends=True),
                    new.splitlines(keepends=True),
                    fromfile=filepath,
                    tofile=filepath)
                sys.stdout.writelines(diff)

    print(f"\n{total_changed} file(s) {'would be changed' if args.dry_run else 'changed'}.")


if __name__ == '__main__':
    main()
