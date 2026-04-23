"""
Helpers for parsing and emitting .skg YAML files in skg's on-disk
format. Used by 05_build_diff.py (see README.org) to do minimal
text-level edits on existing files rather than full parse + re-emit
(which would produce cosmetically large diffs).

The format is a flat top-level mapping with a fixed field order
(see FIELD_ORDER below); each top-level key starts a line with no
indent, and its value runs until the next top-level key or EOF. This
invariant lets parse_fields carve the file into field spans purely
by scanning for keys at column 0.

The ONE wrinkle is body as a YAML block literal:
  body: |-
    line 1
    line 2
  body: |2-       <-- skg's current form (explicit indent indicator)
    line 1
Block-literal lines are indented, so we have to recognize when a
top-level key opens a block and skip its indented continuation lines
until the next column-0 key.

Single-line bodies (quoted or unquoted) have no continuation, so
they're just one line like any other scalar field.

The on-disk format is defined (and must stay consistent with) skg's
Rust code at:
  server/types/nodes/fs.rs         NodeFS::to_yaml + block_scalar_for_body
If that code changes (new field, different body rendering), this
file must track the change to keep diffs minimal. Correctness (can
skg still load the file?) is independent of that — skg always
accepts every YAML scalar style.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

KEY_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*):(?: |$)")

# Field order skg emits. Use this when synthesizing new files.
FIELD_ORDER = [
    "title",
    "aliases",
    "pid",
    "extra_ids",
    "body",
    "contains",
    "subscribes_to",
    "hides_from_its_subscriptions",
    "overrides_view_of",
    "misc",
]


@dataclass
class FieldSpan:
    key: str
    start: int  # inclusive, 0-based line number
    end: int    # exclusive
    lines: list[str]  # the lines as they appear in the file


def parse_fields(text: str) -> list[FieldSpan]:
    """Split the file into top-level field spans."""
    lines = text.splitlines(keepends=True)
    spans: list[FieldSpan] = []
    # Identify line numbers that start top-level keys.
    key_lines: list[int] = []
    i = 0
    in_block = False
    block_end_line = -1
    while i < len(lines):
        line = lines[i]
        if in_block:
            # A block-literal value continues until a line that is not
            # blank and not indented deeper than the block start. We
            # approximate by requiring indent >= 1 space or blank line.
            if line.strip() == "":
                i += 1
                continue
            # If the line starts a new top-level key, the block ends.
            m = KEY_RE.match(line)
            if m and not line.startswith(" ") and not line.startswith("\t"):
                in_block = False
                # fall through to handle this line
            else:
                i += 1
                continue
        m = KEY_RE.match(line)
        if m and not line.startswith(" ") and not line.startswith("\t"):
            key_lines.append(i)
            # Detect block-literal body (e.g. "body: |" or "body: |2-")
            rest = line[m.end():].rstrip("\n")
            if rest.strip().startswith("|"):
                in_block = True
        i += 1
    key_lines.append(len(lines))
    for idx in range(len(key_lines) - 1):
        start = key_lines[idx]
        end = key_lines[idx + 1]
        key = KEY_RE.match(lines[start]).group(1)
        spans.append(FieldSpan(key=key, start=start, end=end,
                               lines=lines[start:end]))
    return spans


def span_text(span: FieldSpan) -> str:
    return "".join(span.lines)


def emit_body_block(body: str) -> str:
    """Emit 'body:' as a block literal, matching skg's fs::block_scalar_for_body.

    Returns the full 'body: |2…\\n  line\\n  line\\n' block, ending with a
    newline. If the body ends with \\n it's `|2`, else `|2-`.
    """
    ends_nl = body.endswith("\n")
    inner = body[:-1] if ends_nl else body
    chomp = "" if ends_nl else "-"
    out = f"body: |2{chomp}\n"
    for line in inner.split("\n"):
        if line == "":
            out += "\n"
        else:
            out += "  " + line + "\n"
    return out


def emit_body(body: str) -> str:
    """Emit 'body: ...' in the form skg would — block literal if possible,
    else single-line (quoted if necessary).

    Mirrors NodeFS::to_yaml's branch: try block; if it doesn't round-trip
    (rare — CR or NUL), fall back to the default yaml serializer.
    """
    # Most strings can be block-emitted. The round-trip guard in skg rejects
    # CR and NUL. For simplicity, we match that.
    if "\r" in body or "\x00" in body:
        import yaml  # noqa: PLC0415
        return "body: " + yaml.safe_dump(body, default_style='"', default_flow_style=False).rstrip() + "\n"
    return emit_body_block(body)


def list_items_of_span(span: FieldSpan) -> list[str]:
    """For a list-valued field, return the UUID/text of each list item, in
    order."""
    out: list[str] = []
    for line in span.lines[1:]:
        stripped = line.rstrip("\n")
        if stripped.startswith("- "):
            out.append(stripped[2:])
    return out


def emit_list_field(key: str, items: list[str]) -> str:
    """Emit a list field. Always uses '- item' per line. If empty, returns ''
    (skg skips empty lists)."""
    if not items:
        return ""
    out = f"{key}:\n"
    for item in items:
        out += f"- {item}\n"
    return out


def find_span(spans: list[FieldSpan], key: str) -> FieldSpan | None:
    for s in spans:
        if s.key == key:
            return s
    return None


def replace_span(text: str, old: FieldSpan, new_text: str) -> str:
    """Replace 'old' span's lines in 'text' with 'new_text' (which should
    end with a newline if non-empty)."""
    lines = text.splitlines(keepends=True)
    return "".join(lines[: old.start]) + new_text + "".join(lines[old.end :])


def insert_span_before(text: str, spans: list[FieldSpan], before_key: str,
                       new_text: str) -> str:
    """Insert new_text before the span with key=before_key. If no such span
    exists, append at end."""
    if not new_text:
        return text
    target = find_span(spans, before_key)
    lines = text.splitlines(keepends=True)
    if target is None:
        # Append.
        if lines and not lines[-1].endswith("\n"):
            lines.append("\n")
        return "".join(lines) + new_text
    return "".join(lines[: target.start]) + new_text + "".join(lines[target.start :])


def insert_span_after(text: str, spans: list[FieldSpan], after_key: str,
                      new_text: str) -> str:
    if not new_text:
        return text
    target = find_span(spans, after_key)
    lines = text.splitlines(keepends=True)
    if target is None:
        if lines and not lines[-1].endswith("\n"):
            lines.append("\n")
        return "".join(lines) + new_text
    return "".join(lines[: target.end]) + new_text + "".join(lines[target.end :])
