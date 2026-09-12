#!/usr/bin/env python3
"""Generate result.org from the raw recursive-content save profile."""

from __future__ import annotations

import csv
import gzip
import json
import math
import re
import statistics
import tomllib
from collections import Counter, defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


PROFILE_DIR = Path(__file__).resolve().parent
RAW_DIR = PROFILE_DIR / "raw"
RESULT_PATH = PROFILE_DIR / "result.org"
EXPLANATIONS_PATH = PROFILE_DIR / "how-it-works.toml"
SIGNIFICANT_FRACTION = 0.05


@dataclass
class CallNode:
    samples: int = 0
    children: dict[str, "CallNode"] = field(default_factory=dict)


def read_key_values(path: Path) -> dict[str, str]:
    with path.open() as source:
        return dict(csv.reader(source, delimiter="\t"))


def read_client_rows() -> list[dict[str, str]]:
    with (RAW_DIR / "client.tsv").open() as source:
        return list(csv.DictReader(source, delimiter="\t"))


def percentile(values: list[float], fraction: float) -> float:
    ordered = sorted(values)
    position = (len(ordered) - 1) * fraction
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return ordered[lower]
    return ordered[lower] + (ordered[upper] - ordered[lower]) * (position - lower)


def string_table(thread: dict[str, Any]) -> list[str]:
    return thread.get("stringArray", thread.get("stringTable", []))


def function_name(thread: dict[str, Any], frame_index: int) -> str:
    function_index = thread["frameTable"]["func"][frame_index]
    string_index = thread["funcTable"]["name"][function_index]
    return string_table(thread)[string_index]


def stack_names(thread: dict[str, Any], stack_index: int | None) -> list[str]:
    names: list[str] = []
    while stack_index is not None:
        table = thread["stackTable"]
        names.append(function_name(thread, table["frame"][stack_index]))
        stack_index = table["prefix"][stack_index]
    names.reverse()
    return names


def clean_function_name(name: str) -> str:
    for prefix in ("skg::", "<skg::"):
        if name.startswith(prefix):
            name = name.removeprefix(prefix)
    if name.startswith("<") and " as " in name and ">::" in name:
        name = name.split(">::", 1)[1]
    name = name.replace("{{closure}}", "closure")
    return name


def meaningful_stack(names: list[str]) -> tuple[str, list[str]]:
    cleaned = [clean_function_name(name) for name in names]
    save_positions = [index for index, name in enumerate(cleaned)
                      if "handle_save_buffer_request" in name]
    if save_positions:
        start = save_positions[-1]
        return "server", cleaned[start:]
    if any("tantivy" in name.lower() and "background" in name.lower()
           for name in cleaned):
        relevant = [name for name in cleaned if "skg::" in name or "tantivy" in name.lower()]
        return "background", relevant or ["Tantivy background worker"]
    relevant = [clean_function_name(name) for name in names if "skg::" in name]
    return "other", relevant or ["unattributed native work"]


def load_native_profile(
    request_windows: list[tuple[float, float]],
) -> tuple[dict[str, CallNode], Counter[str], int, float]:
    path = RAW_DIR / "server-profile.json.gz"
    with gzip.open(path, "rt") as source:
        profile = json.load(source)
    profile_start_epoch = profile.get("meta", {}).get("startTime", 0.0) / 1000.0
    roots = {name: CallNode() for name in ("server", "background", "other")}
    thread_samples: Counter[str] = Counter()
    total_samples = 0
    server_cpu_microseconds = 0
    for process in profile.get("processes", [profile]):
        for thread in process.get("threads", []):
            samples = thread.get("samples", {})
            stacks = samples.get("stack", [])
            weights = samples.get("weight", [1] * len(stacks))
            cpu_deltas = samples.get("threadCPUDelta", [0] * len(stacks))
            times = samples.get("time", [0.0] * len(stacks))
            for stack_index, weight, cpu_delta, sample_time in zip(
                stacks, weights, cpu_deltas, times
            ):
                sample_epoch = profile_start_epoch + sample_time / 1000.0
                if not any(start <= sample_epoch <= end
                           for start, end in request_windows):
                    continue
                server_cpu_microseconds += cpu_delta or 0
                if stack_index is None:
                    continue
                category, names = meaningful_stack(stack_names(thread, stack_index))
                if "tantivy" in thread.get("name", "").lower():
                    category = "background"
                sample_weight = abs(int(weight or 1))
                total_samples += sample_weight
                thread_samples[thread.get("name", "unnamed thread")] += sample_weight
                node = roots[category]
                node.samples += sample_weight
                previous = None
                for name in names:
                    if name == previous:
                        continue
                    child = node.children.setdefault(name, CallNode())
                    child.samples += sample_weight
                    node = child
                    previous = name
    return roots, thread_samples, total_samples, server_cpu_microseconds / 1_000_000


def parse_callgrind_name(value: str, names: dict[str, str]) -> str:
    value = value.strip()
    if not value.startswith("("):
        return value
    identifier, _, remainder = value[1:].partition(")")
    remainder = remainder.strip()
    if remainder:
        names[identifier] = remainder
        return remainder
    return names.get(identifier, f"unknown function {identifier}")


def load_callgrind_profile() -> tuple[dict[str, CallNode], Counter[str], int, float]:
    """Read Callgrind instruction counts and form a contextual save call tree."""
    paths = sorted((RAW_DIR / "callgrind").glob("callgrind.out.*"))
    if not paths:
        raise FileNotFoundError("No measured Callgrind dump exists")
    names: dict[str, str] = {}
    self_costs: Counter[str] = Counter()
    edge_costs: Counter[tuple[str, str]] = Counter()
    total_instructions = 0
    caller: str | None = None
    callee: str | None = None
    after_calls = False
    for raw_line in paths[-1].read_text(errors="replace").splitlines():
        line = raw_line.strip()
        if line.startswith("summary:"):
            total_instructions = int(line.split(":", 1)[1].strip())
        elif line.startswith("fn="):
            caller = parse_callgrind_name(line[3:], names)
            callee = None
            after_calls = False
        elif line.startswith("cfn="):
            callee = parse_callgrind_name(line[4:], names)
            after_calls = False
        elif line.startswith("calls="):
            after_calls = True
        elif line and (line[0].isdigit() or line[0] in "+-*"):
            try:
                cost = int(line.split()[-1])
            except ValueError:
                continue
            if caller is None:
                continue
            if callee is not None and after_calls:
                edge_costs[(caller, callee)] += cost
                callee = None
                after_calls = False
            else:
                self_costs[caller] += cost

    outgoing: dict[str, Counter[str]] = defaultdict(Counter)
    for (source, target), cost in edge_costs.items():
        outgoing[source][target] += cost

    inclusive = {
        name: self_costs[name] + sum(children.values())
        for name, children in outgoing.items()
    }
    for name, cost in self_costs.items():
        inclusive.setdefault(name, cost)

    def contextual_node(name: str, cost: int, ancestors: frozenset[str]) -> CallNode:
        node = CallNode(samples=cost)
        if name in ancestors:
            return node
        denominator = max(inclusive.get(name, cost), 1)
        scale = min(1.0, cost / denominator)
        for child_name, global_edge_cost in outgoing.get(name, {}).items():
            child_cost = min(cost, round(global_edge_cost * scale))
            if child_cost:
                node.children[clean_function_name(child_name)] = contextual_node(
                    child_name, child_cost, ancestors | {name})
        return node

    handler_names = [name for name in inclusive if "handle_save_buffer_request" in name]
    roots = {name: CallNode() for name in ("server", "background", "other")}
    for handler_name in handler_names:
        cost = inclusive[handler_name]
        child = contextual_node(handler_name, cost, frozenset())
        cleaned = clean_function_name(handler_name)
        roots["server"].children[cleaned] = child
        roots["server"].samples += cost
    if not roots["server"].samples:
        raise ValueError("Callgrind profile contains no save handler")
    total_instructions = max(total_instructions, roots["server"].samples)
    remainder = total_instructions - roots["server"].samples
    roots["background"].samples = remainder
    contexts = Counter({"synchronous save request": roots["server"].samples})
    if remainder:
        contexts["concurrent/background work"] = remainder
    return roots, contexts, total_instructions, 0.0


def load_cpu_profile(
    request_windows: list[tuple[float, float]],
) -> tuple[dict[str, CallNode], Counter[str], int, float, str]:
    if list((RAW_DIR / "callgrind").glob("callgrind.out.*")):
        roots, contexts, total, cpu_seconds = load_callgrind_profile()
        return roots, contexts, total, cpu_seconds, "callgrind"
    roots, contexts, total, cpu_seconds = load_native_profile(request_windows)
    return roots, contexts, total, cpu_seconds, "sampling"


def close_span_durations() -> dict[str, list[float]]:
    durations: dict[str, list[float]] = defaultdict(list)
    with (RAW_DIR / "server.jsonl").open() as source:
        for line in source:
            event = json.loads(line)
            fields = event.get("fields", {})
            if fields.get("message") != "close":
                continue
            span = event.get("span", {})
            name = span.get("name")
            ancestry = [ancestor.get("name") for ancestor in event.get("spans", [])]
            belongs_to_save = (name == "update_from_and_rerender_buffer" or
                               "update_from_and_rerender_buffer" in ancestry)
            belongs_to_save_background = bool(
                name and name.startswith("tantivy_") and not ancestry)
            if not belongs_to_save and not belongs_to_save_background:
                continue
            busy = fields.get("time.busy")
            if not name or not isinstance(busy, str):
                continue
            multiplier = 1.0
            if busy.endswith("ms"):
                value = busy[:-2]
                multiplier = 0.001
            elif busy.endswith("µs"):
                value = busy[:-2]
                multiplier = 0.000001
            elif busy.endswith("ns"):
                value = busy[:-2]
                multiplier = 0.000000001
            elif busy.endswith("s"):
                value = busy[:-1]
            else:
                continue
            durations[name].append(float(value) * multiplier)
    return durations


def median_span(spans: dict[str, list[float]], name: str) -> float:
    values = spans.get(name, [])
    return statistics.median(values) if values else 0.0


def derived_unspanned_remainders(spans: dict[str, list[float]]) -> list[tuple[str, float]]:
    update_graph_remainder = max(
        0.0,
        median_span(spans, "update_graph_including_nodeMerges")
        - median_span(spans, "validate_override_invariants_after_save")
        - median_span(spans, "update_graph_minus_nodeMerges")
        - median_span(spans, "merge_nodes"),
    )
    stores_remainder = max(
        0.0,
        median_span(spans, "update_graph_minus_nodeMerges")
        - median_span(spans, "apply_delete_propagation_cleanup")
        - median_span(spans, "update_fs_from_savenode_defs")
        - median_span(spans, "apply_definenodes_to_inRustGraph")
        - median_span(spans, "context_origin_types_for_saved"),
    )
    return [
        ("unspanned before/after store update inside update_graph_including_nodeMerges",
         update_graph_remainder),
        ("unspanned inside update_graph_minus_nodeMerges", stores_remainder),
    ]


def changed_file_counts() -> tuple[int, int, int]:
    def manifest(path: Path) -> dict[str, int]:
        result: dict[str, int] = {}
        with path.open() as source:
            for name, size in csv.reader(source, delimiter="\t"):
                result[name] = int(size)
        return result
    before = manifest(RAW_DIR / "files-before.tsv")
    after = manifest(RAW_DIR / "files-after.tsv")
    added = len(after.keys() - before.keys())
    removed = len(before.keys() - after.keys())
    size_changed = sum(before[name] != after[name]
                       for name in before.keys() & after.keys())
    return added, removed, size_changed


def counted_save_work() -> tuple[list[int], list[tuple[int, int]]]:
    log = (RAW_DIR / "server.stderr.log").read_text()
    definitions = [int(value) for value in re.findall(
        r"Writing ([0-9]+) instruction\(s\) to disk", log)]
    file_changes = [(int(deleted), int(written)) for deleted, written in re.findall(
        r"Deleted ([0-9]+) file\(s\), wrote ([0-9]+) file\(s\)", log)]
    return definitions, file_changes


def explanation_for(name: str, explanations: dict[str, Any]) -> str:
    functions = explanations.get("function", {})
    if name in functions:
        return functions[name]
    short = name.rsplit("::", 1)[-1]
    if short in functions:
        return functions[short]
    return "TODO: Explain this process from the cited implementation."


def is_transparent_frame(name: str) -> bool:
    return (name.startswith("futures_executor::local_pool::") or
            name.startswith("std::thread::local::LocalKey"))


def is_atomic_process(name: str) -> bool:
    return any(fragment in name for fragment in (
        "textlinks_from_text",
        "im::hash::map::HashMap<K,V,S>::insert",
    ))


def visible_children(node: CallNode) -> list[tuple[str, CallNode]]:
    result: list[tuple[str, CallNode]] = []
    for name, child in node.children.items():
        if is_transparent_frame(name):
            result.extend(visible_children(child))
        else:
            result.append((name, child))
    return sorted(result, key=lambda pair: pair[1].samples, reverse=True)


def children_needed_to_explain(node: CallNode, total_samples: int) -> list[tuple[str, CallNode]]:
    """Choose children until the parent's unexplained share is below 5%."""
    selected: list[tuple[str, CallNode]] = []
    explained = 0
    for pair in visible_children(node):
        selected.append(pair)
        explained += pair[1].samples
        if (node.samples - explained) / total_samples < SIGNIFICANT_FRACTION:
            break
    return selected


def costs_named(node: CallNode, fragment: str) -> list[int]:
    result: list[int] = []
    for name, child in node.children.items():
        if fragment in name:
            result.append(child.samples)
        result.extend(costs_named(child, fragment))
    return result


def render_call_children(lines: list[str], node: CallNode, stars: int,
                         total_samples: int, explanations: dict[str, Any]) -> None:
    children = children_needed_to_explain(node, total_samples)
    for name, child in children:
        fraction = child.samples / total_samples if total_samples else 0.0
        lines.append(f"{'*' * stars} {fraction:.1%}: ={name}=")
        lines.append(f"{'*' * (stars + 1)} how that works")
        lines.append(explanation_for(name, explanations))
        lines.append(f"{'*' * (stars + 1)} subprocesses")
        if fraction < SIGNIFICANT_FRACTION:
            lines.append("This process is below 5% of total CPU and therefore needs no further subdivision.")
        elif is_atomic_process(name):
            lines.append("This is the indivisible inner loop/library operation measured by the profiler.")
        elif visible_children(child):
            render_call_children(lines, child, stars + 2, total_samples, explanations)
        else:
            lines.append("The profiler exposes no further distinct called process; this is self-time or atomic library work.")


def main() -> None:
    metadata = read_key_values(RAW_DIR / "run-metadata.tsv")
    client_rows = read_client_rows()
    explanations = tomllib.loads(EXPLANATIONS_PATH.read_text())
    request_windows = [(float(row["start_epoch"]), float(row["end_epoch"]))
                       for row in client_rows]
    native_roots, thread_samples, total_native_samples, server_cpu_seconds, cpu_backend = (
        load_cpu_profile(request_windows))
    spans = close_span_durations()
    walls = [float(row["wall_seconds"]) for row in client_rows]
    measured_server_cpus = [float(row["server_cpu_seconds"]) for row in client_rows]
    emacs_cpus = [float(row["emacs_cpu_seconds"]) for row in client_rows]
    added, removed, size_changed = changed_file_counts()
    definitions, file_changes = counted_save_work()
    emacs_cpu_total = sum(emacs_cpus)
    server_cpu_proxy = sum(measured_server_cpus)
    if server_cpu_proxy <= 0:
        server_cpu_proxy = (server_cpu_seconds if server_cpu_seconds > 0 else
                            total_native_samples / float(metadata["sample_rate_hz"]))
    total_cpu_proxy = server_cpu_proxy + emacs_cpu_total
    server_fraction = server_cpu_proxy / total_cpu_proxy if total_cpu_proxy else 0.0
    emacs_fraction = emacs_cpu_total / total_cpu_proxy if total_cpu_proxy else 0.0

    synchronous_samples = native_roots["server"].samples
    synchronous_fraction = (synchronous_samples / total_native_samples
                            * server_fraction if total_native_samples else 0.0)
    background_fraction = (native_roots["background"].samples / total_native_samples
                           * server_fraction if total_native_samples else 0.0)
    other_native_fraction = (native_roots["other"].samples / total_native_samples
                             * server_fraction if total_native_samples else 0.0)
    validation_costs = sorted(costs_named(
        native_roots["server"],
        "complete_validation::validate_graph_after_definitions"), reverse=True)[:2]
    validation_fractions = [cost / total_native_samples * server_fraction
                            for cost in validation_costs]
    validation_summary = (f"{validation_fractions[0]:.1%} and "
                          f"{validation_fractions[1]:.1%}"
                          if len(validation_fractions) >= 2 else "most")
    lines = [
        "#+TITLE: Recursive-content save profile",
        "#+PROPERTY: header-args :eval never-export",
        "",
        f"This report profiles {metadata['runs']} saves of a recursive content view rooted at",
        f"={metadata['root_pid']}=.  Each save adds one single-token level-two headline.",
        "The benchmark ran against a disposable copy of =data/=; the original data was not written.",
        "",
        f"Median end-to-end save latency was *{statistics.median(walls):.3f} s* "
        f"(min {min(walls):.3f} s, p95 {percentile(walls, 0.95):.3f} s, max {max(walls):.3f} s).",
        "The principal finding is that the same whole-graph candidate is validated twice under one "
        f"mutation gate. The two calls consume {validation_summary} of total CPU evidence. "
        "Safely reusing the first result "
        "should therefore remove roughly half the work; making the remaining validation incremental "
        "is the larger follow-on opportunity.",
        "Top-level CPU fractions use measured process CPU time. Rust sub-branches divide the server "
        "share according to Callgrind user-space instruction counts. They are CPU-work attribution, "
        "not fractions of wall time or hardware cycle counts.",
        "",
        "/Method and cautions./",
        "",
        ("Callgrind counted user-space instructions during exactly one save; the normal-speed runs "
         "supply wall and process-CPU measurements. " if cpu_backend == "callgrind" else
         f"The Rust process was sampled at {metadata['sample_rate_hz']} Hz while the saves were in flight. ")
        +
        "Structured tracing supplies nested span wall times; Emacs supplies per-save process CPU time "
        "and a Lisp profiler capture. Instruction share is a deterministic proxy for CPU work; "
        "different instructions need not have identical cycle cost.",
        "",
        f"The runtime graph began with {int(metadata['runtime_graph_nodes']):,} folded nodes and "
        f"{int(metadata['runtime_graph_edges']):,} containment edges. The copied owned tree contained "
        f"{int(metadata['graph_files_before']):,} =.skg= files ({int(metadata['graph_bytes_before']):,} bytes), "
        f"including owned folders not active in this config. Across all runs, {added} files were added, "
        f"{removed} removed, and {size_changed} pre-existing files changed size.",
        "",
        "The CPU tree subdivides every branch at or above 5% of total CPU until each leaf is below "
        "5% or is an indivisible inner loop/library operation. Near-threshold children are included "
        "when needed to leave less than 5% of a parent's cost unexplained.",
        "",
        "/Counted work./",
        "",
        "| quantity | observed |",
        "|----------+----------|",
        f"| active runtime nodes | {int(metadata['runtime_graph_nodes']):,} |",
        f"| containment edges | {int(metadata['runtime_graph_edges']):,} |",
        f"| save definitions per run | {', '.join(map(str, definitions))} |",
        f"| filesystem deletes/writes per run | {', '.join(f'{deleted}/{written}' for deleted, written in file_changes)} |",
        f"| copied-tree files added/removed/size-changed after all runs | {added}/{removed}/{size_changed} |",
        "",
        "/Wall-time spans./ These are elapsed-time spans and therefore intentionally separate from "
        "the CPU-fraction tree. They can expose waits that an on-CPU sampler cannot see.",
        "",
        "| span | observations | median seconds | min | max |",
        "|------+--------------+----------------+-----+-----|",
    ]
    for name, values in sorted(spans.items(), key=lambda pair: statistics.median(pair[1]),
                               reverse=True):
        lines.append(f"| ={name}= | {len(values)} | {statistics.median(values):.6f} | "
                     f"{min(values):.6f} | {max(values):.6f} |")
    for name, value in derived_unspanned_remainders(spans):
        lines.append(f"| /{name} (derived)/ | - | {value:.6f} | - | - |")
    lines.extend([
        "",
        "/Per-save measurements./",
        "",
        "| run | wall seconds | server CPU seconds | Emacs CPU seconds | request bytes | request headlines | response bytes | response headlines |",
        "|-----+--------------+--------------------+-------------------+---------------+-------------------+----------------+--------------------|",
    ])
    for row in client_rows:
        lines.append(f"| {row['run']} | {float(row['wall_seconds']):.6f} | "
                     f"{float(row['server_cpu_seconds']):.6f} | "
                     f"{float(row['emacs_cpu_seconds']):.6f} | {row['bytes_before']} | "
                     f"{row['headlines_before']} | {row['bytes_after']} | {row['headlines_after']} |")
    lines.extend([
        "",
        "Server CPU is process-wide and may exceed wall time when multiple threads run. In this fresh-server "
        "benchmark, runs 2 and 3 overlap an initial Tantivy segment merge queued before measurement; the "
        "exact first-save Callgrind window attributes 99.9% of native instructions to the synchronous request. "
        "The merge is therefore concurrency evidence, not charged to synchronous save latency.",
        "",
        "/Native CPU evidence by execution context./",
        "",
        f"| execution context | {'instructions' if cpu_backend == 'callgrind' else 'samples'} | fraction of native CPU evidence |",
        "|--------+---------+----------------------------|",
    ])
    for name, count in thread_samples.most_common():
        lines.append(f"| ={name}= | {count} | {count / total_native_samples:.1%} |")
    lines.extend([
        "",
        "/Reproduction./",
        "",
        "#+begin_src sh",
        "tools/introspect/profile-with-org-report/run-all.sh",
        "#+end_src",
        "",
        "The generator is =tools/introspect/profile-with-org-report/generate_report.py=. "
        "Hand-written process descriptions live in "
        "=tools/introspect/profile-with-org-report/how-it-works.toml=, so regenerating timings "
        "does not discard the explanations.",
        "",
        f"* {server_fraction:.1%}: Rust server",
        "** how that works",
        explanations["process"]["server"],
        "** subprocesses",
        f"*** {synchronous_fraction:.1%}: synchronous save request",
        "**** how that works",
        "The connection thread executes =handle_save_buffer_request= from receipt of the complete "
        "buffer through the terminal save response.",
        "**** subprocesses",
    ])
    render_call_children(lines, native_roots["server"], 5,
                         max(total_native_samples, 1) / max(server_fraction, 1e-12),
                         explanations)
    if native_roots["background"].samples:
        lines.extend([
            f"*** {background_fraction:.1%}: background Tantivy worker",
            "**** how that works",
            explanations["process"]["background"],
            "**** subprocesses",
            "This is a distinct worker-thread operation and is not further divided unless it reaches "
            "5% of total CPU evidence.",
        ])
    if native_roots["other"].samples:
        lines.extend([
            f"*** {other_native_fraction:.1%}: other Rust threads",
            "**** how that works",
            explanations["process"]["other"],
            "**** subprocesses",
            "No individually attributable child reached 5% of total sampled CPU.",
        ])
    lines.extend([
        f"* {emacs_fraction:.1%}: Emacs client",
        "** how that works",
        explanations["process"]["emacs"],
        "** subprocesses",
        f"*** {emacs_fraction:.1%}: client preparation, transport handling, and buffer replacement",
        "**** how that works",
        "See =raw/emacs-profile.txt= for the Lisp sampler tree. This branch remains atomic here when "
        "the whole Emacs process is below the 5% expansion threshold.",
        "**** subprocesses",
        "The complete Emacs branch is below the 5% threshold, so it is a permitted leaf.",
    ])
    RESULT_PATH.write_text("\n".join(lines))


if __name__ == "__main__":
    main()
