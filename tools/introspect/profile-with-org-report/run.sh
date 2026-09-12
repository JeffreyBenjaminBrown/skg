#!/usr/bin/env bash

set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd "$profile_dir/../../.." && pwd)"
raw_dir="$profile_dir/raw"
profile_runs="${PROFILE_RUNS:-5}"
profile_rate="${PROFILE_RATE:-499}"
settle_seconds="${PROFILE_SETTLE_SECONDS:-30}"
between_run_seconds="${PROFILE_BETWEEN_RUN_SECONDS:-0.2}"
skip_native="${PROFILE_SKIP_NATIVE:-}"
root_pid="2f77ea44-9e5e-4dfc-b89b-2019971ab05a"
samply_binary="${SAMPLY_BINARY:-/home/ubuntu/.cargo/bin/samply}"
temporary_root="$(mktemp -d "$project_root/target/save-profile.XXXXXX")"
copied_data="$temporary_root/data"
ready_file="$temporary_root/emacs-ready"
go_file="$temporary_root/emacs-go"
done_file="$temporary_root/emacs-done"
server_log="$raw_dir/server.stderr.log"
profile_output="$raw_dir/server-profile.json.gz"
server_pid=""
emacs_pid=""
samply_pid=""

cleanup_profile_processes () {
  if [[ -n "$samply_pid" ]] && kill -0 "$samply_pid" 2>/dev/null; then
    kill -INT "$samply_pid" 2>/dev/null || true
    wait "$samply_pid" 2>/dev/null || true
  fi
  if [[ -n "$emacs_pid" ]] && kill -0 "$emacs_pid" 2>/dev/null; then
    kill "$emacs_pid" 2>/dev/null || true
    wait "$emacs_pid" 2>/dev/null || true
  fi
  if [[ -n "$server_pid" ]] && kill -0 "$server_pid" 2>/dev/null; then
    kill -INT "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
  fi
  if [[ -z "${PROFILE_KEEP_COPY:-}" ]]; then
    rm -rf -- "$temporary_root"
  else
    printf 'Disposable data retained at %s\n' "$temporary_root"
  fi
}

find_available_port () {
  local candidate
  for candidate in $(seq 18732 18832); do
    if ! ss -ltn 2>/dev/null | rg -q ":${candidate}[[:space:]]"; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done
  printf 'No unused profiling port found.\n' >&2
  return 1
}

wait_for_file () {
  local path="$1"
  local process_to_watch="$2"
  local description="$3"
  local attempt
  for attempt in $(seq 1 1200); do
    if [[ -e "$path" ]]; then
      return 0
    fi
    if ! kill -0 "$process_to_watch" 2>/dev/null; then
      printf '%s process exited early.\n' "$description" >&2
      return 1
    fi
    sleep 0.05
  done
  printf 'Timed out waiting for %s.\n' "$description" >&2
  return 1
}

trap cleanup_profile_processes EXIT

if [[ -z "$skip_native" && ! -x "$samply_binary" ]]; then
  printf 'samply is required at %s. See README.org.\n' "$samply_binary" >&2
  exit 1
fi
mkdir -p "$raw_dir"
find "$raw_dir" -mindepth 1 -maxdepth 1 -type f -delete
mkdir -p "$copied_data"
printf 'Copying data into an isolated root...\n'
cp -a "$project_root/data/." "$copied_data/"

profile_port="$(find_available_port)"
sed -i -E "s/^port[[:space:]]*=.*/port = $profile_port/" \
  "$copied_data/skgconfig.toml"
sed -i -E 's/^timing_log[[:space:]]*=.*/timing_log = true/' \
  "$copied_data/skgconfig.toml"
truncate -s 0 "$copied_data/logs/server.jsonl" 2>/dev/null || true
truncate -s 0 "$copied_data/logs/server-to-user.log" 2>/dev/null || true

find "$copied_data/owned" -type f -name '*.skg' -printf '%P\t%s\n' \
  | LC_ALL=C sort > "$raw_dir/files-before.tsv"

printf 'Starting isolated server on port %s...\n' "$profile_port"
RUST_LOG=info "$project_root/target/debug/skg" \
  "$copied_data/skgconfig.toml" >"$server_log" 2>&1 &
server_pid=$!
for _attempt in $(seq 1 1200); do
  if rg -q 'Server ready\.' "$server_log" 2>/dev/null; then
    break
  fi
  if ! kill -0 "$server_pid" 2>/dev/null; then
    printf 'The isolated server exited during startup.\n' >&2
    tail -80 "$server_log" >&2
    exit 1
  fi
  sleep 0.1
done
if ! rg -q 'Server ready\.' "$server_log"; then
  printf 'The isolated server did not become ready.\n' >&2
  exit 1
fi

# The server announces readiness before Tantivy's initial segment merges finish.
# Let those unrelated workers drain before measuring a warm save.
sleep "$settle_seconds"

printf 'Opening the recursive view in batch Emacs...\n'
env \
  PROFILE_PORT="$profile_port" \
  PROFILE_RUNS="$profile_runs" \
  PROFILE_BETWEEN_RUN_SECONDS="$between_run_seconds" \
  PROFILE_ROOT_PID="$root_pid" \
  PROFILE_SERVER_PID="$server_pid" \
  PROFILE_CLOCK_TICKS="$(python3 -c 'import os; print(os.sysconf("SC_CLK_TCK"))')" \
  PROFILE_READY_FILE="$ready_file" \
  PROFILE_GO_FILE="$go_file" \
  PROFILE_DONE_FILE="$done_file" \
  PROFILE_RAW_DIR="$raw_dir" \
  emacs --batch -l "$profile_dir/profile-save.el" \
  >"$raw_dir/emacs.log" 2>&1 &
emacs_pid=$!
wait_for_file "$ready_file" "$emacs_pid" "Emacs view setup"

if [[ -z "$skip_native" ]]; then
  printf 'Attaching native CPU sampler and beginning saves...\n'
  "$samply_binary" record \
    --save-only \
    --unstable-presymbolicate \
    --rate "$profile_rate" \
    --profile-name 'Skg recursive-content saves' \
    --output "$profile_output" \
    --pid "$server_pid" \
    >"$raw_dir/samply.log" 2>&1 &
  samply_pid=$!
  sleep 0.5
else
  printf 'Beginning wall-time-only harness validation...\n'
fi
touch "$go_file"
wait_for_file "$done_file" "$emacs_pid" "measured saves"
wait "$emacs_pid"
emacs_pid=""

if [[ -n "$samply_pid" ]]; then
  kill -INT "$samply_pid" 2>/dev/null || true
  wait "$samply_pid"
  samply_pid=""
fi

cp "$copied_data/logs/server.jsonl" "$raw_dir/server.jsonl"
find "$copied_data/owned" -type f -name '*.skg' -printf '%P\t%s\n' \
  | LC_ALL=C sort > "$raw_dir/files-after.tsv"
{
  printf 'root_pid\t%s\n' "$root_pid"
  printf 'runs\t%s\n' "$profile_runs"
  printf 'sample_rate_hz\t%s\n' "$profile_rate"
  printf 'server_pid\t%s\n' "$server_pid"
  printf 'profile_port\t%s\n' "$profile_port"
  printf 'graph_files_before\t%s\n' "$(wc -l < "$raw_dir/files-before.tsv")"
  printf 'graph_bytes_before\t%s\n' "$(awk -F '\t' '{n += $2} END {print n + 0}' "$raw_dir/files-before.tsv")"
  printf 'runtime_graph_nodes\t%s\n' "$(sed -n -E 's/.*graph validated files=([0-9]+).*/\1/p' "$server_log" | tail -1)"
  printf 'runtime_graph_edges\t%s\n' "$(sed -n -E 's/.* ([0-9]+) edges,.*/\1/p' "$server_log" | tail -1)"
  printf 'git_head\t%s\n' "$(git -C "$project_root" rev-parse HEAD)"
  printf 'run_utc\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} > "$raw_dir/run-metadata.tsv"

if [[ -z "$skip_native" ]]; then
  python3 "$profile_dir/generate_report.py"
  printf 'Report written to %s/result.org\n' "$profile_dir"
else
  printf 'Wall-time evidence collected; native profile intentionally skipped.\n'
fi
