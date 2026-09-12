#!/usr/bin/env bash

set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd "$profile_dir/../../.." && pwd)"
raw_dir="$profile_dir/raw"
callgrind_raw="$raw_dir/callgrind"
valgrind_root="${VALGRIND_ROOT:-$project_root/target/profile-tools/valgrind-install}"
valgrind_binary="$valgrind_root/bin/valgrind"
callgrind_control="$valgrind_root/bin/callgrind_control"
root_pid="2f77ea44-9e5e-4dfc-b89b-2019971ab05a"
settle_seconds="${PROFILE_SETTLE_SECONDS:-30}"
temporary_root="$(mktemp -d "$project_root/target/save-callgrind.XXXXXX")"
copied_data="$temporary_root/data"
ready_file="$temporary_root/emacs-ready"
go_file="$temporary_root/emacs-go"
done_file="$temporary_root/emacs-done"
server_log="$callgrind_raw/server.stderr.log"
server_pid=""
emacs_pid=""

cleanup_profile_processes () {
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
  return 1
}

wait_for_file () {
  local path="$1"
  local process_to_watch="$2"
  local description="$3"
  local attempt
  for attempt in $(seq 1 7200); do
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

if [[ ! -x "$valgrind_binary" || ! -x "$callgrind_control" ]]; then
  printf 'Callgrind is required under %s. See README.org.\n' "$valgrind_root" >&2
  exit 1
fi
mkdir -p "$callgrind_raw" "$copied_data"
find "$callgrind_raw" -mindepth 1 -maxdepth 1 -type f -delete
printf 'Copying data into an isolated Callgrind root...\n'
cp -a "$project_root/data/." "$copied_data/"
profile_port="$(find_available_port)"
sed -i -E "s/^port[[:space:]]*=.*/port = $profile_port/" \
  "$copied_data/skgconfig.toml"
sed -i -E 's/^timing_log[[:space:]]*=.*/timing_log = true/' \
  "$copied_data/skgconfig.toml"
truncate -s 0 "$copied_data/logs/server.jsonl" 2>/dev/null || true

printf 'Starting the isolated server under Callgrind on port %s...\n' "$profile_port"
RUST_LOG=info "$valgrind_binary" \
  --tool=callgrind \
  --instr-atstart=no \
  --callgrind-out-file="$callgrind_raw/callgrind.out" \
  "$project_root/target/debug/skg" "$copied_data/skgconfig.toml" \
  >"$server_log" 2>&1 &
server_pid=$!
for _attempt in $(seq 1 7200); do
  if rg -q 'Server ready\.' "$server_log" 2>/dev/null; then
    break
  fi
  if ! kill -0 "$server_pid" 2>/dev/null; then
    tail -80 "$server_log" >&2
    exit 1
  fi
  sleep 0.1
done
if ! rg -q 'Server ready\.' "$server_log"; then
  printf 'The Callgrind server did not become ready.\n' >&2
  exit 1
fi
sleep "$settle_seconds"

env \
  PROFILE_PORT="$profile_port" \
  PROFILE_RUNS=1 \
  PROFILE_BETWEEN_RUN_SECONDS=0 \
  PROFILE_ROOT_PID="$root_pid" \
  PROFILE_SERVER_PID="$server_pid" \
  PROFILE_CLOCK_TICKS="$(python3 -c 'import os; print(os.sysconf("SC_CLK_TCK"))')" \
  PROFILE_READY_FILE="$ready_file" \
  PROFILE_GO_FILE="$go_file" \
  PROFILE_DONE_FILE="$done_file" \
  PROFILE_RAW_DIR="$callgrind_raw" \
  emacs --batch -l "$profile_dir/profile-save.el" \
  >"$callgrind_raw/emacs.log" 2>&1 &
emacs_pid=$!
wait_for_file "$ready_file" "$emacs_pid" "Emacs view setup"

printf 'Enabling Callgrind for exactly one save...\n'
"$callgrind_control" -i on "$server_pid" >"$callgrind_raw/control.log"
"$callgrind_control" -z "$server_pid" >>"$callgrind_raw/control.log"
touch "$go_file"
wait_for_file "$done_file" "$emacs_pid" "Callgrind save"
"$callgrind_control" --dump=measured-save "$server_pid" >>"$callgrind_raw/control.log"
"$callgrind_control" -i off "$server_pid" >>"$callgrind_raw/control.log"
wait "$emacs_pid"
emacs_pid=""

kill -INT "$server_pid"
wait "$server_pid"
server_pid=""
cp "$copied_data/logs/server.jsonl" "$callgrind_raw/server.jsonl"
"$valgrind_root/bin/callgrind_annotate" --inclusive=yes \
  "$callgrind_raw/callgrind.out.1" >"$callgrind_raw/callgrind-annotate.txt"
python3 "$profile_dir/generate_report.py"
printf 'Callgrind profile added; report written to %s/result.org\n' "$profile_dir"
