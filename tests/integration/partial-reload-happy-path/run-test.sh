#!/bin/bash

# Two-user, two-remote happy path for fork -> pull -> subscribe back -> pull.
# Each client gets its own data root and ownership boundary.  The remotes and
# all four worktrees live below this test directory while the test runs.

set -eu
set -o pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG two-user partial-reload happy-path integration test ==="

TEST_WORK_ROOT=""
CURRENT_SERVER_PID=""
CURRENT_SERVER_LOG=""
FAILURES_FILE="$TEST_DIR/failures.log"
TIMINGS_FILE="$TEST_DIR/timings.log"
CLIENT_LOGS=(
  "$TEST_DIR/client-economist-first.log"
  "$TEST_DIR/client-china-scholar.log"
  "$TEST_DIR/client-economist-final.log"
)
SERVER_LOGS=(
  "$TEST_DIR/server-economist-first.log"
  "$TEST_DIR/server-china-scholar.log"
  "$TEST_DIR/server-economist-final.log"
)

record_runner_failure() {
  local message="$1"
  printf 'FAIL\trunner\t%s\n' "$message" >> "$FAILURES_FILE"
  echo "FAIL: $message"
}

stop_phase_server() {
  if [ -n "$CURRENT_SERVER_PID" ] && kill -0 "$CURRENT_SERVER_PID" 2>/dev/null; then
    echo "Stopping phase server (PID: $CURRENT_SERVER_PID)..."
    kill -INT "$CURRENT_SERVER_PID" 2>/dev/null || true
    local attempt=0
    while [ "$attempt" -lt 100 ] && kill -0 "$CURRENT_SERVER_PID" 2>/dev/null; do
      sleep 0.1
      attempt=$((attempt + 1))
    done
    if kill -0 "$CURRENT_SERVER_PID" 2>/dev/null; then
      record_runner_failure \
        "server did not stop gracefully; forcing PID $CURRENT_SERVER_PID down"
      kill -9 "$CURRENT_SERVER_PID" 2>/dev/null || true
    fi
    wait "$CURRENT_SERVER_PID" 2>/dev/null || true
  fi
  CURRENT_SERVER_PID=""
}

delete_test_database() {
  local database="$1"
  local token
  token=$(curl -s -X POST http://127.0.0.1:8000/v1/signin \
    -H "Content-Type: application/json" \
    -d '{"username":"admin","password":"password"}' 2>/dev/null \
    | grep -oP '"token"\s*:\s*"\K[^"]+' 2>/dev/null) || true
  if [ -n "$token" ]; then
    curl -s -o /dev/null -X DELETE \
      "http://127.0.0.1:8000/v1/databases/$database" \
      -H "Authorization: Bearer $token" 2>/dev/null || true
  fi
}

cleanup_happy_path_test() {
  stop_phase_server
  if [ -n "${ECONOMIST_DB_NAME:-}" ]; then
    delete_test_database "$ECONOMIST_DB_NAME"
  fi
  if [ -n "${CHINA_SCHOLAR_DB_NAME:-}" ]; then
    delete_test_database "$CHINA_SCHOLAR_DB_NAME"
  fi
  case "$TEST_WORK_ROOT" in
    "$TEST_DIR"/work-*) rm -rf -- "$TEST_WORK_ROOT" ;;
  esac
}

trap cleanup_happy_path_test EXIT

start_phase_server() {
  local config="$1"
  local log="$2"
  CURRENT_SERVER_LOG="$log"
  : > "$log"
  (
    cd "$PROJECT_ROOT" || exit 1
    exec target/debug/skg "$config"
  ) > "$log" 2>&1 &
  CURRENT_SERVER_PID=$!
  echo "Started phase server (PID: $CURRENT_SERVER_PID); log: $log"

  local attempt=0
  while [ "$attempt" -lt 300 ]; do
    if grep -q "Server ready\." "$log" 2>/dev/null; then
      echo "Server ready on port $AVAILABLE_PORT"
      return 0
    fi
    if ! kill -0 "$CURRENT_SERVER_PID" 2>/dev/null; then
      record_runner_failure "server died during startup; see $log"
      return 1
    fi
    sleep 0.2
    attempt=$((attempt + 1))
  done
  record_runner_failure "server did not become ready within 60 seconds; see $log"
  return 1
}

run_emacs_phase() {
  local phase="$1"
  local config="$2"
  local log="$3"
  echo "Running Emacs phase: $phase"
  set +e
  (
    cd "$TEST_DIR" || exit 1
    env \
      SKG_TEST_PHASE="$phase" \
      SKG_TEST_PORT="$AVAILABLE_PORT" \
      SKG_TEST_CONFIG="$config" \
      SKG_TEST_WORK_ROOT="$TEST_WORK_ROOT" \
      SKG_TEST_FAILURES="$FAILURES_FILE" \
      SKG_TEST_TIMINGS="$TIMINGS_FILE" \
      SKG_PROJECT_ROOT="$PROJECT_ROOT" \
      timeout 180 emacs --batch -l "$TEST_DIR/test-emacs.el"
  ) > "$log" 2>&1
  local status=$?
  set -e
  cat "$log"
  if [ "$status" -ne 0 ]; then
    record_runner_failure \
      "Emacs phase '$phase' exited $status; see $log"
  fi
}

configure_git_identity() {
  local repository="$1"
  local user="$2"
  git -C "$repository" config user.email "${user}@integration.invalid"
  git -C "$repository" config user.name "$user"
}

check_typedb_server

rm -f "$FAILURES_FILE" "$TIMINGS_FILE" \
  "${CLIENT_LOGS[@]}" "${SERVER_LOGS[@]}"
touch "$FAILURES_FILE" "$TIMINGS_FILE"

TEST_WORK_ROOT=$(mktemp -d "$TEST_DIR/work-XXXXXX")
REMOTES_DIR="$TEST_WORK_ROOT/remotes"
SEEDS_DIR="$TEST_WORK_ROOT/seeds"
ECONOMIST_ROOT="$TEST_WORK_ROOT/economist-client"
CHINA_SCHOLAR_ROOT="$TEST_WORK_ROOT/china-scholar-client"
mkdir -p "$REMOTES_DIR" "$SEEDS_DIR" \
  "$ECONOMIST_ROOT/owned" "$ECONOMIST_ROOT/foreign" \
  "$CHINA_SCHOLAR_ROOT/owned" "$CHINA_SCHOLAR_ROOT/foreign"

# Seed the two on-disk remotes before cloning either user's view of them.
git init -q --initial-branch=main "$SEEDS_DIR/china-scholar-public"
configure_git_identity "$SEEDS_DIR/china-scholar-public" "China-Scholar"
printf '%s\n' 'DEPENDENCIES.toml' > \
  "$SEEDS_DIR/china-scholar-public/.gitignore"
printf '%s\n' \
  'pid: china' \
  'title: China' \
  'contains:' \
  '- chinese-economics' > \
  "$SEEDS_DIR/china-scholar-public/china.skg"
printf '%s\n' \
  'pid: chinese-economics' \
  'title: Chinese Economics' > \
  "$SEEDS_DIR/china-scholar-public/chinese-economics.skg"
git -C "$SEEDS_DIR/china-scholar-public" add .
git -C "$SEEDS_DIR/china-scholar-public" commit -q -m "Seed China notes"
git clone -q --bare "$SEEDS_DIR/china-scholar-public" \
  "$REMOTES_DIR/china-scholar-public.git"

git init -q --initial-branch=main "$SEEDS_DIR/economist-public"
configure_git_identity "$SEEDS_DIR/economist-public" "Economist"
printf '%s\n' 'DEPENDENCIES.toml' > \
  "$SEEDS_DIR/economist-public/.gitignore"
printf '%s\n' \
  'pid: economics' \
  'title: Economics' > \
  "$SEEDS_DIR/economist-public/economics.skg"
git -C "$SEEDS_DIR/economist-public" add .
git -C "$SEEDS_DIR/economist-public" commit -q -m "Seed Economics notes"
git clone -q --bare "$SEEDS_DIR/economist-public" \
  "$REMOTES_DIR/economist-public.git"

git clone -q "$REMOTES_DIR/china-scholar-public.git" \
  "$ECONOMIST_ROOT/foreign/china-scholar-public"
git clone -q "$REMOTES_DIR/economist-public.git" \
  "$ECONOMIST_ROOT/owned/economist-public"
git clone -q "$REMOTES_DIR/china-scholar-public.git" \
  "$CHINA_SCHOLAR_ROOT/owned/china-scholar-public"
git clone -q "$REMOTES_DIR/economist-public.git" \
  "$CHINA_SCHOLAR_ROOT/foreign/economist-public"

configure_git_identity "$ECONOMIST_ROOT/owned/economist-public" "Economist"
configure_git_identity "$ECONOMIST_ROOT/foreign/china-scholar-public" "Economist"
configure_git_identity "$CHINA_SCHOLAR_ROOT/owned/china-scholar-public" "China-Scholar"
configure_git_identity "$CHINA_SCHOLAR_ROOT/foreign/economist-public" "China-Scholar"

AVAILABLE_PORT=$(find_available_port)
ECONOMIST_DB_NAME="$(generate_db_name)-economist"
CHINA_SCHOLAR_DB_NAME="$(generate_db_name)-china-scholar"
ECONOMIST_CONFIG="$ECONOMIST_ROOT/skgconfig.toml"
CHINA_SCHOLAR_CONFIG="$CHINA_SCHOLAR_ROOT/skgconfig.toml"

printf '%s\n' \
  "db_name = \"$ECONOMIST_DB_NAME\"" \
  'tantivy_folder = ".index.tantivy"' \
  'maintenance_archive_folder = "maintenance-archives"' \
  "port = $AVAILABLE_PORT" \
  'beep_when_server_becomes_available = false' \
  'delete_on_quit = true' \
  '' \
  '[[sources]]' \
  'name = "china-scholar-public"' \
  'path = "foreign/china-scholar-public"' \
  '' \
  '[[sources]]' \
  'name = "economist-public"' \
  'path = "owned/economist-public"' > "$ECONOMIST_CONFIG"

printf '%s\n' \
  "db_name = \"$CHINA_SCHOLAR_DB_NAME\"" \
  'tantivy_folder = ".index.tantivy"' \
  'maintenance_archive_folder = "maintenance-archives"' \
  "port = $AVAILABLE_PORT" \
  'beep_when_server_becomes_available = false' \
  'delete_on_quit = true' \
  '' \
  '[[sources]]' \
  'name = "china-scholar-public"' \
  'path = "owned/china-scholar-public"' \
  '' \
  '[[sources]]' \
  'name = "economist-public"' \
  'path = "foreign/economist-public"' > "$CHINA_SCHOLAR_CONFIG"

# Economist forks and publishes first.  The other clone remains stale until
# China-Scholar's client-owned pull.
if start_phase_server "$ECONOMIST_CONFIG" "${SERVER_LOGS[0]}"; then
  run_emacs_phase "economist-first" "$ECONOMIST_CONFIG" "${CLIENT_LOGS[0]}"
fi
stop_phase_server

# China-Scholar sees the published fork only through a real pull, subscribes
# back, and publishes the reciprocal edge.  Economist's foreign clone is
# still stale while this happens.
if start_phase_server "$CHINA_SCHOLAR_CONFIG" "${SERVER_LOGS[1]}"; then
  run_emacs_phase "china-scholar" "$CHINA_SCHOLAR_CONFIG" "${CLIENT_LOGS[1]}"
fi
stop_phase_server

# Economist starts from the stale foreign clone, opens the affected view, and
# pulls the reciprocal subscription through partial rebuild.
if start_phase_server "$ECONOMIST_CONFIG" "${SERVER_LOGS[2]}"; then
  run_emacs_phase "economist-final" "$ECONOMIST_CONFIG" "${CLIENT_LOGS[2]}"
fi
stop_phase_server

for server_log in "${SERVER_LOGS[@]}"; do
  while IFS= read -r warning_line; do
    record_runner_failure \
      "server warning in $(basename "$server_log"): $warning_line"
  done < <(grep ' WARN ' "$server_log" 2>/dev/null || true)
done

echo ""
echo "Measured pull-to-unlock intervals:"
if [ -s "$TIMINGS_FILE" ]; then
  sed 's/^/  /' "$TIMINGS_FILE"
else
  echo "  (none recorded)"
  record_runner_failure "neither pull-to-unlock interval was recorded"
fi

echo ""
if [ -s "$FAILURES_FILE" ]; then
  echo "=== FAILURES (all collected) ==="
  sed 's/^/  /' "$FAILURES_FILE"
  exit 1
fi

echo "PASS: two-user fork/pull/subscribe-back/pull happy path"
exit 0
