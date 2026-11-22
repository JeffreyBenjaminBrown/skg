#!/bin/bash

# all-tests.sh — Run emacs, nvim, nextest, doctests, team tool, AI reader,
# generated docs and integration tests politely.
#
# Results are written here:
# - tests/results/ALL.org: a plain-text org-mode summary, whose first
#   lines name each failure and its log, and which quotes the failure
#   lines of each failed suite and integration test (and of a failed
#   build, after which nothing else runs)
# - tests/results/*.log: individual test results
# It clears tests/results/ at its start, except the subdirectories
# named for a plan number (tests/results/06/), which hold the logs of
# that plan's run and belong to its agents.
#
# Integration tests are run sequentially.  This script is intended to
# keep an interactive machine usable, not to minimize wall-clock time.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_DIR="$PROJECT_ROOT/tests/results"
BEEP="$PROJECT_ROOT/../sound/play-beep.sh beep-soothing"
SOUND=1
source "$SCRIPT_DIR/test-runner-lib.sh"

usage () {
  cat <<EOF
Usage: $0 [--no-sound]

Runs Emacs, Neovim, nextest, doctests, team tool, AI reader
(tools/skg-for-ai/), generated docs and integration tests sequentially with
conservative internal concurrency. Integration tests run once per
client (emacs, then nvim), labeled CLIENT/DIRECTORY in the output.

Options:
  --no-sound   Do not play the completion sound.
  -h, --help   Show this help.

Environment:
  SKG_SKIP_NVIM_INTEGRATION=1
                             Skip the nvim-client integration pass
                             (halves integration wall-clock).
  SKG_CARGO_BUILD_JOBS       Cargo build jobs. Default: 2.
  SKG_NEXTEST_BUILD_JOBS     Build jobs for the nextest phase.
                             Default: half the cores, capped at 8.
  SKG_NEXTEST_JOBS           Nextest jobs. Default: 1.
  SKG_TEST_NICE              nice level for test commands. Default: 15.
  SKG_TEST_IONICE=0          Disable idle I/O priority.
  SKG_FOR_AI_TEST_PORT       Port of the AI reader's scratch servers.
                             Default: 1793, or in a linked git
                             worktree a port derived from its path
                             ('skg_port_for_this_checkout').
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --no-sound)
      SOUND=0
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

mkdir -p "$RESULTS_DIR"
find "$RESULTS_DIR" -mindepth 1 -maxdepth 1 \
  ! \( -type d -regextype posix-extended \
       -regex '.*/[0-9]+([._][0-9]+)*' \) \
  -exec rm -rf {} +

# ── ALL.org ─────────────────────────────────────────────────

ORG="$RESULTS_DIR/ALL.org"
ORG_TITLE="* results from bash/all-tests.sh"

# The lines of a failed suite's log worth reading first: those that
# match PATTERN, without color codes or repeats, at most 30; or, if
# none match, the log's last 15 lines.
failure_lines_of_log () {
  local log_path="$1"
  local pattern="$2"
  local uncolored_log
  local lines
  uncolored_log="$(sed 's/\x1b\[[0-9;]*m//g' "$log_path" 2>/dev/null)"
  lines="$(printf '%s\n' "$uncolored_log" \
    | grep -E -- "$pattern" | awk '!seen[$0]++' | head -n 30)"
  if [ -z "$lines" ]; then
    lines="$(printf '%s\n' "$uncolored_log" | tail -n 15)"
  fi
  printf '%s\n' "$lines"; }

# Under ALL.org's title, when anything failed: each failure, by the
# log that holds its errors, so that ALL.org's first lines name them.
# Each argument is "WHAT [[file:LOG]]".
org_list_of_failures () {
  [ $# -gt 0 ] || return 0
  echo "  Failed (their failure lines are quoted below):"
  local failure
  for failure in "$@"; do
    echo "  - $failure"
  done; }

T_START=$SECONDS
BUILD_JOBS="$(skg_positive_int_or_default "${SKG_CARGO_BUILD_JOBS:-}" 2)"
NEXTEST_BUILD_JOBS="$(skg_positive_int_or_default \
  "${SKG_NEXTEST_BUILD_JOBS:-}" "$(skg_default_jobs 8)")"
NEXTEST_JOBS="$(skg_positive_int_or_default "${SKG_NEXTEST_JOBS:-}" 1)"
export SKG_FOR_AI_TEST_PORT="${SKG_FOR_AI_TEST_PORT:-$(
  skg_port_for_this_checkout 1793 "$PROJECT_ROOT")}"
echo -e "${BOLD}=== All Tests ===${NC}"
echo -e "${DIM}Build jobs: $BUILD_JOBS (nextest phase: $NEXTEST_BUILD_JOBS); nextest jobs: $NEXTEST_JOBS; integration: sequential${NC}"
echo -e "${DIM}AI reader test port: $SKG_FOR_AI_TEST_PORT${NC}"
echo ""

# ── Phase 1: Build ──────────────────────────────────────────────

echo -n "Building... "
T_BUILD=$SECONDS
if skg_low_priority cargo build --jobs "$BUILD_JOBS" --bin skg \
     >"$RESULTS_DIR/build.log" 2>&1; then
  ELAPSED_BUILD=$((SECONDS - T_BUILD))
  echo -e "${GREEN}OK${NC} ${DIM}(${ELAPSED_BUILD}s)${NC}"
else
  ELAPSED_BUILD=$((SECONDS - T_BUILD))
  echo -e "${RED}FAILED${NC}"
  echo ""
  cat "$RESULTS_DIR/build.log"
  {
    echo "$ORG_TITLE"
    org_list_of_failures "Build [[file:build.log]]"
    echo "** FAIL : Build (${ELAPSED_BUILD}s) [[file:build.log]]"
    echo "   #+begin_example"
    failure_lines_of_log "$RESULTS_DIR/build.log" '^error|^ *--> ' \
      | sed 's/^/   /'
    echo "   #+end_example"
  } > "$ORG"
  exit 1
fi

echo ""

# ── Phase 2: Run suites sequentially ────────────────────────────

# Each suite is a function, run from PROJECT_ROOT in a subshell by
# 'run_suite', which logs it to RESULTS_DIR. To add a suite, define
# its function and the pattern of its failure lines, and add one
# 'run_suite' line below; the summary and ALL.org list it from the
# SUITE_* arrays. Under a failed suite, ALL.org quotes the lines of
# its log that match that pattern ('failure_lines_of_log').

suite_emacs () {
  skg_low_priority "$SCRIPT_DIR/emacs-tests.sh"; }
EMACS_FAILURE_LINES='^Ran [0-9]+ tests|^[[:space:]]*FAILED[[:space:]]'

suite_nvim () {
  skg_low_priority "$SCRIPT_DIR/nvim-tests.sh"; }
NVIM_FAILURE_LINES='^(Fail[[:space:]]*\|\||Errors : [1-9])'

suite_nextest () {
  # --build-jobs: without it, cargo compiles/links with every core.
  # The test binaries are large (debug, ~quarter-GB each), so unbounded
  # parallel linking is the heaviest load a test run puts on the
  # machine; nice/ionice govern priority, not RAM.
  skg_low_priority cargo nextest run \
    --build-jobs "$NEXTEST_BUILD_JOBS" \
    --jobs "$NEXTEST_JOBS" --no-fail-fast; }
# 'panicked at' names the file and line of a failed assertion.
NEXTEST_FAILURE_LINES='^[[:space:]]*(Summary|FAIL|TIMEOUT|SIG[A-Z]+|ABORT) \[|panicked at '

suite_doctest () {
  # nextest structurally cannot run doctests, so they get their own
  # phase. --jobs caps build parallelism for politeness, matching the
  # build phase.
  skg_low_priority cargo test --doc --jobs "$BUILD_JOBS"; }
DOCTEST_FAILURE_LINES='\.\.\. FAILED$|^test result:'

suite_team_tools () {
  # The scripts in tools/team/ that the AI team runs, and those of
  # bash/ that have tests; each test makes its own data, and needs no
  # server.
  for test_script in tools/team/test_*.py bash/test_*.py; do
    echo "== $test_script"
    skg_low_priority python3 "$test_script" || exit 1
  done; }
TEAM_TOOLS_FAILURE_LINES='^== |^(FAIL|ERROR): '

suite_ai_reader () {
  # tools/skg-for-ai/: it runs target/debug/skg (built in phase 1)
  # on scratch servers at SKG_FOR_AI_TEST_PORT (set above). It runs
  # alone, like every suite here, so nothing rebuilds the binary or
  # takes that port under it; the integration tests' ports lie far
  # from it ('find_available_port'). A suite run at once from another
  # worktree uses another port ('skg_port_for_this_checkout').
  skg_low_priority python3 tools/skg-for-ai/test_skg_for_ai.py; }
AI_READER_FAILURE_LINES='^FAIL: '

suite_generated_docs () {
  # docs/ and coding-advice/ must be what skg-system/ exports
  # (coding-advice/generated-docs.org), with the binary of phase 1.
  SKG_BIN="$PROJECT_ROOT/target/debug/skg" \
    skg_low_priority bash/check-docs-exported.sh; }
GENERATED_DOCS_FAILURE_LINES='^(missing|differs from a fresh export|not exported from skg-system/): '

# Parallel arrays, one entry per suite run, in run order:
#   SUITE_LABELS[k], SUITE_LOGS[k], SUITE_FAILURE_PATTERNS[k],
#   SUITE_EXITS[k], SUITE_ELAPSED[k]
SUITE_LABELS=()
SUITE_LOGS=()
SUITE_FAILURE_PATTERNS=()
SUITE_EXITS=()
SUITE_ELAPSED=()

# Usage: run_suite LABEL LOG-FILE-NAME FUNCTION FAILURE-LINE-PATTERN
# The pattern is an extended regex ('grep -E').
run_suite () {
  local label="$1"
  local log_name="$2"
  local suite_function="$3"
  local failure_pattern="$4"
  local t_suite=$SECONDS
  local rc
  echo -n "  $label ... "
  if (
    cd "$PROJECT_ROOT"
    "$suite_function"
  ) >"$RESULTS_DIR/$log_name" 2>&1; then
    rc=0
    echo -e "${GREEN}PASS${NC} ${DIM}($((SECONDS - t_suite))s)${NC}"
  else
    rc=$?
    echo -e "${RED}FAIL${NC} ${DIM}($((SECONDS - t_suite))s)${NC}"
  fi
  SUITE_LABELS+=("$label")
  SUITE_LOGS+=("$log_name")
  SUITE_FAILURE_PATTERNS+=("$failure_pattern")
  SUITE_EXITS+=($rc)
  SUITE_ELAPSED+=($((SECONDS - t_suite))); }

echo -e "${YELLOW}Running suites sequentially...${NC}"
echo ""

run_suite "Emacs tests"     emacs.log          suite_emacs \
  "$EMACS_FAILURE_LINES"
run_suite "Neovim tests"    nvim.log           suite_nvim \
  "$NVIM_FAILURE_LINES"
run_suite "Nextest"         nextest.log        suite_nextest \
  "$NEXTEST_FAILURE_LINES"
run_suite "Doctests"        doctest.log        suite_doctest \
  "$DOCTEST_FAILURE_LINES"
run_suite "Team tool tests" team-tools.log     suite_team_tools \
  "$TEAM_TOOLS_FAILURE_LINES"
run_suite "AI reader tests" ai-reader.log      suite_ai_reader \
  "$AI_READER_FAILURE_LINES"
run_suite "Generated docs"  generated-docs.log suite_generated_docs \
  "$GENERATED_DOCS_FAILURE_LINES"

INTEGRATION_DIR="$PROJECT_ROOT/tests/integration"
INTEGRATION_NAMES=()   # each entry is "CLIENT/DIR-NAME"
INTEGRATION_STARTED=()
INTEGRATION_EXITS=()
INTEGRATION_ELAPSED=()
INTEGRATION_FAILED=()  # entries are "CLIENT/DIR-NAME"

# The lines of an integration run's log that say what failed: the
# tests' own FAIL and TIMEOUT lines ('test-nvim-lib.lua',
# 'test-wait.el'), their last phase, and the harness's ERROR lines
# ('test-lib.sh').
INTEGRATION_FAILURE_LINES='FAIL|TIMEOUT|ERROR|Last phase:|panicked'

# The log of one integration run. PREFIX is 'integration' for the
# primary run and 'retry' for the retry; NAME is "CLIENT/DIR-NAME".
integration_log_name () {
  local prefix="$1"
  local name="$2"
  echo "${prefix}-${name%%/*}-${name#*/}.log"; }

# Every integration directory runs once per client. The nvim pass is
# the second half of the wall-clock cost; skip it in a hurry with
# SKG_SKIP_NVIM_INTEGRATION=1.
INTEGRATION_CLIENTS=(emacs nvim)
if [ "${SKG_SKIP_NVIM_INTEGRATION:-0}" = "1" ]; then
  INTEGRATION_CLIENTS=(emacs)
fi

for client in "${INTEGRATION_CLIENTS[@]}"; do
  echo "  Integration tests ($client client) ..."
  for dir in "$INTEGRATION_DIR"/*/; do
    [ -f "$dir/run-test.sh" ] || continue
    if [ "$client" = "nvim" ] && [ ! -f "$dir/test-nvim.lua" ]; then
      continue
    fi
    name="$client/$(basename "$dir")"
    t_integration=$SECONDS
    INTEGRATION_NAMES+=("$name")
    INTEGRATION_STARTED+=($t_integration)
    echo -n "    $name ... "
    if (
      cd "$dir"
      SKG_TEST_CLIENT="$client" skg_low_priority ./run-test.sh
    ) >"$RESULTS_DIR/$(integration_log_name integration "$name")" 2>&1; then
      rc=0
      echo -e "${GREEN}PASS${NC} ${DIM}($((SECONDS - t_integration))s)${NC}"
    else
      rc=$?
      echo -e "${RED}FAIL${NC} ${DIM}($((SECONDS - t_integration))s)${NC}"
      INTEGRATION_FAILED+=("$name")
    fi
    INTEGRATION_EXITS+=($rc)
    INTEGRATION_ELAPSED+=($((SECONDS - t_integration)))
  done
done

echo ""

# ── Phase 3: Retry failed integration tests once ────────────────

# Parallel arrays for retry results:
#   RETRY_NAMES[j], RETRY_EXITS[j], RETRY_ELAPSED[j]
RETRY_NAMES=()
RETRY_EXITS=()
RETRY_ELAPSED=()
RETRY_STILL_FAILING=()

if [ ${#INTEGRATION_FAILED[@]} -gt 0 ]; then
  echo ""
  echo -e "${YELLOW}Retrying ${#INTEGRATION_FAILED[@]} failed integration test(s) once...${NC}"

  for name in "${INTEGRATION_FAILED[@]}"; do
    echo -n "  $name ... "

    # NAME is "CLIENT/DIR-NAME"; split it back apart.
    retry_client="${name%%/*}"
    retry_dir="${name#*/}"
    T_RETRY=$SECONDS
    (
      cd "$INTEGRATION_DIR/$retry_dir"
      SKG_TEST_CLIENT="$retry_client" skg_low_priority ./run-test.sh
    ) >"$RESULTS_DIR/$(integration_log_name retry "$name")" 2>&1
    rc=$?
    elapsed=$((SECONDS - T_RETRY))

    RETRY_NAMES+=("$name")
    RETRY_EXITS+=($rc)
    RETRY_ELAPSED+=($elapsed)

    if [ $rc -eq 0 ]; then
      echo -e "${GREEN}PASS (recovered on retry)${NC}"
    else
      echo -e "${RED}FAIL (real failure)${NC}"
      RETRY_STILL_FAILING+=("$name")
    fi
  done
fi

# ── Phase 4: Summary ───────────────────────────────────────────

echo ""
echo -e "${BOLD}=== Summary ===${NC}"

overall=0

for k in "${!SUITE_LABELS[@]}"; do
  if [ ${SUITE_EXITS[$k]} -eq 0 ]; then
    echo -e "  ${GREEN}PASS${NC}  ${SUITE_LABELS[$k]}"
  else
    echo -e "  ${RED}FAIL${NC}  $(printf '%-14s' "${SUITE_LABELS[$k]}") (see ${SUITE_LOGS[$k]})"
    overall=1
  fi
done

n_total=${#INTEGRATION_NAMES[@]}
n_failed=${#INTEGRATION_FAILED[@]}
n_real=${#RETRY_STILL_FAILING[@]}

if [ $n_failed -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Integration    ($n_total tests)"
elif [ $n_real -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Integration    ($n_total tests, $n_failed recovered on retry)"
else
  echo -e "  ${RED}FAIL${NC}  Integration    ($n_real real failure(s))"
  for name in "${RETRY_STILL_FAILING[@]}"; do
    echo -e "         ${RED}-${NC} $name  (see $(integration_log_name retry "$name"))"
  done
  overall=1
fi

echo ""
echo -e "${DIM}Logs: $RESULTS_DIR/${NC}"

# Beep
if [ "$SOUND" -eq 1 ]; then
  [ -x "$BEEP" ] && "$BEEP" beep-harsh 2>/dev/null
fi

T_TOTAL=$((SECONDS - T_START))

if [ $overall -eq 0 ]; then
  echo -e "${GREEN}${BOLD}All tests passed.${NC}  ${DIM}(${T_TOTAL}s total)${NC}"
else
  echo -e "${RED}${BOLD}Some tests failed.${NC}  ${DIM}(${T_TOTAL}s total)${NC}"
fi

# ── Write ALL.org ─────────────────────────────────────────────

FAILURES_WITH_LOGS=()   # each "WHAT [[file:LOG]]"
for k in "${!SUITE_LABELS[@]}"; do
  [ ${SUITE_EXITS[$k]} -eq 0 ] && continue
  FAILURES_WITH_LOGS+=("${SUITE_LABELS[$k]} [[file:${SUITE_LOGS[$k]}]]")
done
for name in "${RETRY_STILL_FAILING[@]}"; do
  FAILURES_WITH_LOGS+=("integration test $name [[file:$(integration_log_name retry "$name")]]")
done

{
  echo "$ORG_TITLE"
  org_list_of_failures "${FAILURES_WITH_LOGS[@]}"

  # Build
  if [ $ELAPSED_BUILD -ge 0 ]; then
    echo "** PASS : Build (${ELAPSED_BUILD}s) [[file:build.log]]"
  fi

  # The suites, in run order
  for k in "${!SUITE_LABELS[@]}"; do
    if [ ${SUITE_EXITS[$k]} -eq 0 ]; then
      verdict=PASS
    else
      verdict=FAIL
    fi
    echo "** $verdict : ${SUITE_LABELS[$k]} (${SUITE_ELAPSED[$k]}s) [[file:${SUITE_LOGS[$k]}]]"
    if [ "$verdict" = FAIL ]; then
      echo "   #+begin_example"
      failure_lines_of_log "$RESULTS_DIR/${SUITE_LOGS[$k]}" \
                           "${SUITE_FAILURE_PATTERNS[$k]}" \
        | sed 's/^/   /'
      echo "   #+end_example"
    fi
  done

  # Integration
  if [ $n_real -eq 0 ]; then
    echo "** PASS ($n_total of $n_total) : Integration tests"
  else
    echo "** FAIL ($n_real of $n_total) : Integration tests"
  fi

  # *** primary run — failures first, then passes
  echo "*** primary run"
  echo "    Failures are listed first."
  echo "    Names are CLIENT/DIRECTORY; every directory runs once per client."
  for i in "${!INTEGRATION_NAMES[@]}"; do
    [ ${INTEGRATION_EXITS[$i]} -ne 0 ] || continue
    name="${INTEGRATION_NAMES[$i]}"
    echo "**** FAIL  $name (${INTEGRATION_ELAPSED[$i]}s) [[file:$(integration_log_name integration "$name")]]"
  done
  for i in "${!INTEGRATION_NAMES[@]}"; do
    [ ${INTEGRATION_EXITS[$i]} -eq 0 ] || continue
    name="${INTEGRATION_NAMES[$i]}"
    echo "**** PASS  $name (${INTEGRATION_ELAPSED[$i]}s) [[file:$(integration_log_name integration "$name")]]"
  done

  # *** retry — only if there were primary-run failures
  if [ ${#RETRY_NAMES[@]} -eq 0 ]; then
    echo "*** SKIPPED : retry"
  else
    echo "*** retry"
    echo "    Failures are listed first."
    for j in "${!RETRY_NAMES[@]}"; do
      [ ${RETRY_EXITS[$j]} -ne 0 ] || continue
      name="${RETRY_NAMES[$j]}"
      echo "**** FAIL  $name (${RETRY_ELAPSED[$j]}s) [[file:$(integration_log_name retry "$name")]]"
      echo "     #+begin_example"
      failure_lines_of_log \
        "$RESULTS_DIR/$(integration_log_name retry "$name")" \
        "$INTEGRATION_FAILURE_LINES" \
        | sed 's/^/     /'
      echo "     #+end_example"
    done
    for j in "${!RETRY_NAMES[@]}"; do
      [ ${RETRY_EXITS[$j]} -eq 0 ] || continue
      name="${RETRY_NAMES[$j]}"
      echo "**** PASS  $name (${RETRY_ELAPSED[$j]}s) [[file:$(integration_log_name retry "$name")]]"
    done
  fi

} > "$ORG"

exit $overall
