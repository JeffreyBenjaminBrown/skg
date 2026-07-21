#!/bin/bash

# all-tests.sh — Run emacs, nextest, doctests, and integration tests politely.
#
# RESULTS ARE WRITTEN here:
# - tests/results/ALL.org: a plain-text org-mode summary
# - tests/results/*.log: individual test results
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

Runs Emacs, Neovim, nextest, doctests, and integration tests
sequentially with conservative internal concurrency. Integration tests
run once per client (emacs, then nvim), labeled CLIENT/DIRECTORY in the
output.

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
  SKG_TYPEDB_CONCURRENT_TRANSACTIONS
                             TypeDB operation fanout. Default: 4.
  SKG_TEST_NICE              nice level for test commands. Default: 15.
  SKG_TEST_IONICE=0          Disable idle I/O priority.
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

rm -rf "$RESULTS_DIR"
mkdir -p "$RESULTS_DIR"

T_START=$SECONDS
BUILD_JOBS="$(skg_positive_int_or_default "${SKG_CARGO_BUILD_JOBS:-}" 2)"
NEXTEST_BUILD_JOBS="$(skg_positive_int_or_default \
  "${SKG_NEXTEST_BUILD_JOBS:-}" "$(skg_default_jobs 8)")"
NEXTEST_JOBS="$(skg_positive_int_or_default "${SKG_NEXTEST_JOBS:-}" 1)"
export SKG_TYPEDB_CONCURRENT_TRANSACTIONS="$(
  skg_positive_int_or_default "${SKG_TYPEDB_CONCURRENT_TRANSACTIONS:-}" 4)"

echo -e "${BOLD}=== All Tests ===${NC}"
echo -e "${DIM}Build jobs: $BUILD_JOBS (nextest phase: $NEXTEST_BUILD_JOBS); nextest jobs: $NEXTEST_JOBS; integration: sequential; TypeDB fanout: $SKG_TYPEDB_CONCURRENT_TRANSACTIONS${NC}"
echo ""

# ── Phase 1: Build ──────────────────────────────────────────────

echo -n "Building... "
T_BUILD=$SECONDS
if skg_low_priority cargo build --jobs "$BUILD_JOBS" --bin skg --bin cleanup-test-dbs \
     >"$RESULTS_DIR/build.log" 2>&1; then
  ELAPSED_BUILD=$((SECONDS - T_BUILD))
  echo -e "${GREEN}OK${NC} ${DIM}(${ELAPSED_BUILD}s)${NC}"
else
  ELAPSED_BUILD=$((SECONDS - T_BUILD))
  echo -e "${RED}FAILED${NC}"
  echo ""
  cat "$RESULTS_DIR/build.log"
  exit 1
fi

"$PROJECT_ROOT/target/debug/cleanup-test-dbs" >/dev/null 2>&1 || true
echo ""

# ── Phase 2: Run suites sequentially ────────────────────────────

echo -e "${YELLOW}Running suites sequentially...${NC}"
echo ""

T_EMACS=$SECONDS
echo -n "  Emacs tests ... "
if (
  cd "$PROJECT_ROOT"
  skg_low_priority "$SCRIPT_DIR/emacs-tests.sh"
) >"$RESULTS_DIR/emacs.log" 2>&1; then
  EXIT_EMACS=0
  echo -e "${GREEN}PASS${NC} ${DIM}($((SECONDS - T_EMACS))s)${NC}"
else
  EXIT_EMACS=$?
  echo -e "${RED}FAIL${NC} ${DIM}($((SECONDS - T_EMACS))s)${NC}"
fi
ELAPSED_EMACS=$((SECONDS - T_EMACS))

T_NVIM=$SECONDS
echo -n "  Neovim tests ... "
if (
  cd "$PROJECT_ROOT"
  skg_low_priority "$SCRIPT_DIR/nvim-tests.sh"
) >"$RESULTS_DIR/nvim.log" 2>&1; then
  EXIT_NVIM=0
  echo -e "${GREEN}PASS${NC} ${DIM}($((SECONDS - T_NVIM))s)${NC}"
else
  EXIT_NVIM=$?
  echo -e "${RED}FAIL${NC} ${DIM}($((SECONDS - T_NVIM))s)${NC}"
fi
ELAPSED_NVIM=$((SECONDS - T_NVIM))

T_NEXTEST=$SECONDS
echo -n "  Nextest ... "
if (
  cd "$PROJECT_ROOT"
  # --build-jobs: without it, cargo compiles/links with every core.
  # The test binaries are large (debug, ~quarter-GB each), so unbounded
  # parallel linking is the heaviest load a test run puts on the
  # machine; nice/ionice govern priority, not RAM.
  skg_low_priority cargo nextest run \
    --build-jobs "$NEXTEST_BUILD_JOBS" \
    --jobs "$NEXTEST_JOBS" --no-fail-fast
) >"$RESULTS_DIR/nextest.log" 2>&1; then
  EXIT_NEXTEST=0
  echo -e "${GREEN}PASS${NC} ${DIM}($((SECONDS - T_NEXTEST))s)${NC}"
else
  EXIT_NEXTEST=$?
  echo -e "${RED}FAIL${NC} ${DIM}($((SECONDS - T_NEXTEST))s)${NC}"
fi
ELAPSED_NEXTEST=$((SECONDS - T_NEXTEST))

T_DOCTEST=$SECONDS
echo -n "  Doctests ... "
if (
  cd "$PROJECT_ROOT"
  # nextest structurally cannot run doctests, so they get their own
  # phase. They compile and run pure examples and never open TypeDB, so
  # they cannot hit the in-process collision that makes plain `cargo
  # test` flaky here -- no serialization needed. --jobs caps build
  # parallelism for politeness, matching the build phase.
  skg_low_priority cargo test --doc --jobs "$BUILD_JOBS"
) >"$RESULTS_DIR/doctest.log" 2>&1; then
  EXIT_DOCTEST=0
  echo -e "${GREEN}PASS${NC} ${DIM}($((SECONDS - T_DOCTEST))s)${NC}"
else
  EXIT_DOCTEST=$?
  echo -e "${RED}FAIL${NC} ${DIM}($((SECONDS - T_DOCTEST))s)${NC}"
fi
ELAPSED_DOCTEST=$((SECONDS - T_DOCTEST))

INTEGRATION_DIR="$PROJECT_ROOT/tests/integration"
INTEGRATION_NAMES=()   # each entry is "CLIENT/DIR-NAME"
INTEGRATION_STARTED=()
INTEGRATION_EXITS=()
INTEGRATION_ELAPSED=()
INTEGRATION_FAILED=()  # entries are "CLIENT/DIR-NAME"

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
    ) >"$RESULTS_DIR/integration-${client}-$(basename "$dir").log" 2>&1; then
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
    "$PROJECT_ROOT/target/debug/cleanup-test-dbs" >/dev/null 2>&1 || true
    echo -n "  $name ... "

    # NAME is "CLIENT/DIR-NAME"; split it back apart.
    retry_client="${name%%/*}"
    retry_dir="${name#*/}"
    T_RETRY=$SECONDS
    (
      cd "$INTEGRATION_DIR/$retry_dir"
      SKG_TEST_CLIENT="$retry_client" skg_low_priority ./run-test.sh
    ) >"$RESULTS_DIR/retry-${retry_client}-${retry_dir}.log" 2>&1
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

if [ $EXIT_EMACS -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Emacs tests"
else
  echo -e "  ${RED}FAIL${NC}  Emacs tests   (see emacs.log)"
  overall=1
fi

if [ $EXIT_NVIM -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Neovim tests"
else
  echo -e "  ${RED}FAIL${NC}  Neovim tests  (see nvim.log)"
  overall=1
fi

if [ $EXIT_NEXTEST -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Nextest"
else
  echo -e "  ${RED}FAIL${NC}  Nextest        (see nextest.log)"
  overall=1
fi

if [ $EXIT_DOCTEST -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Doctests"
else
  echo -e "  ${RED}FAIL${NC}  Doctests       (see doctest.log)"
  overall=1
fi

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
    echo -e "         ${RED}-${NC} $name  (see retry-${name%%/*}-${name#*/}.log)"
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

ORG="$RESULTS_DIR/ALL.org"

{
  echo "* results from bash/all-tests.sh"

  # Build
  if [ $ELAPSED_BUILD -ge 0 ]; then
    echo "** PASS : Build (${ELAPSED_BUILD}s) [[file:build.log]]"
  fi

  # Emacs
  if [ $EXIT_EMACS -eq 0 ]; then
    echo "** PASS : Emacs tests (${ELAPSED_EMACS}s) [[file:emacs.log]]"
  else
    echo "** FAIL : Emacs tests (${ELAPSED_EMACS}s) [[file:emacs.log]]"
  fi

  # Neovim
  if [ $EXIT_NVIM -eq 0 ]; then
    echo "** PASS : Neovim tests (${ELAPSED_NVIM}s) [[file:nvim.log]]"
  else
    echo "** FAIL : Neovim tests (${ELAPSED_NVIM}s) [[file:nvim.log]]"
  fi

  # Nextest
  if [ $EXIT_NEXTEST -eq 0 ]; then
    echo "** PASS : Nextest (${ELAPSED_NEXTEST}s) [[file:nextest.log]]"
  else
    echo "** FAIL : Nextest (${ELAPSED_NEXTEST}s) [[file:nextest.log]]"
  fi

  # Doctests
  if [ $EXIT_DOCTEST -eq 0 ]; then
    echo "** PASS : Doctests (${ELAPSED_DOCTEST}s) [[file:doctest.log]]"
  else
    echo "** FAIL : Doctests (${ELAPSED_DOCTEST}s) [[file:doctest.log]]"
  fi

  # Integration
  n_pass=$((n_total - n_real))
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
    logname="integration-${name%%/*}-${name#*/}.log"
    echo "**** FAIL  $name (${INTEGRATION_ELAPSED[$i]}s) [[file:$logname]]"
  done
  for i in "${!INTEGRATION_NAMES[@]}"; do
    [ ${INTEGRATION_EXITS[$i]} -eq 0 ] || continue
    name="${INTEGRATION_NAMES[$i]}"
    logname="integration-${name%%/*}-${name#*/}.log"
    echo "**** PASS  $name (${INTEGRATION_ELAPSED[$i]}s) [[file:$logname]]"
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
      logname="retry-${name%%/*}-${name#*/}.log"
      echo "**** FAIL  $name (${RETRY_ELAPSED[$j]}s) [[file:$logname]]"
    done
    for j in "${!RETRY_NAMES[@]}"; do
      [ ${RETRY_EXITS[$j]} -eq 0 ] || continue
      name="${RETRY_NAMES[$j]}"
      logname="retry-${name%%/*}-${name#*/}.log"
      echo "**** PASS  $name (${RETRY_ELAPSED[$j]}s) [[file:$logname]]"
    done
  fi

} > "$ORG"

exit $overall
