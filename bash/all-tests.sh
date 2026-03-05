#!/bin/bash

# all-tests.sh — Run emacs, nextest, and integration tests in parallel.
#
# RESULTS ARE WRITTEN here:
# - tests/results/ALL.org: a plain-text org-mode summary
# - tests/results/*.log: individual test results
#
# INTEGRATION TEST FAILURES ARE RETRIED:
# First they are all run concurrently.
# If any fail, this re-runs the failures in isolation
# to distinguish real failures from concurrency collisions.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_DIR="$PROJECT_ROOT/tests/results"
BEEP="$PROJECT_ROOT/../sound/play-beep.sh"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

rm -rf "$RESULTS_DIR"
mkdir -p "$RESULTS_DIR"

T_START=$SECONDS

echo -e "${BOLD}=== All Tests ===${NC}"
echo ""

# ── Phase 1: Build ──────────────────────────────────────────────

echo -n "Building... "
T_BUILD=$SECONDS
if cargo build --bin skg --bin cleanup-test-dbs \
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

# ── Phase 2: Run all suites in parallel ─────────────────────────

echo -e "${YELLOW}Running all suites in parallel...${NC}"
echo ""

# 2a. Emacs tests
(
  cd "$PROJECT_ROOT"
  "$SCRIPT_DIR/emacs-tests.sh"
) >"$RESULTS_DIR/emacs.log" 2>&1 &
PID_EMACS=$!
T_EMACS=$SECONDS
echo "  Emacs tests       (pid $PID_EMACS)"

# 2b. Nextest
(
  cd "$PROJECT_ROOT"
  cargo nextest run --no-fail-fast
) >"$RESULTS_DIR/nextest.log" 2>&1 &
PID_NEXTEST=$!
T_NEXTEST=$SECONDS
echo "  Nextest            (pid $PID_NEXTEST)"

# 2c. Integration tests — run each one in parallel
INTEGRATION_DIR="$PROJECT_ROOT/tests/integration"
INTEGRATION_NAMES=()
INTEGRATION_PIDS=()

for dir in "$INTEGRATION_DIR"/*/; do
  [ -f "$dir/run-test.sh" ] || continue
  name="$(basename "$dir")"
  INTEGRATION_NAMES+=("$name")
  (
    cd "$dir"
    ./run-test.sh
  ) >"$RESULTS_DIR/integration-$name.log" 2>&1 &
  INTEGRATION_PIDS+=($!)
done
T_INTEGRATION=$SECONDS

echo "  Integration tests  (${#INTEGRATION_NAMES[@]} tests)"
echo ""

# ── Phase 3: Collect results ────────────────────────────────────

echo -e "${YELLOW}Waiting for results...${NC}"

wait_for () {
  # Usage: wait_for PID LABEL T_STARTED
  # Sets global LAST_EXIT
  local pid="$1" label="$2" t_started="$3"
  wait "$pid" 2>/dev/null
  LAST_EXIT=$?
  local elapsed=$((SECONDS - t_started))
  if [ $LAST_EXIT -eq 0 ]; then
    echo -e "  ${GREEN}PASS${NC}  $label  ${DIM}(${elapsed}s)${NC}"
  else
    echo -e "  ${RED}FAIL${NC}  $label  ${DIM}(${elapsed}s)${NC}"
  fi
}

wait_for $PID_EMACS "Emacs tests" $T_EMACS
EXIT_EMACS=$LAST_EXIT
ELAPSED_EMACS=$((SECONDS - T_EMACS))

wait_for $PID_NEXTEST "Nextest" $T_NEXTEST
EXIT_NEXTEST=$LAST_EXIT
ELAPSED_NEXTEST=$((SECONDS - T_NEXTEST))

# Collect integration results into parallel arrays:
#   INTEGRATION_EXITS[i]   — exit code from concurrent run
#   INTEGRATION_ELAPSED[i] — elapsed time from concurrent run
INTEGRATION_EXITS=()
INTEGRATION_ELAPSED=()
INTEGRATION_FAILED=()

for i in "${!INTEGRATION_PIDS[@]}"; do
  name="${INTEGRATION_NAMES[$i]}"
  wait_for "${INTEGRATION_PIDS[$i]}" "Integration / $name" $T_INTEGRATION
  INTEGRATION_EXITS+=($LAST_EXIT)
  INTEGRATION_ELAPSED+=($((SECONDS - T_INTEGRATION)))
  if [ $LAST_EXIT -ne 0 ]; then
    INTEGRATION_FAILED+=("$name")
  fi
done

# ── Phase 4: Retry failed integration tests in isolation ────────

# Parallel arrays for retry results (only for tests that failed concurrently):
#   RETRY_NAMES[j], RETRY_EXITS[j], RETRY_ELAPSED[j]
RETRY_NAMES=()
RETRY_EXITS=()
RETRY_ELAPSED=()
RETRY_STILL_FAILING=()

if [ ${#INTEGRATION_FAILED[@]} -gt 0 ]; then
  echo ""
  echo -e "${YELLOW}Retrying ${#INTEGRATION_FAILED[@]} failed integration test(s) in isolation...${NC}"

  for name in "${INTEGRATION_FAILED[@]}"; do
    "$PROJECT_ROOT/target/debug/cleanup-test-dbs" >/dev/null 2>&1 || true
    echo -n "  $name ... "

    T_RETRY=$SECONDS
    (
      cd "$INTEGRATION_DIR/$name"
      ./run-test.sh
    ) >"$RESULTS_DIR/retry-$name.log" 2>&1
    rc=$?
    elapsed=$((SECONDS - T_RETRY))

    RETRY_NAMES+=("$name")
    RETRY_EXITS+=($rc)
    RETRY_ELAPSED+=($elapsed)

    if [ $rc -eq 0 ]; then
      echo -e "${GREEN}PASS (was a collision)${NC}"
    else
      echo -e "${RED}FAIL (real failure)${NC}"
      RETRY_STILL_FAILING+=("$name")
    fi
  done
fi

# ── Phase 5: Summary ───────────────────────────────────────────

echo ""
echo -e "${BOLD}=== Summary ===${NC}"

overall=0

if [ $EXIT_EMACS -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Emacs tests"
else
  echo -e "  ${RED}FAIL${NC}  Emacs tests   (see emacs.log)"
  overall=1
fi

if [ $EXIT_NEXTEST -eq 0 ]; then
  echo -e "  ${GREEN}PASS${NC}  Nextest"
else
  echo -e "  ${RED}FAIL${NC}  Nextest        (see nextest.log)"
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
    echo -e "         ${RED}-${NC} $name  (see retry-$name.log)"
  done
  overall=1
fi

echo ""
echo -e "${DIM}Logs: $RESULTS_DIR/${NC}"

# Beep
[ -x "$BEEP" ] && "$BEEP" 2>/dev/null

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

  # Nextest
  if [ $EXIT_NEXTEST -eq 0 ]; then
    echo "** PASS : Nextest (${ELAPSED_NEXTEST}s) [[file:nextest.log]]"
  else
    echo "** FAIL : Nextest (${ELAPSED_NEXTEST}s) [[file:nextest.log]]"
  fi

  # Integration
  n_pass=$((n_total - n_real))
  if [ $n_real -eq 0 ]; then
    echo "** PASS ($n_total of $n_total) : Integration tests"
  else
    echo "** FAIL ($n_real of $n_total) : Integration tests"
  fi

  # *** concurrently — failures first, then passes
  echo "*** concurrently"
  echo "    Failures are listed first."
  for i in "${!INTEGRATION_NAMES[@]}"; do
    [ ${INTEGRATION_EXITS[$i]} -ne 0 ] || continue
    name="${INTEGRATION_NAMES[$i]}"
    echo "**** FAIL  $name (${INTEGRATION_ELAPSED[$i]}s) [[file:integration-$name.log]]"
  done
  for i in "${!INTEGRATION_NAMES[@]}"; do
    [ ${INTEGRATION_EXITS[$i]} -eq 0 ] || continue
    name="${INTEGRATION_NAMES[$i]}"
    echo "**** PASS  $name (${INTEGRATION_ELAPSED[$i]}s) [[file:integration-$name.log]]"
  done

  # *** in isolation — only if there were concurrent failures to retry
  if [ ${#RETRY_NAMES[@]} -eq 0 ]; then
    echo "*** SKIPPED : in isolation"
  else
    echo "*** in isolation"
    echo "    Failures are listed first."
    for j in "${!RETRY_NAMES[@]}"; do
      [ ${RETRY_EXITS[$j]} -ne 0 ] || continue
      name="${RETRY_NAMES[$j]}"
      echo "**** FAIL  $name (${RETRY_ELAPSED[$j]}s) [[file:retry-$name.log]]"
    done
    for j in "${!RETRY_NAMES[@]}"; do
      [ ${RETRY_EXITS[$j]} -eq 0 ] || continue
      name="${RETRY_NAMES[$j]}"
      echo "**** PASS  $name (${RETRY_ELAPSED[$j]}s) [[file:retry-$name.log]]"
    done
  fi

} > "$ORG"

exit $overall
