#!/usr/bin/env bash

# Lightweight behavioral checks for bash/quiet-test.sh.  The wrapper's own raw
# logs are intentionally retained under target/test-logs/graph-only/.

set -euo pipefail

readonly TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly REPO_ROOT="$(cd "$TEST_DIR/.." && pwd)"
readonly WRAPPER="$REPO_ROOT/bash/quiet-test.sh"

fail() {
  printf 'quiet-test wrapper self-test: %s\n' "$*" >&2
  exit 1
}

pass_output="$(mktemp "${TMPDIR:-/tmp}/quiet-test-pass.XXXXXX")"
fail_output="$(mktemp "${TMPDIR:-/tmp}/quiet-test-fail.XXXXXX")"
quote_output="$(mktemp "${TMPDIR:-/tmp}/quiet-test-quote.XXXXXX")"
trap 'rm -f "$pass_output" "$fail_output" "$quote_output"' EXIT

"$WRAPPER" --label quiet-wrapper-self-pass --scope bash --scope tests -- \
  bash -c 'printf "test result: ok. 1 passed; 0 failed; 0 ignored\\n"' \
  >"$pass_output"
[ "$(wc -l <"$pass_output")" -eq 1 ] || fail 'successful command was not one summary line'
grep -q '^PASS label=quiet-wrapper-self-pass ' "$pass_output" || fail 'success summary missing'

pass_meta="$(sed -n 's/.* meta=//p' "$pass_output")"
[ -f "$pass_meta" ] || fail 'success metadata was not retained'
grep -q '^FINGERPRINT: [0-9a-f]\{64\}$' "$pass_meta" || fail 'metadata lacks SHA-256 fingerprint'
grep -q '^COMMAND: bash ' "$pass_meta" || fail 'metadata lacks quoted command'

if "$WRAPPER" --label quiet-wrapper-self-fail --scope bash --scope tests -- \
  bash -c 'printf "error: deliberately failing wrapper test\\n"; exit 17' \
  >"$fail_output" 2>&1; then
  fail 'failing command returned success'
fi
grep -q '^FAIL label=quiet-wrapper-self-fail exit=17 ' "$fail_output" || fail 'failure summary missing'
grep -q '^FIRST_ACTIONABLE ' "$fail_output" || fail 'failure lacks bounded diagnostic'
grep -q 'error: deliberately failing wrapper test' "$fail_output" || fail 'failure diagnostic lacks cause'
[ "$(wc -l <"$fail_output")" -le 30 ] || fail 'failure output exceeded bounded diagnostic limit'

odd_argument="spaces ; dollar \\$ and apostrophe '"
"$WRAPPER" --label quiet-wrapper-self-quoting --scope bash --scope tests -- \
  bash -c 'printf "%s\\n" "$1"' quiet-test "$odd_argument" \
  >"$quote_output"
quote_log="$(sed -n 's/.* log=\([^ ]*\) meta=.*/\1/p' "$quote_output")"
[ -f "$quote_log" ] || fail 'quoted-command log was not retained'
grep -Fqx "$odd_argument" "$quote_log" || fail 'argument-array quoting changed command input'

printf 'quiet-test wrapper self-test: PASS\n'
