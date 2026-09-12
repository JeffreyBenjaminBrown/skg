#!/usr/bin/env bash

# Run one test/build command while retaining its complete output out of the
# conversation.  This is deliberately a small argument-array runner, not a
# replacement for bash/all-tests.sh: it never cleans state, retries, or widens
# the requested command.
#
# Usage:
#   bash/quiet-test.sh --label NAME [--scope PATH]... -- command arg...
#
# A fingerprint contains HEAD, the tracked diff within each scope, and the
# names plus SHA-256 values of untracked regular files within each scope.
# Untracked files outside the supplied scopes are listed in metadata but not
# read or fingerprinted.  Omit --scope to use the initiative's normal source
# surface; pass --scope . only when a whole-worktree fingerprint is wanted.

set -u -o pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
readonly LOG_DIR="$REPO_ROOT/target/test-logs/graph-only"

usage() {
  cat <<'EOF'
Usage: bash/quiet-test.sh [OPTIONS] -- COMMAND [ARG...]

Run one command with stdout/stderr captured under target/test-logs/graph-only.

Options:
  --label NAME   Short descriptive name used in filenames and summaries.
  --scope PATH   Repo-relative path whose tracked and untracked state belongs
                 in the fingerprint. Repeatable. Default: graph-refactor
                 source/test/documentation surface.
  -h, --help     Show this help.

The command is executed in the caller's current directory.  The repository is
located from this script, so the wrapper also works for integration commands
run from a subdirectory.
EOF
}

die() {
  printf 'quiet-test: %s\n' "$*" >&2
  exit 2
}

label=""
declare -a scopes=()

while [ "$#" -gt 0 ]; do
  case "$1" in
    --label)
      [ "$#" -ge 2 ] || die '--label needs a value'
      label="$2"
      shift 2
      ;;
    --scope)
      [ "$#" -ge 2 ] || die '--scope needs a repo-relative path'
      case "$2" in
        /*|..|../*|*/../*|*/..)
          die "scope must be inside the repository: $2"
          ;;
      esac
      scopes+=("$2")
      shift 2
      ;;
    --)
      shift
      break
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      die "unknown option before --: $1"
      ;;
  esac
done

[ "$#" -gt 0 ] || die 'supply a command after --'

if [ "${#scopes[@]}" -eq 0 ]; then
  # Keep routine fingerprints relevant to this initiative while avoiding an
  # expensive or privacy-surprising read of unrelated untracked worktrees.
  scopes=(
    Cargo.toml Cargo.lock
    server tests tools bash elisp nvim docs data windows example-data
    README.org api-and-formats.md glossary.md
  )
fi

if [ -z "$label" ]; then
  label="$(basename "$1")"
fi
safe_label="$(printf '%s' "$label" | tr -cs '[:alnum:].+_-' '_')"
[ -n "$safe_label" ] || safe_label="command"

for scope in "${scopes[@]}"; do
  [ -e "$REPO_ROOT/$scope" ] || die "scope does not exist: $scope"
done

hash_file() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum -- "$1" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 -- "$1" | awk '{print $1}'
  else
    die 'need sha256sum or shasum to fingerprint test evidence'
  fi
}

path_is_scoped() {
  local candidate="$1"
  local scope

  # Windows package assembly writes a complete copy of selected repository
  # files here.  It is derived build output, so including it would make an
  # otherwise identical checkpoint depend on whether packaging ran first.
  if [ "$candidate" = "windows/out" ] ||
     [[ "$candidate" == windows/out/* ]]; then
    return 1
  fi

  for scope in "${scopes[@]}"; do
    if [ "$scope" = "." ] || [ "$candidate" = "$scope" ] ||
       [[ "$candidate" == "$scope/"* ]]; then
      return 0
    fi
  done
  return 1
}

mkdir -p "$LOG_DIR"
timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
base="$LOG_DIR/${timestamp}-${safe_label}-$$"
log_path="${base}.log"
meta_path="${base}.meta"
fingerprint_payload="$(mktemp "${TMPDIR:-/tmp}/skg-quiet-test.XXXXXX")"
trap 'rm -f "$fingerprint_payload"' EXIT

declare -a included_untracked=()
declare -a excluded_untracked=()
while IFS= read -r -d '' path; do
  if path_is_scoped "$path"; then
    included_untracked+=("$path")
  else
    excluded_untracked+=("$path")
  fi
done < <(git -C "$REPO_ROOT" ls-files --others --exclude-standard -z)

{
  printf 'quiet-test-fingerprint-v1\n'
  printf 'HEAD %s\n' "$(git -C "$REPO_ROOT" rev-parse HEAD)"
  for scope in "${scopes[@]}"; do
    printf 'SCOPE %q\n' "$scope"
    git -C "$REPO_ROOT" diff --no-ext-diff --binary HEAD -- "$scope"
  done
  for path in "${included_untracked[@]}"; do
    if [ -f "$REPO_ROOT/$path" ]; then
      printf 'UNTRACKED_FILE %q %s\n' "$path" "$(hash_file "$REPO_ROOT/$path")"
    elif [ -L "$REPO_ROOT/$path" ]; then
      printf 'UNTRACKED_SYMLINK %q %q\n' "$path" "$(readlink "$REPO_ROOT/$path")"
    else
      printf 'UNTRACKED_OTHER %q\n' "$path"
    fi
  done
} >"$fingerprint_payload"
fingerprint="$(hash_file "$fingerprint_payload")"

started_iso="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
started_epoch="$(date +%s)"
set +e
"$@" >"$log_path" 2>&1
status=$?
set -e
finished_iso="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
duration=$(( $(date +%s) - started_epoch ))

{
  printf 'FORMAT: quiet-test-v1\n'
  printf 'LABEL: %s\n' "$label"
  printf 'STARTED_UTC: %s\n' "$started_iso"
  printf 'FINISHED_UTC: %s\n' "$finished_iso"
  printf 'DURATION_SECONDS: %s\n' "$duration"
  printf 'EXIT_STATUS: %s\n' "$status"
  printf 'REPOSITORY: %s\n' "$REPO_ROOT"
  printf 'WORKING_DIRECTORY: %s\n' "$PWD"
  printf 'HEAD: %s\n' "$(git -C "$REPO_ROOT" rev-parse HEAD)"
  printf 'FINGERPRINT: %s\n' "$fingerprint"
  printf 'FINGERPRINT_SCOPES:'
  printf ' %q' "${scopes[@]}"
  printf '\n'
  printf 'COMMAND:'
  printf ' %q' "$@"
  printf '\n'
  printf 'RAW_LOG: %s\n' "$log_path"
  printf 'UNTRACKED_INCLUDED:'
  if [ "${#included_untracked[@]}" -eq 0 ]; then
    printf ' none'
  else
    printf ' %q' "${included_untracked[@]}"
  fi
  printf '\nUNTRACKED_EXCLUDED:'
  if [ "${#excluded_untracked[@]}" -eq 0 ]; then
    printf ' none'
  else
    printf ' %q' "${excluded_untracked[@]}"
  fi
  printf '\n'
} >"$meta_path"

summary="$(grep -E 'test result:|[0-9]+ examples, [0-9]+ failures|Finished .*profile' "$log_path" | tail -n 1 || true)"
summary="${summary//$'\r'/}"
if [ -z "$summary" ]; then
  summary='command completed'
fi
summary="${summary:0:240}"

if [ "$status" -eq 0 ]; then
  printf 'PASS label=%s exit=0 duration=%ss summary=%q log=%s meta=%s\n' \
    "$label" "$duration" "$summary" "$log_path" "$meta_path"
  exit 0
fi

printf 'FAIL label=%s exit=%s duration=%ss log=%s meta=%s\n' \
  "$label" "$status" "$duration" "$log_path" "$meta_path"

failure_names="$(awk '
  /^failures:$/ { in_failures=1; next }
  in_failures && /^test result:/ { exit }
  in_failures && /^[[:space:]]+[[:alnum:]_:.-]+$/ {
    gsub(/^[[:space:]]+|[[:space:]]+$/, "")
    print
    count += 1
    if (count == 10) exit
  }
' "$log_path")"
if [ -n "$failure_names" ]; then
  printf 'FAILURES: %s\n' "$(printf '%s' "$failure_names" | paste -sd, -)"
fi

first_line="$(grep -n -m 1 -E 'error(\[[^]]+\])?:|panicked at|^FAILED|^failures:$|AssertionError|^[[:space:]]*E[0-9]+:' "$log_path" | cut -d: -f1 || true)"
if [ -n "$first_line" ]; then
  last_line=$((first_line + 23))
  printf 'FIRST_ACTIONABLE (maximum 24 lines):\n'
  sed -n "${first_line},${last_line}p" "$log_path"
else
  printf 'FIRST_ACTIONABLE (last 24 lines; no standard error marker found):\n'
  tail -n 24 "$log_path"
fi
printf 'RAW_LOG: %s\n' "$log_path"
exit "$status"
