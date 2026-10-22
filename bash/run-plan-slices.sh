#!/usr/bin/env bash
# Run the slices of TODO/total-diff-view/15_plan.org unattended, one
# fresh Claude Code session per slice, so that each slice starts with an
# empty context. The plan, progress.org, git and the auto-memory
# directory are the memory between slices.
#
# Usage: bash/run-plan-slices.sh [FIRST_SLICE] [LAST_SLICE]   (defaults 0 12)
#   SKG_PLAN_MODEL  (default claude-opus-5-5, i.e. Opus 5.5)  and
#   SKG_PLAN_EFFORT (default high)  override what is passed to claude.
# More elaborate:
#   nohup bash/run-plan-slices.sh > TODO/total-diff-view/run-logs/run.out 2>&1 &
#
# Run it from a plain shell in the container, not from inside a Claude
# Code session (claude refuses to nest). Detach it, e.g.
#   nohup bash/run-plan-slices.sh > TODO/total-diff-view/run-logs/run.out 2>&1 &
#
# Commit the plan first: each slice's last commit edits the plan (its
# DONE keyword rides in that commit, never in one of its own). Tracked
# files with uncommitted changes do not stop the run: they are taken to
# be the work a stopped earlier slice left behind, the agent is told
# which files they are, and it commits them with its slice. So commit
# or stash edits of your own before starting.
#
# The loop stops when a slice ends without its headline marked DONE (the
# agent stopped for a question; read progress.org) or when claude exits
# nonzero. Every slice's session id is logged, so a stopped slice can be
# continued with its context:  claude --resume <session-id>
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

PLAN=TODO/total-diff-view/15_plan.org
PROGRESS_HEADLINE='Fixes, features and vocabulary'
PLAN_DIR=$(dirname "$PLAN")
LOG_DIR=$PLAN_DIR/run-logs
MODEL="${SKG_PLAN_MODEL:-claude-opus-5-5}"
EFFORT="${SKG_PLAN_EFFORT:-high}"
FIRST="${1:-0}"
LAST="${2:-12}"
mkdir -p "$LOG_DIR"

# Modified tracked files, space-separated; empty on a clean tree. (awk, not
# grep: under 'set -o pipefail' a grep that matches nothing would exit 1
# and 'set -e' would end the script before it started.)
dirty_files=$(git status --porcelain | awk '!/^\?\?/ { print substr($0, 4) }' | tr '\n' ' ')
dirty_note=""
if [ -n "$dirty_files" ]; then
  echo "run-plan-slices: tracked files are modified and will be handed to the first slice run: $dirty_files" >&2
  dirty_note=" Tracked files with uncommitted changes when you started: $dirty_files. Unless $PLAN_DIR/progress.org says otherwise, they are the work a stopped earlier run of this slice left behind; keep what the plan says to keep, finish the slice on top of them, and commit them with it."
fi

slice_done () { grep -qE "^\*\* DONE Slice $1:" "$PLAN"; }

new_uuid () {
  if command -v uuidgen >/dev/null 2>&1; then uuidgen
  else python3 -c 'import uuid; print(uuid.uuid4())'; fi
}

for n in $(seq "$FIRST" "$LAST"); do
  if slice_done "$n"; then
    echo "slice $n is already DONE; skipping"
    continue
  fi
  session=$(new_uuid)
  echo "$(date -Is) slice $n  session $session" | tee -a "$LOG_DIR/sessions.log"
  prompt="Read coding-advice/ORIENTATION.org and the advice it names for code changes, tests and execution. Then implement slice $n of $PLAN, and only that slice. Read $PLAN_DIR/progress.org first if it exists.$dirty_note The plan is a settled specification; do not reopen its decisions or edit its text except the keyword of the slice headline. Commit as the plan's Commits section says: the slice's last code-modifying commit also carries the slice's documentation changes and the change of its headline in $PLAN from TODO to DONE. Never make a separate commit for those; if something is left over after your last code commit, amend that commit. Resolve ordinary implementation choices yourself; stopping is only for the rare conditions that coding-advice/when-to-stop.org describes. If one of those arises, write the report into $PLAN_DIR/progress.org under its top-level headline '$PROGRESS_HEADLINE', do not mark the slice DONE, and end your turn. Write any log or scratch file of your own under tests/results/, never under TODO/."
  if ! claude -p "$prompt" \
         --model "$MODEL" --effort "$EFFORT" \
         --session-id "$session" \
         --dangerously-skip-permissions \
         --output-format json \
         > "$LOG_DIR/slice-$n.json" 2> "$LOG_DIR/slice-$n.err"; then
    echo "slice $n: claude exited nonzero. See $LOG_DIR/slice-$n.err. Resume with: claude --resume $session" >&2
    exit 1
  fi
  jq -r '.result // empty' "$LOG_DIR/slice-$n.json" > "$LOG_DIR/slice-$n.txt" || true
  if ! slice_done "$n"; then
    echo "slice $n ended without DONE. Read $PLAN_DIR/progress.org and $LOG_DIR/slice-$n.txt. Resume with: claude --resume $session" >&2
    exit 2
  fi
  echo "$(date -Is) slice $n DONE" | tee -a "$LOG_DIR/sessions.log"
  dirty_note="" # whatever was handed over is committed, or the loop has stopped
done
echo "all requested slices are DONE"
