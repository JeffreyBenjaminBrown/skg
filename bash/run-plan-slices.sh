#!/usr/bin/env bash
# Run the slices of TODO/total-diff-view/2_plan.org unattended, one FRESH
# Claude Code session per slice, so that each slice starts with an empty
# context. The plan, progress.org, git and the auto-memory directory are
# the memory between slices.
#
# Usage: bash/run-plan-slices.sh [FIRST_SLICE] [LAST_SLICE]   (defaults 0 9)
#   SKG_PLAN_MODEL  (default claude-opus-5-5, i.e. Opus 5.5)  and
#   SKG_PLAN_EFFORT (default high)  override what is passed to claude.
#
# Run it from a plain shell in the container, not from inside a Claude
# Code session (claude refuses to nest). Detach it, e.g.
#   nohup bash/run-plan-slices.sh > TODO/total-diff-view/run-logs/run.out 2>&1 &
#
# The loop stops when a slice ends without its headline marked DONE (the
# agent stopped for a question; read progress.org) or when claude exits
# nonzero. Every slice's session id is logged, so a stopped slice can be
# continued WITH its context:  claude --resume <session-id>
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

PLAN=TODO/total-diff-view/2_plan.org
PLAN_DIR=$(dirname "$PLAN")
LOG_DIR=$PLAN_DIR/run-logs
MODEL="${SKG_PLAN_MODEL:-claude-opus-5-5}"
EFFORT="${SKG_PLAN_EFFORT:-high}"
FIRST="${1:-0}"
LAST="${2:-9}"
mkdir -p "$LOG_DIR"

if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "run-plan-slices: tracked files have uncommitted changes; commit or stash them first." >&2
  exit 3
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
  prompt="Read coding-advice/ORIENTATION.org and the advice it names for code changes, tests and execution. Then implement slice $n of $PLAN, and only that slice. If $PLAN_DIR/progress.org exists, read it first. The plan is a settled specification; do not reopen its decisions or edit its text except the keyword of the slice headline. When the slice's tests pass and its commits are made, change the slice's headline keyword in $PLAN to DONE and commit that change too. Resolve ordinary implementation choices yourself; stopping is only for the rare conditions that coding-advice/when-to-stop.org describes. If one of those arises, write the report into $PLAN_DIR/progress.org as that file describes, do not mark the slice DONE, and end your turn."
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
done
echo "all requested slices are DONE"
