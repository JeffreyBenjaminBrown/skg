#!/usr/bin/env bash
# claude-usage-poller.sh — background daemon that refreshes a usage snapshot
# on a fixed cadence, so many agents can read current usage WITHOUT each
# spending an API call.
#
#   claude-usage-poller.sh [interval_seconds] [out_file]
#   defaults: 180s cadence, snapshot at $HOME/.claude-usage.json
#
# Run it detached, e.g.:   nohup ./claude-usage-poller.sh &>/dev/null &
# Agents then just read the JSON file (free). The file gains a "polled_at"
# epoch so a reader can tell how stale the snapshot is.
#
# Trade: instead of every agent-check costing a call, ONE poller spends a
# tiny max_tokens=1 Haiku call every `interval`. Over a 5h window at 180s
# that's ~100 negligible calls.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
INTERVAL="${1:-180}"
OUT="${2:-$HOME/.claude-usage.json}"

while true; do
  if snap=$("$HERE/claude-usage.sh" 2>/dev/null); then
    printf '%s\n' "$snap" \
      | python3 -c "import sys,json,time;d=json.load(sys.stdin);d['polled_at']=int(time.time());print(json.dumps(d,indent=2))" \
      > "$OUT.tmp" && mv "$OUT.tmp" "$OUT"   # atomic: readers never see a half-written file
  fi
  # If the probe failed (e.g. token expired), keep the last good snapshot;
  # its stale polled_at is the signal. See coding-advice on why we don't refresh.
  sleep "$INTERVAL"
done
