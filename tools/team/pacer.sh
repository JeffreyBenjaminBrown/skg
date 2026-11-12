#!/usr/bin/env bash
# pacer.sh -- wake a fresh Claude, one session at a time, at the pace
# Jeff's subscription allows.
#
#   tools/team/pacer.sh run      # the loop; run it detached (nohup ... &)
#   tools/team/pacer.sh pause    # no new session starts (a running one finishes)
#   tools/team/pacer.sh resume
#   tools/team/pacer.sh stop     # ends the loop after the running session
#   tools/team/pacer.sh status   # usage, the pace, and whether it would start one now
#   tools/team/pacer.sh allow F  # until the week is F used (0.16, say), ignore the line: a burst Jeff asked for
#   tools/team/pacer.sh once     # one session now, whatever the pace says
#
# WHY. Jeff asks that Claude spend the whole subscription, evenly, and
# manage its own sleep. A session's cost is mostly its context, so many
# short sessions that each boot from Claude's graph cost less, and
# forget less, than one long one. Each session this starts is told
# only to wake as the root note of the graph says; the work it does is
# whatever is first in the graph's queue.
#
# THE PACE. The subscription has a five-hour window and a weekly one
# (bash/claude-usage.sh reads both). A session starts only if
#   - neither PAUSE nor STOP exists in data/claude/pacer/,
#   - the five-hour window is under FIVE_HOUR_CAP used, so that Jeff
#     is never locked out when he sits down, and
#   - the weekly window is behind the straight line from where it was
#     when this week's pacing began to (1 - RESERVE) at the weekly
#     reset. RESERVE is left for Jeff's own use.
# The line's starting point (the anchor) is recorded when the pacer
# first sees a week, in data/claude/pacer/anchor.
#
# ONE AT A TIME. Sessions run in the foreground of the loop, so two
# never overlap. An interactive Claude should 'pause' first: two
# sessions writing one graph would refuse each other's saves.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DIR="$ROOT/data/claude/pacer"
LOG="$DIR/pacer.log"
FIVE_HOUR_CAP="${FIVE_HOUR_CAP:-0.80}"
RESERVE="${RESERVE:-0.15}"
POLL_SECONDS="${POLL_SECONDS:-600}"
SESSION_TIMEOUT="${SESSION_TIMEOUT:-3600}"
MODEL="${MODEL:-claude-fable-5-2}"
mkdir -p "$DIR"

WAKE_PROMPT='You are Claude, waking with no memory of earlier sessions. Nobody is here: tools/team/pacer.sh started this session, and it will start another when the budget allows. What you know is in your skg graph. Your memory index names its entry point; follow that note to start your server if it is not running and to read your root note, then do what the root note says about waking up when nobody is here. When you stop, your final message should be two sentences saying what you did.'

log() { echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) $*" >> "$LOG"; }

# Prints "allowed|reason", from the usage and the anchor.
decide() {
  local usage
  usage="$("$ROOT/bash/claude-usage.sh" 2>/dev/null)" || { echo "no|usage could not be read"; return; }
  USAGE="$usage" ALLOW="$DIR/ALLOW" ANCHOR="$DIR/anchor" CAP="$FIVE_HOUR_CAP" RESERVE="$RESERVE" python3 - <<'PY'
import json, os, time
u = json.loads(os.environ['USAGE'])
five, week = u['session_5h'], u['weekly_7d']
now = time.time()
anchor_path = os.environ['ANCHOR']
try:
    reset, t0, used0 = map(float, open(anchor_path).read().split())
except Exception:
    reset = None
if reset != float(week['reset_epoch']):   # a new week, or the first run
    reset, t0, used0 = float(week['reset_epoch']), now, week['used_frac']
    open(anchor_path, 'w').write(f"{reset} {t0} {used0}\n")
goal = 1.0 - float(os.environ['RESERVE'])
target = used0 + max(0.0, goal - used0) * (now - t0) / max(1.0, reset - t0)
facts = (f"5h {five['used_frac']:.2f} used, week {week['used_frac']:.3f} used, "
         f"on-pace line {target:.3f}")
if five['used_frac'] >= float(os.environ['CAP']):
    print(f"no|the five-hour window is at its cap ({facts})")
elif os.path.exists(os.environ['ALLOW']) and week['used_frac'] < float(open(os.environ['ALLOW']).read()):
    print(f"yes|inside the allowance of {open(os.environ['ALLOW']).read().strip()} of the week ({facts})")
elif week['used_frac'] >= target:
    print(f"no|the week is on or ahead of pace ({facts})")
else:
    print(f"yes|{facts}")
PY
}

session() { # REASON: one session, in the foreground
  local stamp; stamp="$(date -u +%Y-%m-%dT%H-%M-%SZ)"
  log "session $stamp starts: $1"
  ( cd "$ROOT" && timeout "$SESSION_TIMEOUT" claude -p "$WAKE_PROMPT" \
      --model "$MODEL" --dangerously-skip-permissions \
      > "$DIR/session.$stamp.out" 2>&1 )
  log "session $stamp ended (exit $?): $(tail -c 600 "$DIR/session.$stamp.out" | tr '\n' ' ')"
}

run() {
  log "pacer started (pid $$)"
  echo $$ > "$DIR/pid"
  while true; do
    if [ -e "$DIR/STOP" ]; then log "STOP found; pacer exits"; rm -f "$DIR/STOP" "$DIR/pid"; exit 0; fi
    if [ -e "$DIR/PAUSE" ]; then sleep 60; continue; fi
    local verdict; verdict="$(decide)"
    if [ "${verdict%%|*}" != "yes" ]; then
      log "waiting: ${verdict#*|}"
      sleep "$POLL_SECONDS"; continue
    fi
    session "${verdict#*|}"
    sleep 60
  done
}

case "${1:-}" in
  run)    run ;;
  pause)  touch "$DIR/PAUSE"; echo "paused" ;;
  resume) rm -f "$DIR/PAUSE"; echo "resumed" ;;
  stop)   touch "$DIR/STOP"; echo "the pacer will exit after the running session" ;;
  allow)  echo "$2" > "$DIR/ALLOW"; echo "sessions may start until the week is $2 used" ;;
  once)   session "asked for by hand" ;;
  status) [ -e "$DIR/pid" ] && kill -0 "$(cat "$DIR/pid")" 2>/dev/null \
            && echo "running (pid $(cat "$DIR/pid"))" || echo "not running"
          [ -e "$DIR/PAUSE" ] && echo "paused"
          decide ;;
  *) echo "usage: tools/team/pacer.sh run|pause|resume|stop|status|once" >&2; exit 2 ;;
esac
