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
# MAIL FROM JEFF. Jeff writes to Top by adding children to his note
# "to (Top) Claude", so MAIL_FILE changes. The pacer keeps the sha256
# of that file as of the last session it started (data/claude/pacer/
# mail_seen); if the file now differs, a session starts even when the
# week is ahead of pace -- but never past the five-hour cap or into
# the reserve, and never while paused. No model call is involved, and
# MAIL_FILE is only read. The first run just records the baseline.
#
# ONE AT A TIME. Sessions run in the foreground of the loop, so two
# never overlap. An interactive Claude should 'pause' first: two
# sessions writing one graph would refuse each other's saves.
#
# THE CHECK OF JEFF'S FILES. Jeff asked, 2026-11-14, that nothing of
# ours change a file of his unnoticed. Before and after each session
# it starts, the pacer lists his files (path, size, time) into
# data/claude/pacer/jeff-files/, and then runs
# tools/team/check_jeffs_files.py over the session, which appends its
# counts and every line to read to data/claude/pacer/jeff-check.log.
# Once a UTC day it does the same over the time since the last daily
# check, for the sessions it did not start. Listings older than a week
# are deleted; the log keeps what they showed. If the check fails, the
# pacer logs that and goes on. It only lists and reads his files.
#
# THE SPENDING ACCOUNT. After each session it starts, the pacer runs
# tools/team/spending_account.py, so the account in data/claude/spending/
# is never more than one session behind. Claude Code deletes a
# transcript after thirty days, and a row not written by then is lost.
# If the script fails, the pacer logs that and goes on.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DIR="${PACER_DIR:-$ROOT/data/claude/pacer}"
MAIL_FILE="${MAIL_FILE:-$ROOT/data/jeff/public/c6b5b3bc-5fd8-4cbe-98ab-61f703d5250d.skg}"
MAIL_SEEN="$DIR/mail_seen"
LOG="$DIR/pacer.log"
FIVE_HOUR_CAP="${FIVE_HOUR_CAP:-0.80}"
RESERVE="${RESERVE:-0.15}"
POLL_SECONDS="${POLL_SECONDS:-600}"
SESSION_TIMEOUT="${SESSION_TIMEOUT:-6000}" # a session that runs and tends one specialist has taken an hour
ACCOUNT_TIMEOUT=600 # the script read 177 MB of transcripts in half a second, 2026-11-13
MODEL="${MODEL:-claude-fable-5-2}"
CHECK="$ROOT/tools/team/check_jeffs_files.py"
JEFF_LOG="$DIR/jeff-check.log"
LISTINGS="$DIR/jeff-files"
LISTINGS_KEPT_DAYS=7 # a listing is 1.4 MB gzipped (62k files, 2026-11-14)
CHECK_TIMEOUT=600 # the check took a second over a day, 2026-11-14
mkdir -p "$DIR"

WAKE_PROMPT='You are Claude, waking with no memory of earlier sessions. Nobody is here: tools/team/pacer.sh started this session, and it will start another when the budget allows. What you know is in your skg graph. Your memory index names its entry point; follow that note to start your server if it is not running and to read your root note, then do what the root note says about waking up when nobody is here. When you stop, your final message should be two sentences saying what you did.'

log() { echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) $*" >> "$LOG"; }

# Prints the sha256 of Jeff's mail file, or nothing if it cannot be read.
mail_fingerprint() { sha256sum "$MAIL_FILE" 2>/dev/null | cut -d' ' -f1; }

# Succeeds if the mail file differs from what the last started session saw.
mail_is_unread() {
  local now; now="$(mail_fingerprint)"
  [ -n "$now" ] && [ -e "$MAIL_SEEN" ] && [ "$now" != "$(cat "$MAIL_SEEN")" ]
}

remember_mail() { # what a session starting now will read
  local now; now="$(mail_fingerprint)"
  [ -n "$now" ] && echo "$now" > "$MAIL_SEEN"
}

# Prints "allowed|reason", from the usage and the anchor.
decide() {
  local usage
  usage="$("$ROOT/bash/claude-usage.sh" 2>/dev/null)" || { echo "no|usage could not be read"; return; }
  local mail=0; mail_is_unread && mail=1
  MAIL="$mail" USAGE="$usage" ALLOW="$DIR/ALLOW" ANCHOR="$DIR/anchor" CAP="$FIVE_HOUR_CAP" RESERVE="$RESERVE" python3 - <<'PY'
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
    if os.environ['MAIL'] == '1' and week['used_frac'] < goal:
        print(f"yes|mail from Jeff, though the week is ahead of pace ({facts})")
    else:
        print(f"no|the week is on or ahead of pace ({facts})")
else:
    print(f"yes|{facts}")
PY
}

utc_now() { date -u -d "${1:-now}" +%Y-%m-%dT%H:%M:%SZ; } # [WHEN, as 'date -d' reads it]: to the second

session() { # REASON: one session, in the foreground
  local stamp began; stamp="$(date -u +%Y-%m-%dT%H-%M-%SZ)"
  log "session $stamp starts: $1"
  remember_mail
  began="$(utc_now)"
  list_jeffs_files "$stamp.start.gz"
  ( cd "$ROOT" && timeout "$SESSION_TIMEOUT" claude -p "$WAKE_PROMPT" \
      --model "$MODEL" --dangerously-skip-permissions \
      > "$DIR/session.$stamp.out" 2>&1 )
  log "session $stamp ended (exit $?): $(tail -c 600 "$DIR/session.$stamp.out" | tr '\n' ' ')"
  list_jeffs_files "$stamp.end.gz"
  # The window ends a second late: times are to the second, and a write at 17:14:26.5 is in it.
  check_jeffs_files "session $stamp" "$began" "$(utc_now '+1 second')" "$stamp.start.gz" "$stamp.end.gz"
  update_spending_account
}

list_jeffs_files() { # NAME: writes the listing LISTINGS/NAME; logs only a failure
  mkdir -p "$LISTINGS"
  local output
  output="$(cd "$ROOT" && timeout "$CHECK_TIMEOUT" python3 -I "$CHECK" --list "$LISTINGS/$1" 2>&1)" \
    || { log "Jeff's files were not listed into $1: $(printf '%s' "$output" | tail -c 600 | tr '\n' ' ')"; return 1; }
}

check_jeffs_files() { # LABEL SINCE UNTIL BEFORE AFTER: appends to JEFF_LOG; logs a failure in both logs
  local args=("$2" "$3" --log "$JEFF_LOG" --label "$1") output status
  [ -e "$LISTINGS/$4" ] && [ -e "$LISTINGS/$5" ] && args+=(--before "$LISTINGS/$4" --after "$LISTINGS/$5")
  output="$(cd "$ROOT" && timeout "$CHECK_TIMEOUT" python3 -I "$CHECK" "${args[@]}" 2>&1 >/dev/null)"
  status=$?
  if [ "$status" != 0 ]; then
    log "the check of Jeff's files over $1 failed (exit $status): $(printf '%s' "$output" | tail -c 600 | tr '\n' ' ')"
    echo "$(utc_now) $1, $2 to $3: THE CHECK FAILED (exit $status); pacer.log says why" >> "$JEFF_LOG"
  fi
}

daily_check() { # once a UTC day, over the time since the last: the sessions the pacer did not start
  local mark="$LISTINGS/last-daily" now listing last last_listing
  now="$(utc_now)"; listing="day.$(date -u +%Y-%m-%dT%H-%M-%SZ).gz"
  if [ -e "$mark" ]; then
    read -r last last_listing < "$mark"
    [ "${last%%T*}" = "${now%%T*}" ] && return 0
  fi
  list_jeffs_files "$listing" || return 0
  # The first run only records where the first day starts.
  [ -e "$mark" ] && check_jeffs_files "day" "$last" "$now" "$last_listing" "$listing"
  echo "$now $listing" > "$mark"
  find "$LISTINGS" -name '*.gz' -mtime +"$LISTINGS_KEPT_DAYS" -delete
}

update_spending_account() { # logs only a failure
  local output status
  output="$(cd "$ROOT" && timeout "$ACCOUNT_TIMEOUT" python3 tools/team/spending_account.py 2>&1)"
  status=$?
  [ "$status" = 0 ] \
    || log "the spending account was not brought up to date (exit $status): $(printf '%s' "$output" | tail -c 600 | tr '\n' ' ')"
}

run() {
  log "pacer started (pid $$)"
  echo $$ > "$DIR/pid"
  local mail_unreadable_logged=0
  while true; do
    [ -e "$MAIL_SEEN" ] || remember_mail # the first run: no session for mail already there
    if [ -n "$(mail_fingerprint)" ]; then mail_unreadable_logged=0
    elif [ "$mail_unreadable_logged" = 0 ]; then
      log "cannot read Jeff's mail file $MAIL_FILE; pacing as usual"; mail_unreadable_logged=1
    fi
    daily_check
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
          if [ -z "$(mail_fingerprint)" ]; then echo "mail file unreadable"
          elif [ ! -e "$MAIL_SEEN" ]; then echo "no mail baseline yet (the running pacer stores one)"
          elif mail_is_unread; then echo "unread mail from Jeff pending"
          else echo "no unread mail from Jeff"; fi
          decide ;;
  *) echo "usage: tools/team/pacer.sh run|pause|resume|stop|status|once" >&2; exit 2 ;;
esac
