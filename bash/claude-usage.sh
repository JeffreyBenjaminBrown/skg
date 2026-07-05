#!/usr/bin/env bash
# claude-usage.sh — read Claude subscription usage from API rate-limit headers.
# Prints JSON: session (5h) + weekly (7d) utilization, reset epoch, seconds left.
# An agent can run this and modulate model choice on the numbers.
#
# Cost: one max_tokens=1 Haiku call (~free). The usage data lives in the
# response headers, so a request must be made to read it.
set -euo pipefail

CRED="${CLAUDE_CONFIG_DIR:-$HOME/.claude}/.credentials.json"
[ -f "$CRED" ] || CRED="$HOME/.claude/.credentials.json"

TOKEN=$(python3 -c "import json;print(json.load(open('$CRED'))['claudeAiOauth']['accessToken'])")

hdr=$(curl -sS -D - -o /dev/null https://api.anthropic.com/v1/messages \
  -H "authorization: Bearer $TOKEN" \
  -H "anthropic-version: 2023-06-01" \
  -H "anthropic-beta: oauth-2025-04-20" \
  -H "content-type: application/json" \
  -d '{"model":"claude-haiku-4-5-20251001","max_tokens":1,"system":"You are Claude Code, Anthropics official CLI for Claude.","messages":[{"role":"user","content":"hi"}]}')

get() { printf '%s\n' "$hdr" | grep -i "^$1:" | tr -d '\r' | awk '{print $2}'; }

now=$(date +%s)
s_util=$(get anthropic-ratelimit-unified-5h-utilization)
s_reset=$(get anthropic-ratelimit-unified-5h-reset)
w_util=$(get anthropic-ratelimit-unified-7d-utilization)
w_reset=$(get anthropic-ratelimit-unified-7d-reset)
claim=$(get anthropic-ratelimit-unified-representative-claim)

if [ -z "${s_util:-}" ]; then
  echo '{"error":"no rate-limit headers (token expired? run any claude cmd to refresh)"}' >&2
  exit 1
fi

python3 - "$now" "$s_util" "$s_reset" "$w_util" "$w_reset" "$claim" <<'PY'
import sys, json
now, su, sr, wu, wr, claim = sys.argv[1:]
print(json.dumps({
  "session_5h":  {"used_frac": float(su), "used_pct": round(float(su)*100,1),
                  "reset_epoch": int(sr), "seconds_left": int(sr)-int(now)},
  "weekly_7d":   {"used_frac": float(wu), "used_pct": round(float(wu)*100,1),
                  "reset_epoch": int(wr), "seconds_left": int(wr)-int(now)},
  "binding_window": claim,
}, indent=2))
PY
