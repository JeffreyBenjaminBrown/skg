#!/usr/bin/env bash
# Stop-hook wrapper: beep only when the MAIN agent's turn ends,
# and stay silent when a subagent finishes its assignment.
#
# Claude Code delivers a JSON payload on stdin to Stop hooks. Subagent
# completions carry an "agent_id" (and "agent_transcript_path") field
# that a main-agent Stop payload does not. We use that field's presence
# to decide whether to beep, so this works whether the harness routes
# subagent stops through the Stop hook or a separate SubagentStop hook.

payload="$(cat)"

# Subagent completion -> stay silent.
if printf '%s' "$payload" | grep -q '"agent_id"'; then
  exit 0
fi

# Main agent stopped -> beep as before. Forward any args (e.g. sound name).
exec /home/sound/play-beep.sh "$@"
