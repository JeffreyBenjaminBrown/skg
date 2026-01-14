#!/bin/bash

input=$(cat)
# Extract command from JSON without jq - look for "command": "..."
command=$(echo "$input" | grep -oP '"command"\s*:\s*"\K[^"]*' | head -1)

# Only intervene for git commands
if [[ "$command" == git* ]]; then
  # Allowed git commands (read-only + mv)
  if [[ "$command" =~ ^(git\ log|git\ diff|git\ status|git\ mv) ]]; then
    exit 0  # Allow normally
  else
    # Block other git commands
    cat <<EOF
{
  "decision": "block",
  "reason": "Only git log, git diff, git status, and git mv are allowed"
}
EOF
    exit 0
  fi
fi

# Non-git commands: don't intervene
exit 0
