# USAGE:
# - Argument 1: request file.
# - Argument 2: max number of iterations.

set -e

if [ -z "$1" ] || [ -z "$2" ]; then
  echo "Usage: $0 <request-file> <max-iterations>"
  exit 1
fi

for ((i=1; i<=$2; i++)); do
  result=$(claude --dangerously-skip-permissions \
                  -p "$(cat $1)")

  echo "$result"
  echo "* iteration $i" >> iterations.org
  echo "$result" | sed 's/^/  /' >> iterations.org

  if [[ "$result" == *"<promise>COMPLETE</promise>"* ]]; then
    echo "PRD complete after $i iterations."
    aplay /tmp/gnarly.wav 2>/dev/null &
    exit 0
  fi
done
