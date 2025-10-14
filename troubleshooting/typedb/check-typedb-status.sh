#!/bin/bash
# Quick status check for TypeDB server

echo "=== TypeDB Status Check ==="
echo ""

# Check if process is running
if ps aux | grep -v grep | grep typedb > /dev/null; then
    echo "✓ TypeDB process is RUNNING"
    ps aux | grep -v grep | grep typedb | head -1
    echo ""
else
    echo "✗ TypeDB process is NOT running"
    echo ""
    echo "To start TypeDB:"
    echo "  nohup typedb server > /home/ubuntu/logs/typedb.log 2>&1 < /dev/null &"
    exit 1
fi

# Check log file
if [ -f /home/ubuntu/logs/typedb.log ]; then
    echo "=== Last 10 lines of log ==="
    tail -10 /home/ubuntu/logs/typedb.log
    echo ""

    if tail -20 /home/ubuntu/logs/typedb.log | grep -q "Ready!"; then
        echo "✓ Log shows 'Ready!' - TypeDB started successfully"
    else
        echo "⚠ Log doesn't show 'Ready!' - check for errors above"
    fi
else
    echo "✗ Log file not found at /home/ubuntu/logs/typedb.log"
fi

echo ""
echo "=== Summary ==="
echo "If you see 'Address already in use' error when trying to start TypeDB,"
echo "that means it's already running (which is good!)."
echo ""
echo "To restart TypeDB:"
echo "  pkill -f typedb"
echo "  nohup typedb server > /home/ubuntu/logs/typedb.log 2>&1 < /dev/null &"
