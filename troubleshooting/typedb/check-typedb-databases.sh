#!/bin/bash

# Script to classify TypeDB databases as 'corrupt' or 'okay'
# Checks for common corruption indicators

set -e

TYPEDB_DATA_DIR="/opt/typedb/core/server/data"

if [ ! -d "$TYPEDB_DATA_DIR" ]; then
    echo "Error: TypeDB data directory not found at $TYPEDB_DATA_DIR"
    exit 1
fi

echo "Checking TypeDB databases for corruption indicators..."
echo "Data directory: $TYPEDB_DATA_DIR"
echo

declare -a okay_dbs=()
declare -a corrupt_dbs=()
declare -a test_dbs=()

# Function to check if a database appears corrupt
check_database() {
    local db_path="$1"
    local db_name=$(basename "$db_path")

    # Skip if not a directory
    [ ! -d "$db_path" ] && return

    # Classify as test database if matches pattern
    if [[ "$db_name" == *skg-*test* ]]; then
        test_dbs+=("$db_name")
        return
    fi

    # Check for corruption indicators
    local corrupt=false

    # Check if storage directory exists and is accessible
    if [ -d "$db_path/storage" ]; then
        # Try to list storage subdirectories - this will fail if there are file handle issues
        if ! ls "$db_path/storage" >/dev/null 2>&1; then
            corrupt=true
        fi

        # Check for common RocksDB corruption indicators
        if [ -d "$db_path/storage" ]; then
            # Look for .log files which might indicate incomplete operations
            local log_files=$(find "$db_path/storage" -name "*.log" 2>/dev/null | wc -l)
            if [ "$log_files" -gt 10 ]; then
                corrupt=true
            fi

            # Check if we can access key directories
            for subdir in "$db_path/storage"/*; do
                if [ -d "$subdir" ] && ! ls "$subdir" >/dev/null 2>&1; then
                    corrupt=true
                    break
                fi
            done
        fi
    else
        # No storage directory usually indicates corruption or incomplete setup
        corrupt=true
    fi

    if [ "$corrupt" = true ]; then
        corrupt_dbs+=("$db_name")
    else
        okay_dbs+=("$db_name")
    fi
}

echo "Scanning databases..."

# Check all databases
for db_path in "$TYPEDB_DATA_DIR"/*; do
    [ -d "$db_path" ] && check_database "$db_path"
done

# Sort arrays
IFS=$'\n' okay_dbs=($(sort <<<"${okay_dbs[*]}"))
IFS=$'\n' corrupt_dbs=($(sort <<<"${corrupt_dbs[*]}"))
IFS=$'\n' test_dbs=($(sort <<<"${test_dbs[*]}"))

echo
echo "=== CLASSIFICATION RESULTS ==="
echo

echo "✓ OKAY databases (${#okay_dbs[@]}):"
if [ ${#okay_dbs[@]} -eq 0 ]; then
    echo "  (none)"
else
    for db in "${okay_dbs[@]}"; do
        echo "  - $db"
    done
fi

echo
echo "✗ CORRUPT databases (${#corrupt_dbs[@]}):"
if [ ${#corrupt_dbs[@]} -eq 0 ]; then
    echo "  (none)"
else
    for db in "${corrupt_dbs[@]}"; do
        echo "  - $db"
    done
fi

echo
echo "🧪 TEST databases (${#test_dbs[@]}) - safe to delete:"
if [ ${#test_dbs[@]} -eq 0 ]; then
    echo "  (none)"
else
    for db in "${test_dbs[@]}"; do
        echo "  - $db"
    done
fi

echo
echo "=== SUMMARY ==="
echo "Total databases: $((${#okay_dbs[@]} + ${#corrupt_dbs[@]} + ${#test_dbs[@]}))"
echo "Okay: ${#okay_dbs[@]}, Corrupt: ${#corrupt_dbs[@]}, Test: ${#test_dbs[@]}"

if [ ${#corrupt_dbs[@]} -gt 0 ]; then
    echo
    echo "⚠️  WARNING: Found ${#corrupt_dbs[@]} potentially corrupt database(s)."
    echo "   Consider removing them if TypeDB fails to start."
fi

if [ ${#test_dbs[@]} -gt 0 ]; then
    echo
    echo "💡 TIP: You can safely remove all test databases with:"
    echo "   ./clean-typedb-test-databases.sh"
fi