#!/bin/bash

# DEPRECATED: Use the Rust-based cleanup utility instead:
#   cargo run --bin cleanup-test-dbs
#
# This shell script uses filesystem operations which can cause TypeDB to crash
# if databases are open. The Rust utility uses TypeDB's API which is safer.
#
# Script to safely delete TypeDB test databases
# Deletes any database whose name matches "*skg-*test*"
#
# Note: /opt/typedb/core/server/data is a symlink to /var/lib/typedb/core/data
# If this script doesn't work, use the Rust utility above or manual cleanup:
#   cd /var/lib/typedb/core/data && rm -rf skg-test*

set -e

TYPEDB_DATA_DIR="/opt/typedb/core/server/data"

if [ ! -d "$TYPEDB_DATA_DIR" ]; then
    echo "Error: TypeDB data directory not found at $TYPEDB_DATA_DIR"
    exit 1
fi

echo "Scanning for TypeDB test databases to delete..."
echo "Pattern: *skg-*test*"
echo

# Find databases matching the pattern (matches both skg-test and skg-*test*)
test_dbs=$(find "$TYPEDB_DATA_DIR" -maxdepth 1 -type d \( -name "*skg-*test*" -o -name "skg-test" \) | sort)

if [ -z "$test_dbs" ]; then
    echo "No test databases found matching pattern *skg-*test*"
    exit 0
fi

echo "Found test databases to delete:"
echo "$test_dbs" | while read -r db; do
    db_name=$(basename "$db")
    echo "  - $db_name"
done

echo
read -p "Delete these databases? (y/N): " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Deleting test databases..."
    echo "$test_dbs" | while read -r db; do
        db_name=$(basename "$db")
        echo "Deleting: $db_name"
        rm -rf "$db"
    done
    echo "Done. Test databases deleted."
else
    echo "Cancelled. No databases were deleted."
fi