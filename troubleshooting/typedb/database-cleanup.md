# TypeDB Database Cleanup

## Problem: Test Database Accumulation

**Symptom**: Tests fail with "Too many open files" or "Connection Error: Unable to connect to TypeDB server"

**Cause**: Test databases accumulate over time, each consuming file handles. When TypeDB crashes or tests are interrupted, cleanup code doesn't run, leaving databases orphaned.

## Solution: Manual Cleanup

### Quick Fix

```bash
# Clean up all test databases
cargo run --bin cleanup-test-dbs
```

### Delete a Specific Database

```bash
# Delete a single database by name
cargo run --bin delete-one-db <database-name>

# Example:
cargo run --bin delete-one-db test-manual
```

## Available Cleanup Utilities

### 1. cleanup-test-dbs

**Location**: `rust/bin/cleanup-test-dbs.rs`

**Purpose**: Deletes ALL databases starting with "skg-test"

**Usage**:
```bash
cargo run --bin cleanup-test-dbs
```

**Safe to run**: Yes - only deletes test databases, never touches production databases

**When to use**:
- Before running tests if you suspect accumulated databases
- After test failures or interruptions
- When seeing "Too many open files" errors

### 2. delete-one-db

**Location**: `rust/bin/delete-one-db.rs`

**Purpose**: Deletes a single database by exact name

**Usage**:
```bash
cargo run --bin delete-one-db <database-name>
```

**When to use**:
- Cleaning up a specific non-test database
- Removing databases that don't match the "skg-test*" pattern

### 3. clean-typedb-test-databases.sh (DEPRECATED)

**Location**: `troubleshooting/typedb/clean-typedb-test-databases.sh`

**Purpose**: Shell script that deletes test databases using filesystem operations

**Status**: **DEPRECATED** - Use `cleanup-test-dbs` instead

**Why deprecated**:
- Uses filesystem operations which can crash TypeDB if databases are open
- The Rust utilities use TypeDB's API which is safer
- Less reliable with symlinked directories

**When to use**: Only as a fallback if Rust utilities unavailable

## Automatic Cleanup

### Integration Tests

Integration tests (`bash/integration-tests.sh`) automatically run `cleanup-test-dbs` before each run. This ensures zero database accumulation.

### Unit Tests (cargo nextest)

Unit tests use `run_with_test_db()` which calls `cleanup_test_tantivy_and_typedb_dbs()` after each test. However:

- **Failing tests** may panic before cleanup runs
- **Interrupted tests** (Ctrl+C) never reach cleanup
- This leads to slow accumulation (~1-2 databases per run)

**Best practice**: Run `cargo run --bin cleanup-test-dbs` periodically

## Configuration Issue Fixed

**Problem Found (2025-11-18)**: The main development config at `data/skgconfig.toml` was using:
```toml
db_name = "skg-test"
```

This matched the test database pattern, causing it to be deleted by cleanup utilities.

**Solution**: Changed to:
```toml
db_name = "skg-dev"
```

**Lesson**: Never name a development database with "skg-test" prefix - this is reserved for test databases.

## Database Naming Convention

- **Test databases**: `skg-test-*` (automatically cleaned up)
- **Development database**: `skg-dev` (never touched by cleanup)
- **Production database**: `skg` or similar (never touched by cleanup)

## Diagnostic Tools

### TypeDB Status Script
```bash
bash troubleshooting/typedb/check-typedb-status.sh
```
**Shows**: Process status, recent logs, helpful commands

### Database Analysis Script
```bash
bash troubleshooting/typedb/check-typedb-databases.sh
```
**Shows**: Count of test/corrupt/okay databases, detailed analysis, recommendations

Very helpful before deciding what to clean up.

## Monitoring Database Count

### Manual Checks

Check current databases:
```bash
ls -la /opt/typedb/core/server/data/ | grep skg
```

Count test databases:
```bash
ls -la /opt/typedb/core/server/data/ | grep skg-test | wc -l
```

## Failure Modes

### Mode 1: TypeDB crashes during tests
1. Tests create databases
2. TypeDB crashes due to file handle exhaustion
3. Tests panic/error out
4. Cleanup code runs but cannot connect to TypeDB
5. Databases remain in `/opt/typedb/core/server/data/`
6. Next test run loads all old databases → more file handles → faster crash
7. **Vicious cycle**

### Mode 2: Tests interrupted before cleanup
1. Tests interrupted (Ctrl+C, timeout, panic)
2. Cleanup code never runs
3. Databases accumulate

### Mode 3: TypeDB not running during cleanup
1. Test completes
2. Cleanup code tries to delete database
3. TypeDB is not running → cleanup fails silently
4. Database remains

## Prevention

1. **Ensure TypeDB is running** before tests:
   ```bash
   ps aux | grep typedb || (cd /opt/typedb/core && ./typedb server &)
   ```

2. **Run cleanup before tests**:
   ```bash
   cargo run --bin cleanup-test-dbs
   cargo nextest run
   ```

3. **Use integration tests** (they clean up automatically):
   ```bash
   bash bash/integration-tests.sh
   ```

4. **Check database count** periodically:
   ```bash
   ls -la /opt/typedb/core/server/data/ | grep skg-test | wc -l
   ```

## Testing the Fix

After implementing cleanup utilities (2025-11-18):

**cargo nextest** (4 iterations):
- Slow accumulation (~1-2 databases per run)
- TypeDB stayed stable throughout
- No connection errors

**Integration tests** (2 iterations):
- Zero accumulation (perfect cleanup)
- All databases cleaned automatically

**Conclusion**: Problem solved. Accumulation is now manageable.
