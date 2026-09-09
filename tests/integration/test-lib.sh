#!/bin/bash

# Common library for skg integration tests
# This file should be sourced by individual test runners

# Function to send shutdown command to skg server via Emacs
send_shutdown_command() {
  local port=$1
  echo "Sending shutdown command to server on port $port..."

  # Create temporary Emacs Lisp script to send shutdown
  local shutdown_script=$(mktemp)
  cat > "$shutdown_script" << 'EOF'
;; Send shutdown command to skg server
(require 'cl-lib)

(let* ((port (string-to-number (getenv "SKG_TEST_PORT")))
       (tcp-proc (make-network-process
                  :name "skg-shutdown"
                  :host "127.0.0.1"
                  :service port
                  :family 'ipv4)))
  (process-send-string tcp-proc
                       "((request . \"shutdown\") (request-id . \"integration-shutdown\"))\n")
  (sleep-for 0.083)
  (delete-process tcp-proc))
EOF

  # Run the script with a timeout to prevent hanging
  timeout 2 env SKG_TEST_PORT=$port emacs --batch -l "$shutdown_script" 2>/dev/null || true
  rm -f "$shutdown_script"
  sleep 0.1
}

# Function to clean up the background Skg server
cleanup() {
  echo ""
  echo "Cleaning up..."
  rm -f "$TEMP_CONFIG"

  if [ -n "$CARGO_PID" ]; then
    # Check if process is still running
    if kill -0 $CARGO_PID 2>/dev/null; then
      echo "Stopping server process (PID: $CARGO_PID)"

      # Use SIGINT (not SIGTERM) for graceful shutdown
      # The Rust server only handles SIGINT (Ctrl+C), not SIGTERM
      kill -INT $CARGO_PID 2>/dev/null || true

      local wait_count=0
      while [ $wait_count -lt 20 ] && kill -0 $CARGO_PID 2>/dev/null; do
        sleep 0.1
        wait_count=$((wait_count + 1))
      done

      # If still running after 2 seconds, force kill
      if kill -0 $CARGO_PID 2>/dev/null; then
        echo "Force killing server process (PID: $CARGO_PID)"
        kill -9 $CARGO_PID 2>/dev/null || true
      fi

      wait $CARGO_PID 2>/dev/null || true
    fi
  fi
}

# Function to find available port by trying random ports in range
find_available_port() {
  local attempts=0
  local max_attempts=100
  local min_port=1024
  local max_port=9000

  while [ $attempts -lt $max_attempts ]; do
    # Generate random port in the range 1000-9000
    local port=$(( min_port + RANDOM % (max_port - min_port + 1) ))

    # Check if port is available using ss (socket statistics)
    if ! ss -tln 2>/dev/null | grep -q ":$port "; then
      echo $port
      return 0
    fi

    attempts=$(( attempts + 1 ))
  done

  echo "ERROR: No available ports found after $max_attempts attempts" >&2
  exit 1
}

# Function to start skg server with test config
start_skg_server() {
  echo ""
  echo "Starting skg server (direct binary) with test config..."
  cd "$PROJECT_ROOT"
  # Use the pre-built binary instead of cargo run to avoid lock contention
  target/debug/skg "$TEMP_CONFIG" > "$TEST_DIR/server.log" 2>&1 &
  CARGO_PID=$!
  echo "✓ Started skg server (PID: $CARGO_PID) with config: $TEMP_CONFIG"
  echo "  Server logs: $TEST_DIR/server.log"
  echo "Waiting for server to be ready..."

  # Wait for "Server ready." in the log, which is printed after
  # Graph and index initialization completes. Checking the port alone
  # is not enough — the port is bound before init, for the busy signal.
  local max_attempts=300
  local attempt=0
  while [ $attempt -lt $max_attempts ]; do
    if grep -q "Server ready\." "$TEST_DIR/server.log" 2>/dev/null; then
      echo "✓ Server is ready on port $AVAILABLE_PORT"
      return 0
    fi
    if ! kill -0 $CARGO_PID 2>/dev/null; then
      echo "ERROR: Server process died during startup"
      echo "Server log:"
      cat "$TEST_DIR/server.log"
      return 1
    fi
    sleep 0.2
    attempt=$(( attempt + 1 ))
  done

  echo "ERROR: Server did not become ready after 60 seconds"
  echo "Last 30 lines of server log:"
  tail -30 "$TEST_DIR/server.log"
  return 1
}

# Function to clean up Tantivy index contents
cleanup_tantivy_index() {
  local tantivy_dir="$1"
  if [ -d "$tantivy_dir" ]; then
    rm -f "$tantivy_dir"/* 2>/dev/null || true
    echo "✓ Cleaned up Tantivy index contents"
  fi
}

# Function to run Emacs integration test
run_emacs_test() {
  local test_file="$1"
  echo ""
  echo "Running Emacs integration test..."
  cd "$TEST_DIR"
  if SKG_TEST_PORT="$AVAILABLE_PORT" emacs --batch \
      --eval '(progn (require (quote package)) (package-initialize))' \
      -l "$test_file"; then
    echo "✓ Integration test PASSED."
    TEST_RESULT=0
  else
    echo "✗ Integration test FAILED."
    TEST_RESULT=1
  fi
}

# Function to run an nvim-client integration test, the analog of
# run_emacs_test. The test file is a Lua script driven headless.
run_nvim_test() {
  local test_file="$1"
  echo ""
  echo "Running Neovim integration test..."
  cd "$TEST_DIR"
  if SKG_TEST_PORT="$AVAILABLE_PORT" SKG_TEST_CONFIG="$TEMP_CONFIG" \
      nvim --headless -l "$test_file"; then
    echo "✓ Integration test PASSED."
    TEST_RESULT=0
  else
    echo "✗ Integration test FAILED."
    TEST_RESULT=1
  fi
}

# Dispatch on SKG_TEST_CLIENT: each run-test.sh calls this instead of
# run_emacs_test directly, so the same test directory serves both
# clients (bash/all-tests.sh runs the directory once per client).
run_client_test() {
  if [ "${SKG_TEST_CLIENT:-emacs}" = "nvim" ]; then
    run_nvim_test "test-nvim.lua"
  else
    run_emacs_test "test-emacs.el"
  fi
}
