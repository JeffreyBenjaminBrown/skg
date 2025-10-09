#!/bin/bash

# Run all automated Emacs tests for the Skg project

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Running Skg Emacs Tests...${NC}"

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Test directory (relative to project root, which is parent of SCRIPT_DIR)
TEST_DIR="$SCRIPT_DIR/../tests/elisp"

# Check if test directory exists
if [ ! -d "$TEST_DIR" ]; then
    echo -e "${RED}Error: Test directory $TEST_DIR not found${NC}"
    exit 1
fi

# Find all test files
TEST_FILES=$(find "$TEST_DIR" -name "test-*.el" -type f)

if [ -z "$TEST_FILES" ]; then
    echo -e "${RED}Error: No test files found in $TEST_DIR${NC}"
    exit 1
fi

echo -e "${YELLOW}Found test files:${NC}"
for file in $TEST_FILES; do
    echo "  - $(basename "$file")"
done
echo

# Run tests with emacs in batch mode
TEMP_TEST_FILE=$(mktemp)

# Create a temporary elisp file that loads and runs all tests
cat > "$TEMP_TEST_FILE" << 'EOF'
;; Temporary test runner

;; Add current directory to load path for relative requires
(add-to-list 'load-path default-directory)
;; Add elisp directory to load path for project files
(add-to-list 'load-path (expand-file-name "../../elisp" default-directory))

;; Load test files and collect all tests
(let ((test-files 
       (directory-files default-directory t "^test-.*\\.el$"))
      (total-tests 0)
      (failed-tests 0))
  
  ;; Load all test files
  (dolist (file test-files)
    (load file))
  
  ;; Run all tests and capture results
  (let ((ert-batch-backtrace-right-margin 120))
    (ert-run-tests-batch-and-exit)))
EOF

echo -e "${YELLOW}Running tests...${NC}"

# Try different emacs commands
EMACS_CMD=""
for cmd in emacs emacs-nox; do
    if command -v "$cmd" >/dev/null 2>&1; then
        EMACS_CMD="$cmd"
        break
    fi
done

if [ -z "$EMACS_CMD" ]; then
    echo -e "${RED}Error: No emacs command found. Please install emacs.${NC}"
    echo -e "${YELLOW}On Ubuntu/Debian: sudo apt-get install emacs-nox${NC}"
    echo -e "${YELLOW}On macOS: brew install emacs${NC}"
    exit 1
fi

# Change to test directory and run emacs with the temporary test file
if cd "$TEST_DIR" && "$EMACS_CMD" --batch -l "$TEMP_TEST_FILE" 2>&1; then
    echo -e "${GREEN}All tests passed!${NC}"
    EXIT_CODE=0
else
    echo -e "${RED}Some tests failed!${NC}"
    EXIT_CODE=1
fi

# Clean up
rm -f "$TEMP_TEST_FILE"

exit $EXIT_CODE