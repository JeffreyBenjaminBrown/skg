#!/bin/bash

# Run all automated Neovim tests for the Skg project.
# The nvim analog of bash/emacs-tests.sh: unit specs in tests/nvim/,
# run headless via plenary.busted (baked into the docker image as a
# neogit dependency).

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Running Skg Neovim Tests...${NC}"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
source "$SCRIPT_DIR/test-runner-lib.sh"

# Reap orphaned test children before starting. If a previous harness
# run was killed (timeout, C-c), plenary's per-spec child nvims are
# reparented to init and can spin forever at full CPU. The match is
# deliberately narrow -- headless + plenary.busted + parent PID 1 --
# so it can never touch an interactive nvim.
ps -eo pid=,ppid=,args= | while read -r pid ppid args; do
    case "$args" in
        *nvim*--headless*plenary.busted*)
            if [ "$ppid" = "1" ]; then
                kill -9 "$pid" 2>/dev/null \
                  && echo "Reaped orphaned test nvim (pid $pid)"
            fi
            ;;
    esac
done

TEST_DIR="$SCRIPT_DIR/../tests/nvim"

if [ ! -d "$TEST_DIR" ]; then
    echo -e "${RED}Error: Test directory $TEST_DIR not found${NC}"
    exit 1
fi

TEST_FILES=$(find "$TEST_DIR" -name "*_spec.lua" -type f | sort)

if [ -z "$TEST_FILES" ]; then
    echo -e "${RED}Error: No *_spec.lua files found in $TEST_DIR${NC}"
    exit 1
fi

echo -e "${YELLOW}Found test files:${NC}"
for file in $TEST_FILES; do
    echo "  - $(basename "$file")"
done
echo

NVIM_CMD=""
for cmd in nvim; do
    if command -v "$cmd" >/dev/null 2>&1; then
        NVIM_CMD="$cmd"
        break
    fi
done

if [ -z "$NVIM_CMD" ]; then
    echo -e "${RED}Error: No nvim command found. Please install neovim.${NC}"
    exit 1
fi

echo -e "${YELLOW}Running tests...${NC}"

# PlenaryBustedDirectory spawns one child nvim per spec file and exits
# nonzero if any spec fails.
if skg_low_priority "$NVIM_CMD" --headless \
     -c "PlenaryBustedDirectory $TEST_DIR {minimal_init = '$TEST_DIR/minimal_init.lua'}" \
     2>&1; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
