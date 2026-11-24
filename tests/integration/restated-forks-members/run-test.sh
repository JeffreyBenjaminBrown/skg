#!/bin/bash

# Integration test: a member of a forks folder that restates its note
# is drawn as such, '<b unchanged>' over the title of one that restates
# its title and body, '<b's body unchanged>' over the body of one that
# restates its body alone ('docs/forks.org', "A member that restates
# its note"), while the buffer, and so a save, keeps the real text;
# and so is one in a forks folder asked for (C-c l f) under a note
# that is not the view's root. Run once per client (test-emacs.el,
# test-nvim.lua). The data is written here, fresh for each run, since
# the test's save changes it.

set -e

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$TEST_DIR/../../.." && pwd)"

source "$TEST_DIR/../test-lib.sh"

echo "=== SKG Restated Forks Members Integration Test ==="

write_data() {
  rm -rf "$TEST_DIR/data"
  mkdir -p "$TEST_DIR/data/main" "$TEST_DIR/data/.index.tantivy"
  cat > "$TEST_DIR/data/main/F.skg" << 'EOF'
pid: F
title: Same words
body: |-
  A long body
  that its forks restate.
forks:
- G
EOF
  cat > "$TEST_DIR/data/main/P.skg" << 'EOF'
pid: P
title: Parent
contains:
- F
- X
EOF
  cat > "$TEST_DIR/data/main/X.skg" << 'EOF'
pid: X
title: Last
body: |-
  Its own body.
EOF
  cat > "$TEST_DIR/data/main/G.skg" << 'EOF'
pid: G
title: Same words
body: |-
  A long body
  that its forks restate.
EOF
}

remove_data() {
  rm -rf "$TEST_DIR/data"
}

trap 'cleanup; remove_data' EXIT
write_data

make_temp_config_in_data_folder << EOF
[[repos]]
name = "main"
path = "$TEST_DIR/data/main"
EOF

run_server_and_client_test_then_exit
