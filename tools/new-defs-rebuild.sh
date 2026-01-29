# Rebuild new-defs.org with links to all new definitions in the worktree.
# Compares against HEAD, including both tracked changes and untracked .rs files.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT_FILE="$REPO_ROOT/new-defs.org"
TEMP_DIFF=$(mktemp)

cd "$REPO_ROOT"

# Get tracked changes
git diff main --unified=0 -- '*.rs' > "$TEMP_DIFF"

# Append untracked .rs files as diffs
for f in $(git ls-files --others --exclude-standard '*.rs'); do
  echo "diff --git a/$f b/$f" >> "$TEMP_DIFF"
  echo "@@ -0,0 +1,$(wc -l < "$f") @@" >> "$TEMP_DIFF"
  sed 's/^/+/' "$f" >> "$TEMP_DIFF"
done

python3 - "$TEMP_DIFF" "$OUTPUT_FILE" << 'PYEOF'
import re
import sys
from collections import defaultdict

diff_file = sys.argv[1]
output_file = sys.argv[2]

with open(diff_file, 'r') as f:
    content = f.read()

file_pattern = re.compile(r'^diff --git a/(.+?) b/', re.MULTILINE)
hunk_pattern = re.compile(r'^@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@', re.MULTILINE)
def_pattern = re.compile(r'^\+([ \t]*)(pub\s+)?(fn|struct|enum|impl|type|const|static|trait|mod)\s+')

current_file = None
current_line = 0
results = defaultdict(list)

for line in content.split('\n'):
    file_match = file_pattern.match(line)
    if file_match:
        current_file = file_match.group(1)
        continue

    hunk_match = hunk_pattern.match(line)
    if hunk_match:
        current_line = int(hunk_match.group(1))
        continue

    if line.startswith('+') and not line.startswith('+++'):
        def_match = def_pattern.match(line)
        if def_match and current_file:
            def_text = line[1:].strip()
            if any(x in def_text for x in ['(', '{', '<']) or def_text.endswith('{') or re.match(r'^(pub\s+)?(struct|enum|type|trait|mod)\s+\w+', def_text):
                label = def_text.replace('[', '\\[').replace(']', '\\]')
                results[current_file].append((current_line, label))
        current_line += 1

with open(output_file, 'w') as out:
    out.write("#+TITLE: New Definitions in Worktree\n\n")
    for file in sorted(results.keys()):
        out.write(f"* {file}\n")
        for line, label in results[file]:
            plain_label = label.replace('\\[', '[').replace('\\]', ']')
            out.write(f"** [[file:{file}::{line}][{label}]]\n")
            out.write(f"{plain_label}\n")
        out.write("\n")

print(f"Generated {output_file} with {sum(len(v) for v in results.values())} definitions from {len(results)} files")
PYEOF

rm "$TEMP_DIFF"
