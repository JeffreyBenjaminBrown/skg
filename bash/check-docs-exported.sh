#!/bin/bash
# USAGE: bash/check-docs-exported.sh
#   Fails, naming each file, unless the '.org' files under 'docs/' and
#   'coding-advice/' are exactly what bash/export-docs.sh writes now:
#   none differs from a fresh export, none is missing, and none is
#   there that no note of 'skg-system/' exports
#   ('coding-advice/generated-docs.org'). Other files there (say
#   'docs/troubleshooting/check-rlibs.py') are not checked.
#   Run by bash/all-tests.sh. SKG_BIN as for bash/export-docs.sh.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FRESH="$(mktemp -d)"
trap 'rm -rf "$FRESH"' EXIT
"$ROOT/bash/export-docs.sh" "$FRESH" > "$FRESH.report" 2>&1 \
  || { cat "$FRESH.report"; rm -f "$FRESH.report"; exit 1; }
rm -f "$FRESH.report"
failed=0
while read -r file; do
  if [ ! -f "$ROOT/$file" ]; then
    echo "missing: $file (bash/export-docs.sh writes it)"; failed=1
  elif ! cmp -s "$FRESH/$file" "$ROOT/$file"; then
    echo "differs from a fresh export: $file"; failed=1
  fi
done < <(cd "$FRESH" && find . -type f -name '*.org' | sed 's|^\./||' | sort)
while read -r file; do
  if [ ! -f "$FRESH/$file" ]; then
    echo "not exported from skg-system/: $file"; failed=1
  fi
done < <(cd "$ROOT" && find docs coding-advice -type f -name '*.org' | sort)
if [ $failed -ne 0 ]; then
  echo "Edit the notes of skg-system/, not these files; then run"
  echo "bash/export-docs.sh. See coding-advice/generated-docs.org."
fi
exit $failed
