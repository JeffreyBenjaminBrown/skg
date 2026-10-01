#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

OUT="windows/out/skg-windows-package"
rm -rf "$OUT"
mkdir -p "$OUT"

cp windows/install.ps1 "$OUT/"
cp windows/run.ps1 "$OUT/"
cp -R windows/starter-data "$OUT/"
mkdir -p "$OUT/elisp"
(
  cd elisp
  find . -type d -exec mkdir -p "../$OUT/elisp/{}" \;
  find . -type f ! -name '*~' -exec cp "{}" "../$OUT/elisp/{}" \;
)
mkdir -p "$OUT/docs"
cp -R docs/. "$OUT/docs/"
cp LICENSE.md "$OUT/LICENSE.md"
cp -R windows/docs/. "$OUT/docs/"

cat > "$OUT/WINDOWS.org" <<'EOF'
* Skg for Windows
Run these from PowerShell:

#+begin_src powershell
  .\install.ps1
  .\run.ps1
#+end_src

See =docs/windows-user.org= for more detail.
EOF

echo "Wrote $OUT"
