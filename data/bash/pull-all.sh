#!/usr/bin/env bash
set -eu

echo "pull-all.sh no longer runs Git outside Skg maintenance." >&2
echo "Use M-x skg-pull-all in Emacs or :SkgPullAll in Neovim." >&2
echo "Those commands archive dirty editor work before starting Git." >&2
exit 2
