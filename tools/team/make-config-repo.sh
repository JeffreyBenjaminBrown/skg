#!/usr/bin/env bash
# make-config-repo.sh -- make a folder that holds skg configs and
# skgrepos into a Git repo of its own, with one submodule per skgrepo.
#
#   tools/team/make-config-repo.sh FOLDER         # says what it would do; changes nothing
#   tools/team/make-config-repo.sh FOLDER --yes   # does it
#
# FOLDER is a config's folder, like data/jeff/ or data/claude/. The
# new repo tracks
#   - every 'skgconfig.toml' in FOLDER or up to two folders below it,
#   - each skgrepo those configs own (a [[repos]] entry without
#     'foreign = true') that lies inside FOLDER, as a submodule,
#   - a '.gitignore' and the '.gitmodules'.
# A skgrepo becomes a submodule in place: its own '.git', its files
# and its history are not touched, and its uncommitted work stays
# uncommitted. The outer repo records only which commit of it is
# current (the "pin"). Nothing is run inside a skgrepo but two
# read-only questions: its HEAD, and its 'origin'.
#
# A submodule's URL is the skgrepo's own 'origin' if it has one.
# If it has none, the URL is '../NAME.git', NAME the skgrepo's name in
# the config: a path relative to wherever the outer repo is pushed.
# The script says which it used for each.
#
# Ignored: what the server rebuilds or keeps as running state (the
# search index, 'views/', '*.running/', the marks files), and every
# other file or folder no config names, each on a line of its own.
#
# To clone the result elsewhere:
#   git clone --recurse-submodules URL FOLDER
#   git -C FOLDER submodule foreach git checkout -q main
# (A fresh submodule is checked out at its pin, without a branch;
# the second line puts each back on 'main'.)
set -euo pipefail
shopt -s dotglob nullglob
export GIT_OPTIONAL_LOCKS=0 # so no 'git status' here refreshes a skgrepo's index

[ $# -ge 1 ] && [ -d "$1" ] && [ -f "$1/skgconfig.toml" ] \
  || { echo "usage: $0 FOLDER [--yes]   (FOLDER holds a skgconfig.toml)" >&2; exit 2; }
F="$(cd "$1" && pwd -P)"
YES="${2:-}"
[ ! -e "$F/.git" ] || { echo "$F is already a Git repo; nothing done." >&2; exit 1; }

# The configs, relative to F.
CONFIGS=()
for c in "$F"/skgconfig.toml "$F"/*/skgconfig.toml "$F"/*/*/skgconfig.toml; do
  case "$c" in *.running/*) continue ;; esac
  CONFIGS+=("${c#"$F"/}")
done

# Each config's [[repos]] entries, as 'NAME<tab>PATH<tab>FOREIGN', and its tantivy_folder.
repos_of() {
  awk '
    function flush() { if (name != "") print name "\t" path "\t" foreign; name = ""; path = ""; foreign = "false" }
    function value(line) { sub(/^[^=]*=[ \t]*/, "", line); sub(/[ \t]*#.*$/, "", line); gsub(/"/, "", line); return line }
    /^\[\[repos\]\]/ { flush(); next }
    /^\[/            { flush(); next }
    /^name[ \t]*=/    { name = value($0) }
    /^path[ \t]*=/    { path = value($0) }
    /^foreign[ \t]*=/ { foreign = value($0) }
    END { flush() }' "$1"
}
TANTIVY=".index.tantivy"
for c in "${CONFIGS[@]}"; do
  t="$(awk '/^tantivy_folder[ \t]*=/ { sub(/^[^=]*=[ \t]*/, ""); sub(/[ \t]*#.*$/, ""); gsub(/"/, ""); print }' "$F/$c")"
  case " $TANTIVY " in *" $(basename "${t:-.index.tantivy}") "*) ;; *) TANTIVY="$TANTIVY $(basename "$t")" ;; esac
done

# The submodules: each owned skgrepo inside F, once. SUBS holds 'PATH URL', PATH relative to F.
SUBS=(); NOTES=()
for c in "${CONFIGS[@]}"; do
  while IFS=$'\t' read -r name path foreign; do
    [ "$foreign" = true ] && continue
    abs="$(realpath -m "$F/$(dirname "$c")/$path")"
    rel="${abs#"$F"/}"
    case "$abs" in "$F"/*) ;; *) NOTES+=("'$name' ($c) is outside $F, so it is not a submodule here."); continue ;; esac
    printf '%s\n' "${SUBS[@]}" | grep -q "^$rel " && continue
    if ! git -C "$abs" rev-parse -q --verify HEAD >/dev/null 2>&1 \
       || [ "$(git -C "$abs" rev-parse --show-toplevel)" != "$abs" ]; then
      NOTES+=("'$name' ($rel) is not a gitrepo with a commit, so it is not a submodule; it is ignored.")
      continue
    fi
    if url="$(git -C "$abs" remote get-url origin 2>/dev/null)"; then
      NOTES+=("$rel: URL from its origin, $url")
    else
      url="../$name.git"
      NOTES+=("$rel: it has no origin, so its URL is $url, relative to the outer repo's own URL")
    fi
    SUBS+=("$rel $url")
  done < <(repos_of "$F/$c")
done
SUBPATHS=(); for s in "${SUBS[@]}"; do SUBPATHS+=("${s%% *}"); done

# What no config names, found by walking down from F to the configs and submodules.
named() { printf '%s\n' "${CONFIGS[@]}" "${SUBPATHS[@]}" | grep -qx "$1"; }
above_named() { printf '%s\n' "${CONFIGS[@]}" "${SUBPATHS[@]}" | grep -q "^$1/"; }
rebuilt() { case " $TANTIVY views " in *" $(basename "$1") "*) return 0 ;; esac
            case "$(basename "$1")" in *.running|*-marks-*.json) return 0 ;; esac; return 1; }
STRAYS=()
walk() { # DIR, relative to F, with a trailing '/' ('' for F itself)
  local entry rel
  for entry in "$F/$1"*; do
    rel="${entry#"$F"/}"
    if named "$rel" || rebuilt "$rel"; then continue
    elif [ -d "$entry" ] && above_named "$rel"; then walk "$rel/"
    elif [ -d "$entry" ]; then STRAYS+=("/$rel/")
    else STRAYS+=("/$rel")
    fi
  done
}
walk ""

echo "In $F:"
echo "  git init, on branch main"
echo "  track: ${CONFIGS[*]} .gitignore .gitmodules"
for s in "${SUBS[@]}"; do echo "  submodule: ${s%% *}  (pinned at $(git -C "$F/${s%% *}" rev-parse --short HEAD))"; done
for n in "${NOTES[@]}"; do echo "  - $n"; done
echo "  ignore, as rebuilt or running state: $TANTIVY views/ *.running/ *-marks-*.json"
for s in "${STRAYS[@]}"; do echo "  ignore, as no config names it: $s"; done
echo "  commit those, and nothing inside any skgrepo"
[ "$YES" = --yes ] || { echo "Nothing done. Run again with --yes to do it."; exit 0; }
git -C "$F" init -q -b main
git -C "$F" var GIT_COMMITTER_IDENT >/dev/null 2>&1 || {
  rm -rf "$F/.git" # just made, and empty
  echo "Git does not know who you are, so it could not commit; set user.name and user.email first. Nothing done." >&2; exit 1; }
{
  echo "# Rebuilt by the server, or its running state."
  for t in $TANTIVY; do echo "$t/"; done
  echo "views/"
  echo "*.running/"
  echo "*-marks-*.json"
  echo
  echo "# Named by no config here (tools/team/make-config-repo.sh listed them)."
  for s in "${STRAYS[@]}"; do echo "$s"; done
} > "$F/.gitignore"
: > "$F/.gitmodules"
for s in "${SUBS[@]}"; do
  path="${s%% *}"; url="${s#* }"
  git -C "$F" config -f .gitmodules "submodule.$path.path" "$path"
  git -C "$F" config -f .gitmodules "submodule.$path.url" "$url"
  # The pin, written straight into the index: nothing runs inside the skgrepo.
  git -C "$F" update-index --add --cacheinfo "160000,$(git -C "$F/$path" rev-parse HEAD),$path"
done
git -C "$F" add .gitignore .gitmodules "${CONFIGS[@]}"
git -C "$F" commit -q -m "The configs, and each skgrepo as a submodule (tools/team/make-config-repo.sh)"
# 'init' writes only the outer repo's .git/config. (Not 'git submodule sync':
# it would set each skgrepo's own 'origin'.) Its stderr only warns that
# a relative URL has nothing to be relative to until the outer repo has a remote.
git -C "$F" submodule init -q 2>/dev/null
echo "Done: $(git -C "$F" log --oneline -1)"
