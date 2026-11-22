#!/bin/bash

# Shared helpers for keeping test runners from overwhelming an interactive
# machine. Environment overrides:
#   SKG_TEST_NICE=15        nice level for test commands
#   SKG_TEST_IONICE=0       disable ionice wrapping
#   SKG_*_JOBS / *_PARALLEL per runner, documented in each script

skg_cpu_count() {
  if command -v nproc >/dev/null 2>&1; then
    nproc
  elif command -v getconf >/dev/null 2>&1; then
    getconf _NPROCESSORS_ONLN
  else
    echo 2
  fi
}

skg_default_jobs() {
  local max_jobs="${1:-4}"
  local cpus
  cpus="$(skg_cpu_count)"

  case "$cpus" in
    ''|*[!0-9]*) cpus=2 ;;
  esac

  local jobs=$(((cpus + 1) / 2))
  if [ "$jobs" -lt 1 ]; then
    jobs=1
  fi
  if [ "$jobs" -gt "$max_jobs" ]; then
    jobs="$max_jobs"
  fi

  echo "$jobs"
}

skg_positive_int_or_default() {
  local value="$1"
  local default="$2"

  case "$value" in
    ''|*[!0-9]*|0) echo "$default" ;;
    *) echo "$value" ;;
  esac
}

skg_low_priority() {
  local nice_level="${SKG_TEST_NICE:-15}"

  if command -v ionice >/dev/null 2>&1 && [ "${SKG_TEST_IONICE:-1}" != "0" ]; then
    nice -n "$nice_level" ionice -c 3 "$@"
  else
    nice -n "$nice_level" "$@"
  fi
}

# A port for this checkout's scratch test servers: DEFAULT_PORT in the
# main checkout, and in a linked worktree ('git worktree add') an even
# port in 18000-19998 derived from the worktree's path, so suites run
# at once from separate worktrees do not take the same port. Even, so
# a suite may also take the port after it. The range lies between the
# long-lived servers (1729 to 1799) and the integration tests' random
# ports ('find_available_port', 20000 and up).
skg_port_for_this_checkout() {
  local default_port="$1"
  local checkout_root="$2"
  local git_dir
  local common_git_dir
  git_dir="$(git -C "$checkout_root" rev-parse --absolute-git-dir 2>/dev/null)"
  common_git_dir="$(cd "$checkout_root" 2>/dev/null \
    && cd "$(git rev-parse --git-common-dir 2>/dev/null)" 2>/dev/null \
    && pwd -P)"
  if [ -z "$git_dir" ] || [ -z "$common_git_dir" ] \
     || [ "$(cd "$git_dir" && pwd -P)" = "$common_git_dir" ]; then
    echo "$default_port"
    return
  fi
  local path_hash
  path_hash="$(printf '%s' "$checkout_root" | cksum | cut -d' ' -f1)"
  echo $(( 18000 + 2 * (path_hash % 1000) ))
}
