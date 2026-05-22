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
