#!/usr/bin/env bash

set -euo pipefail

profile_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_root="$(cd "$profile_dir/../../.." && pwd)"
tool_root="$project_root/target/profile-tools"
archive="$tool_root/valgrind-3.27.1.tar.bz2"
source_dir="$tool_root/valgrind-3.27.1"
install_dir="$tool_root/valgrind-install"

mkdir -p "$tool_root"
curl -L --fail --output "$archive" \
  https://sourceware.org/pub/valgrind/valgrind-3.27.1.tar.bz2
printf '9b819b1aa88fc5936373fc624aa75723  %s\n' "$archive" | md5sum -c -
python3 -m tarfile -e "$archive" "$tool_root"
(
  cd "$source_dir"
  ./configure --prefix="$install_dir"
  make -j"${PROFILE_BUILD_JOBS:-4}"
  make install
)
printf 'Callgrind installed below %s\n' "$install_dir"
