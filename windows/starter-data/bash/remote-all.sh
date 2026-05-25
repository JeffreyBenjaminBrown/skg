#!/usr/bin/env bash
set -e

for i in $(cat list-of-repositories.txt); do
  cd "$i"
  echo ""
  echo "==============="
  echo "REPO: $i"
  git remote -v
  cd -
done
