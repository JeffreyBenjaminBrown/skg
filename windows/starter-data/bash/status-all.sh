#!/usr/bin/env bash
set -e

for i in $(cat list-of-repositories.txt); do
  cd "$i"
  echo ""
  echo "==============="
  echo "REPO: $i"
  git remote -v | head -n 1 | sed 's/.*github.com:/github:/' | sed 's/ (.*//'
  git status
  cd -
done
