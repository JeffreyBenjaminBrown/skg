#!/usr/bin/env bash
set -e

for i in $(cat list-of-repositories.txt); do
  echo ""
  cd "$i"
  echo "$i"
  git push
  cd -
done
