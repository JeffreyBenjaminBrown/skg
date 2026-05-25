#!/usr/bin/env bash
set -e

for i in $(cat list-of-repositories.txt); do
  echo "STAY for a second -- this will prompt for password."
  echo ""
  echo "$i"
  cd "$i"
  git pull
  cd -
done
