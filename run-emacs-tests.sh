#!/bin/bash
# Simple wrapper to run emacs tests from project root
exec "$(dirname "$0")/emacs-tests.sh" "$@"