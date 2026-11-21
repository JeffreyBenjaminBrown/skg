#!/usr/bin/env bash
# Push Claude's graphs to GitHub: go to each folder, push its main.
# Run from the root of the skg code repo, on the host.
#
# This does nothing else. It creates no repo, checks nothing for
# private mentions, and does not record in data/claude which commit
# each graph is at. tools/team/push-graphs-to-github_baroque.sh does
# all three; run that one after a specialist is born, or when
# data/claude itself should be pushed with fresh pins.

( cd data/claude/public  && git push origin main )
( cd data/claude/private && git push origin main )
( cd data/claude/team/cleo/notes      && git push origin main )
( cd data/claude/team/doc/notes       && git push origin main )
( cd data/claude/team/drew/notes      && git push origin main )
( cd data/claude/team/ed/notes        && git push origin main )
( cd data/claude/team/gen/notes       && git push origin main )
( cd data/claude/team/jesus/notes     && git push origin main )
( cd data/claude/team/libby/notes     && git push origin main )
( cd data/claude/team/mario/notes     && git push origin main )
( cd data/claude/team/porter/notes    && git push origin main )
( cd data/claude/team/praveen/notes   && git push origin main )
( cd data/claude/team/scarlett/notes  && git push origin main )
( cd data/claude/team/sue/notes       && git push origin main )
( cd data/claude/team/tay/notes       && git push origin main )
( cd data/claude/team/terra/notes     && git push origin main )
( cd data/claude/team/terry/notes     && git push origin main )
( cd data/claude/team/tess/notes      && git push origin main )
( cd data/claude/team/tracy/notes     && git push origin main )
( cd data/claude/team/val/notes       && git push origin main )
( cd data/claude/team/vic/notes       && git push origin main )
( cd data/claude         && git push origin main )
