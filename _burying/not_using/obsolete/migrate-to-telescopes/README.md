# Why I buried this

`skg-migrate-to-telescopes` was a one-shot migration: it raised every
owned node's edge privacy to at least its default level, lifting
leak-shaped memberships (a public file naming a more private node's
ID) into their proper telescope sections, rewrote the changed
telescopes byte-stably, and rebuilt the databases. It refused unless
the active source-set was `all`, since it had to see every privacy
level.

It is done. Jeff is the only person whose data ever predated privacy
telescopes, and he has run it. Everyone else starts with telescopes
in place, so there is nothing for it to migrate.

What replaced its user-facing role: a leak-shaped membership is
reported in `telescope-warnings.org` at init and rebuild, and repaired
one edge at a time with `skg-set-relationship-source` (`C-c s r`).
What no repair ever fixed, then or now: a public repo's git HISTORY
keeps any IDs it leaked before the fix.

## What was removed alongside these two files

- `RequestType::MigrateToTelescopes` and the wire string
  `"migrate to telescopes"` (`server/serve/protocol.rs`)
- the dispatch arm (`server/serve.rs`)
- the `pub mod` entry (`server/serve/handlers/mod.rs`)
- the `(require 'skg-request-migrate-to-telescopes)`
  (`elisp/skg-client.el`)
- the endpoint (`api-and-formats.md`) and the command entry
  (`docs/COMMANDS.org`)

## To resurrect it

The handler is self-contained: it reads the in-Rust graph, computes
each edge's default level, and calls `write_nodecomplete_telescope`
plus `rebuild_dbs_in_place`. Restoring it means restoring the two
files and the six wiring points above.

One thing has changed under it since burial: `write_nodecomplete_telescope`
now refuses a node whose home is foreign or whose home section is
titleless (see `error_unless_home_is_writable`). A resurrected
migration would have to skip such nodes rather than propagate the
error, since it walks every owned node at once.

Decided in `TODO/dup-ids-maybe-bad/1_discussion.org` ("let's bury it")
and carried out per `3_plan.org`, work item 10.
