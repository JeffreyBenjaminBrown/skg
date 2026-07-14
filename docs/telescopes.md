# Privacy telescopes: one node, many privacy levels

Skg lets a single node live at several privacy levels at once. The
mechanism is the **privacy telescope**: same-ID `.skg` files across
your sources, each file one **telescope section** holding the slice
of the node recorded at that source's level. There is no linking
relationship between the sections — sharing an ID *is* being the
same node. (Why "telescope"? Because it extends one section at a
time: widening your source-set to the next privacy level pulls out
exactly one more section per node, whereas an accordion — the
term's rejected predecessor — expands uniformly.)

## The privacy order

Your `skgconfig.toml` lists sources in privacy order, most public
first (see `api-and-formats.md`). That order is the ladder of
**privacy levels**. A **source-set** is a prefix of the ladder —
choosing one is answering "what is the most private source I want
visible right now?" — and the reserved set `all` is the whole
ladder. Screen-sharing with a colleague, you might switch to the
`public` prefix; alone, back to `all`.

## What lives where

The most public section carrying a title is the node's **home**. It
holds the title, body, and extra IDs. Every relationship edge (a
`contains` membership, a subscription, a hide, an override) carries
its own privacy level: the level of the section that records it.

Reading the node **folds** the sections, most public first, into one
effective node. Under a restricted source-set you see the **visible
fold** — only the active sections' contributions — while `all` shows
the **full fold**. A more private section can both *add* members and
*place* them: its members interleave into the more public list via
placement anchors, so your private ordering of a public list
survives without the public file recording anything. Saving unfolds
the node back into sections, byte-stably: sections you did not
affect do not change on disk, and foreign sections are never
written.

## Where new relationships land: the sticky-else-default rule

When you save, each relationship edge keeps the level it already had
on disk (**sticky**), unless a `(relSource ...)` atom explicitly
names another level — anything at or above the edge's default. A NEW
edge defaults to the more private of the two endpoints' homes — the
most public level that leaks neither endpoint. Every level is
clamped to be at least the owner's home (a section more public than
the home would imply a title-less public face). Hides floor higher:
a hide reveals that you hide something, so it must be at least as
private as both endpoints and the most public subscription that
explains it.

## Setting a relationship's source

`skg-set-relationship-source` (`C-c s r`, see `docs/COMMANDS.org`;
formerly `skg-privatize-relationship`) sets the level of the
relationship the headline at point represents. It asks the server
for the edge's default and current levels (the "edge level info"
endpoint, see `api-and-formats.md`) and offers the levels at least
as private as the default. An edge sitting above its default is
marked with a red `~NAME` herald and a `(relSource NAME)` metadata
atom; the server re-checks at save that any offered level is no
more public than the edge's default. The canonical use: your public
reading-list node contains a book you would rather not advertise —
privatize the *membership* and the book stays public, the list
stays public, but the edge between them lives in your private
section.

The same gesture lowers privacy: choose any level down to (but not
below) the default, and the next save moves the edge's membership
line into the more public section. Below the default there is
nothing to choose — an edge more public than the more-private of
its endpoints' homes would leak that endpoint's ID. To publicize a
relationship further, publicize the more private endpoint (move its
home) first; the edge's default falls with it, and this gesture can
then follow it down. Note that removing the atom (the gesture's
no-override choice) is not a way down: no atom means "no opinion",
so a saved edge keeps its sticky level.

One shape legitimately sits *below* its default: an edge whose more
private endpoint is FOREIGN, since your telescope has no section at
the foreign level to record it in. Such an edge renders with the
atom (it is off-default), saves back unchanged, and can be raised —
but never lowered further.

## What cannot be expressed

Two things are documented inexpressibles, by design:

- **Private textlinks.** A textlink lives in the body, and the body
  lives in the home section. A link in a public body is public;
  there is no per-link level. Keep the sentence in a private child
  instead.
- **Publicizing below the default.** As above: a relationship's
  privacy can be raised above the default, never lowered below it.

## What leaks, and what migration fixes

Before telescopes, a public file could name a private node's ID (a
"leak-shaped membership"). The server now warns about these
(`telescope-warnings.org` in your data root) rather than erroring.
`skg-migrate-to-telescopes` raises every owned edge's privacy to at
least its default level, moving such memberships into the right
sections and rebuilding the databases. What no migration can fix: if
a public repo ever *committed* a leaked ID, its git history still
holds it; rewriting history is manual.

## Ownership and folders

A source is yours iff its directory sits under the config's
`owned_folder` (default `owned/`) — see `api-and-formats.md`. Only
owned sections are ever written; a save that empties an owned
section deletes the file, and re-privatizing later recreates it.
Each owned source also receives a generated `DEPENDENCIES.toml`
naming the sources it may reference (everything at least as public),
so a repo you share carries its own dependency claim.
