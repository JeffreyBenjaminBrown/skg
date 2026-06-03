# API

Communication between Rust and Emacs is via TCP on port 1730, configurable in any skgconfig.toml. The connection is persistent.

Note: Port 1729 is used for Rust-TypeDB communication (the TypeDB server), not Rust-Emacs communication.

So far there are these endpoints:

## Verify connection
  - Request: ((request . "verify connection"))
  - Response: Plain text with newline termination: "This is the skg server verifying the connection."

## Text search
  - Request: ((request . "text search") (terms . "SEARCH_TERMS") (regex . "BOOL") (body . "BOOL") (operators . "BOOL"))
    - Search always returns every match. "Rooty" nodes (literal roots, cycle-roots, link targets, and things that had an ID when imported) are ranked higher, via their context-origin multiplier.
    - `regex`, `body`, `operators` are optional; each defaults to "false".
      - "regex=true": interpret the query as a per-token regex; a RegexQuery is built directly and the QueryParser is bypassed.
      - "body=true": also search node bodies (titles are always searched).
      - "operators=true": preserve Tantivy phrase and operator syntax (AND / OR / phrase / +foo / -bar / grouping / field:). Intra-word operator chars still get escaped heuristically so C++ etc. remain findable. In regex mode, AND / OR / NOT / +foo / -bar combine per-token regexes at the document level; phrase syntax does not apply.
    - The active source-set is applied before grouping, ranking,
      alias/title selection, and display truncation. Inactive-source
      documents do not influence result order or which title/alias is
      shown for an active result.

  - Phase 1, immediate: Server sends LP buffer content with response-type "search-results". Results are ordinary indefinitive non-content TrueNodes (not special scaffold types).

  - Phase 2, enrichment: A three-message sequence:
    1. Rust sends LP response-type "request-snapshot" with `(("content" "TERMS"))` — asking Emacs for a snapshot of the search buffer matching those terms.
    2. Emacs replies with `((request . "snapshot response") (terms . "TERMS"))\n` followed by `Content-Length: N\r\n\r\n<buffer text>` — the current buffer contents, including any unsaved user edits. Emacs sets the buffer to readonly before sending.
    3. Rust parses the snapshot, inserts containerward ancestry and graphnodestats, and sends LP response-type "search-enrichment" with `(("terms" "TERMS") ("content" "ORG") ("warnings" (...)))`. Emacs replaces the buffer and exits readonly.
    - Enrichment uses the same active source-set as the original
      search. Containerward ancestry truncates before inactive
      containers.

## Single root content tree view from ID
  - Request: ((request . "single root content view") (id . "NODE_ID") (view-uri . "URI"))
  - Response: length-prefixed content, formatted `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` constitutes `LENGTH` bytes. PAYLOAD may contain quotation marks; hence the length prefix. The payload shape is `((content "...") (errors ("..." ...)) (warnings ("..." ...)))`. The document structure is detailed below, under `Single root content tree view`.
  - If `NODE_ID` resolves to an inactive source, the server refuses the
    request with a human-readable message and does not open a buffer.
    Following a link to an inactive-source node behaves the same way.

## Save buffer
  - Request: First `((request . "save buffer") (view-uri . "URI") (point-lines-below-focused-headline . "N") (point-screen-lines-below-window-start . "M"))\n`, then `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` is the buffer content (`LENGTH` bytes).
    - `point-lines-below-focused-headline` is global to the buffer save: the number of text lines from the focused headline to point before save.
    - `point-screen-lines-below-window-start` is global to the buffer save: the number of screen lines from the window's top line to point before save. The server echoes both point fields in the final save response so Emacs can restore point and scroll position after replacing the buffer text.
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Early lock message (sent immediately, before the expensive pipeline):
       `Content-Length: N\r\n\r\n((response-type save-lock) (lock-views ("URI1" "URI2" ...)))`
       The URI list contains every other open view sharing at least one node with the saved view. Over-approximates: may include views that won't actually change. Emacs uses this to lock collateral buffers against edits.
    2. Zero or more collateral-view messages (one per affected view, streamed as each finishes):
       `Content-Length: N\r\n\r\n((response-type collateral-view) (view-uri "URI") (content "..."))`
       Emacs unlocks and updates each buffer as it arrives.
    3. Final save response:
       `Content-Length: N\r\n\r\n((response-type save-result) (content "...") (errors ("..." ...)) (warnings ("..." ...)) (point-lines-below-focused-headline N) (point-screen-lines-below-window-start M))`
       `content` is the re-rendered saved buffer (nil on failure). `errors` is a list of failure-explaining strings. `warnings` is a list of nonfatal messages. Both lists are present and empty if none.
  - If the server errors before sending the early lock message (e.g. malformed request), only one message is sent: the error response in the save-result format.

## Snapshot response (part of search enrichment; see "Text search" above)
  - Request: First `((request . "snapshot response") (terms . "TERMS"))\n`, then `Content-Length: N\r\n\r\n<buffer text>`.
  - Initiated by the client in response to a "request-snapshot" message from the server.
  - Response: LP response-type "search-enrichment" with enriched buffer content and an explicit `warnings` list.

## Get file path
  - Request: ((request . "get file path") (id . "THE_ID") (source . "THE_SOURCE"))
  - Response: Plain text with newline termination containing the file path relative to the skgconfig.toml directory, e.g. `skg/some-uuid.skg`.
  - Errors: If the file does not exist on disk, responds with "File not found: <path>".
  - Does not require TypeDB or Tantivy -- only the config.
  - If the requested source is inactive in the current connection's
    active source-set, the server refuses to expose the path. Direct
    file visits outside Skg are not intercepted.

## Git diff mode toggle
  - Request: ((request . "git diff mode toggle"))
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Git diff mode response:
       `Content-Length: N\r\n\r\n(("response-type" "git-diff-mode") ("content" "Git diff mode enabled"))`
       Includes warnings if any sources are not git-tracked.
    2. Rerender lock, per-view, and done messages (same as "rerender all views"):
       rerender-lock → rerender-view* → rerender-done.
  - Behavior: Toggles per-connection diff mode and re-renders all open views.
    When enabled, subsequent `single root content view` and `save buffer`
    responses include diff annotations showing changes
    between git HEAD and the worktree.

## Diff analysis
  - Request: `((request . "diff analysis") (include-staged . "BOOL") (include-unstaged . "BOOL"))`
    - `include-staged=true`, `include-unstaged=true`: compare HEAD to worktree.
    - `include-staged=true`, `include-unstaged=false`: compare HEAD to index.
    - `include-staged=false`, `include-unstaged=true`: compare index to worktree.
    - Both false is rejected.
  - Response: LP response-type "diff-analysis" with `((content "ORG") (errors ("..." ...)) (warnings ("..." ...)))`.
  - Behavior: Builds a semantic org report of affected nodes across all sources,
    including scalar node fields, aliases, extra IDs, all five schema relations
    in both role directions, text diffs for title/body, duplicate IDs across
    sources, and root/new/deleted/modified buckets.
  - Refuses to produce an ordinary report if any configured source is not a git
    repository, has no HEAD commit, has a selected merge-commit HEAD baseline,
    or contains an unreadable `.skg` blob in the selected snapshots.
  - Refuses to run unless the active source-set is `all`; restricted
    source-set diff reports are not defined yet.

## Rebuild databases
  - Request: ((request . "rebuild dbs"))
  - Response: LP response-type "rebuild-dbs" with `((content "..."))`.
    Content is "Databases rebuilt successfully." on success,
    or "Rebuild failed: ..." on error.
  - Behavior: Wipes and rebuilds both TypeDB and Tantivy from the .skg files on disk. Does not touch the filesystem. Also recomputes context rankings for search. Useful after importing new data or when the databases have stale metadata.

## Rerender all views
  - Request: ((request . "rerender all views"))
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Lock message:
       `Content-Length: N\r\n\r\n((response-type rerender-lock) (lock-views ("URI1" "URI2" ...)))`
       Lists all view URIs that will be re-rendered. Emacs locks those buffers.
    2. Zero or more per-view messages (one per view, streamed as each finishes):
       `Content-Length: N\r\n\r\n((response-type rerender-view) (view-uri "URI") (content "..."))`
       Emacs unlocks and updates each buffer as it arrives.
    3. Done message:
       `Content-Length: N\r\n\r\n((response-type rerender-done) (errors ("..." ...)) (warnings ("..." ...)))`
       Emacs removes the rerender-view handler and unlocks any remaining buffers.
  - Behavior: Re-renders every open view from server memory,
    applying diff annotations if diff mode is enabled.
    Does not save or modify the graph. Used after toggling
    git diff mode to refresh all views without requiring a save.

## Source sets
  - Request: ((request . "list source sets"))
  - Response: LP response-type "source-sets" with
    `((active "NAME") (sets ("NAME1" "NAME2" ...)))`.
  - Request: ((request . "active source set"))
  - Response: LP response-type "active-source-set" with
    `((active "NAME"))`.
  - Request: ((request . "set active source set") (name . "NAME"))
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Source-set response:
       `Content-Length: N\r\n\r\n(("response-type" "active-source-set") ("active" "NAME") ("content" "Active source-set: NAME"))`
    2. View-close/lock messages as needed by the Emacs client.
  - Behavior: The active source-set is per Emacs TCP connection. It
    defaults from `skgconfig.toml`, resets on reconnect, and is not
    written back to the config. Changing it requires confirmation in
    Emacs, closes/unregisters all active Skg buffers for that
    connection, and cancels any in-flight search enrichment.

## Titles by ids
  - Request: ((request . "titles by ids") (ids "uuid1" "uuid2" ...))
  - Response: LP response-type "titles-by-ids" with `((response-type "titles-by-ids") (content ((uuid1 . "title1") (uuid2 . "title2") ...)))`.
  - IDs not found in Tantivy are simply absent from the response, except
    that deleted `.skg` files visible in the current git diff can also
    supply titles.
  - Inactive-source IDs are omitted from the title map, even if they
    exist in Tantivy or on disk. `skg-readable-ids-mode` may still
    shorten UUIDs uniformly, but it must not title-annotate inactive
    IDs.
  - Used by `skg-readable-ids-mode` to annotate UUIDs in magit buffers with their titles.

## Shutdown server
  - Request: ((request . "shutdown"))
  - Has the same effect as sending SIGINT (Ctrl+C) or SIGTERM (kill) to the server.
  - Response: "Server shutting down..."
  - Behavior: `delete_on_quit` might be `= true` in the server's config file. (It defaults to false, and need not be mentioned.) If it's true, then the TypeDB database will be deleted before the server exits. This is primarily for integration tests to prevent database accumulation.
  - TODO | PITFALL: Any client can shut down the server. If ever multiple users share a server, one could bother the other. The server exits immediately after sending the response, which interrupts any in-flight requests from other clients.

## Busy-initializing signal
  - Triggered when Emacs connects while the server is still initializing TypeDB/Tantivy.
  - Response: ((busy-initializing . "human-readable status message"))
  - Emacs should display the message and retry the request (or let the user retry manually).
  - No request triggers this specifically; any request sent during initialization may receive it.

Error responses are sent as simple text.

# Single root content tree view

Rust sends the entire text document to Emacs, org-bullets and all.
Each org-bullet is followed by a space, some metadata, a space,
and then headline text.

The metadata is an s-expression containing bare values and key-value pairs,
wrapped in "(skg ...)" -- something like
```
 (skg (id long-string) repeated (key value) another-value)
```

Any such string is valid metadata
(although much of it might be ignored by Emacs),
if and only if it adheres to the following:

- parentheses
  - The metadata starts with "(skg" and ends with ")".
  - Key-value pairs are wrapped in their own parens: "(key value)".
- whitespace
  - Whitespace separates all elements.
  - Extra whitespace is ignored.
  - Keys and values should contain no whitespace.

Inside a `(node ...)` form, `(parentIs ...)` describes whether the
node participates in the collection represented by its visible parent:

- omitted `parentIs` means `affected`;
- `(parentIs affected)` is accepted but normally omitted when rendered;
- `(parentIs independent)` means the node is preserved/displayed but
  does not alter the parent's collection on save;
- `(parentIs absent)` is rendered for view roots.

`(birth ...)` records generated-view provenance, not save intent:

- omitted `birth` means `unremarkable`;
- `(birth containsParent)` marks containerward ancestry;
- `(birth linksToParent)` marks sourceward link-source ancestry.

# skgconfig.toml

The server's configuration file. See example-data/ for an example.

Pass its path as a command-line argument (default: `data/skgconfig.toml`) when starting the server (see `bash/start-servers.sh`). The directory containing the file is the **data root**; all relative paths in the config are resolved against it.

## Fields

| Field                | Required | Default | Description                                                      |
|----------------------|----------|---------|------------------------------------------------------------------|
| `db_name`            | yes      |         | TypeDB database name.                                            |
| `tantivy_folder`     | yes      |         | Directory for the Tantivy search index. Relative to data root.   |
| `port`               | no       | 1730    | TCP port for Rust-Emacs communication.                           |
| `initial_node_limit` | no       | 1000    | Max nodes to render in initial content views.                    |
| `delete_on_quit`     | no       | false   | Delete the TypeDB database on server shutdown. Mainly for tests. |
| `timing_log`         | no       | false   | When true, writes a JSON log to `<data_root>/logs/server.jsonl`. |
| `auto_audit_daily`   | no       | false   | When true, audits the in-Rust memory against TypeDB at most once per day, backgrounded at lowest priority. Mismatches are appended to `<data_root>/audits.org` and flagged to the client on the next outbound buffer. |
| `beep_when_server_becomes_available` | no | true | When true, plays a local sound after server initialization finishes. |
| `default_source_set` | no | `all` | Source-set active when a client connects. Runtime changes are per connection and are not written back. |

## Sources

Each `[[sources]]` entry defines a directory of `.skg` files:

| Field          | Required | Description |
|----------------|----------|-------------|
| `name`         | yes      | Unique name for this source (used in metadata, provenance). |
| `path`         | yes      | Directory containing `.skg` files. Relative to data root. |
| `user_owns_it` | yes      | `true` if the user can create/edit nodes here; `false` for foreign (read-only) sources. |

The source name `all` is reserved and rejected.

## Source sets

Each `[[source_sets]]` entry defines a named list of source names:

| Field     | Required | Description |
|-----------|----------|-------------|
| `name`    | yes      | Source-set name. Must be unique and must not be `all`. |
| `sources` | yes      | Non-empty list of existing source names. |

The source-set name `all` is predefined and means every configured
source. Users must not define their own `[[source_sets]]` entry named
`all`. Source names and source-set names may otherwise collide; this
allows singleton sets to be named after the source they contain.

Example:

```toml
default_source_set = "public"

[[sources]]
name = "public"
path = "public"
user_owns_it = true

[[sources]]
name = "private"
path = "private"
user_owns_it = true

[[source_sets]]
name = "public"
sources = ["public"]
```

## Log files

The server writes to `<data_root>/logs/`:
- `server-to-user.log` — human-readable log (always on).
- `server.jsonl` — structured JSON log (when `timing_log = true`). Queryable with `jq`.

## Example

See `example-data/skgconfig.toml`.

# The .skg file format

It is a specialization of YAML -- that is, every .skg file is valid YAML, although not vice-versa. The fields might change; the definitive source of truth is in `server/types.rs`. But for the moment they are:

- title: A string, with no newlines. Must be present.
- ids: A list of UUIDs. The first one is the primary id, which is equal to the filename minus the `.skg` extension. Must be non-empty. There can be more than one because nodes might be merged.
- body: An optional string, perhaps including newlines.
- contains: A list of UUIDs. These appear as the node's "children" in an org-view.
- subscribes_to: A list of UUIDs. Can be omitted if empty.
- hides_from_its_subscriptions: A list of UUIDs. Can be omitted if empty.
- overrides_view_of: A list of UUIDs. Can be omitted if empty.

Each node's filename is just its primary ID followed by ".skg".

# The metadata headers

Most org headlines in the client will have a 'metadata header',
consisting of some bare values and some key-value pairs
separated by whitespace. Each kv pair is wrapped in parens: (key value).
The whole collection is wrapped in (skg ...).
This metadata appears right after the org bullet and a whitespace,
and right before another whitespace and the node's title.

The metadata grammar (which keys are recognized and how they're
parsed) lives in `server/serve/parse_metadata_sexp.rs`. The
in-memory types those keys correspond to are in
`server/types/viewnode.rs` (`TrueNode`, `Scaffold`, `ScaffoldKind`, `RoleCol`)
and `server/types/git.rs` (`ExistenceAxes`, `MembershipAxes`, `Sign`).

Metadata is not WYSIWYG; its appearance in the client
is determined by `elisp/heralds-minor-mode`.

Role collection scaffolds are represented in Rust as
`Scaffold::RoleCol { roleCol: RoleCol }`, but their external metadata
remains the established bare scaffold atom:
`subscribeeCol`, `subscriberCol`, `overriddenCol`, `overriderCol`,
`hiderCol`, `hiddenCol`, `hiddenInSubscribeeCol`, or
`hiddenOutsideOfSubscribeeCol`.

## Diff-related metadata

When git diff mode is on, additional keys decorate nodes and
scaffolds with per-stage diff information. The 'staged' stage
compares HEAD to the index; 'unstaged' compares the index to the
worktree.

Inside a `(node ...)` form (TrueNodes):
- `(staged AXES)` and/or `(unstaged AXES)`, where `AXES` is a sequence
  of bare atoms drawn from `{newX, removedX, newM, removedM}`. `X` =
  existence (the node's `.skg` file appeared/disappeared in this
  stage); `M` = membership (the node's appearance at this position in
  the parent's contains list appeared/disappeared in this stage).
- bare atom `notInGit` if the node's source is not a git repo.

At top level inside `(skg ...)` (Scaffolds for Alias / ID):
- `(staged AXES)` and/or `(unstaged AXES)` with `AXES` drawn only
  from `{newM, removedM}` (scaffolds have no `.skg` file of their
  own and so no X axis).

Also at top level, for the TextChanged scaffold:
- `(textChanged STAGE_TAGS)` where `STAGE_TAGS` is a subset of
  `{staged, unstaged}` indicating which side(s) the node's title or
  body changed on.

Examples:
```
(skg (node (id 7) (source main) (unstaged newX newM)))
(skg (node (id 9) (source main) indef (staged removedM) (unstaged newM)))
(skg alias (staged newM))
(skg id (unstaged removedM))
(skg (textChanged staged unstaged))
```

## Inactive-source placeholder metadata

When a node from an inactive source appears as content of an active
container, it renders as a placeholder rather than as a TrueNode:

```
(skg (inactiveNode (id NODE_ID) (source SOURCE)))
```

The headline text is generic, e.g. `node from inactive source`. The
real ID and source are kept in metadata so saving can preserve, reorder,
duplicate, or remove the placeholder in the active container's
`contains` list. Herald rules must not display the source, and title
lookup/readable-ID helpers must not resolve the ID to a title while its
source is inactive.

The placeholder's own content is read-only: editing its title, body,
metadata, source, or ID is a buffer validation error. Moving or deleting
the placeholder as content of an active container is allowed and changes
that container's `contains` list.

In git diff mode, inactive placeholders may show only relationship
position markers such as `(unstaged newM)` or `(unstaged removedM)`.
They must not reveal title/body/alias/source-change diffs or deleted
phantom titles.
