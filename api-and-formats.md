# API

Communication between Rust and Emacs is via TCP on port 1730, configurable in any skgconfig.toml. The connection is persistent.

Note: Port 1729 is used for Rust-TypeDB communication (the TypeDB server), not Rust-Emacs communication.

So far there are these endpoints:

## Verify connection
  - Request: ((request . "verify connection"))
  - Response: Plain text with newline termination: "This is the skg server verifying the connection."

## Search in titles
  - Request: ((request . "title matches") (terms . "SEARCH_TERMS") (scope . "SCOPE"))
    - `scope` is optional; defaults to "rooty".
      - "rooty": only returns nodes with relationships suggesting they are especially rootlike -- so literal roots, but also cycle-roots, link targets, and things that had an ID when imported. Defined in the negative, 'rooty' excludes MultiContained and untyped nodes.
      - "everywhere": returns all Tantivy matches regardless of origin type, including the humblest leaves.

  - Phase 1, immediate: Server sends LP buffer content with response-type "search-results". Results are ordinary indefinitive parent_ignores TrueNodes (not special scaffold types).

  - Phase 2, enrichment: A three-message sequence:
    1. Rust sends LP response-type "request-snapshot" with `(("content" "TERMS"))` — asking Emacs for a snapshot of the search buffer matching those terms.
    2. Emacs replies with `((request . "snapshot response") (terms . "TERMS"))\n` followed by `Content-Length: N\r\n\r\n<buffer text>` — the current buffer contents, including any unsaved user edits. Emacs sets the buffer to readonly before sending.
    3. Rust parses the snapshot, inserts containerward ancestry and graphnodestats, and sends LP response-type "search-enrichment" with `(("terms" "TERMS") ("content" "ORG"))`. Emacs replaces the buffer and exits readonly.

## Single root content tree view from ID
  - Request: ((request . "single root content view") (id . "NODE_ID") (view-uri . "URI"))
  - Response: length-prefixed content, formatted `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` constitutes `LENGTH` bytes. PAYLOAD may contain quotation marks; hence the length prefix. The document structure is detailed below, under `Single root content tree view`.

## Save buffer
  - Request: First `((request . "save buffer") (view-uri . "URI"))\n`, then `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` is the buffer content (`LENGTH` bytes).
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Early lock message (sent immediately, before the expensive pipeline):
       `Content-Length: N\r\n\r\n((response-type save-lock) (lock-views ("URI1" "URI2" ...)))`
       The URI list contains every other open view sharing at least one node with the saved view. Over-approximates: may include views that won't actually change. Emacs uses this to lock collateral buffers against edits.
    2. Zero or more collateral-view messages (one per affected view, streamed as each finishes):
       `Content-Length: N\r\n\r\n((response-type collateral-view) (view-uri "URI") (content "..."))`
       Emacs unlocks and updates each buffer as it arrives.
    3. Final save response:
       `Content-Length: N\r\n\r\n((response-type save-result) (content "...") (errors ("..." ...)))`
       `content` is the re-rendered saved buffer (nil on failure). `errors` is a list of error/warning strings (empty list if none).
  - If the server errors before sending the early lock message (e.g. malformed request), only one message is sent: the error response in the save-result format.

## Snapshot response (part of search enrichment; see "Search in titles" above)
  - Request: First `((request . "snapshot response") (terms . "TERMS"))\n`, then `Content-Length: N\r\n\r\n<buffer text>`.
  - Initiated by the client in response to a "request-snapshot" message from the server.
  - Response: LP response-type "search-enrichment" with enriched buffer content.

## Get file path
  - Request: ((request . "get file path") (id . "THE_ID") (source . "THE_SOURCE"))
  - Response: Plain text with newline termination containing the file path relative to the skgconfig.toml directory, e.g. `skg/some-uuid.skg`.
  - Errors: If the file does not exist on disk, responds with "File not found: <path>".
  - Does not require TypeDB or Tantivy -- only the config.

## Git diff mode toggle
  - Request: ((request . "git diff mode toggle"))
  - Response: Plain text with newline termination.
    "Git diff mode enabled" or "Git diff mode disabled".
  - Behavior: Toggles per-connection state. When enabled,
    subsequent `single root content view` and `save buffer`
    responses include diff annotations showing changes
    between git HEAD and the worktree.

## Git diff mode toggle and rerender
  - Request: ((request . "git diff mode toggle and rerender"))
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Git diff mode response (same as standalone toggle):
       `Content-Length: N\r\n\r\n(("response-type" "git-diff-mode") ("content" "Git diff mode enabled"))`
    2. Rerender lock, per-view, and done messages (same as "rerender all views"):
       rerender-lock → rerender-view* → rerender-done.
  - Behavior: Combines "git diff mode toggle" and "rerender all views" into a single request. Toggles diff mode, sends the mode-change message (with warnings if any sources are not git-tracked), then streams re-rendered views.

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
       `Content-Length: N\r\n\r\n((response-type rerender-done) (errors ("..." ...)))`
       Emacs removes the rerender-view handler and unlocks any remaining buffers.
  - Behavior: Re-renders every open view from server memory,
    applying diff annotations if diff mode is enabled.
    Does not save or modify the graph. Used after toggling
    git diff mode to refresh all views without requiring a save.

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

## parentheses
  - The metadata starts with "(skg" and ends with ")".
  - Key-value pairs are wrapped in their own parens: "(key value)".
- whitespace
  - Whitespace separates all elements.
  - Extra whitespace is ignored.
  - Keys and values should contain no whitespace.

# skgconfig.toml

The server's configuration file. See example-data/ for an example.

Pass its path as a command-line argument (default: `data/skgconfig.toml`) when starting the server (see `bash/run-servers.sh`). The directory containing the file is the **data root**; all relative paths in the config are resolved against it.

## Fields

| Field                | Required | Default | Description                                                      |
|----------------------|----------|---------|------------------------------------------------------------------|
| `db_name`            | yes      |         | TypeDB database name.                                            |
| `tantivy_folder`     | yes      |         | Directory for the Tantivy search index. Relative to data root.   |
| `port`               | no       | 1730    | TCP port for Rust-Emacs communication.                           |
| `initial_node_limit` | no       | 100     | Max nodes to render in initial content views.                    |
| `delete_on_quit`     | no       | false   | Delete the TypeDB database on server shutdown. Mainly for tests. |
| `timing_log`         | no       | false   | When true, writes a JSON log to `<data_root>/logs/server.jsonl`. |

## Sources

Each `[[sources]]` entry defines a directory of `.skg` files:

| Field          | Required | Description |
|----------------|----------|-------------|
| `name`         | yes      | Unique name for this source (used in metadata, provenance). |
| `path`         | yes      | Directory containing `.skg` files. Relative to data root. |
| `user_owns_it` | yes      | `true` if the user can create/edit nodes here; `false` for foreign (read-only) sources. |

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

The possible metadata is specified in `server/types/orgnode.rs`.
Metadata is not WYSIWYG; its appearance in the client
is determined by `elisp/heralds-minor-mode`.
