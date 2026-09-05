# API

Communication between Rust and an Emacs or Neovim client is via TCP on port
1730, configurable in `skgconfig.toml`.  The interactive connection is
persistent.  The server process retains selected stores, maintenance, views,
observation and presentation state across sequential replacement connections.

The first request on every socket declares `(role . "interactive")` or
`(role . "control")`.  The one user-bearing role is `interactive`.  Exactly
one interactive socket may be live; a second is refused before it can obtain
user data or dispatch an ordinary endpoint.  After disconnect, either editor
may replace it, but must complete the census described below before ordinary
requests or writes are enabled.  `control` is content-blind and is allowlisted
to `begin reload batch`, `end reload batch`, and `shutdown`; it cannot query,
save, select disk, approve maintenance, or acknowledge presentation.

After initialization, every server response is length-prefixed as
`Content-Length: LENGTH\r\n\r\nPAYLOAD`; `LENGTH` is the number of
UTF-8 bytes in `PAYLOAD`. The exceptional busy-initializing signal is
described below.

Every client operation carries a fresh connection-local request ID:
`((request . "ENDPOINT") ... (request-id . "ID"))`. Endpoint request
examples below elide only this required final field. The clients serialize
foreground operations on each connection. Search's `snapshot response` is
the one continuation: it repeats the still-active search request ID instead
of allocating a new operation. An authorization retry is a new request and
therefore receives a new ID.

Reconciliation work may additionally carry `(incident-id . "UUID")`.
The server echoes it on every frame.  A request ID owns one wire exchange;
an incident ID owns the longer disk/store/presentation episode and is reused
by retries.  A client rejects a response whose incident identity does not
match its request record.

The related identities are deliberately not interchangeable:

| Identity | Owner and lifetime |
|---|---|
| request ID | One socket request/continuation exchange, ending in one terminal frame. |
| server operation ID | One unsolicited server push; durable state is recovered through status rather than by reusing a request ID. |
| incident ID | One complete durable maintenance or fatal-recovery episode across requests and reconnects. |
| candidate ID | One immutable, validated disk candidate based on a named selected generation. |
| maintenance epoch | One monotonically increasing edit/query/save policy interval. |
| graph generation | One coherent selected in-Rust graph and derived-store transition. |
| manifest revision | One exact selected path/byte manifest; it can advance on a semantic no-op without changing the graph generation. |
| presentation generation | One exact Git HEAD/index/worktree presentation signature. |
| server view revision | One accepted server forest for a view URI. |
| client application token | One exact installation of server text in an editor buffer. |
| client session ID | One editor process identity used to bind archive ownership; it grants no authority by itself. |

Every ordinary response payload carries:

```text
(request-id "ID")
(frame-kind KIND)
```

`response-type` remains as payload metadata, but clients dispatch by the
request ID and then by `frame-kind`; a stale or unknown ID is never routed by
response type. The final frame also carries exactly one of:

```text
(terminal-status complete)
(terminal-status complete-with-rejected-files)
(terminal-status failed)
(terminal-status needs-authorization)
```

Streaming frames such as `save-lock`, `collateral-view`, `rerender-lock`,
`rerender-view`, `search-results`, and `request-snapshot` have no terminal
status. Their operation remains active until its terminal frame. Handler
cleanup is request-local and runs once even when a handler fails. The
length-prefix parser is connection-local and is reset only when the
connection opens or is abandoned.

The shared ugly-telescope release warning is
`OPERATION includes title or body text selected below its node's home
source for PID P.` (or `PIDs P1, P2, ...`). A restricted request that
needs approval instead receives
`((response-type ugly-telescope-confirmation) (operation OPERATION)
(pids ("P" ...)) (prompt "..."))`; the response itself contains no
protected title or body. Except for search's include/exclude choice,
approval is always the exact request-local list
`(allow-ugly-telescopes "P" ...)` and is never cached.

Note: Port 1729 is used for Rust-TypeDB communication (the TypeDB server), not Rust-Emacs communication.

So far there are these endpoints:

## Verify connection

- Interactive request:

  ```text
  ((request . "verify connection")
   (role . "interactive")
   (client-kind . "emacs"|"neovim")
   (client-version . "VERSION")
   (client-session-id . "PROCESS-ID")
   (archive-format-version . 1)
   (native-undo-kind . "undo-fu-session"|"nvim-wundo"|"none")
   (native-undo-version . "VERSION")
   (source-set . "server-default"|"RETAINED-NAME"))
  ```

  `client-session-id` is nonempty and at most 256 bytes.  A claimed source-set
  must either request the server default or exactly match the retained sole
  session.  Archive and native-undo capabilities are checked again against the
  exact locked dirty-buffer census when maintenance starts.  Dirty Emacs undo
  currently requires public `undo-fu-session` 0.8; dirty Neovim undo requires
  `nvim-wundo` matching the advertised Neovim version.  Merely having another
  undo package installed is irrelevant, but an active `undo-tree-mode` is
  refused by the Emacs archive adapter.

- The `verify-connection` response includes compact source inventory and
  archive identities, telescope warnings, unresolved fatal-reload incidents,
  active source-set, graph generation, manifest revision, maintenance epoch
  and state, TypeDB/Tantivy health, and `(census-required true)`.  It never
  includes the complete selected path manifest.

- The client next sends `((request . "client census"))`, optionally carrying
  the active maintenance epoch, followed by a length-prefixed S-expression
  payload of descriptors.  The accepted descriptor authority is:

  ```text
  ((buffer-id . "ID") (kind . "KIND") (view-uri . "URI"|"nil")
   (graph-generation . N) (presentation-generation . N)
   (server-revision . N) (application-token . N)
   (dirty . "true"|"nil") (undo-required . "true"|"nil")
   (last-fetched-sha256 . "SHA256") (current-sha256 . "SHA256"))
  ```

  The server reattaches exact retained authority, reports stale buffer IDs,
  and requests full text only for reconstructible missing views.  If requested,
  `client census texts` carries a second length-prefixed payload with
  `buffer-id`, exact `last-fetched`, and exact `current` text.  No ordinary
  endpoint is admitted until the response says `(census-complete true)`.
  Reconnect assertions never overwrite newer retained server authority.

## Text search
  - Request: `((request . "text search") (terms . "SEARCH_TERMS")
    (regex . "BOOL") (body . "BOOL") (operators . "BOOL"))`, plus
    optional `(ugly-telescopes . "include")` or
    `(ugly-telescopes . "exclude")`.
    - Search always returns every match. "Rooty" nodes (literal roots, cycle-roots, link dests, and things that had an ID when imported) are ranked higher, via their context-origin multiplier.
    - `regex`, `body`, `operators` are optional; each defaults to "false".
      - "regex=true": interpret the query as a per-token regex; a RegexQuery is built directly and the QueryParser is bypassed.
      - "body=true": also search node bodies (titles are always searched).
      - "operators=true": preserve Tantivy phrase and operator syntax (AND / OR / phrase / +foo / -bar / grouping / field:). Intra-word operator chars still get escaped heuristically so C++ etc. remain findable. In regex mode, the query is parsed as a boolean expression over the per-token regexes: AND / OR / NOT / +foo / -bar, conventional precedence (NOT > AND > OR, bare adjacency = OR), and grouping via parens that stand ALONE between whitespace (a paren attached to a pattern is regex syntax within that piece). Malformed operator syntax is an error; phrase syntax does not apply. Detailed in docs/COMMANDS.org.
    - The active source-set is applied before grouping, ranking,
      alias/title selection, and display truncation. Inactive-source
      documents do not influence result order or which title/alias is
      shown for an active result.
    - `ugly-telescopes` is normally absent on the first request. Under
      a restricted source-set, if the index contains any telescope
      whose selected title or body lies below home, the server does no
      query and responds with LP
      `((response-type ugly-telescope-confirmation)
      (operation text-search) (pids ()) (prompt "..."))`. The empty PID
      list is deliberate: a pre-query decision must not disclose which
      ugly node might match. The retry uses `"include"` or `"exclude"`.
      Exclusion is applied inside the Tantivy query, before matching,
      and remains in force through asynchronous enrichment. The choice
      is request-scoped. Source-set `all` needs no choice and returns a
      warning if the results include ugly telescopes.

  - Phase 1, immediate: Server sends LP `((response-type
    search-results) (content "ORG") (warnings ("..." ...)))`.
    Results are ordinary indefinitive non-content TrueNodes (not
    special scaffold types). A no-match or error response has the same
    response type and a single `content` field.

  - Phase 2, enrichment: A three-message sequence:
    1. Rust sends LP response-type "request-snapshot" with `(("content" "TERMS"))` — asking Emacs for a snapshot of the search buffer matching those terms.
    2. Emacs replies with `((request . "snapshot response") (terms . "TERMS") (request-id . "SAME_ID"))\n` followed by `Content-Length: N\r\n\r\n<buffer text>` — the current buffer contents, including any unsaved user edits. Emacs sets the buffer to readonly before sending.
    3. Rust parses the snapshot, inserts containerward ancestry and graphnodestats, and sends LP response-type "search-enrichment" with `(("terms" "TERMS") ("content" "ORG") ("warnings" (...)))`. Emacs replaces the buffer and exits readonly.
    - Enrichment uses the same active source-set as the original
      search. Containerward ancestry truncates before inactive
      containers.

## Single root content tree view from ID
  - Request: `((request . "single root content view")
    (id . "NODE_ID") (view-uri . "URI")
    (override-choice . "CHOICE")
    (fresh-view . "true")
    (allow-ugly-telescopes "PID" ...))`
    - `override-choice` is optional; values are "menu" (the default)
      and "bypass". See "the override-choice menu" below.
    - `fresh-view` is optional and defaults false.  Detached recovery
      uses true so an already-open content root cannot turn the request into
      `switch-to-view`; the successful view is registered under the new
      client-generated URI and opened in an independent client buffer.
  - Response: LP `((response-type content-view) (content "...")
    (errors ("..." ...)) (warnings ("..." ...)))`. The document
    structure is detailed below, under `Single root content tree view`.
    Under a restricted source-set, a response involving an ugly
    telescope instead returns LP `ugly-telescope-confirmation` with
    `(operation single-root-view)` or `(operation override-menu)`, the
    exact PIDs, and a text-free prompt. An approved retry carries those
    exact PIDs in `allow-ugly-telescopes`; approval is not cached.
    Source-set `all` returns the ordinary response with a warning.
  - If `NODE_ID` resolves to an inactive source, the server refuses the
    request with a human-readable message and does not open a buffer.
    Following a link to an inactive-source node behaves the same way.
  - The override-choice menu: when the requested node is overridden
    (an `overrides_view_of` edge points at it, user-owned or
    foreign) and at least one overrider's source is active, the
    server returns, instead of a content view, an ordinary buffer
    of indefinitive nodes: the requested node as root, each visible
    overrider an Independent child of what it overrides, following
    the relation recursively (all edges), each branch stopping with
    the `cycle` viewstat at the first repeated ID. The response
    then carries two extra fields:
    `((content "...") (view-uri "override-menu:PID")
      (to-minibuffer "The requested node is overridden. Choose a destination.")
      (errors ()) (warnings (...)))`.
    The client must adopt the supplied `view-uri` (the server
    registers the menu under it; one menu per node, deduped) and
    show `to-minibuffer` via the echo area only -- never buffer
    text, never a popped window.
    Precedence: an open content view rooted at the node wins (the
    usual `(switch-to-view ...)` reply); a second menu request
    switches to the open menu; the menu appears in diff mode too.
  - `(override-choice . "bypass")` skips the menu and opens the
    requested node itself, drawn raw. Because the bypass-opened root
    is an overridden node drawn raw, its immediate children also draw
    raw (one level); substitution resumes at the grandchildren. See
    "Override substitution" below. Emacs sends bypass from magit
    buffers (readable-ID jumps land on the raw node) and from the
    command `skg-goto-bypassOverride`, the menu's escape hatch.

## Save buffer
  - Request: First `((request . "save buffer") (view-uri . "URI") (point-lines-below-focused-headline . "N") (point-column . "C") (point-screen-lines-below-window-start . "M"))\n`, then `Content-Length: LENGTH\r\n\r\nPAYLOAD`, where `PAYLOAD` is the buffer content (`LENGTH` bytes).
    - Two optional fields drive the fork-confirmation round-trip
      (below): `(fork-approved . "true")` marks a re-issued save whose
      forks the user approved, and
      `(fork-sources ((N . SOURCE) ...))` pairs each forked node's id
      N with the owned source chosen for its clone. Both are absent on
      an ordinary save.
    - `(hoist-approved-pids "PID" ...)` is present only on a retry
      after `telescope-hoist-confirmation`. It is the exact list the
      user approved for scalar publication; it is not a boolean and
      does not authorize any other PID or bypass ordinary save
      validation.
    - `(allow-ugly-telescopes "PID" ...)` is the shared scalar-release
      approval field. On save it appears only when retrying a
      `save-rerender` `ugly-telescope-confirmation`, and authorizes
      those exact PIDs for that response attempt. It is independent of
      Hoist authority: viewing text and publishing it on disk are
      different decisions.
    - `point-lines-below-focused-headline` is global to the buffer save: the number of text lines from the focused headline to point before save.
    - `point-column` is global to the buffer save: the column of point within its line, echoed back so the client can restore the exact cursor position.
    - `point-screen-lines-below-window-start` is global to the buffer save: the number of screen lines from the window's top line to point before save. The server echoes all three point fields in the final save response so Emacs can restore point and scroll position after replacing the buffer text.
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Early lock message (sent immediately, before the expensive pipeline):
       `Content-Length: N\r\n\r\n((response-type save-lock) (lock-views ("URI1" "URI2" ...)))`
       The URI list contains every other open view sharing at least one node with the saved view. Over-approximates: may include views that won't actually change. Emacs uses this to lock collateral buffers against edits.
    2. Save-relax-lock message (sent once the save plan is known):
       `Content-Length: N\r\n\r\n((response-type save-relax-lock) (lock-views ("URI1" "URI2" ...)))`
       Sent once the save plan is known, before the collateral-view stream; carries the exact set of views that will be rerendered, narrowing the earlier over-approximate save-lock set. The client may unlock any view that was locked by save-lock but is absent from this list.
    3. Zero or more collateral-view messages (one per affected view, streamed as each finishes):
       `Content-Length: N\r\n\r\n((response-type collateral-view) (view-uri "URI") (content "..."))`
       Emacs unlocks and updates each buffer as it arrives.
    4. Final save response:
       `Content-Length: N\r\n\r\n((response-type save-result) (content "...") (errors ("..." ...)) (warnings ("..." ...)) (point-lines-below-focused-headline N) (point-column C) (point-screen-lines-below-window-start M))`
       `content` is the re-rendered saved buffer (nil on failure). `errors` is a list of failure-explaining strings. `warnings` is a list of nonfatal messages. Both lists are present and empty if none.
  - The first ALTERNATIVE terminal message: after validation, a save
    rereads every telescope it will write. If any currently selects a
    title or body below home and lacks exact approval, nothing is
    committed and the server replies, in place of save-result,
    `((response-type telescope-hoist-confirmation) (telescopes
    (((pid "P") (home "SOURCE")) ...)) (prompt "..."))`.
    This response contains no title or body. The prompt explains that
    Hoist publishes the selected scalars at home and removes lower
    scalar copies while retaining lower relationships and aliases;
    Abort writes nothing and requires manual `.skg` repair. Hoist
    re-issues the SAME save with `(hoist-approved-pids "P" ...)`.
    The server rereads and reclassifies on that retry, so newly dirty
    PIDs produce another batch confirmation. Each attempt still sends
    `save-lock` first, and the confirmation is terminal for that
    attempt: neither `save-relax-lock`, `collateral-view`, nor
    `save-result` follows it.
  - The second ALTERNATIVE terminal message: a save that detects forks
    (edited foreign nodes, or explicit fork requests) and does NOT
    carry `(fork-approved . "true")` commits nothing and replies, in
    place of save-result,
    `((response-type fork-confirmation) (content "...") (to-minibuffer "..."))`,
    where `content` is the fork-confirmation buffer (an instructions
    headline, then per fork the clone-to-be over the original it
    forks; an unspecified clone source renders as the PICK-A-SOURCE
    placeholder under a `# Suggested source ...` comment). The client
    shows it, collects a source per placeholder, and on approval
    re-issues the SAME save with `(fork-approved . "true")` and
    `(fork-sources ...)`. Hoist is decided first: when a save needs
    both decisions, its approved Hoist retry then returns the fork
    confirmation, and neither attempt writes.
  - The third ALTERNATIVE terminal message protects the saved and
    collateral rerenders. After a valid save has updated disk and the
    derived stores, the server renders every affected view in memory.
    Under a restricted source-set, if the staged forests contain ugly
    telescopes without exact scalar-release approval, it changes no
    open-view registry entry, streams no content, and sends
    `((response-type ugly-telescope-confirmation)
    (operation save-rerender) (pids ("P" ...)) (prompt "..."))`.
    The save itself has succeeded; only its updated text is withheld.
    The client may retry the SAME save with
    `(allow-ugly-telescopes "P" ...)`. Under source-set `all`, or on an
    approved retry, the ordinary save-result carries the shared ugly-
    telescope warning. Declining leaves current client buffers and the
    server's open-view registry unchanged.
  - Exactly one of `save-result`, `telescope-hoist-confirmation`,
    `fork-confirmation`, and the save-rerender
    `ugly-telescope-confirmation` is sent as the terminal response to
    one save attempt.
  - If the server errors before sending the early lock message (e.g. malformed request), only one message is sent: the error response in the save-result format.

## Snapshot response (part of search enrichment; see "Text search" above)
  - Request: First `((request . "snapshot response") (terms . "TERMS") (request-id . "SAME_ID"))\n`, then `Content-Length: N\r\n\r\n<buffer text>`.
  - Initiated by the client in response to a "request-snapshot" message from the server.
  - Response: LP response-type "search-enrichment" with enriched buffer content and an explicit `warnings` list.

## Get file path
  - Request: ((request . "get file path") (id . "THE_ID") (source . "THE_SOURCE"))
  - Response: LP `((response-type get-file-path) (content "PATH"))`,
    normally with `PATH` relative to the data root. Errors use the same
    shape with human-readable `content` beginning `Error:`.
  - The server deliberately returns the computed path when the file no
    longer exists, so deleted nodes can still be located in a git diff.
  - Does not require TypeDB or Tantivy -- only the config.
  - If the requested source is inactive in the retained session's active
    source-set, the server refuses to expose the path. Direct
    file visits outside Skg are not intercepted.

## Git diff mode toggle
  - Request: ((request . "git diff mode toggle"))
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Git diff mode response:
       `Content-Length: N\r\n\r\n(("response-type" "git-diff-mode") ("content" "Git diff mode enabled"))`
       Includes warnings if any sources are not git-tracked.
    2. Rerender lock, per-view, and done messages (same as "rerender all views"):
       rerender-lock → rerender-view* → rerender-done.
  - Behavior: Toggles retained-session diff mode and re-renders all open views.
    When enabled, subsequent `single root content view` and `save buffer`
    responses include diff annotations showing changes
    between git HEAD and the worktree.
  - Refusal: ENABLING diff mode requires the active source-set `all`.
    Under a restricted set the server refuses: the git-diff-mode
    response carries the refusal text as `content`, followed by an
    EMPTY rerender stream (a rerender-lock naming no views, then
    rerender-done with no errors or warnings), so the client's
    preemptive buffer locks and stream guard unwind. Nothing else
    changes. Disabling diff mode is always allowed.
  - Before changing mode or streaming view text, a restricted request
    that would rerender an ugly telescope instead receives LP
    `ugly-telescope-confirmation` with
    `(operation diff-mode-rerender)` and
    the exact PIDs as its terminal frame. A retry
    adds `(allow-ugly-telescopes "PID" ...)`; declining leaves the mode
    and open-view registry unchanged.

## Herald rules
  - Request: ((request . "herald rules"))
  - Response: LP response-type "herald-rules" with `((content "RULES_SEXP"))`.
    `RULES_SEXP` is the herald rule table -- one sexp, rooted at `(skg ...)`,
    in the grammar `elisp/skg-sexpr/skg-lens.el` interprets. Inside it,
    strings are always double-quoted and match atoms are bare symbols;
    the distinction is load-bearing for the lens engine.
  - The table is defined in `server/heralds.rs`, the single home of the
    herald vocabulary and its presentation (labels, colors, order). Emacs
    fetches it once per connection (at `skg-client-init`), caches it for
    the session, and re-fetches on reconnect. If the fetch fails, heralds
    are disabled for the session; there is no client-side fallback table.
  - A Rust unit test (`herald_rules_cover_the_emittable_vocabulary`)
    pins the table to the metadata vocabulary the server can emit, in
    both directions. It pins atom coverage only, not presentation.

## Diff analysis
  - Request: `((request . "diff analysis") (include-staged . "BOOL") (include-unstaged . "BOOL"))`
    - `include-staged=true`, `include-unstaged=true`: compare HEAD to worktree.
    - `include-staged=true`, `include-unstaged=false`: compare HEAD to index.
    - `include-staged=false`, `include-unstaged=true`: compare index to worktree.
    - Both false is rejected.
  - Response: LP response-type "diff-analysis" with `((content "ORG") (errors ("..." ...)) (warnings ("..." ...)))`.
  - Behavior: Builds a semantic org report of affected nodes across all sources,
    including scalar node fields, aliases, extra IDs, all five schema relations
    in both role directions, text diffs for title/body, and
    root/new/deleted/modified buckets. Nodes are compared as whole
    telescopes: each endpoint's section files are grouped by pid and
    folded, so a change to one section reports against the node's
    full fold. A separate bucket ("IDs claimed by more than one
    node") reports duplicate-ID VIOLATIONS — an id claimed by two
    distinct pids at either endpoint. An id merely present in
    several sources is the normal telescope shape and is not
    reported.
  - Refuses to produce an ordinary report if any configured source is not a git
    repository, has no HEAD commit, has a selected merge-commit HEAD baseline,
    or contains an unreadable `.skg` blob in the selected snapshots.
  - Refuses to run unless the active source-set is `all`; restricted
    source-set diff reports are not defined yet.
  - Because it runs only under `all`, a report involving an ugly
    telescope is returned with the shared scalar-release warning; this
    endpoint has no approval retry.

## Stage moves
  - Request: ((request . "stage moves"))
  - Response: LP response-type "stage-moves" with
    `((content "SHELL_SCRIPT") (errors (...)) (warnings (...)))`.
  - Behavior: Scans the git status of every configured source and
    finds each node that "moved". Under telescopes a node's sections
    live in several sources at once, so the signal is TITLE
    presence, not file presence: a node moved iff its title vanished
    from EXACTLY one source (titled in that source's git HEAD,
    absent or titleless in its worktree) and appeared in EXACTLY one
    other (titled in the worktree, absent or titleless in HEAD).
    Titleless section creations and deletions move individual
    relationships between recording sources; they are not node
    moves, and stage as ordinary edits. An ID whose title vanished
    from, or appeared in, more
    than one source has more than one candidate (old, new) pair and
    is skipped.

    Only PURE moves — the old source's file gone from its worktree,
    the new source's file absent from its HEAD — are auto-staged. A
    move mixed into pre-existing section files (the destination
    already recorded relationships for the node, or the old source
    keeps a titleless section) is REPORTED in a comment block
    (`# MOVES DETECTED BUT NOT STAGED: ...`) and never staged, since
    staging those files whole could stage more than the move. The
    script is a bash move-list plus one loop over it:
    ```
    moves=(
      "<uuid> <old-source-dir> <new-source-dir>"
      ...
    )

    ORIGIN=$(pwd)
    for move in "${moves[@]}"; do
      read -r ID OLD NEW <<< "$move"
      echo "---- moving $ID : $OLD -> $NEW"
      cd "$ORIGIN/$OLD" || { echo "  SKIP: cannot enter $OLD"; continue; }
      if [ -e "$ID.skg" ]; then
        echo "  SKIP: $ID.skg is still present in $OLD; leaving it alone"
        continue
      fi
      echo "  removing $ID from $OLD"
      git rm "$ID.skg"
      echo "  adding $ID to $NEW"
      cd "$ORIGIN/$NEW"
      git add "$ID.skg"
    done
    cd "$ORIGIN"
    ```
    The `[ -e "$ID.skg" ]` check guards against a stale entry: `git rm`
    deletes whatever is present and gives no distinct output, so a file
    still present in the old source (the move not yet done on disk) is
    skipped rather than deleted. Source directories are named relative
    to the data root, so the script is meant to be run from there
    (bash); they may nest at any depth below it (e.g.
    `owned/personal-proc`), which is why every `cd` is absolute,
    anchored at `ORIGIN` (the data root, captured before the loop).
    The logic lives in
    `server/git_ops/find_and_stage_moves.rs`.
  - Scans all sources regardless of the active source-set (a move can
    cross source-set boundaries). Sources that are not git repos
    contribute nothing. When no moves are found, the script body is a
    single `# No moves detected.` comment.

## Rebuild databases

- The public editor commands `M-x skg-rebuild-dbs` and `:SkgRebuildDbs`
  bootstrap maintenance with origin `full-rebuild`; see “Durable maintenance
  protocol” below.  They archive editor state before any destructive store
  step, load and fence the governing config file, validate the complete disk
  candidate, then take an exclusive generation gate while reconstructing
  TypeDB, Tantivy, context rankings, and the selected graph/manifest.  Queries
  continue against G0 during preflight but are refused during the visibly
  exclusive reconstruction window.
- The old raw request `((request . "rebuild dbs"))` is retained only as a safe
  migration aid.  It never rebuilds or mutates stores.  It queues exact disk
  observation and returns a `rebuild-dbs` response instructing the user to run
  the editor command, where census and recovery archival are available.
- A valid changed config can replace runtime source catalogs, database/index
  locations, archive root and render limits as part of the same publication.
  Listener, tracing, audit-daemon, readiness-sound and shutdown-handler
  settings are startup-only and cause preflight to stop with an instruction to
  restart the server.  The selected G0 remains queryable after preflight
  failure; a destructive failure attempts complete G0 store reconstruction
  and otherwise leaves maintenance durably blocked.

## Recompute cyclic roots
  - Request: `((request . "recompute cyclic roots"))`.
  - Response: one terminal LP frame of type `recompute-cyclic-roots` whose
    content reports the cyclic-root and rewritten-document counts.  Failure
    is terminal status `failed`, retains the previously selected cache, and
    recommends `skg-rebuild-dbs` when Tantivy is unhealthy.
  - Behavior: computes the authoritative whole-graph cyclic-root set from an
    immutable graph generation without holding the graph-writer lock.  If
    that generation advances, it discards the result and retries.  It writes
    only changed context labels and publishes the cache only after Tantivy
    commits successfully.

## Edge source info
  - Request: ((request . "edge source info") (owner . "ID")
    (member . "ID") (relation . "contains")) — relation is one of
    `contains`, `subscribes_to`, `overrides_view_of`: the three
    relations an explicit `(relSource ...)` atom can name.
  - Response: LP response-type "edge-source-info" with
    `((default "NAME") (current "NAME"))`. `(current ...)` is absent
    when the graph records no such edge (e.g. one typed into a
    buffer and not yet saved). On failure, `((error "..."))` — e.g.
    an endpoint the graph does not know.
  - Behavior: between owned endpoints, `default` is the more-private
    home. From an owned owner to a foreign member, `default` is the
    owner's home regardless of privacy order; this deliberately
    exposes the member ID and relationship to that owned source's
    readers, while avoiding any proposed foreign write. `current` is
    the edge's recording source.
    `skg-set-relationship-source` uses the reply to offer only
    sources the save can accept. Advisory: the save-time floor check
    in `apply_sticky_sources` stays load-bearing, since buffers go
    stale and the atom is plain text.

## Strip body whitespace
  - Request: ((request . "strip body whitespace"))
  - Response: LP response-type "strip-body-whitespace" with
    `((content "..."))`. Content reports how many files changed and
    a per-source breakdown, or "Body whitespace strip failed: ...".
  - Behavior: Strips trailing whitespace from every line of every
    body, and trailing blank lines from each body's tail, in every
    OWNED source in the config. Foreign sources are read-only and
    left untouched (stripping them would make them diverge from
    their upstreams). Rewrites exactly the .skg files whose bodies
    changed (a clean file stays byte-identical); a body that strips
    to nothing is dropped from its file. The in-Rust graph and Tantivy are refreshed to match;
    TypeDB needs no update (it stores no body text, and textlink
    extraction cannot see trailing whitespace). The clients suggest
    reviewing the result with `git diff --ignore-all-space`, which
    should show nothing.

## Export to org
  - Request: `((request . "export to org") (source-set . "NAME")
    (output-dir . "PATH") (allow-ugly-telescopes "PID" ...))`.
    - Both fields are required; a missing or blank `output-dir` is an
      error (the server applies no default -- the client supplies the
      user a default but always sends a value). `output-dir` is
      resolved against the server's working directory (its project
      root): a relative PATH lands under it, an absolute PATH is used
      as-is. The client does not tab-complete it, since the server
      may run in a container whose filesystem differs from the
      client's.
  - Response: LP response-type "export-to-org" with
    `((content "REPORT") (errors ("..." ...)) (warnings ("..." ...)))`.
    `content` is a human-readable report (files written, broken-link
    count, warning count). `errors` is non-empty only on failure
    (bad/unknown source-set name, unreadable `.skg` files, filesystem
    error). `warnings` are non-fatal (skipped roots, ambiguous
    headings, broken links, body lines that look like org structure).
  - Behavior: Reads all `.skg` files fresh from disk, finds every
    export root, and writes each to
    `<output-dir>/<target_filepath>.org` as a recursive content view
    limited to `NAME`, stripped of skg metadata, with
    `[[id:..][label]]` links rewritten to relative org links.
    Existing files are overwritten; others are left untouched. Needs
    neither TypeDB nor Tantivy.
    Before writing anything, a restricted export involving ugly
    telescopes returns LP `ugly-telescope-confirmation` with
    `(operation export-to-org)` and the exact PIDs. An approved retry
    carries those PIDs in `allow-ugly-telescopes`. An `all` export, or
    an approved restricted export, returns the ordinary export response
    with the shared warning.
  - An **export root** is a node one of whose `contains` children has
    a title linking to the instruction node
    `3d9aa9be-d95a-48bc-b362-33f9e7ebdf6f` and a body yielding a
    `target_filepath` (a tolerant `key = value` parse; the value may
    be bare or quoted, but a bare value with whitespace is refused).
    The marker child is omitted from output. A nested export root is
    rendered as a link to its own file, not inlined. Links whose
    dest is not exported under `NAME` point to the export of
    `9ff04e25-01e8-4634-8aa5-f5849bc1eb81` ("Some links might be
    broken."); if that note is itself not exported, the link degrades
    to plain label text.
  - The same export is available as a CLI subcommand (no TypeDB):
    `cargo run --bin skg -- export-org [config-path] [source-set]
    [output-dir] [--include-ugly-telescopes]`. Under a restricted set it
    fails closed before writing unless the flag is present; under `all`
    (and on an approved restricted run) it prints the warning.

## Rerender all views
  - Request: `((request . "rerender all views")
    (allow-ugly-telescopes "PID" ...))`.
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
  - All view text is prepared in memory before the release decision.
    Under a restricted set, ugly content without exact approval returns
    LP `ugly-telescope-confirmation` with
    `(operation rerender-all-views)` and
    the exact PIDs as its terminal frame. No view
    text is sent and no open-view registry entry changes. A retry adds
    `allow-ugly-telescopes`; `all` instead adds the shared warning to
    `rerender-done`.

## Source sets
  - Source-sets are the prefixes of the config's privacy order (see
    "skgconfig.toml > Source sets" below): each source name denotes
    itself plus everything more public, and `all` means every source.
  - Request: ((request . "list source sets"))
  - Response: LP response-type "source-sets" with
    `((active "NAME") (sets ("NAME1" "NAME2" ...)))`. The sets are
    the source names in privacy order (most public first) followed
    by `all`; order is meaningful (it is the privacy ladder), so
    clients should preserve it.
  - Request: ((request . "active source set"))
  - Response: LP response-type "active-source-set" with
    `((active "NAME"))`.
  - Request: `((request . "set active source set") (name . "NAME")
    (allow-ugly-telescopes "PID" ...))`.
  - Response: Multiple length-prefixed messages, sent sequentially:
    1. Source-set response:
       `Content-Length: N\r\n\r\n(("response-type" "active-source-set") ("active" "NAME") ("content" "Active source-set: NAME"))`
    2. The rerender stream, as in "rerender all views":
       rerender-lock → rerender-view* → rerender-done. Each open view
       is re-rendered in place under the new set: nodes whose source
       became inactive are converted and pruned (an inactive node
       with active view-children is retained, read-only), and
       PartnerCols regenerate per the new set, so widening the set
       reveals members and cols.
  - Behavior: The active source-set belongs to the sole retained interactive
    session. It defaults from `skgconfig.toml` when that session is first
    created, survives replacement connections, and is not written back to the
    config. Changing it requires confirmation in the editor, re-renders (not
    closes) all active Skg buffers, and cancels any in-flight search
    enrichment.
  - Refusal: switching to a set other than `all` while git diff mode
    is on is refused, before any side effect (no enrichment
    cancellation, no set change, no rerendering). The refusal — and
    likewise the endpoint's error paths (missing or unknown set
    name) — answers in the normal active-source-set response-type,
    carrying the explanatory text as `content` and the UNCHANGED set
    as `active`, followed by an EMPTY rerender stream (a
    rerender-lock naming no views, then rerender-done), so the
    client's preemptive buffer locks and stream guard unwind.
    Switching TO `all` is always allowed. The resulting
    retained-session invariant: diff mode on implies active source-set
    `all`.
  - Rerender authorization precedes both the set change and release of
    view text. If the proposed set would render an ugly telescope, the
    first reply is LP `ugly-telescope-confirmation` with
    `(operation source-set-switch-rerender)` and exact PIDs as its terminal
    frame. A retry adds `allow-ugly-telescopes`; declining
    leaves the active set and open-view registry unchanged.

## Titles by ids
  - Request: `((request . "titles by ids") (ids "uuid1" "uuid2" ...)
    (allow-ugly-telescopes "PID" ...))`.
  - Response: LP `((response-type titles-by-ids)
    (content (("uuid1" . "title1") ("uuid2" . "title2") ...))
    (warnings ("..." ...)))`.
  - IDs not found in Tantivy are simply absent from the response, except
    that deleted `.skg` files visible in the current git diff can also
    supply titles.
  - Inactive-source IDs are omitted from the title map, even if they
    exist in Tantivy or on disk. `skg-readable-ids-mode` may still
    shorten UUIDs uniformly, but it must not title-annotate inactive
    IDs.
  - Used by `skg-readable-ids-mode` to annotate UUIDs in magit buffers with their titles.
  - Under a restricted source-set, requested titles involving ugly
    telescopes instead return LP `ugly-telescope-confirmation` with
    `(operation titles-by-ids)` and the exact PIDs. An approved retry
    carries those PIDs in `allow-ugly-telescopes`; `all` returns the
    ordinary map with the shared warning.

## Disk observation hint (retired `reload paths` spelling)

- Request: `((request . "reload paths") (paths "PATH" ...))`, or the
  full-corpus form with `(full-sweep . "true")`.
- This compatibility endpoint is observation-only.  It queues work on the
  process-owned low-priority observer and returns terminal response type
  `reload-paths` with `(observation-queued true)`.  It does not allocate an
  incident, lock a view, select disk, or mutate a store.  Direct IDs are
  refused with an instruction to use the maintained editor command.
- The public path/ID commands instead bootstrap `explicit-partial-reload`
  maintenance.  Each resolved primary PID observes every possible
  cross-source telescope section; an extra-ID change takes the conservative
  complete-corpus fallback.  Unknown IDs are retained as rejected outcomes,
  while acknowledged IDs clear from the Emacs selection buffer only after the
  incident's terminal ACK.
- The Rust watcher, not either editor, owns normal source-directory
  observation.  `skg-reload-changed` and a Magit refresh can still request an
  exact BLAKE3 full sweep as a hint.  A raw `.skg` save needs no client hook:
  the server watcher observes it independently of the editor connection.

## Fatal reload recovery

  - A stable reload containing an unloadable telescope commits unrelated
    legal changes, retains that telescope's last-good store state, and writes
    an atomic user-only journal outside every source repository before it
    offers recovery. The journal contains exact incident bytes and absence
    tombstones, the pre-incident/legal telescope manifests, repository bases
    and fatal reasons. It survives disconnect and restart and appears as
    `pending-recovery-incidents` in the verify-connection response.
  - Recovery request: `((request . "reload recover")
    (approved . "true") (incident-id . "UUID"))`. The server rejects a
    request without explicit approval. Emacs supplies the confirmation UI;
    Neovim currently reports the unresolved incident visibly and does not
    recover it.
  - Recovery first revalidates the complete incident-time `.skg` overlay.
    For every safe owned repository it creates, by Git plumbing only, the
    common refs `oops-N_1-pre-incident`,
    `oops-N_2-with-only-legal-changes-from-incident`, and
    `oops-N_3-with-all-changes-from-incident`. The latter two commits are
    siblings whose parent is the first. It never checks out or stages. Only
    after all required refs exist does it restore owned fatal paths to the
    legal manifest. Detached/sequencer and foreign-only repositories are
    warning-only.
  - Successful response type `reload-recovery` names each repository, ref,
    commit and restored path. Failure leaves the journal unresolved. An
    incident-byte mismatch returns terminal status `failed` with
    `(successor-required true)`; Emacs assigns a fresh incident and requests
    an exact full sweep while preserving the original journal and any evidence
    refs already published. An
    explicit permanent dismissal uses the same endpoint with
    `(action . "dismiss") (approved . "true")`; it deletes only the private
    journal and explains that automatic recovery becomes impossible.

## Reload-batch control bracket

  - Begin request: `((request . "begin reload batch") ...)`.
    Response type `reload-batch` carries an opaque `batch-token` and the
    process-wide `active-count`.
  - End request: `((request . "end reload batch")
    (batch-token . "TOKEN") ...)` on the same connection.  A token belongs to
    its connection, so overlapping drivers cannot close each other's bracket.
    EOF releases every token owned by that connection.
  - `data/bash/pull-all.sh` holds this small control connection around its
    serial repository loop and sends end from its exit trap.  If Skg is not
    reachable, pulling still proceeds.  While any token remains active, Skg
    starts no observed reload.
  - When the last token ends or its connection is lost, the server sends the
    process-owned observer one exact full-sweep request and may also send the
    verified interactive client a `reconciliation-ready` progress frame with
    a monotonic `sweep-generation`.  Correctness does not depend on a connected
    editor reacting to that frame, and the shell process never needs access to
    editor-local buffer state.

## Process-owned observation and pending disk work

The server installs one nonrecursive watcher for every normalized source
directory and also schedules exact full sweeps at startup, after watcher gaps,
after an external batch, after source-catalog replacement, and after a save
fence discovers drift.  The low-priority observer reads disk without changing
the selected generation:

- **G0** is the coherent selected graph, TypeDB projection, committed Tantivy
  generation, exact path manifest, and store-health tuple used by queries.
- An **observation** compares exact BLAKE3 path/byte manifests against a named
  G0.  A byte-equivalent result does nothing.  A semantically equal rewrite
  advances only the selected manifest revision.
- **Gdisk** is an immutable exact-byte candidate that folded and validated
  successfully and differs semantically from its base G0.  It has a
  server-allocated candidate ID but is not selected.
- **G1** is that candidate only after maintenance has coherently committed and
  published all selected stores and the exact manifest.

A valid Gdisk changes coordinator state to `pending` with reason
`valid-disk-difference` and queues a server-push `maintenance-offer`.  The push
has a server operation ID plus candidate ID, base graph generation, base
manifest revision, changed primary IDs, observation reasons and paths.  The
user may decline the prompt: G0 remains editable and queryable, but every Skg
view save is refused until the pending disk work is reconciled.  A later disk
change can supersede the candidate.

Invalid or unstable exact bytes produce persistent pending reasons
`invalid-disk` or `unstable-disk` and a server-push `maintenance-status` rather
than a candidate.  Observation failures use `observation-failure`.  None of
these states silently changes selected stores.  `maintenance status` reports
`idle`, `observing`, `pending`, `active`, `terminal`, or
`blocked-store-health`, with explicit kebab-case phase/reason labels rather
than implementation debug text.

## Durable maintenance protocol

Maintenance is the only live route for selecting disk not produced by the
current Skg save.  It covers accepted out-of-band candidates, explicit path/ID
reload, client-owned pull, and full rebuild/config replacement.  One durable
coordinator and a monotonically increasing epoch fence editor writes.  Every
request after bootstrap repeats the exact incident ID and epoch; the server
also requires the same archive-owner client session.

### Entry, locked census, and initial archive

1. The client sends:

   ```text
   ((request . "begin maintenance")
    (origin . "pending-reconciliation"|"explicit-partial-reload"|
              "pull"|"full-rebuild")
    (candidate-id . "UUID"|"none")
    (paths "PATH" ...) (ids "ID" ...)
    (pull-repositories (((repository-key . "KEY")
                         (sources ("SOURCE" ...))) ...)))
   ```

   Only fields appropriate to the origin are present.  Pull requires active
   source-set `all`, and its logical repository/source mapping must match the
   server's configured Git topology.  Every origin refuses entry if a visited
   configured raw `.skg` buffer is modified.

2. The terminal `maintenance-offer` allocates incident ID, maintenance epoch,
   UTC start, strict archive directory name, archive folder/identity, G0,
   retained source-set, targets and candidate.  Both clients immediately put
   the epoch lock on every registered buffer, including buffers created while
   the incident is active, and submit an incident-qualified descriptor census.
   `maintenance locked census` freezes exactly that acknowledged inventory.
   Its checksum is the SHA-256 of sorted buffer IDs separated by NUL bytes.

3. The client writes the complete initial archive under same-filesystem
   `.staging`, verifies closed lengths/checksums, atomically renames it to its
   strict final name, and creates `ARCHIVE-READY` last.  It then sends:

   ```text
   ((request . "maintenance archive ready")
    (maintenance-epoch . N)
    (manifest-sha256 . "SHA256")
    (lock-census-sha256 . "SHA256"))
   ```

   The server independently validates the archive root, private modes,
   complete declared inventory, marker, manifest checksum, incident/epoch,
   client ownership, census checksum and unchanged G0.  Only its exact ACK
   crosses the point at which pull or destructive rebuild work may begin.

If a required native undo sidecar cannot be written, `maintenance archive
failed` records the exact buffer key and reason and returns
`undo-waiver-required`.  `approve undo waiver` must repeat that exact tuple;
it authorizes a text-only retry for that buffer, not a general weakening.
`cancel maintenance` is valid only before `ARCHIVE-READY` and returns the exact
epoch whose locks can be removed.

### Origin work and store selection

`run maintenance origin` advances an archived explicit reload or full rebuild
to server-owned final observation.  For pull it returns
`external-mutation-authorized`: the client then executes configured
repositories serially with argument-vector Git processes and finally sends
`finish maintenance origin` with `external-outcome` `completed`, `failed`, or
`indeterminate`, plus incident-qualified diagnostic details.  A nonzero Git
exit does not prove that disk is unchanged, so final observation still runs.

Partial selection and pull remain queryable through a pinned coherent G0.
Full rebuild takes an exclusive query gate only after complete config/corpus
preflight; requests during its destructive reconstruction receive an explicit
outage error.  Candidate bytes and governing config bytes are revalidated at
the selection boundary.  The server journals candidate identity, store
transition and semantic evidence before publishing G1.  Tantivy acknowledgement
names the exact generation; an incremental failure reconstructs from the
immutable selected graph, while an unrecoverable derived-store failure leaves
store health and maintenance explicitly blocked.

If final disk after an external mutation is invalid or unstable, the incident
stays locked in `blocked-invalid-after-mutation`; it is not rolled back into a
false success.  Preflight failure before destructive work leaves G0 queryable.
After a destructive full-rebuild failure, the server attempts complete G0
reconstruction before reporting the durable failure state.

### Scalar release and view settlement

Once G1 is selected, status carries the exact source inventory/source-set,
G1 graph and manifest generations, Tantivy outcome, server-evidence checksum,
and one settlement per frozen buffer.  A restricted source-set which would
release protected scalar text first returns `needs-scalar-authorization` with
only operation, exact PID list and prompt.  `approve maintenance scalar
release` must repeat the exact list; no staged view text precedes approval.

Each settlement freezes buffer ID/kind/URI, dirty and impact classification,
base graph/presentation generation, server revision and application token,
planned disposition, and required ACK.  The policies are:

- dirty impacted or parse-uncertain views are **retired** in place as detached
  recovery buffers; their text and native undo remain, but live URI and Skg
  save authority are removed;
- dirty orthogonal views are released byte-for-byte against G1 and marked
  presentation-stale; search views are separately marked search-stale;
- clean impacted views receive an exact staged rendering with content SHA-256
  and one-step resulting generations/revision/token;
- clean orthogonal views remain untouched; explicitly disposable clean menus
  may close.

`maintenance view settled` repeats all frozen authority.  An application ACK
also repeats content SHA-256 and every resulting authority counter.  The server
journals each ACK before installing its retained forest and refuses stale,
changed, duplicate-inconsistent, or missing-buffer authority.  Exact replay is
idempotent.

Maintenance never automatically reruns a search.  A refreshed search buffer
keeps its old membership and ranking and is marked search-stale; graph-derived
display such as heralds may separately be presentation-stale.  The warnings
remain visible until an explicit fresh query or applicable rerender clears the
corresponding condition.

### Evidence, final archive, terminal delivery, and reconnect

After every settlement ACK, `maintenance evidence` requests the exact
`server-evidence-sha256`.  The reply is the artifact bundle described below.
The client verifies its descriptor and opaque bytes, installs evidence
categories replay-safely, writes `manifest.final.sexp`, rewrites the human
`incident.org` and interrupted-buffer index, and creates `FINALIZED` last.
It then sends `maintenance archive finalized` with the final-manifest,
transfer-manifest, and complete opaque-byte SHA-256 values.  Changed checksums
are refused even on retry.

`complete maintenance` repeats the final-manifest checksum and yields a
durable `terminal` response containing incident, epoch, disposition, exact
unlock census, selected generations, and per-ID outcomes.  The client unlocks
only that census, then sends `acknowledge terminal maintenance`.  Only this
last exact ACK returns the coordinator to idle and compacts its transaction
journal.  Lost replies at the view, evidence, finalization, completion, or
terminal boundary can be replayed without applying the local action twice.

`maintenance status` is the reconnect/resume endpoint.  An active response
includes origin, phase, targets, frozen census, candidate/G0/G1 evidence and
settlements, including which ACKs are already durable.  A terminal response
replays the exact unlock instruction.  Connection loss never converts an
active incident into idle and never unlocks views merely because a socket
ended.

## Artifact-bundle frames and recovery archives

An artifact bundle uses this header:

```text
Content-Length: TOTAL_BYTES\r\n
Content-Type: application/x-skg-artifact-bundle\r\n
Descriptor-Length: UTF8_DESCRIPTOR_BYTES\r\n
\r\n
DESCRIPTOR || OPAQUE_ARTIFACT_BYTES
```

`Content-Length` covers both portions.  The descriptor is one ordinary
enveloped UTF-8 S-expression; `Descriptor-Length` is its exact byte boundary.
Its ordered artifact records contain safe relative path, purpose, byte offset,
byte length and lowercase SHA-256.  Offsets are contiguous and cover the
opaque tail exactly.  Clients must not decode, normalize newlines, base64, or
count characters in the artifact bytes; non-UTF-8 `.skg` evidence is valid.

The archive root defaults beside `skgconfig.toml` and must be structurally
disjoint from every source.  Directories are mode 0700 and regular files mode
0600 from first creation on POSIX.  Archive operations reject `..`, unsafe
components, symlinks, traversal, undeclared files, changed rereads and
non-private modes.  The portable top-level shape is:

```text
.staging/INCIDENT.CLIENT-NONCE.partial/
TIMESTAMP_UUID/
  ARCHIVE-READY
  FINALIZED
  manifest.initial.sexp
  manifest.final.sexp
  incident.org
  buffer-snapshots/...
  interrupted-buffers/README.org
  new-nodes/... | deleted-nodes/... | modified-nodes/... | invalid-paths/...
```

`ARCHIVE-READY` binds archive format, incident and initial-manifest checksum.
`FINALIZED` binds incident, final-manifest checksum and transfer-manifest
checksum.  Marker filenames alone are not authority.  `incident.org` and
buffer README/diff files are human aids; the versioned manifests and checksums
are machine authority.  Incomplete `.staging` or missing-marker publications
are retained for inspection and never auto-promoted or auto-deleted.

The server's config-keyed private YAML maintenance journal has a different
job: it is temporary transaction authority for phases, store selection,
settlement and transfer ACKs, and may be compacted only after the terminal
ACK.  The client incident directory is permanent recovery evidence.  Skg
reports exact and IEC size for the new archive and all retained archives; it
never deletes one automatically.  A retired buffer is already detached
recovery: open a fresh live view for current G1 rather than granting that old
text a new live URI implicitly.  Archive deletion, when exposed by a client,
must be an explicit confirmed operation confined to one validated incident;
manual deletion has the same irreversible loss of native undo and evidence.

## Shutdown server
  - Request: ((request . "shutdown"))
  - Has the same effect as sending SIGINT (Ctrl+C) or SIGTERM (kill) to the server.
  - Response: LP `((response-type shutdown) (content "Server shutting down..."))`.
  - Behavior: `delete_on_quit` might be `= true` in the server's config file. (It defaults to false, and need not be mentioned.) If it's true, then the TypeDB database will be deleted before the server exits. This is primarily for integration tests to prevent database accumulation.
  - The sole interactive connection or an explicitly authenticated control
    connection may request shutdown.  There is no multi-user or secondary
    interactive role.  The server exits after sending the response, so the
    caller should not use shutdown while another operation is in flight.

## Busy-initializing signal
  - Triggered when an editor connects while the server is still initializing TypeDB/Tantivy.
  - Response: the unprefixed, newline-terminated
    `((busy-initializing . "human-readable status message"))`.
  - The editor should display the message and retry the request (or let the user retry manually).
  - No request triggers this specifically; any request sent during initialization may receive it.

Once initialization is complete, endpoint errors use that endpoint's
normal tagged, length-prefixed response shape (or the shared
`response-type error` dispatcher fallback); they are not bare text.

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

The bare atom `hiddenBody` accompanies `indef` on an indefinitive
node whose graph node HAS a body — one the rendering hides. Herald
"B", hugging the ☮. Display-only: the parser accepts and discards it;
the view regenerates it.

`(birth ...)` records generated-view provenance, not save intent:

- omitted `birth` means `unremarkable`;
- `(birth backpath ROLENAME)` marks a node grafted by the backpath
  engine as an ancestry partner playing ROLENAME toward its org-parent
  (e.g. `container` for a containerward ancestor, `linkSource` for a
  node that links to it, also `linkDest`, `overrider`, `overridden`,
  `hider`, `hidden`, `subscriber`, `subscribee`). ROLENAME is one of the
  nine in `PARTNER_ROLE_VOCAB`
  (`server/dbs/in_rust_graph/relation_accessors.rs`). The graft's herald
  is not a fixed glyph: it is the ancestor-lettered relationship token
  the uniform-herald grammar assembles for that role (see "Stats
  metadata" — e.g. an `overrider` graft reads `Oa`, an `overridden`
  graft `aO`).

## View requests: (viewRequests ...)

`(viewRequests ...)` carries client→server REQUESTS for extra views
(unlike `birth` and the stats below, which are server→client). On save
the server fulfills each request during view completion and then drops
the atom, so a request is transient. Three request forms:

- `(col RELNAME)` — build BOTH cols of the relation, populated from the
  graph. RELNAME is `aliases`, `overrides`, `hides`, or `subscribes`.
  The writable col (`overriddenCol` / `subscribeeCol` / `aliasCol`)
  appears even when empty (its editable "add here" surface); an empty
  read-only col is pruned. Emitted by the `C-c c` commands.
- `(path ROLENAME)` — build the backpath for one partner role, grafting
  the partners as inverted read-only children (each marked `(birth
  backpath ROLENAME)`). ROLENAME is one of the nine in
  `PARTNER_ROLE_VOCAB`. Emitted by the `C-c p` commands.
- `definitiveView` — make an indefinitive, childless node editable.
- `fork` — the explicit `skg-fork-node` gesture: clone this (owned)
  node into a private fork that overrides it. Consumed on the save path
  at fork detection (not during view completion), then dropped. Emitted
  by `skg-fork-node` (`C-c m f`).

## Stats metadata: graphStats and viewStats

Generated display facts decorate a `(node ...)` (never save intent;
the client renders them as heralds via the rule table in
`server/heralds.rs`; see "Herald rules"). Since the uniform-herald
refactor, the graph-wide counts and the view-position RELATIONSHIP
facts are assembled into two compact token strings rather than emitted
as individual atoms:

- `(birthHerald "STRING")` — orange, hugging the ☮; the token(s) for
  the relation(s) that explain why this node was drawn here (its
  "birth"). Absent for roots and parked nodes.
- `(rels "STRING")` — blue; the remaining relationship tokens plus the
  action tokens `Ak` (k aliases) and `Ik` (k extra IDs).

Both strings are assembled by the token grammar in
`server/herald_tokens.rs` (the single source of truth), from
`GraphNodeStats` and `ViewNodeStats` (`server/types/viewnode.rs`).
Each graph relation contributes at most one token of the shape
`[inNum][inLetters] X [outNum][outLetters]`:

- `X` is the relation letter: `C` contains, `L` textlinks_to,
  `H` hides_from_its_subscriptions, `S` subscribes,
  `O` overrides_view_of.
- the numbers are member counts on each side: the INBOUND (left) side
  counts nodes on the far end that point at this node (its containers,
  its overriders, ...); the OUTBOUND (right) side counts those this
  node points to (its contents, the nodes it overrides, ...).
- a lowercase letter flags a tracked ANCESTOR that is a member on that
  side: `a` = visible org-parent, `b` = grandparent, ... (counting all
  viewnodes, col scaffolds included). So an ordinary content child
  reads `aC` ("my parent contains me"); a containerward-ancestry graft
  reads `...Ca` ("I contain my parent", e.g. `1C8a`); a node its parent
  overrides reads `aO`, one that overrides its parent `Oa`. When a
  side's count equals its number of ancestor letters, the count is
  omitted (a lone parent-override is `Oa`, not `1Oa`).
- the `L` token is special: its inbound side is the "surprising links"
  digit form `a(b,c)` and never carries ancestor letters; its outbound
  side is omitted except for the linkSource-birth graft (`La`).

This replaces the retired per-atom scheme. There is no longer a
`(graphStats ...)` sexp on an active node: its counts fold into the
strings above (`overriding`/`subscribing`/`hiding` become the `O`/`S`/
`H` tokens; `aliasing`/`extraIDs` become the `Ak`/`Ik` action tokens),
and the per-relation view-position flags (`containsParent`,
`overridesParent`/`parentOverrides`, `subscribesParent`/
`parentSubscribes`, `hidesParent`/`parentHides`, `grandparentOverrides`/
`grandparentSubscribes`) are gone — those facts are the
ancestor-lettered tokens now. (A phantom, which has graphStats but no
view position, still carries a counts-only `(rels ...)` from
`assemble_counts_only`, and the bare `hiddenBody` atom documented above
still rides an indefinitive node whose graph node has a hidden body.)

`(viewStats ...)` still carries the position facts that are NOT
relationship tokens (the same graph node can warrant different ones
under different parents):

- `cycle` — this position repeats an ancestor's ID;
- `(sourceHerald ⌂:LABEL)` — the node sits at a source boundary (a
  root, or a source differing from its nearest truenode ancestor);
- `(relSource NAME)` — herald red "~NAME", drawn immediately before
  the ⌂ source herald; the recording source of the RELATIONSHIP this
  headline represents (the `contains` edge to a content child, or
  the col's relation for a PartnerCol member), when its privacy was
  deliberately raised above the edge's default. Emitted by render;
  written by `skg-set-relationship-source` (C-c s r); consumed at
  save, where the server enforces the floor (an offered source more
  public than the edge's default is a save error; a source at the
  default or more private is honored, which is how a stuck edge's
  privacy is lowered; a legacy or hand-authored edge whose DISK
  source already sits more public than the default may be held or
  made more private, never made still more public). Absent means the
  edge sits at its default source.
- `(overridesHere N)` — herald red "Oh"; the load-bearing
  substitution marker, documented in the next subsection.

## Override substitution and the overridesHere marker

When view completion would CREATE a viewnode for node N as
recursive content, and a user-owned overrider R of N is visible
under the active source-set, it draws R instead -- transitively (a
user-owned chain D overrides C overrides N is legal and resolves to
the last visible link), cycle-guarded, and never in diff mode.
Existing viewnodes are never rewritten (closing and reopening
normalizes), and only content substitutes: PartnerCol members,
view roots, search results, ancestry insertions and phantoms always
draw the raw node. Moreover, the immediate children of ANY overridden
node drawn raw -- a PartnerCol member, a view root (bypass-opened or
otherwise), or the overridden-as-such (an Affected child of an
overriddenCol) -- also draw raw: the user is looking at the original,
so its children are the original's, not the overrider's. This is one
level deep -- substitution resumes at the grandchildren -- and it is
strict: a node whose only overrider is invisible under the active
source-set is "overridden but drawn raw" too, so its children draw
raw as well.

A substituted viewnode carries the keyed viewStats form
`(overridesHere N)` -- herald red "Oh" -- naming the original it
stands for. The marker is LOAD-BEARING at save: wherever it
appears, extraction collects N rather than the carrying node's own
ID, so a container's contains list round-trips to the original
instead of being rewritten to the overrider. Edits to the drawn
node itself (title, body, cols) still save to the drawn node.
A buffer whose marker the server would not have drawn (the carrier
is not on N's user-owned override chain -- the ownership-gated,
visibility-UNGATED walk out of N) aborts the save with an
explanation. With chains the honest carrier may be a MIDDLE link
(drawn when a later link's source is hidden), so the check accepts
any node on the chain and rejects only off-chain markers. A
source-set switch that makes a
drawn substitute's source inactive turns it into an anonymous
`inactiveNode` (the marker does not survive -- an inactive node is
dataless); the original it stood for is preserved in its container's
contains by the disk merge.

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
| `default_source_set` | no | `all` | Source-set used for a new retained interactive session. Runtime changes belong to that sole session across reconnects and are not written back. |
| `maintenance_archive_folder` | no | `unsaved-work-interrupted-by-rebuild` | Private client recovery-archive root. Relative values resolve beside this config file. The normalized root must be disjoint from every source. |

The handshake returns both the configured archive spelling and the server's
normalized identity.  Each client resolves the configured value beside its
own copy of this same config path, validates that its normalized result agrees,
and refuses unsafe overlap or symlink traversal before maintenance begins.

## Sources

Each `[[sources]]` entry defines a directory of `.skg` files.
**Order is load-bearing**: sources must be listed in privacy order,
most public first, most private last. That order defines the privacy
levels of the telescope model (see `docs/telescopes.md`) and the
available source-sets (below).

| Field          | Required | Description |
|----------------|----------|-------------|
| `path`         | yes      | Directory containing `.skg` files. Relative to data root. |
| `name`         | no       | Unique name for this source (used in metadata, provenance). Defaults to `path`. |
| `abbreviation` | no       | Short label for heralds. Owned sources with defaulted names abbreviate to the path minus the owned folder. |

The source name `all` is reserved and rejected.

**Ownership is by location, not declaration**: a source is owned
(writable) iff its path lies under `<data_root>/<owned_folder>`,
where `owned_folder` is a top-level config field defaulting to
`"owned"`. Everything else is foreign (read-only). The old
`user_owns_it` field is retired and rejected with an error naming
the migration script (`bash/migrate-to-author-folders.sh`).

## Source sets

Source-sets are no longer declared; the retired `[[source_sets]]`
table is rejected with an error. The sets are exactly the
**prefixes of the privacy order**: each source's name denotes the
set consisting of itself and every more-public source, and the
reserved name `all` means every source. Choosing a set is choosing
"the most private source to make available." `default_source_set`
(top-level field) names the set active when a client connects.

Example:

```toml
default_source_set = "public"

# Privacy order: most public first.
[[sources]]
name = "public"
path = "owned/public"   # under owned/ => writable

[[sources]]
name = "private"
path = "owned/private"

[[sources]]
name = "Fred"
path = "Fred"           # foreign => read-only
```

Here the available source-sets are `public` = {public},
`private` = {public, private}, `Fred` = `all` = {public, private,
Fred}.

## DEPENDENCIES.toml

At every server start the server writes a `DEPENDENCIES.toml`
manifest into each OWNED source directory. Its `dependencies` is a
list of pairs, one per source that source may reference — itself and
everything more public, in privacy order, the source itself last.
Each pair has a `path` (data-root-relative directory) and, unless the
source is not a git repo or has no remote, a `git-remote` (the fetch
URL `git remote -v` prints, `origin` preferred), so a receiver knows
where to fetch each dependency; when the chosen remote is not `origin`
its name is also recorded as `git-remote-name`. skg makes no use of
the `git-remote` values itself. For example:

```toml
dependencies = [
  { path = "owned/public", git-remote = "git@github.com:me/public.git" },
  { path = "owned/private", git-remote = "git@github.com:me/private.git" },
]
```

It is generated output — hand edits are clobbered — and lets someone
who receives one of your repos see what it depends on and where to get
it. When a FOREIGN source's manifest orders two sources differently
than this config does, the server warns at init. (The receiver also
still reads the older bare-string `dependencies` shape, so manifests
published before this change keep working.)

## Log files

The server writes to `<data_root>/logs/`:
- `server-to-user.log` — human-readable log (always on).
- `server.jsonl` — structured JSON log (when `timing_log = true`). Queryable with `jq`.

## Example

See `example-data/skgconfig.toml`.

# The .skg file format

It is a specialization of YAML -- every .skg file is valid YAML,
although not vice versa. The definitive source of truth is
`server/types/nodes/fs.rs` (`NodeFS`).

Each file is one **telescope section**: the slice of one node
recorded at one privacy level (see `docs/telescopes.md`). One node =
one ID = same-ID `.skg` files across sources, at most one per
source; the filename is the node's primary ID followed by `.skg` in
every source. The node's **home** is its MOST PUBLIC section, and
that section carries the `title` — an invariant, not a search rule.
A titleless section more public than the title still loads (the
fold reports it) but cannot be rewritten by a save. A field a
section omits is a field that section has no opinion about.
Pre-telescope single-file nodes parse unchanged (one section, title
present, no anchors).

Fields:

- `title`: A string, no newlines. Present in the home section only.
- `pid`: The node's primary ID. Must equal the filename stem. Present
  in every section.
- `extra_ids`: Optional list of additional IDs (from node merges).
  Home section only, by convention.
- `body`: An optional string, perhaps with newlines. Home only.
- `aliases`: Optional list of strings. Each section may contribute
  aliases; an alias's recording source is its section's source.
- `contains`, `subscribes_to`: Ordered relations. ONE flat YAML
  sequence per relation, one entry per line:
  - `- ID` — a member recorded at this section's source. Identical
    syntax in every section, so a membership moving between sources
    diffs as a clean one-line delete/add pair.
  - `- anchor: ID` — a placement anchor: the members after it (until
    the next anchor) insert immediately after the anchored ID, which
    lives in a MORE PUBLIC section. Members before the first anchor
    prepend to the front of the more public list. Anchors are derived
    on save from the in-memory order; hand-written anchors are
    honored on load (a dangling anchor's run falls back with a
    warning, and duplicate anchors concatenate in file order).
- `hides_from_its_subscriptions`, `overrides_view_of`: Unordered
  relations; plain lists of IDs. The effective list is the union
  across sections; each entry's recording source is its section's
  source.

The FOLD of all same-pid sections (most public first) yields the
node; saving UNFOLDS the node back into sections, byte-stably, so
untouched sections do not change on disk. An owned section emptied
by a save is deleted; foreign sections are never touched.

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
`server/types/viewnode.rs` (`TrueNode`, `ViewNodeKind`, `QualCol`,
`Qual`, `PartnerCol`)
and `server/types/git.rs` (`ExistenceAxes`, `MembershipAxes`, `Sign`).

Metadata is not WYSIWYG; its appearance in the client
is determined by `elisp/heralds-minor-mode`, whose rule table is
served by Rust (see "Herald rules" above; the table lives in
`server/heralds.rs`).

PartnerCol scaffolds are represented in Rust as
`ViewNodeKind::PartnerCol (PartnerCol)`, but their external metadata
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
  its parent's collection appeared/disappeared in this stage).
- bare atom `notInGit` if the node's source is not a git repo.

PartnerCol members carry the same axes, in both directions, for
every col: a member removed since HEAD appears as a phantom with
per-stage `removedM`; a member added since HEAD carries per-stage
`newM`; `X` axes describe the member's own file as usual. For an
outbound col (subscribeeCol, overriddenCol, hiddenCol) the signs
come from the owner's relation diff; for an inbound col
(subscriberCol, overriderCol, hiderCol) from the members' files'
diffs (the inverse scan), with phantoms appended after the real
members in sorted-ID order; for the filter cols
(hiddenInSubscribeeCol, hiddenOutsideOfSubscribeeCol) from comparing
the derived membership at HEAD, index and worktree. A col whose
worktree membership is empty but whose HEAD-side membership is not
still renders in diff mode, holding only phantoms. Phantoms define
nothing: saving a buffer ignores them, and a writable col's phantom
is never collected as a member.

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

## Inactive-source nodes: omission, retention, and save preservation

Under a restricted source-set, rendering OMITS nodes from inactive
sources entirely -- content, subscribees, and every PartnerCol's
members alike. No placeholder appears, and omission is recursive: an
omitted container's whole branch is omitted, active descendants
included (they stay reachable by search or direct visit).

Saving preserves what rendering omitted: an omitted relationship
member cannot be deleted (or even seen), so the save merges the
buffer's visible lists with the disk lists -- order-preserving (the
anchored weave) for `contains` and `subscribes_to`, set-difference
for `overrides_view_of`. Edits to visible members, including
deletions, are honored.

The one place an inactive node still appears is RETENTION: when a
source-set switch is applied to an OPEN view (an already-drawn
buffer), an inactive node with active view-children stays on screen
as an anonymous placeholder, so its active descendants are not
orphaned:

```
(skg inactiveNode)
```

The placeholder is DATALESS by design: an inactive node's id, source,
and title all describe content the user hid by restricting the
source-set, so emitting any of it would leak. It carries no id or
source and renders titleless. It is inert: it emits NO save intention
(its membership in its parent's list is owned entirely by the disk
merge, not the buffer), it is not a collateral re-render target, and
editing its (empty) title/body is a buffer validation error. Its
active children remain fully editable. A de-novo render under a
restricted set never produces one -- inactive members are omitted and
their content is never expanded.

Edits to inactive nodes themselves are SUPPRESSED at save, not
fatal: any instruction that would write an inactive source is
dropped and the save succeeds with the warning "Inactive nodes
present in saved buffer remain unchanged in graph." An untouched
stale node saves silently (its identical-to-disk instruction is
discarded as a no-op first).
