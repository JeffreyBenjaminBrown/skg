---
name: verify
description: Drive the running skg server end-to-end (view, edit, save, import, probe) without touching Jeff's real data. Use to verify server-side changes at the real TCP surface.
---

# Verifying skg changes at the TCP surface

The skg server's surface is a sexp-over-TCP protocol (spec:
`docs/api-and-formats.org`). The real clients are Emacs and Neovim, but
the server can be driven directly over the socket. Verified working
2026-10-03.

## Never edit through the main instance

`./bash/start-servers.sh` starts a skg server on Jeff's REAL data
(`data/skgconfig.toml`, port 1731). It runs the release profile
(`target/release/skg`) unless given `--deb` (`target/debug/skg`), and
only with `--hot-reload` does it run under cargo-watch, which rebuilds
and restarts it whenever `server/` changes; `ps` shows which way Jeff's
server is running. Saves and imports through it write Jeff's `.skg`
files. For verification, run a second, isolated instance
on scratch data instead. Instances share nothing but the binary.

## Recipe for an isolated instance

1. Make a scratch dir (the data root) with `skgconfig.toml`:
   ```toml
   tantivy_folder = ".index.tantivy"
   port = 1750                      # any free port; 1731 is Jeff's
   timing_log = false

   [[repos]]
   name = "main"
   path = "me/main"
   ```
   Paths are relative to the data root, the config file's directory.
   A repo is writable unless its entry says `foreign = true`; no folder
   name is special. The owned repos are listed most public first (the
   privacy ladder); foreign repos have no place on it and may be
   listed anywhere. `default_repo_set` must name an owned repo. The old
   `user_owns_it`, `owned_folder` and `db_name` keys are rejected, and
   there is no TypeDB anymore.
   `./target/debug/skg check-config <scratch>/skgconfig.toml` validates
   a config without starting a server.
2. Seed `me/main/1.skg` (a repo folder may also start empty):
   ```yaml
   title: "verify root"
   pid: "1"
   contains: []
   ```
3. Build and start it, recording its PID. This recipe uses the debug
   profile, its own choice (the start script's default is release):
   ```bash
   cargo build --bin skg
   cd <scratch>
   /abs/path/to/target/debug/skg skgconfig.toml > server.log 2>&1 &
   SKG_PID=$!
   ```
   Keep the 'cd' a separate command. In 'cd <scratch> && skg ... &'
   the whole list is backgrounded as a subshell, so '$!' is that
   subshell's PID: killing it leaves the server running on its port.
   Then wait for "Server ready." in `server.log` (a few seconds on a
   tiny dataset). If Jeff's server runs with `--hot-reload --deb`,
   cargo-watch keeps `target/debug/skg` current, so `cargo build` is
   then instant.

## Speaking the protocol

Request: one sexp line + `\n`. Response: one or more
`Content-Length: N\r\n\r\n<payload>` messages (save returns THREE:
save-lock, save-relax-lock, save-result). The client to use is
`tools/skg-for-ai/skg_for_ai.py` (its `README.org` documents it). Give
every command `--port <scratch port>` (or set `SKG_PORT`; the tool's
own default is 1730), from the repo root:
- `python3 tools/skg-for-ai/skg_for_ai.py view 1 --port 1750 --out-dir <scratch>/out`
  writes `view_1.full.org` (the view as the server sent it, metadata
  and all), `view_1.compact.org` (the same lines, metadata replaced by
  a handle) and `view_1.view.json` (view ID, errors, warnings).
  `search TERMS...` does the same for a search.
- `python3 tools/skg-for-ai/skg_for_ai.py raw '<sexp>' --port 1750`
  sends one request line and prints every response.
- `python3 tools/skg-for-ai/skg_for_ai.py raw '<save request sexp>' --saved-buffer body.org --port 1750`
  saves: it sends the file's text as the saved buffer, wrapped in the
  body envelope, and prints every response up to the terminal one.
- `python3 tools/skg-for-ai/skg_for_ai.py import /abs/input/dir main --port 1750`
  previews and applies a Markdown/Org import on one connection, as
  approval tokens require, answering a host-root request with the
  blank one; `--preview-only` cancels instead of applying.

The essentials:
- View: `((request . "single root content view") (id . "1") (view-id . "content-view:1"))`
  → `((response-type content-view) (content "...") (errors ()) (warnings ()))`
- Save: `((request . "save buffer") (view-id . "content-view:1")
  (point-lines-below-focused-headline . "0") (point-column . "0")
  (point-screen-lines-below-window-start . "0"))` + newline, then the
  buffer as an LP body, `((saved-buffer "BUFFER") (other-views ()))`.
  Edit the `.full.org` file the `view` command wrote and pass it as
  `--saved-buffer`. A new child is just a bare headline
  (`** some title`) under the metadata-carrying root line; the
  save-result echoes it back with a fresh UUID, and its `.skg` file
  appears in `me/main/`. A save need not come from the connection
  that opened the view.
- Repos: `((request . "repos"))` lists every skgrepo, its path and
  whether it is owned.

Org export needs no server: `./target/debug/skg export-org
<scratch>/skgconfig.toml <repo-set> <output-dir>` (run it from the
scratch dir, as the output dir is relative to the working directory).

Good probes: view a nonexistent id (an empty `content`, with no
errors; the tool's `view` reports "the graph may lack that ID"); send
a bogus request type (clean `(response-type error)`); re-open the view
fresh and check the edit persisted.

## Cleanup

`kill $SKG_PID`, confirm it is gone (`ps -p $SKG_PID`, and nothing
listening on the scratch port), then delete the scratch dir.

Do not stop a scratch server with `pkill -f` or `kill $(pgrep -f ...)`:
a pattern naming the binary or config also matches the command line of
the shell running it, so the shell kills itself, and a loose pattern can
match Jeff's main server too. Kill the recorded PID, or run the whole
scenario from a script file that records `$!`.

## Gotchas

- Tantivy commits lag saves (see coding-advice/common-gotchas.md);
  direct index reads right after a save race the background writer.
- If Jeff's server runs with `--hot-reload`, editing `server/` makes
  cargo-watch rebuild its binary (`target/release/skg`, or
  `target/debug/skg` with `--deb`) and restart his main server. A
  scratch instance already running keeps its old code; restart it to
  test new code. Running the Rust test suite during such a rebuild has
  produced spurious failures (e.g. "Directory not empty" during test
  cleanup); rerun before believing one.
