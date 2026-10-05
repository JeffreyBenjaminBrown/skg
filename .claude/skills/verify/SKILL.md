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

`./bash/start-servers.sh` starts (under cargo-watch, which rebuilds and
restarts it whenever `server/` changes) a skg server on Jeff's REAL data
(`data/skgconfig.toml`, port 1731). Saves and imports through it write
Jeff's `.skg` files. For verification, run a second, isolated instance
on scratch data instead. Instances share nothing but the binary.

## Recipe for an isolated instance

1. Make a scratch dir (the data root) with `skgconfig.toml`:
   ```toml
   tantivy_folder = ".index.tantivy"
   port = 1750                      # any free port; 1731 is Jeff's
   timing_log = false

   [[sources]]
   name = "main"
   path = "owned/main"
   ```
   Paths are relative to the data root, the config file's directory.
   A source is writable iff its path lies under `owned/` (the
   `owned_folder` config field); any other path is read-only. The old
   `user_owns_it` and `db_name` keys are rejected, and there is no
   TypeDB anymore.
   `./target/debug/skg check-config <scratch>/skgconfig.toml` validates
   a config without starting a server.
2. Seed `owned/main/1.skg` (a source folder may also start empty):
   ```yaml
   title: "verify root"
   pid: "1"
   contains: []
   ```
3. Build and start it, recording its PID:
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
   tiny dataset). If cargo-watch is running, it keeps `target/debug/skg`
   current, so `cargo build` is then instant.

## Speaking the protocol

Request: one sexp line + `\n`. Response: one or more
`Content-Length: N\r\n\r\n<payload>` messages (save returns THREE:
save-lock, save-relax-lock, save-result). A working Python client sits
next to this file. Set `SKG_PORT` (default 1750), then:
- `python3 skg_client.py view 1 "content-view:1"`
- `python3 skg_client.py save "content-view:1" body.org`
- `python3 skg_client.py raw '<sexp>'`
- `python3 skg_client.py import /abs/input/dir main` (previews and
  applies a Markdown/Org import in one connection, as approval tokens
  require; a blank host root is sent if one is requested)

The essentials:
- View: `((request . "single root content view") (id . "1") (view-uri . "content-view:1"))`
  → `((response-type content-view) (content "...") (errors ()) (warnings ()))`
- Save: `((request . "save buffer") (view-uri . "content-view:1")
  (point-lines-below-focused-headline . "0") (point-column . "0")
  (point-screen-lines-below-window-start . "0"))` + newline, then the
  buffer as an LP body. A new child is just a bare headline
  (`** some title`) under the metadata-carrying root line; the
  save-result echoes it back with a fresh UUID, and its `.skg` file
  appears in `owned/main/`.

Org export needs no server: `./target/debug/skg export-org
<scratch>/skgconfig.toml <source-set> <output-dir>` (run it from the
scratch dir, as the output dir is relative to the working directory).

Good probes: view a nonexistent id (renders an `(skg (unknown ...))`
view, not an error); send a bogus request type (clean
`(response-type error)`); re-open the view fresh and check the edit
persisted.

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
- Editing `server/` makes cargo-watch rebuild `target/debug/skg` and
  restart Jeff's main server. A scratch instance already running keeps
  its old code; restart it to test new code. Running the Rust test
  suite during such a rebuild has produced spurious failures (e.g.
  "Directory not empty" during test cleanup); rerun before believing one.
