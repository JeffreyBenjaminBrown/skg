# Usage: How to run Skg

Install a Rust toolchain, then build and start the server with the path to
your configuration file:

```bash
cargo build --release --bin skg
cargo run --bin skg -- path/to/skgconfig.toml
```

The server builds its graph from the configured `.skg` source folders; no
separate database service is required.

Now you can use the [Emacs client](../elisp/skg-client.el).
