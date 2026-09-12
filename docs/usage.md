# Usage: How to run Skg

There are two alternatives:

## Install nothing. Run it in a tailor-made Docker container.

Build the project container described by `bash/docker.sh`.

Start it (using something like the
`docker run` command in `docker.sh`).

Enter it (using something like the
`docker exec` command in `docker.sh`).

Run `cd` to go to the home folder (`/home/ubuntu`).

Then continue at "If you've already got Rust",
below.

## If you've already got Rust

Run `cargo run -- data/skgconfig.toml` (or use `bash/start-servers.sh`).
This starts the Rust server.

Now you can use the [Emacs client](../elisp/skg-client.el).
