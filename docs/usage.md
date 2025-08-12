# Usage: How to run Skg

There are two alternatives:

## Install nothing. Run it in a tailor-made Docker container.

Find that container at https://github.com/JeffreyBenjaminBrown/docker-typedb-rust and build it.

Start it (using something like the
`docker run` command in `docker.sh`).

Enter it (using something like the
`docker exec` command in `docker.sh`).

Run `cd` to go to the home folder (`/home/ubuntu`).

Then continue at "If you've already got Rust and TypeDB",
below.

## If you've already got Rust and TypeDB

First start the TypeDB server
(by typing just that: `typedb server`).

In a separate shell, run `cargo run`.
This starts the Rust server.

Now you can use the [Emacs client](../elisp/client.el).
