# the point

Your notes are trees with hyperlinks, like org-mode. Everyone's are. Surf everyone's notes, rearranging them (really your view of them) however you like.

# data model

See [[the schema](schema.tql)], [[the content model](docs/content-model.md)] and [[the sharing model](docs/sharing-model.md)].

# I'm not done yet.

This really doesn't let you do much yet.

# usage

alternatives

## Install nothing! Run it in a tailor-made Docker container.

Find that container at https://github.com/JeffreyBenjaminBrown/docker-typedb-rust and build it.

Start it (using something like the
`docker run` command in `docker.sh`).

Enter it (using something like the
`docker exec` command in `docker.sh`).

Run `cd` to go to the home folder (`/home/ubuntu`).

Then go to "If you've already got Rust and TypeDB" above.

## Without Docker, if you've already got Rust and TypeDB

First start the TypeDB server
(by typing just that: `typedb server`).

In a separate shell, run `cargo run`.
This starts the Rust server (the bulk of this code).

Now visit [this Emacs lisp program](elisp/client.el)
and read the top comment for further instructions.
