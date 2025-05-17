# What's done, and what's left to do
## What's done
This code so far includes a Rust server and an Emacs client. The server can:

- read and write to files in a format called .skg (it's just .yaml, but not all .yaml is .skg)
- build, modify and consult a model of the data in TypeDB
- build, by consulting TypeDB, a document to send Emacs

The client can:

- Receive that document.
- Let the user edit it.
## What's left to do
Most immediately, the client can't send anything back to Rust. If it could, Rust wouldn't know what to do with it. (It should save the data to disk, update TypeDB and Tantivy, and silently replace the Emacs document with a new one with IDs where the user had created new headings without IDs.) This is sketched in a little more detail in [[TODO.org](./devel-handy/TODO.org)].

For that plan and more in greater detail (plus architectural observations, conceptual problems, and more), see [[my notes in org-roam on Skg](https://github.com/JeffreyBenjaminBrown/public_notes_with_github-navigable_links/blob/main/shareable_knowledge_gardens.org)]
