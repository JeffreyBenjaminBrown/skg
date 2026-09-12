# Skg is built on cool technologies.
Skg uses Rust for the server and immutable in-memory graph, Tantivy for its
derived search index, and Emacs Lisp or Lua for its clients.

Rust surely needs no introduction, but I want to point out that it's actually not that hard, now that AI understands it.

The Rust graph uses typed relationship fields and persistent collections, so
readers retain coherent immutable generations while a writer publishes the
next one atomically.

Lisp might be the most venerated language in history. Emacs is written in Lisp, and is basically an interactive Lisp interpreter. It offers the user a degree of control that beggars belief.
