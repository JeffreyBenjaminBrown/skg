Two-node containment cycle: a contains b, b contains a.
Each node's title equals its ID. No body, aliases, or other fields.

Used by test-emacs.el to test collateral buffer updates: saving
one buffer (with content removed) should propagate structural
changes to the other buffer via the server's other-views-to-update
response mechanism.

Graph structure:

  a ──contains──▷ b
  b ──contains──▷ a
