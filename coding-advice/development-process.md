If you're a human I won't impose by asking you to do this,
but if you're an AI, I think it's helpful:

# Test as you go

How to run the three kinds of tests
(Rust unit, Emacs unit, and integration)
is described in [ai-agents.md](../ai-agents.md).
Sometimes the path of least resistance might seem to be
to code a giant mess of changes and then try to compile them all.
That's usually less effective than interleaving small edits
with testing. It's probably most efficient to just
run the relevant unit tests as you go,
but before completing the implementation,
run the entire suite to make sure everything works.

# Run experiments if appropriate

Your coding need not be limited to production and tests.
When you're using a new language feature,
or implementing a tricky algorithm,
or otherwise treading unfamiliar waters,
it is often useful to write a minimal experiment.
This lets you isolate and investigate the question of interest.
