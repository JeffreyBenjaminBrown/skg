From the root of the project,
some ways to run the Emacs tests include
```
emacs -batch -l tests/tests.el
emacs -batch -l tests/tests.el 2&> test-results-emacs.txt
```

and the Rust tests can be run via
```
cargo test
cargo test 2&> test-results-rust.txt
```

If you use the Docker container,
the Rust tests should be run from within that
(from `/home/ubuntu`, which is the user's home folder)
but the Emacs tests should be run from your host system,
because the Docker container does not have Emacs,
and even if it did it wouldn't have your configuration,
the functionality of which you'll want to test.
