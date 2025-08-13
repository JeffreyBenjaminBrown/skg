From the root of the project,
some ways to run the Emacs tests include
```
emacs -batch -l tests/emacs/test.el
emacs -batch -l tests/emacs/test.el 2&> tests/results-emacs.txt
```

Similarly, some ways to run the Emacs tests include
```
cargo test
cargo test 2&> tests/results-rust.txt
```

If you use the Docker container,
the Rust tests should be run inside it,
from `/home/ubuntu`, which is the user's home folder.
But the Emacs tests should be run from your host system,
because that is where Emacs is run in production
(and the Docker container does not include Emacs).
