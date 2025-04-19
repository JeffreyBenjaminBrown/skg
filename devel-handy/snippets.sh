exit # This is not a unitary script,
     # just some handy commands.

cargo test --test index -- --nocapture

# The Emacs find-grep command,
# excluding ./target and ./.git
find . -path ./target -prune -o -path ./.git -prune -o -type f -exec grep --color=auto -nH --null -i typedb {} +
