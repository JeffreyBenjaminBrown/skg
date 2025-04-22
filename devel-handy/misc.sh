exit # This is not a unitary script,
     # just some handy commands.

cargo run 2>&1 | tee output.log

cargo test --test index -- --nocapture

# The Emacs find-grep command,
# excluding ./target and ./.git
find . -path ./target -prune -o -path ./.git -prune -o -type f \( -name "*.skg" -o -name "*.rs" \) -exec grep --color=auto -nH --null -i nodes_to_replace_view_of {} +
