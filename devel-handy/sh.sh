exit # This is not a unitary script,
     # just some handy commands.

cargo run 2>&1 | tee output.log

cargo test --test index -- --nocapture

# The Emacs find-grep command,
# over .skg, .rs, .md, .tql, and .org files,
# excluding ./target and ./.git
find . -path ./target -prune -o -path ./.git -prune -o -type f \( -name "*.skg" -o -name "*.md" -o -name "*.tql" -o -name "*.org" -o -name "*.rs" \) -exec grep -C 3 --color=auto -nH --null -i unsubscri {} +

for f in $(cat temp/files-to-print.txt); do
    echo "# " $f
    cat $f
    echo ""
done > temp/code
