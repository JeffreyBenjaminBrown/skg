find . -type f \
     \( -name "*.el" -o -name "*.rs" \) -print0 \
    | xargs -0 etags
