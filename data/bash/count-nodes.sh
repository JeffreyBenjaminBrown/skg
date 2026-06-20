find .                          \
   -type d -name .git -prune -o \
   -type f -name '*.org'        \
   -print0                      \
    | xargs -0 grep -e "^\*\+ " \
    | wc
