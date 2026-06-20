find .                                 \
   -type d -name .git -prune -o        \
   -type f -name '*.org'               \
   -exec grep --color=auto -nH --null  \
         -e 9d700d59-a464-4741-b7a1-d952db174456 {} +
