find . -type f -name "*.org" -print0 |
  xargs -0 awk '
      BEGIN{total=0; files=0}
      /^\*+ / {hits[FILENAME]++}
      END{
          for (f in hits) { total += hits[f]; files++ }
          if (files) printf "%.2f\n", total / files;
          else print "0"
      }'
