find . -type f \( -name "*.rs" -o -name "*.el" \) \
  -not -path "*/target/*" \
  -not -path "*/.git/*" \
  | xargs etags -o TAGS
