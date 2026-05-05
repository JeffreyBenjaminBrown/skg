#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
output_path="$repo_root/tools/compare-org-roam-import/output.org"

pairs=(
  "deciduous:deciduous"
  "mincit:mincit"
  "ofiscal:ofiscal"
  "personal-deprecated:personal"
  "personal-ish:personal-ish"
  "personal-most:personal-most"
  "personal-proc:personal-proc"
  "public:public"
)

{
  printf "* quick org-roam import counts\n\n"
  printf "| data / org | skg | org files | bullets | org+bullets | delta |\n"
  printf "|-\n"
} > "$output_path"

for pair in "${pairs[@]}"; do
  data_name="${pair%%:*}"
  org_name="${pair##*:}"
  data_repo="$repo_root/data/$data_name"
  org_repo="$repo_root/org-roam/$org_name"
  data_first_commit="$(
    git -C "$data_repo" rev-list --max-parents=0 HEAD | tail -n 1
  )"
  skg_count="$(
    git -C "$data_repo" ls-tree -r --name-only "$data_first_commit" \
      | grep -Ec '\.skg$' || true
  )"
  org_file_count="$(
    git -C "$org_repo" ls-tree -r --name-only HEAD \
      | grep -Ec '\.org$' || true
  )"
  bullet_count="$(
    git -C "$org_repo" grep -Eh '^\*+ ' HEAD -- '*.org' \
      | wc -l
  )"
  org_node_estimate=$((org_file_count + bullet_count))
  delta=$((skg_count - org_node_estimate))
  printf "| %s | %d | %d | %d | %d | %d |\n" \
    "$data_name / $org_name" \
    "$skg_count" \
    "$org_file_count" \
    "$bullet_count" \
    "$org_node_estimate" \
    "$delta" >> "$output_path"
done

printf "wrote %s\n" "$output_path"
