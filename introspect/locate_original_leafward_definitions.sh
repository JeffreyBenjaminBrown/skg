# PURPOSE:
# This script generates definitions.org, which lists all public definitions
# (functions, structs, enums, etc.) in the Rust codebase, organized by the
# file where they are originally defined.
#
# This is to enforce the coding standard that imports should use the original,
# longer definition path rather than re-exported paths, making it easier to
# find where things are actually defined.
#
# USAGE:
# 1. Generate definitions.org:
#      bash introspect/locate_original_leafward_definitions.sh > definitions.org
#
# 2. Use definitions.org manually to fix imports:
#    - When you see an import like: use crate::foo::bar;
#    - Search definitions.org for "bar" to find where it's originally defined
#    - If it's defined in crate::foo::baz::qux, change the import to:
#      use crate::foo::baz::qux::bar;
#    - This makes it easier to find the actual definition
#
# 3. For AI agents:
#    - Read definitions.org to build a map of where each public item is defined
#    - Search through the codebase for imports that use re-exported paths
#    - For each import, check if it's using the original definition path
#    - If not, replace with the fully-qualified path from definitions.org
#
# ALGORITHM:
# - Finds all .rs files in server/
# - For each file that contains original definitions (not just re-exports):
#   - Creates an org headline with the file path
#   - Lists all public items (functions, structs, enums, traits, types, consts, statics)
#   - Skips files that only contain module declarations and re-exports

cd "$(dirname "$0")/.." || exit 1

echo "* Definitions by file"
echo ""

find rust -name "*.rs" | sort | while read -r file; do
  # Extract public definitions (not re-exports or module declarations)
  # Looks for: pub fn, pub struct, pub enum, pub trait, pub type, pub const, pub static
  definitions=$(grep -E '^\s*pub\s+(fn|struct|enum|trait|type|const|static)\s+' "$file" | \
    sed -E 's/^\s*pub\s+(fn|struct|enum|trait|type|const|static)\s+([a-zA-Z0-9_]+).*/\2/' | \
    sort -u)

  # Only create a section if there are actual definitions
  if [ -n "$definitions" ]; then
    echo "** $file"
    echo "$definitions"
    echo ""
  fi
done
