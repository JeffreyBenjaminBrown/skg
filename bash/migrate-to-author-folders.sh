#!/usr/bin/env bash
# Migrate a config (and its data tree) to the author-folder layout:
# every OWNED source's directory moves under the owned folder
# (default "owned"), its 'path' in the config is updated, and the
# retired 'user_owns_it' lines are deleted. Foreign sources are left
# where they are (anything not under the owned folder is foreign).
#
# USAGE:
#   bash/migrate-to-author-folders.sh path/to/skgconfig.toml [owned_folder]
#
# Idempotent: a config with no 'user_owns_it' lines is left untouched.
# See TODO/user-owned_autofork_chain/5_plan.org, work item
# privacy-order.

set -euo pipefail
CONFIG="${1:?usage: migrate-to-author-folders.sh CONFIG [OWNED_FOLDER]}"
OWNED_FOLDER="${2:-owned}"

python3 - "$CONFIG" "$OWNED_FOLDER" <<'EOF'
import re, sys, pathlib, shutil

config_path = pathlib.Path(sys.argv[1])
owned_folder = sys.argv[2]
data_root = config_path.parent
text = config_path.read_text()

if "user_owns_it" not in text:
    print(f"{config_path}: already migrated (no user_owns_it); nothing to do")
    sys.exit(0)

blocks = re.split(r'(?=^\[\[sources\]\]$)', text, flags=re.M)
out = [blocks[0]]
for block in blocks[1:]:
    m_path = re.search(r'^path[ \t]*=[ \t]*"([^"]+)"', block, flags=re.M)
    m_owns = re.search(r'^user_owns_it[ \t]*=[ \t]*(true|false)', block, flags=re.M)
    owned = bool(m_owns and m_owns.group(1) == "true")
    if owned and m_path:
        old_rel = m_path.group(1)
        if not old_rel.startswith(owned_folder + "/") and not old_rel.startswith("/"):
            new_rel = f"{owned_folder}/{old_rel}"
            old_dir = data_root / old_rel
            new_dir = data_root / new_rel
            if old_dir.exists():
                if str(new_dir).startswith(str(old_dir) + "/"):
                    # e.g. a source whose path IS the owned folder's
                    # name: "owned" -> "owned/owned" needs a hop.
                    tmp = data_root / (old_rel + ".migrating")
                    shutil.move(str(old_dir), str(tmp))
                    new_dir.parent.mkdir(parents=True, exist_ok=True)
                    shutil.move(str(tmp), str(new_dir))
                else:
                    new_dir.parent.mkdir(parents=True, exist_ok=True)
                    shutil.move(str(old_dir), str(new_dir))
                print(f"moved {old_dir} -> {new_dir}")
            block = block.replace(m_path.group(0), f'path = "{new_rel}"')
    block = re.sub(r'^user_owns_it[ \t]*=[ \t]*(true|false)[ \t]*\n', '',
                   block, flags=re.M)
    out.append(block)
config_path.write_text("".join(out))
print(f"rewrote {config_path}")
EOF
