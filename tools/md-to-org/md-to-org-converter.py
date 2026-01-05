#!/usr/bin/env python3
"""Convert markdown files to org-roam format with ID-based links.

Two-pass conversion:
1. First pass: Generate UUIDs for all files and build a mapping
2. Second pass: Convert files using the UUID mapping for links
"""

import re
import os
import uuid
import json

# Database file to store the ID mappings
ID_DB_FILE = '/home/ubuntu/tools/md-to-org/.org-id-db.json'

def generate_uuid():
    """Generate a random UUID in the org-roam format."""
    return str(uuid.uuid4())

def extract_title(content):
    """Extract the title from the first # heading."""
    match = re.search(r'^# (.+)$', content, re.MULTILINE)
    if match:
        return match.group(1)
    return None

def get_all_md_files(base_dir):
    """Get all markdown files to convert."""
    files = []

    # README.md
    readme = os.path.join(base_dir, 'README.md')
    if os.path.exists(readme):
        files.append(readme)

    # docs/*.md
    docs_dir = os.path.join(base_dir, 'docs')
    if os.path.exists(docs_dir):
        for f in os.listdir(docs_dir):
            if f.endswith('.md'):
                files.append(os.path.join(docs_dir, f))

    # docs/orphaned/*.md
    orphaned_dir = os.path.join(docs_dir, 'orphaned')
    if os.path.exists(orphaned_dir):
        for f in os.listdir(orphaned_dir):
            if f.endswith('.md'):
                files.append(os.path.join(orphaned_dir, f))

    return files

def build_id_database(base_dir):
    """First pass: Generate UUIDs for all files and build mapping.

    Reuses existing UUIDs from the database if available, only generating
    new UUIDs for files that don't have one yet. This makes the conversion
    idempotent.
    """
    files = get_all_md_files(base_dir)

    # Load existing database if it exists
    existing_db = {}
    if os.path.exists(ID_DB_FILE):
        try:
            with open(ID_DB_FILE, 'r') as f:
                existing_db = json.load(f)
            print(f"Loaded existing ID database with {len([k for k in existing_db if not k.startswith('__')])} entries")
        except (json.JSONDecodeError, IOError) as e:
            print(f"Warning: Could not load existing database: {e}")
            existing_db = {}

    # Start with existing database
    id_db = existing_db.copy()
    new_count = 0
    reused_count = 0

    for md_path in files:
        # Get the relative path and convert to .org
        rel_path = os.path.relpath(md_path, base_dir)
        org_rel_path = rel_path[:-3] + '.org'
        md_rel_path = rel_path

        # Check if UUID already exists for this path
        if org_rel_path in id_db and isinstance(id_db[org_rel_path], dict):
            file_uuid = id_db[org_rel_path]['uuid']
            reused_count += 1
        else:
            # Generate new UUID
            file_uuid = generate_uuid()
            new_count += 1

        # Update/create entry
        id_db[org_rel_path] = {
            'uuid': file_uuid,
            'md_path': md_rel_path,
        }

        # Also index by just filename for simpler lookups
        org_filename = os.path.basename(org_rel_path)
        md_filename = os.path.basename(md_rel_path)
        id_db[f'__filename__{org_filename}'] = file_uuid
        id_db[f'__filename__{md_filename}'] = file_uuid

    # Save database
    with open(ID_DB_FILE, 'w') as f:
        json.dump(id_db, f, indent=2)

    print(f"ID database: {reused_count} reused, {new_count} new, {len(files)} total files")
    return id_db

def load_id_database():
    """Load the ID database from disk."""
    with open(ID_DB_FILE, 'r') as f:
        return json.load(f)

def resolve_link_to_uuid(url, current_file_dir, base_dir, id_db):
    """Resolve a link URL to a UUID if possible."""
    # Handle relative paths
    if url.startswith('./'):
        url = url[2:]

    # Convert .md to .org for lookup
    if url.endswith('.md'):
        url = url[:-3] + '.org'

    # Try to resolve the path
    if url.startswith('../'):
        # Relative to parent
        full_path = os.path.normpath(os.path.join(current_file_dir, url))
        rel_path = os.path.relpath(full_path, base_dir)
    else:
        # Relative to current dir or absolute
        if os.path.isabs(url):
            rel_path = os.path.relpath(url, base_dir)
        else:
            full_path = os.path.normpath(os.path.join(current_file_dir, url))
            rel_path = os.path.relpath(full_path, base_dir)

    # Look up in database
    if rel_path in id_db:
        return id_db[rel_path]['uuid']

    # Try by filename
    filename = os.path.basename(rel_path)
    filename_key = f'__filename__{filename}'
    if filename_key in id_db:
        return id_db[filename_key]

    return None

def convert_md_to_org(content, current_file_dir, base_dir, id_db):
    """Convert markdown content to org format with ID-based links."""
    lines = content.split('\n')
    result = []
    in_code_block = False
    code_lang = ""

    for line in lines:
        # Handle code blocks
        if line.strip().startswith('```'):
            if not in_code_block:
                in_code_block = True
                code_lang = line.strip()[3:].strip()
                if code_lang:
                    result.append(f'#+begin_src {code_lang}')
                else:
                    result.append('#+begin_example')
            else:
                in_code_block = False
                if code_lang:
                    result.append('#+end_src')
                else:
                    result.append('#+end_example')
                code_lang = ""
            continue

        if in_code_block:
            result.append(line)
            continue

        # Convert headings
        heading_match = re.match(r'^(#{1,6})\s+(.*)$', line)
        if heading_match:
            level = len(heading_match.group(1))
            heading_text = heading_match.group(2)
            heading_text = convert_inline_formatting(heading_text, current_file_dir, base_dir, id_db)
            result.append('*' * level + ' ' + heading_text)
            continue

        # Convert the rest of the line
        line = convert_inline_formatting(line, current_file_dir, base_dir, id_db)
        result.append(line)

    # Remove trailing empty lines
    while result and result[-1].strip() == '':
        result.pop()

    return '\n'.join(result) + '\n'

def convert_inline_formatting(text, current_file_dir, base_dir, id_db):
    """Convert inline markdown formatting to org with ID-based links."""

    def replace_link(match):
        link_text = match.group(1)
        url = match.group(2)

        # External links - keep as is
        if url.startswith('http://') or url.startswith('https://'):
            return f'[[{url}][{link_text}]]'

        # Strip anchor for now (we'll handle these separately if needed)
        anchor = ''
        if '#' in url:
            url, anchor = url.split('#', 1)
            anchor = '#' + anchor

        # Non-.md/.org files - keep as file links
        if not url.endswith('.md') and not url.endswith('.org'):
            # Make relative path work
            if not url.startswith('./') and not url.startswith('../') and not url.startswith('/'):
                url = './' + url
            return f'[[{url}][{link_text}]]'

        # Try to resolve to UUID
        file_uuid = resolve_link_to_uuid(url, current_file_dir, base_dir, id_db)

        if file_uuid:
            # Use ID-based link
            return f'[[id:{file_uuid}][{link_text}]]'
        else:
            # Fallback to file-based link
            if url.endswith('.md'):
                url = url[:-3] + '.org'
            if not url.startswith('./') and not url.startswith('../') and not url.startswith('/'):
                url = './' + url
            return f'[[{url}{anchor}][{link_text}]]'

    text = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', replace_link, text)

    # Convert bold **text** to *text*
    text = re.sub(r'\*\*([^*]+)\*\*', r'*\1*', text)

    # Convert inline code `code` to =code=
    text = re.sub(r'`([^`]+)`', r'=\1=', text)

    return text

def create_org_roam_header(title, uuid_str):
    """Create the org-roam header with PROPERTIES and title."""
    return f""":PROPERTIES:
:ID:       {uuid_str}
:END:
#+title: {title}
"""

def convert_file(md_path, org_path, base_dir, id_db):
    """Second pass: Convert a single markdown file to org-roam format."""
    current_file_dir = os.path.dirname(md_path)

    with open(md_path, 'r') as f:
        content = f.read()

    # Extract title
    title = extract_title(content)
    if not title:
        title = os.path.basename(md_path).replace('.md', '').replace('-', ' ').title()

    # Get UUID from database
    rel_org_path = os.path.relpath(org_path, base_dir)
    file_uuid = id_db[rel_org_path]['uuid']

    # Convert content
    org_content = convert_md_to_org(content, current_file_dir, base_dir, id_db)

    # Create header
    header = create_org_roam_header(title, file_uuid)

    # Combine header and content
    final_content = header + org_content

    with open(org_path, 'w') as f:
        f.write(final_content)

    print(f"Converted: {md_path}")
    print(f"  Title: {title}")
    print(f"  ID: {file_uuid}")

def main():
    base_dir = '/home/ubuntu'

    print("=== Pass 1: Building ID database ===")
    id_db = build_id_database(base_dir)

    print("\n=== Pass 2: Converting files ===")
    files = get_all_md_files(base_dir)

    for md_path in files:
        org_path = md_path[:-3] + '.org'
        convert_file(md_path, org_path, base_dir, id_db)

    print("\n=== Conversion complete! ===")
    print(f"ID database saved to: {ID_DB_FILE}")

if __name__ == '__main__':
    main()
