#!/usr/bin/env python3
"""Restructure org files to have a single root heading."""

import os
import re

def filename_to_title(filename):
    """Convert a filename to a human-readable title."""
    # Remove .org extension
    name = filename.replace('.org', '')
    # Replace hyphens with spaces
    name = name.replace('-', ' ')
    # Capitalize words
    name = name.title()
    # Special cases
    if name == 'Readme':
        name = 'Skg'
    elif name == 'Faq':
        name = 'FAQ'
    return name

def count_root_headings(content):
    """Count the number of level-1 headings."""
    return len(re.findall(r'^(\* )', content, re.MULTILINE))

def demote_all_headings(content):
    """Add one * to every heading line."""
    return re.sub(r'^(\*+ )', r'*\1', content, flags=re.MULTILINE)

def restructure_file(filepath):
    """Restructure a file to have a single root heading if needed."""
    with open(filepath, 'r') as f:
        content = f.read()

    root_count = count_root_headings(content)

    if root_count <= 1:
        # Already single-rooted or no headings
        return False

    # Get title from filename
    filename = os.path.basename(filepath)
    title = filename_to_title(filename)

    # Demote all headings
    new_content = demote_all_headings(content)

    # Add root heading
    new_content = f'* {title}\n{new_content}'

    with open(filepath, 'w') as f:
        f.write(new_content)

    print(f"Restructured: {filepath}")
    print(f"  Title: {title}")
    print(f"  Demoted {root_count} root headings to level 2")
    return True

def main():
    base_dir = '/home/ubuntu'

    files = [os.path.join(base_dir, 'README.org')]

    # Add docs/*.org
    docs_dir = os.path.join(base_dir, 'docs')
    for f in os.listdir(docs_dir):
        if f.endswith('.org'):
            files.append(os.path.join(docs_dir, f))

    # Add docs/orphaned/*.org
    orphaned_dir = os.path.join(docs_dir, 'orphaned')
    if os.path.exists(orphaned_dir):
        for f in os.listdir(orphaned_dir):
            if f.endswith('.org'):
                files.append(os.path.join(orphaned_dir, f))

    restructured = 0
    for filepath in files:
        if restructure_file(filepath):
            restructured += 1

    print(f"\nDone. Restructured {restructured} files.")

if __name__ == '__main__':
    main()
