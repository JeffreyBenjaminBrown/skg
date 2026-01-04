#!/usr/bin/env python3
"""Restructure markdown files to have a single root heading as title."""

import os
import re

def filename_to_title(filename):
    """Convert a filename to a human-readable title."""
    name = filename.replace('.md', '')
    name = name.replace('-', ' ')
    # Capitalize words
    words = name.split()
    capitalized = []
    for word in words:
        # Keep acronyms uppercase, capitalize others
        if word.upper() == word and len(word) > 1:
            capitalized.append(word)
        else:
            capitalized.append(word.capitalize())
    name = ' '.join(capitalized)
    # Special cases
    if name == 'Readme':
        name = 'Skg'
    elif name == 'Faq':
        name = 'FAQ'
    return name

def count_root_headings(content):
    """Count the number of level-1 headings."""
    return len(re.findall(r'^# ', content, re.MULTILINE))

def demote_all_headings(content):
    """Add one # to every heading line."""
    return re.sub(r'^(#+ )', r'#\1', content, flags=re.MULTILINE)

def restructure_file(filepath):
    """Restructure a file to have a single root heading if needed."""
    with open(filepath, 'r') as f:
        content = f.read()

    root_count = count_root_headings(content)

    if root_count == 0:
        # No headings - add title at top
        filename = os.path.basename(filepath)
        title = filename_to_title(filename)
        new_content = f'# {title}\n\n{content}'
        with open(filepath, 'w') as f:
            f.write(new_content)
        print(f"Added title to: {filepath}")
        print(f"  Title: {title}")
        return True
    elif root_count == 1:
        # Already single-rooted, check if title is first line
        first_heading = re.search(r'^# (.+)$', content, re.MULTILINE)
        if first_heading and content.strip().startswith('# '):
            print(f"Already OK: {filepath} (title: {first_heading.group(1)})")
            return False
        else:
            # Single heading but not at start - add title
            filename = os.path.basename(filepath)
            title = filename_to_title(filename)
            new_content = demote_all_headings(content)
            new_content = f'# {title}\n\n{new_content}'
            with open(filepath, 'w') as f:
                f.write(new_content)
            print(f"Restructured: {filepath}")
            print(f"  Title: {title}")
            return True
    else:
        # Multiple root headings - demote all and add title
        filename = os.path.basename(filepath)
        title = filename_to_title(filename)
        new_content = demote_all_headings(content)
        new_content = f'# {title}\n\n{new_content}'
        with open(filepath, 'w') as f:
            f.write(new_content)
        print(f"Restructured: {filepath}")
        print(f"  Title: {title}")
        print(f"  Demoted {root_count} root headings to level 2")
        return True

def main():
    base_dir = '/home/ubuntu'

    files = [os.path.join(base_dir, 'README.md')]

    # Add docs/*.md
    docs_dir = os.path.join(base_dir, 'docs')
    for f in os.listdir(docs_dir):
        if f.endswith('.md'):
            files.append(os.path.join(docs_dir, f))

    # Add docs/orphaned/*.md
    orphaned_dir = os.path.join(docs_dir, 'orphaned')
    if os.path.exists(orphaned_dir):
        for f in os.listdir(orphaned_dir):
            if f.endswith('.md'):
                files.append(os.path.join(orphaned_dir, f))

    restructured = 0
    for filepath in files:
        if os.path.exists(filepath):
            if restructure_file(filepath):
                restructured += 1

    print(f"\nDone. Modified {restructured} files.")

if __name__ == '__main__':
    main()
