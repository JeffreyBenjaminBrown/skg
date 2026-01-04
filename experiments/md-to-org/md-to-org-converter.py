#!/usr/bin/env python3
"""Convert markdown files to org-roam format with proper headers and links."""

import re
import os
import uuid

def generate_uuid():
    """Generate a random UUID in the org-roam format."""
    return str(uuid.uuid4())

def extract_title(content):
    """Extract the title from the first # heading."""
    match = re.search(r'^# (.+)$', content, re.MULTILINE)
    if match:
        return match.group(1)
    return None

def convert_md_to_org(content, file_dir):
    """Convert markdown content to org format."""
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
            heading_text = convert_inline_formatting(heading_text, file_dir)
            result.append('*' * level + ' ' + heading_text)
            continue

        # Convert the rest of the line
        line = convert_inline_formatting(line, file_dir)
        result.append(line)

    # Remove trailing empty lines
    while result and result[-1].strip() == '':
        result.pop()

    return '\n'.join(result) + '\n'

def convert_inline_formatting(text, file_dir):
    """Convert inline markdown formatting to org."""

    def replace_link(match):
        link_text = match.group(1)
        url = match.group(2)

        # External links - keep as is
        if url.startswith('http://') or url.startswith('https://'):
            return f'[[{url}][{link_text}]]'

        # Split anchor if present
        anchor = ''
        if '#' in url:
            url, anchor = url.split('#', 1)
            anchor = '#' + anchor

        # Convert .md to .org only for docs files (not LICENSE.md, etc.)
        if url.endswith('.md'):
            # Check if this looks like a docs file path
            if '/docs/' in url or url.startswith('docs/') or not '/' in url.replace('../', '').replace('./', ''):
                # It's likely a docs file, convert to .org
                if 'LICENSE' not in url:
                    url = url[:-3] + '.org'

        # Make sure relative paths work properly
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

def convert_file(md_path, org_path):
    """Convert a single markdown file to org-roam format."""
    file_dir = os.path.dirname(md_path)

    with open(md_path, 'r') as f:
        content = f.read()

    # Extract title
    title = extract_title(content)
    if not title:
        # Fallback to filename
        title = os.path.basename(md_path).replace('.md', '').replace('-', ' ').title()

    # Generate UUID
    uuid_str = generate_uuid()

    # Convert content
    org_content = convert_md_to_org(content, file_dir)

    # Create header
    header = create_org_roam_header(title, uuid_str)

    # Combine header and content
    final_content = header + org_content

    with open(org_path, 'w') as f:
        f.write(final_content)

    print(f"Converted: {md_path} -> {org_path}")
    print(f"  Title: {title}")
    print(f"  ID: {uuid_str}")

def main():
    base_dir = '/home/ubuntu'

    # Files to convert
    files_to_convert = ['README.md']

    # Add all docs/*.md files
    docs_dir = os.path.join(base_dir, 'docs')
    for f in os.listdir(docs_dir):
        if f.endswith('.md'):
            files_to_convert.append(f'docs/{f}')

    # Add docs/orphaned/*.md files
    orphaned_dir = os.path.join(docs_dir, 'orphaned')
    if os.path.exists(orphaned_dir):
        for f in os.listdir(orphaned_dir):
            if f.endswith('.md'):
                files_to_convert.append(f'docs/orphaned/{f}')

    for rel_path in files_to_convert:
        md_path = os.path.join(base_dir, rel_path)
        org_path = md_path[:-3] + '.org'

        if os.path.exists(md_path):
            convert_file(md_path, org_path)
        else:
            print(f"Warning: {md_path} not found")

    print("\nConversion complete!")

if __name__ == '__main__':
    main()
