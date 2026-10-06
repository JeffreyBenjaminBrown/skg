# Multi-Repo Error Test Fixtures

This directory contains test fixtures for comprehensive multi-repo error validation.

## Repos

### public (owned)
- **pub-1**: title="pub-1", no body, no aliases, no content
- **pub-2**: title="pub-2", no body, no aliases, no content

### private (owned)
- **priv-1**: title="priv-1", no body, no aliases, no content

### ext (foreign)
- **ext-1**: title="ext-1", body="ext-1", alias="ext-1-alias", no content
- **ext-2**: title="ext-2", no body, no aliases, no content
- **ext-3**: title="ext-3", no body, no aliases, no content
- **ext-4**: title="ext-4", no body, no aliases, no content
- **ext-5**: title="ext-5", no body, no aliases, no content
- **ext-6**: title="ext-6", no body, no aliases, no content
- **ext-7**: title="ext-7", body="ext-7-body", no aliases, no content
- **ext-8**: title="ext-8", no body, no aliases, no content
- **ext-9**: title="ext-9", no body, no aliases, no content

## Purpose

These fixtures are used to test various error conditions including:
- Missing repos on root nodes
- Nonexistent repo references
- Modifications to foreign nodes
- Repo conflicts between disk and buffer
- Duplicate editable IDs, and duplicate IDs whose write-protected occurrence carries a stale repo
- Merge operations involving foreign nodes
