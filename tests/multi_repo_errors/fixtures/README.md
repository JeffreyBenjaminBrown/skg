# Multi-Source Error Test Fixtures

This directory contains test fixtures for comprehensive multi-source error validation.

## Sources

### public (user_owns_it = true)
- **pub-1**: title="pub-1", no body, no aliases, no content
- **pub-2**: title="pub-2", no body, no aliases, no content

### private (user_owns_it = true)
- **priv-1**: title="priv-1", no body, no aliases, no content

### ext (user_owns_it = false, read-only)
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
- Missing sources on root nodes
- Nonexistent source references
- Modifications to foreign (read-only) nodes
- Source conflicts between disk and buffer
- Inconsistent sources for duplicate IDs
- Merge operations involving foreign nodes
