-- Integration test: save a content view whose body contains tabs +
-- newlines, then inspect the on-disk .skg file and verify the body
-- was written as a YAML block literal (|...) rather than a
-- double-quoted one-line string full of \n escapes.
-- The Lua mirror of test-emacs.el in this directory.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(45)

print('=== SKG Save-Body-With-Tabs Integration Test ===')

print('=== PHASE 1: construct content view with tab-bearing body ===')
-- Real tabs and real newlines inside the body lines, exactly as the
-- elisp test's org-text (a literal tab character on the third line).
local org_text = table.concat({
  '* (skg (node (id bwt-root) (source main))) bwt-root',
  'line one',
  '\t(elisp-code)',
  'line three',
}, '\n')

local buffer = require('skg.buffer')
local view_uri = buffer.generate_uuid()
buffer.open_org_buffer_from_text(
  org_text, 'skg://skg-body-with-tabs', view_uri)
vim.api.nvim_win_set_cursor(0, { 1, 0 })

print('=== PHASE 2: save buffer ===')
local ok, err = pcall(require('skg.save').request_save_buffer)
if not ok then
  T.fail(string.format('skg.save.request_save_buffer raised: %s',
                       tostring(err)))
end

T.check(T.wait_for_response(), 'save response arrived')

print('=== PHASE 3: file contents ===')
local skg_file = 'data/skg-data/bwt-root.skg'
T.check(vim.fn.filereadable(skg_file) == 1,
        skg_file .. ' exists on disk')
local disk_contents = table.concat(vim.fn.readfile(skg_file), '\n')
print(disk_contents)

-- Elisp's "^body:" matches at the start of ANY line in the string
-- (Emacs regex ^ matches after each newline); mirror that by scanning
-- lines rather than anchoring to the whole string.
local function any_line_starts_with (text, prefix)
  for _, line in ipairs(vim.split(text, '\n')) do
    if line:sub(1, #prefix) == prefix then return true end
  end
  return false
end

-- Assertion 1: body field must be present.
T.check(any_line_starts_with(disk_contents, 'body:'),
        string.format('body field present in %s', skg_file))

-- Assertion 2: must use block-literal style, not double-quoted.
T.check(any_line_starts_with(disk_contents, 'body: |'),
        'body is a block literal (body: |)')

-- Assertion 3: no literal backslash-n escape sequences anywhere.
T.check(not disk_contents:find('\\n', 1, true),
        'no backslash-n escape sequences in the file')

-- Assertion 4: the tab char from the body must be preserved on disk.
T.check(disk_contents:find('\t', 1, true) ~= nil,
        'tab preserved in block-literal body')

T.pass('PASS: body written as block literal; no \\n escapes; tab'
      .. ' preserved')
