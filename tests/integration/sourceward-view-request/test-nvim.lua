-- Integration test for skg sourceward-view request functionality,
-- nvim client. The Lua mirror of test-emacs.el in this directory.
--
-- Unlike most view tests, the base buffer here is authored locally
-- (not fetched from the server) -- the elisp test does the same,
-- building a scratch org buffer by hand before calling the view
-- request. That exercises skg.view_requests / save purely, against a
-- buffer whose skg_view_uri the test itself assigns.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local buffer = require('skg.buffer')
local metadata = require('skg.metadata')
local view_requests = require('skg.view_requests')

local base_buffer_text =
  '* (skg (node (id 1) (source main))) 1\n'
  .. '** (skg (node (id 11))) 11\n'
  .. '** (skg (node (id 12))) 12\n'

---TEXT with all metadata and body content removed except the id
---headline's stars and title. Port of strip-metadata-and-bodies.
---@param text string
---@return string
local function strip_metadata_and_bodies (text)
  local result = {}
  for _, line in ipairs(vim.split(text, '\n')) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split and split.metadata ~= '' then
      table.insert(result, vim.trim(split.stars) .. ' ' .. split.title)
    end
  end
  if #result == 0 then return '' end
  return table.concat(result, '\n') .. '\n'
end

---Reset the scratch view buffer to base_buffer_text under a fresh
---view uri, position the cursor at LINE_NUMBER (0-based, like the
---elisp), request the sourceward view (auto-saves), and return the
---resulting full buffer text. Port of
---skg-sourceward--request-on-line.
---@param line_number integer
---@return string
local function request_on_line (line_number)
  local uri = buffer.generate_uuid()
  local buf = buffer.open_org_buffer_from_text(
    base_buffer_text, 'skg://sourceward-test-view', uri)
  vim.api.nvim_win_set_cursor(0, { line_number + 1, 0 })
  print(string.format(
    'requesting-sourceward-view-line-%d', line_number))
  view_requests.show_paths_through_link_sources() -- auto-saves
  T.check(T.wait_for_response(),
          string.format('sourceward view response arrived for line %d',
                        line_number))
  return T.buffer_text(buf)
end

---Run the request at LINE_NUMBER and assert the resulting full and
---stripped text. Port of skg-sourceward--verify-view.
---@param line_number integer
---@param expected_full string
---@param expected_stripped string
local function verify_view (line_number, expected_full, expected_stripped)
  local buffer_content = request_on_line(line_number)
  print(string.format('Line %d buffer content: %s',
                      line_number, buffer_content))
  T.check(buffer_content == expected_full,
          string.format(
            'line %d: buffer content exactly matches expected',
            line_number))
  local stripped = strip_metadata_and_bodies(buffer_content)
  T.check(stripped == expected_stripped,
          string.format(
            'line %d: stripped content exactly matches expected',
            line_number))
end

print('=== SKG Sourceward View Request Integration Test ===')

local expected_line0 =
  '* (skg (node (id 1) (source main) (parentIs absent)'
  .. ' (rels "C2"))) 1\n'
  .. '** (skg (node (id 11) (source main) (birthHerald "aC")'
  .. ' (rels "1L"))) 11\n'
  .. '** (skg (node (id 12) (source main) (birthHerald "aC"))) 12\n'
local expected_line2 = expected_line0
local expected_changed =
  '* (skg (node (id 1) (source main) (parentIs absent)'
  .. ' (rels "C2"))) 1\n'
  .. '** (skg (node (id 11) (source main) (birthHerald "aC")'
  .. ' (rels "1L"))) 11\n'
  .. '*** (skg (node (id l-11) (source main) (parentIs independent)'
  .. ' indef (birthHerald "La"))) [[id:11][a link to 11]]\n'
  .. '** (skg (node (id 12) (source main) (birthHerald "aC"))) 12\n'
local expected_no_link = '* 1\n** 11\n** 12\n'
local expected_with_link =
  '* 1\n** 11\n*** [[id:11][a link to 11]]\n** 12\n'

verify_view(0, expected_line0, expected_no_link)
verify_view(1, expected_changed, expected_with_link)
verify_view(2, expected_line2, expected_no_link)

T.pass('PASS: Sourceward view scenarios verified')
