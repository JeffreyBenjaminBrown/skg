-- Integration test for skg aliases-view request functionality, nvim
-- client. The Lua mirror of test-emacs.el in this directory.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local sexpr = require('skg.sexpr.parse')
local metadata = require('skg.metadata')

---The kind symbol name from an skg METADATA_TEXT sexp, e.g.
---'(skg (node ...))' -> 'node', '(skg aliasCol)' -> 'aliasCol'.
---Port of skg-aliases--metadata-kind.
---@param metadata_text string
---@return string
local function metadata_kind (metadata_text)
  local sexp = sexpr.read(metadata_text)
  local payload = sexp[2]
  if sexpr.is_list(payload) then
    return sexpr.atom_text(payload[1])
  end
  return sexpr.atom_text(payload)
end

---TEXT with metadata details and body content removed: each headline
---carrying skg metadata is reduced to its stars, the metadata's kind
---symbol, and its title. Port of strip-metadata-details-and-bodies.
---@param text string
---@return string
local function strip_metadata_details_and_bodies (text)
  local result = {}
  for _, line in ipairs(vim.split(text, '\n')) do
    local split = metadata.split_as_stars_metadata_title(line)
    if split and split.metadata ~= '' then
      local stars = vim.trim(split.stars)
      local kind = metadata_kind(split.metadata)
      if split.title == '' then
        table.insert(result, stars .. ' ' .. kind)
      else
        table.insert(result, stars .. ' ' .. kind .. ' ' .. split.title)
      end
    end
  end
  if #result == 0 then return '' end
  return table.concat(result, '\n') .. '\n'
end

---Request an aliases view at LINE_NUMBER (0-based, like the elisp) on
---the 'test-node' content view buffer; return the buffer's full text
---once the response settles. Port of skg-aliases--request-on-line.
---@param line_number integer
---@return string
local function request_on_line (line_number)
  print('Loading node from server...')
  require('skg.content_view')
    .request_single_root_content_view_from_id('test-node')
  local buf = T.wait_for_buffer('skg://Test Node')
  if not buf then T.fail('Content buffer not created') end

  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { line_number + 1, 0 })
  print(string.format(
    'requesting-aliases-view-line-%d', line_number))
  require('skg.view_requests').show_collection_aliases() -- auto-saves
  T.check(T.wait_for_response(),
          string.format('aliases view response arrived for line %d',
                        line_number))
  return T.buffer_text(buf)
end

---Run the request at LINE_NUMBER and assert the resulting full and
---stripped text. Port of skg-aliases--verify-view.
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
  local stripped = strip_metadata_details_and_bodies(buffer_content)
  T.check(stripped == expected_stripped,
          string.format(
            'line %d: stripped content exactly matches expected',
            line_number))
end

print('=== SKG Aliases View Request Integration Test ===')

local expected_with_aliases =
  '* (skg (node (id test-node) (source main) (parentIs absent)'
  .. ' (rels "A2"))) Test Node\n'
  .. '** (skg aliasCol)\n'
  .. '*** (skg alias) first alias\n'
  .. '*** (skg alias) second alias\n'
local expected_stripped =
  '* node Test Node\n** aliasCol\n*** alias first alias\n'
  .. '*** alias second alias\n'

verify_view(0, expected_with_aliases, expected_stripped)

T.pass('PASS: Aliases view verified')
