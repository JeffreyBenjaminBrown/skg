-- Integration test for skg title-matches functionality, nvim client.
-- The Lua mirror of test-emacs.el in this directory: tests the
-- title-matches search API end-to-end, including link visiting.
--
-- This tests both:
--   skg.id_search.goto_id_near_point (elisp skg-goto)
--   skg.search.request_text_search   (elisp skg-request-title-matches)

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(15)

local search = require('skg.search')
local buffer = require('skg.buffer')
local id_search = require('skg.id_search')

---A content-view (non-search) buffer, other than any in EXCLUDED. The
---Lua mirror of elisp's find-skg-content-buffer, which relies on
---Emacs's most-recently-used buffer-list ordering to prefer the
---newest view; nvim's buffer list is creation-ordered instead, so we
---track and exclude buffers we already know about.
---@param excluded integer[]|nil
---@return integer|nil
local function find_skg_content_buffer (excluded)
  local excluded_set = {}
  for _, buf in ipairs(excluded or {}) do excluded_set[buf] = true end
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.b[buf].skg_content_view == true
       and not excluded_set[buf]
       and not vim.api.nvim_buf_get_name(buf):match('^skg://%?') then
      return buf
    end
  end
  return nil
end

---Search for 'apples' and verify search results.
---@return integer search_buf
local function test_title_search ()
  print("=== PHASE 1: Requesting title matches for 'apples' ===")
  search.request_text_search('apples', false, false, false)
  print('Called request_text_search')

  local search_buffer = T.wait_for_buffer(buffer.search_buffer_name('apples'))
  if not search_buffer then
    T.fail('No skg search buffer was created')
  end

  local content = T.buffer_text(search_buffer)
  print('Search results received')
  print('Content: ' .. content)
  if content:find('(id apples)', 1, true) then
    print('PASS: Found apples node in search results')
  else
    T.fail('Expected pattern: (id apples). Got: ' .. content)
  end
  return search_buffer
end

---Visit the apples node from search results.
---@param search_buffer integer
local function test_visit_link (search_buffer)
  print('=== PHASE 2: Testing node visit ===')

  vim.api.nvim_set_current_buf(search_buffer)
  local lines = vim.api.nvim_buf_get_lines(search_buffer, 0, -1, false)
  local target_line, target_col
  for i, line in ipairs(lines) do
    local start = line:find('(id apples)', 1, true)
    if start then
      target_line, target_col = i, start - 1
      break
    end
  end
  if not target_line then
    T.fail('Could not find apples metadata in search buffer; got:\n'
          .. T.buffer_text(search_buffer))
  end

  print('Found apples metadata, positioning cursor')
  vim.api.nvim_win_set_cursor(0, { target_line, target_col })
  print('Line content: ' .. lines[target_line])

  -- Visit via goto_id_near_point (reads id from metadata).
  print('Calling goto_id_near_point...')
  id_search.goto_id_near_point()

  local content_buffer = T.wait_for(function ()
    return find_skg_content_buffer({ search_buffer })
  end)
  if not content_buffer then
    T.fail('Content view buffer was not created')
  end

  local content = T.buffer_text(content_buffer)
  print('Content view buffer created successfully')
  print('Content: ' .. content)
  if content:find('apples', 1, true) then
    print('PASS: Found apples content in view')
  else
    T.fail('Expected apples content not found. Got: ' .. content)
  end
end

print('=== SKG Title Matches Integration Test ===')
local search_buffer = test_title_search()
test_visit_link(search_buffer)

T.pass('PASS: Integration test successful!')
