-- Integration test for skg.id_search link traversal, nvim client. The
-- Lua mirror of test-emacs.el in this directory.
-- Tests following links: title-search -> src -> dest.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(15)

local search = require('skg.search')
local buffer = require('skg.buffer')
local id_search = require('skg.id_search')

---A content-view (non-search) buffer, other than any in EXCLUDED. The
---Lua mirror of elisp's find-skg-content-buffer, which relies on
---Emacs's most-recently-used buffer-list ordering to prefer the
---newest view; nvim's buffer list is creation-ordered instead, so we
---track and exclude buffers we already know about to find the newest
---one.
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

---Search for 'has' and wait for results.
---@return integer search_buf
local function phase_0_search ()
  print("=== PHASE 0: Searching for 'has' ===")
  search.request_text_search('has', false, false, false)
  print('Sent text-search request')

  local search_buf = T.wait_for_buffer(buffer.search_buffer_name('has'), 5)
  if not (search_buf
          and T.buffer_text(search_buf):find('(id src)', 1, true)) then
    T.fail('Timeout waiting for search results')
  end
  return search_buf
end

---From search results, visit src.
---@param search_buffer integer
---@return integer src_view
local function phase_1_visit_src (search_buffer)
  print('=== PHASE 1: Visiting src from search results ===')

  local content = T.buffer_text(search_buffer)
  print('Search results: ' .. content)
  if not content:find('(id src)', 1, true) then
    T.fail('Expected src node in search results, got: ' .. content)
  end
  print('Found src in search results')

  -- Position cursor on the src metadata so goto_id_near_point can
  -- extract the id.
  vim.api.nvim_set_current_buf(search_buffer)
  local lines = vim.api.nvim_buf_get_lines(search_buffer, 0, -1, false)
  local target_line, target_col
  for i, line in ipairs(lines) do
    local start = line:find('(id src)', 1, true)
    if start then target_line, target_col = i, start - 1 break end
  end
  if not target_line then
    T.fail('Could not find (id src) in search results')
  end
  vim.api.nvim_win_set_cursor(0, { target_line, target_col })
  print('Line: ' .. lines[target_line])
  print('Positioned on src metadata, calling goto_id_near_point...')
  id_search.goto_id_near_point()

  -- Wait for content view buffer to show src's content (src's title
  -- contains a [[id:dest][...]] link).
  local view = T.wait_for(function ()
    local buf = find_skg_content_buffer({ search_buffer })
    if buf and T.buffer_text(buf):find('[[id:dest]', 1, true) then
      return buf
    end
    return nil
  end, 5)
  if not view then
    T.fail("Timeout waiting for src content view")
  end
  return view
end

---From src's content view, follow the link to dest.
---@param content_buffer integer
---@param excluded integer[] buffers already known (search buf, src view)
---@return integer dest_view
local function phase_2_visit_dest (content_buffer, excluded)
  print('=== PHASE 2: Following link from src to dest ===')

  local content = T.buffer_text(content_buffer)
  print('Src content view: ' .. content)
  if not content:find('[[id:dest]', 1, true) then
    T.fail('Expected src with link to dest, got: ' .. content)
  end
  print('At src content view with link to dest')

  -- Find and position on the dest link (a real org link in the
  -- title, so visit_link works here).
  vim.api.nvim_set_current_buf(content_buffer)
  local lines = vim.api.nvim_buf_get_lines(content_buffer, 0, -1, false)
  local link_line, center_col
  for i, line in ipairs(lines) do
    if line:find('[[id:dest]', 1, true) then
      -- goto-link-center: the first '][' on the line.
      local center = line:find('][', 1, true)
      if center then
        link_line, center_col = i, center - 1
        break
      end
    end
  end
  if not link_line then
    T.fail('Could not find [[id:dest] link in src, or no ][ in it')
  end
  vim.api.nvim_win_set_cursor(0, { link_line, center_col })
  print('Positioned on link, calling visit_link...')
  id_search.visit_link()

  -- Wait for content view buffer to show dest's content.
  local view = T.wait_for(function ()
    local buf = find_skg_content_buffer(excluded)
    if buf and T.buffer_text(buf):find(
        'Dest need not be aware', 1, true) then
      return buf
    end
    return nil
  end, 5)
  if not view then
    T.fail('Timeout waiting for dest content view')
  end
  return view
end

---Verify we arrived at dest's content view.
---@param content_buffer integer
local function phase_3_verify_dest (content_buffer)
  print('=== PHASE 3: Verifying dest content view ===')
  local content = T.buffer_text(content_buffer)
  print('Content view content: ' .. content)
  if content:find('Dest need not be aware', 1, true) then
    print('Arrived at dest content view!')
    print('Link traversal test complete!')
  else
    T.fail('Expected dest content, got: ' .. content)
  end
end

print('=== SKG Visit-Link Traversal Integration Test ===')
local search_buf = phase_0_search()
local src_view = phase_1_visit_src(search_buf)
local dest_view = phase_2_visit_dest(src_view, { search_buf, src_view })
phase_3_verify_dest(dest_view)

T.pass('PASS: Integration test successful!')
