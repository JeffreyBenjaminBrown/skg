-- Integration test for skg's search-make-link workflow, nvim client.
-- The Lua mirror of test-emacs.el in this directory.
--
-- With point on a blank line between headlines a and b, upon running
-- search_make_link.search_make_link, buffer goes from
--     * a
--
--     * b
--   to
--     * a
--     [[id:X][LABEL]]
--     * b
--
-- Three fixtures: ~parent~ (the node we open a content view of)
-- contains ~child~; ~target~ is the node we'll search for.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(30)

local content_view = require('skg.content_view')
local buffer = require('skg.buffer')
local search_make_link = require('skg.search_make_link')

---Open a content view of the parent node and return its buffer.
---@return integer
local function open_parent_content_view ()
  print('=== PHASE 1: Opening content view for parent ===')
  content_view.request_single_root_content_view_from_id('parent')
  local buf = T.wait_for_buffer('skg://parent node', 15)
  if not buf then
    T.fail('no content view buffer was created')
  end
  -- DEVIATION: there is no skg-content-view-mode in nvim; the closest
  -- observable equivalent is filetype org + the skg_content_view
  -- marker (both set only by our own view code), plus buftype
  -- acwrite standing in for "does not visit a file" (elisp asserted
  -- buffer-file-name was nil).
  if not (vim.bo[buf].filetype == 'org'
          and vim.b[buf].skg_content_view == true) then
    T.fail(string.format(
      'buffer %s is not a content view buffer',
      vim.api.nvim_buf_get_name(buf)))
  end
  if vim.bo[buf].buftype ~= 'acwrite' then
    T.fail(string.format(
      'buffer %s unexpectedly visits a file',
      vim.api.nvim_buf_get_name(buf)))
  end
  print(string.format(
    'content view buffer %s open and not file-visiting',
    vim.api.nvim_buf_get_name(buf)))
  return buf
end

---In BUF, find the parent's headline, insert a blank line below it,
---park point on that blank line. Returns the 1-based line number of
---the blank line.
---@param buf integer
---@return integer
local function park_point_on_blank_line_between_headlines (buf)
  vim.api.nvim_set_current_buf(buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)

  -- Find the line ending the parent's headline (level-1).
  local parent_line = nil
  for i, line in ipairs(lines) do
    if line:find('^%* ') then parent_line = i break end
  end
  if not parent_line then
    T.fail('no level-1 headline found in source buffer')
  end

  -- Sanity-check the next line is the child's headline.
  local child_line_text = lines[parent_line + 1]
  if not (child_line_text and child_line_text:find('^%*%* ')) then
    T.fail(string.format(
      'expected level-2 child headline after parent, got: %s',
      tostring(child_line_text)))
  end

  -- Insert a fresh blank line between the parent's headline and the
  -- child's headline, then park point on that blank line.
  vim.api.nvim_buf_set_lines(buf, parent_line, parent_line, false, { '' })
  local blank_line = parent_line + 1
  vim.api.nvim_win_set_cursor(0, { blank_line, 0 })
  print(string.format('inserted blank line; point at line %d', blank_line))
  print('  buffer now:\n' .. T.buffer_text(buf))
  return blank_line
end

---@param search_terms string
local function invoke_search_make_link (search_terms)
  print('=== PHASE 2: Invoking search_make_link ===')
  local ok, err = pcall(search_make_link.search_make_link, search_terms)
  if not ok then
    T.fail('search_make_link signalled: ' .. tostring(err))
  end
end

---@param search_terms string
local function pick_target_and_finish (search_terms)
  print('=== PHASE 3: Picking target result ===')
  local search_buf_name = buffer.search_buffer_name(search_terms)
  local search_buf = T.wait_for_buffer(search_buf_name, 15)
  if not search_buf then
    T.fail(string.format('search buffer %q never appeared', search_buf_name))
  end
  vim.api.nvim_set_current_buf(search_buf)
  if vim.b[search_buf].skg_link_target_buf == nil then
    T.fail('search buffer was not configured for link creation')
  end

  local lines = vim.api.nvim_buf_get_lines(search_buf, 0, -1, false)
  local target_line, target_col
  for i, line in ipairs(lines) do
    local start = line:find('(id target)', 1, true)
    if start then target_line, target_col = i, start - 1 break end
  end
  if not target_line then
    T.fail('target result not present in search buffer; got:\n'
          .. T.buffer_text(search_buf))
  end

  vim.api.nvim_win_set_cursor(0, { target_line, target_col })
  print('point on target result; calling finish')
  search_make_link.finish()
end

---Verify that SOURCE_BUF has a [[id:target][...]] link followed by
---its own newline, with the child's `** child node' headline still
---intact on the next line -- i.e. the link did NOT eat the blank
---line's trailing newline.
---@param source_buf integer
local function verify_link_on_its_own_line (source_buf)
  print('=== PHASE 4: Verifying link landed on the blank line ===')
  if not vim.api.nvim_buf_is_valid(source_buf) then
    T.fail('source buffer was killed')
  end
  local text = T.buffer_text(source_buf)
  print('Final source buffer:\n' .. text)

  if not text:find('%[%[id:target%]%[[^%]]+%]%]') then
    T.fail('no [[id:target][LABEL]] link found')
  end
  if text:find('%[%[id:target%]%[[^%]]+%]%]%*') then
    T.fail('link is glued to the next headline (the reported bug):\n'
          .. text)
  end
  if not text:find('%[%[id:target%]%[[^%]]+%]%]\n%*%*') then
    T.fail('link not followed by newline + level-2 headline; got:\n'
          .. text)
  end
  print('link is on its own line; ** child node intact on next line')
end

print('=== SKG search-make-link blank-line Integration Test ===')
local source_buf = open_parent_content_view()
park_point_on_blank_line_between_headlines(source_buf)

invoke_search_make_link('target')
pick_target_and_finish('target')

T.wait_for(function ()
  return vim.api.nvim_buf_is_valid(source_buf)
         and T.buffer_text(source_buf):find('[[id:target]', 1, true) ~= nil
end, 5)

verify_link_on_its_own_line(source_buf)
T.pass('PASS: Integration test successful!')
