-- Integration test for the MOTIVATING override-chain case, nvim
-- client. The Lua mirror of test-emacs.el in this directory: fork a
-- foreign N into a public clone C (implicit fork), then run
-- skg.view_requests.fork_node on the DRAWN SUBSTITUTE C and rotate
-- its clone to a PRIVATE source, forming the chain D overrides C
-- overrides N. Viewing N's container then draws the chain end D,
-- marked (overridesHere N), and the save accepts that chain-end
-- carrier.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(90)

local content_view = require('skg.content_view')
local metadata = require('skg.metadata')
local save = require('skg.save')
local view_requests = require('skg.view_requests')

---Return a live skg view buffer whose text mentions (id ID).
local function buffer_showing (id)
  local needle = '(id ' .. id .. ')'
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_view_uri ~= nil
       and T.buffer_text(buf):find(needle, 1, true) then
      return buf
    end
  end
  return nil
end

---Move the cursor in the CURRENT buffer onto the first line containing
---SNIPPET anywhere. Fails the test if no line does.
local function goto_line_containing (snippet, description)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for i, line in ipairs(lines) do
    if line:find(snippet, 1, true) then
      vim.api.nvim_win_set_cursor(0, { i, 0 })
      return i
    end
  end
  T.fail(description or ('could not find a line containing: ' .. snippet))
end

---Move the cursor in the CURRENT buffer onto the first line that
---STARTS WITH SNIPPET (the analog of an elisp "^..."-anchored search).
---Fails the test if no line does.
local function goto_line_starting_with (snippet, description)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  for i, line in ipairs(lines) do
    if line:sub(1, #snippet) == snippet then
      vim.api.nvim_win_set_cursor(0, { i, 0 })
      return i
    end
  end
  T.fail(description or ('could not find a line starting with: ' .. snippet))
end

---Kill any open P buffer and request a fresh single-root view of P;
---wait for and return the new P buffer.
local function reopen_P ()
  local existing = buffer_showing('P')
  if existing then vim.api.nvim_buf_delete(existing, { force = true }) end
  content_view.request_single_root_content_view_from_id('P')
  local p_buf = T.wait_for(function () return buffer_showing('P') end, 10)
  T.check(p_buf, "P's view never appeared")
  return p_buf
end

print('Starting override-chain integration test...')

-- 1. Open P (public); its content is the foreign N.
local p_buf = reopen_P()
vim.api.nvim_set_current_buf(p_buf)
T.check(T.buffer_text(p_buf):find('(id N)', 1, true),
        "P's view does not show foreign N")

-- 2. Implicit fork: make N definitive and edit its title.
local n_line = goto_line_containing('(id N) (source foreign)',
  "could not find N's headline")
local n_line_text =
  vim.api.nvim_buf_get_lines(p_buf, n_line - 1, n_line, false)[1]
local n_line_edited =
  n_line_text:gsub(' indef', ''):gsub('N%-original', 'N-edited')
vim.api.nvim_buf_set_lines(p_buf, n_line - 1, n_line, false,
                           { n_line_edited })
save.request_save_buffer()

-- 3. Approve the foreign fork -> public clone C overrides N.
local confirm_buf_1 = T.wait_for_buffer('skg://fork-confirmation', 10)
T.check(confirm_buf_1, 'no fork-confirmation for N appeared')
vim.api.nvim_set_current_buf(confirm_buf_1)
local clone_line_1 = goto_line_starting_with('* (skg (node (source ',
  'could not find the clone-to-be headline')
metadata.change_source_at_line(clone_line_1, 'public')
save.approve_fork()
print('foreign N forked into a public clone')

-- 4. Reopen P: the public clone C is drawn in N's place.
p_buf = reopen_P()
local drawn = T.wait_for(function ()
  return T.buffer_text(p_buf):find('(overridesHere N)', 1, true)
end, 10)
T.check(drawn, "the public clone was not drawn in N's place")

-- 5. Explicitly fork the DRAWN SUBSTITUTE C.
vim.api.nvim_set_current_buf(p_buf)
goto_line_containing('(overridesHere N)',
  'could not find the drawn substitute')
view_requests.fork_node()

-- 6. In the confirmation buffer, rotate the clone-to-be's source to
--    PRIVATE, then approve -> private D overrides C.
local confirm_buf_2 = T.wait_for_buffer('skg://fork-confirmation', 10)
T.check(confirm_buf_2, 'no fork-confirmation for the clone appeared')
vim.api.nvim_set_current_buf(confirm_buf_2)
local clone_line_2 = goto_line_starting_with('* (skg (node (source ',
  'could not find the clone-to-be headline')
metadata.change_source_at_line(clone_line_2, 'private')
save.approve_fork()
print('the public clone was forked into a private clone (source rotated)')

-- 7. Reopen P (all sources active): the chain end D -- a PRIVATE-source
--    node -- is now drawn in N's place, still marked (overridesHere N).
p_buf = reopen_P()
local function chain_end_drawn ()
  for _, line in ipairs(vim.api.nvim_buf_get_lines(p_buf, 0, -1, false)) do
    if line:find('(overridesHere N)', 1, true)
       and line:find('(source private)', 1, true) then
      return true
    end
  end
  return false
end
local end_drawn = T.wait_for(chain_end_drawn, 10)
T.check(end_drawn, 'the chain end D (a private node) was not drawn for N')
print("the chain end D (private) is drawn in N's place (overridesHere N)")

-- 8. Saving the chain-end view succeeds (tamper check accepts the
--    chain-end carrier).
vim.api.nvim_set_current_buf(p_buf)
save.request_save_buffer()
local saved = T.wait_for(function ()
  return not vim.bo[p_buf].modified
end, 10)
T.check(saved, 'saving the chain-end view did not complete')
print('saving the chain-end view succeeded')

T.pass('PASS: Override-chain integration test successful!')
