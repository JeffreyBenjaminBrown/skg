-- Mirrors tests/elisp/test-skg-insert-heading-repo-prompt.el: after
-- nvim-orgmode's insert-heading actions ('org_insert_heading_respect_content'
-- and 'org_meta_return'), a new headline in a content view that cannot
-- inherit a skgrepo gets minimal metadata with an owned skgrepo,
-- prompted for unless only one is owned, and no edit buffer opens.

local config = require('skg.config')
local metadata_edit = require('skg.metadata_edit')
local picker = require('skg.picker')

local actions = { 'org_mappings.insert_heading_respect_content',
                  'org_mappings.meta_return' }

do -- nvim-orgmode and its parser, baked in beside plenary. The child
   -- nvim the harness starts has plenary on its runtimepath but not
   -- the other packpath plugins, and other specs run without them.
  local busted = vim.api.nvim_get_runtime_file(
    'lua/plenary/busted.lua', false)[1]
  local start = vim.fn.fnamemodify(busted, ':h:h:h:h')
  for _, plugin in ipairs({ 'orgmode', 'nvim-org-treesitter-parser' }) do
    if vim.fn.isdirectory(start .. '/' .. plugin) == 1 then
      vim.opt.runtimepath:append(start .. '/' .. plugin) end
  end
  require('orgmode').setup({})
end

---A content view holding TEXT, the cursor on the line matching
---CURSOR_PATTERN (default: the first). nvim-orgmode finds headlines
---only in a buffer named like an org file.
local function content_view_with (text, cursor_pattern)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_buf_set_name(buf, vim.fn.tempname() .. '.org')
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
  vim.bo[buf].buftype = 'acwrite'
  vim.bo[buf].filetype = 'org'
  vim.b[buf].skg_content_view = true
  local cursor = 1
  if cursor_pattern then
    for line, content in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
      if content:match(cursor_pattern) then cursor = line break end
    end
  end
  vim.api.nvim_win_set_cursor(0, { cursor, 0 })
  return buf
end

---Run ACTION in the current buffer with OWNED as the owned skgrepos,
---answering any skgrepo prompt with 'private'. Returns whether it
---prompted.
local function run_action (action, owned)
  local prompted = false
  local real_owned_repos = config.owned_repos
  local real_picker = picker.completing_read_with_cycle
  config.owned_repos = function () return owned end
  picker.completing_read_with_cycle = function ()
    prompted = true
    return 'private'
  end
  local ok, err = pcall(function ()
    metadata_edit.wrap_org_insert_heading_actions()
    require('orgmode').action(action)
    vim.cmd('stopinsert')
  end)
  config.owned_repos = real_owned_repos
  picker.completing_read_with_cycle = real_picker
  if not ok then error(err, 0) end
  return prompted
end

---Does the current buffer hold a line equal to LINE?
local function has_line (line)
  return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), line)
end

local function edit_buffer_open_p ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.b[buf].skg_edit_source_buf then return true end
  end
  return false
end

describe('the skgrepo prompt after inserting a headline', function ()
  after_each(function ()
    vim.api.nvim_buf_delete(0, { force = true })
  end)

  for _, action in ipairs(actions) do
    it(action .. ' on a root prompts and writes the skgrepo', function ()
      content_view_with('* (skg (node (id x) (repo public))) x')
      assert.is_true(run_action(action, { 'public', 'private' }))
      assert.is_true(has_line('* (skg (node (id x) (repo public))) x'))
      assert.is_true(has_line('* (skg (node (repo private))) '))
      assert.is_false(edit_buffer_open_p())
    end)

    it(action .. ' uses the only owned skgrepo without a prompt',
       function ()
      content_view_with('* (skg (node (id x) (repo only))) x')
      assert.is_false(run_action(action, { 'only' }))
      assert.is_true(has_line('* (skg (node (repo only))) '))
    end)

    it(action .. ' prompts only when the headline cannot inherit',
       function ()
      for _, case in ipairs({
        { '** (skg subscribeeFolder)', false },
        { '** (skg (deleted (id d) (repo public))) d', false },
        { '** (skg bodyFolded (node (id m) (repo public))) m', false },
        { '** (skg tombstone)', true } }) do
        content_view_with(table.concat({
          '* (skg (node (id x) (repo public))) x',
          case[1],
          '*** (skg (node (id y) (repo public))) y' }, '\n'), '^%*%*%* ')
        assert.are.equal(case[2], run_action(action, { 'public', 'private' }),
                         case[1])
        assert.are.equal(case[2],
                         has_line('*** (skg (node (repo private))) '),
                         case[1])
        vim.api.nvim_buf_delete(0, { force = true })
      end
      content_view_with('* placeholder')
    end)
  end

  it('leaves a buffer that is no content view alone', function ()
    content_view_with('* (skg (node (id x) (repo public))) x')
    vim.b.skg_content_view = nil
    assert.is_false(run_action(actions[1], { 'public', 'private' }))
    assert.is_true(has_line('* '))
  end)
end)
