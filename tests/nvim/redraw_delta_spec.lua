-- Mirrors tests/elisp/test-skg-redraw-delta.el: the stripped form
-- against the cases the server and both clients share, and the patch
-- 'save.handle_save_sexp' tries when a save-result names the headlines
-- it changed ('nvim/lua/skg/redraw_delta.lua'). Also: a view opened
-- from text carrying fold and focus atoms acts on them, so its first
-- save can be patched.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local folds = require('skg.folds')
local heralds = require('skg.heralds')
local redraw_delta = require('skg.redraw_delta')
local save = require('skg.save')
local sexpr = require('skg.sexpr.parse')

---The cases of 'tests/shared/fold-and-focus-atom-strip-cases.txt', each
---a list { line, stripped_line }.
---@return string[][]
local function strip_cases ()
  local handle = assert(io.open(_G.skg_test_repo_root()
    .. '/tests/shared/fold-and-focus-atom-strip-cases.txt', 'r'))
  local text = handle:read('*a')
  handle:close()
  local cases = {}
  local case_lines = {}
  local lines = vim.split(text, '\n', { plain = true })
  table.insert(lines, '')
  for _, line in ipairs(lines) do
    if line:sub(1, 1) == '#' then -- a comment
    elseif line == '' then
      if #case_lines > 0 then
        table.insert(cases, case_lines)
        case_lines = {} end
    else table.insert(case_lines, line) end
  end
  return cases
end

describe('skg.redraw_delta stripped form', function ()
  it('strips each line of the shared cases file to the line after it',
     function ()
    local cases = strip_cases()
    assert.is_true(#cases > 20)
    for _, case in ipairs(cases) do
      assert.are.equal(2, #case)
      assert.are.same(case, { case[1], redraw_delta.stripped_line(case[1]) })
    end
  end)

  it('strips a text line by line, headline lines only', function ()
    assert.are.same({
      'preamble folded',
      '* (skg (vnid a) node (gnid a)) a',
      'body (skg folded)',
      '** (skg (vnid b) node (gnid b)) b',
      '' },
      redraw_delta.stripped_lines(table.concat({
        'preamble folded',
        '* (skg (vnid a) focused node (gnid a)) a',
        'body (skg folded)',
        '** (skg (vnid b) folded node (gnid b)) b',
        '' }, '\n')))
  end)
end)

---A view as the buffer shows it, without fold and focus atoms.
local drawn = table.concat({
  '* (skg (vnid a) node (gnid a)) a',
  'a body',
  '** (skg (vnid b) node (gnid b)) bee title',
  'b body',
  'b body 2',
  '*** (skg (vnid c) node (gnid c)) c',
  '** (skg (vnid d) node (gnid d)) d',
  '' }, '\n')

---The server's new text for DRAWN: b's line changed, b's subtree
---folded and focused.
local b_marked = table.concat({
  '* (skg (vnid a) node (gnid a)) a',
  'a body',
  '** (skg (vnid b) folded focused node (gnid b) (diff (ready all))) bee title',
  'b body',
  'b body 2',
  '*** (skg (vnid c) node (gnid c)) c',
  '** (skg (vnid d) node (gnid d)) d',
  '' }, '\n')

---A parsed save-result carrying CONTENT and, unless CHANGED_HEADLINES is
---nil, the delta CHANGED_HEADLINES (a list of viewnode IDs).
---@param content string
---@param changed_headlines string[]|nil
---@return any
local function save_result (content, changed_headlines)
  local entries = { '(content ' .. sexpr.quote_string(content) .. ')',
                    '(errors ())', '(warnings ())' }
  if changed_headlines then
    table.insert(entries, '(changed-headlines ('
                 .. table.concat(changed_headlines, ' ') .. '))') end
  return sexpr.read('(' .. table.concat(entries, ' ') .. ')')
end

---The 1-based line of BUF's first line containing TEXT.
---@param buf integer
---@param text string
---@return integer
local function line_containing (buf, text)
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(text, 1, true) then return index end
  end
  error('no line contains ' .. text)
end

---The display text of the herald on BUF's first line containing TEXT,
---or nil.
---@param buf integer
---@param text string
---@return string|nil
local function herald_display_on_line_containing (buf, text)
  local row = line_containing(buf, text) - 1
  local marks = vim.api.nvim_buf_get_extmarks(
    buf, heralds.namespace, { row, 0 }, { row, -1 }, { details = true })
  return marks[1] and heralds.chunks_text(marks[1][4].virt_text)
end

describe('skg.redraw_delta patch', function ()
  local whole_redraws
  local replace_buffer_with_new_content = save.replace_buffer_with_new_content

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    whole_redraws = 0
    save.replace_buffer_with_new_content = function (...)
      whole_redraws = whole_redraws + 1
      return replace_buffer_with_new_content(...)
    end
  end)
  after_each(function ()
    save.replace_buffer_with_new_content = replace_buffer_with_new_content
    helpers.wipe_skg_buffers()
  end)

  ---A view buffer showing TEXT in the current window.
  ---@param text string
  ---@return integer
  local function open_view (text)
    return buffer.open_org_buffer_from_text(text, 'skg://redraw-delta', 'v1')
  end

  local function buffer_lines (buf)
    return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  end

  it('keeps folds and the cursor, and leaves the stripped new text',
     function ()
    -- A save-result naming b rewrites b's line alone: b's closed
    -- subtree stays closed with b's headline shown, the cursor keeps
    -- its character in b's title, and the buffer is unmodified with
    -- the new text as its baseline.
    local buf = open_view(drawn)
    local b_line = line_containing(buf, '(vnid b)')
    folds.close_fold_at(b_line)
    local title_column = buffer_lines(buf)[b_line]:find('title', 1, true) - 1
    vim.api.nvim_win_set_cursor(0, { b_line, title_column })
    save.handle_save_sexp(buf, save_result(b_marked, { 'b' }))
    assert.are.equal(0, whole_redraws)
    assert.are.same(redraw_delta.stripped_lines(b_marked), buffer_lines(buf))
    assert.are.same({ b_line, title_column + #' (diff (ready all))' },
                    vim.api.nvim_win_get_cursor(0))
    assert.are.equal(b_line, vim.fn.foldclosed(b_line))
    assert.is_true(folds.line_invisible_p(line_containing(buf, 'b body')))
    assert.is_true(folds.line_invisible_p(line_containing(buf, '(vnid c)')))
    assert.is_false(folds.line_invisible_p(line_containing(buf, '(vnid d)')))
    assert.is_false(vim.bo[buf].modified)
    assert.are.equal(table.concat(redraw_delta.stripped_lines(b_marked), '\n'),
                     vim.b[buf].skg_clean_baseline)
  end)

  it('keeps folds above and below a rewritten line', function ()
    -- Rewriting a headline inside a closed fold, and one above a body
    -- fold, opens neither.
    local buf = open_view(drawn)
    local a_line = line_containing(buf, '(vnid a)')
    local b_line = line_containing(buf, '(vnid b)')
    folds.hide_entry(b_line)
    folds.close_fold_at(a_line)
    save.handle_save_sexp(buf, save_result(
      drawn:gsub('%(gnid c%)%) c', '(gnid c)) see')
           :gsub('%(gnid b%)%)', '(gnid b) (diff (ready all)))'),
      { 'b', 'c' }))
    assert.are.equal(0, whole_redraws)
    assert.are.equal(a_line, vim.fn.foldclosed(a_line))
    vim.cmd(a_line .. 'foldopen')
    assert.is_true(folds.headline_body_hidden_p(b_line))
  end)

  it('edits nothing for an empty delta', function ()
    -- '(changed-headlines ())' names a delta: no headline changed.
    local buf = open_view(drawn)
    local tick = vim.api.nvim_buf_get_changedtick(buf)
    save.handle_save_sexp(buf, save_result(drawn, {}))
    assert.are.equal(0, whole_redraws)
    assert.are.equal(tick, vim.api.nvim_buf_get_changedtick(buf))
  end)

  it('redraws the whole buffer without a delta', function ()
    local buf = open_view(drawn)
    save.handle_save_sexp(buf, save_result(b_marked, nil))
    assert.are.equal(1, whole_redraws)
  end)

  it('redraws the whole buffer when the check fails', function ()
    -- The new text differs from the buffer in a line the delta does
    -- not name.
    local content = b_marked:gsub('%(gnid d%)%) d', '(gnid d)) d retitled')
    local buf = open_view(drawn)
    assert.is_false(redraw_delta.patch_buffer_with_delta(buf, content, { 'b' }))
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(drawn, '\n'))
    save.handle_save_sexp(buf, save_result(content, { 'b' }))
    assert.are.equal(1, whole_redraws)
    local whole_redraw = helpers.buffer_with('')
    folds.set_up_window()
    replace_buffer_with_new_content(whole_redraw, content, nil)
    assert.are.same(buffer_lines(whole_redraw), buffer_lines(buf))
    vim.api.nvim_buf_delete(whole_redraw, { force = true })
  end)

  it('edits nothing when a viewnode ID cannot be found', function ()
    local buf = open_view(drawn)
    local tick = vim.api.nvim_buf_get_changedtick(buf)
    assert.is_false(redraw_delta.patch_buffer_with_delta(
      buf, b_marked, { 'b', 'zz' }))
    assert.are.equal(tick, vim.api.nvim_buf_get_changedtick(buf))
    save.handle_save_sexp(buf, save_result(b_marked, { 'b', 'zz' }))
    assert.are.equal(1, whole_redraws)
  end)

  it('re-heralds a noClaim child', function ()
    -- A 'noClaim' child shows a pending detachment only under an
    -- editable parent. When the patch makes its parent editable, the
    -- child, whose line no edit touches, is re-heralded.
    local child =
      '** (skg (vnid c) node (gnid c) (skgRepos (nodeRepo r)) noClaim) c'
    local buf = open_view(
      '* (skg (vnid p) node (gnid p) (skgRepos (nodeRepo r)) writeProtected) p\n'
      .. child)
    assert.is_nil(herald_display_on_line_containing(buf, '(vnid c)'))
    assert.is_true(redraw_delta.patch_buffer_with_delta(
      buf, '* (skg (vnid p) node (gnid p) (skgRepos (nodeRepo r))) p\n' .. child,
      { 'p' }))
    assert.are.equal('detach',
                     herald_display_on_line_containing(buf, '(vnid c)'))
  end)

  it('re-heralds the folder above', function ()
    -- A folder's herald reads its members. When the patch makes its one
    -- changed member ready, the folder's herald shows it ready too, at
    -- once: the patch re-heralds without the watcher's deferred pass.
    local root_and_folder =
      '* (skg (vnid r) node (gnid r) (skgRepos (nodeRepo r))) r\n'
      .. '** (skg (vnid f) containerFolder)\n'
    local member = '*** (skg (vnid d) node (gnid d) (skgRepos (nodeRepo r))'
      .. ' noClaim writeProtected (diff (rel removed) (leaves 1 rel))) d'
    local changed = '☮ its containers changed (other files)'
    local buf = open_view(root_and_folder .. member)
    assert.are.equal(changed,
                     herald_display_on_line_containing(buf, '(vnid f)'))
    assert.is_true(redraw_delta.patch_buffer_with_delta(
      buf,
      root_and_folder
        .. member:gsub('%(leaves 1 rel%)', '(leaves 1 rel) (ready rel)'),
      { 'd' }))
    assert.are.equal(changed .. '✓',
                     herald_display_on_line_containing(buf, '(vnid f)'))
  end)

  it('patches the first save after a view opens with atoms', function ()
    -- The opened view acts on the server's fold and focus atoms and
    -- keeps none as text, so the first save's delta applies.
    -- b carries 'folded' (hidden in its parent's closed fold) and
    -- 'focused' (the cursor's headline).
    local buf = open_view(b_marked)
    local a_line = line_containing(buf, '(vnid a)')
    local b_line = line_containing(buf, '(vnid b)')
    assert.are.same(redraw_delta.stripped_lines(b_marked), buffer_lines(buf))
    assert.are.equal(a_line, vim.fn.foldclosed(b_line))
    assert.are.equal(b_line, vim.api.nvim_win_get_cursor(0)[1])
    assert.is_false(vim.bo[buf].modified)
    assert.are.equal(table.concat(redraw_delta.stripped_lines(b_marked), '\n'),
                     vim.b[buf].skg_clean_baseline)
    save.handle_save_sexp(buf, save_result(
      b_marked:gsub('%(ready all%)', '(ready none)'), { 'b' }))
    assert.are.equal(0, whole_redraws)
    assert.are.equal(a_line, vim.fn.foldclosed(b_line))
  end)
end)
