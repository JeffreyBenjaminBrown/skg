-- Mirrors tests/elisp/test-skg-point-across-redraw.el: a redraw the
-- user did not ask for (a rerender, a save's collateral view) keeps
-- each window's cursor and scroll on its viewnode.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local redraw_whole_view = require('skg.redraw_whole_view')
local sexpr = require('skg.sexpr.parse')

---COUNT headlines, each with a two-line body, numbered from 0, the
---headline numbered N carrying viewnode ID vN.
local function headlines (count)
  local parts = {}
  for n = 0, count - 1 do
    table.insert(parts, string.format(
      '* (skg (vnid v%d) node (gnid g%d)) title %d\nbody %d one\nbody %d two\n',
      n, n, n, n, n)) end
  return table.concat(parts)
end

---The view of COUNT headlines as another writer's change redraws it: a
---new headline above the rest, headline 0 still focused by the last
---save, and headline 1's body grown by a line.
local function changed_elsewhere (count)
  local text = headlines(count)
    :gsub('%(vnid v0%) ', '(vnid v0) focused ')
    :gsub('body 1 two\n', 'body 1 two\nbody 1 three\n')
  return '* (skg (vnid w0) node (gnid new)) a new first headline\n' .. text
end

local function rerender (content)
  local payload = '((view-id "view-point") (content "' .. content .. '"))'
  redraw_whole_view.apply_streamed_view_update(
    payload, sexpr.read(payload), 'rerender', 'rerender-view')
end

local function open_view (text)
  local buf = buffer.open_org_buffer_from_text(text, 'skg://point',
                                               'view-point')
  require('skg.folds').set_up_window()
  return buf
end

---Put the current window's cursor at COLUMN of the line reading TEXT.
local function goto_line (text, column)
  for line_number, line in ipairs(
        vim.api.nvim_buf_get_lines(0, 0, -1, false)) do
    if line == text then
      vim.api.nvim_win_set_cursor(0, { line_number, column or 0 })
      return end end
  error('no line reads ' .. text)
end

local function cursor_line_text ()
  return vim.api.nvim_get_current_line()
end

describe('a redraw the user did not ask for', function ()
  before_each(function ()
    vim.cmd('only')
    helpers.wipe_skg_buffers()
  end)

  it('keeps the cursor on its viewnode', function ()
    open_view(headlines(6))
    goto_line('body 4 two', 3)
    rerender(changed_elsewhere(6))
    assert.are.equal('body 4 two', cursor_line_text())
    assert.are.equal(3, vim.api.nvim_win_get_cursor(0)[2])
  end)

  it('moves the cursor to the nearest remaining headline when its'
     .. ' viewnode is gone', function ()
    open_view(headlines(6))
    goto_line('body 3 one')
    rerender((headlines(6):gsub(
      '%* %(skg %(vnid v3%)[^\n]*\nbody 3 one\nbody 3 two\n', '')))
    assert.is_truthy(cursor_line_text():find('^%* %(skg %(vnid v4%) '))
  end)

  it('keeps each window\'s cursor and screen line', function ()
    open_view(headlines(40))
    local selected = vim.api.nvim_get_current_win()
    vim.cmd('split')
    local other = vim.api.nvim_get_current_win()
    goto_line('body 30 two')
    vim.fn.winrestview({ topline = vim.fn.line('.') - 5 })
    vim.api.nvim_set_current_win(selected)
    goto_line('body 20 one', 2)
    vim.fn.winrestview({ topline = vim.fn.line('.') - 3 })
    rerender(changed_elsewhere(40))
    assert.are.equal('body 20 one', cursor_line_text())
    assert.are.equal(2, vim.api.nvim_win_get_cursor(0)[2])
    assert.are.equal(4, vim.fn.winline())
    vim.api.nvim_win_call(other, function ()
      assert.are.equal('body 30 two', cursor_line_text())
      assert.are.equal(6, vim.fn.winline()) end)
  end)

  it('leaves the cursor under an unsaved headline on the focused one',
     function ()
    local after = headlines(2):gsub('v(%d)', 'x%1')
    open_view(headlines(3) .. '* my new headline\nnew body\n' .. after)
    goto_line('new body')
    rerender(headlines(3)
      .. '* (skg (vnid n1) focused node (gnid new)) my new headline\n'
      .. 'new body\n' .. after)
    assert.are.equal('* (skg (vnid n1) node (gnid new)) my new headline',
                     cursor_line_text())
  end)

  it('keeps the screen line when the redraw closes folds above the'
     .. ' cursor', function ()
    open_view(headlines(40))
    goto_line('body 20 one')
    vim.fn.winrestview({ topline = vim.fn.line('.') - 8 })
    assert.are.equal(9, vim.fn.winline())
    -- Headlines 18 and 19 are new in the redraw, so their folds are
    -- the server's.
    rerender((headlines(40)
      :gsub('%(vnid v18%) ', '(vnid x18) bodyFolded ')
      :gsub('%(vnid v19%) ', '(vnid x19) bodyFolded ')))
    assert.are.equal('body 20 one', cursor_line_text())
    assert.are_not.equal(-1, vim.fn.foldclosed(vim.fn.search('^body 19 one$', 'bnw')))
    assert.are.equal(9, vim.fn.winline())
  end)
end)
