-- Mirrors tests/elisp/test-skg-folds-across-redraw.el: a redraw the
-- user did not ask for (a rerender, a save's collateral view) keeps
-- the folds the user changed since the last save.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local folds = require('skg.folds')
local redraw_whole_view = require('skg.redraw_whole_view')
local sexpr = require('skg.sexpr.parse')

---A view as its last save left it: B folded, so B1 hidden.
local last_save =
  '* (skg (vnid a0) node (gnid ga0)) A\nbody of A\n'
  .. '** (skg (vnid a1) node (gnid ga1)) A1\nbody of A1\n'
  .. '* (skg (vnid b0) node (gnid gb0)) B\nbody of B\n'
  .. '** (skg (vnid b1) folded node (gnid gb1)) B1\nbody of B1\n'
  .. '* (skg (vnid c0) node (gnid gc0)) C\nbody of C\nmore of C\n'

local function rerender (content)
  local payload = '((view-id "view-folds") (content "' .. content .. '"))'
  redraw_whole_view.apply_streamed_view_update(
    payload, sexpr.read(payload), 'rerender', 'rerender-view')
end

---The line of the headline whose title is TITLE.
local function headline_line (title)
  for line_number, line in ipairs(
        vim.api.nvim_buf_get_lines(0, 0, -1, false)) do
    if line:sub(-(#title + 2)) == ') ' .. title then
      return line_number end end
  error('no headline titled ' .. title)
end

local function invisible_p (title)
  return folds.line_invisible_p(headline_line(title))
end

local function body_hidden_p (title)
  return vim.fn.foldclosed(headline_line(title) + 1) ~= -1
end

describe('a redraw the user did not ask for', function ()
  before_each(function ()
    vim.cmd('only')
    helpers.wipe_skg_buffers()
  end)

  it('keeps the folds changed since the last save, and draws a new'
     .. ' headline as the server says', function ()
    local buf = buffer.open_org_buffer_from_text(last_save, 'skg://folds',
                                                 'view-folds')
    folds.set_up_window()
    redraw_whole_view.replace_buffer_with_new_content(buf, last_save, nil) -- the last save's redraw
    assert.is_true(invisible_p('B1'))
    -- The user unfolds B, folds A and hides C's body.
    vim.cmd(string.format('%dfoldopen', headline_line('B')))
    folds.close_fold_at(headline_line('A'))
    folds.hide_entry(headline_line('C'))
    assert.is_true(invisible_p('A1'))
    rerender(last_save
             .. '* (skg (vnid d0) bodyFolded node (gnid gd0)) D\n'
             .. 'body of D\nmore of D\n')
    assert.is_true(invisible_p('A1'))
    assert.is_false(invisible_p('B1'))
    assert.is_false(body_hidden_p('B1'))
    assert.is_true(body_hidden_p('C'))
    assert.is_true(body_hidden_p('D'))
    assert.is_false(invisible_p('D'))
    local text = table.concat(
      vim.api.nvim_buf_get_lines(0, 0, -1, false), '\n')
    assert.is_nil(text:find('folded', 1, true))
    assert.is_nil(text:find('bodyFolded', 1, true))
  end)
end)

describe('a redraw the user did not ask for, after a collapsed'
         .. ' headline lost its children', function ()
  before_each(function ()
    vim.cmd('only')
    helpers.wipe_skg_buffers()
  end)

  it('keeps that headline collapsed, and shows what moved to an open'
     .. ' one and what is new under one', function ()
    local buf = buffer.open_org_buffer_from_text(last_save, 'skg://folds',
                                                 'view-folds')
    folds.set_up_window()
    redraw_whole_view.replace_buffer_with_new_content(buf, last_save, nil) -- the last save's redraw
    assert.is_true(invisible_p('B1'))
    rerender('* (skg (vnid a0) node (gnid ga0)) A\nbody of A\n'
             .. '** (skg (vnid a1) node (gnid ga1)) A1\nbody of A1\n'
             .. '** (skg (vnid n2) folded node (gnid gn2)) N2\n'
             .. '* (skg (vnid b0) node (gnid gb0)) B\nbody of B\nmore of B\n'
             .. '* (skg (vnid c0) node (gnid gc0)) C\nbody of C\nmore of C\n'
             .. '** (skg (vnid b1) folded node (gnid gb1)) B1\nbody of B1\n')
    assert.is_true(body_hidden_p('B'))
    assert.is_false(body_hidden_p('C'))
    assert.is_false(invisible_p('B1'))
    assert.is_false(invisible_p('N2'))
  end)
end)

describe('content_with_fold_atoms', function ()
  it("decides each headline's folded by its parent and bodyFolded by"
     .. ' itself, keeping the focus atom and the title', function ()
    assert.are.equal(
      '* (skg (vnid k1) bodyFolded focused folded node (gnid g1)) folded title\n'
      .. 'body\n'
      .. '** (skg (vnid k2) node (gnid g2)) two\n'
      .. '** (skg (vnid n3) node (gnid g3)) three\n'
      .. '*** (skg (vnid n4) folded node (gnid g4)) four\n'
      .. '* (skg (vnid k5) bodyFolded node (gnid g5)) five\nbody\n'
      .. '* (skg (vnid k6) node (gnid g6)) six\n'
      .. '** (skg (vnid n7) folded node (gnid g7)) seven\n',
      redraw_whole_view.content_with_fold_atoms(
        '* (skg (vnid k1) focused folded node (gnid g1)) folded title\n'
        .. 'body\n'
        .. '** (skg (vnid k2) folded bodyFolded node (gnid g2)) two\n'
        .. '** (skg (vnid n3) node (gnid g3)) three\n'
        .. '*** (skg (vnid n4) folded node (gnid g4)) four\n'
        .. '* (skg (vnid k5) node (gnid g5)) five\nbody\n'
        .. '* (skg (vnid k6) bodyFolded node (gnid g6)) six\n'
        .. '** (skg (vnid n7) node (gnid g7)) seven\n',
        { k1 = 'bodyHidden', k2 = 'open', k5 = 'collapsed',
          k6 = 'collapsed' }))
  end)
end)
