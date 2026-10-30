-- Mirrors tests/elisp/test-skg-org-fold.el, on vim folds driven by
-- skg's buffer-local foldexpr instead of org-fold.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local folds = require('skg.folds')
local metadata = require('skg.metadata')

local function buffer_with (text)
  local buf = helpers.buffer_with(text)
  folds.set_up_window()
  return buf
end

local buffer_text = helpers.buffer_text

---Visible non-empty lines, counting a closed fold's own first line as
---visible (org showed the folded headline too).
local function count_visible_nonempty_lines ()
  local count = 0
  for line = 1, vim.api.nvim_buf_line_count(0) do
    if not folds.line_invisible_p(line)
       and metadata.line_text(line) ~= '' then
      count = count + 1 end
  end
  return count
end

describe('skg.folds', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('round-trips fold state through the atoms', function ()
    -- Mirrors test-skg-fold-round-trip.
    buffer_with(table.concat({
      '* 1',
      '1 body',
      '** 11',
      '*** (skg folded node (gnid 1)) 111',
      '*** (skg folded node (gnid 2) writeProtected) 112',
      '** 12',
      '12 body',
      '*** (skg folded node (gnid 3)) 121',
      '121 body',
      '*** 122',
      '** 13' }, '\n'))
    folds.fold_marked_headlines()
    assert.are.equal(5, count_visible_nonempty_lines())
    folds.remove_folded_metadata()
    folds.add_folded_metadata()
    assert.are.equal(table.concat({
      '* 1',
      '1 body',
      '** 11',
      '*** (skg folded node (gnid 1)) 111',
      '*** (skg folded node (gnid 2) writeProtected) 112',
      '** 12',
      '12 body',
      '*** (skg folded node (gnid 3)) 121',
      '121 body',
      '*** (skg folded) 122',
      '** 13' }, '\n'), buffer_text())
  end)

  it('removes folded atoms of every metadata shape', function ()
    -- Mirrors test-skg-remove-folded-metadata.
    buffer_with(table.concat({
      '* (skg folded) only folded',
      '* (skg folded other) folded first',
      'Body text.',
      '* (skg folded (key value)) folded last',
      '* (skg folded (k v) other) folded middle',
      '* (skg other) no folded' }, '\n'))
    folds.remove_folded_metadata()
    assert.are.equal(table.concat({
      '* (skg) only folded',
      '* (skg other) folded first',
      'Body text.',
      '* (skg (key value)) folded last',
      '* (skg (k v) other) folded middle',
      '* (skg other) no folded' }, '\n'), buffer_text())
  end)

  it('records and restores body folds via bodyFolded', function ()
    -- No exact elisp counterpart file, but the behavior is pinned by
    -- the fold-preservation integration tests; this is the unit-level
    -- shape: hide only a body, serialize, restore.
    buffer_with(table.concat({
      '* top',
      'body one',
      'body two',
      '** child' }, '\n'))
    folds.hide_entry(1)
    assert.is_true(folds.headline_body_hidden_p(1))
    assert.is_false(folds.line_invisible_p(4)) -- the child stays visible
    folds.add_folded_metadata()
    assert.is_true(folds.headline_has_bodyfolded_p(1))
    assert.is_false(folds.headline_has_folded_p(4))
    folds.remove_folded_metadata()
    assert.is_falsy(buffer_text():find('bodyFolded'))
    assert.is_true(folds.headline_body_hidden_p(1))
  end)

  it('cannot body-fold a one-line body (vim fold limitation)',
     function ()
    -- Pins the documented deviation: vim cannot close a single-line
    -- fold, so a one-line body stays visible and its bodyFolded
    -- atom degrades gracefully rather than erroring.
    buffer_with(table.concat({
      '* top', 'only body line', '** child' }, '\n'))
    folds.hide_entry(1)
    assert.is_false(folds.headline_body_hidden_p(1))
    folds.add_folded_metadata()
    assert.is_false(folds.headline_has_bodyfolded_p(1))
  end)

  it('treats a folded subtree headline as visible', function ()
    buffer_with(table.concat({
      '* a',
      '** b',
      '*** c' }, '\n'))
    folds.close_fold_at(1)
    assert.is_false(folds.line_invisible_p(1))
    assert.is_true(folds.line_invisible_p(2))
    assert.is_true(folds.line_invisible_p(3))
    folds.show_all()
    assert.is_false(folds.line_invisible_p(2))
  end)
end)

describe('skg.folds body toggle', function ()
  after_each(function ()
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  ---Is the line holding TEXT hidden? A headline is hidden inside a
  ---closed fold that starts above it; a folded headline's own line
  ---still shows (folds.line_invisible_p). A body line is hidden inside
  ---any closed fold, its body fold's first line included, since that
  ---line shows the fold's placeholder.
  local function hidden_p (text)
    for line = 1, vim.api.nvim_buf_line_count(0) do
      if metadata.line_text(line) == text then
        if metadata.at_headline_p(line) then
          return folds.line_invisible_p(line) end
        return vim.fn.foldclosed(line) ~= -1 end
    end
    error('no line ' .. text)
  end

  it('hides and shows a body, from the headline or the body', function ()
    -- Mirrors test-skg-org-body-toggle.
    buffer_with('* first\nbody line one\nbody line two\n* second')
    folds.org_body_toggle()
    assert.is_true(hidden_p('body line one'))
    folds.org_body_toggle()
    assert.is_false(hidden_p('body line one'))
    vim.api.nvim_win_set_cursor(0, { 3, 4 })
    folds.org_body_toggle()
    assert.are.same({ 1, 0 }, vim.api.nvim_win_get_cursor(0))
    assert.is_true(hidden_p('body line one'))
    vim.api.nvim_win_set_cursor(0, { 4, 0 })
    local notified = {}
    local original_notify = vim.notify
    vim.notify = function (message) table.insert(notified, message) end
    folds.org_body_toggle()
    vim.notify = original_notify
    assert.are.same({ 'This headline has no body.' }, notified)
    assert.are.same({ 4, 0 }, vim.api.nvim_win_get_cursor(0))
    assert.is_true(hidden_p('body line one'))
  end)

  it('opens a folded headline one level', function ()
    -- Mirrors test-skg-org-body-toggle-opens-a-folded-headline-one-level,
    -- with a body of two lines where the elisp test's has one, which
    -- vim cannot hide (the last case below).
    buffer_with(table.concat({
      '* parent', 'parent body', 'more parent body',
      '** first child', 'first child body',
      '*** grandchild', 'grandchild body',
      '** second child', 'second child body',
      '* sibling' }, '\n'))
    folds.close_fold_at(1)
    folds.org_body_toggle()
    assert.is_false(hidden_p('parent body'))
    assert.is_false(hidden_p('** first child'))
    assert.is_true(hidden_p('first child body'))
    assert.is_true(hidden_p('*** grandchild'))
    assert.is_false(hidden_p('** second child'))
    folds.org_body_toggle()
    assert.is_true(hidden_p('parent body'))
    assert.is_false(hidden_p('** first child'))
    folds.org_body_toggle()
    assert.is_false(hidden_p('parent body'))
    assert.is_false(hidden_p('** first child'))
  end)

  it('opens a folded bodyless headline to its children', function ()
    -- Mirrors test-skg-org-body-toggle-opens-a-folded-bodyless-headline.
    buffer_with(table.concat({
      '* parent', '** child', 'child body', '*** grandchild',
      '* sibling' }, '\n'))
    folds.close_fold_at(1)
    folds.org_body_toggle()
    assert.is_false(hidden_p('** child'))
    assert.is_true(hidden_p('child body'))
    assert.is_true(hidden_p('*** grandchild'))
  end)

  it('leaves a one-line body visible, with a message', function ()
    -- No elisp counterpart: org hides a one-line body, vim cannot
    -- close a one-line fold. A second toggle must not close the
    -- headline's subtree instead.
    buffer_with('* top\nonly body line\n** child\nchild body')
    local notified = {}
    local original_notify = vim.notify
    vim.notify = function (message) table.insert(notified, message) end
    folds.org_body_toggle()
    folds.org_body_toggle()
    vim.notify = original_notify
    assert.are.equal(2, #notified)
    assert.is_truthy(notified[1]:find('one-line body', 1, true))
    assert.is_false(hidden_p('only body line'))
    assert.is_false(hidden_p('** child'))
  end)
end)
