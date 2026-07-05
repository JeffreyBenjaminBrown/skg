-- Mirrors tests/elisp/test-skg-org-fold.el, on vim folds driven by
-- skg's buffer-local foldexpr instead of org-fold.

local folds = require('skg.folds')
local metadata = require('skg.metadata')

local function buffer_with (text)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
  vim.api.nvim_set_current_buf(buf)
  folds.set_up_window()
  return buf
end

local function buffer_text ()
  return table.concat(
    vim.api.nvim_buf_get_lines(0, 0, -1, false), '\n')
end

---Visible non-empty lines, counting a closed fold's own first line as
---visible (org showed the folded heading too).
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

  it('round-trips fold state through the markers', function ()
    -- Mirrors test-skg-fold-round-trip.
    buffer_with(table.concat({
      '* 1',
      '1 body',
      '** 11',
      '*** (skg folded (node (id 1))) 111',
      '*** (skg folded (node (id 2) indef)) 112',
      '** 12',
      '12 body',
      '*** (skg folded (node (id 3))) 121',
      '121 body',
      '*** 122',
      '** 13' }, '\n'))
    folds.fold_marked_headlines()
    assert.are.equal(5, count_visible_nonempty_lines())
    folds.remove_folded_markers()
    folds.add_folded_markers()
    assert.are.equal(table.concat({
      '* 1',
      '1 body',
      '** 11',
      '*** (skg (node (id 1)) folded) 111',
      '*** (skg (node (id 2) indef) folded) 112',
      '** 12',
      '12 body',
      '*** (skg (node (id 3)) folded) 121',
      '121 body',
      '*** (skg folded) 122',
      '** 13' }, '\n'), buffer_text())
  end)

  it('removes folded markers of every metadata shape', function ()
    -- Mirrors test-skg-remove-folded-markers.
    buffer_with(table.concat({
      '* (skg folded) only folded',
      '* (skg folded other) folded first',
      'Body text.',
      '* (skg folded (key value)) folded last',
      '* (skg folded (k v) other) folded middle',
      '* (skg other) no folded' }, '\n'))
    folds.remove_folded_markers()
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
    folds.add_folded_markers()
    assert.is_true(folds.headline_has_bodyfolded_p(1))
    assert.is_false(folds.headline_has_folded_p(4))
    folds.remove_folded_markers()
    assert.is_falsy(buffer_text():find('bodyFolded'))
    assert.is_true(folds.headline_body_hidden_p(1))
  end)

  it('cannot body-fold a one-line body (vim fold limitation)',
     function ()
    -- Pins the documented deviation: vim cannot close a single-line
    -- fold, so a one-line body stays visible and its bodyFolded
    -- marker degrades gracefully rather than erroring.
    buffer_with(table.concat({
      '* top', 'only body line', '** child' }, '\n'))
    folds.hide_entry(1)
    assert.is_false(folds.headline_body_hidden_p(1))
    folds.add_folded_markers()
    assert.is_false(folds.headline_has_bodyfolded_p(1))
  end)

  it('treats a folded subtree heading as visible', function ()
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
