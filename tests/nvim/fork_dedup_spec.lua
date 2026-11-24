-- The Lua mirror of tests/elisp/test-skg-fork-dedup.el: extmarks
-- cover a forks-folder member's title or body where it equals its
-- note's ('nvim/lua/skg/fork_dedup.lua'); the text stays as it is.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local fork_dedup = require('skg.fork_dedup')
local heralds = require('skg.heralds')

local text_lines = {
  '* (skg node (gnid F) (skgRepos (nodeRepo main))) Same words',
  'A long body',
  'that its forks restate.',
  '** (skg forksFolder)',
  '*** (skg node (gnid G) (skgRepos (nodeRepo main)) contentInPart) Same words',
  'A long body',
  'that its forks restate.',
  '',
  '*** (skg node (gnid K) (skgRepos (nodeRepo theirs)) contentInPart) Same words',
  'Their own body.',
  '*** (skg node (gnid L) (skgRepos (nodeRepo main)) contentInPart) Other words',
  '** (skg node (gnid C) (skgRepos (nodeRepo main))) Same words',
  'A long body',
  'that its forks restate.' }
local text = table.concat(text_lines, '\n')

---Each cover, in buffer order: { row (0-based), shown text or
---'(lines)' for a mark hiding whole lines }.
local function covers (buf)
  local result = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(
                          buf, fork_dedup.namespace, 0, -1, { details = true })) do
    local details = mark[4]
    table.insert(result, { mark[2], details.virt_text
                           and details.virt_text[1][1] or '(lines)' })
  end
  return result
end

describe('skg.fork_dedup', function ()
  local buf
  before_each(function ()
    buf = helpers.buffer_with(text) or vim.api.nvim_get_current_buf()
    fork_dedup.refresh_buffer(buf)
  end)
  after_each(function ()
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end)

  it('covers what members restate, title and body independently', function ()
    assert.are.same({
      { 4, "<b's title, unchanged>" },
      { 5, "<b's body, unchanged>" },
      { 6, '(lines)' },
      { 8, "<b's title, unchanged>" } }, covers(buf))
    assert.are.equal(text, helpers.buffer_text())
  end)

  it('toggles per buffer', function ()
    fork_dedup.toggle(buf)
    assert.are.same({}, covers(buf))
    fork_dedup.toggle(buf)
    assert.are.equal(4, #covers(buf))
  end)

  it('uncovers what an edit makes differ', function ()
    vim.api.nvim_buf_set_lines(buf, 4, 5, false,
      { '*** (skg node (gnid G) (skgRepos (nodeRepo main)) contentInPart) Same words!' })
    fork_dedup.refresh_rows(buf, { 4 })
    assert.are.same({
      { 5, "<b's body, unchanged>" },
      { 6, '(lines)' },
      { 8, "<b's title, unchanged>" } }, covers(buf))
    vim.api.nvim_buf_set_lines(buf, 2, 3, false, { 'that its forks restate. More.' })
    fork_dedup.refresh_rows(buf, { 2 })
    assert.are.same({ { 8, "<b's title, unchanged>" } }, covers(buf))
  end)

  it('covers a whole view written at once, the folder far from its ends',
     function ()
    helpers.install_fixture_herald_rules()
    local view = buf
    vim.api.nvim_buf_set_lines(view, 0, -1, false,
                               { '* (skg node (gnid R)) Root' })
    heralds.enable(view)
    local nested = { '* (skg node (gnid R)) Root' }
    for _, line in ipairs(text_lines) do
      table.insert(nested, (line:gsub('^%*', '**'))) end
    table.insert(nested, '* (skg node (gnid Z)) Last')
    vim.api.nvim_buf_set_lines(view, 0, -1, false, nested)
    vim.wait(500, function () return #covers(view) == 4 end, 10)
    assert.are.same({
      { 5, "<b's title, unchanged>" },
      { 6, "<b's body, unchanged>" },
      { 7, '(lines)' },
      { 9, "<b's title, unchanged>" } }, covers(view))
    heralds.disable(view)
  end)
end)
