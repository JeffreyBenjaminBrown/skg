local annotations = require('skg.link_annotations')
local sexpr = require('skg.sexpr.parse')

local function marks (buf)
  return vim.api.nvim_buf_get_extmarks(
    buf, annotations.namespace, 0, -1, { details = true })
end

local function suffixes (buf)
  local result = {}
  for _, mark in ipairs(marks(buf)) do
    if mark[4].virt_text then
      local text = ''
      for _, chunk in ipairs(mark[4].virt_text) do text = text .. chunk[1] end
      table.insert(result, text) end end
  return result
end

---{name, lines, live} for each case in the file, which the Rust and
---Emacs tests read too.
local function shared_cases ()
  local path = debug.getinfo(1, 'S').source:sub(2):match('^(.*)/')
    .. '/../shared/literal-link-cases.txt'
  local cases, current = {}, nil
  for line in io.lines(path) do
    if line:sub(1, 5) == '==== ' then
      current = { name = line:sub(6), lines = {} }
    elseif line:sub(1, 10) == '---- live:' then
      current.live = vim.split(vim.trim(line:sub(11)), '%s+',
                               { trimempty = true })
      table.insert(cases, current)
      current = nil
    elseif current then
      table.insert(current.lines, line)
    end
  end
  return cases
end

describe('skg.link_annotations', function ()
  before_each(function ()
    annotations.cache = {}
    annotations.requests = {}
  end)

  it('styles only missing Skg links and adds optional repo suffixes',
     function ()
    local buf = vim.api.nvim_create_buf(true, false)
    local lines = {
      '* Unicode [[id:old-visible][café]] [[id:gone][gone]]',
      'Body [[id:private][private]] [[https://x][web]]' }
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modified = false
    annotations.cache['old-visible'] = { 'resolved', 'visible', 'pub' }
    annotations.cache.gone = { 'missing' }
    annotations.cache.private = { 'inactive' }
    annotations.enable(buf)
    local broken = 0
    for _, mark in ipairs(marks(buf)) do
      if mark[4].hl_group == 'SkgBrokenLink' then
        broken = broken + 1
        assert.are.equal('gone',
          vim.api.nvim_buf_get_lines(buf, mark[2], mark[2] + 1, false)[1]
            :sub(mark[3] + 1, mark[4].end_col)) end
    end
    assert.are.equal(1, broken)
    local yucky = vim.api.nvim_get_hl(0,
      { name = 'SkgHeraldYucky', link = false })
    local broken_link = vim.api.nvim_get_hl(0,
      { name = 'SkgBrokenLink', link = false })
    assert.are.equal(yucky.fg, broken_link.fg)
    assert.is_true(broken_link.underline)
    assert.are.equal(0, #suffixes(buf))
    annotations.toggle_repo_overlay(buf)
    -- the broken link [[id:gone]] gets no suffix
    assert.same({ ' [⌂:pub]', ' [⌂:inactive]' }, suffixes(buf))
    assert.same(lines, vim.api.nvim_buf_get_lines(buf, 0, -1, false))
    assert.is_false(vim.bo[buf].modified)
    annotations.refresh(buf)
    assert.are.equal(2, #suffixes(buf))
    annotations.toggle_repo_overlay(buf)
    assert.are.equal(0, #suffixes(buf))
    annotations.disable(buf)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('drops obsolete replies and associates live replies with their buffer',
     function ()
    local first = vim.api.nvim_create_buf(true, false)
    local second = vim.api.nvim_create_buf(true, false)
    for _, buf in ipairs({ first, second }) do
      vim.api.nvim_buf_set_lines(buf, 0, -1, false,
                                 { '* [[id:node][label]]' })
      annotations.enable(buf)
    end
    annotations.cache = {}
    annotations.epoch = 20
    annotations.requests.old = {
      buf = first,
      generation = vim.b[first].skg_link_annotations_generation,
      tick = vim.api.nvim_buf_get_changedtick(first),
      epoch = 20, ids = { 'node' } }
    annotations.requests.right = {
      buf = second,
      generation = vim.b[second].skg_link_annotations_generation,
      tick = vim.api.nvim_buf_get_changedtick(second),
      epoch = 20, ids = { 'node' } }
    vim.api.nvim_buf_set_lines(first, 0, -1, false,
                               { '* edited [[id:node][label]]' })
    annotations.handle_response(sexpr.read(
      '((request-id "old") (results (("node" missing))))'))
    assert.is_nil(annotations.cache.node)
    annotations.handle_response(sexpr.read(
      '((request-id "right")'
      .. ' (results (("node" resolved "node" "main"))))'))
    assert.same({ 'resolved', 'node', 'main' }, annotations.cache.node)
    assert.is_nil(annotations.requests.old)
    assert.is_nil(annotations.requests.right)
    annotations.requests.old_repo_set = {
      buf = second,
      generation = vim.b[second].skg_link_annotations_generation,
      tick = vim.api.nvim_buf_get_changedtick(second),
      epoch = 20, ids = { 'node' } }
    annotations.epoch = 21
    annotations.handle_response(sexpr.read(
      '((request-id "old_repo_set") (results (("node" missing))))'))
    assert.same({ 'resolved', 'node', 'main' }, annotations.cache.node)
    annotations.requests.dead_buffer = {
      buf = first, generation = 1, tick = 1, epoch = 21,
      ids = { 'node' } }
    vim.api.nvim_buf_delete(first, { force = true })
    annotations.handle_response(sexpr.read(
      '((request-id "dead_buffer") (results (("node" missing))))'))
    assert.same({ 'resolved', 'node', 'main' }, annotations.cache.node)
    annotations.requests.old_connection = {
      buf = second, generation = 1, tick = 1, epoch = 21,
      ids = { 'node' } }
    annotations.connection_reset()
    assert.is_nil(annotations.requests.old_connection)
    annotations.disable(first)
    annotations.disable(second)
    vim.api.nvim_buf_delete(second, { force = true })
  end)
  it('annotates only real links, per the cases shared with Rust and Emacs',
     function ()
    local cases = shared_cases()
    assert.is_true(#cases > 5)
    for _, case in ipairs(cases) do
      local buf = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, case.lines)
      local skgids = {}
      for _, position in ipairs(annotations.collect(buf)) do
        table.insert(skgids, position.id) end
      assert.are.same({ case.name, case.live }, { case.name, skgids })
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end)

  it('ends a block at a headline, as a body ends on the server', function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
      '* h', '#+begin_example', '[[id:a][A]]', '* next [[id:b][B]]' })
    local skgids = {}
    for _, position in ipairs(annotations.collect(buf)) do
      table.insert(skgids, position.id) end
    assert.are.same({ 'b' }, skgids)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)
