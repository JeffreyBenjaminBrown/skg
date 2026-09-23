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
      table.insert(result, mark[4].virt_text[1][1]) end end
  return result
end

describe('skg.link_annotations', function ()
  before_each(function ()
    annotations.cache = {}
    annotations.requests = {}
  end)

  it('styles only missing Skg links and adds optional source suffixes',
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
    assert.are.equal(0, #suffixes(buf))
    annotations.toggle_source_overlay(buf)
    assert.same({ ' [⌂:pub]', ' [⌂:missing]', ' [⌂:inactive]' },
                suffixes(buf))
    assert.same(lines, vim.api.nvim_buf_get_lines(buf, 0, -1, false))
    assert.is_false(vim.bo[buf].modified)
    annotations.refresh(buf)
    assert.are.equal(3, #suffixes(buf))
    annotations.toggle_source_overlay(buf)
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
    annotations.requests.old_source_set = {
      buf = second,
      generation = vim.b[second].skg_link_annotations_generation,
      tick = vim.api.nvim_buf_get_changedtick(second),
      epoch = 20, ids = { 'node' } }
    annotations.epoch = 21
    annotations.handle_response(sexpr.read(
      '((request-id "old_source_set") (results (("node" missing))))'))
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
end)
