local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local config = require('skg.config')
local raw_file = require('skg.raw_file')
local registry = require('skg.buffer_registry')

local function write_bytes (path, bytes)
  local file = assert(io.open(path, 'wb'))
  assert(file:write(bytes))
  assert(file:close())
end

local function read_bytes (path)
  local file = assert(io.open(path, 'rb'))
  local bytes = assert(file:read('*a'))
  assert(file:close())
  return bytes
end

describe('skg configured raw-file fence', function ()
  local directory, path, old_inventory, original_queue

  before_each(function ()
    helpers.wipe_skg_buffers()
    directory = vim.fn.tempname()
    assert.are.equal(1, vim.fn.mkdir(directory, 'p'))
    path = directory .. '/node.skg'
    old_inventory = config.source_inventory
    config.source_inventory = { { name = 'main', path = directory } }
    original_queue = raw_file.queue_observation
  end)

  after_each(function ()
    raw_file.queue_observation = original_queue
    config.source_inventory = old_inventory
    helpers.wipe_skg_buffers()
    vim.fn.delete(directory, 'rf')
  end)

  local function open_raw (text)
    write_bytes(path, text)
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, path)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false,
      vim.split(text:sub(-1) == '\n' and text:sub(1, -2) or text, '\n'))
    vim.bo[buf].endofline = text:sub(-1) == '\n'
    vim.bo[buf].modified = false
    assert.is_truthy(raw_file.enroll(buf, true))
    return buf
  end

  it('refuses an exact-byte mismatch and queues observation', function ()
    local buf = open_raw('pid: old\n')
    local queued = 0
    raw_file.queue_observation = function () queued = queued + 1 end
    write_bytes(path, 'pid: new\n')
    local ok, reason = pcall(raw_file.guard_before_save, buf)
    assert.is_false(ok)
    assert.is_truthy(tostring(reason):find('disk changed', 1, true))
    assert.is_true(vim.b[buf].skg_raw_externally_stale)
    assert.are.equal(1, queued)
    assert.are.equal('pid: new\n', read_bytes(path))
    assert.are.equal('pid: old\n', registry.raw_text(buf))
  end)

  it('advances the exact baseline only after save and queues observation',
     function ()
    local buf = open_raw('pid: old\n')
    local queued = 0
    raw_file.queue_observation = function () queued = queued + 1 end
    write_bytes(path, 'pid: new\n')
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'pid: new' })
    vim.bo[buf].endofline = true
    vim.bo[buf].modified = false
    raw_file.after_save(buf)
    raw_file.guard_before_save(buf)
    assert.are.equal(1, queued)
    assert.is_false(vim.b[buf].skg_raw_externally_stale)
    assert.are.equal('pid: new\n', registry.record(buf).last_fetched)
  end)

  it('refuses every raw save during a maintenance epoch', function ()
    local buf = open_raw('pid: old\n')
    vim.b[buf].skg_maintenance_epoch = 9
    local ok, reason = pcall(raw_file.guard_before_save, buf)
    assert.is_false(ok)
    assert.is_truthy(tostring(reason):find('maintenance epoch 9', 1, true))
  end)

  it('refuses while another live Skg view is logically dirty', function ()
    local buf = open_raw('pid: old\n')
    local view = vim.api.nvim_create_buf(true, true)
    vim.api.nvim_buf_set_name(view, 'skg://dirty-for-raw')
    vim.bo[view].modified = false
    registry.register(view, 'content-view', {
      lifecycle = 'live-view', disposable = false,
    })
    vim.b[view].skg_logical_dirty = true
    local ok, reason = pcall(raw_file.guard_before_save, buf)
    assert.is_false(ok)
    assert.is_truthy(tostring(reason):find('dirty-for-raw', 1, true))
  end)
end)
