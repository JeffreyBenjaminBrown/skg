local archive = require('skg.recovery_archive')
local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')

local incident_id = '12345678-1234-4234-8234-123456789abc'

local function mkdir (path)
  assert(vim.uv.fs_mkdir(path, 448)) -- 0700
end

local function write_private (path, bytes)
  local descriptor = assert(vim.uv.fs_open(path, 'wx', 384))
  local offset = 0
  while offset < #bytes do
    local written = assert(vim.uv.fs_write(
      descriptor, bytes:sub(offset + 1), offset))
    assert.is_true(written > 0)
    offset = offset + written
  end
  assert(vim.uv.fs_fsync(descriptor))
  assert(vim.uv.fs_close(descriptor))
end

local function read_bytes (path)
  local handle = assert(io.open(path, 'rb'))
  local result = handle:read('*a')
  assert(handle:close())
  return result
end

local function raw_text (buf)
  return registry.raw_text(buf)
end

local function install_text_without_undo (buf, text)
  vim.bo[buf].undolevels = -1
  local eol = text:sub(-1) == '\n'
  local body = eol and text:sub(1, -2) or text
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
    vim.split(body, '\n', { plain = true, trimempty = false }))
  vim.bo[buf].endofline = eol
  vim.bo[buf].undolevels = 1000
  vim.bo[buf].modified = false
end

local function fixture ()
  local directory = vim.fn.tempname()
  mkdir(directory)
  mkdir(directory .. '/owned')
  local config_path = directory .. '/skgconfig.toml'
  write_private(config_path, table.concat({
    'port = 1',
    'maintenance_archive_folder = "archive"',
    '',
    '[[sources]]',
    'name = "mine"',
    'path = "owned"',
    '',
  }, '\n'))
  local config = require('skg.config')
  config.config_file_path = config_path
  local skg_state = require('skg.state')
  skg_state.maintenance_archive_folder = 'archive'
  skg_state.maintenance_archive_identity = '/server/mounted/archive'

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buf, 'skg://same title/' .. vim.fs.basename(directory))
  local base = '* Root λ\nbase\n'
  install_text_without_undo(buf, base)
  registry.register(buf, 'content-view', {
    last_fetched = base,
    root_ids = { 'root-a', 'root-b' },
    recipe = { kind = 'single-root', requested = { 'root-a', 'root-b' } },
    graph_generation = 7,
    presentation_generation = 3,
    server_revision = 11,
    application_token = 5,
  })
  vim.b[buf].skg_view_uri = 'view:archive-fixture'
  vim.api.nvim_buf_set_lines(buf, 2, 2, false,
    { 'unsaved café 🐙', 'last line' })
  -- `nofile' scratch buffers never set 'modified'; use the registry's
  -- equivalent logical-dirty bit just as attached workflows do.
  vim.b[buf].skg_logical_dirty = true
  registry.lock_for_maintenance(buf, 4)

  return {
    directory = directory,
    config_path = config_path,
    archive_root = directory .. '/archive',
    buf = buf,
    offer = {
      incident_id = incident_id,
      epoch = 4,
      origin = 'explicit-partial-reload',
      started_at_utc = '2026-09-04T12:34:56.123456Z',
      archive_name = '20260904T123456.123456Z_' .. incident_id,
      source_set = 'all',
      graph_generation = 7,
      manifest_revision = 9,
    },
  }
end

local function cleanup (value)
  if value and value.buf and vim.api.nvim_buf_is_valid(value.buf) then
    vim.bo[value.buf].modifiable = true
    vim.bo[value.buf].modified = false
    pcall(vim.api.nvim_buf_delete, value.buf, { force = true }) end
  if value and value.directory then vim.fn.delete(value.directory, 'rf') end
  require('skg.config').config_file_path = nil
  require('skg.state').maintenance_archive_folder = nil
  require('skg.state').maintenance_archive_identity = nil
end

describe('skg recovery archive', function ()
  it('uses a deterministic, cross-client canonical S-expression', function ()
    assert.are.equal(
      '((archive-format-version 1) (name "a\\n\\"b\\\\c") (empty ()))',
      archive.canonical_sexpr({
        { sexpr.symbol('archive-format-version'), 1 },
        { sexpr.symbol('name'), 'a\n"b\\c' },
        { sexpr.symbol('empty'), {} },
      }))
  end)

  it('publishes exact private artifacts and a checksum-bearing marker',
     function ()
    local value = fixture()
    local before = raw_text(value.buf)
    local ok, result = xpcall(function ()
      return archive.publish_initial(value.offer, { value.buf }, {
        client_nonce = '0123456789abcdef01234567',
      })
    end, debug.traceback)
    if ok then
      local final = result.path
      assert.are.equal(448, vim.uv.fs_lstat(value.archive_root).mode % 512)
      assert.are.equal(448, vim.uv.fs_lstat(final).mode % 512)
      assert.are.equal(before, raw_text(value.buf))
      assert.is_false(vim.bo[value.buf].modifiable)
      local scanner = assert(vim.uv.fs_scandir(final .. '/buffer-snapshots'))
      local buffer_key, kind = vim.uv.fs_scandir_next(scanner)
      assert.is_truthy(buffer_key:match('^view%-1_%x%x%x%x%x%x%x%x%x%x%x%x$'))
      assert.are.equal('directory', kind)
      assert.are.equal(before, read_bytes(final ..
        '/buffer-snapshots/' .. buffer_key .. '/unsaved-changes.org'))

      local manifest_text = read_bytes(final .. '/manifest.initial.sexp')
      assert.are.equal(result.manifest_sha256, vim.fn.sha256(manifest_text))
      local manifest = sexpr.read(manifest_text)
      assert.are.equal('initial', payload.field_text(manifest, 'manifest-kind'))
      assert.are.equal(7, payload.field(manifest, 'g0-graph-generation'))
      assert.are.equal(1, #payload.field(manifest, 'buffers'))
      local marker = sexpr.read(read_bytes(final .. '/ARCHIVE-READY'))
      assert.are.equal(incident_id, payload.field_text(marker, 'incident-id'))
      assert.are.equal(result.manifest_sha256,
        payload.field_text(marker, 'manifest-sha256'))
      assert.is_true(result.sizes.incident_bytes > 0)
      assert.are.equal(1, result.sizes.retained_count)
    end
    cleanup(value)
    assert(ok, result)
  end)

  it('refuses a client-visible archive root overlapping a source', function ()
    local value = fixture()
    require('skg.state').maintenance_archive_folder = 'owned/archive'
    local ok, error_text = pcall(archive.resolve_archive_root)
    cleanup(value)
    assert.is_false(ok)
    assert.is_truthy(tostring(error_text):find('overlap', 1, true))
  end)

  it('leaves an inspectable partial staging tree on a write failure',
     function ()
    local value = fixture()
    local before = raw_text(value.buf)
    local real_write = archive.fs_write
    local calls = 0
    archive.fs_write = function (...)
      calls = calls + 1
      if calls == 2 then return nil, 'simulated disk full' end
      return real_write(...)
    end
    local ok, error_text = pcall(archive.publish_initial,
      value.offer, { value.buf }, {
        client_nonce = 'fedcba9876543210fedcba98',
      })
    archive.fs_write = real_write
    if not ok then
      assert.is_truthy(tostring(error_text):find('simulated disk full', 1, true))
      assert.is_nil(vim.uv.fs_lstat(
        value.archive_root .. '/' .. value.offer.archive_name))
      assert.are.equal('directory', vim.uv.fs_lstat(
        value.archive_root .. '/.staging/' .. incident_id ..
        '.fedcba9876543210fedcba98.partial').type)
      assert.are.equal(before, raw_text(value.buf))
    end
    cleanup(value)
    assert.is_false(ok)
  end)
end)
