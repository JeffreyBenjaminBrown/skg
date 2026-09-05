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

local function replace_bytes (path, bytes)
  local handle = assert(io.open(path, 'wb'))
  assert(handle:write(bytes))
  assert(handle:flush())
  assert(handle:close())
end

local function set_field (record, name, value)
  for _, field in ipairs(record) do
    if tostring(field[1]) == name then
      field[2] = value
      return
    end
  end
  error('missing machine-record field ' .. name)
end

local function retag_client_kind (incident_root, client_kind)
  local initial_path = incident_root .. '/manifest.initial.sexp'
  local initial = sexpr.read(read_bytes(initial_path))
  set_field(initial, 'client-kind', client_kind)
  local initial_bytes = archive.canonical_sexpr(initial) .. '\n'
  replace_bytes(initial_path, initial_bytes)
  local initial_sha = vim.fn.sha256(initial_bytes)

  local ready_path = incident_root .. '/ARCHIVE-READY'
  local ready = sexpr.read(read_bytes(ready_path))
  set_field(ready, 'manifest-sha256', initial_sha)
  replace_bytes(ready_path, archive.canonical_sexpr(ready) .. '\n')

  local final_path = incident_root .. '/manifest.final.sexp'
  local final = sexpr.read(read_bytes(final_path))
  set_field(final, 'initial-manifest-sha256', initial_sha)
  set_field(final, 'client-kind', client_kind)
  local final_bytes = archive.canonical_sexpr(final) .. '\n'
  replace_bytes(final_path, final_bytes)

  local finalized_path = incident_root .. '/FINALIZED'
  local finalized = sexpr.read(read_bytes(finalized_path))
  set_field(finalized, 'manifest-sha256', vim.fn.sha256(final_bytes))
  replace_bytes(finalized_path, archive.canonical_sexpr(finalized) .. '\n')
  return archive.inspect(incident_root)
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

local function f (name, value)
  return { sexpr.symbol(name), value }
end

local function final_bundle (initial)
  local manifest = sexpr.read(read_bytes(
    initial.path .. '/manifest.initial.sexp'))
  local buffer_record = payload.field(manifest, 'buffers')[1]
  local buffer_id = payload.field_text(buffer_record, 'buffer-id')
  local buffer_key = payload.field_text(buffer_record, 'buffer-key')
  local readme = '* Modified node niño\n'
  local raw = string.char(0, 255, 254, 195, 40, 10)
  local opaque = readme .. raw
  local node_root = 'modified-nodes/node-00000000-deadbeefcafe'
  local raw_relative = node_root
    .. '/raw/path-00000000-012345abcdef.after.skg'
  local records = {
    {
      f('artifact-key', 'artifact-00000000'),
      f('relative-path', node_root .. '/README.org'),
      f('purpose', 'node-readme'),
      f('byte-offset', 0),
      f('byte-length', #readme),
      f('sha256', vim.fn.sha256(readme)),
    },
    {
      f('artifact-key', 'artifact-00000001'),
      f('relative-path', raw_relative),
      f('purpose', 'raw-after'),
      f('byte-offset', #readme),
      f('byte-length', #raw),
      f('sha256', vim.fn.sha256(raw)),
    },
  }
  return {
    descriptor = {
      f('artifact-bundle-format-version', 1),
      f('incident-id', incident_id),
      f('maintenance-epoch', 4),
      f('candidate-id', 'abcdefab-1234-4234-8234-abcdefabcdef'),
      f('g0-graph-generation', 7),
      f('g0-manifest-revision', 9),
      f('g1-graph-generation', 8),
      f('g1-manifest-revision', 10),
      f('tantivy-generation', 12),
      f('server-evidence-sha256', string.rep('a', 64)),
      f('transfer-manifest-sha256', string.rep('b', 64)),
      f('artifact-bytes-sha256', vim.fn.sha256(opaque)),
      f('artifact-count', 2),
      f('artifact-bytes', #opaque),
      f('artifacts', records),
    },
    opaque = opaque,
    settlements = {
      {
        f('buffer-id', buffer_id),
        f('buffer-key', buffer_key),
        f('kind', 'content-view'),
        f('view-uri', 'view:archive-fixture'),
        f('dirty', 'true'),
        f('impacted', 'true'),
        f('parse-uncertain', 'nil'),
        f('observed-ids', { 'root-a' }),
        f('resolved-primary-ids', { 'root-a' }),
        f('base-graph-generation', 7),
        f('base-presentation-generation', 3),
        f('base-server-revision', 11),
        f('base-application-token', 5),
        f('planned-disposition', 'interrupted'),
        f('required-ack', 'retirement-ack'),
      },
      {
        f('buffer-id', 'rendered-buffer'),
        f('buffer-key', 'none'),
        f('kind', 'content-view'),
        f('view-uri', 'view:rendered-fixture'),
        f('dirty', 'nil'),
        f('impacted', 'true'),
        f('parse-uncertain', 'nil'),
        f('observed-ids', {}),
        f('resolved-primary-ids', {}),
        f('base-graph-generation', 7),
        f('base-presentation-generation', 3),
        f('base-server-revision', 12),
        f('base-application-token', 6),
        f('planned-disposition', 'refreshed'),
        f('required-ack', 'application-ack'),
        f('application', {
          f('content', 'private rendered text'),
          f('content-sha256', string.rep('c', 64)),
          f('resulting-graph-generation', 8),
          f('resulting-presentation-generation', 4),
          f('resulting-server-revision', 13),
          f('resulting-application-token', 7),
        }),
      },
    },
    raw = raw,
    raw_relative = raw_relative,
  }
end

local function finalized_fixture (value)
  local text = raw_text(value.buf)
  local initial = archive.publish_initial(value.offer, { value.buf }, {
    client_nonce = '0123456789abcdef01234567',
  })
  local bundle = final_bundle(initial)
  archive.finalize(
    initial, bundle.descriptor, bundle.opaque, bundle.settlements)
  local summary = archive.inspect(initial.path)
  local record = payload.field(summary.initial, 'buffers')[1]
  return {
    initial = initial, summary = summary, text = text,
    buffer_key = payload.field_text(record, 'buffer-key'),
  }
end

local function delete_buffer (buf)
  if buf and vim.api.nvim_buf_is_valid(buf) then
    vim.bo[buf].modifiable = true
    vim.bo[buf].modified = false
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end
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

  it('finalizes opaque evidence replay-safely with FINALIZED last',
     function ()
    local value = fixture()
    local ok, error_text = xpcall(function ()
      local initial = archive.publish_initial(value.offer, { value.buf }, {
        client_nonce = '0123456789abcdef01234567',
      })
      local bundle = final_bundle(initial)
      local missing_application = vim.deepcopy(bundle.settlements)
      table.remove(missing_application[2], #missing_application[2])
      local accepted, validation_error = pcall(
        archive.finalize, initial, bundle.descriptor, bundle.opaque,
        missing_application)
      assert.is_false(accepted)
      assert.is_truthy(tostring(validation_error):find(
        'no exact rendered identity', 1, true), tostring(validation_error))
      assert.is_nil(vim.uv.fs_lstat(initial.path .. '/FINALIZED'))
      for _, settlement in ipairs(bundle.settlements) do
        for _, key in ipairs({ 'dirty', 'impacted', 'parse-uncertain' }) do
          for _, field in ipairs(settlement) do
            if payload.field_text({ field }, key) == 'nil' then
              field[2] = sexpr.NIL
            end
          end
        end
      end
      local result = archive.finalize(
        initial, bundle.descriptor, bundle.opaque, bundle.settlements)
      local replayed = archive.finalize(
        initial, bundle.descriptor, bundle.opaque, bundle.settlements)
      local final_bytes = read_bytes(initial.path .. '/manifest.final.sexp')
      local final = sexpr.read(final_bytes)
      assert.are.equal(result.manifest_sha256, vim.fn.sha256(final_bytes))
      assert.are.equal(result.manifest_sha256, replayed.manifest_sha256)
      assert.are.equal(bundle.raw,
        read_bytes(initial.path .. '/' .. bundle.raw_relative))
      assert.are.equal('final', payload.field_text(final, 'manifest-kind'))
      assert.are.equal(2, #payload.field(final, 'node-artifacts'))
      local dispositions = payload.field(final, 'buffer-dispositions')
      assert.are.equal(2, #dispositions)
      local rendered_identity = payload.field(dispositions[2], 'application')
      assert.are.equal(string.rep('c', 64),
        payload.field_text(rendered_identity, 'content-sha256'))
      assert.are.equal(8,
        payload.field(rendered_identity, 'resulting-graph-generation'))
      assert.is_nil(payload.field(rendered_identity, 'content'))
      assert.are.equal('file',
        vim.uv.fs_lstat(initial.path .. '/FINALIZED').type)
      assert.is_truthy(read_bytes(initial.path
        .. '/interrupted-buffers/README.org'):find(
          'buffer%-snapshots/.*/unsaved%-changes.org'))
    end, debug.traceback)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('inspects ready and finalized retained archives without a server',
     function ()
    local value = fixture()
    local ok, error_text = xpcall(function ()
      local initial = archive.publish_initial(value.offer, { value.buf }, {
        client_nonce = '0123456789abcdef01234567',
      })
      local ready = archive.inspect(initial.path)
      assert.are.equal('archive-ready', ready.status)
      assert.are.equal('explicit-partial-reload', ready.origin)
      assert.are.equal(7, ready.g0)
      assert.is_nil(ready.g1)
      assert.are.equal(1, ready.dirty_buffers)
      assert.are.equal(0, ready.changed_nodes)
      assert.is_true(ready.native_undo_compatible)
      assert.is_true(ready.bytes > 0)
      assert.is_truthy(ready.iec)

      local bundle = final_bundle(initial)
      archive.finalize(
        initial, bundle.descriptor, bundle.opaque, bundle.settlements)
      local final = archive.inspect(initial.path)
      assert.are.equal('finalized', final.status)
      assert.are.equal(8, final.g1)
      assert.are.equal(1, final.changed_nodes)
      assert.are.equal(1, final.interrupted_buffers)
      assert.are.equal(0, final.released_buffers)
    end, debug.traceback)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('lists invalid strict-named siblings beside healthy incidents', function ()
    local value = fixture()
    local ok, error_text = xpcall(function ()
      local initial = archive.publish_initial(value.offer, { value.buf }, {
        client_nonce = '0123456789abcdef01234567',
      })
      local invalid_name =
        '20260904T123457.123456Z_abcdefab-1234-4234-8234-abcdefabcdef'
      mkdir(value.archive_root .. '/' .. invalid_name)
      local summaries = archive.list()
      assert.are.equal(2, #summaries)
      local by_name = {}
      for _, summary in ipairs(summaries) do by_name[summary.name] = summary end
      assert.are.equal('invalid', by_name[invalid_name].status)
      assert.are.equal('archive-ready',
        by_name[vim.fs.basename(initial.path)].status)
    end, debug.traceback)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('rejects a corrupt finalized marker during inspection', function ()
    local value = fixture()
    local ok, error_text = xpcall(function ()
      local initial = archive.publish_initial(value.offer, { value.buf }, {
        client_nonce = '0123456789abcdef01234567',
      })
      local bundle = final_bundle(initial)
      archive.finalize(
        initial, bundle.descriptor, bundle.opaque, bundle.settlements)
      replace_bytes(initial.path .. '/FINALIZED',
        '((archive-format-version 1))\n')
      local inspected, inspect_error = pcall(archive.inspect, initial.path)
      assert.is_false(inspected)
      assert.is_truthy(tostring(inspect_error):find('lacks', 1, true))
    end, debug.traceback)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('opens independent authority-free detached recovery buffers', function ()
    local ui = require('skg.recovery_ui')
    local value = fixture()
    local first, second = nil, nil
    local ok, error_text = xpcall(function ()
      local finalized = finalized_fixture(value)
      first = ui.open_interrupted_view(
        finalized.summary, finalized.buffer_key)
      second = ui.open_interrupted_view(
        finalized.summary, finalized.buffer_key)
      assert.is_true(vim.api.nvim_buf_is_valid(first))
      assert.is_true(vim.api.nvim_buf_is_valid(second))
      assert.are_not.equal(first, second)
      assert.are_not.equal(vim.api.nvim_buf_get_name(first),
        vim.api.nvim_buf_get_name(second))
      for _, buf in ipairs({ first, second }) do
        assert.are.equal(finalized.text, raw_text(buf))
        assert.are.equal('acwrite', vim.bo[buf].buftype)
        assert.is_true(vim.b[buf].skg_recovery)
        assert.is_nil(vim.b[buf].skg_view_uri)
        assert.is_nil(registry.record(buf))
        assert.are.equal('native-restored',
          vim.b[buf].skg_recovery_native_undo_status)
        local written = pcall(vim.api.nvim_buf_call, buf, function ()
          vim.cmd('write') end)
        assert.is_false(written)
      end
      vim.api.nvim_buf_set_lines(first, -1, -1, false, { 'edit' })
      assert.are.equal(finalized.text, raw_text(second))
    end, debug.traceback)
    delete_buffer(first)
    delete_buffer(second)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('uses exact text when the native sidecar belongs to Emacs', function ()
    local ui = require('skg.recovery_ui')
    local value = fixture()
    local recovery
    local ok, error_text = xpcall(function ()
      local finalized = finalized_fixture(value)
      finalized.summary = retag_client_kind(
        finalized.summary.path, 'emacs')
      assert.is_false(finalized.summary.native_undo_compatible)
      recovery = ui.open_interrupted_view(
        finalized.summary, finalized.buffer_key)
      assert.are.equal(finalized.text, raw_text(recovery))
      assert.are.equal('text-only-other-client',
        vim.b[recovery].skg_recovery_native_undo_status)
      assert.is_nil(vim.b[recovery].skg_view_uri)
      assert.is_nil(registry.record(recovery))
    end, debug.traceback)
    delete_buffer(recovery)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('refuses corrupt required recovery text before opening a buffer', function ()
    local ui = require('skg.recovery_ui')
    local value = fixture()
    local ok, error_text = xpcall(function ()
      local finalized = finalized_fixture(value)
      local record = payload.field(finalized.summary.initial, 'buffers')[1]
      local current_path
      for _, artifact in ipairs(payload.field(record, 'artifacts')) do
        local relative = payload.field_text(artifact, 'path')
        if vim.endswith(relative, '/unsaved-changes.org') then
          current_path = finalized.summary.path .. '/' .. relative end
      end
      assert.is_truthy(current_path)
      replace_bytes(current_path, 'corrupt\n')
      local opened, open_error = pcall(ui.open_interrupted_view,
        finalized.summary, finalized.buffer_key)
      assert.is_false(opened)
      assert.is_truthy(tostring(open_error):find(
        'exact private recorded artifact', 1, true))
    end, debug.traceback)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('refuses an explicit unknown buffer key without opening a picker', function ()
    local ui = require('skg.recovery_ui')
    local value = fixture()
    local old_picker = ui.buffer_picker
    local picker_called = false
    local ok, error_text = xpcall(function ()
      local finalized = finalized_fixture(value)
      ui.buffer_picker = function () picker_called = true end
      local opened, open_error = pcall(ui.open_interrupted_view,
        finalized.summary, 'unknown-buffer')
      assert.is_false(opened)
      assert.is_truthy(tostring(open_error):find(
        'no interrupted buffer with key unknown-buffer', 1, true))
      assert.is_false(picker_called)
    end, debug.traceback)
    ui.buffer_picker = old_picker
    cleanup(value)
    assert(ok, error_text)
  end)

  it('lists and safely deletes terminal retained incidents', function ()
    local ui = require('skg.recovery_ui')
    local value = fixture()
    local list_buf = nil
    local ok, error_text = xpcall(function ()
      local finalized = finalized_fixture(value)
      list_buf = ui.list_maintenance_incidents()
      assert.is_true(vim.api.nvim_buf_is_valid(list_buf))
      assert.is_truthy(table.concat(
        vim.api.nvim_buf_get_lines(list_buf, 0, -1, false), '\n')
        :find('explicit%-partial%-reload'))
      assert.is_true(ui.delete_maintenance_incident(
        finalized.summary, true))
      assert.is_nil(vim.uv.fs_lstat(finalized.summary.path))
    end, debug.traceback)
    delete_buffer(list_buf)
    cleanup(value)
    assert(ok, error_text)
  end)

  it('refuses deletion while the same incident remains active', function ()
    local ui = require('skg.recovery_ui')
    local skg_state = require('skg.state')
    local value = fixture()
    local old_incident = skg_state.maintenance_client_incident
    local ok, error_text = xpcall(function ()
      local finalized = finalized_fixture(value)
      skg_state.maintenance_client_incident = { incident_id = incident_id }
      local deleted, delete_error = pcall(
        ui.delete_maintenance_incident, finalized.summary, true)
      assert.is_false(deleted)
      assert.is_truthy(tostring(delete_error):find(
        'active maintenance incident', 1, true))
    end, debug.traceback)
    skg_state.maintenance_client_incident = old_incident
    cleanup(value)
    assert(ok, error_text)
  end)

  it('opens a selected recorded root as a fresh live view', function ()
    local ui = require('skg.recovery_ui')
    local content_view = require('skg.content_view')
    local value = fixture()
    local old_picker = ui.root_picker
    local old_request = content_view.request_single_root_content_view_from_id
    local roots, request
    local ok, error_text = xpcall(function ()
      local finalized = finalized_fixture(value)
      ui.root_picker = function (choices, _prompt, callback)
        roots = choices
        return callback(choices[2])
      end
      content_view.request_single_root_content_view_from_id = function (...)
        request = { ... }
      end
      ui.open_fresh_view_for_interrupted(
        finalized.summary, finalized.buffer_key)
      assert.same({ 'root-a', 'root-b' }, roots)
      assert.are.equal('root-b', request[1])
      assert.is_true(request[5])
    end, debug.traceback)
    ui.root_picker = old_picker
    content_view.request_single_root_content_view_from_id = old_request
    cleanup(value)
    assert(ok, error_text)
  end)

  it('reruns an archived search only after confirmation', function ()
    local ui = require('skg.recovery_ui')
    local search = require('skg.search')
    local value = fixture()
    local old_confirm = ui.confirm
    local old_request = search.request_text_search
    local request
    local ok, error_text = xpcall(function ()
      vim.b[value.buf].skg_buffer_kind = 'search-view'
      vim.b[value.buf].skg_recipe = {
        kind = 'search', terms = 'octopus', regex = true,
        body = false, operators = true,
      }
      local finalized = finalized_fixture(value)
      ui.confirm = function () return true end
      search.request_text_search = function (...)
        request = { ... }
      end
      ui.open_fresh_view_for_interrupted(
        finalized.summary, finalized.buffer_key)
      assert.are.equal('octopus', request[1])
      assert.is_true(request[2])
      assert.is_false(request[3])
      assert.is_true(request[4])
      assert.is_truthy(request[6]:find('^search:recovery:'))
    end, debug.traceback)
    ui.confirm = old_confirm
    search.request_text_search = old_request
    cleanup(value)
    assert(ok, error_text)
  end)

  it('refuses changed opaque final evidence before FINALIZED', function ()
    local value = fixture()
    local ok, error_text = xpcall(function ()
      local initial = archive.publish_initial(value.offer, { value.buf }, {
        client_nonce = 'fedcba9876543210fedcba98',
      })
      local bundle = final_bundle(initial)
      local changed = bundle.opaque:sub(1, -2) .. 'c'
      local finalized, final_error = pcall(
        archive.finalize, initial, bundle.descriptor, changed,
        bundle.settlements)
      assert.is_false(finalized)
      assert.is_truthy(tostring(final_error):find('checksum', 1, true))
      assert.is_nil(vim.uv.fs_lstat(initial.path .. '/FINALIZED'))
    end, debug.traceback)
    cleanup(value)
    assert(ok, error_text)
  end)
end)
