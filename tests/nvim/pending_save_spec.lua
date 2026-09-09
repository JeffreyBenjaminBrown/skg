local config = require('skg.config')
local pending = require('skg.pending_save')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

describe('skg.pending_save', function ()
  local temporary

  before_each(function ()
    temporary = vim.fn.tempname()
    vim.fn.mkdir(temporary, 'p')
    config.config_file_path = temporary .. '/skgconfig.toml'
    vim.fn.writefile({ 'port = 1731' }, config.config_file_path)
    state.maintenance_archive_folder = 'archive'
  end)

  after_each(function ()
    config.config_file_path = nil
    state.maintenance_archive_folder = nil
    vim.fn.delete(temporary, 'rf')
  end)

  local function prepare_record ()
    local operation_id = '11111111-2222-4333-8444-555555555555'
    local intent = '((request . "save buffer") (operation-id . "'
      .. operation_id .. '"))'
    local content = '* α\nbody\n'
    local fingerprint = pending.fingerprint(intent, content)
    local request = intent:sub(1, -2)
      .. ' (request-base-fingerprint . "' .. fingerprint .. '"))\n'
    return pending.prepare({
      operation_id = operation_id,
      request_base_fingerprint = fingerprint,
      request = request,
      content = content,
      buffer_id = 'buffer-1',
    })
  end

  it('excludes the framing newline from the fingerprint', function ()
    local intent = '((request . "save buffer") (operation-id . '
      .. '"11111111-2222-4333-8444-555555555555"))'
    assert.are.equal(
      '6a4052d58590c7178c73c389316be38f59c78c4574272a2907f98dac00e363e6',
      pending.fingerprint(intent, '* α\nbody\n'))
  end)

  it('persists exact private bytes and reloads them', function ()
    local record = prepare_record()
    local reloaded = pending.records()[1]
    assert.are.equal(pending.field_text(record, 'request'),
                     pending.field_text(reloaded, 'request'))
    assert.are.equal('* α\nbody\n', pending.field_text(reloaded, 'content'))
    local archive = require('skg.recovery_archive').resolve_archive_root()
    local path = archive .. '/pending-saves/operation-'
      .. pending.field_text(record, 'operation-id') .. '.sexp'
    assert.are.equal(384, vim.uv.fs_lstat(path).mode % 512)
    assert.are.equal(448, vim.uv.fs_lstat(archive .. '/pending-saves').mode % 512)
  end)

  it('retries exact material without allocating a new identity', function ()
    local record = prepare_record()
    local operation_id = pending.field_text(record, 'operation-id')
    local fingerprint = pending.field_text(record, 'request-base-fingerprint')
    local response = {
      { sexpr.symbol('operation-id'), operation_id },
      { sexpr.symbol('request-base-fingerprint'), fingerprint },
      { sexpr.symbol('state'), sexpr.symbol('unknown') },
    }
    local updated = pending.apply_status(record, response)
    local request, content = pending.retry_material(updated)
    assert.is_truthy(request:find(operation_id, 1, true))
    assert.are.equal('* α\nbody\n', content)
    assert.are.equal(1, #pending.records())
  end)

  it('blocks a duplicate save without changing edited text', function ()
    prepare_record()
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'locally edited' })
    local before = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local ok = pcall(pending.assert_none_unresolved)
    assert.is_false(ok)
    assert.are.same(before, vim.api.nvim_buf_get_lines(buf, 0, -1, false))
  end)

  it('compacts only after a terminal result is durable', function ()
    local record = prepare_record()
    assert.is_false(pcall(pending.mark_acknowledged, record))
    pending.mark_terminal(record, '((response-type save-result))', false)
    local compact = pending.mark_acknowledged(record)
    assert.are.equal('acknowledged', pending.field_text(compact, 'state'))
    assert.is_nil(pending.field_text(compact, 'request'))
    assert.is_nil(pending.field_text(compact, 'content'))
    assert.are.equal(0, #pending.unresolved_records())
  end)

  it('marks restart recovery as requiring a fresh view', function ()
    local record = prepare_record()
    local operation_id = pending.field_text(record, 'operation-id')
    local fingerprint = pending.field_text(record, 'request-base-fingerprint')
    local terminal = '((response-type save-result)'
      .. ' (save-operation-state committed)'
      .. ' (recovered-after-restart true) (requires-fresh-view true)'
      .. ' (content ""))'
    local response = {
      { sexpr.symbol('operation-id'), operation_id },
      { sexpr.symbol('request-base-fingerprint'), fingerprint },
      { sexpr.symbol('state'), sexpr.symbol('committed') },
      { sexpr.symbol('terminal-response'), terminal },
    }
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'unsaved newer edit' })
    local before = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local updated = pending.apply_status(record, response)
    assert.are.same(before, vim.api.nvim_buf_get_lines(buf, 0, -1, false))
    assert.are.equal('true',
                     pending.field_text(updated, 'fresh-view-required'))
  end)
end)
