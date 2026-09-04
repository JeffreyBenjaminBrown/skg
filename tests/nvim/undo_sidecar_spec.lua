local undo_sidecar = require('skg.undo_sidecar')

local function private_directory ()
  local path = vim.fn.tempname()
  assert(vim.uv.fs_mkdir(path, 448)) -- 0700
  return path
end

local function write_private (path, bytes)
  local descriptor = assert(vim.uv.fs_open(path, 'w', 384)) -- 0600
  local written = assert(vim.uv.fs_write(descriptor, bytes, 0))
  assert.are.equal(#bytes, written)
  assert(vim.uv.fs_fsync(descriptor))
  assert(vim.uv.fs_close(descriptor))
end

local function raw_text (buf)
  local text = table.concat(
    vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
  if vim.bo[buf].endofline then text = text .. '\n' end
  return text
end

local function new_buffer_with_base (lines)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].undolevels = -1
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].endofline = true
  vim.bo[buf].undolevels = 1000
  vim.bo[buf].modified = false
  return buf
end

local function cleanup (buf, directory)
  if buf and vim.api.nvim_buf_is_valid(buf) then
    vim.bo[buf].modified = false
    vim.api.nvim_buf_delete(buf, { force = true }) end
  if directory then vim.fn.delete(directory, 'rf') end
end

describe('skg native Neovim undo sidecar', function ()
  it('round-trips a branched native undo tree', function ()
    local directory = private_directory()
    local current_path = directory .. '/unsaved-changes.org'
    local sidecar = directory .. '/undo.nvim'
    local buf = new_buffer_with_base({ '* Root λ', 'base' })
    local probe = nil
    local ok, err = xpcall(function ()
      vim.api.nvim_buf_set_lines(buf, 2, 2, false, { 'branch café' })
      vim.api.nvim_buf_call(buf, function () vim.cmd('silent undo') end)
      vim.api.nvim_buf_set_lines(buf, 2, 2, false, { 'branch 🐙' })
      local expected_tree = vim.api.nvim_buf_call(
        buf, function () return vim.fn.undotree() end)
      assert.is_truthy(expected_tree.entries[1].alt)
      write_private(current_path, raw_text(buf))

      local result = undo_sidecar.save(
        buf, current_path, sidecar, directory)
      assert.are.equal('archived', result.status)
      assert.are.equal('nvim-wundo', result.kind)
      assert.are.equal('undo-redo-and-branch-probed', result.validation)
      assert.is_truthy(result.header_identity:match('^%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x%x$'))
      assert.is_true(result.bytes > 0)
      assert.are.equal(384, vim.uv.fs_lstat(sidecar).mode % 512)

      probe = new_buffer_with_base(
        vim.api.nvim_buf_get_lines(buf, 0, -1, false))
      vim.api.nvim_buf_call(probe, function ()
        vim.cmd('silent rundo ' .. vim.fn.fnameescape(sidecar)) end)
      local recovered_tree = vim.api.nvim_buf_call(
        probe, function () return vim.fn.undotree() end)
      assert.same(undo_sidecar._tree_shape(
                    vim.api.nvim_buf_call(
                      buf, function () return vim.fn.undotree() end)),
                  undo_sidecar._tree_shape(recovered_tree))
      local before = raw_text(probe)
      vim.api.nvim_buf_call(probe, function ()
        vim.cmd('silent undo')
        vim.cmd('silent redo')
      end)
      assert.are.equal(before, raw_text(probe))
    end, debug.traceback)
    cleanup(probe)
    cleanup(buf, directory)
    assert(ok, err)
  end)

  it('records empty history without inventing a sidecar', function ()
    local directory = private_directory()
    local current_path = directory .. '/unsaved-changes.org'
    local sidecar = directory .. '/undo.nvim'
    local buf = new_buffer_with_base({ 'unchanged' })
    local ok, err = xpcall(function ()
      write_private(current_path, raw_text(buf))
      local result = undo_sidecar.save(
        buf, current_path, sidecar, directory)
      assert.are.equal('empty', result.status)
      assert.is_nil(vim.uv.fs_lstat(sidecar))
    end, debug.traceback)
    cleanup(buf, directory)
    assert(ok, err)
  end)

  it('refuses to bind undo history to different current text', function ()
    local directory = private_directory()
    local current_path = directory .. '/unsaved-changes.org'
    local sidecar = directory .. '/undo.nvim'
    local buf = new_buffer_with_base({ 'one' })
    local ok, err = xpcall(function ()
      vim.api.nvim_buf_set_lines(buf, 1, 1, false, { 'edited' })
      write_private(current_path, 'different\n')
      assert.has_error(function ()
        undo_sidecar.save(buf, current_path, sidecar, directory)
      end, 'skg native undo archive failed: current buffer text differs from its archive artifact')
    end, debug.traceback)
    cleanup(buf, directory)
    assert(ok, err)
  end)
end)
