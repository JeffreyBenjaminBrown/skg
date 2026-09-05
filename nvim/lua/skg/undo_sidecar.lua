-- Native Neovim undo archive adapter.
--
-- The undo file is deliberately opaque.  We ask Neovim to write and read it,
-- then validate that the recovered tree has the same shape and can make a
-- real round trip through its current branch (and an alternate branch when
-- one exists).

local M = {}

local function fail (message)
  error('skg native undo archive failed: ' .. message, 0)
end

local function absolute (path)
  return vim.fs.normalize(vim.fn.fnamemodify(path, ':p'))
end

local function assert_directory (path, description)
  local stat = vim.uv.fs_lstat(path)
  if not stat or stat.type ~= 'directory' then
    fail(description .. ' is not a directory: ' .. path) end
  if stat.mode % 512 ~= 448 then
    fail(description .. ' is not private (expected mode 0700): ' .. path) end
end

local function assert_confined (path, incident_staging)
  local root = absolute(incident_staging)
  local target = absolute(path)
  assert_directory(root, 'incident staging')
  if target:sub(1, #root + 1) ~= root .. '/' then
    fail('path escapes incident staging: ' .. target) end

  -- Reject every pre-existing symlink component below the already validated
  -- private incident directory.  The archive writer owns this whole chain.
  local parent = vim.fs.dirname(target)
  local relative = parent:sub(#root + 2)
  local cursor = root
  for component in relative:gmatch('[^/]+') do
    cursor = cursor .. '/' .. component
    assert_directory(cursor, 'archive path component')
  end
  return target
end

local function read_regular_file (path, description)
  local stat = vim.uv.fs_lstat(path)
  if not stat or stat.type ~= 'file' then
    fail(description .. ' is not a regular file: ' .. path) end
  local handle, open_error = io.open(path, 'rb')
  if not handle then fail(description .. ' cannot be read: ' .. open_error) end
  local bytes = handle:read('*a')
  local ok, close_error = handle:close()
  if not ok then fail(description .. ' cannot be closed: ' .. close_error) end
  return bytes, stat
end

local function raw_text (buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local text = table.concat(lines, '\n')
  if vim.bo[buf].endofline then text = text .. '\n' end
  return text
end

local function install_raw_text (buf, text)
  local endofline = text:sub(-1) == '\n'
  local body = endofline and text:sub(1, -2) or text
  local lines = vim.split(body, '\n', { plain = true, trimempty = false })
  if #lines == 0 then lines = { '' } end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].endofline = endofline
end

local function entry_shape (entry)
  local result = { seq = entry.seq }
  for _, key in ipairs({ 'newhead', 'curhead', 'save' }) do
    if entry[key] ~= nil then result[key] = entry[key] end end
  if entry.alt then
    result.alt = {}
    for _, alternative in ipairs(entry.alt) do
      table.insert(result.alt, entry_shape(alternative)) end
  end
  return result
end

local function tree_shape (tree)
  local result = {
    seq_cur = tree.seq_cur,
    seq_last = tree.seq_last,
    save_cur = tree.save_cur,
    save_last = tree.save_last,
    entries = {},
  }
  for _, entry in ipairs(tree.entries or {}) do
    table.insert(result.entries, entry_shape(entry)) end
  return result
end

local function first_alternate_sequence (entries)
  for _, entry in ipairs(entries or {}) do
    if entry.alt and entry.alt[1] then return entry.alt[1].seq end
    local nested = first_alternate_sequence(entry.alt)
    if nested then return nested end
  end
  return nil
end

local function buffer_undotree (buf)
  return vim.api.nvim_buf_call(buf, function () return vim.fn.undotree() end)
end

local function command_in_buffer (buf, command)
  local ok, result = pcall(vim.api.nvim_buf_call, buf, function ()
    vim.cmd(command)
  end)
  if not ok then fail(tostring(result)) end
end

local function verify_sidecar (text, sidecar, expected_tree)
  local probe = vim.api.nvim_create_buf(false, true)
  local ok, result = xpcall(function ()
    vim.bo[probe].bufhidden = 'wipe'
    vim.bo[probe].swapfile = false
    vim.bo[probe].undolevels = -1
    install_raw_text(probe, text)
    vim.bo[probe].undolevels = vim.o.undolevels
    vim.bo[probe].filetype = 'org'
    vim.bo[probe].modified = false

    command_in_buffer(
      probe, 'silent rundo ' .. vim.fn.fnameescape(sidecar))
    local recovered_tree = buffer_undotree(probe)
    if not vim.deep_equal(tree_shape(expected_tree),
                          tree_shape(recovered_tree)) then
      fail('recovered undo tree differs from the archived tree') end

    local before = raw_text(probe)
    if recovered_tree.seq_cur > 0 then
      command_in_buffer(probe, 'silent undo')
      command_in_buffer(probe, 'silent redo')
    else
      command_in_buffer(probe, 'silent redo')
      command_in_buffer(probe, 'silent undo')
    end
    if raw_text(probe) ~= before then
      fail('undo/redo probe did not restore exact text') end

    local alternative = first_alternate_sequence(recovered_tree.entries)
    if alternative and recovered_tree.seq_cur > 0
       and alternative ~= recovered_tree.seq_cur then
      command_in_buffer(probe, 'silent undo ' .. tostring(alternative))
      command_in_buffer(
        probe, 'silent undo ' .. tostring(recovered_tree.seq_cur))
      if raw_text(probe) ~= before then
        fail('alternate-branch probe did not restore exact text') end
      return 'undo-redo-and-branch-probed'
    end
    return 'undo-redo-probed'
  end, debug.traceback)
  if vim.api.nvim_buf_is_valid(probe) then
    vim.bo[probe].modified = false
    vim.api.nvim_buf_delete(probe, { force = true })
  end
  if not ok then fail(result) end
  return result
end

local function hex_prefix (bytes, length)
  local result = {}
  for index = 1, math.min(length, #bytes) do
    table.insert(result, string.format('%02x', bytes:byte(index))) end
  return table.concat(result)
end

local function version_string ()
  local version = vim.version()
  return string.format('%d.%d.%d', version.major, version.minor, version.patch)
end

---Write BUF's native undo history and immediately verify a native reload.
---CURRENT_TEXT_PATH must already contain the exact archived buffer bytes.
---@param buf integer
---@param current_text_path string
---@param sidecar_path string
---@param incident_staging string
---@return table
function M.save (buf, current_text_path, sidecar_path, incident_staging)
  if not vim.api.nvim_buf_is_valid(buf)
     or not vim.api.nvim_buf_is_loaded(buf) then
    fail('source buffer is not live') end

  local current_path = assert_confined(current_text_path, incident_staging)
  local sidecar = assert_confined(sidecar_path, incident_staging)
  local archived_text = read_regular_file(current_path, 'current-text artifact')
  local live_text = raw_text(buf)
  if live_text ~= archived_text then
    fail('current buffer text differs from its archive artifact') end
  if vim.uv.fs_lstat(sidecar) then
    fail('undo sidecar already exists: ' .. sidecar) end

  local original_tree = buffer_undotree(buf)
  if #(original_tree.entries or {}) == 0 then
    return {
      status = 'empty', kind = 'nvim-wundo', version = version_string(),
    }
  end

  command_in_buffer(buf,
    'silent wundo! ' .. vim.fn.fnameescape(sidecar))
  local bytes, stat = read_regular_file(sidecar, 'native undo sidecar')
  if #bytes == 0 then fail('native undo sidecar is empty') end
  if stat.mode % 512 ~= 384 then
    fail('native undo sidecar is not private (expected mode 0600)') end

  -- `:wundo' synchronizes the in-memory tree; compare recovery with that
  -- post-write state rather than with the pre-write `synced' flag.
  local written_tree = buffer_undotree(buf)
  local validation = verify_sidecar(archived_text, sidecar, written_tree)
  return {
    status = 'archived',
    kind = 'nvim-wundo',
    version = version_string(),
    header_identity = hex_prefix(bytes, 9),
    validation = validation,
    bytes = #bytes,
    sha256 = vim.fn.sha256(bytes),
    path = sidecar,
  }
end

---Restore an already verified native sidecar into BUF without mutating either
---archive artifact.  EXPECTED_VERSION must match this exact Neovim build.
---@param buf integer
---@param current_text_path string
---@param sidecar_path string
---@param expected_version string|nil
---@return string validation
function M.restore (buf, current_text_path, sidecar_path, expected_version)
  if not vim.api.nvim_buf_is_valid(buf)
     or not vim.api.nvim_buf_is_loaded(buf) then
    fail('recovery buffer is not live') end
  if expected_version and expected_version ~= version_string() then
    fail('unsupported archived undo version ' .. expected_version) end
  local archived_text, text_stat = read_regular_file(
    current_text_path, 'current-text artifact')
  local sidecar_bytes, sidecar_stat = read_regular_file(
    sidecar_path, 'native undo sidecar')
  if text_stat.mode % 512 ~= 384
     or sidecar_stat.mode % 512 ~= 384
     or (text_stat.nlink and text_stat.nlink ~= 1)
     or (sidecar_stat.nlink and sidecar_stat.nlink ~= 1) then
    fail('recovery artifacts are not private single-link files') end
  if raw_text(buf) ~= archived_text then
    fail('recovery buffer text differs from its archive artifact') end

  command_in_buffer(
    buf, 'silent rundo ' .. vim.fn.fnameescape(sidecar_path))
  local recovered_tree = buffer_undotree(buf)
  local validation = verify_sidecar(
    archived_text, sidecar_path, recovered_tree)
  if raw_text(buf) ~= archived_text
     or read_regular_file(current_text_path, 'current-text artifact')
       ~= archived_text
     or read_regular_file(sidecar_path, 'native undo sidecar')
       ~= sidecar_bytes then
    fail('native undo restore changed archived or live text') end
  return validation
end

-- Exposed only to let fixture tests compare a native recovery without
-- depending on volatile undo timestamps or the synchronization flag.
M._tree_shape = tree_shape

return M
