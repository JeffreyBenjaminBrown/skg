-- PURPOSE: Detached, lossless recovery documents for dirty skg views.

local buffer = require('skg.buffer')

local M = {}

---@param baseline string
---@param current string
---@return string
function M.readable_diff (baseline, current)
  local diff = vim.diff(baseline, current, {
    result_type = 'unified',
    ctxlen = 3,
  })
  if diff == '' then return 'No textual changes.\n' end
  return diff:sub(-1) == '\n' and diff or (diff .. '\n')
end

---@param buf integer
---@return string
function M.document (buf)
  local baseline = vim.b[buf].skg_clean_baseline
  local baseline_available = type(baseline) == 'string'
  local current = buffer.text(buf)
  local enrichment_note =
    vim.b[buf].skg_search_enrichment_includes_user_edits == true
    and ('current text includes both unsaved edits and enrichment presentation;'
         .. ' the diff is not a semantic patch of user intent alone')
    or 'no dirty enrichment recorded'
  return table.concat({
    '#+TITLE: Skg unsaved changes recovery',
    '',
    '* Recovery instructions',
    'This is a detached local archive, not a live Skg view. Close archived'
      .. ' views, save the remaining live view, reopen fresh views, and manually'
      .. ' reapply the intended edits. Do not save this entire stale snapshot'
      .. ' over newer graph data.',
    '',
    '* Context',
    '- Buffer name: ' .. vim.api.nvim_buf_get_name(buf),
    '- View URI: ' .. tostring(vim.b[buf].skg_view_uri),
    '- Captured UTC: ' .. os.date('!%Y-%m-%dT%H:%M:%SZ'),
    '- Source-set and git diff mode: unavailable to this client',
    '- Search enrichment: ' .. enrichment_note,
    baseline_available
      and '- Baseline: verified client clean baseline'
      or ('- Baseline: unavailable (the current text is still preserved,'
          .. ' but no verified edit diff can be produced)'),
    '',
    '* Exact snapshots',
    'Each source block is one JSON string. JSON decoding reproduces every'
      .. ' character, including tabs and trailing newlines.',
    '',
    '** Clean baseline',
    baseline_available
      and ('#+begin_src json\n' .. vim.json.encode(baseline)
           .. '\n#+end_src')
      or 'Unavailable.',
    '',
    '** Current text',
    '#+begin_src json',
    vim.json.encode(current),
    '#+end_src',
    '',
    '* Readable unified diff',
    baseline_available
      and ('#+begin_src diff\n' .. M.readable_diff(baseline, current)
           .. '#+end_src')
      or 'Unavailable because the clean baseline was not captured.',
    '',
  }, '\n')
end

---@param path string|nil
---@param overwrite boolean|nil
---@return string|nil
function M.show_unsaved_changes (path, overwrite)
  local source_buf = vim.api.nvim_get_current_buf()
  if not buffer.buffer_p(source_buf)
     or vim.b[source_buf].skg_view_uri == nil then
    error('This buffer is not a live skg view') end
  path = path or vim.fn.input(
    'Write unsaved-changes recovery document: ', '', 'file')
  if not path or path == '' then return nil end
  local destination = vim.fn.fnamemodify(path, ':p')
  if vim.fn.filereadable(destination) == 1 and not overwrite
     and vim.fn.confirm('Overwrite recovery document ' .. destination .. '?',
                        '&Yes\n&No', 2) ~= 1 then
    error('Recovery document was not written') end
  local document = M.document(source_buf)
  local file, open_error = vim.uv.fs_open(destination, 'w', 420)
  if not file then error('Could not open recovery document: ' .. open_error) end
  local written, write_error = vim.uv.fs_write(file, document, -1)
  local close_ok, close_error = vim.uv.fs_close(file)
  if not written then
    error('Could not write recovery document: ' .. tostring(write_error)) end
  if not close_ok then
    error('Could not close recovery document: ' .. tostring(close_error)) end
  vim.cmd('edit ' .. vim.fn.fnameescape(destination))
  vim.notify('Created detached unsaved-changes recovery document: '
             .. destination)
  return destination
end

return M
