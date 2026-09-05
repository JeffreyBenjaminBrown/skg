-- End-to-end client-owned pull through durable maintenance, Neovim client.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(75)

local pull = require('skg.pull')
local state = require('skg.state')

local function view_of_x ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_content_view
       and T.buffer_text(buf):find('(id x)', 1, true) then
      return buf end
  end
  return nil
end

require('skg.content_view').request_single_root_content_view_from_id('x')
local view = T.wait_for(function ()
  local buf = view_of_x()
  return buf and T.buffer_text(buf):find('title before pull', 1, true)
    and buf or nil
end)
T.check(view ~= nil, 'the pre-pull view loaded')

T.check(pull.pull_all(), 'the pull maintenance request started')
T.check(T.wait_for(function ()
  return vim.api.nvim_buf_is_valid(view)
    and T.buffer_text(view):find('title after pull', 1, true)
    and state.maintenance_client_incident == nil
end, 60), 'pull maintenance returned idle with the same live view updated')

local status = vim.system(
  { 'git', '-C', os.getenv('SKG_PULL_REPO'), 'status', '--porcelain' },
  { text = true }):wait()
T.check(status.code == 0 and status.stdout == '',
        'the pulled worktree is clean')

local diagnostics = false
local buffer_inventory = {}
for _, buf in ipairs(vim.api.nvim_list_bufs()) do
  table.insert(buffer_inventory, string.format('%d:%s:%s:%s', buf,
    tostring(vim.api.nvim_buf_is_valid(buf)),
    tostring(vim.api.nvim_buf_is_loaded(buf)),
    vim.api.nvim_buf_get_name(buf)))
  if vim.api.nvim_buf_is_valid(buf)
     and vim.api.nvim_buf_get_name(buf):find('skg://pull/', 1, true) then
    diagnostics = true
    break
  end
end
local diagnostics_message =
  'the incident-qualified pull diagnostics remain available'
if not diagnostics then
  diagnostics_message = diagnostics_message .. '; '
    .. table.concat(buffer_inventory, ' | ')
end
T.check(diagnostics, diagnostics_message)

local finalized = vim.fn.globpath(
  vim.fs.dirname(os.getenv('SKG_TEST_CONFIG')) .. '/maintenance-archives',
  '**/FINALIZED', false, true)
T.check(#finalized > 0, 'a finalized recovery archive remains on disk')

T.pass('PASS: client-owned pull completed through Neovim maintenance')
