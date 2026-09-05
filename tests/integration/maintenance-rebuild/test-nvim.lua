-- End-to-end full rebuild through durable maintenance, Neovim client.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(75)

local maintenance = require('skg.maintenance')
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
  return buf and T.buffer_text(buf):find('title before rebuild', 1, true)
    and buf or nil
end)
T.check(view ~= nil, 'the pre-rebuild view loaded')

local ordinary_handler = maintenance.origin_operation_handlers['full-rebuild']
maintenance.register_origin_operation_handler('full-rebuild',
  function (incident, phase, response)
    if phase == 'archive-ready' then
      local file = assert(io.open(os.getenv('SKG_REBUILD_SOURCE'), 'w'))
      file:write('title: "title after rebuild"\npid: "x"\n')
      file:close()
    end
    return ordinary_handler(incident, phase, response)
  end)

T.check(require('skg.misc_requests').rebuild_dbs(),
        'the full rebuild maintenance request started')
T.check(T.wait_for(function ()
  return state.maintenance_client_incident == nil
    and vim.api.nvim_buf_is_valid(view)
    and T.buffer_text(view):find('title after rebuild', 1, true)
end, 60), 'full rebuild returned idle with the same live view reconciled')

T.check((require('skg.config').store_state.graph_generation or 0) > 1,
        'the selected graph generation advanced')

local finalized = vim.fn.globpath(
  vim.fs.dirname(os.getenv('SKG_TEST_CONFIG')) .. '/maintenance-archives',
  '**/FINALIZED', false, true)
T.check(#finalized > 0, 'a finalized recovery archive remains on disk')

T.pass('PASS: full rebuild completed through Neovim maintenance')
