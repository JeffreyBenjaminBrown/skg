-- End-to-end full rebuild through durable maintenance, Neovim client.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(75)

local maintenance = require('skg.maintenance')
local state = require('skg.state')
local config = require('skg.config')

local function replace_config ()
  local path = assert(os.getenv('SKG_TEST_CONFIG'))
  local file = assert(io.open(path, 'r'))
  local text = file:read('*a')
  file:close()
  local replacements = {
    {'default_source_set = "main"',
     'default_source_set = "replacement"'},
    {'name = "main"', 'name = "replacement"'},
    {'path = "notes"', 'path = "replacement-notes"'},
  }
  for _, replacement in ipairs(replacements) do
    local changed
    text, changed = text:gsub(replacement[1], replacement[2], 1)
    T.check(changed == 1, 'the config replacement target exists')
  end
  file = assert(io.open(path, 'w'))
  file:write(text)
  file:close()
end

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
T.check(state.active_source_set_name == 'main',
        'the initial restricted source-set is active')

local ordinary_handler = maintenance.origin_operation_handlers['full-rebuild']
maintenance.register_origin_operation_handler('full-rebuild',
  function (incident, phase, response)
    if phase == 'archive-ready' then
      replace_config()
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
T.check(state.active_source_set_name == 'all',
        'the absent old source-set fell back exactly to all')
T.check(config.source_inventory[1].name == 'replacement',
        'the client installed the replacement source inventory')

local finalized = vim.fn.globpath(
  vim.fs.dirname(os.getenv('SKG_TEST_CONFIG')) .. '/maintenance-archives',
  '**/FINALIZED', false, true)
T.check(#finalized > 0, 'a finalized recovery archive remains on disk')

T.pass('PASS: full rebuild completed through Neovim maintenance')
