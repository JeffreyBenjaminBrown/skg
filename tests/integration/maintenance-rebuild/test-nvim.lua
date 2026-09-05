-- End-to-end full rebuild through durable maintenance, Neovim client.

local T = dofile('../test-nvim-lib.lua')
T.arm_timeout(75)

local maintenance = require('skg.maintenance')
local state = require('skg.state')
local config = require('skg.config')
local valid_replacement_config

local function replace_config ()
  local path = assert(os.getenv('SKG_TEST_CONFIG'))
  local file = assert(io.open(path, 'r'))
  local text = file:read('*a')
  file:close()
  local replacements = {
    {'default_source_set = "main"',
     'default_source_set = "replacement"'},
    {'maintenance_archive_folder = "maintenance-archives"',
     'maintenance_archive_folder = "replacement-archives"'},
    {'name = "main"', 'name = "replacement"'},
    {'path = "notes"', 'path = "replacement-notes"'},
  }
  for _, replacement in ipairs(replacements) do
    local first, last = text:find(replacement[1], 1, true)
    T.check(first ~= nil, 'the config replacement target exists')
    text = text:sub(1, first - 1) .. replacement[2]
      .. text:sub(last + 1)
  end
  file = assert(io.open(path, 'w'))
  file:write(text)
  file:close()
end

local function view_of (id)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) and vim.b[buf].skg_content_view
       and T.buffer_text(buf):find('(id ' .. id .. ')', 1, true) then
      return buf end
  end
  return nil
end

require('skg.content_view').request_single_root_content_view_from_id('x')
local view = T.wait_for(function ()
  local buf = view_of('x')
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

maintenance.register_origin_operation_handler('full-rebuild',
  function (incident, phase, response)
    if phase == 'archive-ready' then
      local path = os.getenv('SKG_TEST_CONFIG')
      local file = assert(io.open(path, 'r'))
      valid_replacement_config = file:read('*a')
      file:close()
      file = assert(io.open(path, 'w'))
      file:write('this is not valid TOML = [\n')
      file:close()
    end
    return ordinary_handler(incident, phase, response)
  end)
T.check(require('skg.misc_requests').rebuild_dbs(),
        'the invalid full rebuild maintenance request started')
T.check(T.wait_for(function ()
  local file = io.open(os.getenv('SKG_TEST_CONFIG'), 'r')
  if not file then return false end
  local text = file:read('*a')
  file:close()
  return text:find('this is not valid TOML', 1, true) == 1
end, 15), 'the invalid replacement config reached the archive boundary')
T.check(#vim.fn.globpath(
  vim.fs.dirname(os.getenv('SKG_TEST_CONFIG')) .. '/replacement-archives',
  '**/manifest.initial.sexp', false, true) > 0,
  'the next incident used the replacement archive root')
vim.wait(1000)
maintenance.status(true)
T.check(T.wait_for(function ()
  local incident = state.maintenance_client_incident
  return incident and incident.phase == 'server-blocked'
end, 30), 'an invalid replacement config blocked before store mutation')

require('skg.content_view').request_single_root_content_view_from_id('y')
local blocked_view = T.wait_for(function ()
  local buf = view_of('y')
  return buf and T.buffer_text(buf):find(
    'queryable after invalid preflight', 1, true) and buf or nil
end, 15)
T.check(blocked_view ~= nil,
        'the selected graph remained queryable after invalid preflight')
T.check(vim.b[blocked_view].skg_maintenance_epoch
          == state.maintenance_client_incident.epoch,
        'a view opened during blocked maintenance was born locked')

local config_file = assert(io.open(os.getenv('SKG_TEST_CONFIG'), 'w'))
config_file:write(assert(valid_replacement_config))
config_file:close()
maintenance.retry()
T.check(T.wait_for(function ()
  return state.maintenance_client_incident == nil
end, 60), 'the repaired incident retried to its terminal ACK')
T.check(vim.b[blocked_view].skg_maintenance_epoch == nil,
        'the mid-incident view joined terminal settlement and unlocked')

local finalized = vim.fn.globpath(
  vim.fs.dirname(os.getenv('SKG_TEST_CONFIG')) .. '/maintenance-archives',
  '**/FINALIZED', false, true)
T.check(#finalized > 0, 'a finalized recovery archive remains on disk')

T.pass('PASS: full rebuild and invalid preflight completed through Neovim maintenance')
