-- PURPOSE: The entry point of the skg Neovim client; require('skg').
-- The analog of elisp/skg-init.el + the 'skg-client-init' entry in
-- elisp/skg-client.el. Modules land here as they are ported; the
-- module map lives in TODO/vim-client/plan.org.

local M = {}

---The nvim version floor, per the plan's settled decisions.
local minimum_nvim_version = { major = 0, minor = 11 }

---Error unless the running nvim meets the version floor.
function M.check_nvim_version ()
  local v = vim.version()
  if v.major > minimum_nvim_version.major then return end
  if v.major == minimum_nvim_version.major
     and v.minor >= minimum_nvim_version.minor then return end
  error(string.format(
    'skg requires nvim %d.%d or newer; this is %d.%d.%d',
    minimum_nvim_version.major, minimum_nvim_version.minor,
    v.major, v.minor, v.patch))
end

---Absolute path of the directory holding skgconfig.toml, set by init.
---(The analog of 'skg-config-dir'.)
---@type string|nil
M.config_path = nil

---Install process-local commands and server-push handlers.  This is separate
---from connecting so a code reload can rebuild the Lua surface while keeping
---the same editor session and reconnecting only on the next request.
function M.install_session_surface ()
  local state = require('skg.state')
  state.register_server_push_handler(
    'collateral-view',
    require('skg.save').background_collateral_offer_handler)
  state.register_server_push_handler(
    'reconciliation-ready',
    require('skg.misc_requests').reconciliation_ready_handler)
  state.register_server_push_handler(
    'maintenance-offer',
    require('skg.maintenance').server_offer_handler)
  state.register_server_push_handler(
    'maintenance-status',
    require('skg.maintenance').server_status_handler)
  vim.api.nvim_create_user_command('SkgReconcilePendingChanges',
    function () require('skg.maintenance').reconcile_pending() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgMaintenanceStatus',
    function () require('skg.maintenance').status() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgCancelMaintenance',
    function () require('skg.maintenance').cancel() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgReloadIds',
    function (options)
      require('skg.maintenance').reload_ids(options.fargs) end,
    { nargs = '+', force = true })
  vim.api.nvim_create_user_command('SkgReloadPaths',
    function (options)
      require('skg.maintenance').reload_paths(options.fargs) end,
    { nargs = '+', complete = 'file', force = true })
end

---Initialize the client against a server config: remember the
---config, connect, verify, and fetch the herald rule table.
---The analog of 'skg-client-init'.
---@param config_toml_path string path to a skgconfig.toml
function M.init (config_toml_path)
  M.check_nvim_version()
  local absolute = vim.fn.fnamemodify(config_toml_path, ':p')
  if vim.fn.filereadable(absolute) == 0 then
    error('skg: no readable config at ' .. absolute) end
  M.config_path = absolute
  local config = require('skg.config')
  config.config_file_path = absolute
  local client = require('skg.client')
  M.install_session_surface()
  client.port = config.port_from_toml(absolute)
  client.connect()
  require('skg.herald_rules').request_herald_rules()
end

return M
