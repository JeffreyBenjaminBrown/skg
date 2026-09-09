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
  local pull = require('skg.pull')
  require('skg.buffer_registry').install_rebuilding_statusline()
  state.register_server_push_handler(
    'collateral-view',
    require('skg.save').background_collateral_offer_handler)
  state.register_server_push_handler(
    'refresh-queued',
    require('skg.rerender').refresh_queued_handler)
  state.register_server_push_handler(
    'reconciliation-ready',
    require('skg.misc_requests').reconciliation_ready_handler)
  state.register_server_push_handler(
    'maintenance-offer',
    require('skg.maintenance').server_offer_handler)
  state.register_server_push_handler(
    'maintenance-status',
    require('skg.maintenance').server_status_handler)
  state.register_server_push_handler(
    'query-wait-result',
    require('skg.query_wait').result_handler)
  state.register_server_push_handler(
    'query-wait-status',
    require('skg.query_wait').status_push_handler)
  vim.api.nvim_create_user_command('SkgReconcilePendingChanges',
    function () require('skg.maintenance').reconcile_pending() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgMaintenanceStatus',
    function () require('skg.maintenance').status() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgCancelMaintenance',
    function () require('skg.maintenance').cancel() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgRetryMaintenance',
    function () require('skg.maintenance').retry() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgPullAll',
    function () pull.pull_all() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgReloadIds',
    function (options)
      require('skg.maintenance').reload_ids(options.fargs) end,
    { nargs = '+', force = true })
  vim.api.nvim_create_user_command('SkgReloadPaths',
    function (options)
      require('skg.maintenance').reload_paths(options.fargs) end,
    { nargs = '+', complete = 'file', force = true })
  vim.api.nvim_create_user_command('SkgRecoverReloadIncident',
    function (options)
      require('skg.reload_recovery').recover(
        options.args ~= '' and options.args or nil) end,
    { nargs = '?', force = true })
  vim.api.nvim_create_user_command('SkgDismissReloadRecoveryIncident',
    function (options)
      require('skg.reload_recovery').dismiss(
        options.args ~= '' and options.args or nil) end,
    { nargs = '?', force = true })
  vim.api.nvim_create_user_command('SkgPendingSaveStatus',
    function () require('skg.save').pending_save_status() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgPendingSaveInspect',
    function () require('skg.save').inspect_pending_save() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgAcknowledgePendingSave',
    function () require('skg.save').acknowledge_pending_save() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgRetryPendingSave',
    function () require('skg.save').retry_pending_save() end,
    { force = true })
  vim.api.nvim_create_user_command('SkgQueryWaitStatus',
    function (options)
      require('skg.query_wait').status(options.args ~= '' and options.args or nil) end,
    { nargs = '?', force = true })
  vim.api.nvim_create_user_command('SkgQueryWaitCancel',
    function (options)
      require('skg.query_wait').cancel(options.args) end,
    { nargs = 1, force = true })
  vim.api.nvim_create_user_command('SkgQueryWaitRecover',
    function (options)
      require('skg.query_wait').recover(options.fargs[1], options.fargs[2]) end,
    { nargs = '+', force = true })
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
