-- PURPOSE: Force-reload all skg Lua modules.
-- The Lua port of elisp/skg-reload.el: pick up code changes without
-- restarting the editor.
--
-- Buffer-scoped state lives in 'vim.b', so it needs no copying. Process-local
-- session facts do: the deliberate reconnect must not forget an incident,
-- archive checksums, source inventory, or monotonic request identity.
--
-- Like the elisp version, the herald rule table (fetched from the
-- server at connect time, session-only, no on-disk source) is captured
-- before the reload and re-installed after -- protected so that a
-- load error in some other module cannot lose the table for the rest
-- of the session. The reload error itself still propagates.
--
-- One deliberate deviation from elisp: the open TCP connection is
-- CLOSED before the module holding it is discarded. (Emacs keeps the
-- process object alive in its global process list even after the
-- variable naming it is unbound; a dropped Lua uv handle would instead
-- linger open until garbage collection, invisibly.) The client
-- reconnects on the next request, exactly as post-reload Emacs does.

local M = {}

local function capture_session ()
  local old_state = package.loaded['skg.state']
  local old_config = package.loaded['skg.config']
  local old_init = package.loaded['skg']
  local old_client = package.loaded['skg.client']
  if old_state and type(old_state.fail_all_requests) == 'function' then
    old_state.fail_all_requests('client code reload reconnect') end
  local snapshot = {
    state = old_state and {
      active_source_set_name = old_state.active_source_set_name,
      maintenance_archive_folder = old_state.maintenance_archive_folder,
      maintenance_archive_identity = old_state.maintenance_archive_identity,
      maintenance_state = old_state.maintenance_state,
      maintenance_client_incident = old_state.maintenance_client_incident,
      pending_maintenance_offer = old_state.pending_maintenance_offer,
      next_request_number = old_state.next_request_number,
      id_stack = old_state.id_stack,
    } or nil,
    config = old_config and {
      config_file_path = old_config.config_file_path,
      source_inventory = old_config.source_inventory,
      store_state = old_config.store_state,
    } or nil,
    config_path = old_init and old_init.config_path or nil,
    client_port = old_client and old_client.port or nil,
  }
  if old_state and type(old_state.close_connection) == 'function' then
    pcall(old_state.close_connection) end
  return snapshot
end

local function restore_session (snapshot, root_loaded)
  if snapshot.state then
    local restored = require('skg.state')
    for key, value in pairs(snapshot.state) do restored[key] = value end
    restored.connection_handshake_state = nil
    restored.tcp = nil
    vim.g.skg_active_source_set_name = restored.active_source_set_name
  end
  if snapshot.config then
    local restored = require('skg.config')
    for key, value in pairs(snapshot.config) do restored[key] = value end
  end
  local restored_client = require('skg.client')
  restored_client.port = snapshot.client_port
  if root_loaded then
    local restored_init = require('skg')
    restored_init.config_path = snapshot.config_path
    restored_init.install_session_surface()
  end
end

---Unload every skg module and reload the entry point from disk.
function M.reload ()
  local captured_herald_rules = M.herald_rules_if_loaded()
  local session = capture_session()
  local reload_succeeded, reload_error = pcall(function ()
    for module_name in pairs(package.loaded) do
      if module_name == 'skg' or module_name:match('^skg%.') then
        package.loaded[module_name] = nil end
    end
    require('skg') end)
  restore_session(session, reload_succeeded)
  if captured_herald_rules then
    -- Restore whether or not the reload itself succeeded (the
    -- unwind-protect of the elisp version): a failed reload must not
    -- cost the session its only copy of the rule table.
    local ok, herald_rules = pcall(require, 'skg.herald_rules')
    if ok and type(herald_rules.install_rules) == 'function' then
      pcall(herald_rules.install_rules, captured_herald_rules) end
  end
  if not reload_succeeded then error(reload_error) end
  vim.notify('skg: all modules reloaded')
end

---The current herald rule table, if the module holding it is loaded.
---@return any|nil
function M.herald_rules_if_loaded ()
  local herald_rules = package.loaded['skg.herald_rules']
  if herald_rules and type(herald_rules) == 'table'
     and type(herald_rules.get_rules) == 'function' then
    local ok, rules = pcall(herald_rules.get_rules)
    if ok then return rules end
  end
  return nil
end

return M
