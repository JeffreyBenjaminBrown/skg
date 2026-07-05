-- PURPOSE: Force-reload all skg Lua modules.
-- The Lua port of elisp/skg-reload.el: pick up code changes without
-- restarting the editor.
--
-- The elisp version must spare two files from unloading (skg-buffer,
-- because 'unload-feature' would destroy the permanent-local view uri
-- and degrade the major mode; skg-keymaps-and-aliases, because minor
-- modes capture their maps at definition time). Neither hazard exists
-- here: buffer-scoped state lives in 'vim.b' (untouched by reloading
-- Lua modules), and keymaps/autocmds are re-created idempotently when
-- their modules re-require. So the Lua reload clears everything under
-- the 'skg' namespace uniformly.
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

---Unload every skg module and reload the entry point from disk.
function M.reload ()
  local captured_herald_rules = M.herald_rules_if_loaded()
  do -- Close the live connection before its module is discarded.
    local state = package.loaded['skg.state']
    if state and type(state) == 'table'
       and type(state.close_connection) == 'function' then
      pcall(state.close_connection) end
  end
  local reload_succeeded, reload_error = pcall(function ()
    for module_name in pairs(package.loaded) do
      if module_name == 'skg' or module_name:match('^skg%.') then
        package.loaded[module_name] = nil end
    end
    require('skg') end)
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
