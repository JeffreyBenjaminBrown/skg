-- PURPOSE: Force-reload all skg Lua modules.
-- The Lua port of elisp/skg-reload.el: pick up code changes without
-- restarting the editor.
--
-- The elisp version must spare two files from unloading (skg-buffer,
-- because 'unload-feature' would destroy the permanent-local view ID
-- and degrade the major mode; skg-keymaps-and-aliases, because minor
-- modes capture their maps at definition time). Neither hazard exists
-- here: buffer-scoped state lives in 'vim.b' (untouched by reloading
-- Lua modules), and keymaps/autocmds are re-created idempotently when
-- their modules re-require. So the Lua reload clears everything under
-- the 'skg' namespace uniformly.
--
-- Like the elisp version, the herald rule table and the skgrepo label
-- table served beside it (fetched from the server at connect time,
-- session-only, no on-disk skgrepo) are captured before the reload
-- and re-installed after -- protected so that a load error in some
-- other module cannot lose them for the rest of the session. The
-- reload error itself still propagates.
--
-- One deliberate deviation from elisp: the open TCP connection is
-- closed before the module holding it is discarded. (Emacs keeps the
-- process object alive in its global process list even after the
-- variable naming it is unbound; a dropped Lua uv handle would instead
-- linger open until garbage collection, invisibly.) The client
-- reconnects on the next request, exactly as post-reload Emacs does.

local M = {}

---Unload every skg module and reload the entry point from disk.
function M.reload ()
  local captured_herald_rules = M.herald_rules_if_loaded()
  local captured_skgrepo_labels = M.skgrepo_labels_if_loaded()
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
    M.restore_herald_table('install_rules', captured_herald_rules)
  end
  if captured_skgrepo_labels then
    M.restore_herald_table('set_skgrepo_labels', captured_skgrepo_labels)
  end
  if not reload_succeeded then error(reload_error) end
  local annotations = require('skg.link_annotations')
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.b[buf].skg_link_annotations_enabled == true then
      annotations.enable(buf) end
  end
  vim.notify('skg: all modules reloaded')
end

---The current herald rule table, if the module holding it is loaded.
---@return any|nil
function M.herald_rules_if_loaded ()
  return M.herald_table_if_loaded('get_rules')
end

---The current skgrepo label table, if the module holding it is loaded.
---@return table|nil
function M.skgrepo_labels_if_loaded ()
  return M.herald_table_if_loaded('get_skgrepo_labels')
end

---What the getter GETTER_NAME of skg.herald_rules returns, if that
---module is loaded and has it.
---@param getter_name string
---@return any|nil
function M.herald_table_if_loaded (getter_name)
  local herald_rules = package.loaded['skg.herald_rules']
  if herald_rules and type(herald_rules) == 'table'
     and type(herald_rules[getter_name]) == 'function' then
    local ok, table_value = pcall(herald_rules[getter_name])
    if ok then return table_value end
  end
  return nil
end

---Hand TABLE_VALUE to the installer INSTALLER_NAME of the freshly
---loaded skg.herald_rules, ignoring any failure.
---@param installer_name string
---@param table_value any
function M.restore_herald_table (installer_name, table_value)
  local ok, herald_rules = pcall(require, 'skg.herald_rules')
  if ok and type(herald_rules[installer_name]) == 'function' then
    pcall(herald_rules[installer_name], table_value) end
end

return M
