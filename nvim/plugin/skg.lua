-- PURPOSE: Ex-command surface and autocmd wiring for the skg client.
-- The analog of the interactive entry points in
-- elisp/skg-keymaps-and-aliases.el's global bindings. Buffer-local
-- keymaps live in lua/skg/keymaps.lua and attach per skg buffer.
-- Commands are added here as their modules are ported.

if vim.g.loaded_skg then return end
vim.g.loaded_skg = true

vim.api.nvim_create_user_command('SkgInit', function (opts)
  require('skg').init(opts.args)
end, {
  nargs = 1,
  complete = 'file',
  desc = 'Initialize the skg client against a skgconfig.toml',
})

vim.api.nvim_create_user_command('SkgReload', function ()
  require('skg.reload').reload()
end, {
  desc = 'Reload all skg Lua modules from disk (dev convenience)',
})
