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

vim.api.nvim_create_user_command('SkgConnectionVerify', function ()
  require('skg.misc_requests').connection_verify()
end, {
  desc = 'Ping the skg server and echo its reply',
})

vim.api.nvim_create_user_command('SkgConnectionEnd', function ()
  require('skg.client').connection_end()
end, {
  desc = 'Close the TCP connection to the skg server',
})

-- The rest of the command surface (one :Skg* command per user-facing
-- command, lazily resolved) lives in lua/skg/keymaps.lua.
require('skg.keymaps').define_ex_commands()

-- Linkstack paste/pop inside command-line prompts, the analog of the
-- elisp minibuffer bindings (C-c chords are not available in the
-- cmdline; <C-x> chords are free there).
vim.keymap.set('c', '<C-x>i', function ()
  require('skg.linkstack').paste_id() end,
  { desc = 'skg: paste the top-of-stack id' })
vim.keymap.set('c', '<C-x>l', function ()
  require('skg.linkstack').paste_link() end,
  { desc = 'skg: paste a link to the top-of-stack node' })
vim.keymap.set('c', '<C-x>I', function ()
  require('skg.linkstack').pop_id() end,
  { desc = 'skg: pop the top-of-stack id' })
vim.keymap.set('c', '<C-x>L', function ()
  require('skg.linkstack').pop_link() end,
  { desc = 'skg: pop a link to the top-of-stack node' })

-- .skg files get the goto/id-navigation/linkstack subset (the analog
-- of skg-file-minor-mode, auto-enabled by find-file-hook) plus
-- readable ids (title-annotated UUIDs).
vim.api.nvim_create_autocmd({ 'BufReadPost', 'BufNewFile' }, {
  pattern = '*.skg',
  callback = function (event)
    require('skg.keymaps').attach_file_buffer(event.buf)
    require('skg.readable_ids').enable(event.buf)
  end,
})

-- Re-annotate readable ids whenever neogit refreshes its status (the
-- magit-post-refresh-hook analog).
vim.api.nvim_create_autocmd('User', {
  pattern = 'NeogitStatusRefreshed',
  callback = function ()
    local readable_ids = require('skg.readable_ids')
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf)
         and readable_ids.enabled_p(buf) then
        readable_ids.annotate_buffer(buf)
      end
    end
  end,
})
