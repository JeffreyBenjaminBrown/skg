-- PURPOSE: The small one-shot request/response commands:
-- verify-connection, rebuild-dbs and strip-body-whitespace. The Lua
-- port of elisp/skg-request-verify-connection.el,
-- elisp/skg-request-rebuild-dbs.el and
-- elisp/skg-request-strip-body-whitespace.el.

local client = require('skg.client')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

---Verify the connection to the Rust server by sending a simple ping;
---the server's confirmation is echoed to the user.
function M.connection_verify ()
  state.register_response_handler('verify-connection',
    function (_payload, response)
      local content = payload.field(response, 'content')
      local message = 'connected'
      if content ~= nil and not sexpr.is_nil(content) then
        message = sexpr.is_list(content) and sexpr.to_string(content)
                  or sexpr.atom_text(content) end
      vim.notify(message)
    end, true)
  state.lp_reset()
  client.send_string('((request . "verify connection"))\n')
end

---Wipe and rebuild TypeDB and Tantivy from the .skg files on disk.
---Does not touch the filesystem -- only the derived databases.
function M.rebuild_dbs ()
  vim.notify('Rebuilding databases (this may take a while) ...')
  state.register_response_handler('rebuild-dbs',
    function (_payload, response)
      local content = payload.field_text(response, 'content')
      vim.notify((content or 'Rebuild complete.')
                 .. '\nExisting skg views are now invalid.'
                 .. ' Run :SkgCloseAllSkgBuffers to close them.')
    end, true)
  state.lp_reset()
  client.send_string('((request . "rebuild dbs"))\n')
end

---Strip trailing whitespace from every line of every body, in every
---source in the config, foreign ones included. Rewrites exactly the
---.skg files whose bodies change; derived caches are refreshed.
function M.strip_body_whitespace ()
  vim.notify('Stripping trailing whitespace from bodies ...')
  state.register_response_handler('strip-body-whitespace',
    function (_payload, response)
      local content = payload.field_text(response, 'content')
      vim.notify((content or 'Body whitespace strip complete.')
                 .. '\nTo verify nothing but whitespace changed,'
                 .. " review with 'git diff --ignore-all-space'"
                 .. ' (it should show nothing).')
    end, true)
  state.lp_reset()
  client.send_string('((request . "strip body whitespace"))\n')
end

return M
