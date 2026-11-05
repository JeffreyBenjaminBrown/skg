-- PURPOSE: The small one-shot request/response commands:
-- verify-connection, rebuild-session-lifetime-stores and strip-body-whitespace. The Lua
-- port of elisp/skg-request-verify-connection.el,
-- elisp/skg-request-rebuild-session-lifetime-stores.el and
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

---Reload the server's config, and rebuild the in-memory graph and
---Tantivy from authoritative .skg files. The .skg files are not
---changed. An invalid config fails the rebuild, and the server keeps
---serving the old one. After a successful rebuild the herald rules are
---fetched again, since the reloaded config may have changed their
---skgrepo labels, and then every view without unsaved edits is
---rerendered.
function M.rebuild_session_lifetime_stores ()
  vim.notify('Rebuilding session-lifetime stores (this may take a while) ...')
  state.register_response_handler('rebuild-session-lifetime-stores',
    function (_payload, response)
      local content = payload.field_text(response, 'content')
      local failed = content ~= nil
                     and vim.startswith(content, 'Rebuild failed')
      if failed then
        vim.notify(content, vim.log.levels.ERROR)
      else
        vim.notify(content or 'Rebuild complete.')
        -- Outside this handler: the rerender resets the receiver,
        -- which would drop whatever follows the rebuild's answer.
        vim.schedule(M.refetch_herald_rules_and_rerender_all_views) end
    end, true)
  state.lp_reset()
  client.send_string('((request . "rebuild session-lifetime stores"))\n')
end

---Fetch the herald rules, then rerender every view without unsaved
---edits. The server answers in order, so the views are redrawn under
---the new rules. Nothing between the two sends reads the connection,
---so the rerender's reset of the receiver cannot drop the herald
---rules' answer.
function M.refetch_herald_rules_and_rerender_all_views ()
  require('skg.herald_rules').request_herald_rules()
  require('skg.rerender_all_views').request_rerender_all_views()
end

---Strip trailing whitespace from every line of every body, in every
---repo the user owns (foreign skgrepos are never written, so they are left
---untouched). Rewrites exactly the .skg files whose bodies change;
---derived caches are refreshed.
function M.strip_trailing_whitespace_from_bodies ()
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
