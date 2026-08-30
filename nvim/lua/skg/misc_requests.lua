-- PURPOSE: The small one-shot request/response commands:
-- verify-connection, rebuild-dbs, cyclic-root repair and
-- strip-body-whitespace. The Lua
-- port of elisp/skg-request-verify-connection.el,
-- elisp/skg-request-rebuild-dbs.el and
-- elisp/skg-request-strip-body-whitespace.el.

local client = require('skg.client')
local config = require('skg.config')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')
local messages = require('skg.messages')

local M = {}

---Verify the connection to the Rust server by sending a simple ping;
---the server's confirmation is echoed to the user.
function M.connection_verify ()
  state.register_response_handler('verify-connection',
    function (_payload, response)
      config.install_source_inventory(
        payload.field(response, 'source-inventory'))
      config.store_state = {
        graph_generation = payload.field(response, 'graph-generation'),
        path_outcomes = payload.field(response, 'path-outcomes'),
        typedb_health = payload.field(response, 'typedb-health'),
        tantivy_health = payload.field(response, 'tantivy-health'),
      }
      M.show_handshake_telescope_warnings(response)
      local content = payload.field(response, 'content')
      local message = 'connected'
      if content ~= nil and not sexpr.is_nil(content) then
        message = sexpr.is_list(content) and sexpr.to_string(content)
                  or sexpr.atom_text(content) end
      vim.notify(message)
    end, true)
  client.submit_request('((request . "verify connection"))\n')
end

---Show structured initialization/reconnect warnings persistently.
---@param response any
function M.show_handshake_telescope_warnings (response)
  local warnings = payload.field(response, 'telescope-warnings')
  if warnings == nil or not sexpr.is_list(warnings) or #warnings == 0 then
    return end
  local lines = { '* WARNING: Telescope load warnings' }
  for _, warning in ipairs(warnings) do
    table.insert(lines, '** ' ..
      (payload.field_text(warning, 'pid') or '[unknown pid]'))
    table.insert(lines,
      payload.field_text(warning, 'message') or 'Unspecified warning')
    local winners = payload.string_list(
      payload.field(warning, 'winning-paths'))
    if #winners > 0 then
      table.insert(lines, '*** retained owned files')
      for _, path in ipairs(winners) do
        table.insert(lines, '**** ' .. path) end end
    local losers = payload.string_list(
      payload.field(warning, 'ignored-paths'))
    if #losers > 0 then
      table.insert(lines, '*** ignored foreign files')
      for _, path in ipairs(losers) do
        table.insert(lines, '**** ' .. path) end end
  end
  messages.big_nonfatal_message(
    'skg://messages/telescope-warnings',
    string.format('WARNING: Skg loaded with %d telescope warning(s).',
                  #warnings),
    table.concat(lines, '\n'))
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
  client.submit_request('((request . "rebuild dbs"))\n')
end

---Recompute the rank-only cyclic-root cache from the complete current graph.
function M.recompute_cyclicroots ()
  vim.notify('Recomputing cyclic-root search ranking ...')
  state.register_response_handler('recompute-cyclic-roots',
    function (_payload, response)
      local content = payload.field_text(response, 'content')
        or 'Cyclic-root recomputation finished.'
      local status = payload.field_text(response, 'terminal-status')
      vim.notify(content,
        status == 'failed' and vim.log.levels.ERROR or vim.log.levels.INFO)
    end, true)
  client.submit_request('((request . "recompute cyclic roots"))\n')
end

---Strip trailing whitespace from every line of every body, in every
---source the user owns (foreign sources are read-only and left
---untouched). Rewrites exactly the .skg files whose bodies change;
---derived caches are refreshed.
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
  client.submit_request('((request . "strip body whitespace"))\n')
end

return M
