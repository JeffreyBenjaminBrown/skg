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

M.archive_format_version = 1

local function client_version ()
  local version = vim.version()
  return string.format('%d.%d.%d', version.major, version.minor, version.patch)
end

local function handshake_request ()
  return sexpr.to_string({
    sexpr.pair(sexpr.symbol('request'), 'verify connection'),
    sexpr.pair(sexpr.symbol('role'), 'interactive'),
    sexpr.pair(sexpr.symbol('client-kind'), 'neovim'),
    sexpr.pair(sexpr.symbol('client-version'), client_version()),
    sexpr.pair(sexpr.symbol('client-session-id'), state.client_session_id),
    sexpr.pair(sexpr.symbol('archive-format-version'),
               M.archive_format_version),
    sexpr.pair(sexpr.symbol('native-undo-kind'), 'nvim-wundo'),
    sexpr.pair(sexpr.symbol('native-undo-version'), client_version()),
    sexpr.pair(sexpr.symbol('source-set'), state.active_source_set_name),
  }) .. '\n'
end

function M.install_connection_verification (_payload_text, response, tcp)
  config.install_source_inventory(
    payload.field(response, 'source-inventory'))
  state.active_source_set_name =
    payload.field_text(response, 'active-source-set') or 'all'
  vim.g.skg_active_source_set_name = state.active_source_set_name
  state.maintenance_archive_folder =
    payload.field_text(response, 'maintenance-archive-folder')
  state.maintenance_archive_identity =
    payload.field_text(response, 'maintenance-archive-identity')
  state.maintenance_state = {
    epoch = payload.field(response, 'maintenance-epoch'),
    state = payload.field_text(response, 'maintenance-state'),
    census_required = payload.field_text(response, 'census-required'),
  }
  config.store_state = {
    graph_generation = payload.field(response, 'graph-generation'),
    manifest_revision = payload.field(response, 'manifest-revision'),
    typedb_health = payload.field(response, 'typedb-health'),
    tantivy_health = payload.field(response, 'tantivy-health'),
  }
  state.connection_handshake_state = 'census'
  require('skg.maintenance').adopt_handshake_epoch()
  M.show_handshake_telescope_warnings(response)
  M.show_pending_recovery_incidents(response)
  local content = payload.field(response, 'content')
  local message = 'connected'
  if content ~= nil and not sexpr.is_nil(content) then
    message = sexpr.is_list(content) and sexpr.to_string(content)
              or sexpr.atom_text(content) end
  vim.notify(message)
  M.submit_buffer_census(tcp or state.tcp)
end

function M.submit_buffer_census (tcp, maintenance_incident_id, maintenance_epoch)
  local registry = require('skg.buffer_registry')
  local request_text = maintenance_epoch
    and string.format(
      '((request . "client census") (maintenance-epoch . %d))\n',
      maintenance_epoch)
    or '((request . "client census"))\n'
  client.submit_priority_request(tcp, request_text, {
    ['client-census'] = {
      handler = function (_payload_text, response)
        M.handle_buffer_census_response(
          tcp, response, maintenance_incident_id, maintenance_epoch) end,
      one_shot = true,
    },
  }, registry.census_payload(), maintenance_incident_id)
end

local function complete_buffer_census (maintenance_incident_id, maintenance_epoch)
  state.connection_handshake_state = 'verified'
  vim.schedule(function ()
    require('skg.maintenance').resume_after_census(
      maintenance_incident_id, maintenance_epoch) end)
end

function M.handle_buffer_census_response (
    tcp, response, maintenance_incident_id, maintenance_epoch)
  local registry = require('skg.buffer_registry')
  local required = payload.string_list(
    payload.field(response, 'text-required-buffer-ids'))
  require('skg.maintenance').handle_census_stale(payload.string_list(
    payload.field(response, 'stale-buffer-ids')))
  if #required == 0 then
    complete_buffer_census(maintenance_incident_id, maintenance_epoch)
    return
  end
  state.connection_handshake_state = 'census-texts'
  local request_text = maintenance_epoch
    and string.format(
      '((request . "client census texts") (maintenance-epoch . %d))\n',
      maintenance_epoch)
    or '((request . "client census texts"))\n'
  client.submit_priority_request(
    tcp, request_text, {
      ['client-census'] = {
        handler = function (_payload_text, final_response)
          require('skg.maintenance').handle_census_stale(
            payload.string_list(payload.field(
              final_response, 'stale-buffer-ids')))
          if payload.field_text(final_response, 'census-complete') ~= 'true' then
            error('Skg server did not complete the buffer census') end
          complete_buffer_census(maintenance_incident_id, maintenance_epoch)
        end,
        one_shot = true,
      },
    }, registry.census_texts_payload(required), maintenance_incident_id)
end

function M.enqueue_connection_handshake (tcp)
  if state.connection_handshake_state then return end
  state.connection_handshake_state = 'sent'
  client.submit_priority_request(tcp, handshake_request(), {
    ['verify-connection'] = {
      handler = function (payload_text, response)
        M.install_connection_verification(payload_text, response, tcp) end,
      one_shot = true,
    },
  })
end

---Report the exact process-owned sweep which batch closure already queued.
function M.reconciliation_ready_handler (_payload, response)
  local generation = payload.field_text(response, 'sweep-generation') or '?'
  vim.notify('Skg queued post-batch disk observation generation '
    .. generation)
end

---Verify the connection to the Rust server by sending a simple ping;
---the server's confirmation is echoed to the user.
function M.connection_verify ()
  local already_connected = state.tcp and not state.tcp:is_closing()
  client.connect()
  if not already_connected then return end
  state.register_response_handler('verify-connection',
    function (payload_text, response)
      M.install_connection_verification(payload_text, response, state.tcp) end,
    true)
  client.submit_request(handshake_request())
end

---Neovim does not yet implement the destructive confirmation UI. Never let
---that omission make recovery automatic: show every durable incident and
---direct the user to an Emacs client or manual repair.
---@param response any
function M.show_pending_recovery_incidents (response)
  local incidents = payload.field(response, 'pending-recovery-incidents')
  if incidents == nil or not sexpr.is_list(incidents) or #incidents == 0 then
    return end
  local lines = { '* WARNING: Fatal reload recovery is pending' }
  for _, incident in ipairs(incidents) do
    table.insert(lines, '** ' ..
      (payload.field_text(incident, 'incident-id') or '[unknown incident]'))
    local fatal = payload.field(incident, 'fatal')
    if fatal and sexpr.is_list(fatal) then
      for _, item in ipairs(fatal) do
        table.insert(lines, '*** ' ..
          (payload.field_text(item, 'pid') or '[unknown pid]'))
        table.insert(lines,
          payload.field_text(item, 'reason') or 'Unspecified fatal error')
      end
    end
  end
  table.insert(lines, '** what to do')
  table.insert(lines,
    'The Neovim client cannot yet confirm automatic recovery. Use the Emacs client recovery command or repair manually; Skg has retained the incident journal and will not recover automatically.')
  messages.big_nonfatal_message(
    'skg://messages/pending-reload-recovery',
    string.format('WARNING: %d fatal reload recovery incident(s) remain unresolved.',
                  #incidents),
    table.concat(lines, '\n'))
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
  if state.maintenance_client_incident then
    error('Maintenance is already active') end
  local registry = require('skg.buffer_registry')
  local dirty = {}
  local dirty_raw = {}
  for _, buf in ipairs(registry.buffers()) do
    if registry.dirty(buf) then
      table.insert(dirty, buf)
      local record = registry.record(buf)
      if record and record.kind == 'raw-skg-file' then
        table.insert(dirty_raw, vim.api.nvim_buf_get_name(buf)) end
    end
  end
  if #dirty_raw > 0 then
    error('Full rebuild refuses modified raw .skg buffers: '
      .. table.concat(dirty_raw, ', ')) end
  if #dirty > 0 then
    local answer = vim.fn.confirm(string.format(
      'Full rebuild will archive %d dirty Skg view(s). Impacted views may '
        .. 'become detached recovery buffers. Continue?', #dirty),
      '&Continue\n&Cancel', 2)
    if answer ~= 1 then return false end
  end
  vim.notify('Preparing recovery archive for full rebuild ...')
  require('skg.maintenance').begin(
    'full-rebuild', nil, nil, nil, function ()
      vim.notify('Skg databases rebuilt and registered views reconciled')
    end)
  return true
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
