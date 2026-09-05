-- Maintenance epoch and client-owned recovery-archive protocol.

local archive = require('skg.recovery_archive')
local client = require('skg.client')
local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

M.defer = function (callback) vim.schedule(callback) end
M.origin_operation_handlers = {}

---Register HANDLER for a maintenance origin's post-archive work.
---HANDLER receives the process-local incident, durable server phase, and
---parsed response. It returns true when it handles that phase.
---@param origin string
---@param handler fun(incident: table, phase: string, response: any): boolean
function M.register_origin_operation_handler (origin, handler)
  M.origin_operation_handlers[origin] = handler
end

---Dispatch the active incident to its origin adapter.
---@param phase string
---@param response any
---@return boolean handled
function M.dispatch_origin_operation (phase, response)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance origin dispatch has no client state')
  local origin = incident.offer and incident.offer.origin
  local handler = origin and M.origin_operation_handlers[origin]
  return handler and handler(incident, phase, response) == true or false
end

local function sorted_copy (values)
  local result = vim.deepcopy(values or {})
  table.sort(result)
  return result
end

local function equal_lists (left, right)
  if #left ~= #right then return false end
  for index, value in ipairs(left) do
    if value ~= right[index] then return false end end
  return true
end

local function normalized_strings (values, label)
  local result, seen = {}, {}
  for _, value in ipairs(values or {}) do
    if type(value) ~= 'string' or value == '' then
      error('Explicit reload ' .. label .. ' must be nonempty strings') end
    if not seen[value] then
      seen[value] = true
      table.insert(result, value) end
  end
  table.sort(result)
  return result
end

local function config_relative_reload_path (value)
  local config = require('skg.config')
  local normalized = vim.fs.normalize(value)
  local absolute = normalized:sub(1, 1) == '/'
    or normalized:match('^%a:[/\\]') ~= nil
  if not absolute then return normalized end
  if not config.config_file_path then
    error('Cannot translate an absolute reload path before Skg init') end
  local root = vim.fs.dirname(vim.fs.normalize(config.config_file_path))
  local relative = vim.fs.relpath(root, normalized)
  if not relative or relative == '..' or relative:match('^%.%.[/\\]') then
    error('Explicit reload path is outside the skgconfig data root: ' .. value)
  end
  return relative
end

local function registered_ids ()
  local result = {}
  for _, buf in ipairs(registry.buffers()) do
    table.insert(result, registry.record(buf).id) end
  return sorted_copy(result)
end

local function field_present (record, key)
  if not sexpr.is_list(record) then return false end
  for _, entry in ipairs(record) do
    if sexpr.is_pair(entry) and not sexpr.is_list(entry.car)
       and sexpr.atom_text(entry.car) == key then return true end
    if sexpr.is_list(entry) and #entry > 0 and not sexpr.is_list(entry[1])
       and sexpr.atom_text(entry[1]) == key then return true end
  end
  return false
end

local function true_field (record, key)
  return payload.field_text(record, key) == 'true'
end

local function nat (record, key)
  local value = payload.field(record, key)
  if type(value) == 'number' and value >= 0 and value == math.floor(value) then
    return value end
  local text = payload.field_text(record, key)
  if text and text:match('^%d+$') then return tonumber(text) end
  error('Maintenance record has invalid ' .. key)
end

local function sha256_valid (value)
  return type(value) == 'string' and #value == 64
    and value:match('^[0-9a-f]+$') ~= nil
end

local function request (name, fields, raw_fields)
  local result = { sexpr.pair(sexpr.symbol('request'), name) }
  for _, entry in ipairs(fields or {}) do
    table.insert(result,
      sexpr.pair(sexpr.symbol(entry[1]), entry[2])) end
  for _, entry in ipairs(raw_fields or {}) do
    table.insert(result, entry) end
  return sexpr.to_string(result) .. '\n'
end

local function submit_later (callback)
  M.defer(function ()
    local ok, error_text = pcall(callback)
    if not ok then
      vim.notify('Skg maintenance failed: ' .. tostring(error_text),
                 vim.log.levels.ERROR) end
  end)
end

local function warn (message)
  vim.notify(message, vim.log.levels.WARN)
end

local function fail_request (phase, message)
  return function (reason)
    local incident = state.maintenance_client_incident
    if incident then incident.phase = phase end
    warn(message .. ': ' .. tostring(reason))
  end
end

function M.lock_census_sha256 (ids)
  ids = sorted_copy(ids)
  local bytes = table.concat(ids, '\0')
  if #ids > 0 then bytes = bytes .. '\0' end
  return vim.fn.sha256(bytes)
end

function M.set_handshake_summary (maintenance_state, epoch)
  local prior = state.maintenance_state or {}
  state.maintenance_state = {
    epoch = epoch or prior.epoch,
    state = maintenance_state,
    census_required = prior.census_required,
  }
end

function M.adopt_handshake_epoch ()
  local summary = state.maintenance_state
  if not summary or summary.state ~= 'active' then return end
  local epoch = summary.epoch
  if type(epoch) ~= 'number' or epoch < 0 or epoch ~= math.floor(epoch) then
    error('Active maintenance handshake has no valid epoch') end
  local incident = state.maintenance_client_incident
  if incident and incident.epoch ~= epoch then
    error('Maintenance handshake changed the active client epoch') end
  for _, buf in ipairs(registry.buffers()) do
    registry.lock_for_maintenance(buf, epoch) end
end

function M.handle_census_stale (buffer_ids)
  local summary = state.maintenance_state
  local incident = state.maintenance_client_incident
  local protected = {}
  if summary and (summary.state == 'active' or summary.state == 'terminal')
     and incident then
    for _, buffer_id in ipairs(incident.registered_buffer_ids or {}) do
      protected[tostring(buffer_id)] = true end
  end
  local ordinary = {}
  for _, buffer_id in ipairs(buffer_ids or {}) do
    if not protected[tostring(buffer_id)] then
      table.insert(ordinary, buffer_id) end
  end
  registry.mark_census_buffers_stale(ordinary)
end

function M.resume_after_census ()
  local summary = state.maintenance_state
  if summary and (summary.state == 'active' or summary.state == 'terminal') then
    M.status(true) end
end

local function require_client_incident (incident_id, epoch)
  local incident = state.maintenance_client_incident
  if not incident or incident.incident_id ~= incident_id
     or incident.epoch ~= epoch then
    error('Maintenance response names another incident or epoch') end
  return incident
end

function M.record_selection (response)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance selection arrived without client state')
  local values = {
    g1_graph_generation = nat(response, 'g1-graph-generation'),
    g1_manifest_revision = nat(response, 'g1-manifest-revision'),
    tantivy_generation = nat(response, 'tantivy-generation'),
    server_evidence_sha256 = payload.field_text(
      response, 'server-evidence-sha256'),
  }
  if not sha256_valid(values.server_evidence_sha256) then
    error('Maintenance selection authority is incomplete') end
  for key, value in pairs(values) do
    if incident[key] ~= nil and incident[key] ~= value then
      error('Maintenance selection changed its durable authority') end
    incident[key] = value
  end
  incident.phase = 'presenting'
  return incident
end

local function status_challenge (response)
  local challenge = {
    operation = payload.field_text(response, 'operation'),
    pids = payload.string_list(payload.field(response, 'pids')),
    prompt = payload.field_text(response, 'prompt'),
  }
  if not challenge.operation or #challenge.pids == 0 or not challenge.prompt then
    error('Maintenance scalar challenge is incomplete') end
  return challenge
end

function M.prompt_scalar (challenge)
  local answer = vim.fn.confirm(challenge.prompt,
    '&Approve exact release\n&Keep locked', 2)
  if answer == 1 then M.approve_scalar_release()
  else
    vim.notify('Skg maintenance remains locked; approve the stored scalar '
      .. 'challenge to resume.')
  end
end

function M.handle_selection_response (_payload_text, response)
  local status = payload.field_text(response, 'status')
  local incident = assert(state.maintenance_client_incident,
    'Maintenance selection arrived without client state')
  if field_present(response, 'incident-id')
     or field_present(response, 'maintenance-epoch') then
    require_client_incident(
      payload.field_text(response, 'incident-id'),
      nat(response, 'maintenance-epoch'))
  end
  if status ~= 'archive-ready' then M.record_selection(response) end
  if status == 'needs-scalar-authorization' then
    local challenge = status_challenge(response)
    incident.phase = 'awaiting-scalar-authorization'
    incident.scalar_challenge = challenge
    submit_later(function () M.prompt_scalar(challenge) end)
  elseif status == 'candidate-selected' then
    incident.scalar_challenge = nil
    M.install_settlements(payload.field(response, 'view-settlements') or {})
  elseif status == 'archive-ready' then
    incident.phase = 'origin-operation-required'
    if not M.dispatch_origin_operation('archive-ready', response) then
      vim.notify('Skg maintenance archive is durable; its origin operation '
        .. 'is next')
    end
  else
    error('Unexpected maintenance selection status: ' .. tostring(status))
  end
end

function M.handle_origin_started (_payload_text, response)
  local incident = require_client_incident(
    payload.field_text(response, 'incident-id'),
    nat(response, 'maintenance-epoch'))
  if payload.field_text(response, 'status') ~= 'origin-operation-started' then
    error('Server did not start the explicit maintenance origin') end
  incident.phase = 'waiting-for-origin-observation'
  vim.notify('Skg is observing the exact partial-reload targets')
end

function M.run_explicit_origin (incident)
  incident = incident or assert(state.maintenance_client_incident,
    'No explicit partial-reload incident is ready to run')
  if not incident.offer or incident.offer.origin ~= 'explicit-partial-reload'
     or not incident.incident_id or incident.epoch == nil then
    error('No explicit partial-reload incident is ready to run') end
  incident.phase = 'origin-operation-start-pending'
  state.register_response_handler(
    'maintenance-status', M.handle_origin_started, true)
  state.set_request_failure_handler(fail_request(
    'origin-operation-start-pending',
    'Explicit reload worker was not started'))
  client.submit_request(request('run maintenance origin', {
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

function M.explicit_origin_operation_handler (incident, phase, _response)
  if phase ~= 'archive-ready' and phase ~= 'final-observation' then
    return false end
  submit_later(function () M.run_explicit_origin(incident) end)
  return true
end

M.register_origin_operation_handler(
  'explicit-partial-reload', M.explicit_origin_operation_handler)

function M.send_archive_ready ()
  local incident = assert(state.maintenance_client_incident,
    'no client-known maintenance incident')
  if not incident.archive then error('Maintenance has no initial archive') end
  state.register_response_handler(
    'maintenance-status', M.handle_selection_response, true)
  state.set_request_failure_handler(fail_request(
    'archive-ready-ack-pending', 'Initial archive ACK was not delivered'))
  client.submit_request(request('maintenance archive ready', {
    { 'maintenance-epoch', incident.epoch },
    { 'lock-census-sha256', incident.lock_census_sha256 },
    { 'manifest-sha256', incident.archive.manifest_sha256 },
  }), nil, incident.incident_id)
end

function M.approve_scalar_release ()
  local incident = assert(state.maintenance_client_incident,
    'Skg has no client-known maintenance scalar challenge')
  local challenge = incident.scalar_challenge
  if not challenge or not incident.incident_id or not incident.epoch
     or not challenge.pids or #challenge.pids == 0 then
    error('Skg has no client-known maintenance scalar challenge') end
  state.register_response_handler(
    'maintenance-status', M.handle_selection_response, true)
  state.set_request_failure_handler(fail_request(
    'awaiting-scalar-authorization',
    'Maintenance scalar approval was not acknowledged'))
  client.submit_request(request('approve maintenance scalar release', {
    { 'maintenance-epoch', incident.epoch },
    { 'allow-ugly-telescopes', challenge.pids },
  }), nil, incident.incident_id)
end

function M.validate_settlements (settlements, expected_ids)
  if not sexpr.is_list(settlements) then
    error('Maintenance view settlements are malformed') end
  local allowed = {
    ['retirement-ack'] = true, ['release-ack'] = true,
    ['application-ack'] = true, ['close-ack'] = true,
  }
  local seen, ids = {}, {}
  for _, settlement in ipairs(settlements) do
    local buffer_id = payload.field_text(settlement, 'buffer-id')
    local required = payload.field_text(settlement, 'required-ack')
    if not buffer_id or not allowed[required] or seen[buffer_id] then
      error('Maintenance contains a duplicate or invalid settlement') end
    seen[buffer_id] = true
    table.insert(ids, buffer_id)
  end
  if not equal_lists(sorted_copy(ids), sorted_copy(expected_ids)) then
    error('Maintenance settlement inventory differs from the locked census')
  end
  return settlements
end

local function without_ack (settlement)
  local result = {}
  for _, entry in ipairs(settlement) do
    local key
    if sexpr.is_pair(entry) and not sexpr.is_list(entry.car) then
      key = sexpr.atom_text(entry.car)
    elseif sexpr.is_list(entry) and #entry > 0
       and not sexpr.is_list(entry[1]) then
      key = sexpr.atom_text(entry[1])
    end
    if key ~= 'acknowledged' then table.insert(result, entry) end
  end
  return result
end

local function locally_applied (incident, buffer_id)
  local applied = incident.locally_applied or {}
  if applied[buffer_id] == true then return true end
  for _, value in ipairs(applied) do
    if value == buffer_id then return true end end
  return false
end

local function mark_locally_applied (incident, buffer_id)
  incident.locally_applied = incident.locally_applied or {}
  incident.locally_applied[buffer_id] = true
end

function M.require_stable_settlements (old, new)
  if not old then return end
  for _, settlement in ipairs(new) do
    local buffer_id = payload.field_text(settlement, 'buffer-id')
    local prior
    for _, candidate in ipairs(old) do
      if payload.field_text(candidate, 'buffer-id') == buffer_id then
        prior = candidate
        break
      end
    end
    if not prior or not vim.deep_equal(
        without_ack(prior), without_ack(settlement)) then
      error('Maintenance changed the durable settlement for ' .. buffer_id)
    end
  end
end

function M.install_settlements (settlements)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance settlements arrived without client state')
  M.validate_settlements(settlements, incident.registered_buffer_ids)
  M.require_stable_settlements(incident.settlements, settlements)
  local pending, acknowledged = {}, {}
  for _, settlement in ipairs(settlements) do
    local buffer_id = payload.field_text(settlement, 'buffer-id')
    if true_field(settlement, 'acknowledged') then
      if not locally_applied(incident, buffer_id) then
        error('Server acknowledged a settlement not applied locally') end
      table.insert(acknowledged, settlement)
    else
      table.insert(pending, settlement)
    end
  end
  incident.settlements = settlements
  incident.pending_settlements = pending
  incident.acknowledged_settlements = acknowledged
  incident.in_flight_settlement = nil
  incident.phase = 'settling-views'
  submit_later(M.settle_next)
end

local function application_from (settlement)
  local application = payload.field(settlement, 'application')
  if not sexpr.is_list(application) then
    error('Maintenance application settlement has no rendered offer') end
  return application
end

function M.apply_settlement (settlement)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance settlement arrived without client state')
  local buffer_id = payload.field_text(settlement, 'buffer-id')
  local requirement = payload.field_text(settlement, 'required-ack')
  local buf = registry.find_by_id(buffer_id)
  if requirement == 'application-ack' then
    local application = application_from(settlement)
    registry.apply_maintenance_rendered_view(buf, settlement, application,
      incident.epoch, incident.g1_graph_generation)
    for _, message in ipairs(payload.string_list(
        payload.field(application, 'warnings'))) do
      warn(message) end
  elseif requirement == 'release-ack' then
    if not buf then
      error('Maintenance cannot release missing buffer ' .. buffer_id) end
    registry.release_across_maintenance(
      buf, settlement, incident.epoch, incident.g1_graph_generation)
  elseif requirement == 'retirement-ack' then
    if buf then
      registry.retire_for_maintenance(
        buf, settlement, incident.epoch, incident.incident_id)
    else
      warn('Retired buffer ' .. buffer_id
        .. ' is absent; its recovery archive remains durable')
    end
  elseif requirement == 'close-ack' then
    registry.close_for_maintenance(buf, settlement, incident.epoch)
  else
    error('Unknown maintenance settlement action: ' .. tostring(requirement))
  end
end

function M.ack_fields (settlement)
  local uri = payload.field_text(settlement, 'view-uri')
  if not uri or uri == 'nil' then uri = 'none' end
  local fields = {
    { 'buffer-id', payload.field_text(settlement, 'buffer-id') },
    { 'required-ack', payload.field_text(settlement, 'required-ack') },
    { 'view-uri', uri },
    { 'base-graph-generation', nat(settlement, 'base-graph-generation') },
    { 'base-presentation-generation',
      nat(settlement, 'base-presentation-generation') },
    { 'base-server-revision', nat(settlement, 'base-server-revision') },
    { 'base-application-token', nat(settlement, 'base-application-token') },
  }
  if payload.field_text(settlement, 'required-ack') == 'application-ack' then
    local application = application_from(settlement)
    for _, name in ipairs({
      'content-sha256', 'resulting-graph-generation',
      'resulting-presentation-generation', 'resulting-server-revision',
      'resulting-application-token',
    }) do
      local value = name == 'content-sha256'
        and payload.field_text(application, name) or nat(application, name)
      table.insert(fields, { name, value })
    end
  end
  return fields
end

function M.handle_settlement_ack (_payload_text, response)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance settlement ACK arrived without client state')
  local settlement = incident.in_flight_settlement
  if not settlement then
    error('Maintenance settlement ACK arrived without an in-flight action') end
  local buffer_id = payload.field_text(settlement, 'buffer-id')
  if buffer_id ~= payload.field_text(response, 'buffer-id')
     or payload.field_text(settlement, 'required-ack')
        ~= payload.field_text(response, 'required-ack') then
    error('Maintenance settlement ACK response changed identity') end
  local first = incident.pending_settlements[1]
  if not first or buffer_id ~= payload.field_text(first, 'buffer-id') then
    error('Maintenance settlement response arrived out of order') end
  table.insert(incident.acknowledged_settlements, settlement)
  table.remove(incident.pending_settlements, 1)
  incident.in_flight_settlement = nil
  incident.phase = 'settling-views'
  submit_later(M.settle_next)
end

function M.send_settlement_ack (settlement)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance settlement ACK has no client state')
  state.register_response_handler(
    'maintenance-status', M.handle_settlement_ack, true)
  state.set_request_failure_handler(fail_request(
    'view-settlement-ack-pending',
    'Maintenance settlement ACK was not delivered'))
  client.submit_request(request('maintenance view settled',
    vim.list_extend({ { 'maintenance-epoch', incident.epoch } },
      M.ack_fields(settlement))), nil, incident.incident_id)
end

function M.settle_next ()
  local incident = state.maintenance_client_incident
  if not incident or incident.phase ~= 'settling-views' then return end
  local settlement = incident.pending_settlements[1]
  if not settlement then
    M.resume_finalization()
    return
  end
  local buffer_id = payload.field_text(settlement, 'buffer-id')
  local ok, error_text = pcall(function ()
    if not locally_applied(incident, buffer_id) then
      M.apply_settlement(settlement)
      mark_locally_applied(incident, buffer_id)
    end
    incident.in_flight_settlement = settlement
    M.send_settlement_ack(settlement)
  end)
  if not ok then
    incident.phase = 'view-settlement-blocked'
    vim.notify('Maintenance left buffer ' .. tostring(buffer_id)
      .. ' locked: ' .. tostring(error_text), vim.log.levels.ERROR)
  end
end

function M.resume_finalization ()
  local incident = assert(state.maintenance_client_incident,
    'Maintenance finalization has no client state')
  if incident.final_archive then M.send_final_archive_ack()
  else M.request_evidence() end
end

function M.request_evidence ()
  local incident = assert(state.maintenance_client_incident,
    'Maintenance evidence request has no client state')
  if not incident.incident_id or not incident.epoch
     or not sha256_valid(incident.server_evidence_sha256) then
    error('Maintenance selection has no valid server evidence identity') end
  incident.phase = 'requesting-evidence'
  state.register_response_handler(
    'maintenance-evidence', M.handle_evidence, true)
  state.set_request_failure_handler(fail_request(
    'evidence-request-pending', 'Maintenance evidence was not delivered'))
  client.submit_request(request('maintenance evidence', {
    { 'maintenance-epoch', incident.epoch },
    { 'server-evidence-sha256', incident.server_evidence_sha256 },
  }), nil, incident.incident_id)
end

function M.handle_evidence (_payload_text, descriptor, opaque_bytes)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance evidence arrived without client state')
  require_client_incident(incident.incident_id, incident.epoch)
  if payload.field_text(descriptor, 'incident-id') ~= incident.incident_id
     or nat(descriptor, 'maintenance-epoch') ~= incident.epoch then
    error('Maintenance evidence names another incident') end
  local ok, result = pcall(archive.finalize,
    incident.archive, descriptor, opaque_bytes, incident.settlements)
  if not ok then
    incident.phase = 'archive-finalization-failed'
    vim.notify('Maintenance recovery archive could not be finalized: '
      .. tostring(result), vim.log.levels.ERROR)
    error(result, 0)
  end
  incident.evidence = descriptor
  incident.final_archive = result
  incident.phase = 'archive-finalized-locally'
  submit_later(M.send_final_archive_ack)
end

function M.send_final_archive_ack ()
  local incident = assert(state.maintenance_client_incident,
    'Maintenance final archive ACK has no client state')
  local final = assert(incident.final_archive,
    'Maintenance has no finalized client archive')
  state.register_response_handler(
    'maintenance-status', M.handle_final_archive_ack, true)
  state.set_request_failure_handler(fail_request(
    'archive-final-ack-pending', 'Final archive ACK was not delivered'))
  client.submit_request(request('maintenance archive finalized', {
    { 'maintenance-epoch', incident.epoch },
    { 'manifest-sha256', final.manifest_sha256 },
    { 'transfer-manifest-sha256', final.transfer_manifest_sha256 },
    { 'artifact-bytes-sha256', final.artifact_bytes_sha256 },
  }), nil, incident.incident_id)
end

function M.handle_final_archive_ack (_payload_text, response)
  local incident = assert(state.maintenance_client_incident,
    'Final archive ACK arrived without client state')
  local final = assert(incident.final_archive,
    'Final archive ACK arrived without local archive')
  if payload.field_text(response, 'status') ~= 'archive-finalized'
     or payload.field_text(response, 'manifest-sha256')
        ~= final.manifest_sha256
     or payload.field_text(response, 'transfer-manifest-sha256')
        ~= final.transfer_manifest_sha256
     or payload.field_text(response, 'artifact-bytes-sha256')
        ~= final.artifact_bytes_sha256 then
    error('Server did not acknowledge the exact final archive') end
  incident.phase = 'completing'
  submit_later(M.send_complete)
end

function M.send_complete ()
  local incident = assert(state.maintenance_client_incident,
    'Maintenance completion has no client state')
  local final = assert(incident.final_archive,
    'Maintenance completion has no final archive')
  state.register_response_handler(
    'maintenance-status', M.handle_terminal, true)
  state.set_request_failure_handler(fail_request(
    'completion-pending', 'Maintenance completion reply was lost'))
  client.submit_request(request('complete maintenance', {
    { 'maintenance-epoch', incident.epoch },
    { 'manifest-sha256', final.manifest_sha256 },
  }), nil, incident.incident_id)
end

function M.handle_terminal (_payload_text, response)
  local incident = assert(state.maintenance_client_incident,
    'Server has terminal maintenance unknown to this client')
  local final = assert(incident.final_archive,
    'Terminal maintenance has no local final archive')
  local unlock_ids = payload.string_list(
    payload.field(response, 'unlock-buffer-ids'))
  if payload.field_text(response, 'status') ~= 'terminal'
     or payload.field_text(response, 'incident-id') ~= incident.incident_id
     or nat(response, 'maintenance-epoch') ~= incident.epoch
     or payload.field_text(response, 'disposition') ~= 'completed'
     or payload.field_text(response, 'manifest-sha256')
        ~= final.manifest_sha256
     or nat(response, 'selected-graph-generation')
        ~= incident.g1_graph_generation
     or nat(response, 'selected-manifest-revision')
        ~= incident.g1_manifest_revision
     or not equal_lists(sorted_copy(unlock_ids),
       sorted_copy(incident.registered_buffer_ids)) then
    error('Maintenance terminal instruction changed its exact authority') end
  for _, buffer_id in ipairs(unlock_ids) do
    local buf = registry.find_by_id(buffer_id)
    if buf then registry.unlock_after_maintenance(buf, incident.epoch) end
  end
  incident.phase = 'terminal-received'
  incident.terminal = response
  M.set_handshake_summary('terminal', incident.epoch)
  local config = require('skg.config')
  config.store_state = config.store_state or {}
  config.store_state.graph_generation = nat(
    response, 'selected-graph-generation')
  config.store_state.manifest_revision = nat(
    response, 'selected-manifest-revision')
  if incident.terminal_callback and not incident.terminal_callback_fired then
    incident.terminal_callback(response)
    incident.terminal_callback_fired = true
  end
  submit_later(M.send_terminal_ack)
end

function M.send_terminal_ack ()
  local incident = assert(state.maintenance_client_incident,
    'Terminal maintenance ACK has no client state')
  state.register_response_handler(
    'maintenance-status', M.handle_terminal_ack, true)
  state.set_request_failure_handler(fail_request(
    'terminal-received', 'Terminal maintenance ACK was not delivered'))
  client.submit_request(request('acknowledge terminal maintenance', {
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

function M.handle_terminal_ack (_payload_text, response)
  if payload.field_text(response, 'status') ~= 'idle' then
    error('Server did not enter idle after terminal maintenance ACK') end
  M.finish_idle()
end

function M.finish_idle ()
  local incident = assert(state.maintenance_client_incident,
    'Server became idle without client maintenance state')
  if incident.phase ~= 'terminal-received' then
    error('Server became idle before the client received terminal authority')
  end
  local path = incident.final_archive and incident.final_archive.path or '?'
  M.set_handshake_summary('idle', incident.epoch)
  state.maintenance_client_incident = nil
  state.pending_maintenance_offer = nil
  vim.notify('Skg maintenance complete; recovery archive: ' .. path)
end

function M.resume_active (response)
  local incident_id = payload.field_text(response, 'active-incident-id')
  local epoch = nat(response, 'maintenance-epoch')
  local phase = payload.field_text(response, 'phase')
  local incident = require_client_incident(incident_id, epoch)
  local origin = payload.field_text(response, 'origin')
  local paths = payload.string_list(payload.field(response, 'requested-paths'))
  local ids = payload.string_list(payload.field(response, 'requested-ids'))
  if incident.offer and incident.offer.origin
     and incident.offer.origin ~= origin then
    error('Maintenance status changed its origin') end
  if incident.requested_paths
     and not equal_lists(incident.requested_paths, paths) then
    error('Maintenance status changed its requested paths') end
  if incident.requested_ids
     and not equal_lists(incident.requested_ids, ids) then
    error('Maintenance status changed its requested IDs') end
  incident.requested_paths = paths
  incident.requested_ids = ids
  incident.offer = incident.offer or {}
  incident.offer.origin = origin
  M.set_handshake_summary('active', epoch)
  if field_present(response, 'g1-graph-generation') then
    M.record_selection(response) end
  local settlements = payload.field(response, 'view-settlements')
  if settlements ~= nil then
    M.install_settlements(settlements)
  elseif field_present(response, 'scalar-approved')
     and not true_field(response, 'scalar-approved') then
    local challenge = status_challenge(response)
    incident.phase = 'awaiting-scalar-authorization'
    incident.scalar_challenge = challenge
    submit_later(function () M.prompt_scalar(challenge) end)
  elseif field_present(response, 'scalar-approved')
     and true_field(response, 'scalar-approved') and phase == 'presenting' then
    incident.scalar_challenge = status_challenge(response)
    incident.phase = 'resuming-approved-scalar'
    submit_later(M.approve_scalar_release)
  elseif phase == 'finalizing-archive' then
    incident.phase = 'finalizing-archive'
    submit_later(M.resume_finalization)
  elseif phase == 'preparing-archive' then
    incident.phase = 'preparing-archive'
    if incident.archive then submit_later(M.send_archive_ready)
    else submit_later(M.publish_initial) end
  elseif phase == 'archive-ready' or phase == 'running-external-mutation'
      or phase == 'final-observation' then
    incident.phase = phase == 'archive-ready'
      and 'origin-operation-required' or 'waiting-for-origin-observation'
    if not M.dispatch_origin_operation(phase, response) then
      vim.notify('Skg maintenance ' .. incident_id
        .. ' awaits origin ' .. tostring(origin)
        .. ' in server phase ' .. phase)
    end
  elseif phase == 'blocked-invalid-after-mutation'
      or phase == 'blocked-store-health' then
    incident.phase = 'server-blocked'
    vim.notify('Maintenance ' .. incident_id
      .. ' remains locked in server phase ' .. phase, vim.log.levels.ERROR)
  else
    incident.phase = 'waiting-for-server'
    vim.notify('Skg maintenance ' .. incident_id
      .. ' is in server phase ' .. tostring(phase))
  end
end

function M.handle_status (payload_text, response)
  local status = payload.field_text(response, 'status')
  if status == 'active' then
    M.resume_active(response)
  elseif status == 'terminal' then
    M.handle_terminal(payload_text, response)
  elseif status == 'idle' then
    if state.maintenance_client_incident then M.finish_idle()
    else
      M.set_handshake_summary('idle')
      vim.notify('Skg maintenance is idle')
    end
  else
    vim.notify('Skg maintenance: ' .. payload_text)
  end
end

local function approve_undo_waiver (incident, buffer_key, reason)
  state.register_response_handler('maintenance-status', function ()
    incident.undo_waivers[buffer_key] = reason
    submit_later(M.publish_initial)
  end, true)
  client.submit_request(request('approve undo waiver', {
    { 'maintenance-epoch', incident.epoch },
    { 'buffer-key', buffer_key },
    { 'reason', reason },
  }), nil, incident.incident_id)
end

local function send_undo_failure (incident, failure)
  incident.undo_failure = {
    buffer_key = failure.buffer_key, reason = failure.reason,
  }
  state.register_response_handler('maintenance-status',
    function (_payload_text, response)
      local buffer_key = payload.field_text(response, 'buffer-key')
      local reason = payload.field_text(response, 'reason')
      submit_later(function ()
        local answer = vim.fn.confirm(string.format(
          'Native undo could not be archived for %s:\n%s\nContinue only '
            .. 'with exact text and diff recovery?', buffer_key, reason),
          '&Continue\n&Cancel', 2)
        if answer == 1 then
          approve_undo_waiver(incident, buffer_key, reason)
        else M.cancel() end
      end)
    end, true)
  client.submit_request(request('maintenance archive failed', {
    { 'maintenance-epoch', incident.epoch },
    { 'buffer-key', failure.buffer_key },
    { 'reason', failure.reason },
  }), nil, incident.incident_id)
end

function M.publish_initial ()
  local incident = assert(state.maintenance_client_incident,
    'no client-known maintenance incident')
  local ok, result = pcall(archive.publish_initial,
    incident.offer, registry.buffers(), {
      undo_waivers = incident.undo_waivers,
    })
  if ok then
    incident.archive = result
    submit_later(M.send_archive_ready)
  elseif type(result) == 'table'
      and result.kind == 'native-undo-failure' then
    submit_later(function () send_undo_failure(incident, result) end)
  else
    vim.notify('Initial recovery archive failed before risky work: '
      .. tostring(result) .. '\nIncomplete staging data was retained.',
      vim.log.levels.ERROR)
    submit_later(M.cancel)
  end
end

local function handle_bootstrap (
    _payload_text, response, terminal_callback, origin_context)
  local offered_ids = sorted_copy(payload.string_list(
    payload.field(response, 'registered-buffer-ids')))
  local actual_ids = registered_ids()
  local incident = {
    incident_id = assert(payload.field_text(
      response, 'allocated-incident-id'), 'offer has no incident ID'),
    epoch = assert(payload.field(response, 'maintenance-epoch'),
      'offer has no maintenance epoch'),
    phase = 'preparing-archive',
    requested_paths = payload.string_list(
      payload.field(response, 'requested-paths')),
    requested_ids = payload.string_list(
      payload.field(response, 'requested-ids')),
    origin_context = origin_context,
    terminal_callback = terminal_callback,
    terminal_callback_fired = false,
    registered_buffer_ids = offered_ids,
    undo_waivers = {},
    locally_applied = {},
    offer = {
      incident_id = payload.field_text(response, 'allocated-incident-id'),
      epoch = payload.field(response, 'maintenance-epoch'),
      origin = payload.field_text(response, 'origin'),
      started_at_utc = payload.field_text(response, 'started-at-utc'),
      archive_name = payload.field_text(response, 'archive-directory-name'),
      source_set = payload.field_text(response, 'source-set'),
      graph_generation = payload.field(response, 'g0-graph-generation'),
      manifest_revision = payload.field(response, 'g0-manifest-revision'),
    },
  }
  state.maintenance_client_incident = incident
  if not equal_lists(offered_ids, actual_ids) then
    vim.notify('Maintenance census changed before locking; cancelling safely.',
               vim.log.levels.ERROR)
    submit_later(M.cancel)
    return
  end
  local checksum = M.lock_census_sha256(actual_ids)
  if checksum ~= payload.field_text(response, 'lock-census-sha256') then
    vim.notify('Maintenance census checksum does not match; cancelling safely.',
               vim.log.levels.ERROR)
    submit_later(M.cancel)
    return
  end
  incident.lock_census_sha256 = checksum
  M.set_handshake_summary('active', incident.epoch)
  for _, buf in ipairs(registry.buffers()) do
    registry.lock_for_maintenance(buf, incident.epoch) end
  M.publish_initial()
end

---Begin one durable maintenance incident.
---ORIGIN_CONTEXT is opaque process-local adapter state. ORIGIN_FIELDS are
---complete s-expression field forms appended to the bootstrap request.
function M.begin (
    origin, candidate_id, paths, ids, terminal_callback, origin_context,
    origin_fields)
  state.register_response_handler('maintenance-offer',
    function (payload_text, response)
      handle_bootstrap(
        payload_text, response, terminal_callback, origin_context) end,
    true)
  local fields = {
    { 'origin', origin },
    { 'candidate-id', candidate_id or 'none' },
  }
  if paths and #paths > 0 then table.insert(fields, { 'paths', paths }) end
  if ids and #ids > 0 then table.insert(fields, { 'ids', ids }) end
  client.submit_request(request('begin maintenance', fields, origin_fields))
end

local function report_explicit_outcomes (response)
  local outcomes = payload.field(response, 'requested-id-outcomes') or {}
  local rejected = {}
  for _, outcome in ipairs(outcomes) do
    if payload.field_text(outcome, 'status') == 'rejected' then
      table.insert(rejected, string.format('%s (%s)',
        payload.field_text(outcome, 'requested-id') or '?',
        payload.field_text(outcome, 'reason') or 'not acknowledged'))
    end
  end
  if #rejected == 0 then
    vim.notify('Skg explicit partial reload completed.')
  else
    vim.notify('Skg partial reload completed; unresolved IDs: '
      .. table.concat(rejected, ', '), vim.log.levels.WARN)
  end
end

---Begin a durable explicit partial reload by config-relative paths or G0 IDs.
---Absolute local paths beneath the local skgconfig directory are translated
---to config-relative spelling before crossing a host/container boundary.
---@param options table { paths?: string[], ids?: string[], on_terminal?: fun(any) }
---@return boolean started
function M.reload_targets (options)
  options = options or {}
  local ids = normalized_strings(options.ids, 'IDs')
  local paths = {}
  for _, value in ipairs(normalized_strings(options.paths, 'paths')) do
    table.insert(paths, config_relative_reload_path(value)) end
  paths = normalized_strings(paths, 'paths')
  if #paths == 0 and #ids == 0 then
    error('Explicit partial reload requires at least one path or ID') end
  local dirty = 0
  for _, buf in ipairs(registry.buffers()) do
    if registry.dirty(buf) then dirty = dirty + 1 end end
  if dirty > 0 then
    local answer = vim.fn.confirm(string.format(
      'Archive %d dirty Skg view(s) before the partial reload? Impacted '
        .. 'views will become detached recovery buffers.', dirty),
      '&Continue\n&Cancel', 2)
    if answer ~= 1 then return false end
  end
  M.begin('explicit-partial-reload', nil, paths, ids,
    options.on_terminal or report_explicit_outcomes)
  return true
end

function M.reload_ids (ids, on_terminal)
  return M.reload_targets({ ids = ids, on_terminal = on_terminal })
end

function M.reload_paths (paths, on_terminal)
  return M.reload_targets({ paths = paths, on_terminal = on_terminal })
end

function M.reconcile_pending ()
  local offer = state.pending_maintenance_offer
  if not offer then error('Skg has no pending valid disk candidate') end
  local answer = vim.fn.confirm(string.format(
    'Archive dirty work and reconcile candidate %s (%s)?',
    offer.candidate_id,
    #offer.changed_ids > 0 and table.concat(offer.changed_ids, ', ')
      or 'semantic changes'), '&Reconcile\n&Cancel', 2)
  if answer == 1 then
    M.begin('pending-reconciliation', offer.candidate_id) end
end

function M.server_offer_handler (_payload_text, response)
  local offer = {
    candidate_id = payload.field_text(response, 'candidate-id'),
    changed_ids = payload.string_list(
      payload.field(response, 'changed-primary-ids')),
  }
  state.pending_maintenance_offer = offer
  submit_later(function ()
    local answer = vim.fn.confirm(string.format(
      'Skg observed disk changes to %s. Archive dirty work and reconcile now?',
      #offer.changed_ids > 0 and table.concat(offer.changed_ids, ', ')
        or 'the selected corpus'), '&Reconcile\n&Later', 2)
    if answer == 1 then
      M.begin('pending-reconciliation', offer.candidate_id) end
  end)
end

function M.server_status_handler (payload_text, response)
  local status = payload.field_text(response, 'status')
  if status == 'candidate-selected'
     or status == 'needs-scalar-authorization' then
    if not field_present(response, 'incident-id')
       or not field_present(response, 'maintenance-epoch') then
      error('Asynchronous maintenance selection has no exact envelope') end
    M.handle_selection_response(payload_text, response)
  elseif status == 'origin-operation-failed' then
    local incident = require_client_incident(
      payload.field_text(response, 'incident-id'),
      nat(response, 'maintenance-epoch'))
    incident.phase = 'server-blocked'
    vim.notify(string.format(
      'Explicit reload remains locked in server phase %s: %s',
      payload.field_text(response, 'phase') or '?',
      payload.field_text(response, 'error') or 'unknown error'),
      vim.log.levels.ERROR)
  elseif status == 'active' or status == 'terminal' or status == 'idle' then
    M.handle_status(payload_text, response)
  else
    local reason = payload.field_text(response, 'pending-reason')
    if reason then warn('Skg disk observation is pending: ' .. reason)
    else vim.notify('Skg maintenance: ' .. payload_text) end
  end
end

function M.cancel ()
  local incident = assert(state.maintenance_client_incident,
    'no client-known maintenance incident to cancel')
  state.register_response_handler('maintenance-status',
    function (_payload_text, response)
      local epoch = nat(response, 'unlock-maintenance-epoch')
      for _, buf in ipairs(registry.buffers()) do
        registry.unlock_after_maintenance(buf, epoch) end
      state.maintenance_client_incident = nil
      vim.notify('Skg maintenance cancelled before archive publication.')
    end, true)
  state.set_request_failure_handler(fail_request(
    'cancellation-pending', 'Maintenance cancellation was not acknowledged'))
  client.submit_request(request('cancel maintenance', {
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

function M.status (silent)
  state.register_response_handler(
    'maintenance-status', M.handle_status, true)
  if not silent then
    state.set_request_failure_handler(function (reason)
      warn('Maintenance status was not delivered: ' .. tostring(reason))
    end)
  end
  client.submit_request(request('maintenance status'))
end

return M
