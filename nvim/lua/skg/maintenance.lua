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
    local record = registry.record(buf)
    if record.view_write_authority == 'editable' then
      table.insert(result, record.id) end
  end
  return sorted_copy(result)
end

local function refuse_modified_raw_files ()
  local dirty = {}
  for _, buf in ipairs(registry.buffers()) do
    local record = registry.record(buf)
    if record.kind == 'raw-skg-file' and vim.bo[buf].modified then
      local name = vim.api.nvim_buf_get_name(buf)
      table.insert(dirty, name ~= '' and name or ('buffer ' .. tostring(buf)))
    end
  end
  if #dirty > 0 then
    table.sort(dirty)
    error('Maintenance refuses modified raw .skg buffers: '
      .. table.concat(dirty, ', '))
  end
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

local function submit_later (callback, incident_id)
  incident_id = incident_id
    or (state.maintenance_client_incident
      and state.maintenance_client_incident.incident_id)
  M.defer(function ()
    local ok, error_text = pcall(function ()
      if incident_id then
        return state.with_current_maintenance_incident(incident_id, callback)
      end
      return callback()
    end)
    if not ok then
      vim.notify('Skg maintenance failed: ' .. tostring(error_text),
                 vim.log.levels.ERROR) end
  end)
end

local function register_response_handler (kind, handler, one_shot,
                                           incident_id)
  incident_id = incident_id
    or (state.maintenance_client_incident
      and state.maintenance_client_incident.incident_id)
  if not incident_id then
    return state.register_response_handler(kind, handler, one_shot) end
  state.register_response_handler(kind, function (...)
    return state.with_current_maintenance_incident(incident_id, handler, ...)
  end, one_shot)
end

local function set_request_failure_handler (handler, incident_id)
  incident_id = incident_id
    or (state.maintenance_client_incident
      and state.maintenance_client_incident.incident_id)
  if not incident_id then return state.set_request_failure_handler(handler) end
  state.set_request_failure_handler(function (...)
    return state.with_current_maintenance_incident(incident_id, handler, ...)
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

local function pending_incident (incident_id)
  for _, entry in ipairs(state.pending_incidents or {}) do
    if payload.field_text(entry, 'incident-id') == incident_id then
      return entry end
  end
end

local function pending_incident_ids ()
  local ids, seen = {}, {}
  for _, entry in ipairs(state.pending_incidents or {}) do
    local incident_id = payload.field_text(entry, 'incident-id')
    if incident_id and not seen[incident_id] then
      seen[incident_id] = true
      table.insert(ids, incident_id) end
  end
  return ids
end

local function release_incident_restrictions (incident)
  if not incident or incident.epoch == nil then return end
  for _, buf in ipairs(registry.buffers()) do
    registry.unlock_after_maintenance(buf, incident.epoch) end
end

function M.adopt_handshake_epoch (abandoned_id)
  if abandoned_id and state.lookup_maintenance_incident(abandoned_id) then
    state.with_current_maintenance_incident(abandoned_id, function ()
      release_incident_restrictions(state.maintenance_client_incident)
      state.clear_current_maintenance_incident()
    end)
  end
  local summary = state.maintenance_state
  if not summary then return end
  local epoch = summary.epoch
  local incident = state.maintenance_client_incident
  if summary.state ~= 'active' then
    if (summary.state == 'idle' or summary.state == 'observing'
        or summary.state == 'pending') and incident
       and not pending_incident(incident.incident_id) then
      release_incident_restrictions(incident)
      state.clear_current_maintenance_incident()
    end
    return
  end
  if type(epoch) ~= 'number' or epoch < 0 or epoch ~= math.floor(epoch) then
    error('Active maintenance handshake has no valid epoch') end
  if incident and incident.epoch ~= epoch then
    state.clear_current_maintenance_incident()
  end
  local epoch_id
  for _, entry in ipairs(state.pending_incidents or {}) do
    if payload.field(entry, 'maintenance-epoch') == epoch then
      epoch_id = payload.field_text(entry, 'incident-id')
      break
    end
  end
  for _, buf in ipairs(registry.buffers()) do
    if registry.record(buf).view_write_authority == 'editable' then
      registry.lock_for_maintenance(buf, epoch, epoch_id) end
  end
end

function M.handle_census_stale (buffer_ids)
  local protected = {}
  for _, buf in ipairs(registry.buffers()) do
    local record = registry.record(buf)
    if record and record.maintenance_restrictions
       and next(record.maintenance_restrictions) ~= nil then
      protected[tostring(record.id)] = true end
  end
  local ordinary = {}
  for _, buffer_id in ipairs(buffer_ids or {}) do
    if not protected[tostring(buffer_id)] then
      table.insert(ordinary, buffer_id) end
  end
  registry.mark_census_buffers_stale(ordinary)
end

local function require_client_incident (incident_id, epoch)
  local incident = state.maintenance_client_incident
  if not incident or incident.incident_id ~= incident_id
     or incident.epoch ~= epoch then
    error('Maintenance response names another incident or epoch') end
  return incident
end

function M.resume_after_census (maintenance_incident_id, maintenance_epoch)
  if maintenance_incident_id then
    return state.with_current_maintenance_incident(maintenance_incident_id,
      function ()
        local incident = require_client_incident(maintenance_incident_id, maintenance_epoch)
        if incident.phase == 'awaiting-locked-census' then
          M.send_locked_census()
        else M.status(true, maintenance_incident_id) end
      end)
  end
  local summary = state.maintenance_state or {}
  local ids = pending_incident_ids()
  local current_id
  for _, entry in ipairs(state.pending_incidents or {}) do
    if payload.field(entry, 'maintenance-epoch') == summary.epoch then
      current_id = payload.field_text(entry, 'incident-id')
      break
    end
  end
  if summary.state == 'active' or summary.state == 'terminal' then
    local current = state.maintenance_client_incident
    if current and current.phase == 'awaiting-locked-census' then
      M.send_locked_census()
    else M.status(true, current_id) end
    current_id = current_id or (current and current.incident_id)
    for i = #ids, 1, -1 do
      if ids[i] == current_id then table.remove(ids, i) end end
  end
  for _, incident_id in ipairs(ids) do M.status(true, incident_id) end
end

local function refresh_presentation_buffer_ids (response)
  if not field_present(response, 'presentation-buffer-ids') then return end
  local incident = assert(state.maintenance_client_incident,
    'Presentation census arrived without client state')
  local new = payload.string_list(payload.field(
    response, 'presentation-buffer-ids'))
  incident.presentation_buffer_ids = new
end

-- Keep the frozen writable census separate from post-barrier presentation
-- results. Exposed for reconnect/maintenance tests and status consumers.

function M.refresh_presentation_buffer_ids (response)
  refresh_presentation_buffer_ids(response)
end

function M.record_selection (response)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance selection arrived without client state')
  refresh_presentation_buffer_ids(response)
  local selected_source_set = payload.field_text(response, 'source-set')
  local source_inventory = payload.field(response, 'source-inventory')
  local archive_folder = payload.field_text(
    response, 'maintenance-archive-folder')
  local archive_identity = payload.field_text(
    response, 'maintenance-archive-identity')
  local values = {
    g1_graph_generation = nat(response, 'g1-graph-generation'),
    g1_manifest_revision = nat(response, 'g1-manifest-revision'),
    tantivy_generation = nat(response, 'tantivy-generation'),
    server_evidence_sha256 = payload.field_text(
      response, 'server-evidence-sha256'),
  }
  if not selected_source_set or source_inventory == nil
     or not archive_folder or not archive_identity
     or not sha256_valid(values.server_evidence_sha256) then
    error('Maintenance selection authority is incomplete') end
  for key, value in pairs(values) do
    if incident[key] ~= nil and incident[key] ~= value then
      error('Maintenance selection changed its durable authority') end
    incident[key] = value
  end
  if incident.selected_source_set ~= nil
     and incident.selected_source_set ~= selected_source_set then
    error('Maintenance selection changed its source-set authority') end
  incident.selected_source_set = selected_source_set
  if not state.maintenance_historical_status then
    require('skg.config').install_source_inventory(source_inventory)
    if state.active_source_set_name ~= selected_source_set then
      vim.notify('Skg full rebuild changed source-set from '
        .. tostring(state.active_source_set_name) .. ' to '
        .. selected_source_set)
    end
    state.active_source_set_name = selected_source_set
    vim.g.skg_active_source_set_name = selected_source_set
    state.maintenance_archive_folder = archive_folder
    state.maintenance_archive_identity = archive_identity
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
  if status == 'candidate-selected'
     or status == 'needs-scalar-authorization' then
    M.record_selection(response) end
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
  elseif status == 'view-enrollment-pending' then
    incident.phase = 'waiting-for-view-enrollment'
    vim.notify('Skg maintenance is enrolling a newly opened view')
  else
    error('Unexpected maintenance selection status: ' .. tostring(status))
  end
end

function M.handle_origin_started (_payload_text, response)
  local incident = require_client_incident(
    payload.field_text(response, 'incident-id'),
    nat(response, 'maintenance-epoch'))
  if payload.field_text(response, 'status') ~= 'origin-operation-started' then
    error('Server did not start the server-owned maintenance origin') end
  incident.phase = 'waiting-for-origin-observation'
  if incident.offer.origin == 'full-rebuild' then
    vim.notify('Skg is validating the complete disk before exclusive rebuild')
  else vim.notify('Skg is observing the exact partial-reload targets') end
end

function M.run_explicit_origin (incident)
  incident = incident or assert(state.maintenance_client_incident,
    'No explicit partial-reload incident is ready to run')
  if not incident.offer
     or (incident.offer.origin ~= 'explicit-partial-reload'
         and incident.offer.origin ~= 'full-rebuild')
     or not incident.incident_id or incident.epoch == nil then
    error('No server-owned maintenance origin is ready to run') end
  incident.phase = 'origin-operation-start-pending'
  register_response_handler(
    'maintenance-status', M.handle_origin_started, true)
  set_request_failure_handler(fail_request(
    'origin-operation-start-pending',
    incident.offer.origin .. ' worker was not started'))
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
M.register_origin_operation_handler(
  'full-rebuild', M.explicit_origin_operation_handler)

function M.send_archive_ready ()
  local incident = assert(state.maintenance_client_incident,
    'no client-known maintenance incident')
  if not incident.archive then error('Maintenance has no initial archive') end
  register_response_handler(
    'maintenance-status', M.handle_selection_response, true)
  set_request_failure_handler(fail_request(
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
  register_response_handler(
    'maintenance-status', M.handle_selection_response, true)
  set_request_failure_handler(fail_request(
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
    local resolution = payload.field_text(
      settlement, 'settlement-resolution') or 'pending'
    if not buffer_id or not allowed[required] or seen[buffer_id]
       or (resolution ~= 'pending'
         and resolution ~= 'client-acknowledged'
         and resolution ~= 'census-applied'
         and resolution ~= 'census-absent') then
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
    if key ~= 'acknowledged' and key ~= 'settlement-resolution' then
      table.insert(result, entry) end
  end
  return result
end

local function client_acknowledged_settlement (settlement)
  local result = without_ack(settlement)
  table.insert(result, { sexpr.symbol('acknowledged'), 'true' })
  table.insert(result,
    { sexpr.symbol('settlement-resolution'), 'client-acknowledged' })
  return result
end

local function replace_settlement (records, replacement)
  local buffer_id = payload.field_text(replacement, 'buffer-id')
  for index, record in ipairs(records or {}) do
    if payload.field_text(record, 'buffer-id') == buffer_id then
      records[index] = replacement
      return
    end
  end
  error('Maintenance ACK names an uninstalled settlement')
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
    if prior and not vim.deep_equal(
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
      local resolution = payload.field_text(
        settlement, 'settlement-resolution') or 'client-acknowledged'
      local absent = registry.find_by_id(buffer_id) == nil
      if resolution == 'census-absent' and not absent then
        error('Server closed a settlement for a buffer still in the census')
      elseif resolution ~= 'census-absent' and not incident.adopted
         and not locally_applied(incident, buffer_id) then
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

function M.install_preselection_retirements (retirements)
  local incident = assert(state.maintenance_client_incident,
    'Invalid-disk retirements arrived without client state')
  if not sexpr.is_list(retirements) then
    error('Invalid-disk retirements are malformed') end
  local registered = {}
  for _, buffer_id in ipairs(incident.registered_buffer_ids or {}) do
    registered[buffer_id] = true end
  local seen, pending, acknowledged = {}, {}, {}
  for _, retirement in ipairs(retirements) do
    local buffer_id = payload.field_text(retirement, 'buffer-id')
    if not buffer_id or not registered[buffer_id] or seen[buffer_id]
       or not true_field(retirement, 'dirty')
       or not true_field(retirement, 'impacted')
       or payload.field_text(retirement, 'planned-disposition')
          ~= 'interrupted'
       or payload.field_text(retirement, 'required-ack')
          ~= 'retirement-ack' then
      error('Invalid-disk retirement inventory is inconsistent') end
    seen[buffer_id] = true
    if true_field(retirement, 'acknowledged') then
      local resolution = payload.field_text(
        retirement, 'settlement-resolution') or 'client-acknowledged'
      local absent = registry.find_by_id(buffer_id) == nil
      if not locally_applied(incident, buffer_id)
         and not (resolution == 'census-absent' and absent)
         and not incident.adopted then
        error('Server acknowledged an unapplied dirty retirement') end
      table.insert(acknowledged, retirement)
    else
      table.insert(pending, retirement)
    end
  end
  M.require_stable_settlements(
    incident.preselection_retirements, retirements)
  incident.preselection_retirements = retirements
  incident.pending_preselection_retirements = pending
  incident.acknowledged_preselection_retirements = acknowledged
  incident.in_flight_preselection_retirement = nil
  incident.phase = 'settling-preselection-retirements'
  submit_later(M.settle_next_preselection_retirement)
end

function M.settle_next_preselection_retirement ()
  local incident = state.maintenance_client_incident
  if not incident
     or incident.phase ~= 'settling-preselection-retirements' then return end
  local retirement = incident.pending_preselection_retirements[1]
  if not retirement then
    incident.phase = 'server-blocked'
    vim.notify(
      'Skg retired every dirty view; repair disk and retry maintenance')
    return
  end
  local buffer_id = payload.field_text(retirement, 'buffer-id')
  local ok, error_text = pcall(function ()
    if not locally_applied(incident, buffer_id) then
      M.apply_settlement(retirement)
      mark_locally_applied(incident, buffer_id)
    end
    incident.in_flight_preselection_retirement = retirement
    M.send_preselection_retirement_ack(retirement)
  end)
  if not ok then
    incident.phase = 'preselection-retirement-blocked'
    vim.notify('Maintenance could not retire dirty buffer '
      .. tostring(buffer_id) .. ': ' .. tostring(error_text),
      vim.log.levels.ERROR)
  end
end

function M.send_preselection_retirement_ack (retirement)
  local incident = assert(state.maintenance_client_incident,
    'Dirty-buffer retirement ACK has no client state')
  register_response_handler('maintenance-status',
    M.handle_preselection_retirement_ack, true)
  set_request_failure_handler(fail_request(
    'preselection-retirement-ack-pending',
    'Dirty-buffer retirement ACK was not delivered'))
  client.submit_request(request('maintenance view settled',
    vim.list_extend({ { 'maintenance-epoch', incident.epoch } },
      M.ack_fields(retirement))), nil, incident.incident_id)
end

function M.handle_preselection_retirement_ack (_payload_text, response)
  local incident = assert(state.maintenance_client_incident,
    'Dirty-buffer retirement ACK arrived without client state')
  local retirement = incident.in_flight_preselection_retirement
  local first = incident.pending_preselection_retirements[1]
  local buffer_id = retirement
    and payload.field_text(retirement, 'buffer-id') or nil
  local status = payload.field_text(response, 'status')
  if not retirement or not first
     or (status ~= 'invalid-dirty-buffer-retired'
         and status ~= 'all-invalid-dirty-buffers-retired')
     or buffer_id ~= payload.field_text(response, 'buffer-id')
     or buffer_id ~= payload.field_text(first, 'buffer-id')
     or payload.field_text(response, 'required-ack') ~= 'retirement-ack' then
    error('Dirty-buffer retirement ACK changed identity') end
  retirement = client_acknowledged_settlement(retirement)
  replace_settlement(incident.preselection_retirements, retirement)
  table.insert(incident.acknowledged_preselection_retirements, retirement)
  table.remove(incident.pending_preselection_retirements, 1)
  incident.in_flight_preselection_retirement = nil
  incident.phase = 'settling-preselection-retirements'
  submit_later(M.settle_next_preselection_retirement)
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
  local status = payload.field_text(response, 'status')
  if (status ~= 'view-settlement-recorded' and status ~= 'all-views-settled')
     or buffer_id ~= payload.field_text(response, 'buffer-id')
     or payload.field_text(settlement, 'required-ack')
        ~= payload.field_text(response, 'required-ack') then
    error('Maintenance settlement ACK response changed identity') end
  local first = incident.pending_settlements[1]
  if not first or buffer_id ~= payload.field_text(first, 'buffer-id') then
    error('Maintenance settlement response arrived out of order') end
  settlement = client_acknowledged_settlement(settlement)
  replace_settlement(incident.settlements, settlement)
  table.insert(incident.acknowledged_settlements, settlement)
  table.remove(incident.pending_settlements, 1)
  incident.in_flight_settlement = nil
  incident.phase = 'settling-views'
  submit_later(M.settle_next)
end

function M.send_settlement_ack (settlement)
  local incident = assert(state.maintenance_client_incident,
    'Maintenance settlement ACK has no client state')
  register_response_handler(
    'maintenance-status', M.handle_settlement_ack, true)
  set_request_failure_handler(fail_request(
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
  register_response_handler(
    'maintenance-evidence', M.handle_evidence, true)
  set_request_failure_handler(fail_request(
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
  register_response_handler(
    'maintenance-status', M.handle_final_archive_ack, true)
  set_request_failure_handler(fail_request(
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
  register_response_handler(
    'maintenance-status', M.handle_terminal, true)
  set_request_failure_handler(fail_request(
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
  -- Terminal census IDs are identity evidence. Each buffer stays restricted
  -- until its own settlement has been applied and acknowledged.
  incident.phase = 'terminal-received'
  incident.terminal = response
  if not state.maintenance_historical_status then
    M.set_handshake_summary('terminal', incident.epoch) end
  if not state.maintenance_historical_status
     and state.owner_publication_revision == nil then
    local config = require('skg.config')
    config.store_state = config.store_state or {}
    config.store_state.graph_generation = nat(
      response, 'selected-graph-generation')
    config.store_state.manifest_revision = nat(
      response, 'selected-manifest-revision')
  end
  if incident.terminal_callback and not incident.terminal_callback_fired then
    incident.terminal_callback(response)
    incident.terminal_callback_fired = true
  end
  submit_later(M.send_terminal_ack)
end

function M.send_terminal_ack ()
  local incident = assert(state.maintenance_client_incident,
    'Terminal maintenance ACK has no client state')
  register_response_handler(
    'maintenance-status', M.handle_terminal_ack, true)
  set_request_failure_handler(fail_request(
    'terminal-received', 'Terminal maintenance ACK was not delivered'))
  client.submit_request(request('acknowledge terminal maintenance', {
    { 'incident-id', incident.incident_id },
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

function M.handle_terminal_ack (_payload_text, response)
  local incident = assert(state.maintenance_client_incident,
    'Terminal maintenance ACK arrived without client state')
  if payload.field_text(response, 'status') ~= 'terminal-acknowledged'
     or payload.field_text(response, 'incident-id') ~= incident.incident_id
     or nat(response, 'maintenance-epoch') ~= incident.epoch then
    error('Terminal maintenance ACK changed its exact identity') end
  incident.terminal_acknowledged = true
  state.update_global_server_status(response)
  M.finish_idle()
end

function M.finish_idle ()
  local incident = assert(state.maintenance_client_incident,
    'Server became idle without client maintenance state')
  if incident.phase ~= 'terminal-received' then
    error('Server became idle before the client received terminal authority')
  end
  local path = incident.final_archive and incident.final_archive.path or '?'
  state.clear_current_maintenance_incident()
  if not state.maintenance_historical_status then
    state.pending_maintenance_offer = nil end
  vim.notify('Skg maintenance complete; recovery archive: ' .. path)
end

function M.inspect_retained_incident (response, require_final)
  local incident_id = payload.field_text(response, 'active-incident-id')
    or payload.field_text(response, 'incident-id')
  local name = payload.field_text(response, 'archive-directory-name')
  if not name or not state.maintenance_archive_folder then
    error('Retained maintenance archive identity is incomplete') end
  local path = vim.fs.joinpath(state.maintenance_archive_folder, name)
  local summary = archive.inspect(path)
  if summary.incident_id ~= incident_id or summary.name ~= name then
    error('Retained maintenance archive identity does not match the server') end
  local initial_sha = payload.field_text(
    response, 'initial-manifest-sha256')
  if initial_sha and initial_sha ~= 'none'
     and initial_sha ~= summary.initial_manifest_sha256 then
    error('Retained maintenance initial checksum changed') end
  if require_final
     and (summary.status ~= 'finalized'
       or payload.field_text(response, 'manifest-sha256')
          ~= summary.final_manifest_sha256) then
    error('Retained maintenance final checksum changed') end
  return summary
end

function M.adopt_active (response)
  if state.maintenance_client_incident then return end
  local archive_status = payload.field_text(response, 'archive-status')
  if archive_status ~= 'archive-ready' and archive_status ~= 'finalized' then
    error('A replacement editor cannot adopt maintenance before archive-ready')
  end
  local summary = M.inspect_retained_incident(response, false)
  local incident_id = payload.field_text(response, 'active-incident-id')
  local epoch = nat(response, 'maintenance-epoch')
  local finalized = summary.status == 'finalized'
  if archive_status == 'finalized'
     and payload.field_text(response, 'archive-manifest-sha256')
       ~= summary.final_manifest_sha256 then
    error('Retained maintenance final checksum changed') end
  state.replace_current_maintenance_incident({
    incident_id = incident_id, epoch = epoch,
    phase = 'adopting-retained-incident',
    requested_paths = payload.string_list(
      payload.field(response, 'requested-paths')),
    requested_ids = payload.string_list(
      payload.field(response, 'requested-ids')),
    registered_buffer_ids = payload.string_list(
      payload.field(response, 'registered-buffer-ids')),
    undo_waivers = {}, locally_applied = {}, settlements = nil,
    pending_settlements = {}, acknowledged_settlements = {},
    offer = {
      incident_id = incident_id, epoch = epoch,
      origin = payload.field_text(response, 'origin'),
      started_at_utc = payload.field_text(response, 'started-at-utc'),
      archive_name = summary.name,
      archive_folder = state.maintenance_archive_folder,
      archive_identity = state.maintenance_archive_identity,
      source_set = payload.field_text(response, 'source-set'),
      graph_generation = nat(response, 'g0-graph-generation'),
      manifest_revision = nat(response, 'g0-manifest-revision'),
    },
    archive = {
      path = summary.path,
      manifest_sha256 = summary.initial_manifest_sha256,
    },
    final_archive = finalized and {
      path = summary.path,
      manifest_sha256 = summary.final_manifest_sha256,
      transfer_manifest_sha256 = summary.transfer_manifest_sha256,
      artifact_bytes_sha256 = summary.artifact_bytes_sha256,
    } or nil,
    adopted = true, terminal_callback = nil,
    terminal_callback_fired = false,
  })
end

function M.adopt_terminal (response)
  if state.maintenance_client_incident then return end
  local summary = M.inspect_retained_incident(response, true)
  state.replace_current_maintenance_incident({
    incident_id = payload.field_text(response, 'incident-id'),
    epoch = nat(response, 'maintenance-epoch'), phase = 'adopting-terminal',
    registered_buffer_ids = payload.string_list(
      payload.field(response, 'unlock-buffer-ids')),
    g1_graph_generation = nat(response, 'selected-graph-generation'),
    g1_manifest_revision = nat(response, 'selected-manifest-revision'),
    final_archive = {
      path = summary.path,
      manifest_sha256 = summary.final_manifest_sha256,
      transfer_manifest_sha256 = summary.transfer_manifest_sha256,
      artifact_bytes_sha256 = summary.artifact_bytes_sha256,
    },
    adopted = true, terminal_callback = nil,
    terminal_callback_fired = false,
  })
end

function M.resume_active (response)
  local incident_id = payload.field_text(response, 'active-incident-id')
  local epoch = nat(response, 'maintenance-epoch')
  local phase = payload.field_text(response, 'phase')
  M.adopt_active(response)
  local incident = require_client_incident(incident_id, epoch)
  refresh_presentation_buffer_ids(response)
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
  if not state.maintenance_historical_status then
    M.set_handshake_summary('active', epoch) end
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
  elseif phase == 'awaiting-locked-census' then
    incident.phase = 'awaiting-locked-census'
    submit_later(M.send_locked_census)
  elseif phase == 'preparing-archive' then
    if not incident.lock_census_sha256 then
      local offered_ids = sorted_copy(payload.string_list(
        payload.field(response, 'registered-buffer-ids')))
      local actual_ids = registered_ids()
      if not equal_lists(offered_ids, actual_ids) then
        error('Maintenance census changed from its frozen authority') end
      local checksum = M.lock_census_sha256(actual_ids)
      if checksum ~= payload.field_text(response, 'lock-census-sha256') then
        error('Maintenance census checksum does not match') end
      incident.registered_buffer_ids = offered_ids
      incident.lock_census_sha256 = checksum
      local wanted = {}
      for _, id in ipairs(actual_ids) do wanted[id] = true end
      for _, buf in ipairs(registry.buffers()) do
        if wanted[registry.record(buf).id] then
          registry.lock_for_maintenance(buf, epoch) end
      end
    end
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
    incident.server_phase = phase
    incident.blocking_reason =
      payload.field_text(response, 'blocking-reason') or 'unspecified'
    vim.notify(string.format(
      'Maintenance %s remains locked in server phase %s: %s. Repair the '
        .. 'reported problem, then run :SkgRetryMaintenance.',
      incident_id, phase, incident.blocking_reason), vim.log.levels.ERROR)
    local retirements = payload.field(response, 'preselection-retirements')
    if retirements ~= nil then
      M.install_preselection_retirements(retirements) end
  else
    incident.phase = 'waiting-for-server'
    vim.notify('Skg maintenance ' .. incident_id
      .. ' is in server phase ' .. tostring(phase))
  end
end

local function response_incident_id (response)
  return payload.field_text(response, 'incident-id')
    or payload.field_text(response, 'active-incident-id')
end

local function with_response_incident (response, callback)
  local incident_id = response_incident_id(response)
  if not incident_id then return callback() end
  local target = state.lookup_maintenance_incident(incident_id)
  if target then
    return state.with_current_maintenance_incident(incident_id, callback)
  end
  local previous = state.maintenance_client_incident
  if not previous then return callback() end
  local previous_historical = state.maintenance_historical_status
  state.maintenance_historical_status = true
  state.maintenance_client_incident = nil
  local ok, result = xpcall(callback, debug.traceback)
  state.retain_maintenance_incident(state.maintenance_client_incident)
  state.maintenance_client_incident = previous
  state.maintenance_historical_status = previous_historical
  if not ok then error(result) end
  return result
end

local function handle_status_current (payload_text, response)
  local status = payload.field_text(response, 'status')
  if status == 'active' then
    M.resume_active(response)
  elseif status == 'terminal' then
    M.adopt_terminal(response)
    M.handle_terminal(payload_text, response)
  elseif status == 'idle' then
    if state.maintenance_client_incident then M.finish_idle()
    else
      if not state.maintenance_historical_status then
        M.set_handshake_summary('idle') end
      vim.notify('Skg maintenance is idle')
    end
  else
    vim.notify('Skg maintenance: ' .. payload_text)
  end
end

function M.handle_status (payload_text, response, selected_id)
  if selected_id then
    local actual = response_incident_id(response)
    if actual and actual ~= selected_id then
      error('Maintenance status response changed its incident identity') end
    local incident = state.lookup_maintenance_incident(selected_id)
    if incident then
      return state.with_current_maintenance_incident(selected_id, function ()
        return handle_status_current(payload_text, response) end)
    end
  end
  return with_response_incident(response, function ()
    return handle_status_current(payload_text, response) end)
end

local function approve_undo_waiver (incident, buffer_key, reason)
  register_response_handler('maintenance-status', function ()
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
  register_response_handler('maintenance-status',
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
      archive_root = incident.offer.archive_folder,
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

local function offer_for_writer (response)
  return {
    incident_id = payload.field_text(response, 'allocated-incident-id'),
    epoch = payload.field(response, 'maintenance-epoch'),
    origin = payload.field_text(response, 'origin'),
    started_at_utc = payload.field_text(response, 'started-at-utc'),
    archive_name = payload.field_text(response, 'archive-directory-name'),
    archive_folder = payload.field_text(
      response, 'maintenance-archive-folder'),
    archive_identity = payload.field_text(
      response, 'maintenance-archive-identity'),
    source_set = payload.field_text(response, 'source-set'),
    graph_generation = payload.field(response, 'g0-graph-generation'),
    manifest_revision = payload.field(response, 'g0-manifest-revision'),
  }
end

local function lock_offer (response)
  local offered_ids = sorted_copy(payload.string_list(
    payload.field(response, 'registered-buffer-ids')))
  local actual_ids = registered_ids()
  if not equal_lists(offered_ids, actual_ids) then
    error('Maintenance census changed: server froze '
      .. vim.inspect(offered_ids) .. ', client has ' .. vim.inspect(actual_ids))
  end
  local checksum = M.lock_census_sha256(actual_ids)
  if checksum ~= payload.field_text(response, 'lock-census-sha256') then
    error('Maintenance census checksum does not match') end
  local epoch = nat(response, 'maintenance-epoch')
  local wanted = {}
  for _, id in ipairs(actual_ids) do wanted[id] = true end
  for _, buf in ipairs(registry.buffers()) do
    if wanted[registry.record(buf).id] then
      registry.lock_for_maintenance(buf, epoch) end
  end
  return checksum
end

local function handle_bootstrap (
    _payload_text, response, terminal_callback, origin_context)
  local status = payload.field_text(response, 'status')
  local incident_id = assert(payload.field_text(
    response, 'allocated-incident-id'), 'offer has no incident ID')
  local epoch = nat(response, 'maintenance-epoch')
  local offer = offer_for_writer(response)
  if status == 'install-maintenance-epoch-and-submit-locked-census' then
    local incident = {
      incident_id = incident_id,
      epoch = epoch,
      phase = 'awaiting-locked-census',
      requested_paths = payload.string_list(
        payload.field(response, 'requested-paths')),
      requested_ids = payload.string_list(
        payload.field(response, 'requested-ids')),
      origin_context = origin_context,
      terminal_callback = terminal_callback,
      terminal_callback_fired = false,
      registered_buffer_ids = registered_ids(),
      presentation_buffer_ids = {},
      undo_waivers = {},
      locally_applied = {},
      offer = offer,
    }
    state.replace_current_maintenance_incident(incident)
    M.set_handshake_summary('active', epoch)
    local wanted = {}
    for _, id in ipairs(incident.registered_buffer_ids) do wanted[id] = true end
    for _, buf in ipairs(registry.buffers()) do
      if wanted[registry.record(buf).id] then
        registry.lock_for_maintenance(buf, epoch) end
    end
    if not state.maintenance_historical_status then
      state.client_constructor_admission = 'closed' end
    require('skg.misc_requests').submit_buffer_census(
      client.connect(), incident_id, epoch, incident.registered_buffer_ids)
  elseif status == 'locked-census-accepted-publish-initial-archive' then
    local incident = require_client_incident(incident_id, epoch)
    if not vim.deep_equal(offer, incident.offer)
       or payload.field_text(response, 'origin') ~= incident.offer.origin
       or not equal_lists(payload.string_list(
         payload.field(response, 'requested-paths')), incident.requested_paths)
       or not equal_lists(payload.string_list(
         payload.field(response, 'requested-ids')), incident.requested_ids) then
      error('Maintenance locked-census offer changed bootstrap authority')
    end
    incident.registered_buffer_ids = sorted_copy(payload.string_list(
      payload.field(response, 'registered-buffer-ids')))
    local ok, result = pcall(lock_offer, response)
    if not ok then
      vim.notify(tostring(result), vim.log.levels.ERROR)
      submit_later(M.cancel)
      return
    end
    incident.lock_census_sha256 = result
    incident.phase = 'preparing-archive'
    M.publish_initial()
  else
    error('Unexpected maintenance bootstrap status ' .. tostring(status))
  end
end

function M.send_locked_census ()
  local incident = assert(state.maintenance_client_incident,
    'Locked maintenance census has no client state')
  local incident_id = incident.incident_id
  local tcp = client.connect()
  client.submit_priority_request(tcp, request('maintenance locked census', {
    { 'incident-id', incident.incident_id },
    { 'maintenance-epoch', incident.epoch },
    { 'client-constructor-admission', 'closed' },
  }), {
    ['maintenance-offer'] = {
      handler = function (payload_text, response)
        return state.with_current_maintenance_incident(incident_id,
          function () handle_bootstrap(payload_text, response) end) end,
      one_shot = true,
    },
  }, nil, incident.incident_id)
end

---Begin one durable maintenance incident.
---ORIGIN_CONTEXT is opaque process-local adapter state. ORIGIN_FIELDS are
---complete s-expression field forms appended to the bootstrap request.
function M.begin (
    origin, candidate_id, paths, ids, terminal_callback, origin_context,
    origin_fields)
  refuse_modified_raw_files()
  -- Close constructor admission before the bootstrap request is serialized.
  -- Already serialized editable requests may drain while `closing'; the
  -- bootstrap handler advances to `closed' before freezing the census.
  state.client_constructor_admission = 'closing'
  state.set_request_failure_handler(function (_reason)
    if not state.maintenance_client_incident then
      state.client_constructor_admission = 'open' end
  end)
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
     or status == 'needs-scalar-authorization'
     or status == 'view-enrollment-pending' then
    if not field_present(response, 'incident-id')
       or not field_present(response, 'maintenance-epoch') then
      error('Asynchronous maintenance selection has no exact envelope') end
    with_response_incident(response, function ()
      M.handle_selection_response(payload_text, response) end)
  elseif status == 'origin-operation-failed' then
    with_response_incident(response, function ()
      local incident = assert(state.maintenance_client_incident,
        'Maintenance origin failure arrived without client state')
      require_client_incident(
        payload.field_text(response, 'incident-id'),
        nat(response, 'maintenance-epoch'))
      incident.phase = 'server-blocked'
      incident.server_phase = payload.field_text(response, 'phase')
      incident.blocking_reason =
        payload.field_text(response, 'error') or 'unknown error'
      vim.notify(string.format(
        'Maintenance remains locked in server phase %s: %s. Repair the '
          .. 'reported problem, then run :SkgRetryMaintenance.',
        incident.server_phase or '?', incident.blocking_reason),
        vim.log.levels.ERROR)
      local retirements = payload.field(response, 'preselection-retirements')
      if retirements ~= nil then
        M.install_preselection_retirements(retirements) end
    end)
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
  register_response_handler('maintenance-status',
    function (_payload_text, response)
      local epoch = nat(response, 'unlock-maintenance-epoch')
      for _, buf in ipairs(registry.buffers()) do
        registry.unlock_after_maintenance(buf, epoch) end
      state.clear_current_maintenance_incident()
      if not state.maintenance_historical_status then
        state.client_constructor_admission = 'open' end
      vim.notify('Skg maintenance cancelled before archive publication.')
    end, true)
  set_request_failure_handler(fail_request(
    'cancellation-pending', 'Maintenance cancellation was not acknowledged'))
  client.submit_request(request('cancel maintenance', {
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

function M.handle_retry (_payload_text, response)
  local incident = require_client_incident(
    payload.field_text(response, 'incident-id'),
    nat(response, 'maintenance-epoch'))
  if payload.field_text(response, 'status') ~= 'maintenance-retry-queued' then
    error('Server did not queue blocked maintenance recovery') end
  local mode = payload.field_text(response, 'recovery-mode') or 'unknown'
  incident.phase = 'waiting-for-origin-observation'
  incident.server_phase = nil
  incident.blocking_reason = nil
  vim.notify(string.format(
    'Skg queued %s maintenance recovery for %s', mode,
    incident.incident_id))
end

function M.retry ()
  local incident = assert(state.maintenance_client_incident,
    'no client-known blocked maintenance incident')
  if incident.phase ~= 'server-blocked' then
    error('no client-known blocked maintenance incident') end
  incident.phase = 'maintenance-retry-pending'
  register_response_handler(
    'maintenance-status', M.handle_retry, true)
  set_request_failure_handler(function (reason)
    local current = state.maintenance_client_incident
    if current and current.incident_id == incident.incident_id
       and current.epoch == incident.epoch then
      current.phase = 'server-blocked' end
    warn('Maintenance retry was not acknowledged: ' .. tostring(reason))
  end)
  client.submit_request(request('retry maintenance', {
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

function M.status (silent, incident_id)
  incident_id = incident_id or (state.maintenance_client_incident
    and state.maintenance_client_incident.incident_id)
  -- Status may adopt a server-retained incident not yet in the local index.
  state.register_response_handler('maintenance-status', function (text, response)
    return M.handle_status(text, response, incident_id)
  end, true)
  if not silent then
    state.set_request_failure_handler(function (reason)
      warn('Maintenance status was not delivered: ' .. tostring(reason))
    end)
  end
  client.submit_request(request('maintenance status', incident_id and {
    { 'incident-id', incident_id },
  } or nil), nil, incident_id)
end

return M
