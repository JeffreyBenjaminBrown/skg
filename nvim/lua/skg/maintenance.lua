-- Maintenance epoch and client-owned recovery-archive protocol.

local archive = require('skg.recovery_archive')
local client = require('skg.client')
local payload = require('skg.payload')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

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

local function registered_ids ()
  local result = {}
  for _, buf in ipairs(registry.buffers()) do
    table.insert(result, registry.record(buf).id) end
  return sorted_copy(result)
end

function M.lock_census_sha256 (ids)
  ids = sorted_copy(ids)
  local bytes = table.concat(ids, '\0')
  if #ids > 0 then bytes = bytes .. '\0' end
  return vim.fn.sha256(bytes)
end

local function request (name, fields)
  local result = {
    sexpr.pair(sexpr.symbol('request'), name),
  }
  for _, entry in ipairs(fields or {}) do
    table.insert(result,
      sexpr.pair(sexpr.symbol(entry[1]), entry[2])) end
  return sexpr.to_string(result) .. '\n'
end

local function submit_later (callback)
  vim.schedule(function ()
    local ok, error_text = pcall(callback)
    if not ok then
      vim.notify('Skg maintenance failed: ' .. tostring(error_text),
                 vim.log.levels.ERROR) end
  end)
end

local function send_archive_ready (incident)
  state.register_response_handler('maintenance-status',
    function (_payload_text, response)
      incident.phase = 'archive-ready'
      vim.notify(string.format(
        'Skg server verified recovery archive %s (%s bytes).',
        payload.field_text(response, 'verified-manifest-sha256') or '?',
        payload.field_text(response, 'archive-file-bytes') or '?'))
    end, true)
  client.submit_request(request('maintenance archive ready', {
    { 'maintenance-epoch', incident.epoch },
    { 'lock-census-sha256', incident.lock_census_sha256 },
    { 'manifest-sha256', incident.archive.manifest_sha256 },
  }), nil, incident.incident_id)
end

local function approve_undo_waiver (incident, buffer_key, reason)
  state.register_response_handler('maintenance-status',
    function ()
      incident.undo_waivers[buffer_key] = reason
      submit_later(function () M.publish_initial() end)
    end, true)
  client.submit_request(request('approve undo waiver', {
    { 'maintenance-epoch', incident.epoch },
    { 'buffer-key', buffer_key },
    { 'reason', reason },
  }), nil, incident.incident_id)
end

local function send_undo_failure (incident, failure)
  state.register_response_handler('maintenance-status',
    function (_payload_text, response)
      local buffer_key = payload.field_text(response, 'buffer-key')
      local reason = payload.field_text(response, 'reason')
      submit_later(function ()
        local answer = vim.fn.confirm(
          string.format(
            'Native undo could not be archived for %s:\n%s\nContinue only with exact text and diff recovery?',
            buffer_key, reason), '&Continue\n&Cancel', 2)
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
    submit_later(function () send_archive_ready(incident) end)
    return
  end
  if type(result) == 'table' and result.kind == 'native-undo-failure' then
    submit_later(function () send_undo_failure(incident, result) end)
    return
  end
  vim.notify('Initial recovery archive failed before risky work: '
    .. tostring(result) .. '\nIncomplete staging data was retained.',
    vim.log.levels.ERROR)
  submit_later(function () M.cancel() end)
end

local function handle_bootstrap (_payload_text, response)
  local offered_ids = sorted_copy(payload.string_list(
    payload.field(response, 'registered-buffer-ids')))
  local actual_ids = registered_ids()
  local incident = {
    incident_id = assert(payload.field_text(
      response, 'allocated-incident-id'), 'offer has no incident ID'),
    epoch = assert(payload.field(response, 'maintenance-epoch'),
      'offer has no maintenance epoch'),
    phase = 'preparing-archive',
    undo_waivers = {},
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
    submit_later(function () M.cancel() end)
    return
  end
  local checksum = M.lock_census_sha256(actual_ids)
  if checksum ~= payload.field_text(response, 'lock-census-sha256') then
    vim.notify('Maintenance census checksum does not match; cancelling safely.',
               vim.log.levels.ERROR)
    submit_later(function () M.cancel() end)
    return
  end
  incident.lock_census_sha256 = checksum
  for _, buf in ipairs(registry.buffers()) do
    registry.lock_for_maintenance(buf, incident.epoch) end
  M.publish_initial()
end

function M.begin (origin, candidate_id)
  state.register_response_handler(
    'maintenance-offer', handle_bootstrap, true)
  client.submit_request(request('begin maintenance', {
    { 'origin', origin },
    { 'candidate-id', candidate_id or 'none' },
  }))
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

function M.cancel ()
  local incident = assert(state.maintenance_client_incident,
    'no client-known maintenance incident to cancel')
  state.register_response_handler('maintenance-status',
    function (_payload_text, response)
      local epoch = payload.field(response, 'unlock-maintenance-epoch')
      for _, buf in ipairs(registry.buffers()) do
        registry.unlock_after_maintenance(buf, epoch) end
      state.maintenance_client_incident = nil
      vim.notify('Skg maintenance cancelled before archive publication.')
    end, true)
  client.submit_request(request('cancel maintenance', {
    { 'maintenance-epoch', incident.epoch },
  }), nil, incident.incident_id)
end

function M.status ()
  state.register_response_handler('maintenance-status',
    function (payload_text)
      vim.notify('Skg maintenance: ' .. payload_text) end, true)
  client.submit_request(request('maintenance status'))
end

return M
