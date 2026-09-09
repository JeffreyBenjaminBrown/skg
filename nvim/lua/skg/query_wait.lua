-- PURPOSE: Durable query-wait state shared by the Neovim search adapter.
-- A pending wait stores only its recipe, destination, and operation identity.

local buffer = require('skg.buffer')
local client = require('skg.client')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}
state.query_waits = state.query_waits or {}

local retained_statuses = {
  pending = true, blocked = true, executing = true, ready = true,
}

local function reconciliation_active ()
  return state.rebuilding
    or (state.maintenance_client_incident ~= nil
        and state.maintenance_client_incident.phase ~= 'terminal')
    or (state.graph_transition_status ~= nil
        and state.graph_transition_status ~= 'idle')
    or state.graph_write_admission == 'closed'
end

local function target ()
  local incident = state.maintenance_client_incident
  if incident and incident.incident_id and incident.epoch
     and incident.phase ~= 'terminal' then
    return incident.incident_id, incident.epoch, incident.candidate_id end
  for _, summary in ipairs(state.pending_incidents or {}) do
    -- pending-incidents is report-only and can retain older incidents.
    -- Use an incident only when the server explicitly marks it active.
    local id = payload.field_text(summary, 'active-incident-id')
    local epoch = payload.field(summary, 'maintenance-epoch')
    local candidate = payload.field_text(summary, 'candidate-id')
    local phase = payload.field_text(summary, 'phase')
    if phase ~= 'terminal' and (id or candidate) then
      return id, epoch, candidate end
  end
  local offer = state.pending_maintenance_offer
  if offer and offer.candidate_id then
    return nil, nil, offer.candidate_id end
  error('Skg reconciliation has no active incident or candidate target')
end

local function recipe (terms, regex, body, operators, ugly_choice)
  return {
    kind = 'text-search', terms = terms,
    regex = regex == true, body = body == true,
    operators = operators == true,
    ugly_telescopes = ugly_choice or 'default',
    source_set = state.active_source_set_name,
  }
end

local function recipe_text (value)
  return sexpr.to_string({
    sexpr.pair(sexpr.symbol('kind'), value.kind),
    sexpr.pair(sexpr.symbol('terms'), value.terms),
    sexpr.pair(sexpr.symbol('regex'), value.regex and 'true' or 'false'),
    sexpr.pair(sexpr.symbol('body'), value.body and 'true' or 'false'),
    sexpr.pair(sexpr.symbol('operators'), value.operators and 'true' or 'false'),
    sexpr.pair(sexpr.symbol('ugly-telescopes'), value.ugly_telescopes),
    sexpr.pair(sexpr.symbol('source-set'), value.source_set),
  })
end

local function recipe_digest (value)
  return vim.fn.sha256(recipe_text(value))
end

local function destination (buf)
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return nil end
  local record = require('skg.buffer_registry').record(buf)
  if not record then return nil end
  return {
    client_buffer_id = record.id,
    view_uri = record.view_uri,
    server_session_id = record.server_session_id,
    client_application_token = record.application_token,
    graph_generation = record.graph_generation,
    presentation_generation = record.presentation_generation,
    server_revision = record.server_revision,
    source_set = record.source_set,
    base_content_sha256 = record.last_fetched_sha256,
    destination_state = vim.bo[buf].modified and 'dirty' or 'clean',
  }
end

local function fields (record, request_name)
  local id, epoch = record.incident_id, record.maintenance_epoch
  local dest = record.buffer and destination(record.buffer)
  local result = {
    sexpr.pair(sexpr.symbol('request'), request_name),
    sexpr.pair(sexpr.symbol('query-operation-id'), record.query_operation_id),
  }
  if state.server_session_id and not dest then
    table.insert(result, sexpr.pair(sexpr.symbol('server-session-id'),
                                    state.server_session_id)) end
  if id then table.insert(result, sexpr.pair(sexpr.symbol('incident-id'), id)) end
  if epoch then table.insert(result, sexpr.pair(sexpr.symbol('maintenance-epoch'), epoch)) end
  if record.candidate_id then
    table.insert(result, sexpr.pair(sexpr.symbol('candidate-id'), record.candidate_id)) end
  if request_name == 'query wait' then
    table.insert(result, sexpr.pair(sexpr.symbol('outcome-kind'), 'graph-publication'))
    table.insert(result, sexpr.pair(sexpr.symbol('query-recipe'), recipe_text(record.recipe)))
    table.insert(result, sexpr.pair(sexpr.symbol('query-recipe-digest'), record.recipe_digest))
  elseif request_name == 'query wait status' then
    if record.recipe_digest then
      table.insert(result, sexpr.pair(sexpr.symbol('query-recipe-digest'),
                                      record.recipe_digest)) end
  end
  if dest then
    for key, value in pairs(dest) do
      table.insert(result, sexpr.pair(sexpr.symbol(key:gsub('_', '-')), value)) end
  end
  return sexpr.to_string(result) .. '\n'
end

local function record_status (response, operation_id)
  local record = state.query_waits[operation_id]
  if not record then
    record = {
      query_operation_id = operation_id, status = 'pending',
      incident_id = nil, maintenance_epoch = nil, candidate_id = nil,
      terms = nil, recipe = nil, recipe_text = nil, recipe_digest = nil,
      result_digest = nil, freshness = nil, reason = nil, buffer = nil,
      last_notified_state = nil,
    }
    state.query_waits[operation_id] = record
  end
  local recipe_value = payload.field_text(response, 'query-recipe')
  local recipe_hash = payload.field_text(response, 'query-recipe-digest')
  if recipe_value then
    if not recipe_hash or vim.fn.sha256(recipe_value) ~= recipe_hash then
      error('Skg query wait status recipe digest mismatch') end
    if record.recipe_digest and record.recipe_digest ~= recipe_hash then
      error('Skg query wait status recipe identity changed') end
    record.recipe_text = recipe_value
    record.recipe_digest = recipe_hash
  elseif recipe_hash and not record.recipe_digest then
    record.recipe_digest = recipe_hash
  elseif recipe_hash and record.recipe_digest ~= recipe_hash then
    error('Skg query wait status recipe identity changed')
  end
  local status = payload.field_text(response, 'status')
    or payload.field_text(response, 'query-wait-status')
    or 'pending'
  record.status = status
  record.reason = payload.field_text(response, 'reason')
  record.result_digest = payload.field_text(response, 'result-digest')
  return record
end

local function notify_status (record)
  local signature = tostring(record.status) .. '\0'
    .. tostring(record.reason or '')
  if record.last_notified_state == signature then return end
  record.last_notified_state = signature
  local suffix = record.reason and (' (' .. record.reason .. ')') or ''
  vim.notify(string.format('Skg query wait %s: %s%s',
                           record.query_operation_id, record.status, suffix))
end

function M.ingest_pending (entries)
  for _, entry in ipairs(entries or {}) do
    local id = payload.field_text(entry, 'query-operation-id')
    local status = payload.field_text(entry, 'status')
    if id and retained_statuses[status] then
      local record = state.query_waits[id] or {
        query_operation_id = id, status = status,
        incident_id = nil, maintenance_epoch = nil, candidate_id = nil,
        terms = nil, recipe = nil, recipe_text = nil, recipe_digest = nil,
        result_digest = nil, freshness = nil, reason = nil, buffer = nil,
      }
      record.status = status
      local incident_id = payload.field_text(entry, 'incident-id')
      local epoch = payload.field(entry, 'maintenance-epoch')
      local candidate_id = payload.field_text(entry, 'candidate-id')
      if incident_id ~= nil then record.incident_id = incident_id end
      if epoch ~= nil then record.maintenance_epoch = epoch end
      if candidate_id ~= nil then record.candidate_id = candidate_id end
      state.query_waits[id] = record
    end
  end
end

local function status_handler (operation_id)
  return function (_payload_text, response)
    state.require_current_server_session(response)
    local record = record_status(response, operation_id)
    if record then notify_status(record) end
  end
end

local function status_push_handler (_payload_text, response)
  state.require_current_server_session(response)
  local operation_id = payload.field_text(response, 'query-operation-id')
  if operation_id then notify_status(record_status(response, operation_id)) end
end

M.status_push_handler = status_push_handler

function M.policy_choice ()
  if not reconciliation_active() then return 'current' end
  return vim.fn.confirm(
    'Graph/search reconciliation is pending. Use the current snapshot?',
    '&Current\n&Wait', 1) == 2 and 'wait' or 'current'
end

function M.submit (terms, regex, body, operators, ugly_choice, operation_id)
  if not reconciliation_active() then
    error('Skg has no pending reconciliation to wait for') end
  local incident_id, epoch, candidate_id = target()
  local id = operation_id or buffer.generate_uuid()
  local query_recipe = recipe(terms, regex, body, operators, ugly_choice)
  local uri = 'search:wait:' .. id
  local placeholder = buffer.open_org_buffer_from_text(
    string.format('* SKG search waiting\n\nTerms: %s\nWait status: pending\n', terms),
    'skg://search-wait', uri, {
      kind = 'search-view', lifecycle = 'live-view', disposable = false,
      force_new = true,
      recipe = query_recipe, server_session_id = state.server_session_id,
      view_write_authority = 'read-only', graph_generation =
        (require('skg.config').store_state or {}).graph_generation,
      presentation_generation = 0, server_revision = 0,
      application_token = 1,
    })
  vim.b[placeholder].skg_query_operation_id = id
  state.query_waits[id] = {
    query_operation_id = id, status = 'pending',
    incident_id = incident_id, maintenance_epoch = epoch,
    candidate_id = candidate_id,
    terms = terms,
    recipe = query_recipe, recipe_digest = recipe_digest(query_recipe),
    buffer = placeholder,
  }
  local record = state.query_waits[id]
  state.register_response_handler('query-wait-status', status_handler(id), true)
  client.submit_request(fields(record, 'query wait'))
  return id
end

local function exact_destination (response, record)
  local buf = record.buffer
  if not buf or not vim.api.nvim_buf_is_valid(buf) then
    return false, 'destination is missing' end
  local current = destination(buf)
  if not current then return false, 'destination is unregistered' end
  local result_token = payload.field(response,
                                    'resulting-client-application-token')
  local freshness = payload.field_text(response, 'freshness')
  if not freshness then return false, 'freshness is missing' end
  if type(result_token) ~= 'number' then
    return false, 'resulting-client-application-token is missing' end
  local checks = {
    ['client-buffer-id'] = current.client_buffer_id,
    ['view-uri'] = current.view_uri,
    ['server-session-id'] = current.server_session_id,
    ['expected-client-application-token'] = current.client_application_token,
    ['query-operation-id'] = vim.b[buf].skg_query_operation_id,
    ['query-recipe-digest'] = record.recipe_digest,
    ['expected-graph-generation'] = current.graph_generation,
    ['expected-presentation-generation'] = current.presentation_generation,
    ['source-set'] = current.source_set,
    ['base-content-sha256'] = current.last_fetched_sha256,
  }
  checks['expected-server-revision'] = current.server_revision
  for key, actual in pairs(checks) do
    local expected = payload.field_text(response, key)
    if not expected then return false, key .. ' is missing' end
    if tostring(actual) ~= expected then
      return false, key .. ' changed' end
  end
  local result_revision_key = payload.field_text(response, 'server-revision')
    and 'server-revision' or 'manifest-revision'
  for _, key in ipairs({ 'graph-generation', 'presentation-generation',
                         result_revision_key }) do
    if not payload.field_text(response, key) then
      return false, key .. ' is missing' end
  end
  if vim.bo[buf].modified then return false, 'destination is dirty' end
  return true
end

local function send_applied (record, response)
  local id = record.query_operation_id
  local ack = {
    sexpr.pair(sexpr.symbol('request'), 'query wait applied'),
    sexpr.pair(sexpr.symbol('query-operation-id'), id),
    sexpr.pair(sexpr.symbol('result-digest'), record.result_digest
      or payload.field_text(response, 'result-digest') or ''),
    sexpr.pair(sexpr.symbol('applied'), 'true'),
  }
  if record.buffer and vim.api.nvim_buf_is_valid(record.buffer) then
    for key, value in pairs(destination(record.buffer)) do
      table.insert(ack, sexpr.pair(sexpr.symbol(key:gsub('_', '-')), value)) end
  end
  client.submit_request(sexpr.to_string(ack) .. '\n')
end

function M.result_handler (_payload_text, response)
  local id = payload.field_text(response, 'query-operation-id')
  local record = id and state.query_waits[id]
  if not record then
    vim.notify('Skg received an unknown query wait result ' .. tostring(id),
               vim.log.levels.WARN)
    return end
  state.require_current_server_session(response)
  if state.view_write_authority_from_response(response) ~= 'read-only' then
    error('Skg query wait result must be read-only') end
  if record.status == 'delivered'
     and record.result_digest == payload.field_text(response, 'result-digest') then
    send_applied(record, response)
    return end
  local ok, reason = exact_destination(response, record)
  if not ok then
    record.status, record.reason = 'destination-rejected', reason
    vim.notify('Skg query wait result kept pending: ' .. reason,
               vim.log.levels.WARN)
    return end
  local content = payload.field_text(response, 'content') or ''
  local digest = payload.field_text(response, 'result-digest')
  if not digest or vim.fn.sha256(content) ~= digest then
    record.status, record.reason = 'destination-rejected',
      'result digest does not match content'
    vim.notify('Skg query wait result kept pending: content digest mismatch',
               vim.log.levels.WARN)
    return end
  local buf = record.buffer
  require('skg.buffer_registry').apply_server_text(buf, content, {
    view_uri = vim.b[buf].skg_view_uri,
    application_token = vim.b[buf].skg_application_token,
    graph_generation = tonumber(payload.field_text(
      response, 'expected-graph-generation'))
      or vim.b[buf].skg_graph_generation,
    presentation_generation = tonumber(
      payload.field_text(response, 'expected-presentation-generation'))
      or vim.b[buf].skg_presentation_generation,
    server_revision = tonumber(payload.field_text(response, 'expected-server-revision'))
      or vim.b[buf].skg_server_revision,
  })
  vim.b[buf].skg_graph_generation = tonumber(
    payload.field_text(response, 'graph-generation'))
    or vim.b[buf].skg_graph_generation
  vim.b[buf].skg_presentation_generation = tonumber(
    payload.field_text(response, 'presentation-generation'))
    or vim.b[buf].skg_presentation_generation
  vim.b[buf].skg_server_revision = tonumber(
    payload.field_text(response, 'server-revision')
    or payload.field_text(response, 'manifest-revision'))
    or vim.b[buf].skg_server_revision
  vim.b[buf].skg_query_operation_id = id
  local expected_result_token = tonumber(
    payload.field_text(response, 'resulting-client-application-token'))
  if expected_result_token ~= vim.b[buf].skg_application_token then
    record.status, record.reason = 'destination-rejected',
      'resulting application token changed'
    return end
  vim.bo[buf].modifiable = false
  record.status = 'delivered'
  record.result_digest = digest
  record.freshness = payload.field_text(response, 'freshness')
  state.register_response_handler('query-wait-applied', status_handler(id), true)
  send_applied(record, response)
end

function M.resume_all ()
  for id, record in pairs(state.query_waits) do
    if record.status == 'pending' or record.status == 'blocked'
       or record.status == 'ready' or record.status == 'delivered' then
      if record.buffer and vim.api.nvim_buf_is_valid(record.buffer) then
        vim.b[record.buffer].skg_server_session_id = state.server_session_id
      end
      state.register_response_handler('query-wait-status', status_handler(id), true)
      client.submit_request(fields(record, 'query wait status'))
    end
  end
end

function M.cancel (operation_id)
  local record = state.query_waits[operation_id]
  if not record then error('Unknown query operation ' .. tostring(operation_id)) end
  state.register_response_handler('query-wait-status', status_handler(operation_id), true)
  client.submit_request(fields(record, 'query wait cancel'))
end

function M.recover (operation_id, terms)
  if not operation_id or operation_id == '' then
    error('A query operation ID is required') end
  local record = state.query_waits[operation_id]
  if not record then
    record = {
      query_operation_id = operation_id, status = 'pending',
      terms = terms or '', recipe = nil, recipe_digest = nil,
      incident_id = nil, maintenance_epoch = nil, candidate_id = nil,
      buffer = nil,
    }
    state.query_waits[operation_id] = record
  end
  return M.status(operation_id)
end

function M.status (operation_id)
  if not operation_id or operation_id == '' then
    local ids = {}
    for id, record in pairs(state.query_waits) do
      table.insert(ids, string.format('%s: %s', id, record.status)) end
    table.sort(ids)
    vim.notify('Skg query waits: ' .. table.concat(ids, '; '))
    return ids
  end
  local record = state.query_waits[operation_id]
  if not record then error('Unknown query operation ' .. tostring(operation_id)) end
  state.register_response_handler('query-wait-status', status_handler(operation_id), true)
  client.submit_request(fields(record, 'query wait status'))
end

if state.pending_query_waits_raw ~= nil then
  M.ingest_pending(state.pending_query_waits_raw)
end

return M
