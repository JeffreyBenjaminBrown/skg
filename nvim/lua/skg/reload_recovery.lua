-- Explicit client UI for the server's older fatal reload-recovery journal.
-- This is separate from portable maintenance archives: recovery may create
-- oops-* Git refs and restore owned fatal paths, so it always requires an
-- incident-specific confirmation.

local client = require('skg.client')
local messages = require('skg.messages')
local payload = require('skg.payload')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local M = {}

M.confirm = function (prompt)
  return vim.fn.confirm(prompt, '&Yes\n&No', 2) == 1
end

M.incident_picker = function (ids, prompt)
  return require('skg.picker').completing_read_with_cycle(prompt, ids, {
    require_match = true,
  })
end

local function incidents ()
  return state.pending_recovery_incidents or {}
end

local function incident_ids ()
  local result = {}
  for _, incident in ipairs(incidents()) do
    local id = payload.field_text(incident, 'incident-id')
    if id then table.insert(result, id) end
  end
  return result
end

local function resolve_incident_id (incident_id, prompt)
  if incident_id and incident_id ~= '' then return incident_id end
  local ids = incident_ids()
  if #ids == 0 then error('Skg knows of no unresolved recovery incident') end
  return M.incident_picker(ids, prompt)
end

local function remove_incident (incident_id)
  local retained = {}
  for _, incident in ipairs(incidents()) do
    if payload.field_text(incident, 'incident-id') ~= incident_id then
      table.insert(retained, incident) end
  end
  state.pending_recovery_incidents = retained
end

local function recovery_report (response)
  local lines = {
    '* Fatal reload recovery complete',
    payload.field_text(response, 'content') or 'Recovered.',
    '** repositories and refs',
  }
  local repositories = payload.field(response, 'repositories')
  if repositories and sexpr.is_list(repositories) and #repositories > 0 then
    for _, repository in ipairs(repositories) do
      table.insert(lines, '*** ' ..
        (payload.field_text(repository, 'root') or '[unknown repository]'))
      table.insert(lines, '**** pre-incident: ' ..
        (payload.field_text(repository, 'pre-ref') or '?') .. ' (' ..
        (payload.field_text(repository, 'pre-commit') or '?') .. ')')
      table.insert(lines, '**** legal: ' ..
        (payload.field_text(repository, 'legal-ref') or '?') .. ' (' ..
        (payload.field_text(repository, 'legal-commit') or '?') .. ')')
      table.insert(lines, '**** complete incident: ' ..
        (payload.field_text(repository, 'incident-ref') or '?') .. ' (' ..
        (payload.field_text(repository, 'incident-commit') or '?') .. ')')
    end
  else table.insert(lines, 'None.') end
  table.insert(lines, '** restored owned paths')
  local restored = payload.string_list(payload.field(response, 'restored-paths'))
  if #restored == 0 then table.insert(lines, 'None.')
  else
    for _, path in ipairs(restored) do table.insert(lines, '*** ' .. path) end
  end
  table.insert(lines, '** warnings')
  local warnings = payload.string_list(payload.field(response, 'warnings'))
  if #warnings == 0 then table.insert(lines, 'None.')
  else
    for _, warning in ipairs(warnings) do
      table.insert(lines, '*** ' .. warning) end
  end
  return table.concat(lines, '\n') .. '\n'
end

local function queue_full_sweep ()
  state.register_response_handler('reload-paths', function (_, response)
    if payload.field_text(response, 'observation-queued') ~= 'true' then
      vim.notify(payload.field_text(response, 'content')
        or 'Skg could not queue the successor observation', vim.log.levels.WARN)
    end
  end, true)
  client.submit_request(
    '((request . "reload paths") (full-sweep . "true"))\n')
end

local function handle_recovery_response (incident_id, response)
  local status = payload.field_text(response, 'terminal-status')
  local content = payload.field_text(response, 'content')
  if status == 'complete' then
    remove_incident(incident_id)
    messages.big_nonfatal_message(
      'skg://messages/reload-recovery/' .. incident_id,
      content or 'Fatal reload recovery complete',
      recovery_report(response))
    return
  end
  local successor = payload.field_text(response, 'successor-required') == 'true'
  messages.big_nonfatal_message(
    'skg://messages/reload-recovery/' .. incident_id,
    'WARNING: Fatal reload recovery did not complete',
    '* Recovery stopped\n' .. (content or 'Unknown recovery error') ..
      '\n\nThe incident journal remains available.' ..
      (successor and
        '\nSkg queued a new exact sweep to classify the changed bytes as a successor incident.'
        or ''))
  if successor then queue_full_sweep() end
end

function M.recover (incident_id)
  incident_id = resolve_incident_id(
    incident_id, 'Fatal reload incident: ')
  if not incident_id then return end
  if not M.confirm('Create recovery refs and restore fatal files for ' ..
                   incident_id .. '?') then return end
  state.register_response_handler('reload-recovery', function (_, response)
    handle_recovery_response(incident_id, response)
  end, true)
  client.submit_request(
    '((request . "reload recover") (approved . "true"))\n',
    nil, incident_id)
end

function M.dismiss (incident_id)
  incident_id = resolve_incident_id(
    incident_id, 'Dismiss fatal reload incident: ')
  if not incident_id then return end
  if not M.confirm('Delete recovery evidence for ' .. incident_id ..
      '? Automatic recovery will become impossible.') then return end
  state.register_response_handler('reload-recovery', function (_, response)
    local status = payload.field_text(response, 'terminal-status')
    if status == 'complete' then remove_incident(incident_id) end
    vim.notify(payload.field_text(response, 'content')
      or 'Recovery dismissal failed',
      status == 'complete' and vim.log.levels.INFO or vim.log.levels.WARN)
  end, true)
  client.submit_request(
    '((request . "reload recover") (action . "dismiss")'
      .. ' (approved . "true"))\n', nil, incident_id)
end

function M.install_pending (response)
  local pending = payload.field(response, 'pending-recovery-incidents')
  state.pending_recovery_incidents =
    pending and sexpr.is_list(pending) and pending or {}
  if #state.pending_recovery_incidents == 0 then return end
  local lines = { '* WARNING: Fatal reload recovery is pending' }
  for _, incident in ipairs(state.pending_recovery_incidents) do
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
    'Run :SkgRecoverReloadIncident to inspect and explicitly confirm recovery. Skg will not recover automatically. If you accept losing automatic recovery, :SkgDismissReloadRecoveryIncident deletes its private journal without changing sources or Git.')
  messages.big_nonfatal_message(
    'skg://messages/pending-reload-recovery',
    string.format('WARNING: %d fatal reload recovery incident(s) remain unresolved.',
                  #state.pending_recovery_incidents),
    table.concat(lines, '\n'))
end

return M
