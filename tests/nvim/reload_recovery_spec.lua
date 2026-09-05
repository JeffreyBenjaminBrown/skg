local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local client = require('skg.client')
local recovery = require('skg.reload_recovery')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local incident_id = '12345678-1234-4234-8234-123456789abc'

local function pending_response ()
  return sexpr.read(
    '((pending-recovery-incidents'
      .. ' (((incident-id "' .. incident_id .. '")'
      .. ' (fatal (((pid broken-node) (reason "invalid yaml"))))))))')
end

local function delete_named_buffer (name)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == name then
      vim.bo[buf].modified = false
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
end

local function named_buffer (name)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == name then return buf end
  end
  return nil
end

describe('skg fatal reload recovery', function ()
  local server
  local old_confirm
  local old_picker

  before_each(function ()
    old_confirm = recovery.confirm
    old_picker = recovery.incident_picker
    recovery.confirm = function () return true end
    state.pending_recovery_incidents = {}
  end)

  after_each(function ()
    recovery.confirm = old_confirm
    recovery.incident_picker = old_picker
    state.pending_recovery_incidents = {}
    if server then server.close() server = nil end
    helpers.reset_client_state()
    delete_named_buffer('skg://messages/pending-reload-recovery')
    delete_named_buffer('skg://messages/reload-recovery/' .. incident_id)
  end)

  it('installs, confirms, and reports one exact recovered incident', function ()
    local seen
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('(request . "reload recover")', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type reload-recovery)'
          .. ' (incident-id "' .. incident_id .. '")'
          .. ' (content "Recovered one path")'
          .. ' (repositories (((root "/repo") (pre-ref "oops-1_pre")'
          .. ' (pre-commit aaa) (legal-ref "oops-1_legal")'
          .. ' (legal-commit bbb) (incident-ref "oops-1_complete")'
          .. ' (incident-commit ccc))))'
          .. ' (restored-paths ("/repo/owned/node.skg"))'
          .. ' (warnings ()))'))
      end
    end)
    client.connect()
    vim.wait(2000, function ()
      return state.connection_handshake_state == 'verified' end, 10)
    recovery.install_pending(pending_response())
    recovery.recover(incident_id)
    vim.wait(2000, function () return seen ~= nil end, 10)
    vim.wait(2000, function ()
      return #state.pending_recovery_incidents == 0 end, 10)
    assert.is_truthy(seen:find('(approved . "true")', 1, true))
    assert.is_truthy(seen:find(
      '(incident-id . "' .. incident_id .. '")', 1, true))
    local report = named_buffer(
      'skg://messages/reload-recovery/' .. incident_id)
    assert.is_truthy(report)
    local text = table.concat(
      vim.api.nvim_buf_get_lines(report, 0, -1, false), '\n')
    assert.is_truthy(text:find('Recovered one path', 1, true))
    assert.is_truthy(text:find('oops-1_complete', 1, true))
    assert.is_truthy(text:find('/repo/owned/node.skg', 1, true))
  end)

  it('retains a failed incident and queues its successor sweep', function ()
    local recovery_seen, sweep_seen
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('(request . "reload recover")', 1, true) then
        recovery_seen = line
        respond(helpers.framed(
          '((response-type reload-recovery)'
          .. ' (incident-id "' .. incident_id .. '")'
          .. ' (terminal-status failed)'
          .. ' (content "disk changed") (successor-required true))'))
      elseif line:find('(full-sweep . "true")', 1, true) then
        sweep_seen = line
        respond(helpers.framed(
          '((response-type reload-paths) (observation-queued true))'))
      end
    end)
    client.connect()
    vim.wait(2000, function ()
      return state.connection_handshake_state == 'verified' end, 10)
    recovery.install_pending(pending_response())
    recovery.recover(incident_id)
    vim.wait(3000, function ()
      return recovery_seen ~= nil and sweep_seen ~= nil end, 10)
    assert.are.equal(1, #state.pending_recovery_incidents)
    assert.is_truthy(sweep_seen)
  end)

  it('dismisses only after explicit confirmation', function ()
    local seen
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('(request . "reload recover")', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type reload-recovery)'
          .. ' (incident-id "' .. incident_id .. '")'
          .. ' (content "Dismissed"))'))
      end
    end)
    client.connect()
    vim.wait(2000, function ()
      return state.connection_handshake_state == 'verified' end, 10)
    recovery.install_pending(pending_response())
    recovery.dismiss(incident_id)
    vim.wait(2000, function ()
      return seen ~= nil and #state.pending_recovery_incidents == 0 end, 10)
    assert.is_truthy(seen:find('(action . "dismiss")', 1, true))
    assert.is_truthy(seen:find('(approved . "true")', 1, true))
    assert.are.equal(0, #state.pending_recovery_incidents)
  end)
end)
