local config = require('skg.config')
local maintenance = require('skg.maintenance')
local payload = require('skg.payload')
local pull = require('skg.pull')
local registry = require('skg.buffer_registry')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local incident_id = '12345678-1234-4234-8234-123456789abc'

local function f (name, value)
  return { sexpr.symbol(name), value }
end

local function repository_record (key, sources)
  return {
    f('repository-key', key),
    f('sources', sources),
  }
end

local function pull_incident (context)
  return {
    incident_id = incident_id,
    epoch = 9,
    offer = { origin = 'pull' },
    origin_context = context,
  }
end

local function clear_registered_buffers ()
  for _, buf in ipairs(registry.buffers()) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.bo[buf].modified = false
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf)
       and vim.api.nvim_buf_get_name(buf):match('^skg://pull/') then
      pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
  end
end

describe('skg Neovim client-owned pull', function ()
  local originals

  before_each(function ()
    originals = {
      is_directory = pull.is_directory,
      realpath = pull.realpath,
      discover_git_root = pull.discover_git_root,
      local_repositories = pull.local_repositories,
      dirty_buffers = pull.dirty_buffers,
      defer = pull.defer,
      confirm = pull.confirm,
      start_terminal = pull.start_terminal,
      jobstart = pull.jobstart,
      job_status = pull.job_status,
      send_finish = pull.send_finish,
      finish_origin = pull.finish_origin,
      maintenance_begin = maintenance.begin,
      client_submit = require('skg.client').submit_request,
      config_file = config.config_file,
      source_paths = config.source_paths_from_toml,
    }
    state.maintenance_client_incident = nil
    state.active_source_set_name = 'all'
    state.request_draft = nil
    config.source_inventory = nil
    clear_registered_buffers()
  end)

  after_each(function ()
    for name, value in pairs(originals) do
      if name == 'maintenance_begin' then maintenance.begin = value
      elseif name == 'client_submit' then
        require('skg.client').submit_request = value
      elseif name == 'config_file' then config.config_file = value
      elseif name == 'source_paths' then config.source_paths_from_toml = value
      else pull[name] = value end
    end
    state.maintenance_client_incident = nil
    state.request_draft = nil
    config.source_inventory = nil
    clear_registered_buffers()
  end)

  it('groups matching local source paths by canonical Git root', function ()
    config.source_inventory = {
      { name = 'three', configured_path = 'sources/three' },
      { name = 'one', configured_path = 'sources/one' },
      { name = 'two', configured_path = 'sources/two' },
    }
    config.config_file = function () return '/config/skgconfig.toml' end
    config.source_paths_from_toml = function () return {
      { name = 'one', path = '/client/one' },
      { name = 'two', path = '/client/two' },
      { name = 'three', path = '/client/three' },
    } end
    pull.is_directory = function () return true end
    local realpaths = {
      ['/client/one'] = '/physical/one',
      ['/client/two'] = '/physical/two',
      ['/client/three'] = '/physical/three',
      ['/config/sources/one'] = '/physical/one',
      ['/config/sources/two'] = '/physical/two',
      ['/config/sources/three'] = '/physical/three',
      ['/repository/a'] = '/repository/a',
      ['/repository/b'] = '/repository/b',
    }
    pull.realpath = function (path) return realpaths[path] end
    pull.discover_git_root = function (path)
      if path == '/client/three' then return '/repository/b' end
      return '/repository/a'
    end

    local repositories = pull.local_repositories()
    assert.are.same({ '/repository/a', '/repository/b' }, {
      repositories[1].root, repositories[2].root,
    })
    assert.are.same({ 'one', 'two' }, repositories[1].sources)
    assert.are.same({ 'three' }, repositories[2].sources)
    assert.are.equal(pull.repository_key({ 'one', 'two' }),
      repositories[1].key)
  end)

  it('requires verified source inventory before reading local config',
     function ()
    config.config_file = function () error('read local config') end
    assert.has_error(pull.local_repositories,
      'Skg has no verified server source inventory')
  end)

  it('refuses a limited source-set before repository preflight', function ()
    state.active_source_set_name = 'private'
    pull.local_repositories = function () error('computed repositories') end
    maintenance.begin = function () error('began maintenance') end
    assert.has_error(pull.pull_all,
      "Pull requires source-set 'all'; switch from 'private' before pulling")
  end)

  it('begins one incident carrying only the logical repository map',
     function ()
    local repositories = {
      {
        key = pull.repository_key({ 'one' }),
        root = '/client/repository with space',
        sources = { 'one' },
      },
    }
    pull.local_repositories = function () return repositories end
    pull.dirty_buffers = function () return {} end
    local arguments
    maintenance.begin = function (...) arguments = { ... } end
    assert.is_true(pull.pull_all())
    assert.are.equal('pull', arguments[1])
    assert.are.equal(pull.terminal, arguments[5])
    assert.are.equal(repositories, arguments[6].repositories)
    local rendered = sexpr.to_string(arguments[7])
    assert.is_nil(rendered:find('/client/', 1, true))
    local mapping = payload.field(arguments[7], 'pull-repositories')
    assert.are.equal(repositories[1].key,
      payload.field_text(mapping[1], 'repository-key'))
    assert.are.same({ 'one' }, payload.string_list(
      payload.field(mapping[1], 'sources')))
  end)

  it('refuses a dirty raw file before maintenance', function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, '/client/raw.skg')
    registry.register(buf, 'raw-skg-file')
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'dirty' })
    vim.bo[buf].modified = true
    pull.local_repositories = function () return {
      { key = string.rep('a', 64), root = '/repo', sources = { 'one' } },
    } end
    local began = false
    maintenance.begin = function () began = true end
    assert.has_error(pull.pull_all,
      'Pull refuses modified raw .skg buffers: /client/raw.skg')
    assert.is_false(began)
  end)

  it('lists dirty views and explains partial-failure recovery', function ()
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, 'skg://dirty-view')
    registry.register(buf, 'content-view')
    vim.b[buf].skg_logical_dirty = true
    pull.local_repositories = function () return {
      { key = string.rep('a', 64), root = '/repo', sources = { 'one' } },
    } end
    local prompt
    pull.confirm = function (message)
      prompt = message
      return 2
    end
    local began = false
    maintenance.begin = function () began = true end
    assert.is_false(pull.pull_all())
    assert.is_false(began)
    assert.is_not_nil(prompt:find('skg://dirty-view [content-view]', 1, true))
    assert.is_not_nil(prompt:find(
      'orthogonal buffers will remain editable', 1, true))
    assert.is_not_nil(prompt:find(
      'partially failed pull will still reconcile the final disk', 1, true))
  end)

  it('opens an incident-qualified interactive terminal with an argv list',
     function ()
    local repository = {
      key = string.rep('a', 64),
      root = '/repo with space',
      sources = { 'one' },
    }
    local context = pull.new_context({ repository })
    state.maintenance_client_incident = pull_incident(context)
    local argv, options
    pull.jobstart = function (job_argv, job_options)
      argv, options = job_argv, job_options
      return 17
    end
    local job, buf = pull.start_terminal(repository, context, function () end)
    assert.are.equal(17, job)
    assert.are.same({ 'git', '-C', '/repo with space', 'pull' }, argv)
    assert.is_true(options.term)
    assert.is_not_nil(vim.api.nvim_buf_get_name(buf):find(
      'skg://pull/' .. incident_id, 1, true))
    assert.are.equal('hide', vim.bo[buf].bufhidden)
  end)

  it('runs repository jobs serially and reports partial failure', function ()
    local repositories = {
      { key = string.rep('a', 64), root = '/one', sources = { 'one' } },
      { key = string.rep('b', 64), root = '/two', sources = { 'two' } },
    }
    local context = pull.new_context(repositories)
    context.remaining = vim.deepcopy(repositories)
    state.maintenance_client_incident = pull_incident(context)
    local started, callbacks, scheduled = {}, {}, {}
    pull.start_terminal = function (repository, _context, callback)
      table.insert(started, repository.key)
      callbacks[repository.key] = callback
      return #started + 10
    end
    pull.defer = function (callback) table.insert(scheduled, callback) end
    local finished
    pull.finish_origin = function (outcome, details)
      finished = { outcome = outcome, details = details }
    end

    pull.start_next()
    assert.are.same({ repositories[1].key }, started)
    callbacks[repositories[1].key](11, 0, 'exit')
    assert.are.equal(1, #scheduled)
    table.remove(scheduled, 1)()
    assert.are.same({ repositories[1].key, repositories[2].key }, started)
    callbacks[repositories[2].key](12, 1, 'exit')
    table.remove(scheduled, 1)()
    assert.are.equal('failed', finished.outcome)
    assert.are.equal(2, #finished.details)
    assert.is_nil(finished.details[1]:find('/one', 1, true))
    assert.matches('exit 1$', finished.details[2])
  end)

  it('reports a replayed authorization without a live child as indeterminate',
     function ()
    local context = pull.new_context({})
    state.maintenance_client_incident = pull_incident(context)
    local scheduled = {}
    pull.defer = function (callback) table.insert(scheduled, callback) end
    local finished
    pull.finish_origin = function (outcome, details)
      finished = { outcome = outcome, details = details }
    end
    pull.handle_authorization(nil, {
      f('status', 'external-mutation-authorized'),
      f('incident-id', incident_id), f('maintenance-epoch', 9),
      f('phase', 'running-external-mutation'), f('replayed', 'true'),
    })
    assert.are.equal(1, #scheduled)
    scheduled[1]()
    assert.are.equal('indeterminate', finished.outcome)
    assert.matches('without a live owned Git child', finished.details[1])
  end)

  it('resends a known result when running maintenance reconnects', function ()
    local context = pull.new_context({})
    context.external_result = {
      outcome = 'failed', details = { 'repository failed' },
    }
    state.maintenance_client_incident = pull_incident(context)
    local scheduled = {}
    pull.defer = function (callback) table.insert(scheduled, callback) end
    local sent
    pull.send_finish = function (record) sent = record end
    pull.resume_running('lost child')
    scheduled[1]()
    assert.are.equal(context.external_result, sent)
  end)

  it('preserves a live child and a scheduled repository hop on reconnect',
     function ()
    local repository = {
      key = string.rep('a', 64), root = '/repo', sources = { 'one' },
    }
    local context = pull.new_context({ repository })
    context.current_job = {
      id = 17, key = repository.key, repository = repository,
    }
    state.maintenance_client_incident = pull_incident(context)
    pull.job_status = function () return 'running' end
    pull.defer = function () error('scheduled duplicate work') end
    assert.is_true(pull.resume_running('lost child'))

    context.current_job = nil
    context.advance_pending = true
    assert.is_true(pull.resume_running('lost scheduled hop'))
  end)

  it('restores the journaled mapping and result in final observation',
     function ()
    local sources = { 'one' }
    local key = pull.repository_key(sources)
    local context = pull.new_context({
      { key = key, root = '/repo', sources = sources },
    })
    state.maintenance_client_incident = pull_incident(context)
    local scheduled = {}
    pull.defer = function (callback) table.insert(scheduled, callback) end
    local sent
    pull.send_finish = function (record) sent = record end
    local response = {
      f('pull-repositories', { repository_record(key, sources) }),
      f('external-outcome', 'failed'),
      f('external-details', { 'repository failed', 'disk may differ' }),
    }
    assert.is_true(pull.origin_operation_handler(
      state.maintenance_client_incident, 'final-observation', response))
    assert.are.same({ { key = key, sources = sources } },
      context.server_repositories)
    scheduled[1]()
    assert.are.same({
      outcome = 'failed',
      details = { 'repository failed', 'disk may differ' },
    }, sent)
  end)

  it('refuses a server repository topology change', function ()
    local local_sources = { 'one', 'two' }
    local local_key = pull.repository_key(local_sources)
    local server_key = pull.repository_key({ 'one' })
    local context = pull.new_context({
      { key = local_key, root = '/repo', sources = local_sources },
    })
    state.maintenance_client_incident = pull_incident(context)
    assert.has_error(function ()
      pull.install_server_repositories({
        f('pull-repositories', {
          repository_record(server_key, { 'one' }),
        }),
      })
    end)
  end)

  it('reports the exact external result and details to the incident',
     function ()
    local context = pull.new_context({})
    state.maintenance_client_incident = pull_incident(context)
    local wire, request_incident
    require('skg.client').submit_request = function (
        request_wire, _content, incident)
      wire, request_incident = request_wire, incident
    end
    pull.finish_origin('failed', { 'one failed', 'disk may differ' })
    local parsed = sexpr.read(wire)
    assert.are.equal('finish maintenance origin',
      payload.field_text(parsed, 'request'))
    assert.are.equal('failed', payload.field_text(parsed, 'external-outcome'))
    assert.are.same({ 'one failed', 'disk may differ' }, payload.string_list(
      payload.field(parsed, 'external-details')))
    assert.are.equal(incident_id, request_incident)
    assert.are.equal('origin-completion-pending',
      state.maintenance_client_incident.phase)
  end)

  it('installs the public pull command with the session surface', function ()
    require('skg').install_session_surface()
    assert.are.equal(2, vim.fn.exists(':SkgPullAll'))
  end)
end)
