-- Mirrors tests/elisp/test-skg-save-response-folded-root.el (point
-- and fold restoration, warning channels) and the client-side halves
-- of test-skg-fork-confirmation.el (request fields, the confirmation
-- buffer's source walk, approve/decline behavior), driven end-to-end
-- through the loopback fake server. The live-server counterparts live
-- in tests/integration/.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local config = require('skg.config')
local registry = require('skg.buffer_registry')
local folds = require('skg.folds')
local lock = require('skg.lock')
local metadata = require('skg.metadata')
local save = require('skg.save')
local state = require('skg.state')

local function open_view (text, name, uri)
  local buf = buffer.open_org_buffer_from_text(text, name, uri)
  require('skg.folds').set_up_window()
  return buf
end

describe('skg.save request strings', function ()
  it('carries the uri and the three point fields', function ()
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 2,
      column = 5,
      screen_lines_below_window_start = 3 }, nil, nil)
    assert.are.equal(
      '((request . "save buffer") (view-uri . "uri-1")'
      .. ' (point-lines-below-focused-headline . "2")'
      .. ' (point-column . "5")'
      .. ' (point-screen-lines-below-window-start . "3"))\n',
      line)
  end)

  it('adds fork-approved and fork-sources when given', function ()
    -- Mirrors test-skg-fork-confirmation's request-field cases.
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 0,
      column = 0,
      screen_lines_below_window_start = 0 },
      true, { { 'id-a', 'src-1' }, { 'id-b', 'src-2' } })
    assert.is_truthy(line:find('(fork-approved . "true")', 1, true))
    assert.is_truthy(line:find(
      '(fork-sources (("id-a" . "src-1") ("id-b" . "src-2")))',
      1, true))
  end)

  it('adds exact hoist-approved pids when given', function ()
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 0,
      column = 0,
      screen_lines_below_window_start = 0 },
      nil, nil, { 'pid-a', 'pid-b' })
    assert.is_truthy(line:find(
      '(hoist-approved-pids "pid-a" "pid-b")', 1, true))
    assert.is_falsy(line:find('hoist-approved . "true"', 1, true))
  end)

  it('adds exact scalar-release pids when given', function ()
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 0,
      column = 0,
      screen_lines_below_window_start = 0 },
      nil, nil, nil, { 'ugly-a', 'ugly-b' })
    assert.is_truthy(line:find(
      '(allow-ugly-telescopes "ugly-a" "ugly-b")', 1, true))
  end)

  it('puts operation identity on the request with fingerprint final', function ()
    local operation_id = '11111111-2222-4333-8444-555555555555'
    local fingerprint = string.rep('a', 64)
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 0,
      column = 0,
      screen_lines_below_window_start = 0,
    }, nil, nil, nil, nil, nil, operation_id, fingerprint)
    assert.is_truthy(line:find(
      '(operation-id . "' .. operation_id .. '")', 1, true))
    local ending = ' (request-base-fingerprint . "'
      .. fingerprint .. '"))\n'
    assert.are.equal(ending, line:sub(-#ending))
  end)
end)

describe('skg.save pipeline', function ()
  local server
  local pending_root

  before_each(function ()
    pending_root = vim.fn.tempname()
    vim.fn.mkdir(pending_root, 'p')
    config.config_file_path = pending_root .. '/skgconfig.toml'
    vim.fn.writefile({ 'port = 1731' }, config.config_file_path)
    state.maintenance_archive_folder = 'archive'
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
    config.store_state = nil
    config.config_file_path = nil
    state.maintenance_archive_folder = nil
    vim.fn.delete(pending_root, 'rf')
  end)

  it('refuses known rebuilding before taking a transient save lock',
     function ()
    local buf = open_view(
      '* (skg (node (id root))) root', 'skg://rebuilding',
      'uri-rebuilding')
    local before = registry.raw_text(buf)
    state.rebuilding = true
    local ok, err = pcall(save.request_save_buffer)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find(
      'nothing was saved; try again when ready', 1, true))
    assert.are.equal(before, registry.raw_text(buf))
    assert.is_falsy(vim.b[buf].skg_save_locked)
    assert.is_nil(lock.stream_in_progress)
  end)

  it('refuses a known maintenance restriction before the save lock',
     function ()
    local buf = open_view(
      '* (skg (node (id root))) root', 'skg://maintenance-save',
      'uri-maintenance-save')
    registry.lock_for_maintenance(buf, 4)
    local ok, err = pcall(save.request_save_buffer)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find(
      'maintenance-locked for epoch 4', 1, true))
    assert.is_falsy(vim.b[buf].skg_save_locked)
    assert.are.equal(4, registry.record(buf).maintenance_epoch)
    assert.is_false(vim.bo[buf].modifiable)
  end)

  it('refuses a read-only result before taking a save lock', function ()
    local buf = buffer.open_org_buffer_from_text(
      '* (skg (node (id root))) result', 'skg://readonly-result',
      'uri-readonly-result', {
        view_write_authority = 'read-only',
      })
    vim.api.nvim_set_current_buf(buf)
    assert.is_false(vim.bo[buf].modifiable)
    local ok, err = pcall(save.request_save_buffer)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('read-only result authority', 1, true))
    assert.is_falsy(vim.b[buf].skg_save_locked)
    assert.is_nil(lock.stream_in_progress)
  end)

  it('round-trips a save: markers out, redraw in, point restored',
     function ()
    local seen_request = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('save buffer', 1, true) then
        seen_request = line
        respond(helpers.framed(
          '((response-type save-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type save-result)'
          .. ' (content "* (skg (node (id root)) focused) root'
          .. '\\nroot body'
          .. '\\n** (skg (node (id child)) folded) child")'
          .. ' (errors ()) (warnings ())'
          .. ' (point-lines-below-focused-headline 1)'
          .. ' (point-column 3)'
          .. ' (point-screen-lines-below-window-start 1))'))
      end
    end)
    local buf = open_view(
      '* (skg (node (id root))) root\nroot body\nedited line',
      'skg://save-me', 'uri-save')
    vim.api.nvim_win_set_cursor(0, { 2, 4 })
    save.request_save_buffer()
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil and seen_request ~= nil
             and vim.bo[buf].modifiable
    end, 10)
    -- The request went out with the markers embedded.
    assert.is_truthy(seen_request:find('uri-save', 1, true))
    -- The buffer holds the redraw, markers stripped.
    local text = table.concat(
      vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
    assert.is_falsy(text:find('focused', 1, true))
    assert.is_falsy(text:find('folded', 1, true))
    assert.is_truthy(text:find('child', 1, true))
    -- The folded marker was acted on: the child is hidden.
    assert.is_true(folds.line_invisible_p(3))
    -- Point: one line below the focused headline, byte column 3.
    local cursor = vim.api.nvim_win_get_cursor(0)
    assert.are.same({ 2, 3 }, cursor)
    -- Unlocked, unmodified.
    assert.is_true(vim.bo[buf].modifiable)
    assert.is_false(vim.bo[buf].modified)
  end)

  it('advances the graph baseline inherited by later buffers', function ()
    config.store_state = { graph_generation = 1 }
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('save buffer', 1, true) then
        respond(helpers.framed(
          '((response-type save-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type save-result)'
          .. ' (content "* (skg (node (id root))) root\\n")'
          .. ' (errors ()) (warnings ()) (root-ids (root))'
          .. ' (graph-generation 2) (presentation-generation 0)'
          .. ' (server-revision 1) (client-application-token 2))'))
      end
    end)
    local saved = open_view(
      '* (skg (node (id root))) root', 'skg://save-generation',
      'uri-save-generation')
    assert.are.equal(1, registry.record(saved).graph_generation)
    save.request_save_buffer()
    assert.is_true(vim.wait(3000, function ()
      return lock.stream_in_progress == nil
             and config.store_state.graph_generation == 2
    end, 10))
    local later = buffer.open_org_buffer_from_text(
      '* later', 'skg://later-generation', 'uri-later-generation', {
        kind = 'new-empty-content-view', recipe = { kind = 'new-empty' },
      })
    assert.are.equal(2, registry.record(later).graph_generation)
    config.observe_graph_generation(1)
    assert.are.equal(2, config.store_state.graph_generation)
  end)

  it('locks all views, then unlocks non-collateral on save-lock',
     function ()
    local respond_fn = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('save buffer', 1, true) then
        respond_fn = respond
        respond(helpers.framed(
          '((response-type save-lock)'
          .. ' (lock-views (uri-collateral)))'))
      end
    end)
    local saved = open_view('* (skg (node (id a))) a',
                            'skg://a', 'uri-saved')
    local collateral = open_view('* (skg (node (id b))) b',
                                 'skg://b', 'uri-collateral')
    local bystander = open_view('* (skg (node (id c))) c',
                                'skg://c', 'uri-bystander')
    vim.api.nvim_set_current_buf(saved)
    save.request_save_buffer()
    -- Immediately after sending, everything is locked.
    assert.is_false(vim.bo[saved].modifiable)
    assert.is_false(vim.bo[collateral].modifiable)
    assert.is_false(vim.bo[bystander].modifiable)
    vim.wait(3000, function ()
      return vim.bo[bystander].modifiable end, 10)
    -- save-lock arrived: the bystander is free, the saved view and
    -- the collateral stay locked until their updates arrive.
    assert.is_true(vim.bo[bystander].modifiable)
    assert.is_false(vim.bo[saved].modifiable)
    assert.is_false(vim.bo[collateral].modifiable)
    -- Stream the collateral update, then the terminal result.
    respond_fn(helpers.framed(
      '((response-type collateral-view) (view-uri uri-collateral)'
      .. ' (content "* (skg (node (id b))) b updated"))'))
    vim.wait(3000, function ()
      return vim.bo[collateral].modifiable end, 10)
    assert.is_true(vim.bo[collateral].modifiable)
    assert.are.equal('* (skg (node (id b))) b updated',
      vim.api.nvim_buf_get_lines(collateral, 0, 1, false)[1])
    assert.is_false(vim.bo[saved].modifiable) -- still awaiting result
    respond_fn(helpers.framed(
      '((response-type save-result)'
      .. ' (content "* (skg (node (id a))) a") (errors ())'
      .. ' (warnings ()))'))
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    assert.is_true(vim.bo[saved].modifiable)
  end)

  it('shows the warning channel on save-result', function ()
    -- Mirrors test-skg-save-response-folded-root's warning cases.
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('save buffer', 1, true) then
        respond(helpers.framed(
          '((response-type save-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type save-result)'
          .. ' (content "* (skg (node (id a))) a")'
          .. ' (errors ()) (warnings ("careful there")))'))
      end
    end)
    open_view('* (skg (node (id a))) a', 'skg://warn', 'uri-warn')
    save.request_save_buffer()
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    local found = nil
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf)
         == 'skg://messages/save-warnings' then found = buf end
    end
    assert.is_truthy(found)
    local text = table.concat(
      vim.api.nvim_buf_get_lines(found, 0, -1, false), '\n')
    assert.is_truthy(text:find('careful there', 1, true))
    vim.api.nvim_buf_delete(found, { force = true })
  end)

  it('keeps edited text and unresolved identity for a blocked result',
     function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('save buffer', 1, true) then
        local operation_id = line:match(
          '%(operation%-id%s+%.%s+"([^"]+)"%)')
        local fingerprint = line:match(
          '%(request%-base%-fingerprint%s+%.%s+"([^"]+)"%)')
        respond(helpers.framed(
          '((response-type save-result)'
          .. ' (content "server replacement")'
          .. ' (operation-id "' .. operation_id .. '")'
          .. ' (request-base-fingerprint "' .. fingerprint .. '")'
          .. ' (save-operation-state blocked)'
          .. ' (reason "authorized effects require recovery"))'))
      end
    end)
    local buf = open_view(
      '* (skg (node (id root))) locally edited',
      'skg://blocked-save', 'uri-blocked-save')
    vim.bo[buf].modified = true
    local before = registry.raw_text(buf)
    save.request_save_buffer()
    assert.is_true(vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10))
    assert.are.equal(before, registry.raw_text(buf))
    assert.is_true(vim.bo[buf].modified)
    assert.are.equal(1, #require('skg.pending_save').unresolved_records())
  end)

  it('refuses a second save while one streams', function ()
    server = helpers.connect_to_fake_server(function () end)
    open_view('* (skg (node (id a))) a', 'skg://guard', 'uri-guard')
    save.request_save_buffer()
    local ok, err = pcall(save.request_save_buffer)
    assert.is_false(ok)
    -- Like Emacs, the buffer lock rejects first (there the message
    -- was skg's own; here it is vim's modifiable error -- the
    -- documented deviation); the stream guard is the second line of
    -- defense.
    local message = tostring(err)
    assert.is_truthy(message:find('already in progress')
                     or message:find('modifiable')
                     or message:find('is unresolved'))
  end)

  it('keeps the first save lock when a refused second record settles',
     function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.b[buf].skg_view_uri = 'skg://owned-save-lock'
    vim.bo[buf].modifiable = true
    lock.begin_stream('save', 'first-save')
    lock.lock_for_save(buf, 'first-save')
    state.set_request_finalizer(function ()
      lock.end_stream('first-save')
      lock.unlock_all_save_locked('first-save')
    end)
    local first = state.take_request_record()
    state.set_request_finalizer(function ()
      lock.end_stream('second-save')
      lock.unlock_all_save_locked('second-save')
    end)
    local second = state.take_request_record()

    local original_handler = save.handle_save_response
    save.handle_save_response = function () end
    save.save_result_handler(buf, {}, 'second-save')
    save.handle_save_response = original_handler
    state.finish_request(second.id, 'complete')
    assert.is_true(vim.b[buf].skg_save_locked)
    assert.are.equal('save', lock.stream_in_progress)

    state.finish_request(first.id, 'complete')
    assert.is_falsy(vim.b[buf].skg_save_locked)
    assert.is_nil(lock.stream_in_progress)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('keeps the first save lock for a duplicate operation with a new request owner',
     function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.b[buf].skg_view_uri = 'skg://duplicate-save-lock'
    vim.bo[buf].modifiable = true
    local operation_id = 'same-durable-operation'
    lock.begin_stream('save', 'first-request')
    lock.lock_for_save(buf, 'first-request')
    state.set_request_finalizer(function ()
      lock.end_stream('first-request')
      lock.unlock_all_save_locked('first-request')
    end)
    local first = state.take_request_record()
    state.set_request_finalizer(function ()
      lock.end_stream('second-request')
      lock.unlock_all_save_locked('second-request')
    end)
    local second = state.take_request_record()

    assert.are.equal('same-durable-operation', operation_id)
    state.finish_request(second.id, 'refused')
    assert.is_true(vim.b[buf].skg_save_locked)
    assert.are.equal('first-request', lock.stream_owner)

    state.finish_request(first.id, 'complete')
    assert.is_falsy(vim.b[buf].skg_save_locked)
    assert.is_nil(lock.stream_in_progress)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('does not let an owner-specific cleanup clear legacy state', function ()
    lock.stream_in_progress = 'legacy save'
    lock.stream_owner = nil
    lock.end_stream('new-request')
    assert.are.equal('legacy save', lock.stream_in_progress)
    lock.end_stream()
    assert.is_nil(lock.stream_in_progress)
  end)

  it('refuses to save a buffer with no view uri', function ()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_set_current_buf(buf)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '* headline' })
    local ok, err = pcall(save.request_save_buffer)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('view uri is nil'))
  end)
end)

describe('skg.save fork confirmation', function ()
  local server
  local pending_root

  before_each(function ()
    pending_root = vim.fn.tempname()
    vim.fn.mkdir(pending_root, 'p')
    config.config_file_path = pending_root .. '/skgconfig.toml'
    vim.fn.writefile({ 'port = 1731' }, config.config_file_path)
    state.maintenance_archive_folder = 'archive'
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
    config.config_file_path = nil
    state.maintenance_archive_folder = nil
    vim.fn.delete(pending_root, 'rf')
  end)

  local fork_confirmation_payload =
    '((response-type fork-confirmation)'
    .. ' (content "* (skg (node (source PICK-A-SOURCE))) clone-to-be'
    .. '\\n** (skg (node (id foreign-1))) the original")'
    .. ' (to-minibuffer "Approve or decline the fork."))'

  local function save_and_get_confirmation ()
    local requests = {}
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('save buffer', 1, true) then
        table.insert(requests, line)
        if #requests == 1 then
          -- The real server emits the early save-lock before it
          -- detects forks; the fork handler relies on that when it
          -- balances the pending count.
          respond(helpers.framed(
            '((response-type save-lock) (lock-views ()))'))
          respond(helpers.framed(fork_confirmation_payload))
        else
          respond(helpers.framed(
            '((response-type save-lock) (lock-views ()))'))
          respond(helpers.framed(
            '((response-type save-result)'
            .. ' (content "* (skg (node (id foreign-1))) saved")'
            .. ' (errors ()) (warnings ()))'))
        end
      end
    end)
    local origin = open_view(
      '* (skg (node (id foreign-1)'
      .. ' (viewRequests fork))) the original edited',
      'skg://origin', 'uri-origin')
    save.request_save_buffer()
    vim.wait(3000, function ()
      return vim.api.nvim_buf_get_name(
        vim.api.nvim_get_current_buf()) == 'skg://fork-confirmation'
    end, 10)
    return origin, requests
  end

  it('walks the confirmation buffer for {id, source} pairs',
     function ()
    -- Mirrors the two-level source walk (+ no leak across parents).
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
      '* (skg (node (source src-one))) clone one',
      '** (skg (node (id orig-1))) original one',
      '* metadata-less parent',
      '** (skg (node (id orphan))) must not inherit src-one',
      '* (skg (node (source src-two))) clone two',
      '** (skg (node (id orig-2))) original two' })
    assert.are.same(
      { { 'orig-1', 'src-one' }, { 'orig-2', 'src-two' } },
      save.fork_sources_from_confirmation_buffer(buf))
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('refuses a delayed fork continuation before constructing or prompting',
     function ()
    local old_constructor = state.client_constructor_admission
    local before = #registry.buffers()
    state.client_constructor_admission = 'closed'
    local ok, err = pcall(save.show_fork_confirmation,
                          '* (skg (node (source PICK-A-SOURCE))) clone', nil)
    state.client_constructor_admission = old_constructor
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('constructor admission is closed',
                                        1, true))
    assert.are.equal(before, #registry.buffers())
  end)

  it('shows the confirmation buffer, unlocked and navigable',
     function ()
    local origin = save_and_get_confirmation()
    local confirm = vim.api.nvim_get_current_buf()
    assert.are.equal('skg://fork-confirmation',
                     vim.api.nvim_buf_get_name(confirm))
    assert.is_true(vim.bo[confirm].modifiable)
    assert.is_true(vim.bo[origin].modifiable) -- everything unlocked
    assert.is_nil(vim.b[confirm].skg_view_uri)
    local record = registry.record(confirm)
    assert.are.equal('fork-confirmation', record.kind)
    assert.are.equal(registry.record(origin).id, record.origin_buffer_id)
    assert.is_true(registry.record(origin).logical_dirty)
    assert.are.equal(0, state.lp_pending_count) -- balanced
  end)

  it('refuses approval while the placeholder source remains',
     function ()
    save_and_get_confirmation()
    local ok, err = pcall(save.approve_fork)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('Pick a source'))
  end)

  it('approves after a source is chosen, re-saving with the pairs',
     function ()
    local origin, requests = save_and_get_confirmation()
    local confirm = vim.api.nvim_get_current_buf()
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    metadata.change_source_at_line(1, 'my-source')
    save.approve_fork()
    vim.wait(3000, function () return #requests >= 2 end, 10)
    assert.are.equal(2, #requests)
    assert.is_truthy(requests[2]:find(
      '(fork-approved . "true")', 1, true))
    assert.is_truthy(requests[2]:find(
      '(fork-sources (("foreign-1" . "my-source")))', 1, true))
    assert.is_false(vim.api.nvim_buf_is_valid(confirm))
    -- The origin's fork atom survived to the re-save.
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    assert.are.equal(origin, vim.api.nvim_get_current_buf())
  end)

  it('declining strips the fork atom from the origin', function ()
    local origin = save_and_get_confirmation()
    save.decline_fork()
    local text = table.concat(
      vim.api.nvim_buf_get_lines(origin, 0, -1, false), '\n')
    assert.is_falsy(text:find('fork', 1, true))
    -- The confirmation buffer stays open for reference.
    assert.are.equal('skg://fork-confirmation',
      vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()))
  end)
end)
