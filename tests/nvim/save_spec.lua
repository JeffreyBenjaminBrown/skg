-- Mirrors tests/elisp/test-skg-save-response-folded-root.el (point
-- and fold restoration, warning channels) and the client-side halves
-- of test-skg-fork-confirmation.el (request fields, the confirmation
-- buffer's skgrepo walk, approve/decline behavior), driven end-to-end
-- through the loopback fake server. The live-server counterparts live
-- in tests/integration/.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local folds = require('skg.folds')
local lock = require('skg.lock')
local metadata = require('skg.metadata')
local save = require('skg.save')
local search = require('skg.search')
local sexpr = require('skg.sexpr.parse')
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

  it('adds approved-forks and fork-repos when given', function ()
    -- Mirrors test-skg-fork-confirmation's request-field cases.
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 0,
      column = 0,
      screen_lines_below_window_start = 0 },
      true, { { 'id-a', 'src-1' }, { 'id-b', 'src-2' } })
    assert.is_truthy(line:find('(approved-forks . "true")', 1, true))
    assert.is_truthy(line:find(
      '(fork-repos (("id-a" . "src-1") ("id-b" . "src-2")))',
      1, true))
  end)

  it('adds exact approved-hoist pids when given', function ()
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 0,
      column = 0,
      screen_lines_below_window_start = 0 },
      nil, nil, { 'pid-a', 'pid-b' })
    assert.is_truthy(line:find(
      '(approved-hoist-pids "pid-a" "pid-b")', 1, true))
    assert.is_falsy(line:find('hoist-approved . "true"', 1, true))
  end)

  it('adds exact text-release pids when given', function ()
    local line = save.save_request_string('uri-1', {
      lines_below_focused_headline = 0,
      column = 0,
      screen_lines_below_window_start = 0 },
      nil, nil, nil, { 'overPrivateText-a', 'overPrivateText-b' })
    assert.is_truthy(line:find(
      '(approved-overPrivateText-pids "overPrivateText-a" "overPrivateText-b")', 1, true))
  end)
end)

describe('skg.save pipeline', function ()
  local server

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
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

  it('save markers leave a clean buffer clean, so nothing asks to confirm',
     function ()
    local asked = 0
    local real_confirm = vim.fn.confirm
    vim.fn.confirm = function () asked = asked + 1 return 2 end
    local dirty = open_view('* (skg (node (id a))) a', 'skg://a', 'uri-a')
    vim.api.nvim_buf_set_lines(dirty, 1, 1, false, { 'edit' })
    local clean = open_view(
      '* (skg (node (id b))) b\n** (skg (node (id c))) c',
      'skg://b', 'uri-b')
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    save.buffer_snapshot_with_save_markers(clean, true)
    vim.wait(100, function () return false end)
    vim.fn.confirm = real_confirm
    assert.are.equal(0, asked)
    assert.is_false(vim.bo[clean].modified)
  end)

  it('retains broad locks until save-relax-lock narrows them',
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
    vim.wait(3000, function () return respond_fn ~= nil end, 10)
    vim.wait(50)
    -- save-lock only acknowledges the broad client lock.  In particular,
    -- views unknown to the server cannot be released from that message.
    assert.is_false(vim.bo[bystander].modifiable)
    assert.is_false(vim.bo[saved].modifiable)
    assert.is_false(vim.bo[collateral].modifiable)
    respond_fn(helpers.framed(
      '((response-type save-relax-lock)'
      .. ' (lock-views (uri-collateral)))'))
    vim.wait(3000, function () return vim.bo[bystander].modifiable end, 10)
    assert.is_true(vim.bo[bystander].modifiable)
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

  it('keeps dirty conflict-check inputs locked through relaxation',
     function ()
    local respond_fn = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('save buffer', 1, true) then
        respond_fn = respond
        respond(helpers.framed(
          '((response-type save-lock) (lock-views ()))'))
      end
    end)
    local saved = open_view('* (skg (node (id a))) a',
                            'skg://a', 'uri-saved')
    local dirty = open_view('* (skg (node (id b))) b\nlocal edit',
                            'skg://b', 'uri-dirty')
    vim.bo[dirty].modified = true
    local clean = open_view('* (skg (node (id c))) c',
                            'skg://c', 'uri-clean')
    vim.api.nvim_set_current_buf(saved)
    save.request_save_buffer()
    vim.wait(3000, function () return respond_fn ~= nil end, 10)
    respond_fn(helpers.framed(
      '((response-type save-relax-lock) (lock-views (uri-dirty)))'))
    vim.wait(3000, function () return vim.bo[clean].modifiable end, 10)
    assert.is_false(vim.bo[saved].modifiable)
    assert.is_false(vim.bo[dirty].modifiable)
    assert.is_true(vim.bo[clean].modifiable)
    respond_fn(helpers.framed(
      '((response-type save-result)'
      .. ' (content "* (skg (node (id a))) a")'
      .. ' (errors ()) (warnings ()))'))
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    assert.is_true(vim.bo[dirty].modifiable)
  end)

  it('retains every lock after a malformed relaxation', function ()
    local saved = open_view('* (skg (node (id a))) a',
                            'skg://a', 'uri-saved')
    local other = open_view('* (skg (node (id b))) b',
                            'skg://b', 'uri-other')
    lock.lock_all_skg_buffers()
    save.save_relax_lock_handler(
      'uri-saved', sexpr.read('((lock-views not-a-list))'))
    assert.is_false(vim.bo[saved].modifiable)
    assert.is_false(vim.bo[other].modifiable)
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
                     or message:find('modifiable'))
  end)

  it('a pending save blocks search until terminal cleanup', function ()
    server = helpers.connect_to_fake_server(function () end)
    open_view('* (skg (node (id a))) a', 'skg://guard-search',
              'uri-guard-search')
    save.request_save_buffer()
    local ok, err = pcall(
      search.request_text_search, 'blocked', false, false, false)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find(
      'save already in progress', 1, true))
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

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  local fork_confirmation_payload =
    '((response-type fork-confirmation)'
    .. ' (content "* (skg (node (repo PICK-A-REPO))) clone-to-be'
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
    local source = open_view(
      '* (skg (node (id foreign-1)'
      .. ' (viewRequests fork))) the original edited',
      'skg://source', 'uri-origin')
    save.request_save_buffer()
    vim.wait(3000, function ()
      return vim.api.nvim_buf_get_name(
        vim.api.nvim_get_current_buf()) == 'skg://fork-confirmation'
    end, 10)
    return source, requests
  end

  it('walks the confirmation buffer for {id, repo} pairs',
     function ()
    -- Mirrors the two-level skgrepo walk (+ no leak across parents).
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
      '* (skg (node (repo src-one))) clone one',
      '** (skg (node (id orig-1))) original one',
      '* metadata-less parent',
      '** (skg (node (id orphan))) must not inherit src-one',
      '* (skg (node (repo src-two))) clone two',
      '** (skg (node (id orig-2))) original two' })
    assert.are.same(
      { { 'orig-1', 'src-one' }, { 'orig-2', 'src-two' } },
      save.fork_repos_from_confirmation_buffer(buf))
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('shows the confirmation buffer, unlocked and navigable',
     function ()
    local source = save_and_get_confirmation()
    local confirm = vim.api.nvim_get_current_buf()
    assert.are.equal('skg://fork-confirmation',
                     vim.api.nvim_buf_get_name(confirm))
    assert.is_true(vim.bo[confirm].modifiable)
    assert.is_true(vim.bo[source].modifiable) -- everything unlocked
    assert.is_nil(vim.b[confirm].skg_view_uri)
    assert.are.equal(0, state.lp_pending_count) -- balanced
  end)

  it('refuses approval while the placeholder repo remains',
     function ()
    save_and_get_confirmation()
    local ok, err = pcall(save.approve_fork)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('Pick a repo'))
  end)

  it('approves after a repo is chosen, re-saving with the pairs',
     function ()
    local source, requests = save_and_get_confirmation()
    local confirm = vim.api.nvim_get_current_buf()
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
    metadata.change_repo_at_line(1, 'my-repo')
    save.approve_fork()
    vim.wait(3000, function () return #requests >= 2 end, 10)
    assert.are.equal(2, #requests)
    assert.is_truthy(requests[2]:find(
      '(approved-forks . "true")', 1, true))
    assert.is_truthy(requests[2]:find(
      '(fork-repos (("foreign-1" . "my-repo")))', 1, true))
    assert.is_false(vim.api.nvim_buf_is_valid(confirm))
    -- The source buffer's fork atom survived to the re-save.
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    assert.are.equal(source, vim.api.nvim_get_current_buf())
  end)

  it('declining strips the fork atom from the source buffer', function ()
    local source = save_and_get_confirmation()
    save.decline_fork()
    local text = table.concat(
      vim.api.nvim_buf_get_lines(source, 0, -1, false), '\n')
    assert.is_falsy(text:find('fork', 1, true))
    -- The confirmation buffer stays open for reference.
    assert.are.equal('skg://fork-confirmation',
      vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()))
  end)
end)
