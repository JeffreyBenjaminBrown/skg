-- Coverage for the rerender stream and its consumers (skgrepo
-- sets), plus the report-shaped requests (investigate unknown node,
-- export). Mirrors
-- tests/elisp/test-skg-investigate-unknown-node.el's wiring cases and
-- test-skg-warning-channel.el's rerender-done
-- channel, end-to-end through the loopback fake server.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local client = require('skg.client')
local investigate_unknown_node = require('skg.investigate_unknown_node')
local length_prefix = require('skg.length_prefix')
local lock = require('skg.lock')
local rerender = require('skg.rerender')
local repo_sets = require('skg.repo_sets')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local function wipe_named (name)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == name then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
end

describe('skg stream consumers', function ()
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
    wipe_named('skg://messages/rerender')
    wipe_named('skg://unknown-node')
  end)

  it('sends a streaming request after the prologue', function ()
    -- Mirrors test-skg-send-streaming-request-sends-after-the-prologue.
    local calls = {}
    local stubs = {
      { lock, 'begin_stream', function (label)
          table.insert(calls, 'begin_stream ' .. label) end },
      { lock, 'lock_all_skg_buffers', function ()
          table.insert(calls, 'lock') end },
      { rerender, 'register_rerender_stream_handlers', function ()
          table.insert(calls, 'rerender handlers') end },
      { state, 'register_response_handler',
        function (response_type, _handler, one_shot)
          table.insert(calls, 'handler ' .. response_type
                              .. (one_shot and ' one-shot' or '')) end },
      { state, 'lp_reset', function () table.insert(calls, 'reset') end },
      { client, 'send_string', function (text)
          table.insert(calls, 'send ' .. text) end } }
    local originals = {}
    for i, stub in ipairs(stubs) do
      originals[i] = stub[1][stub[2]]
      stub[1][stub[2]] = stub[3] end
    local ok, err = pcall(rerender.send_streaming_request,
      'the label', 'the-answer',
      { sexpr.pair(sexpr.symbol('request'), 'a request') },
      function () end,
      { ['other-answer'] = function () end })
    for i, stub in ipairs(stubs) do stub[1][stub[2]] = originals[i] end
    assert(ok, err)
    assert.are.same({ 'begin_stream the label',
                      'lock',
                      'rerender handlers',
                      'handler the-answer one-shot',
                      'handler other-answer one-shot',
                      'reset',
                      'send ((request . "a request"))\n' },
                    calls)
  end)

  it('registers an over-private-text retry after the answer handlers',
     function ()
    -- Mirrors the last case of
    -- test-skg-send-streaming-request-sends-after-the-prologue.
    local calls = {}
    local stubs = {
      { lock, 'begin_stream', function () end },
      { lock, 'lock_all_skg_buffers', function () end },
      { rerender, 'register_rerender_stream_handlers', function () end },
      { state, 'register_response_handler',
        function (response_type) table.insert(calls,
                                              'handler ' .. response_type) end },
      { rerender, 'register_overPrivateText_confirmation',
        function (_retry, unfired_response_type)
          table.insert(calls, 'confirmation ' .. unfired_response_type) end },
      { state, 'lp_reset', function () table.insert(calls, 'reset') end },
      { client, 'send_string', function () table.insert(calls, 'send') end } }
    local originals = {}
    for i, stub in ipairs(stubs) do
      originals[i] = stub[1][stub[2]]
      stub[1][stub[2]] = stub[3] end
    local ok, err = pcall(rerender.send_streaming_request,
      'the label', 'the-answer',
      { sexpr.pair(sexpr.symbol('request'), 'a request') },
      function () end, nil, function () end)
    for i, stub in ipairs(stubs) do stub[1][stub[2]] = originals[i] end
    assert(ok, err)
    assert.are.same({ 'handler the-answer', 'confirmation the-answer',
                      'reset', 'send' },
                    calls)
  end)

  it('unwinds a streaming request whose send fails', function ()
    -- Mirrors test-skg-send-streaming-request-unwinds-a-failed-send.
    local original_send = client.send_string
    client.send_string = function () error('connection lost') end
    local ok = pcall(rerender.send_streaming_request,
      'the label', 'the-answer',
      { sexpr.pair(sexpr.symbol('request'), 'a request') },
      function () end, { ['other-answer'] = function () end },
      function () end)
    client.send_string = original_send
    assert.is_false(ok)
    assert.are.same({}, state.response_handler_map)
    assert.are.equal(0, state.lp_pending_count)
    assert.is_nil(lock.stream_in_progress)
  end)

  it("cancels the repo-set switch's answer on a confirmation and"
     .. ' retries with the approved PIDs', function ()
    -- Mirrors
    -- test-skg-restrict-repo-set-confirmation-cancels-its-answer-and-retries.
    local sent = {}
    local original_send, original_confirm =
      client.send_string, vim.fn.confirm
    client.send_string = function (text) table.insert(sent, text) end
    vim.fn.confirm = function () return 1 end
    local ok, err = pcall(function ()
      repo_sets.restrict_repo_set('public')
      assert.are.equal(1, #sent)
      assert.is_falsy(sent[1]:find('approved-overPrivateText-pids', 1, true))
      assert.is_truthy(state.response_handler_map['skgrepo-restriction'])
      assert.are.equal(3, state.lp_pending_count) -- lock, done, the answer
      length_prefix.dispatch_by_type(
        '((response-type overPrivateText-telescope-confirmation)'
        .. ' (operation rerender) (pids (p1 p2)) (prompt "Include?"))')
      assert.is_nil(state.response_handler_map['skgrepo-restriction'])
      assert.are.equal(2, state.lp_pending_count)
      length_prefix.dispatch_by_type(
        '((response-type rerender-lock) (lock-views ()))')
      length_prefix.dispatch_by_type(
        '((response-type rerender-done) (errors ()) (warnings ()))')
      vim.wait(2000, function () return #sent == 2 end, 10)
      assert.are.equal(2, #sent)
      assert.are.equal(
        '((request . "set skgrepo restriction") (name . "public")'
        .. ' (approved-overPrivateText-pids "p1" "p2"))\n',
        sent[2])
      assert.is_truthy(state.response_handler_map['skgrepo-restriction'])
    end)
    client.send_string, vim.fn.confirm = original_send, original_confirm
    assert(ok, err)
  end)

  it("joins a response's errors into one line", function ()
    assert.are.equal('first; second', rerender.response_errors_line(
      sexpr.read('((errors ("first" "second")) (warnings ("w")))')))
    assert.are.equal('only', rerender.response_errors_line(
      sexpr.read('((errors ("only")))')))
    assert.is_nil(rerender.response_errors_line(
      sexpr.read('((errors ()) (warnings ("w")))')))
    assert.is_nil(rerender.response_errors_line(
      sexpr.read('((content "x"))')))
  end)

  it('drives the rerender stream: lock, per-view update, done with'
     .. ' warnings', function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('rerender all views', 1, true) then
        respond(helpers.framed(
          '((response-type rerender-lock)'
          .. ' (lock-views (view_id-a)))'))
        respond(helpers.framed(
          '((response-type rerender-view) (view-id view_id-a)'
          .. ' (content "* (skg node (gnid a)) a rerendered"))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ())'
          .. ' (warnings ("stale herald")))'))
      end
    end)
    local a = buffer.open_org_buffer_from_text(
      '* (skg node (gnid a)) a', 'skg://a', 'view_id-a')
    local b = buffer.open_org_buffer_from_text(
      '* (skg node (gnid b)) b', 'skg://b', 'view_id-b')
    rerender.request_rerender_all_views()
    -- Locked immediately; b unlocks on rerender-lock (not listed).
    assert.is_false(vim.bo[a].modifiable)
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    assert.is_true(vim.bo[a].modifiable)
    assert.is_true(vim.bo[b].modifiable)
    assert.are.equal('* (skg node (gnid a)) a rerendered',
      vim.api.nvim_buf_get_lines(a, 0, 1, false)[1])
    -- The warning channel fired (mirrors test-skg-warning-channel).
    local found = nil
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf)
         == 'skg://messages/rerender' then found = buf end
    end
    assert.is_truthy(found)
    assert.is_truthy(table.concat(
      vim.api.nvim_buf_get_lines(found, 0, -1, false), '\n')
      :find('stale herald', 1, true))
  end)

  it('appends the corrections on rerender-done to the server-warnings'
     .. ' buffer', function ()
    local server_warnings = require('skg.server_warnings')
    wipe_named(server_warnings.buffer_name)
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('rerender all views', 1, true) then
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ()) (warnings ())'
          .. ' (corrections ((content-view:3'
          .. ' "Drew what the graph says belongs here.\\n*** new child\\n")'
          .. ' ("search:two words"'
          .. ' "Dropped a duplicate.\\n*** twin\\n"))))'))
      end
    end)
    buffer.open_org_buffer_from_text(
      '* (skg node (gnid a)) a', 'skg://corrected', 'content-view:3')
    require('skg.messages').scratch_org_buffer(
      server_warnings.buffer_name, '* an earlier startup warning')
    rerender.request_rerender_all_views()
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    local found = nil
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf)
         == server_warnings.buffer_name then found = buf end
    end
    assert.is_truthy(found)
    assert.are.same({
      '* an earlier startup warning',
      '* skg://corrected',
      '** Drew what the graph says belongs here.',
      '*** new child',
      '* search:two words',
      '** Dropped a duplicate.',
      '*** twin',
    }, vim.api.nvim_buf_get_lines(found, 0, -1, false))
    wipe_named(server_warnings.buffer_name)
  end)

  it('switches the repo-set and rides the rerender stream',
     function ()
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('set skgrepo restriction', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type skgrepo-restriction) (restriction "public")'
          .. ' (content "Skgrepo restriction: public"))'))
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ())'
          .. ' (warnings ()))'))
      end
    end)
    repo_sets.restrict_repo_set('public')
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil and seen ~= nil end, 10)
    assert.are.equal(
      '((request . "set skgrepo restriction") (name . "public"))',
      seen)
  end)

  it('investigates the unknown headline at point and shows the answer'
     .. ' read-only, with the navigation keymap', function ()
    -- Mirrors test-skg-investigate-unknown-node.el.
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('investigate unknown node', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type investigate-unknown-node)'
          .. ' (content "* gone-id\\n** never present in the git history'
          .. ' of any skgrepo") (errors ()) (warnings ()))'))
      end
    end)
    local view = buffer.open_org_buffer_from_text(
      '* (skg node (gnid p)) p\n** (skg unknown (gnid gone-id)) ',
      'skg://p', 'view_id-p')
    vim.api.nvim_set_current_buf(view)
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    investigate_unknown_node.investigate_unknown_node()
    vim.wait(3000, function () return seen ~= nil end, 10)
    assert.are.equal(
      '((request . "investigate unknown node") (id . "gone-id"))', seen)
    local answer = nil
    vim.wait(2000, function ()
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_get_name(buf) == 'skg://unknown-node'
        then answer = buf end
      end
      return answer ~= nil
    end, 10)
    assert.is_truthy(answer)
    assert.are.equal('org', vim.bo[answer].filetype)
    assert.is_false(vim.bo[answer].modifiable)
    assert.is_truthy(table.concat(
      vim.api.nvim_buf_get_lines(answer, 0, -1, false), '\n')
      :find('never present', 1, true))
    local has_goto = false
    for _, map in ipairs(vim.api.nvim_buf_get_keymap(answer, 'n')) do
      if map.desc and map.desc:find('Open a view') then
        has_goto = true end
    end
    assert.is_true(has_goto)
  end)

  it('prompts for the ID away from an unknown headline', function ()
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('investigate unknown node', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type investigate-unknown-node)'
          .. ' (content "typed-id resolves to node t (typed-id) in main")'
          .. ' (errors ()) (warnings ()))'))
      end
    end)
    local view = buffer.open_org_buffer_from_text(
      '* (skg node (gnid p)) p', 'skg://p', 'view_id-p')
    vim.api.nvim_set_current_buf(view)
    local original_input = vim.fn.input
    local prompted = nil
    vim.fn.input = function (prompt) prompted = prompt return 'typed-id' end
    investigate_unknown_node.investigate_unknown_node()
    vim.fn.input = original_input
    assert.are.equal('Investigate ID: ', prompted)
    vim.wait(3000, function () return seen ~= nil end, 10)
    assert.is_truthy(seen:find('(id . "typed-id")', 1, true))
  end)
end)
