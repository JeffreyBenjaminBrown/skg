-- Client coverage for the stage-and-commit command.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local lock = require('skg.lock')
local stage_and_commit = require('skg.stage_and_commit')

local NEEDS_CONFIRMATION =
  '((response-type stage-and-commit) (needs-confirmation . t)'
  .. ' (processed-summary (processed 1) (changed 3) (foreign 0) (order-only 0) (notEditable 0))'
  .. ' (errors ()) (warnings ()))'

describe('skg stage and commit', function ()
  local server
  local original_input, original_notify

  local function delete_commit_message_buffer ()
    local buf = stage_and_commit.commit_message_buffer()
    if buf then vim.api.nvim_buf_delete(buf, { force = true }) end
  end

  ---Open the commit-message buffer, write TEXT in it, and write it
  ---(':w'), which sends.
  local function write_and_send (text)
    stage_and_commit.stage_and_commit()
    local buf = stage_and_commit.commit_message_buffer()
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, '\n'))
    vim.api.nvim_buf_call(buf, function () vim.cmd('write') end)
    return buf
  end

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    helpers.reset_client_state()
    lock.end_stream()
    original_input, original_notify = vim.fn.input, vim.notify
    delete_commit_message_buffer()
  end)

  after_each(function ()
    vim.fn.input, vim.notify = original_input, original_notify
    if server then server.close() server = nil end
    delete_commit_message_buffer()
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  ---Answer the first request with NEEDS_CONFIRMATION, the second with
  ---commits and a rerendered, now empty, view.
  local function serve (requests)
    return helpers.connect_to_fake_server(function (line, respond)
      table.insert(requests, line)
      if #requests == 1 then
        respond(helpers.framed(NEEDS_CONFIRMATION))
        respond(helpers.framed('((response-type rerender-lock) (lock-views ()))'))
      else
        respond(helpers.framed(
          '((response-type stage-and-commit)'
          .. ' (commits ((public 5f135fea1585) (private bff3a57e0670)))'
          .. ' (errors ()) (warnings ()))'))
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ("diff-view: all changes")))'))
        respond(helpers.framed(
          '((response-type rerender-view) (view-id "diff-view: all changes") (content ""))'))
      end
      respond(helpers.framed(
        '((response-type rerender-done) (errors ()) (warnings ()))'))
    end)
  end

  it('resends confirmed on "yes", reports the commits, and empties the view', function ()
    local requests, notices, prompts = {}, {}, {}
    local typed = { 'yes' }
    vim.fn.input = function (prompt)
      table.insert(prompts, prompt)
      return table.remove(typed, 1) end
    vim.notify = function (text) table.insert(notices, text) end
    server = serve(requests)
    local view = buffer.open_org_buffer_from_text(
      '* (skg node (gnid a)) a', 'skg://diff-view: all changes',
      'diff-view: all changes')
    write_and_send('one message')
    vim.wait(3000, function ()
      return #requests == 2 and #notices > 0 and lock.stream_in_progress == nil
    end, 10)
    assert.are.equal(2, #requests, vim.inspect(requests))
    assert.is_truthy(requests[1]:find('(request . "stage and commit")', 1, true))
    assert.is_truthy(requests[1]:find('(message-lines "one message")', 1, true))
    assert.is_falsy(requests[1]:find('confirmed', 1, true))
    assert.is_truthy(requests[2]:find('(confirmed . t)', 1, true))
    assert.is_truthy(prompts[1]:find('1 of 3 changed files are processed', 1, true))
    assert.is_nil(stage_and_commit.commit_message_buffer())
    assert.is_truthy(vim.tbl_contains(
      notices, 'Committed in public (5f135fe), private (bff3a57).'), vim.inspect(notices))
    assert.are.same({ '' }, vim.api.nvim_buf_get_lines(view, 0, -1, false))
    assert.is_true(vim.bo[view].modifiable)
  end)

  it('sends a message of several lines one string per line', function ()
    local requests = {}
    vim.fn.input = function () return 'n' end
    server = serve(requests)
    write_and_send('summary\n\n  indented\n\n  ')
    vim.wait(3000, function ()
      return #requests == 1 and lock.stream_in_progress == nil end, 10)
    assert.are.equal(1, #requests, vim.inspect(requests))
    assert.is_truthy(requests[1]:find(
      '(message-lines "summary" "" "  indented")', 1, true), requests[1])
  end)

  it('does not resend unless the word typed is exactly "yes"', function ()
    local requests = {}
    local typed = { 'y' }
    vim.fn.input = function () return table.remove(typed, 1) end
    server = serve(requests)
    write_and_send('one message')
    vim.wait(3000, function ()
      return #requests == 1 and lock.stream_in_progress == nil end, 10)
    vim.wait(100, function () return #requests > 1 end, 10)
    assert.are.equal(1, #requests, vim.inspect(requests))
  end)

  it('names the gitrepos with entries already staged in the prompt', function ()
    local requests, prompts = {}, {}
    local typed = { 'no' }
    vim.fn.input = function (prompt)
      table.insert(prompts, prompt)
      return table.remove(typed, 1) end
    server = helpers.connect_to_fake_server(function (line, respond)
      table.insert(requests, line)
      respond(helpers.framed(
        '((response-type stage-and-commit) (needs-confirmation . t)'
        .. ' (already-staged (("/data/public" "notes.txt" "a b.skg")))'
        .. ' (processed-summary (processed 3) (changed 3) (foreign 0) (order-only 0) (notEditable 0))'
        .. ' (errors ()) (warnings ()))'))
      respond(helpers.framed('((response-type rerender-lock) (lock-views ()))'))
      respond(helpers.framed(
        '((response-type rerender-done) (errors ()) (warnings ()))'))
    end)
    write_and_send('one message')
    vim.wait(3000, function ()
      return #prompts == 1 and lock.stream_in_progress == nil end, 10)
    assert.are.equal(
      'Commit ready. All 3 of 3 changed files are processed.'
      .. ' Already staged, so committed too: /data/public: 2 paths'
      .. ' (listed in *skg already staged*).'
      .. ' Type yes to commit anyway: ',
      prompts[1])
    assert.are.same({ '* /data/public', 'notes.txt', 'a b.skg' },
      vim.api.nvim_buf_get_lines(
        vim.fn.bufnr('*skg already staged*'), 0, -1, false))
    assert.are.equal(1, #requests, vim.inspect(requests))
  end)

  for _, case in ipairs({
      { name = 'a decline', answer = function () return 'no' end },
      { name = 'an interrupt',
        answer = function () error('Keyboard interrupt') end } }) do
    it('says "Not committed" on ' .. case.name
       .. ', with the stream done and the message kept', function ()
      local requests, notices = {}, {}
      local not_committed =
        'Not committed; your message is kept in skg://commit-message.'
      vim.fn.input = case.answer
      vim.notify = function (text) table.insert(notices, text) end
      server = serve(requests)
      write_and_send('summary\n\nbody')
      vim.wait(3000, function ()
        return vim.tbl_contains(notices, not_committed) end, 10)
      assert.is_true(vim.tbl_contains(notices, not_committed),
                     vim.inspect({ notices = notices, requests = requests }))
      assert.is_nil(lock.stream_in_progress)
      assert.are.equal(1, #requests, vim.inspect(requests))
      stage_and_commit.stage_and_commit()
      assert.are.same({ 'summary', '', 'body' }, vim.api.nvim_buf_get_lines(
        stage_and_commit.commit_message_buffer(), 0, -1, false))
    end)
  end

  it('keeps the message until it is moot', function ()
    for _, case in ipairs({
        { result = '((commits ((public 5f135fea))) (errors ()) (warnings ()))',
          kept = false, line = 'Committed in public (5f135fe).' },
        { result = '((commits ()) (errors ()) (warnings ()))',
          kept = false, line = 'Nothing was committed.' },
        { result = '((commits ()) (errors ("skgrepo private: no git author is configured")) (warnings ()))',
          kept = true,
          line = 'Nothing was committed. Errors: skgrepo private: no git author'
            .. ' is configured Your message is kept in skg://commit-message.' },
        { result = '((errors ("A commit message is required.")) (warnings ()))',
          kept = true,
          line = 'Nothing was committed. Errors: A commit message is required.'
            .. ' Your message is kept in skg://commit-message.' } }) do
      local notices = {}
      vim.notify = function (text) table.insert(notices, text) end
      stage_and_commit.stage_and_commit()
      stage_and_commit.report_result(
        require('skg.sexpr.parse').read(case.result))
      assert.are.equal(case.line, notices[#notices])
      assert.are.equal(case.kept, stage_and_commit.commit_message_buffer() ~= nil)
      delete_commit_message_buffer()
    end
  end)

  it('binds <localleader>cc, and cancels with <localleader>ck, deleting the buffer and sending nothing', function ()
    local requests, notices = {}, {}
    vim.notify = function (text) table.insert(notices, text) end
    server = serve(requests)
    stage_and_commit.stage_and_commit()
    local buf = stage_and_commit.commit_message_buffer()
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'never sent' })
    local cancel = vim.fn.maparg('<localleader>ck', 'n', false, true)
    assert.are.equal(1, cancel.buffer)
    assert.are.equal(1, vim.fn.maparg('<localleader>cc', 'n', false, true).buffer)
    vim.api.nvim_buf_call(buf, cancel.callback)
    assert.is_nil(stage_and_commit.commit_message_buffer())
    assert.are.equal('Commit canceled; nothing was staged or committed.',
                     notices[#notices])
    vim.wait(100, function () return #requests > 0 end, 10)
    assert.are.equal(0, #requests)
  end)

  it('refuses a dirty view and an empty message', function ()
    local view = buffer.open_org_buffer_from_text(
      '* (skg node (gnid a)) a', 'skg://a', 'a-view_id')
    vim.bo[view].modified = true
    local ok, err = pcall(stage_and_commit.stage_and_commit)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('save or revert', 1, true))
    vim.bo[view].modified = false
    stage_and_commit.stage_and_commit()
    local buf = stage_and_commit.commit_message_buffer()
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '  ', '' })
    ok, err = pcall(stage_and_commit.send_from_buffer, buf)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('commit message is required', 1, true))
    assert.is_truthy(stage_and_commit.commit_message_buffer())
  end)

  it('reports errors with the commits', function ()
    assert.are.equal(
      'Nothing was committed. Errors: skgrepo private: no git author is configured',
      stage_and_commit.result_message(require('skg.sexpr.parse').read(
        '((commits ()) (errors ("skgrepo private: no git author is configured")) (warnings ()))')))
  end)
end)
