-- Client coverage for the stage-and-commit command.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local lock = require('skg.lock')
local stage_and_commit = require('skg.stage_and_commit')

local NEEDS_CONFIRMATION =
  '((response-type stage-and-commit) (needs-confirmation . t)'
  .. ' (readiness-summary (ready 1) (changed 3) (foreign 0) (order-only 0) (unshown 0))'
  .. ' (errors ()) (warnings ()))'

describe('skg stage and commit', function ()
  local server
  local original_input, original_notify

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    helpers.reset_client_state()
    lock.end_stream()
    original_input, original_notify = vim.fn.input, vim.notify
  end)

  after_each(function ()
    vim.fn.input, vim.notify = original_input, original_notify
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  ---Answer the first request with NEEDS_CONFIRMATION, the second with
  ---commits and a redrawn, now empty, view.
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
          '((response-type rerender-view) (view-id "diff-view: all changes") (content ))'))
      end
      respond(helpers.framed(
        '((response-type rerender-done) (errors ()) (warnings ()))'))
    end)
  end

  it('resends confirmed on "yes", reports the commits, and empties the view', function ()
    local requests, notices, prompts = {}, {}, {}
    local typed = { 'one message', 'yes' }
    vim.fn.input = function (prompt)
      table.insert(prompts, prompt)
      return table.remove(typed, 1) end
    vim.notify = function (text) table.insert(notices, text) end
    server = serve(requests)
    local view = buffer.open_org_buffer_from_text(
      '* (skg (node (id a))) a', 'skg://diff-view: all changes',
      'diff-view: all changes')
    stage_and_commit.stage_and_commit()
    vim.wait(3000, function ()
      return #requests == 2 and #notices > 0 and lock.stream_in_progress == nil
    end, 10)
    assert.are.equal(2, #requests, vim.inspect(requests))
    assert.is_truthy(requests[1]:find('(request . "stage and commit")', 1, true))
    assert.is_truthy(requests[1]:find('(message . "one message")', 1, true))
    assert.is_falsy(requests[1]:find('confirmed', 1, true))
    assert.is_truthy(requests[2]:find('(confirmed . t)', 1, true))
    assert.is_truthy(prompts[2]:find('1 of 3 changed files are ready', 1, true))
    assert.is_truthy(vim.tbl_contains(
      notices, 'Committed in public (5f135fe), private (bff3a57).'), vim.inspect(notices))
    assert.are.same({ '' }, vim.api.nvim_buf_get_lines(view, 0, -1, false))
    assert.is_true(vim.bo[view].modifiable)
  end)

  it('does not resend unless the word typed is exactly "yes"', function ()
    local requests = {}
    local typed = { 'one message', 'y' }
    vim.fn.input = function () return table.remove(typed, 1) end
    server = serve(requests)
    stage_and_commit.stage_and_commit()
    vim.wait(3000, function ()
      return #requests == 1 and lock.stream_in_progress == nil end, 10)
    vim.wait(100, function () return #requests > 1 end, 10)
    assert.are.equal(1, #requests, vim.inspect(requests))
  end)

  it('refuses a dirty view and an empty message', function ()
    local view = buffer.open_org_buffer_from_text(
      '* (skg (node (id a))) a', 'skg://a', 'a-view_id')
    vim.bo[view].modified = true
    local ok, err = pcall(stage_and_commit.stage_and_commit)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('save or revert', 1, true))
    vim.bo[view].modified = false
    vim.fn.input = function () return '  ' end
    ok, err = pcall(stage_and_commit.stage_and_commit)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('commit message is required', 1, true))
  end)

  it('reports errors with the commits', function ()
    assert.are.equal(
      'Nothing was committed. Errors: skgrepo private: no git author is configured',
      stage_and_commit.result_message(require('skg.sexpr.parse').read(
        '((commits ()) (errors ("skgrepo private: no git author is configured")) (warnings ()))')))
  end)
end)
