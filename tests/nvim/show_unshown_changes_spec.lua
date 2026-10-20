-- Client coverage for the show-unshown-changes command.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local lock = require('skg.lock')
local show_unshown_changes = require('skg.show_unshown_changes')

describe('skg show unshown changes', function ()
  local server
  local original_notify

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    helpers.reset_client_state()
    lock.end_stream()
    original_notify = vim.notify
  end)

  after_each(function ()
    vim.notify = original_notify
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  it('refuses a modified buffer', function ()
    local requests = {}
    server = helpers.connect_to_fake_server(function (line)
      table.insert(requests, line) end)
    local view = buffer.open_org_buffer_from_text(
      '* (skg (node (id a))) a', 'skg://diff-view: all changes',
      'diff-view: all changes')
    vim.api.nvim_set_current_buf(view)
    vim.bo[view].modified = true
    local ok, err = pcall(show_unshown_changes.show_unshown_changes)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('Save the buffer first.', 1, true))
    vim.wait(100, function () return #requests > 0 end, 10)
    assert.are.equal(0, #requests)
  end)

  it('sends the request on a clean buffer and redraws it', function ()
    local requests, notices = {}, {}
    vim.notify = function (text) table.insert(notices, text) end
    server = helpers.connect_to_fake_server(function (line, respond)
      table.insert(requests, line)
      respond(helpers.framed(
        '((response-type show-unshown-changes)'
        .. ' (readiness-summary (ready 1) (changed 3) (foreign 0) (order-only 0) (unshown 0))'
        .. ' (errors ()) (warnings ()))'))
      respond(helpers.framed(
        '((response-type rerender-lock) (lock-views ("diff-view: all changes")))'))
      respond(helpers.framed(
        '((response-type rerender-view) (view-id "diff-view: all changes")'
        .. ' (content "* (skg (node (id b))) b"))'))
      respond(helpers.framed(
        '((response-type rerender-done) (errors ()) (warnings ()))'))
    end)
    local view = buffer.open_org_buffer_from_text(
      '* (skg (node (id a))) a', 'skg://diff-view: all changes',
      'diff-view: all changes')
    vim.api.nvim_set_current_buf(view)
    show_unshown_changes.show_unshown_changes()
    vim.wait(3000, function ()
      return #notices > 0 and lock.stream_in_progress == nil end, 10)
    assert.are.equal(1, #requests, vim.inspect(requests))
    assert.is_truthy(requests[1]:find(
      '(request . "show unshown changes")', 1, true))
    assert.is_truthy(requests[1]:find(
      '(view-id . "diff-view: all changes")', 1, true))
    assert.is_truthy(vim.tbl_contains(
      notices, '1 of 3 changed files are ready'), vim.inspect(notices))
    assert.are.same({ '* (skg (node (id b))) b' },
                    vim.api.nvim_buf_get_lines(view, 0, -1, false))
  end)
end)
