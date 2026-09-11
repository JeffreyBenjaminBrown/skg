-- Client coverage for the explicit cleanup of an Unknown's raw ID.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local cleanup = require('skg.delete_absent_references')
local lock = require('skg.lock')

describe('skg absent-reference cleanup', function ()
  local server

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    helpers.reset_client_state()
    lock.end_stream()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  it('requires a clean Unknown, confirms, retries, and reports success', function ()
    local requests, notices = {}, {}
    local original_confirm, original_notify = vim.fn.confirm, vim.notify
    vim.fn.confirm = function () return 1 end
    vim.notify = function (text) table.insert(notices, text) end
    server = helpers.connect_to_fake_server(function (line, respond)
      table.insert(requests, line)
      if #requests == 1 then
        respond(helpers.framed(
          '((response-type delete-references-confirmation)'
          .. ' (approved-preview "opaque-token")'
          .. ' (content "* References to absent ID gone"))'))
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ()) (warnings ()))'))
      else
        respond(helpers.framed(
          '((response-type delete-references-result)'
          .. ' (content "* Absent-reference cleanup complete"))'))
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ()) (warnings ()))'))
      end
    end)
    local unknown = buffer.open_org_buffer_from_text(
      '* (skg (node (id owner))) owner\n** (skg (unknown (id gone)))',
      'skg://owner', 'cleanup-uri')
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    cleanup.request()
    vim.wait(3000, function ()
      return #requests == 2 and #notices > 0 and lock.stream_in_progress == nil
    end, 10)
    vim.fn.confirm, vim.notify = original_confirm, original_notify
    assert.are.equal(2, #requests, vim.inspect(requests))
    assert.is_truthy(requests[1]:find('delete references to absent node', 1, true))
    assert.is_truthy(requests[1]:find('(id . "gone")', 1, true))
    assert.is_truthy(requests[2]:find('(approved%-preview . "opaque%-token")'))
    assert.is_truthy(table.concat(notices, '\n')
      :find('Absent%-reference cleanup complete'))
    assert.is_true(vim.bo[unknown].modifiable)
  end)

  it('refuses a non-Unknown point and every dirty view', function ()
    local normal = buffer.open_org_buffer_from_text(
      '* (skg (node (id owner))) owner', 'skg://owner', 'normal-uri')
    local ok, err = pcall(cleanup.request)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('Unknown headline', 1, true))
    vim.api.nvim_buf_set_lines(normal, 0, -1, false, {
      '* (skg (unknown (id gone)))' })
    vim.bo[normal].modified = true
    ok, err = pcall(cleanup.request)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('save or revert', 1, true))
  end)
end)
