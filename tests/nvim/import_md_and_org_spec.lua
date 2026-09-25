-- Client flow for additive mixed Markdown/Org import.
local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')
local importer = require('skg.import_md_and_org')
local buffer = require('skg.buffer')
local state = require('skg.state')

describe('skg mixed Markdown/Org import', function ()
  local server
  before_each(function ()
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
  end)
  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
  end)

  it('prompts for host root only when requested, confirms a token, and reports', function ()
    local requests = {}
    local original_input, original_confirm = vim.fn.input, vim.fn.confirm
    vim.fn.input = function () return '/host/notes' end
    vim.fn.confirm = function () return 1 end
    server = helpers.connect_to_fake_server(function (line, respond)
      table.insert(requests, line)
      if #requests == 1 then
        respond(helpers.framed(
          '((response-type import-md-and-org-host-mapping-needed)'
          .. ' (content "Host mapping needed"))'))
      elseif #requests == 2 then
        respond(helpers.framed(
          '((response-type import-md-and-org-preview)'
          .. ' (content "Valid preview") (approval-token "opaque"))'))
      elseif #requests == 3 then
        respond(helpers.framed(
          '((response-type import-md-and-org-result)'
          .. ' (content "Imported") (record-id "record"))'))
      else
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ()) (warnings ()))'))
      end
    end)
    helpers.install_fixture_herald_rules()
    local dirty = buffer.open_org_buffer_from_text(
      '* (skg (node (id dirty))) Unsaved\nlocal edit',
      'skg://dirty', 'dirty-uri')
    vim.bo[dirty].modified = true
    importer.import_md_and_org('/container/notes', 'private')
    local complete = vim.wait(3000, function () return #requests >= 4 end, 10)
    vim.fn.input, vim.fn.confirm = original_input, original_confirm
    assert.is_true(complete, vim.inspect(requests))
    assert.is_truthy(requests[1]:find('input-directory', 1, true))
    assert.is_truthy(requests[2]:find('/host/notes', 1, true))
    assert.is_truthy(requests[3]:find('opaque', 1, true))
    assert.is_truthy(requests[4]:find('dirty-uri', 1, true))
    assert.is_true(vim.bo[dirty].modified)
    assert.is_nil(state.response_handler_map['import-md-and-org-preview'])
  end)

  it('declining a valid preview sends cancel and leaves no handlers', function ()
    local requests = {}
    local original_confirm = vim.fn.confirm
    vim.fn.confirm = function () return 2 end
    server = helpers.connect_to_fake_server(function (line, respond)
      table.insert(requests, line)
      if #requests == 1 then
        respond(helpers.framed(
          '((response-type import-md-and-org-preview)'
          .. ' (content "Valid preview") (approval-token "opaque"))'))
      else
        respond(helpers.framed(
          '((response-type import-md-and-org-result) (content "Cancelled"))'))
      end
    end)
    importer.import_md_and_org('/container/notes', 'private')
    local complete = vim.wait(3000, function () return #requests == 2 end, 10)
    vim.fn.confirm = original_confirm
    assert.is_true(complete, vim.inspect(requests))
    assert.is_truthy(requests[2]:find('cancel', 1, true))
    vim.wait(100, function ()
      return state.response_handler_map['import-md-and-org-result'] == nil end, 10)
    assert.is_nil(state.response_handler_map['import-md-and-org-result'])
  end)
end)
