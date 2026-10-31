-- Coverage for skg.server_warnings: the startup warnings, appended to
-- the server-warnings buffer, after whatever it held. Mirrors the
-- Emacs client's skg--server-warnings-handler.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local messages = require('skg.messages')
local server_warnings = require('skg.server_warnings')
local sexpr = require('skg.sexpr.parse')

local function warnings_buffer ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == server_warnings.buffer_name then
      return buf end
  end
  return nil
end

local function wipe_warnings_buffer ()
  local buf = warnings_buffer()
  if buf then vim.api.nvim_buf_delete(buf, { force = true }) end
end

describe('skg.server_warnings startup warnings', function ()
  local real_notify
  local notified

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    wipe_warnings_buffer()
    notified = {}
    real_notify = vim.notify
    vim.notify = function (message) table.insert(notified, message) end
  end)

  after_each(function ()
    vim.notify = real_notify
    helpers.wipe_skg_buffers()
    wipe_warnings_buffer()
  end)

  it('startup warnings go after what the buffer held, under their'
     .. ' own headline', function ()
    messages.scratch_org_buffer(server_warnings.buffer_name,
                                '* Startup warnings\n** an earlier warning')
    server_warnings.server_warnings_handler(sexpr.read(
      '((response-type server-warnings)'
      .. ' (warnings ("first warning" "second warning")))'))
    assert.are.same({
      '* Startup warnings', '** an earlier warning',
      '* Startup warnings', '** first warning', '** second warning',
    }, vim.api.nvim_buf_get_lines(warnings_buffer(), 0, -1, false))
    assert.are.same({
      'The skg server reported 2 startup warnings - see '
      .. server_warnings.buffer_name }, notified)
  end)

  it('an empty (warnings ...) list opens nothing', function ()
    server_warnings.server_warnings_handler(sexpr.read(
      '((response-type server-warnings) (warnings ()))'))
    assert.is_nil(warnings_buffer())
    assert.are.same({}, notified)
  end)
end)
