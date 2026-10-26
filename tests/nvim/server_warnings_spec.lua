-- Coverage for skg.server_warnings: each view's buffer corrections,
-- and the startup warnings, appended to the server-warnings buffer,
-- after whatever it held. Mirrors the Emacs client's
-- skg-show-corrections-in-server-warnings and
-- skg--server-warnings-handler.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
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

describe('skg.server_warnings buffer corrections', function ()
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

  it('appends one level-1 headline per view and one level-2 headline'
     .. ' per message, after what the buffer held', function ()
    buffer.open_org_buffer_from_text(
      '* (skg node (gnid a)) a', 'skg://shown-view', 'content-view:5')
    messages.scratch_org_buffer(server_warnings.buffer_name,
                                '* an earlier startup warning')
    server_warnings.show_corrections(sexpr.read(
      '((content-view:5'
      .. ' "Drew what the graph says belongs here.\\n*** new child'
      .. '\\n- viewnode ID: 0d. To find it: C-c g v 0d\\n"'
      .. ' "Dropped a duplicate.\\n*** twin\\n- viewnode ID: 0e.\\n")'
      .. ' ("search:two words"'
      .. ' "Drew what the graph says belongs here.\\n*** hit\\n"))'))
    local buf = warnings_buffer()
    assert.is_truthy(buf)
    assert.are.same({
      '* an earlier startup warning',
      '* Buffer corrections in skg://shown-view',
      '** Drew what the graph says belongs here.',
      '*** new child',
      '- viewnode ID: 0d. To find it: <localleader>gv 0d',
      '** Dropped a duplicate.',
      '*** twin',
      '- viewnode ID: 0e.',
      '* Buffer corrections in search:two words',
      '** Drew what the graph says belongs here.',
      '*** hit',
    }, vim.api.nvim_buf_get_lines(buf, 0, -1, false))
    assert.are.same({
      'skg: the server corrected 3 headlines in 2 views - see '
      .. server_warnings.buffer_name }, notified)
  end)

  it('a second batch goes after the first', function ()
    server_warnings.show_corrections(sexpr.read(
      '((content-view:1 "First.\\n*** one\\n"))'))
    server_warnings.show_corrections(sexpr.read(
      '((content-view:2 "Second.\\n*** two\\n"))'))
    assert.are.same({
      '* Buffer corrections in content-view:1', '** First.', '*** one',
      '* Buffer corrections in content-view:2', '** Second.', '*** two',
    }, vim.api.nvim_buf_get_lines(warnings_buffer(), 0, -1, false))
    assert.are.equal(
      'skg: the server corrected 1 headline in 1 view - see '
      .. server_warnings.buffer_name, notified[2])
  end)

  it('startup warnings go after the buffer corrections, under their'
     .. ' own headline', function ()
    server_warnings.show_corrections(sexpr.read(
      '((content-view:1 "First.\\n*** one\\n"))'))
    server_warnings.show_server_warnings(sexpr.read(
      '((response-type server-warnings)'
      .. ' (warnings ("first warning" "second warning")))'))
    assert.are.same({
      '* Buffer corrections in content-view:1', '** First.', '*** one',
      '* Startup warnings', '** first warning', '** second warning',
    }, vim.api.nvim_buf_get_lines(warnings_buffer(), 0, -1, false))
    assert.are.equal(
      'The skg server reported 2 startup warnings - see '
      .. server_warnings.buffer_name, notified[2])
  end)

  it('an absent or empty (corrections ...) list does nothing', function ()
    server_warnings.show_corrections(nil)
    server_warnings.show_corrections(sexpr.read('()'))
    server_warnings.show_corrections(sexpr.read('nil'))
    assert.is_nil(warnings_buffer())
    assert.are.same({}, notified)
  end)
end)
