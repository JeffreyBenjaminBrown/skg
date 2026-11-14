-- Switching whose overrides a view follows. Mirrors
-- tests/elisp/test-skg-overrides-followed.el.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local client = require('skg.client')
local keymaps = require('skg.keymaps')
local length_prefix = require('skg.length_prefix')
local lock = require('skg.lock')
local overrides_followed = require('skg.overrides_followed')
local save = require('skg.save')
local state = require('skg.state')

---A clean content view named VIEW_ID, current.
local function view (view_id)
  return buffer.open_org_buffer_from_text(
    '* (skg node (gnid r)) r', 'skg://' .. view_id, view_id)
end

describe('skg.overrides_followed', function ()
  local sent, shown
  local real_send, real_notify, real_confirm

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    helpers.reset_client_state()
    sent, shown = {}, {}
    real_send, real_notify, real_confirm =
      client.send_string, vim.notify, vim.fn.confirm
    client.send_string = function (text) table.insert(sent, text) end
    vim.notify = function (message) table.insert(shown, message) end
  end)

  after_each(function ()
    client.send_string, vim.notify, vim.fn.confirm =
      real_send, real_notify, real_confirm
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
  end)

  it('refuses a buffer with unsaved edits, sending nothing', function ()
    local buf = view('r')
    vim.bo[buf].modified = true
    local ok, err = pcall(overrides_followed.switch_overrides_followed)
    assert.is_false(ok)
    assert.are.equal(
      'Save or revert this buffer first: switching whose overrides it'
      .. ' follows redraws it, which would lose its unsaved edits',
      tostring(err))
    assert.are.same({}, sent)
    assert.is_nil(lock.stream_in_progress)
  end)

  it('refuses outside a skg view', function ()
    helpers.buffer_with('* a')
    local ok, err = pcall(overrides_followed.switch_overrides_followed)
    assert.is_false(ok)
    assert.are.equal('Not in a skg view', tostring(err))
    assert.are.same({}, sent)
  end)

  it('asks for the other choice: a fresh view the user\'s, one that'
     .. " follows the user's its root authors'", function ()
    local buf = view('r')
    overrides_followed.switch_overrides_followed()
    assert.are.equal(
      '((request . "set overrides followed") (view-id . "r")'
      .. ' (overrides-followed . "user"))\n', sent[1])
    assert.is_truthy(state.response_handler_map['overrides-followed'])
    assert.are.equal('overrides followed', lock.stream_in_progress)
    assert.is_false(vim.bo[buf].modifiable) -- the stream's lock
    helpers.reset_client_state()
    vim.b[buf].skg_overrides_followed = 'user'
    overrides_followed.switch_overrides_followed()
    assert.are.equal(
      '((request . "set overrides followed") (view-id . "r")'
      .. ' (overrides-followed . "root authors"))\n', sent[2])
  end)

  it('sets the buffer and the statusline mark from the answer', function ()
    local buf = view('r')
    overrides_followed.handler(require('skg.sexpr.parse').read(
      '((response-type overrides-followed) (view-id "r")'
      .. ' (overrides-followed "user"))'))
    assert.are.equal('user', vim.b[buf].skg_overrides_followed)
    assert.are.equal(':your overrides',
                     buffer.overrides_followed_mode_line(buf))
    assert.is_truthy(shown[1]:find('<localleader>vp switches back', 1, true))
    overrides_followed.handler(require('skg.sexpr.parse').read(
      '((response-type overrides-followed) (view-id "r")'
      .. ' (overrides-followed "root authors"))'))
    assert.is_nil(vim.b[buf].skg_overrides_followed)
    assert.are.equal('', buffer.overrides_followed_mode_line(buf))
  end)

  it('leaves the choice on a refusal, and says why', function ()
    local buf = view('p')
    overrides_followed.handler(require('skg.sexpr.parse').read(
      '((response-type overrides-followed) (view-id "p")'
      .. ' (overrides-followed "root authors")'
      .. ' (content "Every tree in this buffer is rooted at a node you'
      .. ' own, so it shows your overrides already."))'))
    assert.is_nil(vim.b[buf].skg_overrides_followed)
    assert.is_truthy(shown[#shown]:find('shows your overrides already',
                                        1, true))
  end)

  it('dispatches the answer and the stream: the choice is set, the'
     .. ' view redrawn and unlocked', function ()
    local buf = view('r')
    overrides_followed.switch_overrides_followed()
    length_prefix.dispatch_by_type(
      '((response-type overrides-followed) (view-id "r")'
      .. ' (overrides-followed "user"))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-lock) (lock-views (r)))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-view) (view-id r)'
      .. ' (content "* (skg node (gnid r)) r redrawn"))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-done) (errors ()) (warnings ()))')
    assert.are.equal('user', vim.b[buf].skg_overrides_followed)
    assert.are.equal('* (skg node (gnid r)) r redrawn',
      vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1])
    assert.is_nil(lock.stream_in_progress)
    assert.is_true(vim.bo[buf].modifiable)
    assert.are.equal(0, state.lp_pending_count)
  end)

  it('retries with the approved pids after a privacy challenge', function ()
    local buf = view('r')
    vim.fn.confirm = function () return 1 end
    overrides_followed.switch_overrides_followed()
    length_prefix.dispatch_by_type(
      '((response-type overPrivateText-telescope-confirmation)'
      .. ' (operation overrides-followed-switch) (pids (p1 p2))'
      .. ' (prompt "Include?"))')
    assert.is_nil(state.response_handler_map['overrides-followed'])
    length_prefix.dispatch_by_type(
      '((response-type rerender-lock) (lock-views ()))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-done) (errors ()) (warnings ()))')
    vim.wait(2000, function () return #sent == 2 end, 10)
    assert.are.equal(
      '((request . "set overrides followed") (view-id . "r")'
      .. ' (overrides-followed . "user")'
      .. ' (approved-overPrivateText-pids "p1" "p2"))\n', sent[2])
    assert.is_nil(vim.b[buf].skg_overrides_followed)
  end)

  it('rides in the save request, survives a filetype change, and a'
     .. ' fresh view starts at the default', function ()
    local buf = view('r')
    local position = { lines_below_focused_headline = 0, column = 0,
                       screen_lines_below_window_start = 0 }
    assert.is_falsy(save.save_request_sexp(
      'r', position, nil, nil, nil, nil, nil, nil,
      vim.b[buf].skg_overrides_followed):find('overrides-followed', 1, true))
    vim.b[buf].skg_overrides_followed = 'user'
    vim.bo[buf].filetype = 'text'
    vim.bo[buf].filetype = 'org'
    assert.are.equal('user', vim.b[buf].skg_overrides_followed)
    assert.is_truthy(save.save_request_sexp(
      'r', position, nil, nil, nil, nil, nil, nil,
      vim.b[buf].skg_overrides_followed):find(
        '(view-id . "r") (overrides-followed . "user")', 1, true))
    -- The save itself reads the buffer's choice: its request line is
    -- the first thing it sends (the envelope follows).
    save.request_save_buffer()
    assert.is_truthy(sent[1]:find('save buffer', 1, true))
    assert.is_truthy(sent[1]:find(
      '(overrides-followed . "user")', 1, true))
    helpers.reset_client_state()
    -- Reopening a view in the same buffer is a fresh view.
    local again = view('r')
    assert.are.equal(buf, again)
    assert.is_nil(vim.b[again].skg_overrides_followed)
  end)

  it('is on <localleader>vp and :SkgSwitchOverridesFollowed', function ()
    local found = nil
    for _, binding in ipairs(keymaps.content_view_bindings) do
      if binding[1] == 'vp' then found = binding[2] end end
    assert.are.equal('SwitchOverridesFollowed', found)
    local spec = keymaps.commands.SwitchOverridesFollowed
    assert.are.equal('skg.overrides_followed', spec.module)
    assert.are.equal('switch_overrides_followed', spec.fn)
    keymaps.define_ex_commands()
    assert.are.equal(2, vim.fn.exists(':SkgSwitchOverridesFollowed'))
    local buf = view('r')
    local key = vim.api.nvim_replace_termcodes(
      vim.g.maplocalleader or '\\', true, false, true)
    assert.is_truthy(vim.fn.maparg('<localleader>vp', 'n', false, true).desc)
    vim.bo[buf].modified = true
    vim.api.nvim_feedkeys(key .. 'vp', 'x', false)
    assert.is_truthy(shown[#shown]:find('Save or revert this buffer first',
                                        1, true))
  end)
end)
