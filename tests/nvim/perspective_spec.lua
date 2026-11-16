-- Cycling a view's perspective. Mirrors
-- tests/elisp/test-skg-perspective.el.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local client = require('skg.client')
local keymaps = require('skg.keymaps')
local length_prefix = require('skg.length_prefix')
local lock = require('skg.lock')
local perspective = require('skg.perspective')
local save = require('skg.save')
local state = require('skg.state')

---A clean content view named VIEW_ID, current.
local function view (view_id)
  return buffer.open_org_buffer_from_text(
    '* (skg node (gnid r)) r', 'skg://' .. view_id, view_id)
end

describe('skg.perspective', function ()
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
    local ok, err = pcall(perspective.cycle_perspective)
    assert.is_false(ok)
    assert.are.equal(
      'Save or revert this buffer first: changing its perspective'
      .. ' redraws it, which would lose its unsaved edits',
      tostring(err))
    assert.are.same({}, sent)
    assert.is_nil(lock.stream_in_progress)
  end)

  it('refuses outside a skg view', function ()
    helpers.buffer_with('* a')
    local ok, err = pcall(perspective.cycle_perspective)
    assert.is_false(ok)
    assert.are.equal('Not in a skg view', tostring(err))
    assert.are.same({}, sent)
  end)

  it('asks for the next perspective, whatever the buffer shows: the'
     .. ' server computes the cycle', function ()
    local buf = view('r')
    perspective.cycle_perspective()
    assert.are.equal(
      '((request . "set perspective") (view-id . "r")'
      .. ' (perspective . "next"))\n', sent[1])
    assert.is_truthy(state.response_handler_map['perspective'])
    assert.are.equal('cycle perspective', lock.stream_in_progress)
    assert.is_false(vim.bo[buf].modifiable) -- the stream's lock
    helpers.reset_client_state()
    buffer.set_perspective(buf, 'skgrepo tracy', 'the perspective of tracy')
    perspective.cycle_perspective()
    assert.are.equal(
      '((request . "set perspective") (view-id . "r")'
      .. ' (perspective . "next"))\n', sent[2])
  end)

  it('sets the buffer and the statusline mark from the answer, and'
     .. ' names the perspective', function ()
    local buf = view('r')
    perspective.handler(require('skg.sexpr.parse').read(
      '((response-type perspective) (view-id "r")'
      .. ' (perspective "skgrepo main")'
      .. ' (perspective-name "the perspective of main"))'))
    assert.are.equal('skgrepo main', vim.b[buf].skg_perspective)
    assert.are.equal(':the perspective of main',
                     buffer.perspective_mode_line(buf))
    assert.are.equal('Showing the perspective of main.', shown[1])
    perspective.handler(require('skg.sexpr.parse').read(
      '((response-type perspective) (view-id "r")'
      .. ' (perspective "skgrepo tracy")'
      .. ' (perspective-name "the perspective of tracy"))'))
    assert.are.equal('skgrepo tracy', vim.b[buf].skg_perspective)
    assert.are.equal(':the perspective of tracy',
                     buffer.perspective_mode_line(buf))
    assert.are.equal('Showing the perspective of tracy.', shown[2])
    perspective.handler(require('skg.sexpr.parse').read(
      '((response-type perspective) (view-id "r")'
      .. ' (perspective "root homes")'
      .. ' (perspective-name "the perspective of each root\'s home skgrepo (tracy)"))'))
    assert.is_nil(vim.b[buf].skg_perspective)
    assert.is_nil(vim.b[buf].skg_perspective_name)
    assert.are.equal('', buffer.perspective_mode_line(buf))
    assert.are.equal("Showing the perspective of each root's home skgrepo (tracy).",
                     shown[3])
  end)

  it('leaves the choice on a refusal, and says why', function ()
    local buf = view('p')
    perspective.handler(require('skg.sexpr.parse').read(
      '((response-type perspective) (view-id "p")'
      .. ' (perspective "root homes")'
      .. ' (perspective-name "the perspective of each root\'s home skgrepo (main)")'
      .. ' (content "No other perspective would show this buffer'
      .. ' differently from the perspective of each root\'s home skgrepo."))'))
    assert.is_nil(vim.b[buf].skg_perspective)
    assert.is_truthy(shown[#shown]:find("No other perspective", 1, true))
  end)

  it('dispatches the answer and the stream: the choice is set, the'
     .. ' view redrawn and unlocked', function ()
    local buf = view('r')
    perspective.cycle_perspective()
    length_prefix.dispatch_by_type(
      '((response-type perspective) (view-id "r")'
      .. ' (perspective "skgrepo main")'
      .. ' (perspective-name "the perspective of main"))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-lock) (lock-views (r)))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-view) (view-id r)'
      .. ' (content "* (skg node (gnid r)) r redrawn"))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-done) (errors ()) (warnings ()))')
    assert.are.equal('skgrepo main', vim.b[buf].skg_perspective)
    assert.are.equal('* (skg node (gnid r)) r redrawn',
      vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1])
    assert.is_nil(lock.stream_in_progress)
    assert.is_true(vim.bo[buf].modifiable)
    assert.are.equal(0, state.lp_pending_count)
  end)

  it('retries with the approved pids after a privacy challenge', function ()
    local buf = view('r')
    vim.fn.confirm = function () return 1 end
    perspective.cycle_perspective()
    length_prefix.dispatch_by_type(
      '((response-type overPrivateText-telescope-confirmation)'
      .. ' (operation perspective-switch) (pids (p1 p2))'
      .. ' (prompt "Include?"))')
    assert.is_nil(state.response_handler_map['perspective'])
    length_prefix.dispatch_by_type(
      '((response-type rerender-lock) (lock-views ()))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-done) (errors ()) (warnings ()))')
    vim.wait(2000, function () return #sent == 2 end, 10)
    assert.are.equal(
      '((request . "set perspective") (view-id . "r")'
      .. ' (perspective . "next")'
      .. ' (approved-overPrivateText-pids "p1" "p2"))\n', sent[2])
    assert.is_nil(vim.b[buf].skg_perspective)
  end)

  it('rides in the save request, survives a filetype change, and a'
     .. ' fresh view starts at the default', function ()
    local buf = view('r')
    local position = { lines_below_focused_headline = 0, column = 0,
                       screen_lines_below_window_start = 0 }
    assert.is_falsy(save.save_request_sexp(
      'r', position, nil, nil, nil, nil, nil, nil,
      vim.b[buf].skg_perspective):find('perspective', 1, true))
    buffer.set_perspective(buf, 'skgrepo tracy', 'the perspective of tracy')
    vim.bo[buf].filetype = 'text'
    vim.bo[buf].filetype = 'org'
    assert.are.equal('skgrepo tracy', vim.b[buf].skg_perspective)
    assert.is_truthy(save.save_request_sexp(
      'r', position, nil, nil, nil, nil, nil, nil,
      vim.b[buf].skg_perspective):find(
        '(view-id . "r") (perspective . "skgrepo tracy")', 1, true))
    -- The save itself reads the buffer's choice: its request line is
    -- the first thing it sends (the envelope follows).
    save.request_save_buffer()
    assert.is_truthy(sent[1]:find('save buffer', 1, true))
    assert.is_truthy(sent[1]:find(
      '(perspective . "skgrepo tracy")', 1, true))
    helpers.reset_client_state()
    -- Reopening a view in the same buffer is a fresh view.
    local again = view('r')
    assert.are.equal(buf, again)
    assert.is_nil(vim.b[again].skg_perspective)
    assert.is_nil(vim.b[again].skg_perspective_name)
  end)

  it('is on <localleader>vo and :SkgCyclePerspective, and vp is free',
     function ()
    local found, vp = nil, nil
    for _, binding in ipairs(keymaps.content_view_bindings) do
      if binding[1] == 'vo' then found = binding[2] end
      if binding[1] == 'vp' then vp = binding[2] end end
    assert.are.equal('CyclePerspective', found)
    assert.is_nil(vp)
    local spec = keymaps.commands.CyclePerspective
    assert.are.equal('skg.perspective', spec.module)
    assert.are.equal('cycle_perspective', spec.fn)
    keymaps.define_ex_commands()
    assert.are.equal(2, vim.fn.exists(':SkgCyclePerspective'))
    local buf = view('r')
    local key = vim.api.nvim_replace_termcodes(
      vim.g.maplocalleader or '\\', true, false, true)
    assert.is_truthy(vim.fn.maparg('<localleader>vo', 'n', false, true).desc)
    vim.bo[buf].modified = true
    vim.api.nvim_feedkeys(key .. 'vo', 'x', false)
    assert.is_truthy(shown[#shown]:find('Save or revert this buffer first',
                                        1, true))
  end)
end)
