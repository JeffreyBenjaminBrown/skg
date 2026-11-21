-- Read receipts. Mirrors tests/elisp/test-skg-receipt.el.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local client = require('skg.client')
local keymaps = require('skg.keymaps')
local length_prefix = require('skg.length_prefix')
local receipt = require('skg.receipt')
local rerender = require('skg.rerender_all_views')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

---A clean view of a mailbox, current, the cursor in an item's body.
local function mailbox_view ()
  local buf = buffer.open_org_buffer_from_text(
    '* (skg node (gnid t) (skgRepos (nodeRepo sender))) to reader\n'
    .. '** (skg node (gnid m1) (skgRepos (nodeRepo sender))) message one\n'
    .. 'Its body.',
    'skg://mailbox', 'mailbox')
  vim.api.nvim_win_set_cursor(0, { 3, 0 })
  return buf
end

describe('skg.receipt', function ()
  local sent, shown
  local real_send, real_notify, real_input, real_select

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    helpers.reset_client_state()
    sent, shown = {}, {}
    real_send, real_notify, real_input, real_select =
      client.send_string, vim.notify, vim.fn.input, vim.ui.select
    client.send_string = function (text) table.insert(sent, text) end
    vim.notify = function (message) table.insert(shown, message) end
    rerender.after_empty_stream = nil
  end)

  after_each(function ()
    client.send_string, vim.notify, vim.fn.input, vim.ui.select =
      real_send, real_notify, real_input, real_select
    rerender.after_empty_stream = nil
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
  end)

  it('refuses a buffer with unsaved edits, sending nothing', function ()
    local buf = mailbox_view()
    vim.bo[buf].modified = true
    local ok, err = pcall(receipt.receipt)
    assert.is_false(ok)
    assert.is_truthy(tostring(err):find('unsaved edits', 1, true))
    assert.are.same({}, sent)
  end)

  it('sends the item of the headline above the cursor, and "Read." by'
     .. ' default', function ()
    mailbox_view()
    vim.fn.input = function () return '' end
    receipt.receipt()
    assert.are.equal(
      '((request . "receipt") (item . "m1") (text . "Read.")'
      .. ' (exclude-view-ids))\n', sent[1])
    assert.is_truthy(state.response_handler_map['receipt'])
  end)

  it('shows what it wrote', function ()
    receipt.handler('m1', 'Read.', nil, nil, sexpr.read(
      '((response-type receipt) (preview false) (lines ("Forks \'message'
      .. ' one\'." "Hides \'message one\' from \'f\'.")) (receipt-id r)'
      .. ' (holder-id h))'))
    assert.are.equal("Forks 'message one'.\nHides 'message one' from 'f'.",
                     shown[1])
  end)

  it('asks for a skgrepo after the empty stream, and sends again with'
     .. ' it', function ()
    receipt.handler('mp', 'ok', nil, nil, sexpr.read(
      '((response-type receipt) (refusal "Name the skgrepo.")'
      .. ' (choose-skgrepo (reader-public reader-private)))'))
    assert.are.same({}, sent)
    vim.ui.select = function (items, _opts, on_choice)
      assert.are.same({ 'reader-public', 'reader-private' }, items)
      on_choice('reader-private', 2) end
    rerender.after_empty_stream()
    assert.is_truthy(sent[1]:find(
      '(item . "mp") (text . "ok") (fork-into . "reader-private")', 1, true))
  end)

  it('asks for a mailbox after the empty stream, and sends again with'
     .. ' it', function ()
    receipt.handler('m3', 'ok', nil, nil, sexpr.read(
      '((response-type receipt) (refusal "Name the mailbox.")'
      .. ' (choose-mailbox (t u))'
      .. ' (choose-mailbox-titles ("to reader" "also to reader")))'))
    vim.ui.select = function (items, _opts, on_choice)
      assert.are.same({ 'to reader (t)', 'also to reader (u)' }, items)
      on_choice(items[2], 2) end
    rerender.after_empty_stream()
    assert.is_truthy(sent[1]:find(
      '(item . "m3") (text . "ok") (mailbox . "u")', 1, true))
  end)

  it('shows a refusal without choices, and asks nothing', function ()
    receipt.handler('x', 'ok', nil, nil, sexpr.read(
      '((response-type receipt) (refusal "No note has the ID x."))'))
    assert.are.equal('skg receipt: No note has the ID x.', shown[1])
    assert.is_nil(rerender.after_empty_stream)
  end)

  it('dispatches the answer and the stream, unlocking the view', function ()
    local buf = mailbox_view()
    vim.fn.input = function () return 'Done.' end
    receipt.receipt()
    length_prefix.dispatch_by_type(
      '((response-type receipt) (preview false) (lines ("Hides it."))'
      .. ' (receipt-id r) (holder-id h))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-lock) (lock-views (mailbox)))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-view) (view-id mailbox)'
      .. ' (content "* (skg node (gnid t)) to reader"))')
    length_prefix.dispatch_by_type(
      '((response-type rerender-done) (errors ()) (warnings ()))')
    assert.are.equal('Hides it.', shown[1])
    assert.is_true(vim.bo[buf].modifiable)
  end)

  it('is on <localleader>ma, the twin of C-c m a', function ()
    local found = false
    for _, binding in ipairs(keymaps.content_view_bindings) do
      if binding[1] == 'ma' and binding[2] == 'Receipt' then found = true end end
    assert.is_true(found)
    assert.are.equal('receipt', keymaps.commands.Receipt.fn)
  end)
end)
