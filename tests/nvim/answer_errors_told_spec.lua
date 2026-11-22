-- Mirrors tests/elisp/test-skg-answer-errors-told.el: each handler
-- that catches an error while applying the server's answer logs it,
-- but the log is off by default, so it must also notify the user
-- (length_prefix.tell_answer_not_applied); otherwise a save whose
-- answer could not be applied looks like a save that did nothing.
-- Each case makes the answer unreadable by making the payload
-- accessors throw, which every handler calls first inside its pcall.

local diff_view = require('skg.diff_view')
local length_prefix = require('skg.length_prefix')
local lock = require('skg.lock')
local payload = require('skg.payload')
local rerender_all_views = require('skg.rerender_all_views')
local save = require('skg.save')
local save_confirmations = require('skg.save.confirmations')
local redraw_whole_view = require('skg.redraw_whole_view')
local state = require('skg.state')

local CONFIRMATION_FAILED =
  'the save stopped for a confirmation, which failed, so it may not have saved'

local function failing_accessor ()
  error('skg-gone-after-reload') end

---Run FN with the payload accessors throwing and the save stream's
---cleanup stubbed out; return the notifications it showed.
local function told_by (fn)
  local shown = {}
  local saved = {
    notify = vim.notify,
    field = payload.field,
    field_text = payload.field_text,
    end_stream = lock.end_stream,
    unlock_all_save_locked = lock.unlock_all_save_locked }
  vim.notify = function (message) table.insert(shown, message) end
  payload.field = failing_accessor
  payload.field_text = failing_accessor
  lock.end_stream = function () end
  lock.unlock_all_save_locked = function () end
  local ok, err = pcall(fn)
  vim.notify = saved.notify
  payload.field = saved.field
  payload.field_text = saved.field_text
  lock.end_stream = saved.end_stream
  lock.unlock_all_save_locked = saved.unlock_all_save_locked
  assert(ok, err)
  return shown
end

local function assert_told (shown, answer, consequence)
  local wanted_start = 'skg: ' .. consequence .. ': '
  local wanted_answer =
    "applying the server's " .. answer .. ' answer failed ('
  for _, message in ipairs(shown) do
    if message:sub(1, #wanted_start) == wanted_start
       and message:find(wanted_answer, 1, true)
       and message:find('skg-gone-after-reload', 1, true) then
      return end
  end
  error('no notification for ' .. answer .. ' among: '
        .. vim.inspect(shown))
end

describe('a handler that fails on the server answer says so', function ()
  before_each(function ()
    state.response_handler_map = {}
    state.lp_pending_count = 0
  end)

  it('save-result', function ()
    local buf = vim.api.nvim_create_buf(false, true)
    assert_told(told_by(function ()
                  save.save_result_handler(buf, {}) end),
                'save-result',
                'the server may have saved, but the buffer was not updated')
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  it('save-lock', function ()
    assert_told(told_by(function () save.broad_save_lock_handler({}) end),
                'save-lock', 'views stay locked until the save ends')
  end)

  it('save-relax-lock', function ()
    assert_told(told_by(function ()
                  save.save_relax_lock_handler('view-id-saved', {}) end),
                'save-relax-lock', 'views stay locked until the save ends')
  end)

  it('collateral-view and rerender-view', function ()
    assert_told(told_by(function ()
                  redraw_whole_view.apply_streamed_view_update(
                    '', {}, 'save', 'collateral-view', true) end),
                'collateral-view',
                'the server may have saved, but a view was not redrawn')
    assert_told(told_by(function ()
                  redraw_whole_view.apply_streamed_view_update(
                    '', {}, 'rerender', 'rerender-view') end),
                'rerender-view', 'a view was not redrawn')
  end)

  it('fork-confirmation', function ()
    assert_told(told_by(function ()
                  save_confirmations.fork_confirmation_handler(0, {}) end),
                'fork-confirmation',
                'nothing was saved, and the fork confirmation was not shown')
  end)

  it('telescope-hoist-confirmation', function ()
    assert_told(told_by(function ()
                  save_confirmations.telescope_hoist_confirmation_handler(0, {}) end),
                'telescope-hoist-confirmation', CONFIRMATION_FAILED)
  end)

  it('delete-from-confirmation', function ()
    assert_told(told_by(function ()
                  save_confirmations.delete_from_confirmation_handler(0, {}) end),
                'delete-from-confirmation', CONFIRMATION_FAILED)
  end)

  it('strandeds-confirmation', function ()
    assert_told(told_by(function ()
                  save_confirmations.strandeds_confirmation_handler(0, {}) end),
                'strandeds-confirmation', CONFIRMATION_FAILED)
  end)

  it('rerender-lock', function ()
    assert_told(told_by(function ()
                  rerender_all_views.register_rerender_stream_handlers()
                  state.response_handler_map['rerender-lock'].handler('', {})
                end),
                'rerender-lock',
                'every view was unlocked before the rerender redrew it')
  end)

  it('diff-view', function ()
    local outcome
    assert_told(told_by(function ()
                  outcome = diff_view.handle_diffView_response(
                    {}, 'all changes', 'diff-view-id') end),
                'diff-view', 'the diff view was not shown')
    assert.are.equal('failed', outcome)
  end)

  it('the dispatcher, for an error a handler lets out', function ()
    state.register_response_handler('save-result',
      function () error('skg-gone-after-reload') end, true)
    local shown = {}
    local real_notify = vim.notify
    vim.notify = function (message) table.insert(shown, message) end
    length_prefix.dispatch_by_type('((response-type save-result))')
    vim.notify = real_notify
    assert_told(shown, 'save-result',
                'the server may have acted (saved, say), but the client'
                  .. ' did not finish handling its answer')
  end)
end)
