-- Coverage for skg.repo_files_integrated: the message the server sends
-- unprompted when it integrated files changed on disk. Mirrors
-- tests/elisp/test-skg-repo-files-integrated.el.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local length_prefix = require('skg.length_prefix')
local lock = require('skg.lock')
local repo_files_integrated = require('skg.repo_files_integrated')
local rerender = require('skg.rerender_all_views')
local server_warnings = require('skg.server_warnings')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')

local function wipe_warnings_buffer ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == server_warnings.buffer_name then
      pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
  end
end

local function warnings_text ()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == server_warnings.buffer_name then
      return table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
    end
  end
  return nil
end

---A scratch buffer standing for an open view VIEW_ID, with unsaved
---edits iff DIRTY.
local function view_buffer (view_id, dirty)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.b[buf].skg_view_id = view_id
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { '* a headline' })
  vim.bo[buf].modified = dirty
  return buf
end

describe('skg.repo_files_integrated', function ()
  local buffers
  local real_notify
  local real_request
  local requested

  before_each(function ()
    buffers = {}
    requested = nil
    wipe_warnings_buffer()
    lock.end_stream()
    real_notify = vim.notify
    vim.notify = function () end
    real_request = rerender.request_rerender_views_excluding
    rerender.request_rerender_views_excluding =
      function (excluded) requested = excluded end
  end)

  after_each(function ()
    rerender.request_rerender_views_excluding = real_request
    vim.notify = real_notify
    for _, buf in ipairs(buffers) do
      vim.bo[buf].modified = false
      pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
    wipe_warnings_buffer()
    lock.end_stream()
    helpers.reset_client_state()
  end)

  it('is dispatched though no request registered a handler', function ()
    local handled = nil
    local saved = state.unprompted_response_handler_map['repo-files-integrated']
    state.unprompted_response_handler_map['repo-files-integrated'] =
      function (payload_text) handled = payload_text end
    state.response_handler_map = {}
    length_prefix.dispatch_by_type(
      '((response-type repo-files-integrated) (warnings ()) (view-ids ()))')
    state.unprompted_response_handler_map['repo-files-integrated'] = saved
    assert.is_truthy(handled and handled:find('repo-files-integrated', 1, true))
    assert.are.equal(0, state.lp_pending_count)
  end)

  it('is registered as unprompted', function ()
    repo_files_integrated.register()
    assert.is_function(
      state.unprompted_response_handler_map['repo-files-integrated'])
  end)

  it('notifies, without the warnings buffer, when it only counts files', function ()
    local notified = nil
    vim.notify = function (text) notified = text end
    repo_files_integrated.handler(sexpr.read(
      '((response-type repo-files-integrated) '
      .. '(warnings ("Integrated 2 files that changed on disk in skgrepo \'first\'." '
      .. '"Integrated 1 file that changed on disk in skgrepo \'main\'.")) '
      .. '(view-ids ()))'))
    assert.are.equal(
      'skg: repo first: 2 files integrated; repo main: 1 file integrated',
      notified)
    assert.is_nil(warnings_text())
  end)

  it('appends real warnings, whole, to the server-warnings buffer', function ()
    repo_files_integrated.handler(sexpr.read(
      '((response-type repo-files-integrated) '
      .. '(warnings ("Integrated 2 files that changed on disk in skgrepo \'first\'." '
      .. '"Node bad-id changed on disk but could not be read.")) '
      .. '(view-ids ()))'))
    assert.are.equal(
      "* Repo changes\n** Integrated 2 files that changed on disk in skgrepo 'first'.\n"
      .. '** Node bad-id changed on disk but could not be read.',
      warnings_text())
  end)

  it('rerenders the clean views it names, excluding every other', function ()
    table.insert(buffers, view_buffer('named-clean', false))
    table.insert(buffers, view_buffer('named-dirty', true))
    table.insert(buffers, view_buffer('unnamed', false))
    repo_files_integrated.rerender_views_changed_on_disk(
      { 'named-clean', 'named-dirty' }, 0)
    table.sort(requested)
    assert.are.same({ 'named-dirty', 'unnamed' }, requested)
  end)

  it('asks for nothing when no named view is clean', function ()
    table.insert(buffers, view_buffer('named-dirty', true))
    repo_files_integrated.rerender_views_changed_on_disk(
      { 'named-dirty', 'not-open' }, 0)
    assert.is_nil(requested)
  end)

  it('waits for a stream in flight', function ()
    table.insert(buffers, view_buffer('named-clean', false))
    local real_defer = vim.defer_fn
    local deferred = nil
    vim.defer_fn = function (fn, ms) deferred = { fn = fn, ms = ms } end
    lock.begin_stream('save')
    repo_files_integrated.rerender_views_changed_on_disk({ 'named-clean' }, 0)
    vim.defer_fn = real_defer
    assert.is_nil(requested)
    assert.are.equal(repo_files_integrated.retry_milliseconds, deferred.ms)
    lock.end_stream()
    deferred.fn()
    assert.are.same({}, requested)
  end)
end)
