local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local flag = require('skg.flag_state')
local compare = require('skg.sexpr.compare')
local config = require('skg.config')
local metadata = require('skg.metadata')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')
local client = require('skg.client')

local buffer_with = helpers.buffer_with

local function has_request (line, value)
  local meta = metadata.metadata_sexp_at_line_or_nil(line)
  return compare.subtree_p(meta, sexpr.read(string.format(
    '(skg node (editRequest (flag noSearchMatching %s)))', value)))
end

describe('skg.flag_state staging', function ()
  local real_owned_skgrepos = config.owned_repos
  local real_notify = vim.notify
  local real_send_string = client.send_string
  local real_picker = picker.completing_read_with_cycle

  before_each(function ()
    config.owned_repos = function () return { 'main' } end
    vim.notify = function () end
  end)

  after_each(function ()
    config.owned_repos = real_owned_skgrepos
    vim.notify = real_notify
    client.send_string = real_send_string
    picker.completing_read_with_cycle = real_picker
    state.response_handler_map['flag-state'] = nil
    state.lp_pending_count = 0
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('stamps the exact set and clear request shapes', function ()
    buffer_with({ '* (skg node (gnid root) (skgRepos (nodeRepo main))) root' })
    flag._apply(1, true, false)
    assert.is_true(has_request(1, 'true'))

    buffer_with({ '* (skg node (gnid root) (skgRepos (nodeRepo main))) root' })
    flag._apply(1, false, false)
    assert.is_true(has_request(1, 'false'))
  end)

  it('recurses only through true content and skips ineligible PIDs', function ()
    buffer_with({
      '* (skg node (gnid root) (skgRepos (nodeRepo main))) root',
      '** (skg node (gnid child) (skgRepos (nodeRepo main))) child',
      '** (skg node (gnid child) (skgRepos (nodeRepo main))) duplicate child',
      '** (skg node (gnid protected) (skgRepos (nodeRepo main)) writeProtected) protected',
      '** (skg node (gnid foreign) (skgRepos (nodeRepo elsewhere))) foreign',
      '** (skg node (gnid conflict) (skgRepos (nodeRepo main)) (editRequest delete)) conflict',
      '** (skg node (gnid link) (skgRepos (nodeRepo main)) noClaim) link',
      '*** (skg node (gnid under-link) (skgRepos (nodeRepo main))) under link',
      '** (skg aliasFolder) aliases',
      '*** (skg node (gnid under-folder) (skgRepos (nodeRepo main))) under folder',
      '* (skg node (gnid sibling) (skgRepos (nodeRepo main))) sibling' })

    flag._apply(1, true, true)

    assert.is_true(has_request(1, 'true'))
    assert.is_true(has_request(2, 'true'))
    assert.is_false(has_request(3, 'true'))
    for _, line in ipairs({ 4, 5, 6, 7, 8, 9, 10, 11 }) do
      assert.is_false(has_request(line, 'true'), 'line ' .. line)
    end
  end)

  it('lets a headline holding only a readiness request take the flag'
     .. ' request', function ()
    -- Mirrors test-skg-flag-a-readiness-request-takes-the-flag-request:
    -- such a headline holds no edit request
    -- (metadata.node_edit_request_p), so the command asks the server,
    -- and a recursive staging stamps it, keeping the readiness request
    -- after the flag request.
    local sent
    client.send_string = function (text) sent = text end
    buffer_with({
      '* (skg node (gnid root) (skgRepos (nodeRepo main)) (editRequest (ready node))) root',
      '** (skg node (gnid child) (skgRepos (nodeRepo main)) (editRequest (unready rel)) (diff (rel added))) child' })
    flag.set_flag_search_matching()
    assert.is_truthy(sent:find('(request . "flag state")', 1, true))
    flag._apply(1, true, true)
    assert.are.same({
      '* (skg node (gnid root) (skgRepos (nodeRepo main)) (editRequest (flag noSearchMatching true) (ready node))) root',
      '** (skg node (gnid child) (skgRepos (nodeRepo main)) (editRequest (flag noSearchMatching true) (unready rel)) (diff (rel added))) child' },
      vim.api.nvim_buf_get_lines(0, 0, -1, false))
  end)

  it('uses server state, follows its extmark, and only stages', function ()
    local sent, initial
    client.send_string = function (text) sent = text end
    picker.completing_read_with_cycle = function (_prompt, _choices, opts)
      initial = opts.initial_input
      return 'no search matching'
    end
    local buf = buffer_with({
      '* (skg node (gnid root) (skgRepos (nodeRepo main))) root' })
    flag._request(false)
    assert.is_truthy(sent:find('(request . "flag state")', 1, true))
    vim.api.nvim_buf_set_lines(buf, 0, 0, false, { 'preamble' })
    local handler = state.response_handler_map['flag-state'].handler
    handler('', sexpr.read(
      '((response-type flag-state) (id "root")'
      .. ' (flag "noSearchMatching") (value "false")'
      .. ' (repo "main") (owned "true"))'))
    assert.are.equal('search matching', initial)
    assert.is_true(has_request(2, 'true'))
  end)
end)
