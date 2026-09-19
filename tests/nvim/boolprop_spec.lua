local boolprop = require('skg.boolprop')
local compare = require('skg.sexpr.compare')
local config = require('skg.config')
local metadata = require('skg.metadata')
local picker = require('skg.picker')
local sexpr = require('skg.sexpr.parse')
local state = require('skg.state')
local client = require('skg.client')

local function buffer_with (lines)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  return buf
end

local function has_request (line, value)
  local meta = metadata.metadata_sexp_at_line_or_nil(line)
  return compare.subtree_p(meta, sexpr.read(string.format(
    '(skg (node (editRequest (property noSearchMatching %s))))', value)))
end

describe('skg.boolprop staging', function ()
  local real_owned_sources = config.owned_sources
  local real_notify = vim.notify
  local real_send_string = client.send_string
  local real_picker = picker.completing_read_with_cycle

  before_each(function ()
    config.owned_sources = function () return { 'main' } end
    vim.notify = function () end
  end)

  after_each(function ()
    config.owned_sources = real_owned_sources
    vim.notify = real_notify
    client.send_string = real_send_string
    picker.completing_read_with_cycle = real_picker
    state.response_handler_map['property-state'] = nil
    state.lp_pending_count = 0
    pcall(vim.api.nvim_buf_delete,
          vim.api.nvim_get_current_buf(), { force = true })
  end)

  it('stamps the exact set and clear request shapes', function ()
    buffer_with({ '* (skg (node (id root) (source main))) root' })
    boolprop._apply(1, true, false)
    assert.is_true(has_request(1, 'true'))

    buffer_with({ '* (skg (node (id root) (source main))) root' })
    boolprop._apply(1, false, false)
    assert.is_true(has_request(1, 'false'))
  end)

  it('recurses only through true content and skips ineligible PIDs', function ()
    buffer_with({
      '* (skg (node (id root) (source main))) root',
      '** (skg (node (id child) (source main))) child',
      '** (skg (node (id child) (source main))) duplicate child',
      '** (skg (node (id protected) (source main) writeProtected)) protected',
      '** (skg (node (id foreign) (source elsewhere))) foreign',
      '** (skg (node (id conflict) (source main) (editRequest delete))) conflict',
      '** (skg (node (id link) (source main) (affectsParent false))) link',
      '*** (skg (node (id under-link) (source main))) under link',
      '** (skg aliasFolder) aliases',
      '*** (skg (node (id under-folder) (source main))) under folder',
      '* (skg (node (id sibling) (source main))) sibling' })

    boolprop._apply(1, true, true)

    assert.is_true(has_request(1, 'true'))
    assert.is_true(has_request(2, 'true'))
    assert.is_false(has_request(3, 'true'))
    for _, line in ipairs({ 4, 5, 6, 7, 8, 9, 10, 11 }) do
      assert.is_false(has_request(line, 'true'), 'line ' .. line)
    end
  end)

  it('uses server state, follows its extmark, and only stages', function ()
    local sent, initial
    client.send_string = function (text) sent = text end
    picker.completing_read_with_cycle = function (_prompt, _choices, opts)
      initial = opts.initial_input
      return 'no search matching'
    end
    local buf = buffer_with({
      '* (skg (node (id root) (source main))) root' })
    boolprop._request(false)
    assert.is_truthy(sent:find('(request . "property state")', 1, true))
    vim.api.nvim_buf_set_lines(buf, 0, 0, false, { 'preamble' })
    local handler = state.response_handler_map['property-state'].handler
    handler('', sexpr.read(
      '((response-type property-state) (id "root")'
      .. ' (property "noSearchMatching") (value "false")'
      .. ' (source "main") (user-owned "true"))'))
    assert.are.equal('search matching', initial)
    assert.is_true(has_request(2, 'true'))
  end)
end)
