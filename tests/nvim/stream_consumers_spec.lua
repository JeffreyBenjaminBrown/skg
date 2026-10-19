-- Coverage for the rerender stream and its consumers (skgrepo
-- sets), plus the report-shaped requests (investigate unknown node,
-- export). Mirrors
-- tests/elisp/test-skg-investigate-unknown-node.el's wiring cases and
-- test-skg-warning-channel.el's rerender-done
-- channel, end-to-end through the loopback fake server.

local helpers = dofile(
  debug.getinfo(1, 'S').source:sub(2):match('^(.*)/') .. '/helpers.lua')

local buffer = require('skg.buffer')
local investigate_unknown_node = require('skg.investigate_unknown_node')
local lock = require('skg.lock')
local rerender = require('skg.rerender')
local repo_sets = require('skg.repo_sets')

local function wipe_named (name)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buf) == name then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
end

describe('skg stream consumers', function ()
  local server

  before_each(function ()
    helpers.install_fixture_herald_rules()
    helpers.wipe_skg_buffers()
    lock.end_stream()
  end)

  after_each(function ()
    if server then server.close() server = nil end
    helpers.reset_client_state()
    helpers.wipe_skg_buffers()
    lock.end_stream()
    wipe_named('skg://messages/rerender')
    wipe_named('skg://unknown-node')
  end)

  it('drives the rerender stream: lock, per-view update, done with'
     .. ' warnings', function ()
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('rerender all views', 1, true) then
        respond(helpers.framed(
          '((response-type rerender-lock)'
          .. ' (lock-views (view_id-a)))'))
        respond(helpers.framed(
          '((response-type rerender-view) (view-id view_id-a)'
          .. ' (content "* (skg (node (id a))) a rerendered"))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ())'
          .. ' (warnings ("stale herald")))'))
      end
    end)
    local a = buffer.open_org_buffer_from_text(
      '* (skg (node (id a))) a', 'skg://a', 'view_id-a')
    local b = buffer.open_org_buffer_from_text(
      '* (skg (node (id b))) b', 'skg://b', 'view_id-b')
    rerender.request_rerender_all_views()
    -- Locked immediately; b unlocks on rerender-lock (not listed).
    assert.is_false(vim.bo[a].modifiable)
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil end, 10)
    assert.is_true(vim.bo[a].modifiable)
    assert.is_true(vim.bo[b].modifiable)
    assert.are.equal('* (skg (node (id a))) a rerendered',
      vim.api.nvim_buf_get_lines(a, 0, 1, false)[1])
    -- The warning channel fired (mirrors test-skg-warning-channel).
    local found = nil
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_get_name(buf)
         == 'skg://messages/rerender' then found = buf end
    end
    assert.is_truthy(found)
    assert.is_truthy(table.concat(
      vim.api.nvim_buf_get_lines(found, 0, -1, false), '\n')
      :find('stale herald', 1, true))
  end)

  it('switches the repo-set and rides the rerender stream',
     function ()
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('set skgrepo restriction', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type skgrepo-restriction) (restriction "public")'
          .. ' (content "Skgrepo restriction: public"))'))
        respond(helpers.framed(
          '((response-type rerender-lock) (lock-views ()))'))
        respond(helpers.framed(
          '((response-type rerender-done) (errors ())'
          .. ' (warnings ()))'))
      end
    end)
    repo_sets.restrict_repo_set('public')
    vim.wait(3000, function ()
      return lock.stream_in_progress == nil and seen ~= nil end, 10)
    assert.are.equal(
      '((request . "set skgrepo restriction") (name . "public"))',
      seen)
  end)

  it('investigates the unknown headline at point and shows the answer'
     .. ' read-only, with the navigation keymap', function ()
    -- Mirrors test-skg-investigate-unknown-node.el.
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('investigate unknown node', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type investigate-unknown-node)'
          .. ' (content "* gone-id\\n** never present in the git history'
          .. ' of any skgrepo") (errors ()) (warnings ()))'))
      end
    end)
    local view = buffer.open_org_buffer_from_text(
      '* (skg (node (id p))) p\n** (skg (unknown (id gone-id))) ',
      'skg://p', 'view_id-p')
    vim.api.nvim_set_current_buf(view)
    vim.api.nvim_win_set_cursor(0, { 2, 0 })
    investigate_unknown_node.investigate_unknown_node()
    vim.wait(3000, function () return seen ~= nil end, 10)
    assert.are.equal(
      '((request . "investigate unknown node") (id . "gone-id"))', seen)
    local answer = nil
    vim.wait(2000, function ()
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_get_name(buf) == 'skg://unknown-node'
        then answer = buf end
      end
      return answer ~= nil
    end, 10)
    assert.is_truthy(answer)
    assert.are.equal('org', vim.bo[answer].filetype)
    assert.is_false(vim.bo[answer].modifiable)
    assert.is_truthy(table.concat(
      vim.api.nvim_buf_get_lines(answer, 0, -1, false), '\n')
      :find('never present', 1, true))
    local has_goto = false
    for _, map in ipairs(vim.api.nvim_buf_get_keymap(answer, 'n')) do
      if map.desc and map.desc:find('Open a view') then
        has_goto = true end
    end
    assert.is_true(has_goto)
  end)

  it('prompts for the ID away from an unknown headline', function ()
    local seen = nil
    server = helpers.connect_to_fake_server(function (line, respond)
      if line:find('investigate unknown node', 1, true) then
        seen = line
        respond(helpers.framed(
          '((response-type investigate-unknown-node)'
          .. ' (content "typed-id resolves to node t (typed-id) in main")'
          .. ' (errors ()) (warnings ()))'))
      end
    end)
    local view = buffer.open_org_buffer_from_text(
      '* (skg (node (id p))) p', 'skg://p', 'view_id-p')
    vim.api.nvim_set_current_buf(view)
    local original_input = vim.fn.input
    local prompted = nil
    vim.fn.input = function (prompt) prompted = prompt return 'typed-id' end
    investigate_unknown_node.investigate_unknown_node()
    vim.fn.input = original_input
    assert.are.equal('Investigate ID: ', prompted)
    vim.wait(3000, function () return seen ~= nil end, 10)
    assert.is_truthy(seen:find('(id . "typed-id")', 1, true))
  end)
end)
